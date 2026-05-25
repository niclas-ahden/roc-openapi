module [
    ValidationError,
    validate,
    navigate_ref, # used by OpenApi.roc for spec navigation
]

import json_value.JsonValue exposing [JsonValue]

## A single validation error with path, message, and the JSON Schema keyword that failed.
ValidationError : {
    path : List Str,
    message : Str,
    keyword : Str,
}

## Validate a JSON value against a JSON Schema.
## root_doc is the full document (for $ref resolution).
validate : JsonValue, JsonValue, JsonValue, List Str -> List ValidationError
validate = |value, schema, root_doc, path|
    when resolve_schema(schema, root_doc, []) is
        Ok(resolved_schema) -> validate_resolved(value, resolved_schema, root_doc, path)
        Err(CircularRef(ref_str)) -> [{ path, message: "Circular $$ref: ${ref_str}", keyword: "$ref" }]

validate_resolved : JsonValue, JsonValue, JsonValue, List Str -> List ValidationError
validate_resolved = |value, schema, root_doc, path|
    # nullable short-circuit: if schema has "nullable": true and value is null, skip all validation
    if is_schema_nullable(schema) && JsonValue.is_null(value) then
        []
    else
        List.join([
            validate_type(value, schema, path),
            validate_required(value, schema, path),
            validate_properties(value, schema, root_doc, path),
            validate_items(value, schema, root_doc, path),
            validate_min_length(value, schema, path),
            validate_max_length(value, schema, path),
            validate_max_items(value, schema, path),
            validate_max_properties(value, schema, path),
            validate_enum(value, schema, path),
            validate_unique_items(value, schema, path),
            validate_additional_properties(value, schema, root_doc, path),
            validate_format(value, schema, path),
            validate_any_of(value, schema, root_doc, path),
            validate_one_of(value, schema, root_doc, path),
            validate_all_of(value, schema, root_doc, path),
        ])

is_schema_nullable : JsonValue -> Bool
is_schema_nullable = |schema|
    when JsonValue.get_field(schema, "nullable") is
        Ok(val) -> JsonValue.get_bool(val) |> Result.with_default(Bool.false)
        Err(_) -> Bool.false

# ── $ref resolution ──

## Resolve $ref chains using a visited set to detect cycles.
resolve_schema : JsonValue, JsonValue, List Str -> Result JsonValue [CircularRef Str]
resolve_schema = |schema, root_doc, seen|
    when JsonValue.get_field(schema, "$ref") is
        Ok(ref_val) ->
            when JsonValue.get_str(ref_val) is
                Ok(ref_str) ->
                    if List.contains(seen, ref_str) then
                        Err(CircularRef(ref_str))
                    else
                        when navigate_ref(root_doc, ref_str) is
                            Ok(resolved) -> resolve_schema(resolved, root_doc, List.append(seen, ref_str))
                            Err(_) -> Ok(schema) # broken ref: validate against the ref object itself

                Err(_) -> Ok(schema)

        Err(_) -> Ok(schema)

## Navigate a $ref path like "#/components/schemas/Foo" through the document tree.
## Reference tokens are decoded per RFC 6901: `~1` → `/` then `~0` → `~`.
navigate_ref : JsonValue, Str -> Result JsonValue [RefNotFound Str]
navigate_ref = |root, ref_str|
    path_str = ref_str |> Str.replace_first("#/", "")
    segments = Str.split_on(path_str, "/") |> List.map(decode_ref_token)

    List.walk_try(segments, root, |current, segment|
        JsonValue.get_field(current, segment)
        |> Result.map_err(|_| RefNotFound(ref_str))
    )

# RFC 6901: substitute `~1` → `/` first, then `~0` → `~` (order matters so
# that `~01` decodes to `~1` rather than to `/`).
decode_ref_token : Str -> Str
decode_ref_token = |token|
    token
    |> Str.replace_each("~1", "/")
    |> Str.replace_each("~0", "~")

# ── type ──

validate_type : JsonValue, JsonValue, List Str -> List ValidationError
validate_type = |value, schema, path|
    when JsonValue.get_field(schema, "type") is
        Ok(type_val) ->
            actual_type = JsonValue.type_name(value)
            when JsonValue.get_str(type_val) is
                Ok(expected_type) ->
                    # Single type string
                    if check_type_match(value, expected_type, actual_type) then
                        []
                    else
                        [{ path, message: "Expected type ${expected_type} but got ${actual_type}", keyword: "type" }]

                Err(_) ->
                    # Try as array of types: ["string", "null"]
                    when JsonValue.get_array(type_val) is
                        Ok(type_list) ->
                            type_strings = List.keep_oks(type_list, JsonValue.get_str)
                            if List.any(type_strings, |t| check_type_match(value, t, actual_type)) then
                                []
                            else
                                type_names = Str.join_with(type_strings, ", ")
                                [{ path, message: "Expected one of types [${type_names}] but got ${actual_type}", keyword: "type" }]

                        Err(_) -> []

        Err(_) -> []

check_type_match : JsonValue, Str, Str -> Bool
check_type_match = |value, expected_type, actual_type|
    if expected_type == "integer" then
        when JsonValue.get_number(value) is
            Ok(n) -> Num.to_str(n) == Num.to_str(Num.floor(n))
            Err(_) -> Bool.false
    else
        actual_type == expected_type

# ── required ──

validate_required : JsonValue, JsonValue, List Str -> List ValidationError
validate_required = |value, schema, path|
    when JsonValue.get_field(schema, "required") is
        Ok(required_val) ->
            when JsonValue.get_array(required_val) is
                Ok(required_list) ->
                    when JsonValue.get_object(value) is
                        Ok(fields) ->
                            field_names = List.map(fields, .key)
                            List.keep_if(required_list, |req_val|
                                when JsonValue.get_str(req_val) is
                                    Ok(req_name) -> !(List.contains(field_names, req_name))
                                    Err(_) -> Bool.false
                            )
                            |> List.map(|req_val|
                                req_name = JsonValue.get_str(req_val) |> Result.with_default("?")
                                { path, message: "Missing required field: ${req_name}", keyword: "required" }
                            )

                        Err(_) -> []

                Err(_) -> []

        Err(_) -> []

# ── properties ──

validate_properties : JsonValue, JsonValue, JsonValue, List Str -> List ValidationError
validate_properties = |value, schema, root_doc, path|
    when JsonValue.get_field(schema, "properties") is
        Ok(props_val) ->
            when (JsonValue.get_object(value), JsonValue.get_object(props_val)) is
                (Ok(value_fields), Ok(prop_schemas)) ->
                    required_names = get_required_names(schema)

                    List.join_map(value_fields, |{ key, value: field_value }|
                        when List.find_first(prop_schemas, |ps| ps.key == key) is
                            Ok({ value: prop_schema }) ->
                                # null values on non-required fields are treated as absent.
                                # Many APIs (including SendGrid) accept null for optional fields
                                # even when the schema doesn't explicitly include "null" in the type.
                                if JsonValue.is_null(field_value) && !(List.contains(required_names, key)) then
                                    []
                                else
                                    validate(field_value, prop_schema, root_doc, List.append(path, key))

                            Err(_) -> []
                    )

                _ -> []

        Err(_) -> []

get_required_names : JsonValue -> List Str
get_required_names = |schema|
    when JsonValue.get_field(schema, "required") is
        Ok(req_val) ->
            when JsonValue.get_array(req_val) is
                Ok(req_list) -> List.keep_oks(req_list, JsonValue.get_str)
                Err(_) -> []

        Err(_) -> []

# ── items ──

validate_items : JsonValue, JsonValue, JsonValue, List Str -> List ValidationError
validate_items = |value, schema, root_doc, path|
    when JsonValue.get_field(schema, "items") is
        Ok(items_schema) ->
            when JsonValue.get_array(value) is
                Ok(items) ->
                    List.map_with_index(items, |item, idx|
                        validate(item, items_schema, root_doc, List.append(path, Num.to_str(idx)))
                    )
                    |> List.join

                Err(_) -> []

        Err(_) -> []

# ── minLength ──

validate_min_length : JsonValue, JsonValue, List Str -> List ValidationError
validate_min_length = |value, schema, path|
    when JsonValue.get_field(schema, "minLength") is
        Ok(min_val) ->
            when (JsonValue.get_str(value), JsonValue.get_number(min_val)) is
                (Ok(s), Ok(min)) ->
                    len = JsonValue.count_scalar_values(s) |> Num.to_f64
                    if len < min then
                        [{ path, message: "String length ${Num.to_str(len)} is less than minLength ${Num.to_str(min)}", keyword: "minLength" }]
                    else
                        []

                _ -> []

        Err(_) -> []

# ── maxLength ──

validate_max_length : JsonValue, JsonValue, List Str -> List ValidationError
validate_max_length = |value, schema, path|
    when JsonValue.get_field(schema, "maxLength") is
        Ok(max_val) ->
            when (JsonValue.get_str(value), JsonValue.get_number(max_val)) is
                (Ok(s), Ok(max)) ->
                    len = JsonValue.count_scalar_values(s) |> Num.to_f64
                    if len > max then
                        [{ path, message: "String length ${Num.to_str(len)} exceeds maxLength ${Num.to_str(max)}", keyword: "maxLength" }]
                    else
                        []

                _ -> []

        Err(_) -> []

# ── maxItems ──

validate_max_items : JsonValue, JsonValue, List Str -> List ValidationError
validate_max_items = |value, schema, path|
    when JsonValue.get_field(schema, "maxItems") is
        Ok(max_val) ->
            when (JsonValue.get_array(value), JsonValue.get_number(max_val)) is
                (Ok(items), Ok(max)) ->
                    len = List.len(items) |> Num.to_f64
                    if len > max then
                        [{ path, message: "Array has ${Num.to_str(len)} items, exceeds maxItems ${Num.to_str(max)}", keyword: "maxItems" }]
                    else
                        []

                _ -> []

        Err(_) -> []

# ── maxProperties ──

validate_max_properties : JsonValue, JsonValue, List Str -> List ValidationError
validate_max_properties = |value, schema, path|
    when JsonValue.get_field(schema, "maxProperties") is
        Ok(max_val) ->
            when (JsonValue.get_object(value), JsonValue.get_number(max_val)) is
                (Ok(fields), Ok(max)) ->
                    len = List.len(fields) |> Num.to_f64
                    if len > max then
                        [{ path, message: "Object has ${Num.to_str(len)} properties, exceeds maxProperties ${Num.to_str(max)}", keyword: "maxProperties" }]
                    else
                        []

                _ -> []

        Err(_) -> []

# ── enum ──

validate_enum : JsonValue, JsonValue, List Str -> List ValidationError
validate_enum = |value, schema, path|
    when JsonValue.get_field(schema, "enum") is
        Ok(enum_val) ->
            when JsonValue.get_array(enum_val) is
                Ok(allowed) ->
                    if List.contains(allowed, value) then
                        []
                    else
                        allowed_strs = List.map(allowed, |a|
                            when JsonValue.get_str(a) is
                                Ok(s) -> "\"${s}\""
                                Err(_) -> Inspect.to_str(a)
                        )
                        allowed_list = Str.join_with(allowed_strs, ", ")
                        actual_str =
                            when JsonValue.get_str(value) is
                                Ok(s) -> "\"${s}\""
                                Err(_) -> Inspect.to_str(value)
                        [{ path, message: "Value ${actual_str} not in enum [${allowed_list}]", keyword: "enum" }]

                Err(_) -> []

        Err(_) -> []

# ── uniqueItems ──

validate_unique_items : JsonValue, JsonValue, List Str -> List ValidationError
validate_unique_items = |value, schema, path|
    when JsonValue.get_field(schema, "uniqueItems") is
        Ok(unique_val) ->
            when (JsonValue.get_bool(unique_val), JsonValue.get_array(value)) is
                (Ok(unique), Ok(items)) ->
                    if unique && has_duplicates(items, 0) then
                        [{ path, message: "Array contains duplicate items", keyword: "uniqueItems" }]
                    else
                        []

                _ -> []

        Err(_) -> []

has_duplicates : List JsonValue, U64 -> Bool
has_duplicates = |items, idx|
    when List.get(items, idx) is
        Ok(item) ->
            rest = List.drop_first(items, idx + 1)
            if List.contains(rest, item) then
                Bool.true
            else
                has_duplicates(items, idx + 1)

        Err(_) -> Bool.false

# ── additionalProperties ──

validate_additional_properties : JsonValue, JsonValue, JsonValue, List Str -> List ValidationError
validate_additional_properties = |value, schema, root_doc, path|
    when JsonValue.get_field(schema, "additionalProperties") is
        Ok(ap_schema) ->
            when (JsonValue.get_object(value), JsonValue.get_object(schema |> get_properties_or_empty)) is
                (Ok(value_fields), Ok(defined_props)) ->
                    defined_keys = List.map(defined_props, .key)
                    extra_fields = List.keep_if(value_fields, |f| !(List.contains(defined_keys, f.key)))

                    when JsonValue.get_bool(ap_schema) is
                        Ok(ap_bool) ->
                            if ap_bool == Bool.false then
                                List.map(extra_fields, |f|
                                    { path: List.append(path, f.key), message: "Additional property '${f.key}' not allowed", keyword: "additionalProperties" }
                                )
                            else
                                []

                        Err(_) ->
                            # additionalProperties is a schema: validate extra fields against it
                            List.join_map(extra_fields, |f|
                                validate(f.value, ap_schema, root_doc, List.append(path, f.key))
                            )

                _ -> []

        Err(_) -> []

get_properties_or_empty : JsonValue -> JsonValue
get_properties_or_empty = |schema|
    when JsonValue.get_field(schema, "properties") is
        Ok(props) -> props
        Err(_) -> JsonValue.object([])

# ── format ──

validate_format : JsonValue, JsonValue, List Str -> List ValidationError
validate_format = |value, schema, path|
    when JsonValue.get_field(schema, "format") is
        Ok(format_val) ->
            when (JsonValue.get_str(format_val), JsonValue.get_str(value)) is
                (Ok("email"), Ok(s)) ->
                    if is_valid_email(s) then
                        []
                    else
                        [{ path, message: "Invalid email format: ${s}", keyword: "format" }]

                _ -> []

        Err(_) -> []

is_valid_email : Str -> Bool
is_valid_email = |s|
    when Str.split_first(s, "@") is
        Ok({ before, after }) ->
            !(Str.is_empty(before)) && !(Str.is_empty(after)) && Str.contains(after, ".")

        Err(_) -> Bool.false

# ── anyOf ──

validate_any_of : JsonValue, JsonValue, JsonValue, List Str -> List ValidationError
validate_any_of = |value, schema, root_doc, path|
    when JsonValue.get_field(schema, "anyOf") is
        Ok(any_of_val) ->
            when JsonValue.get_array(any_of_val) is
                Ok(sub_schemas) ->
                    if List.any(sub_schemas, |sub| List.is_empty(validate(value, sub, root_doc, path))) then
                        []
                    else
                        count = List.len(sub_schemas) |> Num.to_str
                        [{ path, message: "Value does not match any of ${count} schemas in anyOf", keyword: "anyOf" }]

                Err(_) -> []

        Err(_) -> []

# ── oneOf ──

validate_one_of : JsonValue, JsonValue, JsonValue, List Str -> List ValidationError
validate_one_of = |value, schema, root_doc, path|
    when JsonValue.get_field(schema, "oneOf") is
        Ok(one_of_val) ->
            when JsonValue.get_array(one_of_val) is
                Ok(sub_schemas) ->
                    # Try discriminator optimization first
                    when get_discriminator_schema(schema, value, root_doc) is
                        Ok(target_schema) ->
                            validate(value, target_schema, root_doc, path)

                        Err(NoDiscriminator) ->
                            # Short-circuit: stop counting after 2 matches (already invalid)
                            match_count = count_matches(sub_schemas, value, root_doc, path, 0, 0)
                            if match_count == 1 then
                                []
                            else if match_count == 0 then
                                count = List.len(sub_schemas) |> Num.to_str
                                [{ path, message: "Value does not match any of ${count} schemas in oneOf", keyword: "oneOf" }]
                            else
                                [{ path, message: "Value matches multiple schemas in oneOf, expected exactly 1", keyword: "oneOf" }]

                        Err(DiscriminatorFailed(msg)) ->
                            [{ path, message: msg, keyword: "discriminator" }]

                Err(_) -> []

        Err(_) -> []

count_matches : List JsonValue, JsonValue, JsonValue, List Str, U64, U64 -> U64
count_matches = |sub_schemas, value, root_doc, path, idx, matches|
    if matches >= 2 then
        matches # short-circuit: already invalid
    else
        when List.get(sub_schemas, idx) is
            Ok(sub) ->
                new_matches = if List.is_empty(validate(value, sub, root_doc, path)) then matches + 1 else matches
                count_matches(sub_schemas, value, root_doc, path, idx + 1, new_matches)

            Err(_) -> matches

get_discriminator_schema : JsonValue, JsonValue, JsonValue -> Result JsonValue [NoDiscriminator, DiscriminatorFailed Str]
get_discriminator_schema = |schema, value, root_doc|
    disc = JsonValue.get_field(schema, "discriminator") |> Result.map_err(|_| NoDiscriminator)?
    prop_name = JsonValue.get_field(disc, "propertyName")
        |> Result.try(JsonValue.get_str)
        |> Result.map_err(|_| NoDiscriminator)?

    disc_value = JsonValue.get_field(value, prop_name)
        |> Result.try(JsonValue.get_str)
        |> Result.map_err(|_| DiscriminatorFailed("Missing or non-string discriminator property '${prop_name}'"))?

    # Look up in mapping, or default to #/components/schemas/{value}
    ref_str =
        when JsonValue.get_field(disc, "mapping") is
            Ok(mapping) ->
                when JsonValue.get_field(mapping, disc_value) is
                    Ok(ref_val) -> JsonValue.get_str(ref_val) |> Result.with_default("#/components/schemas/${disc_value}")
                    Err(_) -> "#/components/schemas/${disc_value}"

            Err(_) -> "#/components/schemas/${disc_value}"

    navigate_ref(root_doc, ref_str)
    |> Result.map_err(|_| DiscriminatorFailed("Discriminator value '${disc_value}' does not resolve via ${ref_str}"))

# ── allOf ──

validate_all_of : JsonValue, JsonValue, JsonValue, List Str -> List ValidationError
validate_all_of = |value, schema, root_doc, path|
    when JsonValue.get_field(schema, "allOf") is
        Ok(all_of_val) ->
            when JsonValue.get_array(all_of_val) is
                Ok(sub_schemas) ->
                    List.join_map(sub_schemas, |sub|
                        validate(value, sub, root_doc, path)
                    )

                Err(_) -> []

        Err(_) -> []

# ── Tests ──

# type validation
expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("string") }])
    validate(JsonValue.string("hello"), schema, schema, []) == []

expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("string") }])
    errors = validate(JsonValue.number(42.0), schema, schema, [])
    List.len(errors) == 1 && (List.first(errors) |> Result.map_ok(.keyword) |> Result.with_default("")) == "type"

expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("integer") }])
    validate(JsonValue.number(42.0), schema, schema, []) == []

expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("integer") }])
    errors = validate(JsonValue.number(3.14), schema, schema, [])
    List.len(errors) == 1

expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("object") }])
    validate(JsonValue.object([]), schema, schema, []) == []

expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("array") }])
    validate(JsonValue.array([]), schema, schema, []) == []

expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("boolean") }])
    validate(JsonValue.bool(Bool.true), schema, schema, []) == []

# required
expect
    schema = JsonValue.object([
        { key: "type", value: JsonValue.string("object") },
        { key: "required", value: JsonValue.array([JsonValue.string("name"), JsonValue.string("email")]) },
    ])
    value = JsonValue.object([{ key: "name", value: JsonValue.string("Alice") }, { key: "email", value: JsonValue.string("a@b.com") }])
    validate(value, schema, schema, []) == []

expect
    schema = JsonValue.object([
        { key: "type", value: JsonValue.string("object") },
        { key: "required", value: JsonValue.array([JsonValue.string("name"), JsonValue.string("email")]) },
    ])
    value = JsonValue.object([{ key: "name", value: JsonValue.string("Alice") }])
    errors = validate(value, schema, schema, [])
    List.len(errors) == 1 && (List.first(errors) |> Result.map_ok(.keyword) |> Result.with_default("")) == "required"

# properties (nested validation)
expect
    schema = JsonValue.object([
        { key: "type", value: JsonValue.string("object") },
        { key: "properties", value: JsonValue.object([
            { key: "age", value: JsonValue.object([{ key: "type", value: JsonValue.string("integer") }]) },
        ]) },
    ])
    value = JsonValue.object([{ key: "age", value: JsonValue.string("not a number") }])
    errors = validate(value, schema, schema, [])
    List.len(errors) == 1 && (List.first(errors) |> Result.map_ok(.path) |> Result.with_default([])) == ["age"]

# items (array element validation)
expect
    schema = JsonValue.object([
        { key: "type", value: JsonValue.string("array") },
        { key: "items", value: JsonValue.object([{ key: "type", value: JsonValue.string("string") }]) },
    ])
    value = JsonValue.array([JsonValue.string("a"), JsonValue.number(1.0)])
    errors = validate(value, schema, schema, [])
    List.len(errors) == 1 && (List.first(errors) |> Result.map_ok(.path) |> Result.with_default([])) == ["1"]

# minLength
expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("string") }, { key: "minLength", value: JsonValue.number(1.0) }])
    validate(JsonValue.string("x"), schema, schema, []) == []

expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("string") }, { key: "minLength", value: JsonValue.number(1.0) }])
    errors = validate(JsonValue.string(""), schema, schema, [])
    List.len(errors) == 1

# maxLength
expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("string") }, { key: "maxLength", value: JsonValue.number(3.0) }])
    validate(JsonValue.string("abc"), schema, schema, []) == []

expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("string") }, { key: "maxLength", value: JsonValue.number(3.0) }])
    errors = validate(JsonValue.string("abcd"), schema, schema, [])
    List.len(errors) == 1

# maxItems
expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("array") }, { key: "maxItems", value: JsonValue.number(2.0) }])
    validate(JsonValue.array([JsonValue.null, JsonValue.null]), schema, schema, []) == []

expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("array") }, { key: "maxItems", value: JsonValue.number(1.0) }])
    errors = validate(JsonValue.array([JsonValue.null, JsonValue.null]), schema, schema, [])
    List.len(errors) == 1

# enum
expect
    schema = JsonValue.object([{ key: "enum", value: JsonValue.array([JsonValue.string("a"), JsonValue.string("b")]) }])
    validate(JsonValue.string("a"), schema, schema, []) == []

expect
    schema = JsonValue.object([{ key: "enum", value: JsonValue.array([JsonValue.string("a"), JsonValue.string("b")]) }])
    errors = validate(JsonValue.string("c"), schema, schema, [])
    List.len(errors) == 1

# uniqueItems
expect
    schema = JsonValue.object([{ key: "uniqueItems", value: JsonValue.bool(Bool.true) }])
    validate(JsonValue.array([JsonValue.number(1.0), JsonValue.number(2.0)]), schema, schema, []) == []

expect
    schema = JsonValue.object([{ key: "uniqueItems", value: JsonValue.bool(Bool.true) }])
    errors = validate(JsonValue.array([JsonValue.number(1.0), JsonValue.number(1.0)]), schema, schema, [])
    List.len(errors) == 1

# format: email
expect
    schema = JsonValue.object([{ key: "format", value: JsonValue.string("email") }])
    validate(JsonValue.string("user@example.com"), schema, schema, []) == []

expect
    schema = JsonValue.object([{ key: "format", value: JsonValue.string("email") }])
    errors = validate(JsonValue.string("not-an-email"), schema, schema, [])
    List.len(errors) == 1

# $ref resolution
expect
    root = JsonValue.object([
        { key: "components", value: JsonValue.object([
            { key: "schemas", value: JsonValue.object([
                { key: "Name", value: JsonValue.object([{ key: "type", value: JsonValue.string("string") }]) },
            ]) },
        ]) },
    ])
    schema = JsonValue.object([{ key: "$ref", value: JsonValue.string("#/components/schemas/Name") }])
    validate(JsonValue.string("hello"), schema, root, []) == []

expect
    root = JsonValue.object([
        { key: "components", value: JsonValue.object([
            { key: "schemas", value: JsonValue.object([
                { key: "Name", value: JsonValue.object([{ key: "type", value: JsonValue.string("string") }]) },
            ]) },
        ]) },
    ])
    schema = JsonValue.object([{ key: "$ref", value: JsonValue.string("#/components/schemas/Name") }])
    errors = validate(JsonValue.number(42.0), schema, root, [])
    List.len(errors) == 1

# Combined: required + properties + type
expect
    schema = JsonValue.object([
        { key: "type", value: JsonValue.string("object") },
        { key: "required", value: JsonValue.array([JsonValue.string("email")]) },
        { key: "properties", value: JsonValue.object([
            { key: "email", value: JsonValue.object([
                { key: "type", value: JsonValue.string("string") },
                { key: "format", value: JsonValue.string("email") },
            ]) },
        ]) },
    ])
    value = JsonValue.object([{ key: "email", value: JsonValue.string("a@b.com") }])
    validate(value, schema, schema, []) == []

# No schema constraints = anything passes
expect
    schema = JsonValue.object([])
    validate(JsonValue.string("anything"), schema, schema, []) == []

# ── Additional tests for missing coverage ──

# maxProperties: pass
expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("object") }, { key: "maxProperties", value: JsonValue.number(2.0) }])
    value = JsonValue.object([{ key: "a", value: JsonValue.number(1.0) }])
    validate(value, schema, schema, []) == []

# maxProperties: fail
expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("object") }, { key: "maxProperties", value: JsonValue.number(1.0) }])
    value = JsonValue.object([{ key: "a", value: JsonValue.number(1.0) }, { key: "b", value: JsonValue.number(2.0) }])
    errors = validate(value, schema, schema, [])
    List.len(errors) == 1 && (List.first(errors) |> Result.map_ok(.keyword) |> Result.with_default("")) == "maxProperties"

# additionalProperties false, no extra fields (pass)
expect
    schema = JsonValue.object([
        { key: "type", value: JsonValue.string("object") },
        { key: "properties", value: JsonValue.object([{ key: "name", value: JsonValue.object([]) }]) },
        { key: "additionalProperties", value: JsonValue.bool(Bool.false) },
    ])
    value = JsonValue.object([{ key: "name", value: JsonValue.string("ok") }])
    validate(value, schema, schema, []) == []

# additionalProperties false, extra field (fail)
expect
    schema = JsonValue.object([
        { key: "type", value: JsonValue.string("object") },
        { key: "properties", value: JsonValue.object([{ key: "name", value: JsonValue.object([]) }]) },
        { key: "additionalProperties", value: JsonValue.bool(Bool.false) },
    ])
    value = JsonValue.object([{ key: "name", value: JsonValue.string("ok") }, { key: "extra", value: JsonValue.number(1.0) }])
    errors = validate(value, schema, schema, [])
    List.len(errors) == 1 && (List.first(errors) |> Result.map_ok(.keyword) |> Result.with_default("")) == "additionalProperties"

# additionalProperties with sub-schema: extra field validated against it
expect
    schema = JsonValue.object([
        { key: "type", value: JsonValue.string("object") },
        { key: "properties", value: JsonValue.object([{ key: "name", value: JsonValue.object([]) }]) },
        { key: "additionalProperties", value: JsonValue.object([{ key: "type", value: JsonValue.string("string") }]) },
    ])
    value = JsonValue.object([{ key: "name", value: JsonValue.string("ok") }, { key: "extra", value: JsonValue.number(1.0) }])
    errors = validate(value, schema, schema, [])
    List.len(errors) == 1 && (List.first(errors) |> Result.map_ok(.keyword) |> Result.with_default("")) == "type"

# required with multiple missing fields: should get one error per missing field
expect
    schema = JsonValue.object([
        { key: "type", value: JsonValue.string("object") },
        { key: "required", value: JsonValue.array([JsonValue.string("a"), JsonValue.string("b"), JsonValue.string("c")]) },
    ])
    value = JsonValue.object([])
    errors = validate(value, schema, schema, [])
    List.len(errors) == 3

# null value on non-required property: should be skipped (no error)
expect
    schema = JsonValue.object([
        { key: "type", value: JsonValue.string("object") },
        { key: "properties", value: JsonValue.object([
            { key: "name", value: JsonValue.object([{ key: "type", value: JsonValue.string("string") }]) },
        ]) },
    ])
    value = JsonValue.object([{ key: "name", value: JsonValue.null }])
    validate(value, schema, schema, []) == []

# null value on REQUIRED property: should fail
expect
    schema = JsonValue.object([
        { key: "type", value: JsonValue.string("object") },
        { key: "required", value: JsonValue.array([JsonValue.string("name")]) },
        { key: "properties", value: JsonValue.object([
            { key: "name", value: JsonValue.object([{ key: "type", value: JsonValue.string("string") }]) },
        ]) },
    ])
    value = JsonValue.object([{ key: "name", value: JsonValue.null }])
    errors = validate(value, schema, schema, [])
    List.any(errors, |e| e.keyword == "type")

# RFC 6901: ~1 in a token decodes to /, ~0 to ~
expect
    root = JsonValue.object([
        { key: "paths", value: JsonValue.object([
            { key: "/users/{id}", value: JsonValue.object([
                { key: "weird~key", value: JsonValue.object([{ key: "type", value: JsonValue.string("integer") }]) },
            ]) },
        ]) },
    ])
    schema = JsonValue.object([{ key: "$ref", value: JsonValue.string("#/paths/~1users~1{id}/weird~0key") }])
    validate(JsonValue.number(42.0), schema, root, []) == []

# broken $ref: should not crash, validates against the ref object itself
expect
    root = JsonValue.object([])
    schema = JsonValue.object([{ key: "$ref", value: JsonValue.string("#/nonexistent/path") }])
    validate(JsonValue.string("anything"), schema, root, []) == []

# chained $ref: A refs B, B refs C, C has the actual schema
expect
    root = JsonValue.object([
        { key: "defs", value: JsonValue.object([
            { key: "A", value: JsonValue.object([{ key: "$ref", value: JsonValue.string("#/defs/B") }]) },
            { key: "B", value: JsonValue.object([{ key: "$ref", value: JsonValue.string("#/defs/C") }]) },
            { key: "C", value: JsonValue.object([{ key: "type", value: JsonValue.string("integer") }]) },
        ]) },
    ])
    schema = JsonValue.object([{ key: "$ref", value: JsonValue.string("#/defs/A") }])
    validate(JsonValue.number(42.0), schema, root, []) == []

# circular $ref: should produce an error, not stack overflow
expect
    root = JsonValue.object([
        { key: "defs", value: JsonValue.object([
            { key: "A", value: JsonValue.object([{ key: "$ref", value: JsonValue.string("#/defs/B") }]) },
            { key: "B", value: JsonValue.object([{ key: "$ref", value: JsonValue.string("#/defs/A") }]) },
        ]) },
    ])
    schema = JsonValue.object([{ key: "$ref", value: JsonValue.string("#/defs/A") }])
    errors = validate(JsonValue.string("anything"), schema, root, [])
    List.len(errors) == 1 && (List.first(errors) |> Result.map_ok(.keyword) |> Result.with_default("")) == "$ref"

# items with $ref: validate array elements through a ref
expect
    root = JsonValue.object([
        { key: "components", value: JsonValue.object([
            { key: "schemas", value: JsonValue.object([
                { key: "Num", value: JsonValue.object([{ key: "type", value: JsonValue.string("integer") }]) },
            ]) },
        ]) },
    ])
    schema = JsonValue.object([
        { key: "type", value: JsonValue.string("array") },
        { key: "items", value: JsonValue.object([{ key: "$ref", value: JsonValue.string("#/components/schemas/Num") }]) },
    ])
    value = JsonValue.array([JsonValue.number(1.0), JsonValue.string("not a number")])
    errors = validate(value, schema, root, [])
    List.len(errors) == 1

# minLength with multi-byte characters: counts characters not bytes
expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("string") }, { key: "minLength", value: JsonValue.number(2.0) }])
    # "ö" is 1 character but 2 bytes. Should fail minLength 2
    errors = validate(JsonValue.string("ö"), schema, schema, [])
    List.len(errors) == 1

# email format edge cases
expect
    schema = JsonValue.object([{ key: "format", value: JsonValue.string("email") }])
    validate(JsonValue.string("@domain.com"), schema, schema, []) |> List.len == 1 # empty local part

expect
    schema = JsonValue.object([{ key: "format", value: JsonValue.string("email") }])
    validate(JsonValue.string("user@"), schema, schema, []) |> List.len == 1 # empty domain

expect
    schema = JsonValue.object([{ key: "format", value: JsonValue.string("email") }])
    validate(JsonValue.string("user@domain"), schema, schema, []) |> List.len == 1 # no dot in domain

# ── nullable tests ──

# nullable: true + null value → passes
expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("string") }, { key: "nullable", value: JsonValue.bool(Bool.true) }])
    validate(JsonValue.null, schema, schema, []) == []

# nullable: true + non-null wrong type → still fails
expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("string") }, { key: "nullable", value: JsonValue.bool(Bool.true) }])
    errors = validate(JsonValue.number(42.0), schema, schema, [])
    List.any(errors, |e| e.keyword == "type")

# no nullable + null → type error
expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.string("string") }])
    errors = validate(JsonValue.null, schema, schema, [])
    List.any(errors, |e| e.keyword == "type")

# ── type-as-array tests ──

# ["string", "null"] + string → passes
expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.array([JsonValue.string("string"), JsonValue.string("null")]) }])
    validate(JsonValue.string("hello"), schema, schema, []) == []

# ["string", "null"] + null → passes
expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.array([JsonValue.string("string"), JsonValue.string("null")]) }])
    validate(JsonValue.null, schema, schema, []) == []

# ["string", "null"] + number → fails
expect
    schema = JsonValue.object([{ key: "type", value: JsonValue.array([JsonValue.string("string"), JsonValue.string("null")]) }])
    errors = validate(JsonValue.number(42.0), schema, schema, [])
    List.len(errors) == 1 && (List.first(errors) |> Result.map_ok(.keyword) |> Result.with_default("")) == "type"

# ── anyOf tests ──

# anyOf: matches first schema
expect
    schema = JsonValue.object([{ key: "anyOf", value: JsonValue.array([
        JsonValue.object([{ key: "type", value: JsonValue.string("string") }]),
        JsonValue.object([{ key: "type", value: JsonValue.string("integer") }]),
    ]) }])
    validate(JsonValue.string("hello"), schema, schema, []) == []

# anyOf: matches second schema
expect
    schema = JsonValue.object([{ key: "anyOf", value: JsonValue.array([
        JsonValue.object([{ key: "type", value: JsonValue.string("string") }]),
        JsonValue.object([{ key: "type", value: JsonValue.string("integer") }]),
    ]) }])
    validate(JsonValue.number(42.0), schema, schema, []) == []

# anyOf: matches none
expect
    schema = JsonValue.object([{ key: "anyOf", value: JsonValue.array([
        JsonValue.object([{ key: "type", value: JsonValue.string("string") }]),
        JsonValue.object([{ key: "type", value: JsonValue.string("integer") }]),
    ]) }])
    errors = validate(JsonValue.bool(Bool.true), schema, schema, [])
    List.len(errors) == 1 && (List.first(errors) |> Result.map_ok(.keyword) |> Result.with_default("")) == "anyOf"

# anyOf: with $ref
expect
    root = JsonValue.object([
        { key: "components", value: JsonValue.object([
            { key: "schemas", value: JsonValue.object([
                { key: "Str", value: JsonValue.object([{ key: "type", value: JsonValue.string("string") }]) },
            ]) },
        ]) },
        { key: "anyOf", value: JsonValue.array([
            JsonValue.object([{ key: "$ref", value: JsonValue.string("#/components/schemas/Str") }]),
        ]) },
    ])
    validate(JsonValue.string("hello"), root, root, []) == []

# ── oneOf tests ──

# oneOf: matches exactly one
expect
    schema = JsonValue.object([{ key: "oneOf", value: JsonValue.array([
        JsonValue.object([{ key: "type", value: JsonValue.string("string") }]),
        JsonValue.object([{ key: "type", value: JsonValue.string("integer") }]),
    ]) }])
    validate(JsonValue.string("hello"), schema, schema, []) == []

# oneOf: matches none
expect
    schema = JsonValue.object([{ key: "oneOf", value: JsonValue.array([
        JsonValue.object([{ key: "type", value: JsonValue.string("string") }]),
        JsonValue.object([{ key: "type", value: JsonValue.string("integer") }]),
    ]) }])
    errors = validate(JsonValue.bool(Bool.true), schema, schema, [])
    List.len(errors) == 1 && (List.first(errors) |> Result.map_ok(.keyword) |> Result.with_default("")) == "oneOf"

# oneOf: matches multiple (both accept numbers)
expect
    schema = JsonValue.object([{ key: "oneOf", value: JsonValue.array([
        JsonValue.object([{ key: "type", value: JsonValue.string("number") }]),
        JsonValue.object([{ key: "type", value: JsonValue.string("number") }]),
    ]) }])
    errors = validate(JsonValue.number(1.0), schema, schema, [])
    List.len(errors) == 1 && (List.first(errors) |> Result.map_ok(.keyword) |> Result.with_default("")) == "oneOf"

# oneOf: with discriminator
expect
    root = JsonValue.object([
        { key: "components", value: JsonValue.object([
            { key: "schemas", value: JsonValue.object([
                { key: "Cat", value: JsonValue.object([
                    { key: "type", value: JsonValue.string("object") },
                    { key: "required", value: JsonValue.array([JsonValue.string("kind"), JsonValue.string("purrs")]) },
                    { key: "properties", value: JsonValue.object([
                        { key: "kind", value: JsonValue.object([{ key: "type", value: JsonValue.string("string") }]) },
                        { key: "purrs", value: JsonValue.object([{ key: "type", value: JsonValue.string("boolean") }]) },
                    ]) },
                ]) },
                { key: "Dog", value: JsonValue.object([
                    { key: "type", value: JsonValue.string("object") },
                    { key: "required", value: JsonValue.array([JsonValue.string("kind"), JsonValue.string("barks")]) },
                    { key: "properties", value: JsonValue.object([
                        { key: "kind", value: JsonValue.object([{ key: "type", value: JsonValue.string("string") }]) },
                        { key: "barks", value: JsonValue.object([{ key: "type", value: JsonValue.string("boolean") }]) },
                    ]) },
                ]) },
            ]) },
        ]) },
    ])
    schema = JsonValue.object([
        { key: "oneOf", value: JsonValue.array([
            JsonValue.object([{ key: "$ref", value: JsonValue.string("#/components/schemas/Cat") }]),
            JsonValue.object([{ key: "$ref", value: JsonValue.string("#/components/schemas/Dog") }]),
        ]) },
        { key: "discriminator", value: JsonValue.object([
            { key: "propertyName", value: JsonValue.string("kind") },
            { key: "mapping", value: JsonValue.object([
                { key: "cat", value: JsonValue.string("#/components/schemas/Cat") },
                { key: "dog", value: JsonValue.string("#/components/schemas/Dog") },
            ]) },
        ]) },
    ])
    value = JsonValue.object([
        { key: "kind", value: JsonValue.string("cat") },
        { key: "purrs", value: JsonValue.bool(Bool.true) },
    ])
    validate(value, schema, root, []) == []

# ── allOf tests ──

# allOf: satisfies all
expect
    schema = JsonValue.object([{ key: "allOf", value: JsonValue.array([
        JsonValue.object([{ key: "type", value: JsonValue.string("object") }, { key: "required", value: JsonValue.array([JsonValue.string("name")]) }]),
        JsonValue.object([{ key: "type", value: JsonValue.string("object") }, { key: "required", value: JsonValue.array([JsonValue.string("age")]) }]),
    ]) }])
    value = JsonValue.object([{ key: "name", value: JsonValue.string("Alice") }, { key: "age", value: JsonValue.number(30.0) }])
    validate(value, schema, schema, []) == []

# allOf: fails one sub-schema
expect
    schema = JsonValue.object([{ key: "allOf", value: JsonValue.array([
        JsonValue.object([{ key: "type", value: JsonValue.string("object") }, { key: "required", value: JsonValue.array([JsonValue.string("name")]) }]),
        JsonValue.object([{ key: "type", value: JsonValue.string("object") }, { key: "required", value: JsonValue.array([JsonValue.string("age")]) }]),
    ]) }])
    value = JsonValue.object([{ key: "name", value: JsonValue.string("Alice") }])
    errors = validate(value, schema, schema, [])
    List.any(errors, |e| e.keyword == "required" && Str.contains(e.message, "age"))

# allOf: $ref base + inline extension (schema inheritance)
expect
    root = JsonValue.object([
        { key: "components", value: JsonValue.object([
            { key: "schemas", value: JsonValue.object([
                { key: "Base", value: JsonValue.object([
                    { key: "type", value: JsonValue.string("object") },
                    { key: "required", value: JsonValue.array([JsonValue.string("id")]) },
                    { key: "properties", value: JsonValue.object([
                        { key: "id", value: JsonValue.object([{ key: "type", value: JsonValue.string("integer") }]) },
                    ]) },
                ]) },
            ]) },
        ]) },
    ])
    schema = JsonValue.object([{ key: "allOf", value: JsonValue.array([
        JsonValue.object([{ key: "$ref", value: JsonValue.string("#/components/schemas/Base") }]),
        JsonValue.object([
            { key: "type", value: JsonValue.string("object") },
            { key: "required", value: JsonValue.array([JsonValue.string("name")]) },
            { key: "properties", value: JsonValue.object([
                { key: "name", value: JsonValue.object([{ key: "type", value: JsonValue.string("string") }]) },
            ]) },
        ]),
    ]) }])
    value = JsonValue.object([{ key: "id", value: JsonValue.number(1.0) }, { key: "name", value: JsonValue.string("Alice") }])
    validate(value, schema, root, []) == []

# ── nullable + composition tests ──

# nullable: true + anyOf + null value → passes (Stripe's #1 pattern: anyOf: [{$ref}], nullable: true)
expect
    root = JsonValue.object([
        { key: "components", value: JsonValue.object([
            { key: "schemas", value: JsonValue.object([
                { key: "Profile", value: JsonValue.object([
                    { key: "type", value: JsonValue.string("object") },
                    { key: "required", value: JsonValue.array([JsonValue.string("name")]) },
                ]) },
            ]) },
        ]) },
    ])
    schema = JsonValue.object([
        { key: "anyOf", value: JsonValue.array([
            JsonValue.object([{ key: "$ref", value: JsonValue.string("#/components/schemas/Profile") }]),
        ]) },
        { key: "nullable", value: JsonValue.bool(Bool.true) },
    ])
    validate(JsonValue.null, schema, root, []) == []

# nullable: true + anyOf + non-null value → validated against anyOf sub-schemas
expect
    schema = JsonValue.object([
        { key: "anyOf", value: JsonValue.array([
            JsonValue.object([{ key: "type", value: JsonValue.string("string") }]),
        ]) },
        { key: "nullable", value: JsonValue.bool(Bool.true) },
    ])
    validate(JsonValue.number(42.0), schema, schema, []) |> List.len > 0

# nullable: true + oneOf + null value → passes
expect
    schema = JsonValue.object([
        { key: "oneOf", value: JsonValue.array([
            JsonValue.object([{ key: "type", value: JsonValue.string("string") }]),
            JsonValue.object([{ key: "type", value: JsonValue.string("integer") }]),
        ]) },
        { key: "nullable", value: JsonValue.bool(Bool.true) },
    ])
    validate(JsonValue.null, schema, schema, []) == []

# anyOf with nullable sub-schema (alternative pattern: anyOf: [{type: string}, {type: null}])
expect
    schema = JsonValue.object([
        { key: "anyOf", value: JsonValue.array([
            JsonValue.object([{ key: "type", value: JsonValue.string("string") }]),
            JsonValue.object([{ key: "type", value: JsonValue.string("null") }]),
        ]) },
    ])
    validate(JsonValue.null, schema, schema, []) == []

# oneOf short-circuit: ensure we don't blow up on many sub-schemas
expect
    many_schemas = List.repeat(JsonValue.object([{ key: "type", value: JsonValue.string("number") }]), 20)
    schema = JsonValue.object([{ key: "oneOf", value: JsonValue.array(many_schemas) }])
    # number matches all 20 → error (matches multiple), but should not be slow
    errors = validate(JsonValue.number(1.0), schema, schema, [])
    List.len(errors) == 1 && (List.first(errors) |> Result.map_ok(.keyword) |> Result.with_default("")) == "oneOf"

# ── properties with $ref ──

# Property schema is a $ref: should resolve and validate
expect
    root = JsonValue.object([
        { key: "components", value: JsonValue.object([
            { key: "schemas", value: JsonValue.object([
                { key: "Email", value: JsonValue.object([
                    { key: "type", value: JsonValue.string("string") },
                    { key: "format", value: JsonValue.string("email") },
                ]) },
            ]) },
        ]) },
    ])
    schema = JsonValue.object([
        { key: "type", value: JsonValue.string("object") },
        { key: "properties", value: JsonValue.object([
            { key: "email", value: JsonValue.object([{ key: "$ref", value: JsonValue.string("#/components/schemas/Email") }]) },
        ]) },
    ])
    value = JsonValue.object([{ key: "email", value: JsonValue.string("user@example.com") }])
    validate(value, schema, root, []) == []

# Property $ref: invalid value should error with correct path
expect
    root = JsonValue.object([
        { key: "components", value: JsonValue.object([
            { key: "schemas", value: JsonValue.object([
                { key: "Email", value: JsonValue.object([
                    { key: "type", value: JsonValue.string("string") },
                ]) },
            ]) },
        ]) },
    ])
    schema = JsonValue.object([
        { key: "type", value: JsonValue.string("object") },
        { key: "properties", value: JsonValue.object([
            { key: "email", value: JsonValue.object([{ key: "$ref", value: JsonValue.string("#/components/schemas/Email") }]) },
        ]) },
    ])
    value = JsonValue.object([{ key: "email", value: JsonValue.number(123.0) }])
    errors = validate(value, schema, root, [])
    List.len(errors) == 1
    && (List.first(errors) |> Result.map_ok(.path) |> Result.with_default([])) == ["email"]
    && (List.first(errors) |> Result.map_ok(.keyword) |> Result.with_default("")) == "type"

# ── discriminator failure cases ──

# Discriminator property missing from value
expect
    root = JsonValue.object([
        { key: "components", value: JsonValue.object([
            { key: "schemas", value: JsonValue.object([
                { key: "Cat", value: JsonValue.object([{ key: "type", value: JsonValue.string("object") }]) },
            ]) },
        ]) },
    ])
    schema = JsonValue.object([
        { key: "oneOf", value: JsonValue.array([
            JsonValue.object([{ key: "$ref", value: JsonValue.string("#/components/schemas/Cat") }]),
        ]) },
        { key: "discriminator", value: JsonValue.object([
            { key: "propertyName", value: JsonValue.string("kind") },
        ]) },
    ])
    value = JsonValue.object([{ key: "name", value: JsonValue.string("Whiskers") }])
    errors = validate(value, schema, root, [])
    List.len(errors) == 1 && (List.first(errors) |> Result.map_ok(.keyword) |> Result.with_default("")) == "discriminator"

# Discriminator value doesn't resolve to a schema
expect
    root = JsonValue.object([
        { key: "components", value: JsonValue.object([
            { key: "schemas", value: JsonValue.object([]) },
        ]) },
    ])
    schema = JsonValue.object([
        { key: "oneOf", value: JsonValue.array([
            JsonValue.object([{ key: "type", value: JsonValue.string("object") }]),
        ]) },
        { key: "discriminator", value: JsonValue.object([
            { key: "propertyName", value: JsonValue.string("kind") },
        ]) },
    ])
    value = JsonValue.object([{ key: "kind", value: JsonValue.string("unknown") }])
    errors = validate(value, schema, root, [])
    List.len(errors) == 1 && (List.first(errors) |> Result.map_ok(.keyword) |> Result.with_default("")) == "discriminator"

# ── enum error message includes values ──

expect
    schema = JsonValue.object([{ key: "enum", value: JsonValue.array([JsonValue.string("a"), JsonValue.string("b")]) }])
    errors = validate(JsonValue.string("c"), schema, schema, [])
    msg = List.first(errors) |> Result.map_ok(.message) |> Result.with_default("")
    List.len(errors) == 1 && Str.contains(msg, "\"c\"") && Str.contains(msg, "\"a\"")
