## JSON Schema validation over `JsonValue` trees, for the keywords OpenAPI 3.x
## request bodies use. `OpenApi` finds the schema in a spec and hands it here.
##
## A keyword that is absent, or whose value has the wrong JSON type, puts no
## constraint on the value. The value is validated as a request body, so a
## `readOnly` property is never required.
import JsonValue
import Pattern
import ValidationError

## A document to validate against, such as a spec, with every `pattern` in it
## compiled.
Doc : { root : JsonValue, patterns : Dict(Str, Try(Pattern, [Unsupported(Str)])) }

# Where validation is: the document, the place in the body that errors report,
# and the refs entered at that place.
At : { doc : Doc, path : List(Str), entered : List(Str) }

Schema :: [].{

	## Prepare a document to validate against: a spec, or a schema that is its
	## own root. Every `pattern` in it is compiled here, once, rather than for
	## every value checked against it.
	prepare : JsonValue -> Doc
	prepare = |root| { root, patterns: add_patterns(Dict.empty(), root) }

	## Validate `value` against `schema`, whose `$ref`s point into `doc`.
	validate : JsonValue, JsonValue, Doc -> List(ValidationError)
	validate = |value, schema, doc| validate_at(value, schema, { doc, path: [], entered: [] })

	## Follow a node's `$ref` chain to the node it ends at. A node without a
	## `$ref`, or with one that does not resolve, is returned as it is.
	resolve_ref : JsonValue, JsonValue -> Try(JsonValue, [CircularRef(Str)])
	resolve_ref = |node, root_doc| follow_refs(node, root_doc, []).map_ok(|{ node: target, refs: _ }| target)

	## `schema` and every schema under its `allOf`, `anyOf` and `oneOf`, at
	## any depth, with their `$ref`s followed. Together they say what types a
	## value may have, which is what reading a form, whose fields are all
	## text, needs to know.
	branches : JsonValue, JsonValue -> List(JsonValue)
	branches = |schema, root_doc| collect_branches(schema, root_doc, [])
}

# `entered` holds the refs followed on the way here, so that a schema that
# includes itself is collected once.
collect_branches : JsonValue, JsonValue, List(Str) -> List(JsonValue)
collect_branches = |schema, root_doc, entered|
	match follow_refs(schema, root_doc, []) {
		Ok({ node, refs }) =>
			if refs.any(|ref_str| entered.contains(ref_str)) {
				[]
			} else {
				subs = ["allOf", "anyOf", "oneOf"].join_map(|keyword| typed_field(node, keyword, JsonValue.get_array).ok_or([]))
				[node].concat(subs.join_map(|sub| collect_branches(sub, root_doc, entered.concat(refs))))
			}

		Err(CircularRef(_)) => []
	}

# $ref resolution

# `seen` holds the refs already followed, which is how a cycle is detected. The
# result is the node the chain ends at, and the refs on the way there.
follow_refs : JsonValue, JsonValue, List(Str) -> Try({ node : JsonValue, refs : List(Str) }, [CircularRef(Str)])
follow_refs = |node, root_doc, seen|
	match typed_field(node, "$ref", JsonValue.get_str) {
		Ok(ref_str) =>
			if seen.contains(ref_str) {
				Err(CircularRef(ref_str))
			} else {
				match navigate_ref(root_doc, ref_str) {
					Ok(target) => follow_refs(target, root_doc, seen.append(ref_str))
					Err(_) => Ok({ node, refs: seen })
				}
			}

		Err(_) => Ok({ node, refs: seen })
	}

# Navigate a $ref path like "#/components/schemas/Foo" through the document tree.
navigate_ref : JsonValue, Str -> Try(JsonValue, [RefNotFound(Str)])
navigate_ref = |root_doc, ref_str| {
	var $current = root_doc
	for token in ref_str.drop_prefix("#/").split_on("/") {
		$current = $current.get_field(decode_ref_token(token)) ? |_| RefNotFound(ref_str)
	}
	Ok($current)
}

# RFC 6901: `~1` stands for `/` and `~0` for `~`. `~1` has to go first, so that
# `~01` decodes to `~1` rather than to `/`.
decode_ref_token : Str -> Str
decode_ref_token = |token|
	token.replace_each("~1", "/").replace_each("~0", "~")

# Patterns

# Every `pattern` in a node, compiled and added to `compiled`. A field named
# "pattern" outside of a schema, such as in an example, is compiled too, which
# costs a little time and nothing else.
add_patterns : Dict(Str, Try(Pattern, [Unsupported(Str)])), JsonValue -> Dict(Str, Try(Pattern, [Unsupported(Str)]))
add_patterns = |compiled, node|
	match (node.get_object(), node.get_array()) {
		(Ok(fields), _) => fields.fold(compiled, |so_far, { key, value }| add_patterns(add_pattern(so_far, key, value), value))
		(_, Ok(items)) => items.fold(compiled, add_patterns)
		_ => compiled
	}

add_pattern : Dict(Str, Try(Pattern, [Unsupported(Str)])), Str, JsonValue -> Dict(Str, Try(Pattern, [Unsupported(Str)]))
add_pattern = |compiled, key, value|
	match value.get_str() {
		Ok(source) if key == "pattern" and !compiled.contains(source) => compiled.insert(source, Pattern.parse(source))
		_ => compiled
	}

# Field lookup

# A field of a JSON object, read with one of the `JsonValue.get_*` accessors.
# `Missing` covers a field that is absent and one of another JSON type.
typed_field : JsonValue, Str, (JsonValue -> Try(a, err)) -> Try(a, [Missing])
typed_field = |object, name, get| {
	value = object.get_field(name) ? |_| Missing
	get(value).map_err(|_| Missing)
}

# The names a schema lists under "required".
required_names : JsonValue -> List(Str)
required_names = |schema|
	typed_field(schema, "required", JsonValue.get_array).ok_or([]).keep_oks(JsonValue.get_str)

# The schemas a schema lists under "properties".
property_schemas : JsonValue -> Try(List({ key : Str, value : JsonValue }), [NotAnObject])
property_schemas = |schema|
	match schema.get_field("properties") {
		Ok(properties) => properties.get_object()
		Err(_) => Ok([])
	}

# The schema a schema gives its property `name` under "properties".
property_schema : JsonValue, Str -> Try(JsonValue, [NotFound])
property_schema = |schema, name| {
	properties = schema.get_field("properties") ? |_| NotFound
	properties.get_field(name).map_err(|_| NotFound)
}

# A value as error messages show it, which is as JSON.
describe : JsonValue -> Str
describe = |value| value.to_str()

# Validation

validate_at : JsonValue, JsonValue, At -> List(ValidationError)
validate_at = |value, schema, at| validate_via(value, schema, [], at)

# Validate against `schema`, reached through the refs in `refs`, and through
# its own `$ref` chain. A ref already entered at this place in the body is
# being checked there already. Meeting it again, as when a base schema lists
# its subtypes under `oneOf` and each subtype includes the base with `allOf`,
# adds nothing, rather than going round forever.
validate_via : JsonValue, JsonValue, List(Str), At -> List(ValidationError)
validate_via = |value, schema, refs, at|
	match follow_refs(schema, at.doc.root, refs) {
		Ok({ node, refs: chain }) =>
			if chain.any(|ref_str| at.entered.contains(ref_str)) {
				[]
			} else {
				validate_resolved(value, node, { ..at, entered: at.entered.concat(chain) })
			}

		Err(CircularRef(ref_str)) => [{ path: at.path, message: "Circular $ref: ${ref_str}", keyword: "$ref" }]
	}

# The place in the body one field or item further in, where no ref has been
# entered yet.
deeper : At, Str -> At
deeper = |at, segment| { ..at, path: at.path.append(segment), entered: [] }

validate_resolved : JsonValue, JsonValue, At -> List(ValidationError)
validate_resolved = |value, schema, at| {
	nullable = typed_field(schema, "nullable", JsonValue.get_bool).ok_or(Bool.False)

	# OpenAPI 3.0: a null value satisfies a nullable schema whatever else it says.
	if nullable and value.is_null() {
		[]
	} else {
		path = at.path
		List.join([
			validate_type(value, schema, path),
			validate_required(value, schema, at),
			validate_properties(value, schema, at),
			validate_items(value, schema, at),
			validate_sizes(value, schema, path),
			validate_range(value, schema, path),
			validate_multiple_of(value, schema, path),
			validate_enum(value, schema, path),
			validate_const(value, schema, path),
			validate_unique_items(value, schema, path),
			validate_additional_properties(value, schema, at),
			validate_pattern(value, schema, at),
			validate_format(value, schema, path),
			validate_any_of(value, schema, at),
			validate_one_of(value, schema, at),
			validate_all_of(value, schema, at),
			validate_not(value, schema, at),
		])
	}
}

# type

# "type" is a single name, or a list of names such as ["string", "null"].
validate_type : JsonValue, JsonValue, List(Str) -> List(ValidationError)
validate_type = |value, schema, path| {
	actual = value.type_name()

	match (typed_field(schema, "type", JsonValue.get_str), typed_field(schema, "type", JsonValue.get_array)) {
		(Ok(expected), _) =>
			if type_matches(value, expected, actual) {
				[]
			} else {
				[{ path, message: "Expected type ${expected} but got ${actual}", keyword: "type" }]
			}

		(_, Ok(type_list)) => {
			expected = type_list.keep_oks(JsonValue.get_str)
			if expected.any(|name| type_matches(value, name, actual)) {
				[]
			} else {
				[{ path, message: "Expected one of types [${Str.join_with(expected, ", ")}] but got ${actual}", keyword: "type" }]
			}
		}

		_ => []
	}
}

# JSON has no integer type, so "integer" is a number without a fraction.
type_matches : JsonValue, Str, Str -> Bool
type_matches = |value, expected, actual|
	if expected == "integer" {
		value.get_number().map_ok(is_integer).ok_or(Bool.False)
	} else {
		actual == expected
	}

is_integer : F64 -> Bool
is_integer = |n|
	match n.floor_to_i128_try() {
		Ok(floored) => F64.is_float_eq(n, floored.to_f64())
		Err(_) => Bool.False
	}

# required

# A `readOnly` property is one the server sets, and OpenAPI only requires it in
# responses, so a request body may leave it out.
validate_required : JsonValue, JsonValue, At -> List(ValidationError)
validate_required = |value, schema, at|
	match value.get_object() {
		Ok(fields) => {
			present = fields.map(|field| field.key)
			required_names(schema)
				.drop_if(|name| present.contains(name) or is_read_only(schema, name, at.doc.root))
				.map(|name| { path: at.path, message: "Missing required field: ${name}", keyword: "required" })
		}

		Err(_) => []
	}

# Whether the property `name` is marked `readOnly`, next to its `$ref` or in
# the schema the `$ref` points to.
is_read_only : JsonValue, Str, JsonValue -> Bool
is_read_only = |schema, name, root_doc|
	match property_schema(schema, name) {
		Ok(property) => {
			marked = |node| typed_field(node, "readOnly", JsonValue.get_bool) == Ok(Bool.True)
			marked(property) or marked(Schema.resolve_ref(property, root_doc).ok_or(property))
		}

		Err(_) => Bool.False
	}

# properties

validate_properties : JsonValue, JsonValue, At -> List(ValidationError)
validate_properties = |value, schema, at|
	match (value.get_object(), typed_field(schema, "properties", JsonValue.get_object)) {
		(Ok(fields), Ok(schemas)) => {
			required = required_names(schema)

			fields.join_map(
				|{ key, value: field_value }|
					match schemas.find_first(|property| property.key == key) {
						# A null on a field that is not required counts as absent.
						# Many APIs (SendGrid among them) accept null for optional
						# fields even when the schema does not list "null" as a type.
						Ok(_) if field_value.is_null() and !required.contains(key) => []
						Ok(property) => validate_at(field_value, property.value, deeper(at, key))
						Err(_) => []
					},
			)
		}

		_ => []
	}

# items

validate_items : JsonValue, JsonValue, At -> List(ValidationError)
validate_items = |value, schema, at|
	match (value.get_array(), schema.get_field("items")) {
		(Ok(items), Ok(items_schema)) =>
			List.join(
				items.map_with_index(
					|item, index| validate_at(item, items_schema, deeper(at, index.to_str())),
				),
			)

		_ => []
	}

# minLength, maxLength, minItems, maxItems, minProperties, maxProperties

# The length of a string counts Unicode scalar values, not bytes.
validate_sizes : JsonValue, JsonValue, List(Str) -> List(ValidationError)
validate_sizes = |value, schema, path| {
	length = value.get_str().map_ok(|str| count_scalar_values(str).to_f64())
	item_count = value.get_array().map_ok(|items| items.len().to_f64())
	property_count = value.get_object().map_ok(|fields| fields.len().to_f64())

	List.join([
		check_limit(schema, "minLength", length, F64.is_lt, |actual, limit| "String length ${actual} is less than minLength ${limit}", path),
		check_limit(schema, "maxLength", length, F64.is_gt, |actual, limit| "String length ${actual} exceeds maxLength ${limit}", path),
		check_limit(schema, "minItems", item_count, F64.is_lt, |actual, limit| "Array has ${actual} items, fewer than minItems ${limit}", path),
		check_limit(schema, "maxItems", item_count, F64.is_gt, |actual, limit| "Array has ${actual} items, exceeds maxItems ${limit}", path),
		check_limit(schema, "minProperties", property_count, F64.is_lt, |actual, limit| "Object has ${actual} properties, fewer than minProperties ${limit}", path),
		check_limit(schema, "maxProperties", property_count, F64.is_gt, |actual, limit| "Object has ${actual} properties, exceeds maxProperties ${limit}", path),
	])
}

# Every byte of UTF-8 starts a scalar value, except a continuation byte.
count_scalar_values : Str -> U64
count_scalar_values = |str|
	str.to_utf8().count_if(|byte| byte.bitwise_and(0xC0) != 0x80)

# One error when `measured` breaks the limit that `keyword` sets, which is when
# `breaks(measured, limit)`. `measured` is an `Err` for a value the keyword
# does not apply to, such as an array for minLength.
check_limit : JsonValue, Str, Try(F64, err), (F64, F64 -> Bool), (Str, Str -> Str), List(Str) -> List(ValidationError)
check_limit = |schema, keyword, measured, breaks, message, path|
	match (measured, typed_field(schema, keyword, JsonValue.get_number)) {
		(Ok(actual), Ok(limit)) if breaks(actual, limit) => [{ path, message: message(actual.to_str(), limit.to_str()), keyword }]
		_ => []
	}

# minimum, maximum, exclusiveMinimum, exclusiveMaximum

# OpenAPI 3.0 makes "minimum" and "maximum" exclusive with a boolean
# "exclusiveMinimum" or "exclusiveMaximum" next to them. OpenAPI 3.1 gives
# those two a number of their own instead.
validate_range : JsonValue, JsonValue, List(Str) -> List(ValidationError)
validate_range = |value, schema, path| {
	number = value.get_number()
	exclusive = |keyword| typed_field(schema, keyword, JsonValue.get_bool).ok_or(Bool.False)

	List.join([
		if exclusive("exclusiveMinimum") {
			check_limit(schema, "minimum", number, F64.is_lte, |actual, limit| "Value ${actual} is not greater than exclusive minimum ${limit}", path)
		} else {
			check_limit(schema, "minimum", number, F64.is_lt, |actual, limit| "Value ${actual} is less than minimum ${limit}", path)
		},
		if exclusive("exclusiveMaximum") {
			check_limit(schema, "maximum", number, F64.is_gte, |actual, limit| "Value ${actual} is not less than exclusive maximum ${limit}", path)
		} else {
			check_limit(schema, "maximum", number, F64.is_gt, |actual, limit| "Value ${actual} exceeds maximum ${limit}", path)
		},
		check_limit(schema, "exclusiveMinimum", number, F64.is_lte, |actual, limit| "Value ${actual} is not greater than exclusiveMinimum ${limit}", path),
		check_limit(schema, "exclusiveMaximum", number, F64.is_gte, |actual, limit| "Value ${actual} is not less than exclusiveMaximum ${limit}", path),
	])
}

# multipleOf

validate_multiple_of : JsonValue, JsonValue, List(Str) -> List(ValidationError)
validate_multiple_of = |value, schema, path|
	match (value.get_number(), typed_field(schema, "multipleOf", JsonValue.get_number)) {
		(Ok(number), Ok(divisor)) if divisor > 0 and !is_multiple(number, divisor) =>
			[{ path, message: "Value ${number.to_str()} is not a multiple of ${divisor.to_str()}", keyword: "multipleOf" }]

		_ => []
	}

# Floats do not divide exactly, 0.3 / 0.1 is 2.9999999999999996, so a quotient
# this close to a whole number counts as one.
is_multiple : F64, F64 -> Bool
is_multiple = |number, divisor| {
	quotient = number / divisor
	match quotient.round_to_i128_try() {
		Ok(whole) => F64.is_approx_eq(quotient, whole.to_f64(), { rel: 1e-9, abs: 1e-9 })
		# Every float this far from zero is a whole number.
		Err(OutOfRange) => Bool.True
	}
}

# enum

validate_enum : JsonValue, JsonValue, List(Str) -> List(ValidationError)
validate_enum = |value, schema, path|
	match typed_field(schema, "enum", JsonValue.get_array) {
		Ok(allowed) =>
			if allowed.contains(value) {
				[]
			} else {
				allowed_list = Str.join_with(allowed.map(describe), ", ")
				[{ path, message: "Value ${describe(value)} not in enum [${allowed_list}]", keyword: "enum" }]
			}

		Err(_) => []
	}

# const

validate_const : JsonValue, JsonValue, List(Str) -> List(ValidationError)
validate_const = |value, schema, path|
	match schema.get_field("const") {
		Ok(expected) if value != expected => [{ path, message: "Value ${describe(value)} is not the const ${describe(expected)}", keyword: "const" }]
		_ => []
	}

# uniqueItems

validate_unique_items : JsonValue, JsonValue, List(Str) -> List(ValidationError)
validate_unique_items = |value, schema, path|
	match (value.get_array(), typed_field(schema, "uniqueItems", JsonValue.get_bool)) {
		(Ok(items), Ok(unique)) =>
			if unique and has_duplicates(items) {
				[{ path, message: "Array contains duplicate items", keyword: "uniqueItems" }]
			} else {
				[]
			}

		_ => []
	}

has_duplicates : List(JsonValue) -> Bool
has_duplicates = |items|
	match items {
		[first, .. as rest] =>
			if rest.contains(first) {
				Bool.True
			} else {
				has_duplicates(rest)
			}

		[] => Bool.False
	}

# additionalProperties

# Either `false`, which forbids fields "properties" does not name, or a schema
# those fields have to match.
validate_additional_properties : JsonValue, JsonValue, At -> List(ValidationError)
validate_additional_properties = |value, schema, at|
	match (value.get_object(), property_schemas(schema), schema.get_field("additionalProperties")) {
		(Ok(fields), Ok(schemas), Ok(additional)) => {
			named = schemas.map(|property| property.key)
			extra = fields.drop_if(|field| named.contains(field.key))

			match additional.get_bool() {
				Ok(allowed) =>
					if allowed {
						[]
					} else {
						extra.map(
							|field| { path: at.path.append(field.key), message: "Additional property '${field.key}' not allowed", keyword: "additionalProperties" },
						)
					}

				Err(_) => extra.join_map(|field| validate_at(field.value, additional, deeper(at, field.key)))
			}
		}

		_ => []
	}

# pattern

# A pattern that uses what `Pattern` does not support is not checked. One
# whose search gives up on this string fails, since the string might not
# match.
validate_pattern : JsonValue, JsonValue, At -> List(ValidationError)
validate_pattern = |value, schema, at|
	match (value.get_str(), typed_field(schema, "pattern", JsonValue.get_str)) {
		(Ok(str), Ok(source)) => {
			# Only a schema from outside the document, as in a test, has a
			# pattern that `prepare` did not compile.
			compiled =
				match at.doc.patterns.get(source) {
					Ok(found) => found
					Err(KeyNotFound) => Pattern.parse(source)
				}

			match compiled {
				Ok(pattern) =>
					match pattern.matches(str) {
						Ok(matched) => if matched [] else [{ path: at.path, message: "Value ${describe(value)} does not match pattern ${source}", keyword: "pattern" }]
						Err(TooComplex) => [{ path: at.path, message: "Value takes too long to match against pattern ${source}", keyword: "pattern" }]
					}

				Err(Unsupported(_)) => []
			}
		}

		_ => []
	}

# format

# Only "email" is checked. Every other format passes.
validate_format : JsonValue, JsonValue, List(Str) -> List(ValidationError)
validate_format = |value, schema, path|
	match (value.get_str(), typed_field(schema, "format", JsonValue.get_str)) {
		(Ok(str), Ok("email")) =>
			if is_valid_email(str) {
				[]
			} else {
				[{ path, message: "Invalid email format: ${str}", keyword: "format" }]
			}

		_ => []
	}

# Lax on purpose: something before the @, and a dot somewhere after it.
is_valid_email : Str -> Bool
is_valid_email = |str|
	match str.split_first("@") {
		Ok({ before, after }) => !before.is_empty() and !after.is_empty() and after.contains(".")
		Err(_) => Bool.False
	}

# anyOf and oneOf

# How the value fares against each schema in turn, until `enough` of them
# match: the numbers, counted from 1, of those it matches, and the errors of
# those it does not.
try_each : List(JsonValue), JsonValue, At, U64 -> { matched : List(U64), failed : List({ number : U64, errors : List(ValidationError) }) }
try_each = |sub_schemas, value, at, enough|
	sub_schemas
		.map_with_index(|sub, index| { sub, number: index + 1 })
		.fold_until(
			{ matched: [], failed: [] },
			|tried, { sub, number }|
				if tried.matched.len() >= enough {
					Break(tried)
				} else {
					match validate_at(value, sub, at) {
						[] => Continue({ ..tried, matched: tried.matched.append(number) })
						errors => Continue({ ..tried, failed: tried.failed.append({ number, errors }) })
					}
				},
		)

# The message for a value that matches none of the schemas under `keyword`,
# with why it fails each one.
no_match : Str, List({ number : U64, errors : List(ValidationError) }) -> Str
no_match = |keyword, failed| {
	reasons = failed.map(|{ number, errors }| ". Schema ${number.to_str()}: ${Str.join_with(errors.map(ValidationError.to_str), ", ")}")
	"Value does not match any of the ${failed.len().to_str()} schemas in ${keyword}${Str.join_with(reasons, "")}"
}

validate_any_of : JsonValue, JsonValue, At -> List(ValidationError)
validate_any_of = |value, schema, at|
	match typed_field(schema, "anyOf", JsonValue.get_array) {
		Ok(sub_schemas) => {
			tried = try_each(sub_schemas, value, at, 1)
			if tried.matched.is_empty() {
				[{ path: at.path, message: no_match("anyOf", tried.failed), keyword: "anyOf" }]
			} else {
				[]
			}
		}

		Err(_) => []
	}

# A discriminator names the one sub-schema to validate against, so the others
# are never tried. Without one, trying stops at the second match, since that
# already fails oneOf.
validate_one_of : JsonValue, JsonValue, At -> List(ValidationError)
validate_one_of = |value, schema, at|
	match typed_field(schema, "oneOf", JsonValue.get_array) {
		Ok(sub_schemas) =>
			match discriminated_schema(schema, value, at.doc.root) {
				Ok({ target, ref_str }) => validate_via(value, target, [ref_str], at)
				Err(NoDiscriminator) => {
					tried = try_each(sub_schemas, value, at, 2)
					match tried.matched {
						[_] => []
						[first, second, ..] => [{ path: at.path, message: "Value matches schemas ${first.to_str()} and ${second.to_str()} in oneOf, expected exactly 1", keyword: "oneOf" }]
						[] => [{ path: at.path, message: no_match("oneOf", tried.failed), keyword: "oneOf" }]
					}
				}

				Err(DiscriminatorFailed(message)) => [{ path: at.path, message, keyword: "discriminator" }]
			}

		Err(_) => []
	}

# The schema the discriminator picks for this value, and the ref to it. The
# value's `propertyName` field is looked up in "mapping", which gives a $ref or
# the name of a schema under #/components/schemas. A value it does not list
# names a schema there itself.
discriminated_schema : JsonValue, JsonValue, JsonValue -> Try({ target : JsonValue, ref_str : Str }, [NoDiscriminator, DiscriminatorFailed(Str)])
discriminated_schema = |schema, value, root_doc| {
	discriminator = schema.get_field("discriminator") ? |_| NoDiscriminator
	property_name = typed_field(discriminator, "propertyName", JsonValue.get_str) ? |_| NoDiscriminator

	tag = typed_field(value, property_name, JsonValue.get_str) ? |_| DiscriminatorFailed("Missing or non-string discriminator property '${property_name}'")

	name_or_ref =
		match discriminator.get_field("mapping") {
			Ok(mapping) => typed_field(mapping, tag, JsonValue.get_str).ok_or(tag)
			Err(_) => tag
		}
	ref_str = if name_or_ref.starts_with("#") name_or_ref else "#${JsonValue.pointer(["components", "schemas", name_or_ref])}"

	target = navigate_ref(root_doc, ref_str) ? |_| DiscriminatorFailed("Discriminator value '${tag}' does not resolve via ${ref_str}")
	Ok({ target, ref_str })
}

# allOf

validate_all_of : JsonValue, JsonValue, At -> List(ValidationError)
validate_all_of = |value, schema, at|
	match typed_field(schema, "allOf", JsonValue.get_array) {
		Ok(sub_schemas) => sub_schemas.join_map(|sub| validate_at(value, sub, at))
		Err(_) => []
	}

# not

validate_not : JsonValue, JsonValue, At -> List(ValidationError)
validate_not = |value, schema, at|
	match schema.get_field("not") {
		Ok(forbidden) if validate_at(value, forbidden, at).is_empty() =>
			[{ path: at.path, message: "Value matches the schema in not", keyword: "not" }]

		_ => []
	}

# Tests

# The tests spell values, schemas and documents as JSON text, the way a spec
# or a payload holds them.
json : Str -> JsonValue
json = |text|
	match JsonValue.parse(text.to_utf8()) {
		Ok(value) => value
		Err(InvalidJson(message)) => {
			crash "A test's JSON does not parse: ${message}"
		}
	}

# Validate a value against a schema that is its own root document.
check : Str, Str -> List(ValidationError)
check = |value, schema| check_in(value, schema, schema)

# Validate a value against a schema whose `$ref`s point into `root_doc`.
check_in : Str, Str, Str -> List(ValidationError)
check_in = |value, schema, root_doc|
	Schema.validate(json(value), json(schema), Schema.prepare(json(root_doc)))

failed_keywords : List(ValidationError) -> List(Str)
failed_keywords = |errors| errors.map(|error| error.keyword)

messages : List(ValidationError) -> List(Str)
messages = |errors| errors.map(|error| error.message)

# type

string_schema : Str
string_schema =
	\\{ "type": "string" }

integer_schema : Str
integer_schema =
	\\{ "type": "integer" }

expect check("\"hello\"", string_schema) == []

expect failed_keywords(check("42", string_schema)) == ["type"]

expect check("42", integer_schema) == []

expect check("42.0", integer_schema) == []

expect failed_keywords(check("3.14", integer_schema)) == ["type"]

expect check("{}", "{ \"type\": \"object\" }") == []

expect check("[]", "{ \"type\": \"array\" }") == []

expect check("true", "{ \"type\": \"boolean\" }") == []

expect check("1.5", "{ \"type\": \"number\" }") == []

# type as a list of names

string_or_null_schema : Str
string_or_null_schema =
	\\{ "type": ["string", "null"] }

expect check("\"hello\"", string_or_null_schema) == []

expect check("null", string_or_null_schema) == []

expect failed_keywords(check("42", string_or_null_schema)) == ["type"]

# required

name_and_email_required : Str
name_and_email_required =
	\\{ "type": "object", "required": ["name", "email"] }

expect {
	value =
		\\{ "name": "Alice", "email": "a@b.com" }
	check(value, name_and_email_required) == []
}

expect {
	value =
		\\{ "name": "Alice" }
	messages(check(value, name_and_email_required)) == ["Missing required field: email"]
}

# One error per missing field.
expect failed_keywords(check("{}", name_and_email_required)) == ["required", "required"]

# readOnly

# The server sets "id", so a request may leave it out even though it is
# required.
read_only_doc : Str
read_only_doc =
	\\{
	\\  "type": "object",
	\\  "required": ["id", "name"],
	\\  "properties": {
	\\    "id": { "type": "integer", "readOnly": true },
	\\    "name": { "type": "string" },
	\\    "created": { "$ref": "#/components/schemas/Timestamp" }
	\\  },
	\\  "components": { "schemas": { "Timestamp": { "type": "string", "readOnly": true } } }
	\\}

expect check("{ \"name\": \"Alice\" }", read_only_doc) == []

expect messages(check("{ \"id\": 1 }", read_only_doc)) == ["Missing required field: name"]

# readOnly in the schema a $ref points to.
expect {
	schema =
		\\{ "type": "object", "required": ["created"], "properties": { "created": { "$ref": "#/components/schemas/Timestamp" } } }
	check_in("{}", schema, read_only_doc) == []
}

# properties

name_property_schema : Str
name_property_schema =
	\\{
	\\  "type": "object",
	\\  "properties": { "name": { "type": "string" } }
	\\}

# Errors carry the path to the field.
expect {
	errors = check("{ \"name\": 42 }", name_property_schema)
	errors.map(|error| error.path) == [["name"]] and failed_keywords(errors) == ["type"]
}

# A null on a field that is not required counts as absent.
expect check("{ \"name\": null }", name_property_schema) == []

# A null on a required field is validated like any other value.
expect {
	schema =
		\\{
		\\  "type": "object",
		\\  "required": ["name"],
		\\  "properties": { "name": { "type": "string" } }
		\\}
	failed_keywords(check("{ \"name\": null }", schema)) == ["type"]
}

expect {
	schema =
		\\{
		\\  "type": "object",
		\\  "required": ["email"],
		\\  "properties": { "email": { "type": "string", "format": "email" } }
		\\}
	check("{ \"email\": \"a@b.com\" }", schema) == []
}

# items

expect {
	schema =
		\\{ "type": "array", "items": { "type": "string" } }
	errors = check("[\"a\", 1]", schema)
	errors.map(|error| error.path) == [["1"]] and failed_keywords(errors) == ["type"]
}

# minLength, maxLength

min_length_schema : Str
min_length_schema =
	\\{ "type": "string", "minLength": 2 }

max_length_schema : Str
max_length_schema =
	\\{ "type": "string", "maxLength": 3 }

expect check("\"ab\"", min_length_schema) == []

expect messages(check("\"a\"", min_length_schema)) == ["String length 1 is less than minLength 2"]

# "ö" is one character in two bytes, and it is characters that count.
expect failed_keywords(check("\"ö\"", min_length_schema)) == ["minLength"]

expect check("\"abc\"", max_length_schema) == []

expect check("\"åäö\"", max_length_schema) == []

expect messages(check("\"abcd\"", max_length_schema)) == ["String length 4 exceeds maxLength 3"]

# A length limit says nothing about a value that is not a string.
expect check("[1, 2, 3, 4]", "{ \"maxLength\": 3 }") == []

# minItems, maxItems

expect check("[null, null]", "{ \"type\": \"array\", \"minItems\": 2, \"maxItems\": 2 }") == []

expect messages(check("[null]", "{ \"type\": \"array\", \"minItems\": 2 }")) == ["Array has 1 items, fewer than minItems 2"]

expect failed_keywords(check("[null, null]", "{ \"type\": \"array\", \"maxItems\": 1 }")) == ["maxItems"]

# minProperties, maxProperties

expect check("{ \"a\": 1 }", "{ \"type\": \"object\", \"minProperties\": 1, \"maxProperties\": 2 }") == []

expect failed_keywords(check("{}", "{ \"type\": \"object\", \"minProperties\": 1 }")) == ["minProperties"]

expect failed_keywords(check("{ \"a\": 1, \"b\": 2 }", "{ \"type\": \"object\", \"maxProperties\": 1 }")) == ["maxProperties"]

# minimum, maximum

range_schema : Str
range_schema =
	\\{ "type": "integer", "minimum": 1, "maximum": 12 }

expect check("1", range_schema) == [] and check("12", range_schema) == []

expect messages(check("0", range_schema)) == ["Value 0 is less than minimum 1"]

expect messages(check("13", range_schema)) == ["Value 13 exceeds maximum 12"]

expect check("0.0043", "{ \"minimum\": 0.0043, \"maximum\": 0.15 }") == []

expect failed_keywords(check("0.2", "{ \"minimum\": 0.0043, \"maximum\": 0.15 }")) == ["maximum"]

# A range says nothing about a value that is not a number.
expect failed_keywords(check("\"0\"", range_schema)) == ["type"]

# OpenAPI 3.0: a boolean makes the limit next to it exclusive.
exclusive_by_flag : Str
exclusive_by_flag =
	\\{ "minimum": 1, "exclusiveMinimum": true, "maximum": 12, "exclusiveMaximum": true }

expect check("2", exclusive_by_flag) == []

expect messages(check("1", exclusive_by_flag)) == ["Value 1 is not greater than exclusive minimum 1"]

expect messages(check("12", exclusive_by_flag)) == ["Value 12 is not less than exclusive maximum 12"]

# A false one leaves it inclusive.
expect check("1", "{ \"minimum\": 1, \"exclusiveMinimum\": false }") == []

# OpenAPI 3.1: the exclusive limits are numbers of their own.
exclusive_by_number : Str
exclusive_by_number =
	\\{ "exclusiveMinimum": 1, "exclusiveMaximum": 12 }

expect check("2", exclusive_by_number) == []

expect failed_keywords(check("1", exclusive_by_number)) == ["exclusiveMinimum"]

expect failed_keywords(check("12", exclusive_by_number)) == ["exclusiveMaximum"]

# multipleOf

expect check("10", "{ \"multipleOf\": 5 }") == []

expect messages(check("7", "{ \"multipleOf\": 5 }")) == ["Value 7 is not a multiple of 5"]

# 0.3 / 0.1 is 2.9999999999999996 in floats, which still counts.
expect check("0.3", "{ \"multipleOf\": 0.1 }") == []

expect failed_keywords(check("0.35", "{ \"multipleOf\": 0.1 }")) == ["multipleOf"]

expect check("1e300", "{ \"multipleOf\": 3 }") == []

# A divisor that is not positive constrains nothing.
expect check("7", "{ \"multipleOf\": 0 }") == []

# enum

enum_schema : Str
enum_schema =
	\\{ "enum": ["a", "b", 1] }

expect check("\"a\"", enum_schema) == []

expect check("1", enum_schema) == []

# The message shows the value and what was allowed as JSON.
expect messages(check("\"c\"", enum_schema)) == ["Value \"c\" not in enum [\"a\", \"b\", 1]"]

expect messages(check("{ \"x\": [true, null] }", enum_schema)) == ["Value {\"x\":[true,null]} not in enum [\"a\", \"b\", 1]"]

# An object is in the enum with its fields in any order.
expect check("{ \"b\": 2, \"a\": 1 }", "{ \"enum\": [{ \"a\": 1, \"b\": 2 }] }") == []

# const

expect check("\"v2\"", "{ \"const\": \"v2\" }") == []

expect messages(check("\"v1\"", "{ \"const\": \"v2\" }")) == ["Value \"v1\" is not the const \"v2\""]

expect check("null", "{ \"const\": null }") == []

expect failed_keywords(check("{ \"a\": 2 }", "{ \"const\": { \"a\": 1 } }")) == ["const"]

expect check("{ \"b\": [2], \"a\": 1 }", "{ \"const\": { \"a\": 1, \"b\": [2] } }") == []

# uniqueItems

expect check("[1, 2]", "{ \"uniqueItems\": true }") == []

expect failed_keywords(check("[1, 2, 1]", "{ \"uniqueItems\": true }")) == ["uniqueItems"]

expect check("[1, 1]", "{ \"uniqueItems\": false }") == []

# Objects with the same fields in another order are duplicates.
expect failed_keywords(check("[{ \"a\": 1, \"b\": 2 }, { \"b\": 2, \"a\": 1 }]", "{ \"uniqueItems\": true }")) == ["uniqueItems"]

# additionalProperties

no_additional_schema : Str
no_additional_schema =
	\\{
	\\  "type": "object",
	\\  "properties": { "name": {} },
	\\  "additionalProperties": false
	\\}

expect check("{ \"name\": \"ok\" }", no_additional_schema) == []

expect {
	errors = check("{ \"name\": \"ok\", \"extra\": 1 }", no_additional_schema)
	errors.map(|error| error.path) == [["extra"]] and failed_keywords(errors) == ["additionalProperties"]
}

# As a schema, it is what the fields "properties" does not name have to match.
expect {
	schema =
		\\{
		\\  "type": "object",
		\\  "properties": { "name": {} },
		\\  "additionalProperties": { "type": "string" }
		\\}
	errors = check("{ \"name\": 1, \"extra\": 1, \"other\": \"ok\" }", schema)
	errors.map(|error| error.path) == [["extra"]] and failed_keywords(errors) == ["type"]
}

# pattern

sid_schema : Str
sid_schema =
	\\{ "type": "string", "pattern": "^AC[0-9a-fA-F]{32}$" }

expect check("\"AC${Str.repeat("0", 32)}\"", sid_schema) == []

expect messages(check("\"AC123\"", sid_schema)) == ["Value \"AC123\" does not match pattern ^AC[0-9a-fA-F]{32}$"]

# A pattern matches anywhere in the string unless it is anchored.
expect check("\"see github.com\"", "{ \"pattern\": \"github\" }") == []

# A pattern that uses what is not supported is not checked.
expect check("\"b\"", "{ \"pattern\": \"(?=a)\" }") == []

# A pattern says nothing about a value that is not a string.
expect check("42", "{ \"pattern\": \"^a$\" }") == []

# A search that gives up fails the value, since it might not match.
expect messages(check("\"${Str.repeat("a", 40)}\"", "{ \"pattern\": \"^(a+)+b$\" }")) == ["Value takes too long to match against pattern ^(a+)+b$"]

# Every pattern in a document is compiled once, when it is prepared, wherever
# it is.
expect {
	doc = Schema.prepare(json("{ \"a\": { \"pattern\": \"^x$\" }, \"b\": [{ \"pattern\": \"^x$\" }, { \"pattern\": \"(?=y)\" }], \"pattern\": {} }"))
	doc.patterns.keys().len() == 2 and doc.patterns.get("^x$").is_ok() and doc.patterns.get("(?=y)").map_ok(|compiled| compiled.is_err()) == Ok(Bool.True)
}

# format

email_schema : Str
email_schema =
	\\{ "format": "email" }

expect check("\"user@example.com\"", email_schema) == []

expect failed_keywords(check("\"not-an-email\"", email_schema)) == ["format"]

# Nothing before the @.
expect failed_keywords(check("\"@domain.com\"", email_schema)) == ["format"]

# Nothing after the @.
expect failed_keywords(check("\"user@\"", email_schema)) == ["format"]

# No dot after the @.
expect failed_keywords(check("\"user@domain\"", email_schema)) == ["format"]

# Formats other than email are not checked.
expect check("\"not a date\"", "{ \"format\": \"date-time\" }") == []

# No constraints means anything passes.
expect check("\"anything\"", "{}") == []

# $ref

components_doc : Str
components_doc =
	\\{
	\\  "components": {
	\\    "schemas": {
	\\      "Name": { "type": "string" },
	\\      "Count": { "type": "integer" },
	\\      "Profile": { "type": "object", "required": ["name"] },
	\\      "Base": {
	\\        "type": "object",
	\\        "required": ["id"],
	\\        "properties": { "id": { "type": "integer" } }
	\\      },
	\\      "Cat": {
	\\        "type": "object",
	\\        "required": ["kind", "purrs"],
	\\        "properties": { "kind": { "type": "string" }, "purrs": { "type": "boolean" } }
	\\      },
	\\      "Dog": {
	\\        "type": "object",
	\\        "required": ["kind", "barks"],
	\\        "properties": { "kind": { "type": "string" }, "barks": { "type": "boolean" } }
	\\      }
	\\    }
	\\  }
	\\}

name_ref : Str
name_ref =
	\\{ "$ref": "#/components/schemas/Name" }

expect check_in("\"hello\"", name_ref, components_doc) == []

expect failed_keywords(check_in("42", name_ref, components_doc)) == ["type"]

# RFC 6901: ~1 in a token decodes to /, and ~0 to ~.
expect {
	root_doc =
		\\{ "paths": { "/users/{id}": { "weird~key": { "type": "integer" } } } }
	schema =
		\\{ "$ref": "#/paths/~1users~1{id}/weird~0key" }
	check_in("42", schema, root_doc) == [] and failed_keywords(check_in("\"x\"", schema, root_doc)) == ["type"]
}

# A $ref that does not resolve constrains nothing.
expect check_in("\"anything\"", "{ \"$ref\": \"#/nonexistent/path\" }", "{}") == []

ref_chains_doc : Str
ref_chains_doc =
	\\{
	\\  "defs": {
	\\    "A": { "$ref": "#/defs/B" },
	\\    "B": { "$ref": "#/defs/C" },
	\\    "C": { "type": "integer" },
	\\    "Loop": { "$ref": "#/defs/Pool" },
	\\    "Pool": { "$ref": "#/defs/Loop" }
	\\  }
	\\}

# A refers to B, which refers to C.
expect check_in("42", "{ \"$ref\": \"#/defs/A\" }", ref_chains_doc) == []

expect failed_keywords(check_in("\"x\"", "{ \"$ref\": \"#/defs/A\" }", ref_chains_doc)) == ["type"]

# A cycle is an error, not a stack overflow.
expect failed_keywords(check_in("\"anything\"", "{ \"$ref\": \"#/defs/Loop\" }", ref_chains_doc)) == ["$ref"]

expect Schema.resolve_ref(json("{ \"$ref\": \"#/defs/A\" }"), json(ref_chains_doc)) == Ok(json("{ \"type\": \"integer\" }"))

expect Schema.resolve_ref(json("{ \"$ref\": \"#/defs/Loop\" }"), json(ref_chains_doc)) == Err(CircularRef("#/defs/Loop"))

# Schemas that come back to themselves through allOf, anyOf, oneOf, not or a
# discriminator, without going deeper into the value. Meeting one again adds
# nothing, since it is being checked already.
cycles_doc : Str
cycles_doc =
	\\{
	\\  "components": {
	\\    "schemas": {
	\\      "Self": { "allOf": [{ "$ref": "#/components/schemas/Self" }], "type": "integer" },
	\\      "Pet": {
	\\        "type": "object",
	\\        "required": ["kind", "name"],
	\\        "properties": { "kind": { "type": "string" }, "name": { "type": "string" } },
	\\        "oneOf": [{ "$ref": "#/components/schemas/Cat" }, { "$ref": "#/components/schemas/Dog" }],
	\\        "discriminator": { "propertyName": "kind", "mapping": { "cat": "Cat", "dog": "Dog" } }
	\\      },
	\\      "Cat": {
	\\        "allOf": [
	\\          { "$ref": "#/components/schemas/Pet" },
	\\          { "type": "object", "required": ["purrs"], "properties": { "purrs": { "type": "boolean" } } }
	\\        ]
	\\      },
	\\      "Dog": {
	\\        "allOf": [
	\\          { "$ref": "#/components/schemas/Pet" },
	\\          { "type": "object", "required": ["barks"], "properties": { "barks": { "type": "boolean" } } }
	\\        ]
	\\      },
	\\      "Animal": {
	\\        "type": "object",
	\\        "required": ["name"],
	\\        "oneOf": [{ "$ref": "#/components/schemas/Bird" }, { "$ref": "#/components/schemas/Fish" }]
	\\      },
	\\      "Bird": { "allOf": [{ "$ref": "#/components/schemas/Animal" }, { "required": ["wings"] }] },
	\\      "Fish": { "allOf": [{ "$ref": "#/components/schemas/Animal" }, { "required": ["fins"] }] },
	\\      "Node": {
	\\        "type": "object",
	\\        "properties": { "name": { "type": "string" }, "child": { "$ref": "#/components/schemas/Node" } }
	\\      }
	\\    }
	\\  }
	\\}

check_cycle : Str, Str -> List(ValidationError)
check_cycle = |value, name| check_in(value, "{ \"$ref\": \"#/components/schemas/${name}\" }", cycles_doc)

expect check_cycle("1", "Self") == []

expect failed_keywords(check_cycle("\"1\"", "Self")) == ["type"]

# A base that lists its subtypes, which include the base.
expect check_cycle("{ \"kind\": \"cat\", \"name\": \"Tom\", \"purrs\": true }", "Pet") == []

expect messages(check_cycle("{ \"kind\": \"cat\", \"purrs\": true }", "Pet")) == ["Missing required field: name"]

expect messages(check_cycle("{ \"kind\": \"cat\", \"name\": \"Tom\" }", "Pet")) == ["Missing required field: purrs"]

# A subtype on its own still takes in the base.
expect messages(check_cycle("{ \"kind\": \"dog\", \"barks\": true }", "Dog")) == ["Missing required field: name"]

# The same without a discriminator, so each subtype is tried.
expect check_cycle("{ \"name\": \"Tweety\", \"wings\": 2 }", "Animal") == []

expect failed_keywords(check_cycle("{ \"name\": \"Nemo\", \"wings\": 2, \"fins\": 4 }", "Animal")) == ["oneOf"]

# A schema met again deeper in the value is checked again there.
expect {
	errors = check_cycle("{ \"child\": { \"child\": { \"name\": 1 } } }", "Node")
	errors.map(|error| error.path) == [["child", "child", "name"]] and failed_keywords(errors) == ["type"]
}

# Branches: a schema, and those under its allOf, anyOf and oneOf, at any
# depth, each once even where they include themselves.
expect {
	names = Schema.branches(json("{ \"$ref\": \"#/components/schemas/Pet\" }"), json(cycles_doc)).map(|branch| branch.get_field("required").map_ok(JsonValue.to_str) ?? "")
	names == ["[\"kind\",\"name\"]", "", "[\"purrs\"]", "", "[\"barks\"]"]
}

# A schema met twice at one place, but not inside itself, is checked twice.
expect failed_keywords(check_in("\"x\"", "{ \"allOf\": [${name_ref}, { \"$ref\": \"#/components/schemas/Count\" }, { \"$ref\": \"#/components/schemas/Count\" }] }", components_doc)) == ["type", "type"]

# $ref under items
expect {
	schema =
		\\{ "type": "array", "items": { "$ref": "#/components/schemas/Count" } }
	errors = check_in("[1, \"not a number\"]", schema, components_doc)
	errors.map(|error| error.path) == [["1"]] and failed_keywords(errors) == ["type"]
}

# $ref under properties

name_ref_property : Str
name_ref_property =
	\\{
	\\  "type": "object",
	\\  "properties": { "name": { "$ref": "#/components/schemas/Name" } }
	\\}

expect check_in("{ \"name\": \"Alice\" }", name_ref_property, components_doc) == []

expect {
	errors = check_in("{ \"name\": 123 }", name_ref_property, components_doc)
	errors.map(|error| error.path) == [["name"]] and failed_keywords(errors) == ["type"]
}

# nullable

nullable_string_schema : Str
nullable_string_schema =
	\\{ "type": "string", "nullable": true }

expect check("null", nullable_string_schema) == []

expect failed_keywords(check("42", nullable_string_schema)) == ["type"]

expect failed_keywords(check("null", string_schema)) == ["type"]

# anyOf

string_or_integer_any_of : Str
string_or_integer_any_of =
	\\{ "anyOf": [{ "type": "string" }, { "type": "integer" }] }

expect check("\"hello\"", string_or_integer_any_of) == []

expect check("42", string_or_integer_any_of) == []

expect failed_keywords(check("true", string_or_integer_any_of)) == ["anyOf"]

expect check_in("\"hello\"", "{ \"anyOf\": [${name_ref}] }", components_doc) == []

# Null as one of the alternatives.
expect check("null", "{ \"anyOf\": [{ \"type\": \"string\" }, { \"type\": \"null\" }] }") == []

# Stripe's most common pattern: anyOf over a $ref, made nullable.
nullable_profile : Str
nullable_profile =
	\\{ "anyOf": [{ "$ref": "#/components/schemas/Profile" }], "nullable": true }

expect check_in("null", nullable_profile, components_doc) == []

expect check_in("{ \"name\": \"Alice\" }", nullable_profile, components_doc) == []

expect failed_keywords(check_in("42", nullable_profile, components_doc)) == ["anyOf"]

# The message says why the value fails each schema.
expect messages(check("true", string_or_integer_any_of)) == ["Value does not match any of the 2 schemas in anyOf. Schema 1: /: Expected type string but got boolean. Schema 2: /: Expected type integer but got boolean"]

expect {
	schema =
		\\{ "type": "object", "properties": { "tags": { "anyOf": [{ "type": "array", "items": { "type": "string" } }, { "enum": [""] }] } } }
	messages(check("{ \"tags\": [\"a\", 1] }", schema)) == ["Value does not match any of the 2 schemas in anyOf. Schema 1: /tags/1: Expected type string but got number. Schema 2: /tags: Value [\"a\",1] not in enum [\"\"]"]
}

# oneOf

string_or_integer_one_of : Str
string_or_integer_one_of =
	\\{ "oneOf": [{ "type": "string" }, { "type": "integer" }] }

expect check("\"hello\"", string_or_integer_one_of) == []

expect messages(check("true", string_or_integer_one_of)) == ["Value does not match any of the 2 schemas in oneOf. Schema 1: /: Expected type string but got boolean. Schema 2: /: Expected type integer but got boolean"]

# An integer is a number too, so both match.
expect messages(check("1", "{ \"oneOf\": [{ \"type\": \"string\" }, { \"type\": \"number\" }, { \"type\": \"integer\" }] }")) == ["Value matches schemas 2 and 3 in oneOf, expected exactly 1"]

expect check("null", "{ \"oneOf\": [{ \"type\": \"string\" }, { \"type\": \"integer\" }], \"nullable\": true }") == []

# Counting stops at the second match, however many schemas there are.
expect {
	sub_schemas = Str.join_with(List.repeat("{ \"type\": \"number\" }", 20), ", ")
	failed_keywords(check("1", "{ \"oneOf\": [${sub_schemas}] }")) == ["oneOf"]
}

# discriminator

pet_one_of : Str
pet_one_of =
	\\{ "$ref": "#/components/schemas/Cat" }, { "$ref": "#/components/schemas/Dog" }

pet_by_mapping : Str
pet_by_mapping =
	\\{
	\\  "oneOf": [${pet_one_of}],
	\\  "discriminator": {
	\\    "propertyName": "kind",
	\\    "mapping": { "cat": "#/components/schemas/Cat", "dog": "#/components/schemas/Dog" }
	\\  }
	\\}

# Without a mapping the value names the schema under #/components/schemas.
pet_by_name : Str
pet_by_name =
	\\{ "oneOf": [${pet_one_of}], "discriminator": { "propertyName": "kind" } }

expect check_in("{ \"kind\": \"cat\", \"purrs\": true }", pet_by_mapping, components_doc) == []

# Only the schema the discriminator picks is validated against.
expect messages(check_in("{ \"kind\": \"dog\", \"purrs\": true }", pet_by_mapping, components_doc)) == ["Missing required field: barks"]

expect check_in("{ \"kind\": \"Dog\", \"barks\": true }", pet_by_name, components_doc) == []

# The value lacks the discriminator property.
expect failed_keywords(check_in("{ \"name\": \"Whiskers\" }", pet_by_name, components_doc)) == ["discriminator"]

# The discriminator value names no schema.
expect messages(check_in("{ \"kind\": \"unknown\" }", pet_by_name, components_doc)) == ["Discriminator value 'unknown' does not resolve via #/components/schemas/unknown"]

# A mapping may give the name of a schema rather than a $ref.
pet_by_mapped_name : Str
pet_by_mapped_name =
	\\{ "oneOf": [${pet_one_of}], "discriminator": { "propertyName": "kind", "mapping": { "cat": "Cat", "dog": "Dog" } } }

expect check_in("{ \"kind\": \"cat\", \"purrs\": true }", pet_by_mapped_name, components_doc) == []

expect messages(check_in("{ \"kind\": \"dog\", \"purrs\": true }", pet_by_mapped_name, components_doc)) == ["Missing required field: barks"]

# A value the mapping does not list names a schema itself.
expect check_in("{ \"kind\": \"Dog\", \"barks\": true }", pet_by_mapped_name, components_doc) == []

# A name with "/" or "~" in it is escaped in the $ref it makes.
expect {
	root_doc =
		\\{ "components": { "schemas": { "a/b~c": { "required": ["x"] } } } }
	schema =
		\\{ "oneOf": [{ "$ref": "#/components/schemas/a~1b~0c" }], "discriminator": { "propertyName": "kind" } }
	check_in("{ \"kind\": \"a/b~c\", \"x\": 1 }", schema, root_doc) == [] and messages(check_in("{ \"kind\": \"a/b~c\" }", schema, root_doc)) == ["Missing required field: x"]
}

# allOf

name_and_age_all_of : Str
name_and_age_all_of =
	\\{
	\\  "allOf": [
	\\    { "type": "object", "required": ["name"] },
	\\    { "type": "object", "required": ["age"] }
	\\  ]
	\\}

expect check("{ \"name\": \"Alice\", \"age\": 30 }", name_and_age_all_of) == []

expect messages(check("{ \"name\": \"Alice\" }", name_and_age_all_of)) == ["Missing required field: age"]

# Schema inheritance: a $ref to the base, extended inline.
expect {
	schema =
		\\{
		\\  "allOf": [
		\\    { "$ref": "#/components/schemas/Base" },
		\\    {
		\\      "type": "object",
		\\      "required": ["name"],
		\\      "properties": { "name": { "type": "string" } }
		\\    }
		\\  ]
		\\}
	check_in("{ \"id\": 1, \"name\": \"Alice\" }", schema, components_doc) == []
		and failed_keywords(check_in("{ \"id\": \"1\" }", schema, components_doc)) == ["type", "required"]
}

# not

expect check("1", "{ \"not\": { \"type\": \"string\" } }") == []

expect messages(check("\"a\"", "{ \"not\": { \"type\": \"string\" } }")) == ["Value matches the schema in not"]
