module [
    validate_request,
    ValidationError,
]

import json_value.JsonValue exposing [JsonValue]
import Schema

## A validation error with path, message, and the JSON Schema keyword that failed.
##
## `path` is a list of segments: `["personalizations", "0", "to", "0", "email"]`
## `keyword` is the JSON Schema keyword: `"type"`, `"required"`, `"minLength"`, etc.
ValidationError : Schema.ValidationError

## Validate an HTTP request body against an OpenAPI spec.
##
## ```roc
## OpenApi.validate_request({
##     spec: spec_bytes,
##     method: "post",
##     path: "/v3/mail/send",
##     body: request_body_bytes,
## })
## ```
##
## Returns `Ok([])` if valid, `Ok([...errors])` if there are validation errors.
validate_request :
    {
        spec : List U8,
        method : Str,
        path : Str,
        body : List U8,
    }
    -> Result (List ValidationError) [
        SpecParseError Str,
        BodyParseError Str,
        PathNotFound Str,
        MethodNotFound Str,
        NoRequestBody,
        NoJsonSchema,
    ]
validate_request = |{ spec, method, path, body }|
    spec_tree = JsonValue.parse(spec)
        |> Result.map_err(|JsonParseError(msg)| SpecParseError(msg))?

    body_tree = JsonValue.parse(body)
        |> Result.map_err(|JsonParseError(msg)| BodyParseError(msg))?

    schema = find_request_body_schema(spec_tree, method, path)?

    errors = Schema.validate(body_tree, schema, spec_tree, [])

    Ok(errors)

# ── OpenAPI spec navigation ──

find_request_body_schema :
    JsonValue, Str, Str
    -> Result JsonValue [
        PathNotFound Str,
        MethodNotFound Str,
        NoRequestBody,
        NoJsonSchema,
    ]
find_request_body_schema = |spec, method, path|
    paths_obj = JsonValue.get_field(spec, "paths")
        |> Result.map_err(|_| PathNotFound(path))?

    path_obj = JsonValue.get_field(paths_obj, path)
        |> Result.map_err(|_| PathNotFound(path))?

    method_obj = JsonValue.get_field(path_obj, method)
        |> Result.map_err(|_| MethodNotFound(method))?

    # requestBody might be a $ref
    request_body_raw = JsonValue.get_field(method_obj, "requestBody")
        |> Result.map_err(|_| NoRequestBody)?
    request_body = resolve_if_ref(spec, request_body_raw, [])

    # Navigate: content -> {content-type} -> schema
    # Try application/json first, then application/x-www-form-urlencoded
    content = JsonValue.get_field(request_body, "content")
        |> Result.map_err(|_| NoJsonSchema)?

    content_schema =
        when JsonValue.get_field(content, "application/json") is
            Ok(json_content) -> JsonValue.get_field(json_content, "schema")
            Err(_) ->
                when JsonValue.get_field(content, "application/x-www-form-urlencoded") is
                    Ok(form_content) -> JsonValue.get_field(form_content, "schema")
                    Err(_) -> Err(FieldNotFound("schema"))

    schema = content_schema |> Result.map_err(|_| NoJsonSchema)?

    # The schema itself might be a $ref
    Ok(resolve_if_ref(spec, schema, []))

## Resolve $ref chains during OpenAPI navigation. Uses Schema.navigate_ref
## for the actual ref path traversal and a visited set for cycle detection.
resolve_if_ref : JsonValue, JsonValue, List Str -> JsonValue
resolve_if_ref = |root, node, seen|
    when JsonValue.get_field(node, "$ref") is
        Ok(ref_val) ->
            when JsonValue.get_str(ref_val) is
                Ok(ref_str) ->
                    if List.contains(seen, ref_str) then
                        node # circular: return as-is
                    else
                        when Schema.navigate_ref(root, ref_str) is
                            Ok(resolved) -> resolve_if_ref(root, resolved, List.append(seen, ref_str))
                            Err(_) -> node

                Err(_) -> node

        Err(_) -> node
