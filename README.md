# roc-openapi

Validate HTTP request bodies against [OpenAPI](https://www.openapis.org/) specs in Roc. Pure functions, zero external dependencies.

## Example usage

```roc
app [main!] {
    pf: platform "https://github.com/growthagent/basic-cli/releases/download/0.27.0/G-A6F5ny0IYDx4hmF3t_YPHUSR28c9ZXMBnh0FEJjwk.tar.br",
    openapi: "https://github.com/niclas-ahden/roc-openapi/releases/download/0.1.0/HASH.tar.br",
}

import pf.File
import openapi.OpenApi

main! = |_args|
    spec = File.read_bytes!("spec/openapi.json")?

    body = Str.to_utf8(
        """
        {"personalizations": [{"to": [{"email": "user@example.com"}]}], "from": {"email": "sender@example.com"}}
        """,
    )

    errors = OpenApi.validate_request({ spec, method: "post", path: "/v3/mail/send", body })?

    if List.is_empty(errors) then
        Ok({})
    else
        errors
        |> List.map(|e| "  ${Str.join_with(e.path, ".")}: ${e.message}")
        |> Str.join_with("\n")
        |> |msg| Err(ValidationFailed(msg))
```

## What it validates

Given an OpenAPI 3.x spec, validates request bodies against JSON Schema keywords:

- `type` (string, object, array, integer, boolean, number; single or array)
- `required` fields
- `properties` (recursive nested validation)
- `items` (array element validation)
- `$ref` resolution (local references, cycle detection via visited set)
- `nullable` (OpenAPI 3.0.x; null values accepted when true)
- `anyOf` (value matches at least one sub-schema)
- `oneOf` (value matches exactly one sub-schema)
- `allOf` (value matches all sub-schemas)
- `discriminator` (optimized `oneOf` selection by property value)
- `format: email` (lax: checks for `@` and a `.` in the domain part; not RFC 5322)
- `minLength`, `maxLength` (counts Unicode scalar values, not bytes)
- `maxItems`, `maxProperties`
- `enum`, `uniqueItems`
- `additionalProperties`

## API

### `validate_request`

```roc
OpenApi.validate_request :
    { spec : List U8, method : Str, path : Str, body : List U8 }
    -> Result (List ValidationError) [SpecParseError Str, BodyParseError Str, PathNotFound Str, MethodNotFound Str, NoRequestBody, NoJsonSchema]
```

Returns `Ok([])` if valid, `Ok([...errors])` if there are validation errors.

### `ValidationError`

```roc
{ path : List Str, message : Str, keyword : Str }
```

- `path`: JSON pointer segments, e.g. `["personalizations", "0", "to", "0", "email"]`
- `keyword`: the JSON Schema keyword that failed, e.g. `"type"`, `"required"`, `"minLength"`
- `message`: human-readable description

## Tested against

Validated with real-world OpenAPI specs from:
- **SendGrid** (1.2K lines): basic schemas
- **Twilio** (46K lines): `nullable`-heavy, no composition
- **Stripe** (196K lines): heavy `anyOf`/`oneOf` usage
- **GitHub** (326K lines): `allOf` inheritance, `discriminator`

## Limitations

- **Request body only**: does not validate responses, query parameters, headers, or path parameters.
- **Null tolerance**: `null` values on non-required properties are treated as absent.
- **`format` only validates `email`**: other formats (`uri`, `date-time`, etc.) are silently skipped, per JSON Schema spec.
- **Spec re-parsed per call**: `validate_request` parses the spec bytes on each invocation. A `parse_spec` function is planned for repeated use.
- **Quadratic on large inputs**: `uniqueItems` and per-property schema lookup are O(n²) in the array length and property count. Fine for typical payloads; don't expose this validator to untrusted callers without a size cap.

## Documentation

View the full API documentation at [https://niclas-ahden.github.io/roc-openapi/](https://niclas-ahden.github.io/roc-openapi/).
