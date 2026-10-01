# roc-openapi

Validate HTTP request bodies, JSON or forms, against [OpenAPI](https://www.openapis.org/) specs in Roc. It is pure Roc, so it runs on any platform.

## Example usage

```roc
app [main!] {
	pf: platform "https://github.com/niclas-ahden/basic-cli/releases/download/0.27.0/HZanbveSUDoJF8LypR663eH7PpaKEKG36eErEQzmV1Qs.tar.zst",
	openapi: "https://github.com/niclas-ahden/roc-openapi/releases/download/0.2.0/J4TCLrSzmB2XKHN9BLxcREjG4LUUgkH4uUoMZcsy32Qm.tar.zst",
}

import pf.Stdout
import openapi.OpenApi

## Validates two request bodies against the spec below and prints what is
## wrong with each.
main! = |_args| {
	# Parse the spec once, then validate as many requests as you like.
	api = OpenApi.parse(spec.to_utf8())?

	report!(api, "{\"name\": \"gadget\", \"tags\": [\"new\"]}")?
	report!(api, "{\"tags\": [\"new\", 7]}")
}

report! = |api, body| {
	Stdout.line!("POST /widgets ${body}")?

	match api.validate_request({ method: "POST", path: "/widgets", content_type: "application/json", body: body.to_utf8() }) {
		Ok({}) => Stdout.line!("  valid")
		Err(Invalid(errors)) => {
			for error in errors {
				Stdout.line!("  ${error.to_str()}")?
			}
			Ok({})
		}

		Err(problem) => Stdout.line!("  could not validate: ${Str.inspect(problem)}")
	}
}

# A real program would read the spec from a file. It is inlined here so that
# the example is one file.
spec : Str
spec =
	\\{
	\\  "paths": {
	\\    "/widgets": {
	\\      "post": {
	\\        "requestBody": {
	\\          "content": {
	\\            "application/json": {
	\\              "schema": {
	\\                "type": "object",
	\\                "required": ["name"],
	\\                "properties": {
	\\                  "name": { "type": "string" },
	\\                  "tags": { "type": "array", "items": { "type": "string" } }
	\\                }
	\\              }
	\\            }
	\\          }
	\\        }
	\\      }
	\\    }
	\\  }
	\\}
```

It prints:

```
POST /widgets {"name": "gadget", "tags": ["new"]}
  valid
POST /widgets {"tags": ["new", 7]}
  /: Missing required field: name
  /tags/1: Expected type string but got number
```

This is [`examples/main.roc`](./examples/main.roc).

## What it validates

Given an OpenAPI 3.x spec, validates request bodies against JSON Schema keywords:

- `type` (string, object, array, integer, boolean, number and null, as a single name or a list of names)
- `required` fields, except `readOnly` ones, which OpenAPI only requires in responses
- `properties` (recursive nested validation)
- `items` (array element validation)
- `$ref` resolution (local references, a chain of `$ref`s that loops is an error)
- Schemas that refer to themselves. One that comes back to itself without going deeper into the body, such as a base that lists its subtypes under `oneOf` while each subtype includes the base with `allOf`, is checked once there.
- `nullable` (OpenAPI 3.0.x, null values accepted when true)
- `anyOf` (value matches at least one sub-schema, and the error says why it fails each)
- `oneOf` (value matches exactly one sub-schema, and the error says why it fails each, or which two it matches)
- `allOf` (value matches all sub-schemas)
- `not` (value does not match the sub-schema)
- `discriminator` (picks the `oneOf` sub-schema by property value, through `mapping`, which gives a `$ref` or a schema name, or by schema name)
- `format: email` (lax: checks for an `@` and a `.` in the domain part, not RFC 5322)
- `pattern` (ECMAScript regular expressions, matched anywhere in the string unless anchored)
- `minLength`, `maxLength` (counts Unicode scalar values, not bytes)
- `minItems`, `maxItems`, `minProperties`, `maxProperties`
- `minimum`, `maximum`, `exclusiveMinimum`, `exclusiveMaximum` (both the OpenAPI 3.0 boolean form and the 3.1 number form)
- `multipleOf` (with a tolerance for floats, so `0.3` is a multiple of `0.1`)
- `enum`, `const`, `uniqueItems` (objects are equal with their fields in any order)
- `additionalProperties` (`false` or a schema)
- `required` on the request body itself: an empty body is valid unless the spec requires one

## Media types

The request's Content-Type picks the media type in the spec, and so the schema: the media type with the same type and subtype, else a wildcard such as `application/*` that covers it, else `*/*`. Parameters such as `; charset=utf-8` do not count. A request body that is a `$ref` is resolved.

The Content-Type also says how the body is read:

- **JSON**: `application/json`, `text/json`, and any type ending in `+json`, such as `application/merge-patch+json`.
- **Forms**: `application/x-www-form-urlencoded`. Every field of a form is text, so each is read as what its schema takes: a number, a boolean, the text itself, null for empty text, or a list of one. Where a schema has `anyOf` or `oneOf`, each of those is tried, so Stripe's `cancel_at=1700000000` is a number and `cancel_at=max_period_end` the text. A name given more than once, or with `[]`, makes a list, and brackets nest, so `items[0][price]=p1&metadata[order]=7` is `{"items": [{"price": "p1"}], "metadata": {"order": "7"}}` where the schema says `items` is an array.
- **Text**: `text/plain` is a string.

A media type without a schema takes any body that reads as its type.

## API

### `OpenApi.parse`

```roc
OpenApi.parse : List(U8) -> Try(OpenApi, [SpecParseError(Str)])
```

Parses a spec from its JSON text. Do it once, and validate any number of requests against the result. A spec in YAML has to be converted to JSON first.

When the text is not JSON, the message says what was expected and where, as a JSON Pointer:

```
Expected ',' and a field, or '}', after this field at /paths/~1widgets/post/summary
Invalid value at /components/schemas/Widget/required/2
```

A problem inside a single value, such as a bad escape in a string, is an `Invalid value`, because Roc's builtin parser does not say more. JSON bodies that do not parse get the same messages, in `BodyParseError`.

### `with_max_body_depth`

```roc
with_max_body_depth : OpenApi, U64 -> OpenApi
```

Sets how deep the arrays and objects of a request body may nest, which is 32 unless you change it. `{"a": [1]}` nests 2 deep. A deeper body is a `BodyParseError("Nested deeper than 32 levels")`, before it is parsed:

```roc
api = OpenApi.parse(spec_bytes)?.with_max_body_depth(64)
```

The limit keeps a body from overflowing the stack, which would end the program. Parsing a body, and validating it against a schema that refers to itself, takes stack for every level: about 10 KB with `--opt=speed`, and several times that for a level that goes through `allOf`, `anyOf` or `oneOf`. 32 such levels fit in the 2 MB stack a thread commonly gets. The interpreter backend takes about four times as much. Parsing stops at 2000 levels whatever the limit.

### `validate_request`

```roc
validate_request :
    OpenApi, { method : Str, path : Str, content_type : Str, body : List(U8) }
    -> Try({}, [Invalid(List(ValidationError)), BodyParseError(Str), PathNotFound(Str), MethodNotFound(Str), NoRequestBody, UnsupportedMediaType(Str), CannotDecode(Str)])
```

`Ok({})` means the body is valid. A body that breaks the schema is `Err(Invalid(errors))`, with every way it does. So is an empty body when the spec marks the request body `required`. The other errors mean the request could not be validated at all:

- `BodyParseError`: the body does not read as its media type, or nests deeper than `with_max_body_depth` allows.
- `PathNotFound` and `MethodNotFound`: the spec does not list them.
- `NoRequestBody`: the operation takes no body.
- `UnsupportedMediaType`: the request body in the spec has no media type for `content_type`, which the error names as it was given. An empty `content_type` matches only `*/*`.
- `CannotDecode`: it has one, but this package does not read bodies of that type, such as `multipart/form-data`.

`method` is any case, such as `POST` or `post`.

`content_type` is the request's Content-Type header, such as `application/json` or `application/x-www-form-urlencoded; charset=utf-8`. [Media types](#media-types) says what it picks and how the body is read.

`path` is the request's path, such as `/repos/roc-lang/roc/labels`, or its whole URL. Its query is ignored. It is matched against the spec's path templates, such as `/repos/{owner}/{repo}/labels` or Twilio's `/Calls/{Sid}.json`, with or without the base path of a server, such as the `/v1` of `https://api.example.com/v1`. When more than one template fits, the most specific wins, so `/users/me` beats `/users/{id}`. A template matches itself too, so passing the one from the spec works as well. A trailing slash makes another path, so `/users/` does not match `/users`.

`body` is empty for a request without one. That is valid unless the spec marks the request body `required`, in which case it is `Invalid`, with the error `/: Missing required request body`, whatever the `content_type`.

Where an invalid body is a failure, such as in a test, `?` is all it takes:

```roc
api = OpenApi.parse(spec_bytes)?
api.validate_request({ method: "POST", path: "/v3/mail/send", content_type: "application/json", body })?
```

Match on `Invalid` to handle the errors yourself:

```roc
match api.validate_request({ method: "POST", path: "/v1/subscriptions", content_type, body }) {
    Ok({}) => ...
    Err(Invalid(errors)) => ... # Every error, such as errors.map(|error| error.to_str())
    Err(UnsupportedMediaType(_)) => ... # Such as a 415 response
    Err(problem) => ... # The spec has no such operation, or the body cannot be read
}
```

### `ValidationError`

```roc
{ path : List(Str), message : Str, keyword : Str }
```

- `path`: JSON Pointer segments, e.g. `["personalizations", "0", "to", "0", "email"]`
- `keyword`: the JSON Schema keyword that failed, e.g. `"type"`, `"required"`, `"minLength"`
- `message`: human-readable description

`error.to_str()` puts it on one line, its place in the body first, as a JSON Pointer. `/` is the body itself:

```
/personalizations/0/to/0/email: Expected type string but got number
/: Missing required field: from
```

## Tested against

`./tests.roc` validates payloads against the OpenAPI specs of:

- **SendGrid** (1.2K lines): JSON bodies, basic schemas
- **Twilio** (46K lines): form bodies with integer, boolean and list fields, `pattern`, no composition
- **Stripe** (196K lines): form bodies with fields in brackets, heavy `anyOf` usage
- **GitHub** (326K lines): JSON bodies, `allOf` inheritance, `oneOf`, `discriminator`, `minItems`, `pattern`

## Limitations

- **Request body only**: does not validate responses, query parameters, headers, or path parameters.
- **JSON, forms and text only**: bodies of other media types, such as `multipart/form-data`, are `CannotDecode`.
- **Forms make lists and nest by name only**: the `encoding` of a media type is not read, so a list written on one line, such as `tags=a,b` or `tags=a|b`, is one piece of text.
- **Base paths from the spec and paths only**: the `servers` of the spec, and of a path, give the base paths a request's path may start with. Those of a single operation do not. The host in a URL is not checked.
- **Null tolerance**: `null` values on non-required properties are treated as absent.
- **`format` only validates `email`**: other formats (`uri`, `date-time`, etc.) are skipped, which JSON Schema allows.
- **`pattern` skips what it cannot do**: lookaround, backreferences and Unicode property escapes such as `\p{L}` are not supported, and a `pattern` that uses them is not checked.
- **`pattern` gives up on a long search**: a search that takes over a million steps fails the value with a `pattern` error, since it might not match. A pattern like `(a+)+b` gets there on a short string, and a simple one such as `^[a-z]+$` on a string of some hundreds of thousands of characters.
- **Local `$ref`s only**: a `$ref` into another file is left unresolved, and an unresolved `$ref` constrains nothing.
- **Quadratic on large inputs**: `uniqueItems`, object equality and per-property schema lookup are O(n²) in the array length and property count. That is fine for typical payloads. `with_max_body_depth` caps how deep a body nests but not how big it is, so cap the size of bodies from untrusted callers.

## Requirements

The compiler this package answers for is the one `flake.nix` pins. `nix develop` puts it on your PATH, and CI gates on it. Releases name the newest Roc nightly the suite passed on in their notes.

## Testing

The unit tests are the `expect`s in `package/`:

```sh
roc test package/main.roc
```

The integration tests are the `tests/*_test.roc` apps, which validate payloads against the committed SendGrid, GitHub, Stripe and Twilio specs. `./tests.roc` runs them in parallel through [roc-spec](https://github.com/niclas-ahden/roc-spec), and takes a filename pattern and `--fail-fast`:

```sh
./tests.roc
./tests.roc stripe
```

`./update-fixtures.roc` downloads the specs again from their upstream sources. CI runs all of this, plus the examples and a bundle check, through [roc-workflows](https://github.com/niclas-ahden/roc-workflows).

## Documentation

View the full API documentation at [https://niclas-ahden.github.io/roc-openapi/](https://niclas-ahden.github.io/roc-openapi/).
