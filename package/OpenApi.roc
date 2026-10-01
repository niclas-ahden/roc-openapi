## Validate HTTP request bodies, JSON or forms, against an OpenAPI 3.x spec.
import Form
import JsonValue
import PathTemplate
import Schema
import ValidationError

# A path in the spec's "paths", to match the paths of requests against: its
# key, and its template, with the base path of one of its servers in front or
# without one.
Route : { key : Str, template : PathTemplate }

## An OpenAPI 3.x spec, parsed once to validate any number of requests
## against.
OpenApi :: { doc : Schema.Doc, routes : List(Route), max_body_depth : U64 }.{

	## Parse a spec from its JSON text. `SpecParseError` when the text is not
	## JSON, with what was expected and where, such as
	## `Expected ':' after this field name at /paths/~1widgets`, or when it is
	## JSON but not an object.
	##
	## ```roc
	## api = OpenApi.parse(spec_bytes)?
	## ```
	parse : List(U8) -> Try(OpenApi, [SpecParseError(Str)])
	parse = |bytes| {
		spec = JsonValue.parse(bytes) ? |InvalidJson(message)| SpecParseError(message)
		match spec.get_object() {
			Ok(_) => Ok(OpenApi.{ doc: Schema.prepare(spec), routes: routes_of(spec), max_body_depth: default_max_body_depth })
			Err(_) => Err(SpecParseError("Expected an object but got ${spec.type_name()}"))
		}
	}

	## Let the arrays and objects of a request body nest `max_depth` deep,
	## rather than 32. `{"a": [1]}` nests 2 deep. A deeper body is a
	## `BodyParseError`, before it is parsed.
	##
	## ```roc
	## api = OpenApi.parse(spec_bytes)?.with_max_body_depth(64)
	## ```
	##
	## The limit keeps a body from overflowing the stack, which would end the
	## program. Parsing a body, and validating it against a schema that refers
	## to itself, takes stack for every level: about 10 KB with `--opt=speed`,
	## and several times that for a level that goes through `allOf`, `anyOf`
	## or `oneOf`. 32 such levels fit in the 2 MB stack a thread commonly
	## gets. The interpreter backend takes about four times as much. Parsing
	## stops at 2000 levels whatever the limit.
	with_max_body_depth : OpenApi, U64 -> OpenApi
	with_max_body_depth = |api, max_depth| OpenApi.{ doc: api.doc, routes: api.routes, max_body_depth: max_depth }

	## Validate a request body against the schema the spec gives for its
	## operation and media type. `Ok({})` means the body is valid.
	##
	## ```roc
	## api.validate_request({ method: "POST", path: "/repos/roc-lang/roc/labels", content_type: "application/json", body })?
	## ```
	##
	## `method` is any case, such as `POST` or `post`.
	##
	## `path` is the request's path, such as `/repos/roc-lang/roc/labels`, or
	## its whole URL. It is matched against the spec's path templates, such as
	## `/repos/{owner}/{repo}/labels`, with or without the base path of a
	## server, such as the `/v1` of `https://api.example.com/v1`. The most
	## specific template wins, so `/users/me` beats `/users/{id}`. A template
	## is a path too, which matches itself. A trailing slash makes another
	## path, so `/users/` does not match `/users`.
	##
	## `content_type` is the request's Content-Type header, such as
	## `application/json; charset=utf-8`. It picks the media type in the
	## spec: the one with the same type and subtype, else a wildcard such as
	## `application/*` that covers it, else `*/*`. Parameters such as
	## `charset` do not count. An empty `content_type` matches only `*/*`.
	##
	## It also says how the body is read:
	##
	## - JSON, for `application/json`, `text/json` and types that end in
	##   `+json`, such as `application/merge-patch+json`.
	## - A form, for `application/x-www-form-urlencoded`. Its text is read as
	##   what the schema says each field is, so `count=2` is a number where
	##   the schema wants an integer. A name given more than once makes a
	##   list, and brackets nest, so `items[0][price]=p1` is
	##   `{"items": [{"price": "p1"}]}`.
	## - A string, for `text/plain`.
	##
	## A media type without a schema takes any body that reads as its type.
	##
	## `body` is empty for a request without one. That is valid unless the
	## spec marks the request body `required`, whatever the `content_type`.
	##
	## The errors:
	##
	## - `Invalid` when the body breaks the schema, with every way it does, or
	##   when it is empty and the spec requires one.
	## - `BodyParseError` when the body does not read as its type, or nests
	##   deeper than `with_max_body_depth` allows.
	## - `PathNotFound` and `MethodNotFound` when the spec does not list them.
	## - `NoRequestBody` when the operation takes no body.
	## - `UnsupportedMediaType` when the request body in the spec has no media
	##   type for `content_type`, as given.
	## - `CannotDecode` when it has one, but its bodies are of a type this
	##   package does not read, such as `multipart/form-data`.
	validate_request : OpenApi, { method : Str, path : Str, content_type : Str, body : List(U8) } -> Try({}, [Invalid(List(ValidationError)), BodyParseError(Str), PathNotFound(Str), MethodNotFound(Str), NoRequestBody, UnsupportedMediaType(Str), CannotDecode(Str)])
	validate_request = |api, { method, path, content_type, body }| {
		key = find_path(api.routes, path)?
		request_body = find_request_body(api.doc.root, method, key)?

		if body.is_empty() {
			if is_required(request_body) {
				Err(Invalid([{ path: [], message: "Missing required request body", keyword: "required" }]))
			} else {
				Ok({})
			}
		} else {
			media_type = find_media_type(request_body, content_type)?
			schemas = media_type.get_field("schema").map_ok(|schema| [schema]).ok_or([])
			value = decode_body(body, essence(content_type), schemas, api)?

			match schemas.join_map(|schema| Schema.validate(value, schema, api.doc)) {
				[] => Ok({})
				errors => Err(Invalid(errors))
			}
		}
	}
}

# How deep request bodies may nest unless `with_max_body_depth` says
# otherwise.
default_max_body_depth : U64
default_max_body_depth = 32

# Paths

# Every path in the spec, as it is and behind each base path its servers give
# it. The "servers" of a path replace the spec's.
routes_of : JsonValue -> List(Route)
routes_of = |spec| {
	spec_bases = base_paths(spec)
	fields_of(spec, "paths").join_map(
		|{ key, value: path_item }| {
			bases = if path_item.get_field("servers").is_ok() base_paths(path_item) else spec_bases
			[""].concat(bases).map(|base| { key, template: PathTemplate.parse("${base}${key}") })
		},
	)
}

# The base paths of the servers a node lists, without duplicates, and without
# the empty one that a server such as "https://api.github.com" has.
base_paths : JsonValue -> List(Str)
base_paths = |node| {
	servers =
		match node.get_field("servers") {
			Ok(list) => list.get_array().ok_or([])
			Err(_) => []
		}
	servers
		.keep_oks(url_of)
		.map(PathTemplate.base_path)
		.fold([], |bases, base| if base.is_empty() or bases.contains(base) bases else bases.append(base))
}

url_of : JsonValue -> Try(Str, [NoUrl])
url_of = |server|
	match server.get_field("url") {
		Ok(url) => url.get_str().map_err(|_| NoUrl)
		Err(_) => Err(NoUrl)
	}

# The fields of the object at `name`, none when there is no object.
fields_of : JsonValue, Str -> List({ key : Str, value : JsonValue })
fields_of = |node, name|
	match node.get_field(name) {
		Ok(field) => field.get_object().ok_or([])
		Err(_) => []
	}

# The key in the spec's "paths" for a request to `path`. A key is its own
# match, and anything else goes to the most specific template it matches.
# Between equally specific ones, the first in the spec wins.
find_path : List(Route), Str -> Try(Str, [PathNotFound(Str)])
find_path = |routes, path|
	if routes.any(|route| route.key == path) {
		Ok(path)
	} else {
		request = PathTemplate.request_segments(path)
		best = routes.fold(
			Err(NoMatch),
			|found, route|
				match (route.template.rank(request), found) {
					(Ok(rank), Ok(best_so_far)) if !is_more_specific(rank, best_so_far.rank) => found
					(Ok(rank), _) => Ok({ key: route.key, rank })
					(Err(NoMatch), _) => found
				},
		)
		match best {
			Ok(winner) => Ok(winner.key)
			Err(NoMatch) => Err(PathNotFound(path))
		}
	}

# Whether one rank from `PathTemplate.rank` is lower than another where they
# first differ.
is_more_specific : List(U8), List(U8) -> Bool
is_more_specific = |rank, other|
	match (rank, other) {
		([first, .. as rest], [other_first, .. as other_rest]) =>
			if first == other_first {
				is_more_specific(rest, other_rest)
			} else {
				first < other_first
			}

		_ => Bool.False
	}

# Operations

# The request body at paths -> {key} -> {method} -> requestBody.
find_request_body : JsonValue, Str, Str -> Try(JsonValue, [PathNotFound(Str), MethodNotFound(Str), NoRequestBody])
find_request_body = |spec, method, key| {
	paths = spec.get_field("paths") ? |_| PathNotFound(key)
	path_item = paths.get_field(key) ? |_| PathNotFound(key)

	# HTTP spells methods in uppercase and OpenAPI in lowercase. A path item
	# has fields that are not methods too, such as "parameters".
	lowercase_method = method.with_ascii_lowercased()
	operation =
		if http_methods.contains(lowercase_method) {
			path_item.get_field(lowercase_method) ? |_| MethodNotFound(method)
		} else {
			return Err(MethodNotFound(method))
		}

	# The request body may be a $ref into components/requestBodies.
	request_body_ref = operation.get_field("requestBody") ? |_| NoRequestBody
	Ok(Schema.resolve_ref(request_body_ref, spec).ok_or(request_body_ref))
}

# Whether a request has to have a body, which OpenAPI says it does not unless
# "required" is true.
is_required : JsonValue -> Bool
is_required = |request_body|
	match request_body.get_field("required") {
		Ok(required) => required.get_bool() == Ok(Bool.True)
		Err(_) => Bool.False
	}

http_methods : List(Str)
http_methods = ["get", "put", "post", "delete", "options", "head", "patch", "trace"]

# Media types

# The media type in content -> {media type} of a request body for a request's
# Content-Type: the one with the same type and subtype, else the wildcard for
# its type, such as `text/*`, else `*/*`.
find_media_type : JsonValue, Str -> Try(JsonValue, [UnsupportedMediaType(Str)])
find_media_type = |request_body, content_type| {
	media_types =
		match request_body.get_field("content") {
			Ok(content) => content.get_object().ok_or([])
			Err(_) => []
		}
	first_where = |name|
		media_types
			.find_first(|media_type| essence(media_type.key) == name)
			.map_ok(|media_type| media_type.value)

	request_type = essence(content_type)
	main_type = request_type.split_first("/").map_ok(|{ before, after: _ }| before).ok_or(request_type)
	first_where(request_type)
		.on_err(|_| first_where("${main_type}/*"))
		.on_err(|_| first_where("*/*"))
		.map_err(|_| UnsupportedMediaType(content_type))
}

# The value a body of the media type `media_type` holds, for the schemas in
# `schemas`, which a form needs to know what its text stands for.
decode_body : List(U8), Str, List(JsonValue), OpenApi -> Try(JsonValue, [BodyParseError(Str), CannotDecode(Str)])
decode_body = |body, media_type, schemas, api|
	if is_json(media_type) {
		JsonValue.parse_with_max_depth(body, api.max_body_depth).map_err(|InvalidJson(message)| BodyParseError(message))
	} else if media_type == "application/x-www-form-urlencoded" {
		Form.decode(body, schemas, api.doc, api.max_body_depth).map_err(|InvalidForm(message)| BodyParseError(message))
	} else if media_type == "text/plain" {
		match Str.from_utf8(body) {
			Ok(text) => Ok(JsonValue.(String(text)))
			Err(BadUtf8({ index, problem: _ })) => Err(BodyParseError("Invalid UTF-8 at byte ${index.to_str()}"))
		}
	} else {
		Err(CannotDecode(media_type))
	}

# A media type without its parameters, in lowercase, so that
# "Application/JSON; charset=utf-8" is "application/json".
essence : Str -> Str
essence = |media_type|
	media_type
		.split_first(";")
		.map_ok(|{ before, after: _ }| before)
		.ok_or(media_type)
		.trim()
		.with_ascii_lowercased()

# What the WHATWG MIME Sniffing standard calls a JSON MIME type:
# application/json, text/json, or a subtype that ends in "+json", such as
# application/merge-patch+json. Not application/x-ndjson or
# application/json-seq, whose bodies hold more than one JSON value.
is_json : Str -> Bool
is_json = |name| {
	subtype = name.split_first("/").map_ok(|{ before: _, after }| after).ok_or("")
	name == "application/json" or name == "text/json" or subtype.ends_with("+json")
}

# Tests

# POST /widgets takes a JSON object with a required "name". POST /forms takes
# the same as a form, through a request body that is a $ref, and requires the
# body. GET /widgets has no request body, and POST /notes takes text. PATCH
# /patches takes a JSON patch, or text. PUT /widgets/{id} takes a widget too,
# but PUT /widgets/mine requires "owner", and POST /files/{name}.json requires
# "data". POST /trees takes arrays of arrays, as deep as they go. POST
# /uploads takes what this package cannot read, and any other application
# type as long as it is an object. POST /anything takes anything. The server
# puts /v1 in front.
test_spec : Str
test_spec =
	\\{
	\\  "servers": [{ "url": "https://api.example.com/v1/" }],
	\\  "paths": {
	\\    "/widgets": {
	\\      "parameters": [],
	\\      "get": {},
	\\      "post": {
	\\        "requestBody": {
	\\          "content": { "application/json": { "schema": { "$ref": "#/components/schemas/Widget" } } }
	\\        }
	\\      }
	\\    },
	\\    "/forms": {
	\\      "post": { "requestBody": { "$ref": "#/components/requestBodies/WidgetForm" } }
	\\    },
	\\    "/notes": {
	\\      "post": { "requestBody": { "content": { "text/plain": { "schema": { "type": "string", "maxLength": 5 } } } } }
	\\    },
	\\    "/patches": {
	\\      "patch": {
	\\        "requestBody": {
	\\          "content": {
	\\            "text/plain": { "schema": { "type": "string" } },
	\\            "application/merge-patch+json; charset=utf-8": { "schema": { "$ref": "#/components/schemas/Widget" } }
	\\          }
	\\        }
	\\      }
	\\    },
	\\    "/widgets/{id}": {
	\\      "put": { "requestBody": { "content": { "application/json": { "schema": { "$ref": "#/components/schemas/Widget" } } } } }
	\\    },
	\\    "/widgets/mine": {
	\\      "put": { "requestBody": { "content": { "application/json": { "schema": { "required": ["owner"] } } } } }
	\\    },
	\\    "/files/{name}.json": {
	\\      "post": { "requestBody": { "content": { "application/json": { "schema": { "required": ["data"] } } } } }
	\\    },
	\\    "/trees": {
	\\      "post": { "requestBody": { "content": { "application/json": { "schema": { "$ref": "#/components/schemas/Tree" } } } } }
	\\    },
	\\    "/uploads": {
	\\      "post": {
	\\        "requestBody": {
	\\          "content": {
	\\            "multipart/form-data": { "schema": { "type": "object" } },
	\\            "image/*": { "schema": { "type": "string", "format": "binary" } },
	\\            "application/*": { "schema": { "type": "object" } }
	\\          }
	\\        }
	\\      }
	\\    },
	\\    "/anything": {
	\\      "post": { "requestBody": { "content": { "*/*": {} } } }
	\\    }
	\\  },
	\\  "components": {
	\\    "requestBodies": {
	\\      "WidgetForm": {
	\\        "required": true,
	\\        "content": {
	\\          "application/x-www-form-urlencoded": { "schema": { "$ref": "#/components/schemas/Widget" } }
	\\        }
	\\      }
	\\    },
	\\    "schemas": {
	\\      "Widget": {
	\\        "type": "object",
	\\        "required": ["name"],
	\\        "properties": {
	\\          "name": { "type": "string" },
	\\          "count": { "type": "integer" },
	\\          "tags": { "type": "array", "items": { "type": "string" } }
	\\        }
	\\      },
	\\      "Tree": { "type": "array", "items": { "$ref": "#/components/schemas/Tree" } }
	\\    }
	\\  }
	\\}

test_api : OpenApi
test_api = OpenApi.parse(test_spec.to_utf8()) ?? crash "The test spec does not parse"

json_type : Str
json_type = "application/json"

form_type : Str
form_type = "application/x-www-form-urlencoded"

# A request with a body of `content_type`.
validate_as : Str, Str, Str, Str -> Try({}, _)
validate_as = |content_type, method, path, body| test_api.validate_request({ method, path, content_type, body: body.to_utf8() })

# A request with a JSON body.
validate : Str, Str, Str -> Try({}, _)
validate = |method, path, body| validate_as(json_type, method, path, body)

# The errors of an invalid request, on one line each.
errors_as : Str, Str, Str, Str -> List(Str)
errors_as = |content_type, method, path, body|
	match validate_as(content_type, method, path, body) {
		Err(Invalid(errors)) => errors.map(|error| error.to_str())
		_ => []
	}

# The keywords an invalid request with a JSON body fails on.
failed_keywords : Str, Str, Str -> List(Str)
failed_keywords = |method, path, body|
	match validate(method, path, body) {
		Err(Invalid(errors)) => errors.map(|error| error.keyword)
		_ => []
	}

# The messages an invalid request with a JSON body gets.
messages_for : Str, Str, Str -> List(Str)
messages_for = |method, path, body|
	match validate(method, path, body) {
		Err(Invalid(errors)) => errors.map(|error| error.message)
		_ => []
	}

expect validate("post", "/widgets", "{ \"name\": \"gadget\" }") == Ok({})

expect failed_keywords("post", "/widgets", "{}") == ["required"]

expect failed_keywords("post", "/widgets", "{ \"name\": 123 }") == ["type"]

# Methods are any case.
expect failed_keywords("POST", "/widgets", "{}") == ["required"]

expect failed_keywords("Post", "/widgets", "{}") == ["required"]

expect validate("post", "/nope", "{}") == Err(PathNotFound("/nope"))

# Paths

expect messages_for("PUT", "/widgets/42", "{}") == ["Missing required field: name"]

# A template is a path too.
expect messages_for("PUT", "/widgets/{id}", "{}") == ["Missing required field: name"]

# Literal text beats a variable, even though the spec lists the variable first.
expect messages_for("PUT", "/widgets/mine", "{}") == ["Missing required field: owner"]

# A variable inside a segment.
expect messages_for("POST", "/files/report.json", "{}") == ["Missing required field: data"]

expect messages_for("POST", "/files/a%20b.json", "{}") == ["Missing required field: data"]

# A whole URL, with the server's base path and a query.
expect messages_for("PUT", "https://api.example.com/v1/widgets/42?dry=1", "{}") == ["Missing required field: name"]

# The base path without the host.
expect messages_for("POST", "/v1/widgets", "{}") == ["Missing required field: name"]

expect validate("PUT", "/widgets/42/parts", "{}") == Err(PathNotFound("/widgets/42/parts"))

expect validate("POST", "/files/report.xml", "{}") == Err(PathNotFound("/files/report.xml"))

# The path decides the operation before the method is looked at.
expect validate("POST", "/widgets/42", "{}") == Err(MethodNotFound("POST"))

# The servers of a path replace the spec's.
upload_spec : Str
upload_spec =
	\\{
	\\  "servers": [{ "url": "https://api.example.com/v1" }],
	\\  "paths": {
	\\    "/files": {
	\\      "servers": [{ "url": "https://uploads.example.com/upload" }],
	\\      "post": { "requestBody": { "content": { "application/json": { "schema": { "required": ["data"] } } } } }
	\\    }
	\\  }
	\\}

upload : Str -> Try({}, _)
upload = |path| {
	api = OpenApi.parse(upload_spec.to_utf8()) ?? crash "The upload spec does not parse"
	api.validate_request({ method: "POST", path, content_type: json_type, body: "{ \"data\": 1 }".to_utf8() })
}

expect upload("https://uploads.example.com/upload/files") == Ok({})

expect upload("/files") == Ok({})

expect upload("/v1/files") == Err(PathNotFound("/v1/files"))

# MethodNotFound names the method as it was given.
expect validate("DELETE", "/widgets", "{}") == Err(MethodNotFound("DELETE"))

# A field of a path item that is not a method.
expect validate("parameters", "/widgets", "{}") == Err(MethodNotFound("parameters"))

expect validate("get", "/widgets", "{}") == Err(NoRequestBody)

# The operation is looked up before the body is read.
expect validate("get", "/widgets", "") == Err(NoRequestBody)

# Bodies

expect validate("post", "/widgets", "not json") == Err(BodyParseError("Invalid value at /"))

expect validate("post", "/widgets", "{ \"name\": \"gadget\", }") == Err(BodyParseError("Expected ',' and a field, or '}', after this field at /name"))

# An empty body is no body, which is valid unless the spec requires one,
# whatever the content type.
expect validate("post", "/widgets", "") == Ok({})

expect validate_as("", "post", "/notes", "") == Ok({})

# This one is required in a request body that is a $ref.
expect errors_as(form_type, "post", "/forms", "") == ["/: Missing required request body"]

# A body that is only whitespace is not empty, and not JSON either.
expect validate("post", "/widgets", " ") == Err(BodyParseError("Invalid value at /"))

# Forms

expect validate_as(form_type, "post", "/forms", "name=gadget&count=2&tags=a&tags=b") == Ok({})

expect validate_as("application/x-www-form-urlencoded; charset=utf-8", "post", "/forms", "name=gadget+one") == Ok({})

expect errors_as(form_type, "post", "/forms", "count=two&tags[0]=a&tags[1]=7") == ["/: Missing required field: name", "/count: Expected type integer but got string"]

expect errors_as(form_type, "post", "/forms", "name[first]=gadget") == ["/name: Expected type string but got object"]

expect validate_as(form_type, "post", "/forms", "name=a&name[b]=c") == Err(BodyParseError("Both a value and fields at /name"))

expect validate_as(form_type, "post", "/forms", "name=%E2%9C") == Err(BodyParseError("Invalid percent-encoding in %E2%9C"))

# A form's brackets count toward the depth.
expect validate_as(form_type, "post", "/forms", "name=a&deep${Str.repeat("[a]", 32)}=1") == Err(BodyParseError("Nested deeper than 32 levels"))

# Media types

# The spec takes only a form here.
expect validate("post", "/forms", "{ \"name\": \"gadget\" }") == Err(UnsupportedMediaType("application/json"))

# Of two media types, the one the request names.
expect errors_as("application/merge-patch+json", "PATCH", "/patches", "{}") == ["/: Missing required field: name"]

expect validate_as("text/plain; charset=utf-8", "PATCH", "/patches", "{}") == Ok({})

# Text is a string.
expect validate_as("text/plain", "post", "/notes", "hello") == Ok({})

expect errors_as("text/plain", "post", "/notes", "hello, world") == ["/: String length 12 exceeds maxLength 5"]

expect test_api.validate_request({ method: "post", path: "/notes", content_type: "text/plain", body: ['h', 0xFF] }) == Err(BodyParseError("Invalid UTF-8 at byte 1"))

expect validate("post", "/notes", "\"hello\"") == Err(UnsupportedMediaType("application/json"))

# Types this package cannot read, named without their parameters.
expect validate_as("multipart/form-data; boundary=x", "post", "/uploads", "--x--") == Err(CannotDecode("multipart/form-data"))

expect validate_as("image/png", "post", "/uploads", "PNG") == Err(CannotDecode("image/png"))

# application/* covers JSON.
expect errors_as(json_type, "post", "/uploads", "[]") == ["/: Expected type object but got array"]

expect validate_as("text/csv", "post", "/uploads", "a,b") == Err(UnsupportedMediaType("text/csv"))

# A media type without a schema takes any body that reads as its type.
expect validate("post", "/anything", "[1]") == Ok({})

expect validate("post", "/anything", "[1") == Err(BodyParseError("Expected ',' and a value, or ']', after this item at /0"))

expect validate_as("", "post", "/anything", "x") == Err(CannotDecode(""))

# Depth

nested : U64 -> Str
nested = |depth| Str.repeat("[", depth).concat(Str.repeat("]", depth))

expect validate("POST", "/trees", nested(32)) == Ok({})

expect validate("POST", "/trees", nested(33)) == Err(BodyParseError("Nested deeper than 32 levels"))

# Deep enough to overflow the stack if it were validated, so it is not.
expect validate("POST", "/trees", nested(2000)) == Err(BodyParseError("Nested deeper than 32 levels"))

expect {
	api = test_api.with_max_body_depth(64)
	deep = |depth| api.validate_request({ method: "POST", path: "/trees", content_type: json_type, body: nested(depth).to_utf8() })
	deep(64) == Ok({}) and deep(65) == Err(BodyParseError("Nested deeper than 64 levels"))
}

# One parsed spec validates any number of bodies.
expect {
	described = |result|
		match result {
			Ok({}) => ["valid"]
			Err(Invalid(errors)) => errors.map(|error| error.to_str())
			Err(_) => ["could not validate"]
		}
	bodies = ["{}", "{ \"name\": 1 }", "{ \"name\": \"ok\" }"]
	expected = [["/: Missing required field: name"], ["/name: Expected type string but got number"], ["valid"]]
	bodies.map(|body| described(validate("POST", "/widgets", body))) == expected
}

expect
	match OpenApi.parse("not json".to_utf8()) {
		Err(SpecParseError(message)) => message == "Invalid value at /"
		_ => Bool.False
	}

expect
	match OpenApi.parse("[]".to_utf8()) {
		Err(SpecParseError(message)) => message == "Expected an object but got array"
		_ => Bool.False
	}

# A spec without "paths" has no path to find.
expect {
	api = OpenApi.parse("{}".to_utf8()) ?? crash "{} is an object"
	api.validate_request({ method: "post", path: "/widgets", content_type: json_type, body: "{}".to_utf8() }) == Err(PathNotFound("/widgets"))
}

# Picking a media type

expect
	[
		"application/json",
		"Application/JSON; charset=utf-8",
		"text/json",
		"application/merge-patch+json",
		"application/vnd.github+json",
		"application/*+json",
	].all(|name| is_json(essence(name)))

expect
	[
		"application/x-ndjson",
		"application/json-seq",
		"application/jsonl",
		"text/plain",
		"application/x-www-form-urlencoded",
		"*/*",
	].all(|name| !is_json(essence(name)))

# The media type picked from a "content" that lists `names`, in order, for a
# request of `content_type`.
picked : List(Str), Str -> Try(Str, [UnsupportedMediaType(Str)])
picked = |names, content_type| {
	fields = Str.join_with(names.map(|name| "\"${name}\": \"${name}\""), ", ")
	request_body = JsonValue.parse("{ \"content\": {${fields}} }".to_utf8()) ?? crash "The test request body is JSON"
	find_media_type(request_body, content_type).map_ok(|media_type| media_type.get_str() ?? "")
}

expect picked(["text/plain", "application/json"], "application/json") == Ok("application/json")

# Parameters and case do not count.
expect picked(["application/json; charset=utf-8"], "Application/JSON") == Ok("application/json; charset=utf-8")

expect picked(["application/json"], "application/json; charset=utf-8") == Ok("application/json")

# The same type beats a wildcard, wherever the wildcard is.
expect picked(["*/*", "application/*", "application/json"], "application/json") == Ok("application/json")

expect picked(["*/*", "application/*"], "application/xml") == Ok("application/*")

expect picked(["text/*", "*/*"], "application/xml") == Ok("*/*")

# A "+json" type is a type of its own.
expect picked(["application/json"], "application/merge-patch+json") == Err(UnsupportedMediaType("application/merge-patch+json"))

# No content type matches only */*.
expect picked(["*/*"], "") == Ok("*/*")

expect picked(["application/json"], "") == Err(UnsupportedMediaType(""))
