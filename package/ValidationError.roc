## What is wrong with a request body, and where.
##
## `path` is where in the body, as JSON Pointer segments such as
## `["personalizations", "0", "to", "0", "email"]`. It is empty for the body
## itself.
##
## `message` describes the problem for people, such as
## `"Missing required field: from"`.
##
## `keyword` is the JSON Schema keyword that failed, such as `"type"`,
## `"required"` or `"minLength"`.
import JsonValue

ValidationError := { path : List(Str), message : Str, keyword : Str }.{

	## The error on one line, its place in the body first, as a JSON Pointer:
	##
	## ```
	## /personalizations/0/to/0/email: Expected type string but got number
	## /: Missing required field: from
	## ```
	##
	## `/` is the body itself.
	to_str : ValidationError -> Str
	to_str = |error| "${JsonValue.pointer(error.path)}: ${error.message}"

	is_eq : ValidationError, ValidationError -> Bool
	is_eq = |a, b| a.path == b.path and a.message == b.message and a.keyword == b.keyword
}

expect ValidationError.{ path: ["a", "0", "b"], message: "Wrong", keyword: "type" }.to_str() == "/a/0/b: Wrong"

expect ValidationError.{ path: [], message: "Missing required field: from", keyword: "required" }.to_str() == "/: Missing required field: from"

expect ValidationError.{ path: ["a/b", "~c"], message: "Wrong", keyword: "type" }.to_str() == "/a~1b/~0c: Wrong"
