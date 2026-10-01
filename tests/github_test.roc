app [main!] {
	pf: platform "https://github.com/niclas-ahden/basic-cli/releases/download/0.27.0/HZanbveSUDoJF8LypR663eH7PpaKEKG36eErEQzmV1Qs.tar.zst",
	openapi: "../package/main.roc",
	spec: "https://github.com/niclas-ahden/roc-spec/releases/download/0.6.0/9ThTkhd7zrviwQpM3LvGd7pvzGhr4ZXNmWJV7pTJc9AJ.tar.zst",
}

import pf.OsStr exposing [OsStr]
import pf.Path
import openapi.OpenApi
import spec.Assert

## Payloads against the GitHub REST API spec (12 MB, 326K lines): parsing at
## scale, required fields, type checks, oneOf, minItems and pattern.
main! : List(OsStr) => Try({}, _)
main! = |_args| {
	api = OpenApi.parse(Path.read_bytes!(Path.utf8("tests/github_api.json"))?)?

	# Creating a label requires "name".
	labels = "/repos/{owner}/{repo}/labels"
	Assert.eq(validate(api, "POST", labels, "{\"name\": \"bug\", \"color\": \"ff0000\"}"), Ok({}))?
	Assert.eq(failed_keywords(api, "POST", labels, "{\"color\": \"ff0000\"}"), Ok(["required"]))?
	Assert.eq(failed_keywords(api, "POST", labels, "{\"name\": 123}"), Ok(["type"]))?

	# A request's own path or URL, matched against the templates.
	Assert.eq(failed_keywords(api, "POST", "/repos/roc-lang/roc/labels", "{\"color\": \"ff0000\"}"), Ok(["required"]))?
	Assert.eq(validate(api, "POST", "https://api.github.com/repos/roc-lang/roc/labels", "{\"name\": \"bug\"}"), Ok({}))?

	# "generate-jitconfig" is literal text, which beats the {runner_id} beside
	# it, a path with no POST.
	Assert.eq(failed_keywords(api, "POST", "/orgs/acme/actions/runners/generate-jitconfig", "{}"), Ok(["required", "required", "required"]))?

	# A gist's "public" is oneOf a boolean and a string enum of "true" and
	# "false", so a number matches neither.
	Assert.eq(validate(api, "POST", "/gists", "{\"files\": {\"test.txt\": {\"content\": \"hello\"}}, \"public\": true}"), Ok({}))?
	Assert.eq(validate(api, "POST", "/gists", "{\"files\": {\"test.txt\": {\"content\": \"hello\"}}, \"public\": \"true\"}"), Ok({}))?
	Assert.eq(failed_keywords(api, "POST", "/gists", "{\"files\": {\"test.txt\": {\"content\": \"hello\"}}, \"public\": 42}"), Ok(["oneOf"]))?

	# A runner gets at least one label.
	runner_labels = "/orgs/{org}/actions/runners/{runner_id}/labels"
	Assert.eq(validate(api, "POST", runner_labels, "{\"labels\": [\"gpu\"]}"), Ok({}))?
	Assert.eq(failed_keywords(api, "POST", runner_labels, "{\"labels\": []}"), Ok(["minItems"]))?

	# A secret's value is base64.
	secret = "/orgs/{org}/actions/secrets/{secret_name}"
	secret_body = |value| "{\"encrypted_value\": \"${value}\", \"key_id\": \"1\", \"visibility\": \"all\"}"
	Assert.eq(validate(api, "PUT", secret, secret_body("aGVsbG8=")), Ok({}))?
	Assert.eq(failed_keywords(api, "PUT", secret, secret_body("not base64!")), Ok(["pattern"]))
}

## Whether a request with `body` is valid.
validate : OpenApi, Str, Str, Str -> Try({}, _)
validate = |api, method, path, body| api.validate_request({ method, path, content_type: "application/json", body: body.to_utf8() })

## The keywords an invalid request with `body` fails on.
failed_keywords : OpenApi, Str, Str, Str -> Try(List(Str), [NotInvalid(Str)])
failed_keywords = |api, method, path, body|
	match validate(api, method, path, body) {
		Err(Invalid(errors)) => Ok(errors.map(|error| error.keyword))
		other => Err(NotInvalid(Str.inspect(other)))
	}
