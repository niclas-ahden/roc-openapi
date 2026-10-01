app [main!] {
	pf: platform "https://github.com/niclas-ahden/basic-cli/releases/download/0.27.0/HZanbveSUDoJF8LypR663eH7PpaKEKG36eErEQzmV1Qs.tar.zst",
	openapi: "../package/main.roc",
	spec: "https://github.com/niclas-ahden/roc-spec/releases/download/0.6.0/9ThTkhd7zrviwQpM3LvGd7pvzGhr4ZXNmWJV7pTJc9AJ.tar.zst",
}

import pf.OsStr exposing [OsStr]
import pf.Path
import openapi.OpenApi
import spec.Assert

## Mail send payloads against the SendGrid v3 spec, and the errors
## `validate_request` gives before it gets as far as validating.
main! : List(OsStr) => Try({}, _)
main! = |_args| {
	api = OpenApi.parse(Path.read_bytes!(Path.utf8("tests/tsg_mail_v3.json"))?)?
	send = |body| api.validate_request({ method: "POST", path: "/v3/mail/send", content_type: "application/json", body: body.to_utf8() })

	Assert.eq(send(valid_full_payload), Ok({}))?
	Assert.eq(send(valid_minimal_payload), Ok({}))?

	# A request's own URL.
	Assert.eq(api.validate_request({ method: "POST", path: "https://api.sendgrid.com/v3/mail/send", content_type: "application/json", body: valid_minimal_payload.to_utf8() }), Ok({}))?

	missing_from = errors_of(send(missing_from_payload))?
	Assert.eq(missing_from.map(|error| error.message), ["Missing required field: from"])?

	# "personalizations" is a string where the spec wants an array.
	wrong_type = errors_of(send(wrong_type_payload))?
	Assert.eq(wrong_type.map(|error| (error.path, error.keyword)), [(["personalizations"], "type")])?

	# An email that is a number, four levels down.
	invalid_nested = errors_of(send(invalid_nested_payload))?
	Assert.eq(invalid_nested.map(|error| error.to_str()), ["/personalizations/0/to/0/email: Expected type string but got number"])?

	empty_body = errors_of(send("{}"))?
	Assert.eq(empty_body.map(|error| error.message), ["Missing required field: personalizations", "Missing required field: from"])?

	Assert.eq(
		api.validate_request({ method: "POST", path: "/v3/nonexistent", content_type: "application/json", body: "{}".to_utf8() }),
		Err(PathNotFound("/v3/nonexistent")),
	)?
	Assert.eq(
		api.validate_request({ method: "DELETE", path: "/v3/mail/send", content_type: "application/json", body: "{}".to_utf8() }),
		Err(MethodNotFound("DELETE")),
	)?
	Assert.eq(
		api.validate_request({ method: "POST", path: "/v3/mail/send", content_type: "application/x-www-form-urlencoded", body: "from=a".to_utf8() }),
		Err(UnsupportedMediaType("application/x-www-form-urlencoded")),
	)?
	Assert.eq(send("not json"), Err(BodyParseError("Invalid value at /")))?
	Assert.eq(send("{\"from\": }"), Err(BodyParseError("Invalid value at /from")))
}

## The errors of a body that breaks the spec.
errors_of = |result|
	match result {
		Err(Invalid(errors)) => Ok(errors)
		other => Err(NotInvalid(Str.inspect(other)))
	}

valid_full_payload : Str
valid_full_payload =
	\\{
	\\    "personalizations": [{
	\\        "to": [{"email": "to@example.com", "name": "Recipient"}],
	\\        "cc": [{"email": "cc@example.com"}],
	\\        "bcc": [{"email": "bcc@example.com"}],
	\\        "subject": "Full test"
	\\    }],
	\\    "from": {"email": "sender@example.com", "name": "Sender"},
	\\    "content": [{"type": "text/plain", "value": "Hello"}, {"type": "text/html", "value": "<p>Hello</p>"}],
	\\    "reply_to": {"email": "reply@example.com"}
	\\}

valid_minimal_payload : Str
valid_minimal_payload =
	\\{
	\\    "personalizations": [{
	\\        "to": [{"email": "to@example.com"}]
	\\    }],
	\\    "from": {"email": "sender@example.com"},
	\\    "subject": "Minimal"
	\\}

missing_from_payload : Str
missing_from_payload =
	\\{
	\\    "personalizations": [{
	\\        "to": [{"email": "to@example.com"}]
	\\    }],
	\\    "subject": "No from"
	\\}

wrong_type_payload : Str
wrong_type_payload =
	\\{
	\\    "personalizations": "not an array",
	\\    "from": {"email": "sender@example.com"}
	\\}

invalid_nested_payload : Str
invalid_nested_payload =
	\\{
	\\    "personalizations": [{
	\\        "to": [{"email": 12345}]
	\\    }],
	\\    "from": {"email": "sender@example.com"}
	\\}
