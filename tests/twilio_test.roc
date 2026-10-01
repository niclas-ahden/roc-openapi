app [main!] {
	pf: platform "https://github.com/niclas-ahden/basic-cli/releases/download/0.27.0/HZanbveSUDoJF8LypR663eH7PpaKEKG36eErEQzmV1Qs.tar.zst",
	openapi: "../package/main.roc",
	spec: "https://github.com/niclas-ahden/roc-spec/releases/download/0.6.0/9ThTkhd7zrviwQpM3LvGd7pvzGhr4ZXNmWJV7pTJc9AJ.tar.zst",
}

import pf.OsStr exposing [OsStr]
import pf.Path
import openapi.OpenApi
import spec.Assert

## Form bodies against the Twilio API v2010 spec (1.8 MB, 46K lines), which
## takes every request body as a form, with integer, boolean and list fields,
## patterns, and no composition keywords.
main! : List(OsStr) => Try({}, _)
main! = |_args| {
	api = OpenApi.parse(Path.read_bytes!(Path.utf8("tests/twilio_api_v2010.json"))?)?
	accounts = "/2010-04-01/Accounts.json"

	# Every field is optional, so no body at all is fine.
	Assert.eq(validate(api, "POST", accounts, ""), Ok({}))?
	Assert.eq(validate(api, "POST", accounts, "FriendlyName=Test+Account"), Ok({}))?

	# Twilio takes forms only.
	Assert.eq(
		api.validate_request({ method: "POST", path: accounts, content_type: "application/json", body: "{}".to_utf8() }),
		Err(UnsupportedMediaType("application/json")),
	)?

	# A call needs "To" and "From". "Timeout" is an integer, "Record" a
	# boolean, and "StatusCallbackEvent" a list, given once per item.
	calls = "/2010-04-01/Accounts/{AccountSid}/Calls.json"
	call = "To=%2B15550001&From=%2B15550002"
	Assert.eq(validate(api, "POST", calls, call), Ok({}))?
	Assert.eq(validate(api, "POST", calls, "${call}&Timeout=30&Record=true&StatusCallbackEvent=initiated&StatusCallbackEvent=ringing"), Ok({}))?
	Assert.eq(failed_keywords(api, "POST", calls, "Timeout=30"), Ok(["required", "required"]))?
	Assert.eq(errors(api, "POST", calls, "${call}&Timeout=soon&Record=yes"), Ok(["/Timeout: Expected type integer but got string", "/Record: Expected type boolean but got string"]))?

	# An application SID is "AP" and 32 hex digits.
	sid = "AP${Str.repeat("0a", 16)}"
	Assert.eq(validate(api, "POST", calls, "${call}&ApplicationSid=${sid}"), Ok({}))?
	# Of the right length, but not hex.
	Assert.eq(failed_keywords(api, "POST", calls, "${call}&ApplicationSid=AP${Str.repeat("g", 32)}"), Ok(["pattern"]))?

	# A request's own URL, matched against the templates. The SID in
	# "{Sid}.json" shares its segment with literal text.
	account = "https://api.twilio.com/2010-04-01/Accounts/AC${Str.repeat("0a", 16)}"
	Assert.eq(validate(api, "POST", "${account}/Calls.json", "${call}&ApplicationSid=${sid}"), Ok({}))?
	Assert.eq(failed_keywords(api, "POST", "${account}/Calls/CA${Str.repeat("0a", 16)}.json", "Method=PUT"), Ok(["enum"]))
}

## Whether a request with the form `body` is valid.
validate : OpenApi, Str, Str, Str -> Try({}, _)
validate = |api, method, path, body| api.validate_request({ method, path, content_type: "application/x-www-form-urlencoded", body: body.to_utf8() })

## The keywords an invalid request with the form `body` fails on.
failed_keywords : OpenApi, Str, Str, Str -> Try(List(Str), [NotInvalid(Str)])
failed_keywords = |api, method, path, body|
	match validate(api, method, path, body) {
		Err(Invalid(problems)) => Ok(problems.map(|problem| problem.keyword))
		other => Err(NotInvalid(Str.inspect(other)))
	}

## The errors of an invalid request with the form `body`, on one line each.
errors : OpenApi, Str, Str, Str -> Try(List(Str), [NotInvalid(Str)])
errors = |api, method, path, body|
	match validate(api, method, path, body) {
		Err(Invalid(problems)) => Ok(problems.map(|problem| problem.to_str()))
		other => Err(NotInvalid(Str.inspect(other)))
	}
