app [main!] {
	pf: platform "https://github.com/niclas-ahden/basic-cli/releases/download/0.27.0/HZanbveSUDoJF8LypR663eH7PpaKEKG36eErEQzmV1Qs.tar.zst",
	openapi: "../package/main.roc",
	spec: "https://github.com/niclas-ahden/roc-spec/releases/download/0.6.0/9ThTkhd7zrviwQpM3LvGd7pvzGhr4ZXNmWJV7pTJc9AJ.tar.zst",
}

import pf.OsStr exposing [OsStr]
import pf.Path
import openapi.OpenApi
import spec.Assert

## Form bodies against the Stripe spec (7.4 MB, 196K lines): parsing at scale,
## required fields, fields in brackets, and anyOf between a type and the empty
## string that unsets a field.
main! : List(OsStr) => Try({}, _)
main! = |_args| {
	api = OpenApi.parse(Path.read_bytes!(Path.utf8("tests/stripe_spec3.json"))?)?

	# An account link requires "account" and "type", and a body.
	Assert.eq(validate(api, "POST", "/v1/account_links", "account=acct_123&type=account_onboarding"), Ok({}))?
	Assert.eq(failed_keywords(api, "POST", "/v1/account_links", "expand[]=account"), Ok(["required", "required"]))?
	Assert.eq(errors(api, "POST", "/v1/account_links", ""), Ok(["/: Missing required request body"]))?

	# An account's "metadata" is anyOf an object of strings and the empty
	# string, which unsets it. Text alone is neither.
	Assert.eq(validate(api, "POST", "/v1/accounts", "metadata[order_id]=6735&metadata[note]="), Ok({}))?
	Assert.eq(validate(api, "POST", "/v1/accounts", "metadata="), Ok({}))?
	Assert.eq(failed_keywords(api, "POST", "/v1/accounts", "metadata=12345"), Ok(["anyOf"]))?

	# A subscription's items are numbered, and their fields in brackets.
	subscriptions = "/v1/subscriptions"
	Assert.eq(validate(api, "POST", subscriptions, "customer=cus_1&items[0][price]=price_1&items[0][quantity]=2&items[1][price]=price_2"), Ok({}))?
	Assert.eq(validate(api, "POST", subscriptions, "customer=cus_1&expand[]=customer&expand[]=latest_invoice&cancel_at_period_end=true"), Ok({}))?
	Assert.eq(errors(api, "POST", subscriptions, "customer=cus_1&items[0][quantity]=two"), Ok(["/items/0/quantity: Expected type integer but got string"]))?
	Assert.eq(errors(api, "POST", subscriptions, "customer=cus_1&items[0][price_data][recurring][interval]=hour"), Ok(["/items/0/price_data: Missing required field: currency", "/items/0/price_data: Missing required field: product", "/items/0/price_data/recurring/interval: Value \"hour\" not in enum [\"day\", \"month\", \"week\", \"year\"]"]))?

	# "cancel_at" is a timestamp or a name, and "application_fee_percent" a
	# number or the empty string.
	Assert.eq(validate(api, "POST", subscriptions, "customer=cus_1&cancel_at=1700000000&application_fee_percent=12.5"), Ok({}))?
	Assert.eq(validate(api, "POST", subscriptions, "customer=cus_1&cancel_at=max_period_end&application_fee_percent="), Ok({}))?
	Assert.eq(failed_keywords(api, "POST", subscriptions, "customer=cus_1&cancel_at=soon"), Ok(["anyOf"]))
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
