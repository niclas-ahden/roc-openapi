#!/usr/bin/env roc
## Refreshes the OpenAPI specs that tests/*_test.roc validate against from
## their upstream sources. The specs are committed so that CI does not have to
## hit the network on every run. Run this when you want to test against an
## updated upstream spec, then run ./tests.roc and commit what changed.
##
## Run from the repository root.
app [main!] {
	pf: platform "https://github.com/niclas-ahden/basic-cli/releases/download/0.27.0/HZanbveSUDoJF8LypR663eH7PpaKEKG36eErEQzmV1Qs.tar.zst",
	# The request and response types basic-cli's Http module takes and returns.
	http: "https://github.com/roc-lang/http/releases/download/2.0.0/6ZUwqYhCS8PU9Mo6MF7oV82ET2o7KYb57CLKDq4cq4sS.tar.zst",
}

import pf.Http
import pf.OsStr exposing [OsStr]
import pf.Path
import pf.Stdout
import http.Request
import http.Response

fixtures : List({ name : Str, file : Str, url : Str })
fixtures = [
	{
		name: "SendGrid mail v3",
		file: "tests/tsg_mail_v3.json",
		url: "https://raw.githubusercontent.com/twilio/sendgrid-oai/main/spec/json/tsg_mail_v3.json",
	},
	{
		name: "Twilio API v2010",
		file: "tests/twilio_api_v2010.json",
		url: "https://raw.githubusercontent.com/twilio/twilio-oai/main/spec/json/twilio_api_v2010.json",
	},
	{
		name: "Stripe",
		file: "tests/stripe_spec3.json",
		url: "https://raw.githubusercontent.com/stripe/openapi/master/openapi/spec3.json",
	},
	{
		name: "GitHub REST API",
		file: "tests/github_api.json",
		url: "https://raw.githubusercontent.com/github/rest-api-description/main/descriptions/api.github.com/api.github.com.json",
	},
]

main! : List(OsStr) => Try({}, _)
main! = |_args| {
	for { name, file, url } in fixtures {
		Stdout.line!("Downloading the ${name} spec...")?
		response = Http.send!(Request.from_method(GET).with_uri(url))?

		# Anything but a 200 has an error page for a body, which must not
		# replace the spec.
		status = Response.status(response)
		if status != 200 {
			return Err(DownloadFailed({ url, status }))
		}

		body = Response.body(response)
		Path.write_bytes!(Path.utf8(file), body)?
		Stdout.line!("  ${body.len().to_str()} bytes to ${file}")?
	}

	Stdout.line!("Done. Run ./tests.roc to test.")
}
