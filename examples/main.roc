app [main!] {
	pf: platform "https://github.com/niclas-ahden/basic-cli/releases/download/0.27.0/HZanbveSUDoJF8LypR663eH7PpaKEKG36eErEQzmV1Qs.tar.zst",
	openapi: "../package/main.roc",
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
