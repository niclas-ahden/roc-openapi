app [main!] {
    pf: platform "https://github.com/growthagent/basic-cli/releases/download/0.27.0/G-A6F5ny0IYDx4hmF3t_YPHUSR28c9ZXMBnh0FEJjwk.tar.br",
    openapi: "../package/main.roc",
}

import pf.Arg
import pf.File
import pf.Stdout

import openapi.OpenApi

## Integration tests against the Twilio API v2010 OpenAPI spec (1.8MB, 46K lines).
## Tests: parsing at scale, nullable fields, type checking.
## Twilio uses no composition keywords: pure nullable + basic types.

main! : List Arg.Arg => Result {} _
main! = |_args|
    spec = File.read_bytes!("tests/twilio_api_v2010.json")
        |> Result.map_err(|e| SpecReadFailed(Inspect.to_str(e)))?

    # 1. Valid POST: FriendlyName is optional, so {} is valid
    errors1 = OpenApi.validate_request({
        spec,
        method: "post",
        path: "/2010-04-01/Accounts.json",
        body: Str.to_utf8("{}"),
    })?
    assert_no_errors("empty_body_valid", errors1)?

    # 2. Valid POST with FriendlyName
    errors2 = OpenApi.validate_request({
        spec,
        method: "post",
        path: "/2010-04-01/Accounts.json",
        body: Str.to_utf8("{\"FriendlyName\": \"Test Account\"}"),
    })?
    assert_no_errors("with_friendly_name", errors2)?

    # 3. Wrong type for FriendlyName
    errors3 = OpenApi.validate_request({
        spec,
        method: "post",
        path: "/2010-04-01/Accounts.json",
        body: Str.to_utf8("{\"FriendlyName\": 12345}"),
    })?
    assert_has_keyword("wrong_type_friendly_name", errors3, "type")?

    # 4. Null for FriendlyName: Twilio marks fields as nullable
    # Since FriendlyName is not required and nullable, null should pass
    errors4 = OpenApi.validate_request({
        spec,
        method: "post",
        path: "/2010-04-01/Accounts.json",
        body: Str.to_utf8("{\"FriendlyName\": null}"),
    })?
    assert_no_errors("null_friendly_name", errors4)?

    Stdout.line!("Twilio: all tests passed")

assert_no_errors : Str, List OpenApi.ValidationError => Result {} _
assert_no_errors = |name, errors|
    if List.is_empty(errors) then
        Ok({})
    else
        Err(TestFailed(name, errors))

assert_has_keyword : Str, List OpenApi.ValidationError, Str => Result {} _
assert_has_keyword = |name, errors, keyword|
    if List.any(errors, |e| e.keyword == keyword) then
        Ok({})
    else
        Err(TestExpectedKeyword(name, keyword, errors))
