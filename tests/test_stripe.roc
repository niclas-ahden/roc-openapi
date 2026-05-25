app [main!] {
    pf: platform "https://github.com/growthagent/basic-cli/releases/download/0.27.0/G-A6F5ny0IYDx4hmF3t_YPHUSR28c9ZXMBnh0FEJjwk.tar.br",
    openapi: "../package/main.roc",
}

import pf.Arg
import pf.File
import pf.Stdout

import openapi.OpenApi

## Integration tests against the Stripe OpenAPI spec (7.4MB, 196K lines).
## Tests: parsing at scale, required fields, type checking, anyOf validation.

main! : List Arg.Arg => Result {} _
main! = |_args|
    spec = File.read_bytes!("tests/stripe_spec3.json")
        |> Result.map_err(|e| SpecReadFailed(Inspect.to_str(e)))?

    # 1. Valid POST: requires "account" and "type"
    errors1 = OpenApi.validate_request({
        spec,
        method: "post",
        path: "/v1/account_links",
        body: Str.to_utf8("{\"account\": \"acct_123\", \"type\": \"account_onboarding\"}"),
    })?
    assert_no_errors("valid_account_link", errors1)?

    # 2. Missing required fields
    errors2 = OpenApi.validate_request({
        spec,
        method: "post",
        path: "/v1/account_links",
        body: Str.to_utf8("{}"),
    })?
    assert_has_keyword("missing_required", errors2, "required")?

    # 3. anyOf: metadata accepts an object (key-value pairs)
    # Stripe schema: metadata is anyOf: [{type: object, additionalProperties: string}, {enum: [""], type: string}]
    errors3 = OpenApi.validate_request({
        spec,
        method: "post",
        path: "/v1/accounts",
        body: Str.to_utf8("{\"metadata\": {\"key1\": \"value1\"}}"),
    })?
    assert_no_errors("anyOf_metadata_object", errors3)?

    # 4. anyOf: metadata also accepts empty string "" (to unset)
    errors4 = OpenApi.validate_request({
        spec,
        method: "post",
        path: "/v1/accounts",
        body: Str.to_utf8("{\"metadata\": \"\"}"),
    })?
    assert_no_errors("anyOf_metadata_empty_string", errors4)?

    # 5. anyOf: metadata rejects a number (matches neither sub-schema)
    errors5 = OpenApi.validate_request({
        spec,
        method: "post",
        path: "/v1/accounts",
        body: Str.to_utf8("{\"metadata\": 12345}"),
    })?
    assert_has_keyword("anyOf_metadata_wrong_type", errors5, "anyOf")?

    # 6. Wrong type for a simple string field
    errors6 = OpenApi.validate_request({
        spec,
        method: "post",
        path: "/v1/apple_pay/domains",
        body: Str.to_utf8("{\"domain_name\": 12345}"),
    })?
    assert_has_keyword("wrong_type", errors6, "type")?

    Stdout.line!("Stripe: all tests passed")

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
