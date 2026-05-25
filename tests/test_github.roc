app [main!] {
    pf: platform "https://github.com/growthagent/basic-cli/releases/download/0.27.0/G-A6F5ny0IYDx4hmF3t_YPHUSR28c9ZXMBnh0FEJjwk.tar.br",
    openapi: "../package/main.roc",
}

import pf.Arg
import pf.File
import pf.Stdout

import openapi.OpenApi

## Integration tests against the GitHub REST API OpenAPI spec (12MB, 326K lines).
## Tests: parsing at scale, required fields, type checking, oneOf validation.

main! : List Arg.Arg => Result {} _
main! = |_args|
    spec = File.read_bytes!("tests/github_api.json")
        |> Result.map_err(|e| SpecReadFailed(Inspect.to_str(e)))?

    # 1. Valid POST to create label: requires "name"
    errors1 = OpenApi.validate_request({
        spec,
        method: "post",
        path: "/repos/{owner}/{repo}/labels",
        body: Str.to_utf8("{\"name\": \"bug\", \"color\": \"ff0000\"}"),
    })?
    assert_no_errors("valid_label", errors1)?

    # 2. Missing required "name"
    errors2 = OpenApi.validate_request({
        spec,
        method: "post",
        path: "/repos/{owner}/{repo}/labels",
        body: Str.to_utf8("{\"color\": \"ff0000\"}"),
    })?
    assert_has_keyword("missing_name", errors2, "required")?

    # 3. Wrong type for name
    errors3 = OpenApi.validate_request({
        spec,
        method: "post",
        path: "/repos/{owner}/{repo}/labels",
        body: Str.to_utf8("{\"name\": 123}"),
    })?
    assert_has_keyword("wrong_type_name", errors3, "type")?

    # 4. oneOf: gists "public" field accepts boolean or string
    # Schema: public is oneOf: [{type: string, enum: [true, false]}, {type: boolean}]
    # (note: the enum values are strings "true"/"false" not booleans)
    errors4a = OpenApi.validate_request({
        spec,
        method: "post",
        path: "/gists",
        body: Str.to_utf8("{\"files\": {\"test.txt\": {\"content\": \"hello\"}}, \"public\": true}"),
    })?
    assert_no_errors("oneOf_public_boolean", errors4a)?

    errors4b = OpenApi.validate_request({
        spec,
        method: "post",
        path: "/gists",
        body: Str.to_utf8("{\"files\": {\"test.txt\": {\"content\": \"hello\"}}, \"public\": \"true\"}"),
    })?
    assert_no_errors("oneOf_public_string", errors4b)?

    # 5. oneOf: gists "public" rejects a number (matches neither boolean nor string enum)
    errors5 = OpenApi.validate_request({
        spec,
        method: "post",
        path: "/gists",
        body: Str.to_utf8("{\"files\": {\"test.txt\": {\"content\": \"hello\"}}, \"public\": 42}"),
    })?
    assert_has_keyword("oneOf_public_wrong_type", errors5, "oneOf")?

    Stdout.line!("GitHub: all tests passed")

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
