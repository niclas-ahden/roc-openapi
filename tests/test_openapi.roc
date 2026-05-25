app [main!] {
    pf: platform "https://github.com/growthagent/basic-cli/releases/download/0.27.0/G-A6F5ny0IYDx4hmF3t_YPHUSR28c9ZXMBnh0FEJjwk.tar.br",
    openapi: "../package/main.roc",
}

import pf.Arg
import pf.File
import pf.Stdout

import openapi.OpenApi

## Integration tests: validate real payloads against the SendGrid OpenAPI spec.

main! : List Arg.Arg => Result {} _
main! = |_args|
    spec = File.read_bytes!("tests/tsg_mail_v3.json")
        |> Result.map_err(|e| SpecReadFailed(Inspect.to_str(e)))?

    # 1. Valid full payload: should produce 0 errors
    run_test!("valid_full", spec, valid_full_payload, |errors|
        if List.len(errors) == 0 then
            Ok({})
        else
            Err(ExpectedNoErrors(errors))
    )?

    # 2. Valid minimal payload: should produce 0 errors
    run_test!("valid_minimal", spec, valid_minimal_payload, |errors|
        if List.len(errors) == 0 then
            Ok({})
        else
            Err(ExpectedNoErrors(errors))
    )?

    # 3. Missing required field (no "from"): should error
    run_test!("missing_from", spec, missing_from_payload, |errors|
        has_required_error = List.any(errors, |e| e.keyword == "required" && Str.contains(e.message, "from"))
        if has_required_error then
            Ok({})
        else
            Err(ExpectedRequiredError(errors))
    )?

    # 4. Wrong type (personalizations is a string, not array): should error
    run_test!("wrong_type", spec, wrong_type_payload, |errors|
        has_type_error = List.any(errors, |e| e.keyword == "type")
        if has_type_error then
            Ok({})
        else
            Err(ExpectedTypeError(errors))
    )?

    # 5. Invalid nested field (email is a number): should error with deep path
    run_test!("invalid_nested", spec, invalid_nested_payload, |errors|
        has_deep_error = List.any(errors, |e| List.len(e.path) > 2)
        if has_deep_error then
            Ok({})
        else
            Err(ExpectedDeepError(errors))
    )?

    # 6. Empty body {}: should error for missing personalizations and from
    run_test!("empty_body", spec, Str.to_utf8("{}"), |errors|
        missing_count = List.count_if(errors, |e| e.keyword == "required")
        if missing_count >= 2 then
            Ok({})
        else
            Err(ExpectedMultipleRequiredErrors(errors))
    )?

    # 7. Path not found
    path_result = OpenApi.validate_request({ spec, method: "post", path: "/v3/nonexistent", body: Str.to_utf8("{}") })
    when path_result is
        Err(PathNotFound(_)) -> {}
        _ -> Err(ExpectedPathNotFound)?

    # 8. Method not found
    method_result = OpenApi.validate_request({ spec, method: "delete", path: "/v3/mail/send", body: Str.to_utf8("{}") })
    when method_result is
        Err(MethodNotFound(_)) -> {}
        _ -> Err(ExpectedMethodNotFound)?

    # 9. Invalid JSON body
    invalid_json_result = OpenApi.validate_request({ spec, method: "post", path: "/v3/mail/send", body: Str.to_utf8("not json") })
    when invalid_json_result is
        Err(BodyParseError(_)) -> {}
        _ -> Err(ExpectedBodyParseError)?

    # 10. Empty byte body (not even {})
    empty_result = OpenApi.validate_request({ spec, method: "post", path: "/v3/mail/send", body: [] })
    when empty_result is
        Err(BodyParseError(_)) -> {}
        _ -> Err(ExpectedBodyParseErrorForEmpty)?

    Stdout.line!("All integration tests passed!")

run_test! : Str, List U8, List U8, (List OpenApi.ValidationError -> Result {} _) => Result {} _
run_test! = |name, spec, body, check|
    errors = OpenApi.validate_request({ spec, method: "post", path: "/v3/mail/send", body })?
    check(errors) |> Result.map_err(|e| TestFailed(name, Inspect.to_str(e)))

# ── Test payloads ──

valid_full_payload =
    Str.to_utf8(
        """
        {
            "personalizations": [{
                "to": [{"email": "to@example.com", "name": "Recipient"}],
                "cc": [{"email": "cc@example.com"}],
                "bcc": [{"email": "bcc@example.com"}],
                "subject": "Full test"
            }],
            "from": {"email": "sender@example.com", "name": "Sender"},
            "content": [{"type": "text/plain", "value": "Hello"}, {"type": "text/html", "value": "<p>Hello</p>"}],
            "reply_to": {"email": "reply@example.com"}
        }
        """,
    )

valid_minimal_payload =
    Str.to_utf8(
        """
        {
            "personalizations": [{
                "to": [{"email": "to@example.com"}]
            }],
            "from": {"email": "sender@example.com"},
            "subject": "Minimal"
        }
        """,
    )

missing_from_payload =
    Str.to_utf8(
        """
        {
            "personalizations": [{
                "to": [{"email": "to@example.com"}]
            }],
            "subject": "No from"
        }
        """,
    )

wrong_type_payload =
    Str.to_utf8(
        """
        {
            "personalizations": "not an array",
            "from": {"email": "sender@example.com"}
        }
        """,
    )

invalid_nested_payload =
    Str.to_utf8(
        """
        {
            "personalizations": [{
                "to": [{"email": 12345}]
            }],
            "from": {"email": "sender@example.com"}
        }
        """,
    )
