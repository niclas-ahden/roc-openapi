#!/usr/bin/env bash
set -eo pipefail

cd "$(dirname "$0")"

FILTER="${1:-}"
FAILED=0
PASSED=0
SKIPPED=0


# Unit tests (expect blocks via roc test)
for test_file in package/*.roc; do
    test_name=$(basename "$test_file" .roc)

    if [[ -n "$FILTER" && ! "$test_name" =~ $FILTER ]]; then
        continue
    fi

    # Skip files with no `expect` blocks (e.g. package/main.roc).
    if ! grep -q '^expect' "$test_file"; then
        continue
    fi

    echo -n "Running: $test_name ... "
    if output=$(roc test "$test_file" --linker=legacy 2>&1); then
        echo "PASS"
        PASSED=$((PASSED + 1))
    else
        echo "FAIL"
        echo "$output"
        FAILED=$((FAILED + 1))
    fi
done

# Integration tests (app binaries via roc dev)
for test_file in tests/test_*.roc; do
    test_name=$(basename "$test_file" .roc)

    if [[ -n "$FILTER" && ! "$test_name" =~ $FILTER ]]; then
        continue
    fi

    echo -n "Running: $test_name ... "
    if output=$(roc dev "$test_file" --linker=legacy 2>&1); then
        echo "PASS"
        PASSED=$((PASSED + 1))
    else
        echo "FAIL"
        echo "$output"
        FAILED=$((FAILED + 1))
    fi
done

echo
echo "Results: $PASSED passed, $FAILED failed"

if [[ $FAILED -gt 0 ]]; then
    exit 1
fi
