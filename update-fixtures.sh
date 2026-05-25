#!/usr/bin/env bash

# Refresh the committed OpenAPI spec fixtures used by tests/test_*.roc from
# their upstream sources. The fixtures are committed so CI does not have to
# hit the network on every run; re-run this script when you want to test
# against an updated upstream spec, then commit the resulting changes.
#
# Usage: ./update-fixtures.sh

set -euo pipefail

cd "$(dirname "$0")/tests"

echo "Downloading Twilio API v2010 spec..."
curl -sS "https://raw.githubusercontent.com/twilio/twilio-oai/main/spec/json/twilio_api_v2010.json" > twilio_api_v2010.json
echo "  $(wc -c < twilio_api_v2010.json | tr -d ' ') bytes"

echo "Downloading Stripe spec..."
curl -sS "https://raw.githubusercontent.com/stripe/openapi/master/openapi/spec3.json" > stripe_spec3.json
echo "  $(wc -c < stripe_spec3.json | tr -d ' ') bytes"

echo "Downloading GitHub REST API spec..."
curl -sS "https://raw.githubusercontent.com/github/rest-api-description/main/descriptions/api.github.com/api.github.com.json" > github_api.json
echo "  $(wc -c < github_api.json | tr -d ' ') bytes"

echo "Done. Run ./tests.sh to test."
