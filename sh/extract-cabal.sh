#!/bin/sh

BUILD_DIR=${1:-"."}
BUILD_PLAN="$BUILD_DIR/dist-newstyle/cache/plan.json"
if [ ! -f "$BUILD_PLAN" ]; then
    echo "Build plan does not exists in directory $BUILD_DIR"
    exit 1
fi

# JQ query as heredoc (we need to double escape slashes)
read -d '' JQ_QUERY << EOF
."install-plan" |
 map(select(.type             == "configured" and
            ."pkg-src".type   == "repo-tar" and
            ."component-name" == "lib") |
     (."pkg-name" + ": \\\\"" + ."pkg-version" + "\\\\"")) |
 join("\\\\n")
EOF

jq -r < "$BUILD_PLAN" "$JQ_QUERY" | \
    awk '{$1 = sprintf("%-32s", $1)} 1'

