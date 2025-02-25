#!/usr/bin/env bash
set -e
set -x
set -o pipefail

PASS="lol"

[ -z "$SRC" ] && SRC="$(pwd)"
[ -z "$INNER_CMD" ] && INNER_CMD="cat"

function expect_eq() {
    [ "$1" = "$2" ] || {
        echo "$1 != $2"
        exit 1
    }
}

TMP="$(mktemp -d)"

cabal run "$SRC/tests/genPass.hs" "$PASS" | grep '|' > "$TMP/admin.pass"
cat "$TMP/admin.pass"

cabal install --project-dir="$SRC" --installdir="$TMP"
(cd "$TMP"; "$TMP/ttds-web" "$INNER_CMD") &
PID="$!"

# Give the server some time to start up.
sleep 1

################
# Raw Commands #
################

# Check basic functionality of /raw. Remember, our $INNER_CMD is cat.
RES="$(curl -v -X POST -H "Auth: $PASS" "localhost:8080/raw/test")"
expect_eq "$RES" "test"

###############################
# Pane creation and deletion. #
###############################

# Let's create a pane.
EX_TOK="$(curl --fail-with-body -v -X POST "localhost:8080/pane/ex/create?color=%23000000")"

# Now, let's try to delete it without our token. This should fail.
curl -v -X DELETE "localhost:8080/pane/ex" 2>&1 | grep 401

# Delete it correctly this time.
curl --fail-with-body -v -X DELETE -H "Auth: $EX_TOK" "localhost:8080/pane/ex"

kill "$PID"
rm -r "$TMP"
