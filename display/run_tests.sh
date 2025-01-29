#!/usr/bin/env sh

PREFIX='expect-'
dump_dir='./tests'

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
	echo "Usage: $0 [--help] [DUMP_DIR]"
	echo "Compare dumped pixel buffers in DUMP_DIR against their expected counterparts (whose filenames are prefixed with '$PREFIX')."
	echo "DUMP_DIR is '$dump_dir' by default."
	exit 0
fi

if [ "$#" -gt 1 ]; then
	echo "$0: too many arguments (try --help for help)"
	exit 1
elif [ "$#" -eq 1 ]; then
	dump_dir="$1"
fi

find "$dump_dir" -type f -name "${PREFIX}*.data" |
{
	count_pass=0
	count_fail=0

	while IFS= read -r expect_path
	do
		test_name=$(basename "$expect_path")
		test_name=${test_name#"$PREFIX"}
		test_dir=$(dirname "$expect_path")
		test_path="${test_dir}/${test_name}"

		if diff "$expect_path" "$test_path"; then
			echo "$0: $test_path PASSED"
			count_pass=$((count_pass + 1))
		else
			echo "$0: $test_path FAILED"
			count_fail=$((count_fail + 1))
		fi
	done
	count_total=$((count_pass + count_fail))

	echo "$count_pass/$count_total tests PASSED"
	if [ $count_fail -eq 0 ]; then
		exit 0
	else
		exit 1
	fi
}
