#!/bin/bash

: "${AOC_YEAR:=2024}"

get_result() {
	day_no_dec="${1#dec*}"
	day_unpadded="${day_no_dec#0*}"
	curl --silent --show-error --fail --cookie aoc_cookie.txt "https://adventofcode.com/$AOC_YEAR/day/$day_unpadded" | grep -oP '<p>Your puzzle answer was <code>\d*</code>' | grep -oP '\d*'
}

check() {
	EXPECTED="$(get_result "$1")"
	if [ "$(echo "$EXPECTED" | wc -l)" -ne 2 ]; then
		echo "Expected result for $1 has wrong number of lines ($(echo "$EXPECTED" | wc -l))"
		return 1
	fi
	ACTUAL=$(make --silent "$1")
	if [ "$(echo "$ACTUAL" | wc -l)" -ne 2 ]; then
		echo "Actual result for $1 has wrong number of lines ($(echo "$ACTUAL" | wc -l))"
		return 1
	fi

	echo "$1:"
	if diff <(echo "$ACTUAL") <(echo "$EXPECTED"); then
		echo "checks out"
	fi
}

if [ "$#" -eq 0 ]; then
	set -- bin/dec*
fi

for file in "$@"; do
	day="$(basename -s .ml "$file")"
	check "$day"
done
