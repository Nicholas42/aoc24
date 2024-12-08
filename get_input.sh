#!/bin/sh

set -e

: "${AOC_YEAR:=2024}"
DAY=${1:-$(date +%d)}
DAY_UNPADDED=$(echo "$DAY" | sed 's/^0//')
FNAME="inputs/dec$DAY.txt"

mkdir -p "$(dirname "$FNAME")"
curl --silent --show-error --fail -o "$FNAME" --cookie aoc_cookie.txt "https://adventofcode.com/$AOC_YEAR/day/$DAY_UNPADDED/input"

if [ ! -f "bin/dec$DAY.ml" ]; then
    cat <<EOF >"bin/dec$DAY.ml"
open Aoc24

let part1 = print_endline "Hello part1!"
let part2 = print_endline "Hello part2!"

let () = part1 ; part2
EOF
fi

if ! grep -q "dec$DAY" bin/dune; then
    sed -i -e "s/\\(public_names.*\\))/\\1 dec$DAY)/" bin/dune
fi
