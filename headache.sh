#!/bin/bash -e

dirs=(
    # Add new directories below:
    "src"
    "test"
    "systems/cello/e12"
    "systems/cello/e19"
    "systems/cello/e31"
    "systems/cello/e53"
    "systems/cello/e55"
    "systems/cello/just"
    "systems/cello/pythagorean"
    "systems/edos"
)

for dir in "${dirs[@]}"; do
    # Apply headache to .ml files
    headache -c .headache.config -h COPYING.HEADER ${dir}/*.ml

    # Check if .mli files exist in the directory, if so apply headache
    if ls ${dir}/*.mli 1> /dev/null 2>&1; then
        headache -c .headache.config -h COPYING.HEADER ${dir}/*.mli
    fi
done

dune fmt
