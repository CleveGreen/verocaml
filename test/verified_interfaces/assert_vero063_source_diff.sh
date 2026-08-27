#!/bin/sh
set -eu

root=${1:?usage: assert_vero063_source_diff.sh REPOSITORY}
exec python3 "$root/test/architecture_authority/policy.py" replay \
  --repository "$root" \
  --record "$root/test/architecture_authority/records/vero063.json"
