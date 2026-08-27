#!/usr/bin/env python3
import pathlib
import sys

import policy


def run(
    suite,
    arguments,
    *,
    parametric=False,
    policy_path=policy.DEFAULT_LIVE_POLICY,
    ratchet_path=policy.DEFAULT_PARAMETRIC_RATCHET,
):
    if len(arguments) != 5:
        raise SystemExit(
            "usage: architecture_check.py SOURCE INSTALL RECEIPT INVENTORY"
        )
    source = pathlib.Path(arguments[1]).resolve()
    install = pathlib.Path(arguments[2]).resolve()
    receipt = pathlib.Path(arguments[3]).resolve()
    inventory = pathlib.Path(arguments[4]).resolve()
    try:
        if parametric:
            policy.verify_parametric(
                source,
                install,
                receipt,
                inventory,
                policy_path,
                ratchet_path,
            )
        else:
            policy.verify_live(
                source,
                install,
                receipt,
                inventory,
                suite,
                policy_path,
            )
    except policy.PolicyError as error:
        print(f"{error.category}: {error.detail}", file=sys.stderr)
        raise SystemExit(1)
