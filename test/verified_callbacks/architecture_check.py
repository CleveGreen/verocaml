#!/usr/bin/env python3
import pathlib
import sys


sys.dont_write_bytecode = True
source = pathlib.Path(sys.argv[1]).resolve()
sys.path.insert(0, str(source / "test/architecture_authority"))
from live_wrapper import run

run("verified-callbacks", sys.argv)
