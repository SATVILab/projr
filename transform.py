#!/usr/bin/env python3

import re
import subprocess
from pathlib import Path

FILE = Path("tests/testthat/test-dest-send-prepare.R")

# Read file
lines = FILE.read_text(encoding="utf-8").splitlines(keepends=True)

out = []

for line in lines:
    # Match: output_level = <something> BUT NOT ending with ')'
    if re.match(r'^\s*output_level\s*=\s*[^)]*\s*$', line):
        # Remove trailing comma from the previous line
        if out:
            out[-1] = re.sub(r',\s*$', '', out[-1])
        # Drop this line entirely
        continue

    out.append(line)

# Write back to the same file
FILE.write_text("".join(out), encoding="utf-8")

# Run styler on the same file
subprocess.run(
    [
        "Rscript",
        "-e",
        f'styler::style_file("{FILE.as_posix()}")'
    ],
    check=True
)
