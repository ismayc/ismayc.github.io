"""``ouanalytics-check <case_dir>...``: recompute each golden case in Python,
compare it with R's outputs, and run the data checks.

Prints one line per table and per check; exits 1 on any difference or
violation, 0 when everything matches.
"""

from __future__ import annotations

import sys
from pathlib import Path

from .compare import compare_case
from .io import case_kind
from .validate import validate_case

USAGE = "usage: ouanalytics-check <case_dir> [<case_dir> ...]"


def check_case(case_dir: Path) -> tuple[bool, list[str]]:
    """(all ok, report lines) for one case folder."""
    try:
        kind = case_kind(case_dir)
    except ValueError as err:
        return False, [f"{case_dir}: ERROR {err}"]
    lines = [f"{case_dir} ({kind})"]
    ok = True
    for result in compare_case(case_dir):
        ok &= result.ok
        lines += result.lines("  ")
    for result in validate_case(case_dir):
        ok &= result.ok
        lines += result.lines("  validate: ")
    return ok, lines


def main(argv: list[str] | None = None) -> int:
    args = sys.argv[1:] if argv is None else argv
    if not args:
        print(USAGE, file=sys.stderr)
        return 2
    all_ok = True
    for arg in args:
        ok, lines = check_case(Path(arg))
        all_ok &= ok
        print("\n".join(lines))
    print("all cases match" if all_ok else "DIFFERENCES FOUND")
    return 0 if all_ok else 1


if __name__ == "__main__":
    sys.exit(main())
