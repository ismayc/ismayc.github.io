"""Shared paths and helpers for the tests."""

from __future__ import annotations

import shutil
from pathlib import Path

import pytest

GOLDEN = Path(__file__).parent / "golden"
POOL_CASES = ["oct21", "fixture"]
SEEDING_CASES = [f"seeding_{i}" for i in range(1, 7)]
ALL_CASES = POOL_CASES + SEEDING_CASES


@pytest.fixture
def copy_case(tmp_path):
    """Copy a golden case into tmp_path so a test can break the copy."""

    def _copy(name: str) -> Path:
        dest = tmp_path / name
        shutil.copytree(GOLDEN / name, dest)
        return dest

    return _copy
