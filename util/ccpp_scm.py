#!/usr/bin/env python3
"""Prepare isolated, revision-locked source trees for CATChem/CCPP-SCM work."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
from datetime import datetime, timezone

ROOT = Path(__file__).resolve().parents[1]
DEFAULT_LOCK = ROOT / "tests/ccpp-scm/source-lock.json"
COMPONENTS = ("scm", "framework", "physics", "catchem", "gocart", "musica")
HEX40 = set("0123456789abcdef")
REQUIRED_BUILD_TESTS = ("test_ccpp_context", "test_ccpp_conversions", "test_ccpp_layout",
                        "test_ccpp_time_splitting", "test_ccpp_operation_recovery")
SUITE_TO_PROCESS = {"CATChem_Dust": "dust", "CATChem_SeaSalt": "seasalt",
                    "CATChem_DryDep": "drydep", "CATChem_WetDep": "wetdep",
                    "CATChem_Settling": "settling", "CATChem_SO4Chem": "so4chem",
                    "CATChem_CarbChem": "carbchem", "CATChem_Photolysis": "photolysis",
                    "CATChem_GasChem": "gaschem"}


def all_catchem_suites() -> list[str]:
    """Return all tracked individual, coupled, and subcycle suite variants for one cap."""
    return sorted(path.stem.removeprefix('suite_') for path in
                  (ROOT / 'tests/ccpp/suites').glob('suite_CATChem_*.xml'))


class MissingInputError(RuntimeError):
    """Raised when external, required SCM test data have not been staged."""


def run(command: list[str], *, cwd: Path | None = None, timeout: int = 3600) -> str:
    try:
        result = subprocess.run(command, cwd=cwd, text=True, stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, check=False, timeout=timeout)
    except subprocess.TimeoutExpired as exc:
        raise RuntimeError(f"Command timed out after {timeout} seconds: {command!r}") from exc
    if result.returncode:
        raise RuntimeError(f"Command failed ({result.returncode}): {command!r}\n{result.stdout}")
    return result.stdout.strip()


def load_lock(path: Path) -> dict:
    lock = json.loads(path.read_text())
    for name in COMPONENTS:
        item = lock.get("components", {}).get(name, {})
        revision = item.get("revision", "")
        if len(revision) != 40 or not set(revision.lower()) <= HEX40:
            raise ValueError(f"{name} revision must be a full 40-character commit hash")
        if not item.get("repository", "").startswith("https://"):
            raise ValueError(f"{name} repository must be an explicit HTTPS URL")
    return lock


def compiler_info() -> dict:
    names = {"c": os.environ.get("CC", "cc"),
             "cxx": os.environ.get("CXX", "c++"),
             "fortran": os.environ.get("FC", "gfortran")}
    info = {}
    for lang, executable in names.items():
        resolved = shutil.which(executable)
        if not resolved:
            raise RuntimeError(f"Required {lang} compiler {executable!r} was not found on PATH")
        version = run([resolved, "--version"]).splitlines()[0]
        info[lang] = {"requested": executable, "path": str(Path(resolved).resolve()), "version": version}
    # A mixed compiler family is allowed when the platform toolchain requires it, but captured.
    return info


def validate_required_tests(cmake_file: Path, tests_dir: Path) -> None:
    """Fail closed when a required CCPP regression is absent or unregistered."""
    cmake_tests = cmake_file.read_text()
    missing = [name for name in REQUIRED_BUILD_TESTS
               if not (tests_dir / f"{name}.cpp").is_file()
               and not (tests_dir / f"{name}.F90").is_file()]
    unregistered = [name for name in REQUIRED_BUILD_TESTS
                    if f"add_test(NAME {name} " not in cmake_tests
                    and f"add_test(\n    NAME {name}" not in cmake_tests
                    and f"catchem_add_architecture_test({name} " not in cmake_tests]
    if missing or unregistered:
        raise RuntimeError(f"Required CCPP tests are absent or unregistered: missing={missing}, "
                           f"unregistered={unregistered}")


def clone_locked(url: str, revision: str, destination: Path, commands: list[list[str]]) -> None:
    if destination.exists():
        if any(destination.iterdir()):
            actual = run(["git", "-C", str(destination), "rev-parse", "HEAD"])
            if actual != revision:
                raise RuntimeError(f"Refusing to reuse {destination}: HEAD {actual} != locked {revision}")
            return
        destination.rmdir()
    destination.parent.mkdir(parents=True, exist_ok=True)
    local_candidates = {
        "scm": os.environ.get("CCPP_SCM_SOURCE", "/Users/barry/Documents/ccpp-scm-1"),
        "framework": os.environ.get("CCPP_FRAMEWORK_SOURCE", "/Users/barry/Documents/ccpp-scm-1/ccpp/framework"),
        "physics": os.environ.get("CCPP_PHYSICS_SOURCE", "/Users/barry/Documents/ccpp-scm-1/ccpp/physics"),
        "catchem": os.environ.get("CATCHEM_SOURCE", str(ROOT)),
        "gocart": os.environ.get("GOCART_SOURCE", str(ROOT / "src/external/GOCART")),
        "musica": os.environ.get("MUSICA_SOURCE", str(ROOT / "src/external/musica")),
    }
    component = next((n for n, x in lock_components().items() if x.get("repository") == url), None)
    local = Path(local_candidates[component]).expanduser() if component in local_candidates else None
    source = str(local) if local and (local / ".git").exists() else url
    clone = ["git", "clone", "--no-hardlinks", "--no-checkout", source, str(destination)]
    commands.append(clone)
    run(clone)
    checkout = ["git", "-C", str(destination), "checkout", "--detach", revision]
    commands.append(checkout)
    run(checkout)
    actual = run(["git", "-C", str(destination), "rev-parse", "HEAD"])
    if actual != revision:
        raise RuntimeError(f"Checkout mismatch for {url}: expected {revision}, got {actual}")


def lock_components() -> dict:
    return _ACTIVE_LOCK.get("components", {})


_ACTIVE_LOCK: dict = {}


def prepare(args: argparse.Namespace) -> None:
    lock_path = Path(args.source_lock).resolve()
    lock = load_lock(lock_path)
    _ACTIVE_LOCK.clear()
    _ACTIVE_LOCK.update(lock)
    work = Path(args.work_dir).resolve()
    if work == ROOT or work == ROOT.parent or work in ROOT.parents:
        raise ValueError("--work-dir must be a dedicated generated directory, not a source checkout")
    work.mkdir(parents=True, exist_ok=True)
    compilers = compiler_info()
    commands: list[list[str]] = []

    # Separate source checkouts prevent overlay changes from contaminating the baseline.
    scm = lock["components"]["scm"]
    submodule_sources = {
        "ccpp/framework": Path(os.environ.get("CCPP_FRAMEWORK_SOURCE", "/Users/barry/Documents/ccpp-scm-1/ccpp/framework")),
        "ccpp/physics": Path(os.environ.get("CCPP_PHYSICS_SOURCE", "/Users/barry/Documents/ccpp-scm-1/ccpp/physics")),
        "CMakeModules": Path(os.environ.get("CCPP_CMAKEMODULES_SOURCE", "/Users/barry/Documents/ccpp-scm-1/CMakeModules")),
    }
    for tree in ("baseline-source", "overlay-source"):
        target = work / tree
        if target.exists():
            actual = run(["git", "-C", str(target), "rev-parse", "HEAD"])
            if actual != scm["revision"]:
                raise RuntimeError(f"Refusing to replace existing work tree {target} (HEAD={actual})")
        else:
            clone_locked(scm["repository"], scm["revision"], target, commands)
        submodule_names = {"ccpp-framework": "ccpp/framework", "ccpp-physics": "ccpp/physics",
                           "CMakeModules": "CMakeModules"}
        for name, path in submodule_names.items():
            source = submodule_sources[path]
            if (source / ".git").exists():
                run(["git", "-C", str(target), "config", f"submodule.{name}.url", str(source)])
        submodules = ["git", "-c", "protocol.file.allow=always", "-C", str(target),
                      "submodule", "update", "--init"]
        commands.append(submodules)
        run(submodules)
        for component, subpath in (("framework", "ccpp/framework"), ("physics", "ccpp/physics")):
            actual_submodule = run(["git", "-C", str(target / subpath), "rev-parse", "HEAD"])
            expected = lock["components"][component]["revision"]
            if actual_submodule != expected:
                raise RuntimeError(f"{subpath} gitlink mismatch: expected {expected}, got {actual_submodule}")

        # SCM's pinned physics checkout has four nested source submodules. Reuse only the
        # matching checkouts from the read-only sibling when available, then verify each
        # against the gitlink recorded by the locked physics commit.
        physics_source = Path(os.environ.get("CCPP_PHYSICS_SOURCE", "/Users/barry/Documents/ccpp-scm-1/ccpp/physics"))
        physics_target = target / "ccpp/physics"
        nested_paths = (
            "physics/CONV/C3",
            "physics/MP/TEMPO/TEMPO",
            "physics/Radiation/RRTMGP/rte-rrtmgp",
            "physics/SFC_Layer/MYNN/MYNN",
        )
        for nested_path in nested_paths:
            nested_source = physics_source / nested_path
            if (nested_source / ".git").exists():
                run(["git", "-C", str(physics_target), "config",
                     f"submodule.{nested_path}.url", str(nested_source)])
        nested_update = ["git", "-c", "protocol.file.allow=always", "-C", str(physics_target),
                         "submodule", "update", "--init", *nested_paths]
        commands.append(nested_update)
        run(nested_update)
        for nested_path in nested_paths:
            expected_line = run(["git", "-C", str(physics_target), "ls-tree", "HEAD", nested_path])
            expected_nested = expected_line.split()[2] if expected_line else ""
            actual_nested = run(["git", "-C", str(physics_target / nested_path), "rev-parse", "HEAD"])
            if actual_nested != expected_nested:
                raise RuntimeError(f"{nested_path} gitlink mismatch: expected {expected_nested}, got {actual_nested}")

        # Case datasets are not part of the SCM Git tree. Copy available user data into
        # the disposable work tree and hash every staged file without touching its source.
        local_scm = Path(os.environ.get("CCPP_SCM_SOURCE", "/Users/barry/Documents/ccpp-scm-1"))
        staged_hashes = {}
        for name in ("processed_case_input", "physics_input_data"):
            source_data = local_scm / "scm/data" / name
            target_data = target / "scm/data" / name
            if source_data.is_dir():
                # Large locked SCM data (notably the 12-month GOCART climatology)
                # may already be staged elsewhere in this workspace. Link those
                # files by relative path to avoid duplicating many gigabytes in a
                # clean source tree. Data outside the workspace is copied so its
                # path remains available in the mounted container.
                try:
                    source_data.resolve().relative_to(ROOT.resolve())
                    link_workspace_data = True
                except ValueError:
                    link_workspace_data = False
                if link_workspace_data:
                    for original in sorted(p for p in source_data.rglob("*") if p.is_file()):
                        relative = original.relative_to(source_data)
                        staged = target_data / relative
                        staged.parent.mkdir(parents=True, exist_ok=True)
                        if not staged.exists() and not staged.is_symlink():
                            staged.symlink_to(os.path.relpath(original, staged.parent))
                else:
                    shutil.copytree(source_data, target_data, dirs_exist_ok=True)
                for file_path in sorted(p for p in target_data.rglob("*") if p.is_file()):
                    hasher = hashlib.sha256()
                    with file_path.open("rb") as staged_file:
                        for chunk in iter(lambda: staged_file.read(1024 * 1024), b""):
                            hasher.update(chunk)
                    digest = hasher.hexdigest()
                    staged_hashes[file_path.relative_to(target).as_posix()] = digest
        (target / "data-input-checksums.json").write_text(json.dumps(staged_hashes, indent=2) + "\n")

    catchem = lock["components"]["catchem"]
    cache_catchem = work / "_source-cache" / "catchem"
    clone_locked(catchem["repository"], catchem["revision"], cache_catchem, commands)
    catchem_submodules = {
        "src/external/GOCART": ("src/external/GOCART", Path(os.environ.get("GOCART_SOURCE", str(ROOT / "src/external/GOCART")))),
        "src/external/musica": ("src/external/musica", Path(os.environ.get("MUSICA_SOURCE", str(ROOT / "src/external/musica")))),
    }
    for name, (path, source) in catchem_submodules.items():
        if (source / ".git").exists():
            run(["git", "-C", str(cache_catchem), "config", f"submodule.{name}.url", str(source)])
    run(["git", "-c", "protocol.file.allow=always", "-C", str(cache_catchem),
         "submodule", "update", "--init"])
    for component, subpath in (("gocart", "src/external/GOCART"), ("musica", "src/external/musica")):
        actual_submodule = run(["git", "-C", str(cache_catchem / subpath), "rev-parse", "HEAD"])
        expected = lock["components"][component]["revision"]
        if actual_submodule != expected:
            raise RuntimeError(f"CATChem {subpath} gitlink mismatch: expected {expected}, got {actual_submodule}")

    patch_root = ROOT / "tests/ccpp-scm/patches"
    overlay_digest = hashlib.sha256()
    overlay_files = sorted(p for p in patch_root.rglob("*") if p.is_file()) if patch_root.exists() else []
    for patch_file in overlay_files:
        overlay_digest.update(patch_file.relative_to(patch_root).as_posix().encode())
        overlay_digest.update(b"\0")
        overlay_digest.update(patch_file.read_bytes())
    actual_overlay_hash = overlay_digest.hexdigest() if overlay_files else None
    expected_overlay_hash = lock.get("overlay_sha256")
    if expected_overlay_hash is not None and expected_overlay_hash != actual_overlay_hash:
        raise RuntimeError(f"Overlay checksum mismatch: lock={expected_overlay_hash}, actual={actual_overlay_hash}")

    provenance = {
        "schema_version": 1,
        "prepared_utc": datetime.now(timezone.utc).isoformat(),
        "source_lock": str(lock_path),
        "source_revisions": {name: lock["components"][name]["revision"] for name in COMPONENTS},
        "compiler": compilers,
        "commands": commands,
        "trees": {name: str(work / name) for name in ("baseline-source", "overlay-source")},
        "overlay_sha256": actual_overlay_hash,
        "staged_input_checksum_files": {tree: str(work / tree / "data-input-checksums.json")
                                        for tree in ("baseline-source", "overlay-source")},
        "status": "prepared; locked submodules verified; case data checksums are captured by case staging"
    }
    (work / "prepare-provenance.json").write_text(json.dumps(provenance, indent=2) + "\n")
    print(json.dumps(provenance, indent=2))


def baseline(args: argparse.Namespace) -> None:
    lock_path = Path(args.source_lock).resolve()
    lock = load_lock(lock_path)
    work = Path(args.work_dir).resolve()
    source = work / "baseline-source"
    build = work / "baseline-build"
    run_dir = work / "baseline-run"
    evidence_dir = work / "evidence"
    evidence_dir.mkdir(parents=True, exist_ok=True)
    evidence_path = evidence_dir / "baseline.json"
    commands = []
    checks = []
    container_image = args.docker_image
    build_volume = args.docker_build_volume
    cmake_suites = f"{args.suite},{args.suite}_ps" if args.case.lower() == "bomex" else args.suite
    if container_image:
        compiler_output = run(["docker", "run", "--rm", "--platform", "linux/amd64",
                               "--entrypoint", "/bin/bash", container_image, "-c",
                               "gcc --version | head -n 1; g++ --version | head -n 1; gfortran --version | head -n 1"])
        compiler = {name: line for name, line in zip(("c", "cxx", "fortran"), compiler_output.splitlines())}
    else:
        compilers = compiler_info()
        compiler = {key: value["version"] for key, value in compilers.items()}
    evidence = {
        "schema_version": 1,
        "source_revisions": {name: lock["components"][name]["revision"] for name in COMPONENTS},
        "compiler": compiler,
        "case": args.case,
        "suite": args.suite,
        "cmake_suites": cmake_suites,
        "container_image": container_image,
        "build_volume": build_volume if container_image else None,
        "commands": commands,
        "checks": checks,
        "status": "running"
    }

    def execute(label: str, command: list[str], cwd: Path, logfile: Path) -> str:
        commands.append(command)
        proc = subprocess.run(command, cwd=cwd, text=True, stdout=subprocess.PIPE,
                              stderr=subprocess.STDOUT, check=False)
        logfile.parent.mkdir(parents=True, exist_ok=True)
        logfile.write_text(proc.stdout)
        checks.append({"name": label, "status": "passed" if proc.returncode == 0 else "failed",
                       "details": str(logfile)})
        evidence_path.write_text(json.dumps(evidence, indent=2) + "\n")
        if proc.returncode:
            raise RuntimeError(f"{label} failed with exit {proc.returncode}; see {logfile}\n{proc.stdout[-4000:]}")
        return proc.stdout

    def execute_container(label: str, script: str, logfile: Path) -> str:
        command = ["docker", "run", "--rm", "--platform", "linux/amd64",
                   "-v", f"{ROOT}:/workspace",
                   "--mount", f"type=volume,source={build_volume},target=/build",
                   "-w", "/workspace", "--entrypoint", "/bin/bash", container_image,
                   "-c", script]
        commands.append(command)
        proc = subprocess.run(command, cwd=ROOT, text=True, stdout=subprocess.PIPE,
                              stderr=subprocess.STDOUT, check=False)
        logfile.parent.mkdir(parents=True, exist_ok=True)
        logfile.write_text(proc.stdout)
        checks.append({"name": label, "status": "passed" if proc.returncode == 0 else "failed",
                       "details": str(logfile)})
        evidence_path.write_text(json.dumps(evidence, indent=2) + "\n")
        if proc.returncode:
            raise RuntimeError(f"{label} failed with exit {proc.returncode}; see {logfile}\n{proc.stdout[-4000:]}")
        return proc.stdout

    try:
        if not (source / "CMakeLists.txt").is_file():
            raise RuntimeError(f"Prepared SCM source tree is missing: {source / 'CMakeLists.txt'}; run prepare first")
        required_assets = [source / "scm/data/processed_case_input/bomex_SCM_driver.nc"]
        climo_root = source / "scm/data/physics_input_data"
        required_climo = ([climo_root / f"aeroclim.m{month:02d}.nc" for month in range(1, 13)] +
                          [climo_root / f"optics_{species}.dat" for species in ("BC", "DU", "OC", "SS", "SU")])
        missing_climo = [str(path) for path in required_climo if not path.is_file() or path.stat().st_size == 0]
        if missing_climo:
            raise MissingInputError("Required GFS v17 GOCART climatology files are absent or empty: " +
                                    ", ".join(missing_climo))
        missing = [str(path) for path in required_assets if not path.is_file()]
        if missing:
            raise MissingInputError("Required stock SCM case input is absent: " + ", ".join(missing))
        if run_dir.exists():
            shutil.rmtree(run_dir)
        run_dir.mkdir(parents=True)
        if container_image:
            source_in_container = "/workspace/" + source.relative_to(ROOT).as_posix()
            setup = ('VIEW=$(find /opt/views/._view -mindepth 1 -maxdepth 1 -type d | head -n 1); '
                     'test -n "$VIEW" || { echo "Spack view not found"; exit 2; }; '
                     'export PATH="$VIEW/bin:$PATH" LD_LIBRARY_PATH="$VIEW/lib" '
                     'CMAKE_PREFIX_PATH="$VIEW" CC=gcc CXX=g++ FC=gfortran; ')
            # The named Docker volume can be reused with a different work-dir.
            # Its generated CMake cache embeds the old source path, so always
            # rebuild this disposable directory for the requested source tree.
            configure = (f'rm -rf /build/baseline-build && '
                         f'cmake -S "{source_in_container}" -B /build/baseline-build '
                         f'-DCCPP_SUITES={cmake_suites} -DCMAKE_BUILD_TYPE={args.build_type}')
            execute_container("configure", setup + configure, work / "logs/baseline-configure.log")
            # The pinned host metadata contains case-variant duplicate module producers, so
            # build serially to prevent GCC Fortran's shared .mod outputs from racing.
            execute_container("build", setup + "cmake --build /build/baseline-build --parallel 1",
                              work / "logs/baseline-build.log")
            run_script = (f'cd "{source_in_container}" && /usr/bin/python3 scm/src/run_scm.py '
                          f'--case "{args.case}" --suite "{args.suite}" '
                          f'--bin_dir /build/baseline-build '
                          f'--run_dir "/workspace/{run_dir.relative_to(ROOT).as_posix()}" '
                          '--case_data_dir scm/data/processed_case_input')
            execute_container("run", setup + run_script, work / "logs/baseline-run.log")
        else:
            configure = ["cmake", "-S", str(source), "-B", str(build),
                         f"-DCCPP_SUITES={cmake_suites}", f"-DCMAKE_BUILD_TYPE={args.build_type}"]
            execute("configure", configure, ROOT, work / "logs/baseline-configure.log")
            build_command = ["cmake", "--build", str(build), "--parallel", str(args.jobs)]
            execute("build", build_command, ROOT, work / "logs/baseline-build.log")
            runner = source / "scm/src/run_scm.py"
            run_command = [sys.executable, str(runner), "--case", args.case, "--suite", args.suite,
                           "--bin_dir", str(build), "--run_dir", str(run_dir),
                           "--case_data_dir", "scm/data/processed_case_input"]
            execute("run", run_command, source, work / "logs/baseline-run.log")
        outputs = (sorted(str(path) for path in run_dir.rglob("*.nc")
                          if path.is_file() and not path.is_symlink()) if run_dir.exists() else [])
        if not outputs:
            raise RuntimeError(f"SCM baseline completed without a NetCDF output under {run_dir}")
        evidence["output_files"] = outputs
        evidence["status"] = "passed"
    except Exception as exc:
        evidence["status"] = "blocked" if isinstance(exc, MissingInputError) else "failed"
        evidence["failure"] = str(exc)
        evidence_path.write_text(json.dumps(evidence, indent=2) + "\n")
        raise
    evidence_path.write_text(json.dumps(evidence, indent=2) + "\n")
    print(json.dumps(evidence, indent=2))


def overlay_hash() -> str | None:
    patch_root = ROOT / "tests/ccpp-scm/patches"
    patch_files = sorted(p for p in patch_root.rglob("*") if p.is_file()) if patch_root.exists() else []
    if not patch_files:
        return None
    digest = hashlib.sha256()
    for patch_file in patch_files:
        digest.update(patch_file.relative_to(patch_root).as_posix().encode())
        digest.update(b"\0")
        digest.update(patch_file.read_bytes())
    return digest.hexdigest()


def probe(args: argparse.Namespace) -> None:
    lock_path = Path(args.source_lock).resolve()
    lock = load_lock(lock_path)
    work = Path(args.work_dir).resolve()
    source = work / "overlay-source"
    framework = source / "ccpp/framework"
    baseline_evidence_path = work / "evidence/baseline.json"
    evidence_path = work / "evidence/g0.json"
    evidence_path.parent.mkdir(parents=True, exist_ok=True)
    revisions = {name: lock["components"][name]["revision"] for name in COMPONENTS}
    image = args.docker_image
    current_overlay_hash = overlay_hash()
    evidence = {
        "schema_version": 1,
        "source_revisions": revisions,
        "compiler": {"c": "", "cxx": "", "fortran": ""},
        "commands": [],
        "checks": [],
        "process_coverage": {"required": ["dust", "seasalt", "drydep", "wetdep", "settling",
                                             "so4chem", "carbchem", "photolysis", "gaschem"],
                             "completed": []},
        "case_results": [{"case_id": "ccpp-constituent-probe", "status": "not_run",
                           "outputs": [], "budgets": []}],
        "overlay_sha256": current_overlay_hash or "0" * 64,
        "generated_signatures": {"lifecycle": [], "constituent_helpers": [],
                                 "full_array_bindings": [], "compatibility_flags": []},
        "instance_results": [],
        "stock_baseline_comparison": {"status": "not_comparable", "case": "", "suite": "",
                                       "source_revisions_match": False, "output_sha256": "0" * 64,
                                       "comparison_scope": "not run"},
        "container_image": image,
        "status": "blocked",
    }
    commands: list[list[str]] = evidence["commands"]
    checks: list[dict[str, str]] = evidence["checks"]

    def record(name: str, status: str, details: str) -> None:
        checks.append({"name": name, "status": status, "details": details})
        evidence_path.write_text(json.dumps(evidence, indent=2) + "\n")

    def execute_container(label: str, script: str) -> str:
        view_setup = ('VIEW=$(find /opt/views/._view -mindepth 1 -maxdepth 1 -type d | head -n 1); '
                      'test -n "$VIEW" || { echo "Spack view not found"; exit 2; }; '
                      'export PATH="$VIEW/bin:$PATH" LD_LIBRARY_PATH="$VIEW/lib" '
                      'CMAKE_PREFIX_PATH="$VIEW" CC=gcc CXX=g++ FC=gfortran; ')
        command = ["docker", "run", "--rm", "--platform", "linux/amd64",
                   "-v", f"{ROOT}:/workspace",
                   "--mount", f"type=volume,source={args.docker_build_volume},target=/build",
                   "-w", "/workspace", "--entrypoint", "/bin/bash", image, "-c",
                   view_setup + script]
        commands.append(command)
        result = subprocess.run(command, cwd=ROOT, text=True, stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, check=False)
        log_path = work / f"logs/probe-{label}.log"
        log_path.parent.mkdir(parents=True, exist_ok=True)
        log_path.write_text(result.stdout)
        record(label, "passed" if result.returncode == 0 else "failed", str(log_path))
        if result.returncode:
            raise RuntimeError(f"probe {label} failed with exit {result.returncode}; see {log_path}\n"
                               f"{result.stdout[-4000:]}")
        return result.stdout

    try:
        if current_overlay_hash is None:
            raise RuntimeError("No overlay patch exists; refusing to run G0 without tracked changes")
        if lock.get("overlay_sha256") != current_overlay_hash:
            raise RuntimeError(f"Overlay checksum mismatch: lock={lock.get('overlay_sha256')}, "
                               f"actual={current_overlay_hash}")
        if not framework.is_dir():
            raise RuntimeError(f"Prepared overlay framework is missing: {framework}; run prepare first")
        actual_framework = run(["git", "-C", str(framework), "rev-parse", "HEAD"])
        if actual_framework != revisions["framework"]:
            raise RuntimeError(f"Framework revision mismatch: expected {revisions['framework']}, "
                               f"got {actual_framework}")
        patch_file = ROOT / "tests/ccpp-scm/patches/0001-constituent-probe.patch"
        check_cmd = ["git", "-C", str(framework), "apply", "--check", str(patch_file)]
        commands.append(check_cmd)
        check_result = subprocess.run(check_cmd, text=True, stdout=subprocess.PIPE,
                                      stderr=subprocess.STDOUT, check=False)
        if check_result.returncode == 0:
            apply_cmd = ["git", "-C", str(framework), "apply", str(patch_file)]
            commands.append(apply_cmd)
            run(apply_cmd)
            patch_state = "applied"
        else:
            reverse_cmd = ["git", "-C", str(framework), "apply", "--reverse", "--check", str(patch_file)]
            commands.append(reverse_cmd)
            reverse_result = subprocess.run(reverse_cmd, text=True, stdout=subprocess.PIPE,
                                            stderr=subprocess.STDOUT, check=False)
            if reverse_result.returncode:
                raise RuntimeError("Probe patch is neither applicable nor already applied: " +
                                   check_result.stdout + reverse_result.stdout)
            patch_state = "already applied"
        record("overlay_patch", "passed", patch_state)

        scm_capgen = source / "cmake/ccpp_capgen.cmake"
        capgen_text = scm_capgen.read_text()
        required_flags = ("--legacy-mode", "--gfs-dim-aliases")
        absent_flags = [flag for flag in required_flags if flag not in capgen_text]
        if absent_flags:
            raise RuntimeError(f"Pinned SCM capgen compatibility flags are missing: {absent_flags}")
        evidence["generated_signatures"]["compatibility_flags"] = list(required_flags)
        record("capgen_compatibility_flags", "passed", ", ".join(required_flags))

        if not baseline_evidence_path.is_file():
            raise RuntimeError(f"Stock SCM baseline evidence is missing: {baseline_evidence_path}")
        baseline_evidence = json.loads(baseline_evidence_path.read_text())
        baseline_revisions = baseline_evidence.get("source_revisions", {})
        revisions_match = baseline_evidence.get("status") == "passed" and baseline_revisions == revisions
        baseline_outputs = baseline_evidence.get("output_files", [])
        if not revisions_match or not baseline_outputs:
            raise RuntimeError("A passed stock baseline with matching locked revisions is required")
        baseline_output = Path(baseline_outputs[0])
        if not baseline_output.is_file():
            raise RuntimeError(f"Stock baseline output is missing: {baseline_output}")
        baseline_digest = hashlib.sha256(baseline_output.read_bytes()).hexdigest()
        evidence["stock_baseline_comparison"] = {
            "status": "passed",
            "case": baseline_evidence.get("case", ""),
            "suite": baseline_evidence.get("suite", ""),
            "source_revisions_match": True,
            "output_sha256": baseline_digest,
            "comparison_scope": "stock BOMEX output is fingerprinted as the locked reference; the no-op probe validates ABI/lifecycle only and makes no scientific-equivalence claim",
        }
        record("stock_baseline_reference", "passed", str(baseline_output))

        compiler_output = execute_container("compiler", "gcc --version | head -n 1; "
                                             "g++ --version | head -n 1; gfortran --version | head -n 1")
        compiler_lines = compiler_output.splitlines()
        evidence["compiler"] = {"c": compiler_lines[0], "cxx": compiler_lines[1],
                                "fortran": compiler_lines[2]}

        source_in_container = "/workspace/" + (framework / "end-to-end-tests").relative_to(ROOT).as_posix()
        setup = f"rm -rf /build/constituent-probe && " \
                f"cmake -S {source_in_container} -B /build/constituent-probe " \
                "-DOPENMP=OFF -DCMAKE_Fortran_COMPILER=gfortran " \
                "-DCMAKE_Fortran_FLAGS='-ffree-line-length-none -cpp' -DCMAKE_BUILD_TYPE=Debug"
        execute_container("configure", setup)
        execute_container("build", "cmake --build /build/constituent-probe "
                          "--target test_constituent_probe.x --parallel 1")
        execute_container("ctest", "ctest --test-dir /build/constituent-probe "
                          "-R '^test_constituent_probe$' --output-on-failure")
        executable = "/build/constituent-probe/constituent_probe/test_constituent_probe.x"
        probe_output = execute_container("run", executable)
        if "PASS: merge, conflict rejection, slot order, full arrays, two instances" not in probe_output:
            raise RuntimeError("Probe executable did not report its full pass marker")
        parsed_results = []
        current_order: dict[int, list[str]] = {}
        for line in probe_output.splitlines():
            order_match = re.search(r"HOST_INPUT_ORDER (\d+) (shared,host|host,shared)", line)
            if order_match:
                current_order[int(order_match.group(1))] = order_match.group(2).split(",")
            result_match = re.search(
                r"INSTANCE_RESULT (\d+) count/shared/host/scheme=(\d+) (\d+) (\d+) (\d+)", line)
            if result_match:
                instance, count, shared, host, scheme = map(int, result_match.groups())
                parsed_results.append({"instance": instance,
                                       "host_registration_order": current_order.get(instance, []),
                                       "constituent_count": count,
                                       "indices": {"probe_shared_tracer": shared,
                                                   "probe_host_tracer": host,
                                                   "probe_scheme_tracer": scheme},
                                       "status": "passed"})
        if len(parsed_results) != 2 or parsed_results[0]["host_registration_order"] == parsed_results[1]["host_registration_order"]:
            raise RuntimeError("Probe output does not prove two distinct host registration orders")
        evidence["instance_results"] = parsed_results
        record("two_instance_exchange", "passed", probe_output.strip().replace("\n", " | "))

        generated_output = execute_container(
            "signatures",
            "python3 - <<'PY'\n"
            "import json, re\n"
            "from pathlib import Path\n"
            "root = Path('/build/constituent-probe/constituent_probe/ccpp')\n"
            "host = (root / 'probe_host_ccpp_cap.F90').read_text()\n"
            "helpers = (root / 'ccpp_host_constituents.F90').read_text()\n"
            "group = (root / 'ccpp_constituent_probe_suite_physics_cap.F90').read_text()\n"
            "def signature(text, name):\n"
            " m = re.search(r'(?im)^\\s*subroutine\\s+' + re.escape(name) + r'\\s*\\((.*?)\\)', text, re.S)\n"
            " if not m: raise SystemExit('missing generated signature: ' + name)\n"
            " return name + '(' + ' '.join(m.group(1).split()) + ')'\n"
            "result = {'lifecycle': [signature(host, n) for n in ('ccpp_register','ccpp_init','ccpp_physics_run')],\n"
            " 'constituent_helpers': [signature(helpers, n) for n in ('ccpp_register_constituents','ccpp_initialize_constituents')],\n"
            " 'full_array_bindings': [x.strip() for x in group.splitlines() if re.search(r'ccpp_(constituents|constituent_tendencies|constituent_properties)=', x)]}\n"
            "print('SIGNATURES_JSON ' + json.dumps(result))\n"
            "PY")
        signature_line = next((line for line in generated_output.splitlines()
                               if line.startswith("SIGNATURES_JSON ")), None)
        if not signature_line:
            raise RuntimeError("Could not capture generated CCPP signatures")
        signatures = json.loads(signature_line.removeprefix("SIGNATURES_JSON "))
        if not any("errflg" in sig for sig in signatures["lifecycle"]):
            raise RuntimeError("Generated SCM-style lifecycle signatures did not expose errflg")
        if not all("errcode" in sig for sig in signatures["constituent_helpers"]):
            raise RuntimeError("Generated constituent helpers did not expose errcode")
        if len(signatures["full_array_bindings"]) != 3:
            raise RuntimeError("Generated cap is missing a full constituent array binding")
        signatures["compatibility_flags"] = list(required_flags)
        evidence["generated_signatures"] = signatures
        record("generated_signatures", "passed", json.dumps(signatures, sort_keys=True))

        evidence["case_results"][0] = {"case_id": "ccpp-constituent-probe", "status": "passed",
                                        "outputs": ["two registry instances", "full constituent no-op exchange"],
                                        "budgets": []}
        evidence["status"] = "passed"
        evidence_path.write_text(json.dumps(evidence, indent=2) + "\n")
        print(json.dumps(evidence, indent=2))
    except Exception as exc:
        evidence["status"] = "failed" if evidence["checks"] else "blocked"
        if evidence["case_results"][0]["status"] == "not_run":
            evidence["case_results"][0]["status"] = "blocked"
        evidence["failure"] = str(exc)
        evidence_path.write_text(json.dumps(evidence, indent=2) + "\n")
        raise


def _run_case_command(args: argparse.Namespace, process: str, case: str, suite: str | None = None,
                     *, build_only: bool = False, science_profile: str | None = None,
                      keep_existing: bool = False, processes: list[str] | None = None,
                      case_group: str | None = None, suite_set: list[str] | None = None,
                      process_set: list[str] | None = None,
                      shared_build_dir: Path | None = None) -> None:
    """Run one process through the shared locked SCM case driver."""
    command = [sys.executable, str(ROOT / "tests/ccpp-scm/run_case.py"),
               "--work-dir", str(Path(args.work_dir).resolve()), "--process", process,
               "--case", case, "--runtime", str(args.runtime), "--timestep", str(args.timestep),
               "--jobs", str(args.jobs), "--science-profile", science_profile or args.science_profile]
    if suite:
        command.extend(["--suite", suite])
    if processes:
        command.extend(["--processes", ",".join(processes)])
    if case_group:
        command.extend(["--case-group", case_group])
    if suite_set:
        command.extend(["--suite-set", ",".join(suite_set)])
    if process_set:
        command.extend(["--process-set", ",".join(process_set)])
    if shared_build_dir:
        command.extend(["--shared-build-dir", str(shared_build_dir)])
    if args.docker_image:
        command.extend(["--docker-image", args.docker_image])
    if args.registry_order:
        command.extend(["--registry-order", args.registry_order])
    if build_only:
        command.append("--build-only")
    if keep_existing:
        command.append("--keep-existing")
    if args.emit_boundary_ledger:
        command.append("--emit-boundary-ledger")
    if args.emit_met_ledger:
        command.append("--emit-met-ledger")
    run(command)


def build(args: argparse.Namespace) -> None:
    """Configure and compile each requested real SCM process cap."""
    validate_required_tests(ROOT / "tests/CMakeLists.txt", ROOT / "tests")
    processes = args.processes.split(",")
    if not processes or any(process not in {"dust", "seasalt", "drydep", "wetdep", "settling",
                                            "so4chem", "carbchem", "photolysis", "gaschem"}
                            for process in processes):
        raise ValueError("--processes must be a comma-separated list of CATChem process names")
    if args.cases == "coupled":
        groups = {
            "CATChem_EmissionChemistryRemoval": ("emission-chemistry-removal", "gabls3_noahmp"),
            "CATChem_PhotolysisGasChem": ("photolysis-gaschem", "arm_sgp_summer_1997_A"),
        }
        selected_groups = ([name.strip().removeprefix("suite_") for name in args.suite.split(",") if name.strip()]
                           if args.suite else list(groups))
        unknown = [name for name in selected_groups if name not in groups]
        if unknown:
            raise ValueError("coupled build suites must be one of " + ", ".join(groups))
        group_processes = {
            "CATChem_EmissionChemistryRemoval": ("dust", "seasalt", "wetdep", "settling", "drydep", "so4chem", "carbchem"),
            "CATChem_PhotolysisGasChem": ("photolysis", "gaschem"),
        }
        selected_processes = tuple(
            dict.fromkeys(process for name in selected_groups for process in group_processes[name]))
        suite_set = (all_catchem_suites() if not args.suite else
                     [variant for name in selected_groups for variant in (name, f"{name}_ps")])
        shared_build_dir = Path(args.work_dir).resolve() / "cases" / "coupled-shared-cap" / "build"
        anchor = selected_groups[0]
        group, default_case = groups[anchor]
        _run_case_command(args, "dust" if group == "emission-chemistry-removal" else "photolysis",
                          args.case or default_case, anchor, build_only=True,
                          science_profile=args.science_profile,
                          keep_existing=True, processes=list(group_processes[anchor]), case_group=group,
                          suite_set=suite_set, process_set=list(selected_processes),
                          shared_build_dir=shared_build_dir)
        return
    suites = [name.strip() for name in args.suite.split(",") if name.strip()] if args.suite else []
    if suites:
        unknown = [name for name in suites if name not in SUITE_TO_PROCESS]
        if unknown:
            raise ValueError("--suite accepts comma-separated individual suite names; unknown: " + ", ".join(unknown))
        selected = [(SUITE_TO_PROCESS[suite], suite) for suite in suites]
    else:
        selected = [(process, None) for process in processes]
    defaults = {"dust": "arm_sgp_summer_1997_A", "seasalt": "COMBLE", "drydep": "arm_sgp_summer_1997_A",
                "wetdep": "twpice", "settling": "bomex", "so4chem": "arm_sgp_summer_1997_A",
                "carbchem": "bomex", "photolysis": "arm_sgp_summer_1997_A",
                "gaschem": "arm_sgp_summer_1997_A"}
    for process, suite in selected[:1]:
        case = args.case or defaults[process]
        # Only Photolysis and GasChem consume MUSICA. Keep the required Kokkos/
        # MUSICA profile for those caps without rebuilding unrelated third-party
        # chemistry and radiation libraries for every other process suite.
        _run_case_command(args, process, case, suite, build_only=True, science_profile='all',
                          keep_existing=True, suite_set=all_catchem_suites(),
                          process_set=list(SUITE_TO_PROCESS.values()),
                          shared_build_dir=Path(args.work_dir).resolve() / 'cases/coupled-shared-cap/build')


def run_cases(args: argparse.Namespace) -> None:
    """Execute required individual or coupled case groups using the real SCM host."""
    if args.cases == "coupled":
        groups = {
            "CATChem_EmissionChemistryRemoval": (
                "emission-chemistry-removal", "gabls3_noahmp",
                ["dust", "seasalt", "wetdep", "settling", "drydep", "so4chem", "carbchem"]),
            "CATChem_PhotolysisGasChem": (
                "photolysis-gaschem", "arm_sgp_summer_1997_A", ["photolysis", "gaschem"]),
        }
        selected_groups = ([name.strip().removeprefix("suite_") for name in args.suite.split(",") if name.strip()]
                           if args.suite else list(groups))
        unknown = [name for name in selected_groups if name not in groups]
        if unknown:
            raise ValueError("coupled run suites must be one of " + ", ".join(groups))
        # Default validation uses the combined cap. An explicit suite selection
        # builds only that suite family and its process wrappers for focused CI.
        group_processes = {
            "CATChem_EmissionChemistryRemoval": ["dust", "seasalt", "wetdep", "settling", "drydep", "so4chem", "carbchem"],
            "CATChem_PhotolysisGasChem": ["photolysis", "gaschem"],
        }
        selected_processes = (list(SUITE_TO_PROCESS.values()) if not args.suite else
                              list(dict.fromkeys(process for name in selected_groups for process in group_processes[name])))
        suite_set = (all_catchem_suites() if not args.suite else
                     [variant for name in selected_groups for variant in (name, f"{name}_ps")])
        shared_build_dir = Path(args.work_dir).resolve() / "cases" / "coupled-shared-cap" / "build"
        for suite in selected_groups:
            group, default_case, processes = groups[suite]
            cases = [args.case or default_case]
            if group == "photolysis-gaschem":
                cases = ([args.case] if args.case else
                         ["arm_sgp_summer_1997_A", "arm_sgp_summer_1997_A_night"])
            for selected_case in cases:
                _run_case_command(args, processes[0], selected_case, suite,
                                  science_profile=args.science_profile, keep_existing=True,
                                  processes=processes, case_group=group, suite_set=suite_set,
                                  process_set=selected_processes, shared_build_dir=shared_build_dir)
        return
    defaults = {"dust": "arm_sgp_summer_1997_A", "seasalt": "COMBLE", "drydep": "arm_sgp_summer_1997_A",
                "wetdep": "twpice", "settling": "bomex", "so4chem": "arm_sgp_summer_1997_A",
                "carbchem": "bomex", "photolysis": "arm_sgp_summer_1997_A",
                "gaschem": "arm_sgp_summer_1997_A"}
    if args.suite:
        suites = [name.strip() for name in args.suite.split(",") if name.strip()]
        unknown = [name for name in suites if name not in SUITE_TO_PROCESS]
        if unknown:
            raise ValueError("--suite accepts comma-separated individual suite names; unknown: " + ", ".join(unknown))
        selected = [(SUITE_TO_PROCESS[name], defaults[SUITE_TO_PROCESS[name]], name) for name in suites]
    else:
        selected = [(name, args.case or case, None) for name, case in defaults.items()]
    for process, case, suite in selected:
        _run_case_command(args, process, case, suite, science_profile='all', keep_existing=True,
                          suite_set=all_catchem_suites(), process_set=list(SUITE_TO_PROCESS.values()),
                          shared_build_dir=Path(args.work_dir).resolve() / 'cases/coupled-shared-cap/build')


def verify(args: argparse.Namespace) -> None:
    """Fail closed unless every required manifest has frozen, passing SCM evidence."""
    script = ROOT / "tests/verify_ccpp_science.py"
    run([sys.executable, str(script), "--work-dir", str(Path(args.work_dir).resolve()),
         "--cases", args.cases])


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    p = sub.add_parser("prepare", help="create locked baseline and overlay source trees")
    p.add_argument("--work-dir", required=True)
    p.add_argument("--source-lock", default=str(DEFAULT_LOCK))
    p.set_defaults(func=prepare)
    b = sub.add_parser("baseline", help="build and run the locked stock SCM BOMEX/GFS v17 baseline")
    b.add_argument("--work-dir", required=True)
    b.add_argument("--source-lock", default=str(DEFAULT_LOCK))
    b.add_argument("--case", default="bomex")
    b.add_argument("--suite", default="SCM_GFS_v17_p8_ugwpv1")
    b.add_argument("--build-type", default="Debug")
    b.add_argument("--jobs", type=int, default=max(1, os.cpu_count() or 1))
    b.add_argument("--docker-image", help="run configure/build/case in this pinned Linux image")
    b.add_argument("--docker-build-volume", default="catchem-ccpp-baseline-build",
                   help="Docker volume for a Linux case-sensitive build tree")
    b.set_defaults(func=baseline)
    p = sub.add_parser("probe", help="apply the locked probe overlay and record G0 lifecycle evidence")
    p.add_argument("--work-dir", required=True)
    p.add_argument("--source-lock", default=str(DEFAULT_LOCK))
    p.add_argument("--docker-image", default="sha256:40f52f1034c182b9da330f544d469974e2abad3be24b5afb18e0351ec7164628",
                   help="pinned GCC image extended with the SCM Python runtime dependencies")
    p.add_argument("--docker-build-volume", default="catchem-ccpp-baseline-build",
                   help="Docker volume for generated CCPP sources and Fortran modules")
    p.set_defaults(func=probe)
    for name, function, help_text in (("build", build, "configure and build required SCM process suites"),
                                      ("run", run_cases, "run a required SCM case group"),
                                      ("verify", verify, "verify frozen science acceptance evidence")):
        command = sub.add_parser(name, help=help_text)
        command.add_argument("--work-dir", required=True)
        command.add_argument("--docker-image", default="sha256:40f52f1034c182b9da330f544d469974e2abad3be24b5afb18e0351ec7164628")
        command.add_argument("--suite", help="explicit comma-separated SCM suite names")
        command.add_argument("--case", help="override the default SCM case for build")
        command.add_argument("--runtime", type=int, default=600)
        command.add_argument("--timestep", type=int, default=600)
        command.add_argument("--jobs", type=int, default=1)
        command.add_argument("--science-profile", choices=("process", "all"), default="all")
        command.add_argument("--registry-order", choices=("manifest", "reverse"), default="manifest")
        command.add_argument("--emit-boundary-ledger", action="store_true")
        command.add_argument("--emit-met-ledger", action="store_true")
        command.add_argument("--cases", choices=("individual", "coupled"), default="individual")
        if name == "build":
            command.add_argument("--processes", default="dust,seasalt,drydep,wetdep,settling,so4chem,carbchem,photolysis,gaschem")
        command.set_defaults(func=function)
    args = parser.parse_args()
    try:
        args.func(args)
    except (OSError, ValueError, RuntimeError, json.JSONDecodeError) as exc:
        print(f"ccpp_scm: error: {exc}", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
