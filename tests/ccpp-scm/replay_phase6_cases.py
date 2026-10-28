#!/usr/bin/env python3
"""Rebuild same-boundary native references from the current coupled SCM evidence."""

from __future__ import annotations

import argparse
from datetime import datetime
import json
import os
from pathlib import Path
import re

import yaml

from run_case import ROOT, run_step, digest, catchem_build_source_digest
from export_ccpp_met_ledger import load_blocks, make_profile as met_profile
from export_ccpp_boundary_chemistry import make_profile as chemistry_profile
from compare_ccpp_native_state import compare as compare_state
from compare_ccpp_photolysis_rates import compare as compare_rates


def write_json(path: Path, value: dict) -> None:
    """Write a reproducible reference artifact without nonfinite JSON values."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2, allow_nan=False) + '\n')


def admitted_clock(evidence: dict, log: str) -> datetime:
    """Resolve the first SCM physics clock from its trace or locked case input."""
    match = re.search(
        r'CATCHEM_INTERVAL_CLOCK interval=\d+ year=(\d+) month=(\d+) day=(\d+) hour=(\d+) minute=(\d+) second=(\d+)', log)
    if match:
        return datetime(*map(int, match.groups()))

    # Some generated SCM cap builds omit the optional clock trace while still
    # retaining the immutable DEPHY case input. SCM's first host physics call
    # uses the case start time (also recorded explicitly for staged cases).
    staged = evidence.get('staged_case_input') or {}
    staged_clock = staged.get('clock_start_utc')
    if staged_clock:
        return datetime.fromisoformat(staged_clock.replace('Z', '+00:00')).replace(tzinfo=None)
    case_path = Path(evidence['case_input']['path'])
    try:
        from netCDF4 import Dataset
        with Dataset(case_path) as dataset:
            case_start = str(dataset.start_date)
    except (OSError, KeyError, AttributeError, ImportError) as error:
        raise RuntimeError('Native replay requires the actually admitted SCM clock or readable DEPHY case start') from error
    return datetime.fromisoformat(case_start.replace(' ', 'T'))


def replay(work: Path, runner: Path, image: str, group: str) -> dict:
    """Replay each captured process boundary using its exact state, fields, and clock."""
    output = work / 'native-replay'
    output.mkdir(parents=True, exist_ok=True)
    candidates = sorted((work / 'cases').glob(f'{group}-*/evidence.json'))
    if not candidates:
        raise RuntimeError(f'No completed coupled SCM evidence for {group}')
    reports = []
    for evidence_path in candidates:
        evidence = json.loads(evidence_path.read_text())
        if evidence.get('scientific_acceptance', {}).get('status') != 'passed':
            raise RuntimeError(f'SCM science did not complete: {evidence_path}')
        log = Path(evidence['logs']['run']).read_text()
        clock = admitted_clock(evidence, log)
        ledger = Path(evidence['met_input_ledger']['path'])
        met = load_blocks(ledger)
        blocks = len(evidence['host_update_ledger'])
        state_rows = evidence['boundary_state_ledger']
        if len(state_rows) % blocks:
            raise RuntimeError('SCM state rows do not form complete update blocks')
        per_block = len(state_rows) // blocks
        is_chapman = group == 'photolysis-gaschem'
        selected = ['photolysis-gaschem'] if is_chapman else evidence['processes']
        for sequence, process in enumerate(selected):
            tag = ('chapman-night' if evidence['case'].endswith('_night') else 'chapman-day') if is_chapman else process
            staged = output / f'{tag}-boundary.json'
            sub_evidence = {**evidence, 'process': process,
                            'boundary_state_ledger': state_rows[sequence * per_block:(sequence + 1) * per_block],
                            'source_evidence': str(evidence_path), 'source_evidence_sha256': digest(evidence_path)}
            write_json(staged, sub_evidence)
            token = sub_evidence['boundary_state_ledger'][0]['interval_token']
            subcycle = sub_evidence['boundary_state_ledger'][0]['subcycle']
            seed = output / f'{tag}-initial.json'
            write_json(seed, chemistry_profile(staged, token, subcycle))
            met_process = 'photolysis' if is_chapman else process
            block = next(item for item in met if item['process'] == met_process)
            profile, provenance = met_profile(block, ledger, sequence, clock)
            profile_path = output / f'{tag}-met.json'
            write_json(profile_path, profile)
            write_json(output / f'{tag}-met-provenance.json', provenance)
            if is_chapman:
                config_path = ROOT / 'tests/ccpp-scm/cases/configs/gaschem.yml'
            else:
                original = ROOT / 'tests/ccpp-scm/cases/configs/emission-chemistry-removal.yml'
                config = yaml.safe_load(original.read_text())
                config['simulation']['species_filename'] = '/workspace/tests/Configs/Default/CATChem_species.yml'
                config['processes'] = {process: config['processes'][process]}
                config['run_phases'] = {'replay': {'processes': [process]}}
                config['diagnostics'] = {'output': {'enabled': False},
                                         'collection': {'enabled': True, 'buffer_size': 1}}
                config_path = output / f'{tag}-config.yml'
                config_path.write_text(yaml.safe_dump(config, sort_keys=False))
            snapshot = output / f'{tag}-native.json'
            case_runtime = evidence_path.parent / 'output'
            run_step('independent native reference', [str(runner), '--config', str(config_path),
                     '--met-profile', str(profile_path), '--initial-snapshot', str(seed),
                     '--snapshot', str(snapshot), '--dt', '600', '--steps', '1'],
                     case_runtime, output / f'{tag}-native.log', os.environ.copy(), image)
            result = compare_state(staged, snapshot, token, subcycle, 1.0e-6)
            result.update(source_evidence=str(evidence_path), source_evidence_sha256=digest(evidence_path),
                          comparison_evidence=str(staged), native_snapshot=str(snapshot),
                          native_runner=str(runner), native_runner_sha256=digest(runner),
                          catchem_source_sha256=catchem_build_source_digest())
            write_json(output / f'{tag}-state-parity.json', result)
            if result['status'] != 'passed':
                raise RuntimeError(f'Native state reference failed for {tag}: {result}')
            reports.append({'tag': tag, 'kind': 'state', 'status': result['status'],
                            'max_relative_error': result['max_relative_error']})
            if is_chapman:
                rate_result = compare_rates(evidence_path, snapshot, 1.0e-12, 1.0e-15)
                rate_result.update(source_evidence=str(evidence_path), source_evidence_sha256=digest(evidence_path),
                                   native_snapshot=str(snapshot), native_runner_sha256=digest(runner),
                                   catchem_source_sha256=catchem_build_source_digest())
                write_json(output / f'{tag}-rate-parity.json', rate_result)
                reports.append({'tag': tag, 'kind': 'rates', 'status': rate_result['status'],
                                'max_relative_error': rate_result['max_relative_error']})
    summary = {'schema_version': 1, 'status': 'passed', 'case_group': group, 'reports': reports,
               'native_runner': str(runner), 'native_runner_sha256': digest(runner)}
    write_json(output / f'{group}-native-summary.json', summary)
    return summary


def main() -> int:
    """Run the current locked coupled native comparisons with the pinned compiler image."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--work-dir', type=Path, required=True)
    parser.add_argument('--native-runner', type=Path, required=True)
    parser.add_argument('--docker-image', required=True)
    parser.add_argument('--group', choices=('emission-chemistry-removal', 'photolysis-gaschem'), required=True)
    args = parser.parse_args()
    print(json.dumps(replay(args.work_dir.resolve(), args.native_runner.resolve(), args.docker_image, args.group), indent=2))
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
