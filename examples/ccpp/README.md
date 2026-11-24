# CATChem CCPP integration example

[`tests/Configs/Default/CATChem_species.yml`](../../tests/Configs/Default/CATChem_species.yml)
is the authoritative default CATChem species list used by the CCPP-SCM demonstrations. Keep
mechanism species and process properties there; do not maintain a second CCPP copy. Species names
are also their CCPP standard names by default. Set `__ccpp_standard_name` in the authoritative
file only when a host's canonical standard name differs.

The OpenAtmos v1 compatible extension key `__units` declares each species' unit string. The CCPP
manifest reads this value from the same species file. CATChem's legacy defaults remain dry-air mass
mixing ratio when `__amount_type` and `__air_basis` are omitted. CATChem accepts unprefixed
spellings for backwards compatibility; new OpenAtmos v1 extensions should use the `__` prefix.

Gas species require their molecular weight from this manifest for dry-air ppmv conversion.
For aerosol species the adapter uses mass ratios and their declared aerosol classification;
it does not interpret aerosol molar mass as a gas conversion property. Do not change the
air basis implicitly: for host arrays normalized by moist air or total air including
condensate, set `__air_basis: moist` or `__air_basis: total_with_condensate` and provide the
matching host and dry-air densities.

The CCPP suite declaration controls process selection and ordering. The CATChem process
configuration enables and configures those same schemes; reconcile the two before cap
generation. The pinned example suite is `SCM_GFS_v17_p8_ugwpv1`; process driver suites and
the SCM case selected for each are declared in the case manifests under
[`tests/ccpp-scm/cases`](../../tests/ccpp-scm/cases/). Examples include:

| CCPP process | Reproducible SCM case manifest | Purpose |
|---|---|---|
| Dust | [`dust.yml`](../../tests/ccpp-scm/cases/dust.yml) | Ginoux surface emission on ARM SGP |
| SeaSalt | [`seasalt.yml`](../../tests/ccpp-scm/cases/seasalt.yml) | GEOS12 ocean emission on COMBLE |
| DryDep | [`drydep.yml`](../../tests/ccpp-scm/cases/drydep.yml) | Wesely gas and GOCART aerosol deposition on ARM SGP |
| WetDep | [`wetdep.yml`](../../tests/ccpp-scm/cases/wetdep.yml) | Jacob wet removal on TWPICE |
| Settling | [`settling.yml`](../../tests/ccpp-scm/cases/settling.yml) | GOCART aerosol settling on BOMEX |
| SO4Chem | [`so4chem.yml`](../../tests/ccpp-scm/cases/so4chem.yml) | sulfur chemistry on ARM SGP |
| CarbChem | [`carbchem.yml`](../../tests/ccpp-scm/cases/carbchem.yml) | carbonaceous aerosol aging on BOMEX |
| Photolysis | [`photolysis.yml`](../../tests/ccpp-scm/cases/photolysis.yml) | TUV-x daylight and night rates on ARM SGP |
| GasChem | [`gaschem.yml`](../../tests/ccpp-scm/cases/gaschem.yml) | MICM chemistry consuming fresh Photolysis rates |

The coupled surface/chemistry/removal and Photolysis/GasChem schedules are specified in
[`emission-chemistry-removal.yml`](../../tests/ccpp-scm/cases/emission-chemistry-removal.yml)
and [`photolysis-gaschem.yml`](../../tests/ccpp-scm/cases/photolysis-gaschem.yml).
They use the same framework-owned `ccpp_constituents` array; CATChem resolves species by
CCPP standard name and preserves unrelated slots. Independent analytic conversions,
permuted registry order and explicit dry/moist/total air bases are covered by
[`constituent-conformance.yml`](../../tests/ccpp-scm/cases/constituent-conformance.yml).

Photolysis and GasChem builds require the pinned MUSICA stack (including TUV-x and MICM);
the other GOCART surface/deposition processes require the locked GOCART sources and case
inputs. Runs that need dust/sea-salt climatology, aerosol optics or photolysis cross sections
must use the staged inputs named by their manifests. A scheme missing its configured
dependency is a build or initialization error, not an implicit no-op.

Use `python3 tests/run_ccpp_scm_suite.py` for the CI suite described in
[`tests/ccpp-scm/README.md`](../../tests/ccpp-scm/README.md). It retrieves locked input data by
default; offline runs can provide a complete SCM `scm/data` tree. This species file does not replace
CCPP's host-side constituent-property registration and merge/allocation lifecycle. Framework,
physics, SCM, input data, compiler and
container revisions are locked in the test harness; a generic CCPP build or a test stub is not
equivalent runtime evidence.
