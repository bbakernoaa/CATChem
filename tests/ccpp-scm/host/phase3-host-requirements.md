# Phase 3 SCM host integration requirements

The locked SCM revision exposes the physics state through `physics_type` and calls only the
`radiation`, `phys_ps`, and `phys_ts` CCPP groups. Its existing tracer path is the GFS-specific
`Statein%qgrs` pointer into `scm_state%state_tracer`; it does not declare the generic CCPP
`ccpp_constituents` array or a CATChem context.

A Phase 3 host patch must therefore:

1. Add a host-owned CATChem context/config path, interval token, and complete CCPP constituent
   array with CCPP metadata. Populate the array from case inputs and retain the merged property
   pointers for the entire CCPP lifecycle.
2. Create/bind/initialize one CATChem context per SCM physics instance before the selected process
   scheme's CCPP init phase, and destroy it after all schemes finalize.
3. Route calls through the shared CCPP array. Each selected process imports current host fields,
   calls only `run_selected_process`, and publishes a checked contribution. Process failures must
   propagate through SCM's `errflg` path.
4. Add current meteorological fields, static species physical properties, optional optics resources,
   solar clock inputs, and surface data required by the configured CATChem process contracts.
5. Insert the selected process at a declared phase in the SCM group call sequence. Preserve SO4Chem
   after Dust, SeaSalt, WetDep, and Settling for the requested sulfur-cycle ordering.
6. Expose a runtime marker emitted only after exactly one CATChem selected-process execution; the
   case harness must reject missing, duplicate, or mismatched markers.

Adding a scheme to a suite XML is insufficient by itself: `ccpp_capgen` must also receive its
metadata, the SCM `ccpp/CMakeLists.txt` must include its scheme metadata, host variables and host
metadata must resolve every scheme argument, and the SCM host must invoke the generated group at a
physics phase. No acceptance case is complete until its selected process produces a nonzero,
physically bounded signal and its budget checks pass.

The current isolated SCM overlay also adds name-based synchronization between GFS `qgrs` and
the CCPP constituent state. It imports matching tracers after CCPP constituent allocation and
before each physics group, then exports the updated shared array to GFS after each group. It
uses CCPP constituent `diagnostic_name` values and the framework's 1-based `const_index`.
Unmatched registered constituents remain in CCPP storage. This bridge has not yet passed a
locked SCM compile or run: selected-suite cap generation currently stops because the SCM host
does not expose `catchem_clay_fraction`, which the Dust scheme requires.

The lifecycle metadata and Dust suite entries were verified with pinned capgen. Capgen resolves
those phases and stops at missing `catchem_clay_fraction`. The constituent bridge has not yet
passed a locked SCM compile or run.
