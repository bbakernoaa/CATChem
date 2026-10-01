// settling_gocart_kernels.hpp
//
// Native C++ transcription of the GOCART2G gravitational-settling science, so
// a CATChem process can run settling without the Fortran science bridge. The
// formulas, constants, and arithmetic ordering mirror the upstream Fortran
// kernels (Chem_CalcVsettle, ParticleSwelling_Fitzgerald/_Gerber,
// SettlingSolverUFS, findKlid, and the Chem_Settling column driver) as closely
// as a C++ port can; the fall speed and transport are computed here, while the
// wet particle radius and density may be supplied by an external aerosol model
// (the caller decides) or by the swelling parameterizations below.
//
// All arrays are single-column and ordered top-to-bottom (level index 0 is the
// model top), matching how the Fortran scheme reverses the host's bottom-to-top
// vertical before calling the kernels. Levels above the active pressure lid are
// expected to be present but carry zero settling velocity, so they stay inert.
//
// Real-8 throughout: the Fortran science is compiled -fdefault-real-8, so a
// double-precision C++ port reproduces it to floating-point roundoff.
#pragma once

#include <cstddef>

namespace catchem {
    namespace settling {

        // Standard gravity used by the schemes (mirrors the bridge's g0).
        inline constexpr double kGravity = 9.80665;      // [m s^-2]
        // Pressure lid height used by the settling scheme (mirrors plid).
        inline constexpr double kPressureLidHpa = 0.01;  // [hPa]

        // Dynamic viscosity of air from the corrected Sutherland equation, at
        // the same temperature the fall-speed kernel uses. Exposed for tests.
        double dynamic_viscosity(double temperature_k);

        // Single-level aerosol fall speed [m s^-1] for a particle of `radius`
        // [m] and density `rhop` [kg m^-3] in air of density `rhoa`
        // [kg m^-3] and temperature `tmpu` [K]. Includes the Cunningham slip
        // correction and, above a Reynolds number of 0.01, the Pruppacher and
        // Klett drag correction, exactly as the upstream kernel does.
        double calc_vsettle(double radius, double rhop, double rhoa, double tmpu, double grav);

        // Fill a top-to-bottom fall-speed profile from per-level wet radius and
        // wet density plus the air state. `n` levels; any of the output/inputs
        // may be null only if unused. This is the vector form of calc_vsettle.
        void calc_vsettle_profile(const double* radius_wet, const double* rhop_wet, const double* rhoa,
                                  const double* tmpu, int n, double grav, double* vsettle_out);

        // Maring et al. (2003) upward-velocity correction applied to a fall
        // speed in place: v = max(1e-9, v - 0.33e-2).
        void apply_maring_correction(double* vsettle, int n);

        // Hygroscopic swelling following Fitzgerald (1975): from a dry radius
        // [m] and dry density [kg m^-3] at relative humidity `rh` (fraction,
        // not percent), return the wet radius and wet density. Below the 0.80
        // deliquescence threshold the particle is returned unchanged.
        void swelling_fitzgerald(double radius_dry, double rhop_dry, double rh, double* radius_out,
                                 double* rhop_out);

        // Hygroscopic swelling following Gerber (1985); same contract as the
        // Fitzgerald entry point.
        void swelling_gerber(double radius_dry, double rhop_dry, double rh, double* radius_out, double* rhop_out);

        // Swelling dispatch mirroring the Fortran flag selector: 0 = no
        // swelling, 1 = Fitzgerald, 2 = Gerber. Any other flag leaves the
        // particle dry, matching the upstream default branch.
        void swell(double radius_dry, double rhop_dry, double rh, int flag, double* radius_out, double* rhop_out);

        // Whether the configured swelling method actually resizes a particle at
        // relative humidity `rh` (fraction). This is the activation rule the
        // Fortran baseline applies, so a caller that swaps in a different size
        // source must gate on it to stay faithful: Fitzgerald resizes only above
        // its 0.80 deliquescence threshold (below it the Fortran returns the dry
        // particle for every species); Gerber resizes at all relative
        // humidities; no-swelling and unknown flags never resize. A caller
        // substituting its own sizing where this returns false would swell a
        // particle the baseline leaves dry.
        bool swelling_active(double rh, int flag);

        // Pressure-lid index: the level whose pressure is nearest `plid_hpa`
        // (converted to Pa), scanning top-to-bottom with the first minimum
        // winning ties, exactly as findKlid does. Returns a zero-based index
        // into `press` (length `n`); the active settling column starts here.
        int find_klid(const double* press, int n, double plid_hpa);

        // Layer thickness [m] from hydrostatic balance, given geometric-height
        // edges ordered top-to-bottom (length n+1). Writes `dz` (length n):
        // level k spans edges k and k+1, and the bottom level spans edge n-1 to
        // the surface edge n.
        void layer_thickness(const double* hghte_edges, int n, double* dz_out);

        // Column dry mass [kg m^-2] = sum over active levels of qa * delp / g,
        // from zero-based level `klid0` through `n-1` inclusive. `qa` is mixing
        // ratio [kg kg^-1], `delp` pressure thickness [Pa].
        double column_mass(const double* qa, const double* delp, int klid0, int n, double grav);

        // Vertical gravitational-settling transport for one column, the UFS
        // scheme: a CFL-substepped, first-order, mass-conserving, non-negative
        // top-to-bottom transfer driven by per-level settling rate tau = vs/dz.
        // `qa` (length n) is updated in place; `delp`, `dz`, `vs` are inputs
        // (length n). Reproduces the upstream sub-step selection including the
        // case where the coarse CFL criterion yields no transport this step.
        void settling_solver_ufs(double cdt, const double* delp, const double* dz, const double* vs, double* qa,
                                 int n);

    } // namespace settling
} // namespace catchem
