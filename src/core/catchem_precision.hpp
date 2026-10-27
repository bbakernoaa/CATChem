/**
 * @file catchem_precision.hpp
 * @brief Single switch selecting the working real type for the CATChem core.
 *
 * @details Every physical constant and meteorology utility in the core is
 * declared in terms of catchem::fp, so the whole library is rebuilt at either
 * double or single precision by the USE_REAL8 preprocessor flag (set by the
 * CMake configuration to match the host model's compile-time real kind). Code
 * that must interoperate with a Fortran host of a fixed kind should use fp
 * rather than hardcoding double, because the two branches are not layout
 * compatible.
 */
#pragma once

namespace catchem {

#ifdef USE_REAL8
    /// Working real type when USE_REAL8 is defined (Fortran kind=8 host).
    using fp = double;
#else
    /// Working real type when USE_REAL8 is absent (Fortran kind=4 host).
    using fp = float;
#endif

} // namespace catchem
