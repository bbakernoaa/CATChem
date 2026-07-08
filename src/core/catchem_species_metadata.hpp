#pragma once
#include <string>
#include <vector>

namespace catchem {

struct SpeciesMetadata {
    // Names
    std::string short_name;
    std::string long_name;
    std::string description;

    // Classification switches
    bool is_gas = false;
    bool is_aerosol = false;
    bool is_tracer = false;
    bool is_advected = true;
    bool is_drydep = false;
    bool is_wetdep = false;
    bool is_photolysis = false;
    bool is_gocart_aero = false;
    bool is_dust = false;
    bool is_seasalt = false;

    // Physical / Numerical properties
    double mw_g = 0.0;
    double density = 0.0;
    double radius = 0.0;
    double lower_radius = 0.0;
    double upper_radius = 0.0;
    double viscosity = 0.0;

    // Dry deposition parameters
    double dd_f0 = 0.0;
    double dd_hstar = 0.0;
    double dd_DvzAerSnow = 0.0;
    double dd_DvzMinVal_snow = 0.0;
    double dd_DvzMinVal_land = 0.0;

    // Wet deposition parameters
    double wd_retfactor = 0.0;
    bool wd_LiqAndGas = false;
    double wd_convfacI2G = 0.0;
    std::vector<double> wd_rainouteff;
    std::string mie_name;
};

} // namespace catchem
