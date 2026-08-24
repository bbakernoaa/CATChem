#include "catchem_api.hpp"
#include "catchem_config_manager.hpp"
#include <cassert>
#include <string>

#ifndef CATCHEM_TEST_SOURCE_DIR
#define CATCHEM_TEST_SOURCE_DIR "."
#endif

int main() {
    const std::string fixtures = std::string(CATCHEM_TEST_SOURCE_DIR) + "/fixtures/";
    catchem::ConfigManager valid;
    valid.load_from_file(fixtures + "platform_integrity_valid.yml");
    valid.load_species_file(fixtures + "platform_integrity_species.yml");
    assert(!valid.validate().has_errors());
    assert(valid.data.species[0].name == "CO");
    assert(valid.data.species[1].name == "O3");

    catchem::ConfigManager invalid;
    invalid.load_from_file(fixtures + "platform_integrity_invalid.yml");
    catchem::SpeciesConfig first;
    first.name = "O3";
    catchem::SpeciesConfig duplicate;
    duplicate.name = "o3";
    invalid.data.species = {first, duplicate};
    catchem::EmissionFieldMapping mapping;
    mapping.map = {"MISSING", "O3"};
    mapping.scale = {1.0};
    invalid.data.emission_mappings["anthro"].fields["x"] = mapping;
    const auto& report = invalid.validate();
    assert(report.has_errors());
    const std::string text = report.format();
    for (const char* required : {"simulation/species_filename", "simulation/nx", "grid/number_of_levels",
                                 "diagnostics/output/frequency", "unknown process", "duplicate species",
                                 "different lengths", "absent from active mechanism", "unknown_section"})
        assert(text.find(required) != std::string::npos);

    void* core = reinterpret_cast<void*>(1);
    assert(catchem_core_create_from_config_checked((fixtures + "platform_integrity_invalid.yml").c_str(), &core) !=
           CATCHEM_SUCCESS);
    assert(core == nullptr);
    return 0;
}
