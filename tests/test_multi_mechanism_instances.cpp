#include "catchem_chem_state.hpp"
#include <cassert>

int main() {
    catchem::ConfigManager first_config;
    first_config.data.mechanism_identity = "first";
    first_config.data.species = {{"alpha"}, {"beta"}};
    catchem::ConfigManager second_config;
    second_config.data.mechanism_identity = "second";
    second_config.data.species = {{"gamma"}, {"delta"}, {"epsilon"}};

    catchem::ChemState first;
    catchem::ChemState second;
    first.load_from_config_manager(first_config);
    second.load_from_config_manager(second_config);

    assert(first.mechanism->identity == "first");
    assert(second.mechanism->identity == "second");
    assert(first.mechanism->species.size() == 2);
    assert(second.mechanism->species.size() == 3);
    assert(!first.mechanism->contains("gamma"));
    assert(!second.mechanism->contains("alpha"));
    return 0;
}
