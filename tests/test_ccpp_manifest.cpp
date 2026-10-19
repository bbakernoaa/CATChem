#include "catchem_manifest.hpp"
#include "catchem_api.hpp"
#include "catchem_runtime_lease.hpp"
#include "catchem_test_config.hpp"
#include <cassert>
#include <filesystem>
#include <fstream>
#include <stdexcept>
#include <string>

namespace {
std::string fixture(const char* name) {
    return std::string(catchem::test::SOURCE_DIR) + "/tests/fixtures/" + name;
}
void write_config(const std::filesystem::path& dir, const std::string& species) {
    std::ofstream(dir / "species.yml") << species;
    std::ofstream(dir / "config.yml") << "simulation:\n  name: manifest-test\n  species_filename: species.yml\nprocesses:\n  dust:\n    activate: false\n";
}
}

int main() {
    const auto before = catchem::RuntimeLease::snapshot();
    catchem::MechanismManifest a(fixture("platform_integrity_valid.yml"));
    catchem::MechanismManifest b(fixture("platform_integrity_valid.yml"));
    assert(!a.fingerprint().empty() && a.fingerprint() == b.fingerprint());
    assert(a.species().size() == 3);
    assert(a.species_index("co") == 0);
    catchem::MechanismManifest disabled(fixture("configured_process_disabled.yml"));
    assert(disabled.processes().size() == 1 && !disabled.processes()[0].available);
    assert(catchem::RuntimeLease::snapshot().lease_count == before.lease_count);
    void* handle = nullptr;
    assert(catchem_manifest_open_checked(fixture("configured_process_disabled.yml").c_str(), &handle) == CATCHEM_SUCCESS);
    int required = 0;
    assert(catchem_manifest_get_species_name_checked(handle, 0, nullptr, 0, &required) == CATCHEM_SUCCESS);
    assert(required > 1);
    char short_buffer[2] = {};
    assert(catchem_manifest_get_species_name_checked(handle, 0, short_buffer, 2, &required) == CATCHEM_EXTENT_MISMATCH);
    assert(short_buffer[0] == '\0');
    catchem_manifest_species_v1 desc{};
    desc.schema_version = 1;
    desc.struct_size = sizeof(desc);
    char name_buf[256] = {}, standard_buf[256] = {}, units_buf[64] = {};
    assert(catchem_manifest_get_species_v1_checked(handle, 0, &desc, name_buf, sizeof(name_buf), standard_buf,
                                                   sizeof(standard_buf), units_buf, sizeof(units_buf)) == CATCHEM_SUCCESS);
    assert(desc.name_required_len == static_cast<int>(std::string(name_buf).size() + 1));
    assert(std::string(name_buf) == "CO" && std::string(standard_buf) == "CO");
    assert(catchem_manifest_destroy_checked(handle) == CATCHEM_SUCCESS);
    int stale_count = 0;
    assert(catchem_manifest_get_species_count_checked(handle, &stale_count) == CATCHEM_INVALID_HANDLE);

    const auto dir = std::filesystem::temp_directory_path() / "catchem-manifest-tests";
    std::filesystem::create_directories(dir);
    write_config(dir, "- name: " + std::string(80, 'A') + "\n  ccpp_standard_name: " + std::string(140, 'b') +
                       "\n  aliases: [long-alias]\n  units: ppmv\n  amount_type: mole_fraction\n  air_basis: dry\n");
    catchem::MechanismManifest long_names((dir / "config.yml").string());
    assert(long_names.species()[0].name.size() == 80);
    assert(long_names.species()[0].standard_name.size() == 140);
    assert(long_names.species_index("LONG-ALIAS") == 0);
    assert(long_names.species()[0].host_representation.amount_type == catchem::AmountType::MoleFraction);
    assert(long_names.species()[0].host_representation.air_basis == catchem::AirBasis::Dry);
    void* fingerprint_handle = nullptr;
    assert(catchem_manifest_open_checked((dir / "config.yml").string().c_str(), &fingerprint_handle) == CATCHEM_SUCCESS);
    assert(catchem_manifest_validate_config_checked(fingerprint_handle, (dir / "config.yml").string().c_str()) == CATCHEM_SUCCESS);
    write_config(dir, "- name: " + std::string(80, 'A') + "\n  ccpp_standard_name: " + std::string(140, 'b') +
                       "\n  aliases: [long-alias]\n  units: mol mol-1\n");
    assert(catchem_manifest_validate_config_checked(fingerprint_handle, (dir / "config.yml").string().c_str()) == CATCHEM_CONTRACT_VIOLATION);
    assert(catchem_manifest_destroy_checked(fingerprint_handle) == CATCHEM_SUCCESS);

    write_config(dir, "- name: A\n  aliases: [shared]\n- name: B\n  aliases: [SHARED]\n");
    bool collision = false;
    try { catchem::MechanismManifest invalid((dir / "config.yml").string()); }
    catch (const std::invalid_argument&) { collision = true; }
    assert(collision);

    write_config(dir, "- name: A\n  units: furlongs per fortnight\n");
    bool bad_units = false;
    try { catchem::MechanismManifest invalid((dir / "config.yml").string()); }
    catch (const std::invalid_argument&) { bad_units = true; }
    assert(bad_units);
    std::filesystem::remove_all(dir);
    return 0;
}
