#include "catchem_core_architecture_test_helpers.hpp"
#include "catchem_execution_plan.hpp"
#include "catchem_photolysis_conversion.hpp"
#include "catchem_time_state.hpp"
#include <array>
#include <cassert>
#include <cmath>

namespace {

/** @brief Test process that publishes one named photolysis-rate product. */
class RateProducer final : public catchem::test::RecordingProcess {
public:
    /** @brief Construct a producer with the supplied event ledger. */
    RateProducer(const std::string& name, std::vector<std::string>& events)
        : RecordingProcess(name, events) {}

    /** @brief Declare this process as the owner of the selected J-rate. */
    catchem::ProcessContract get_contract() const override {
        catchem::ProcessContract contract;
        contract.process_name = get_name();
        contract.fields.push_back(catchem::host_field_3d("T", "K"));
        contract.produced_products.push_back({"photolysis_rate_jfoo", "s-1", true});
        return contract;
    }
};

/** @brief Test process with an optional mechanism-dependent PHOTO requirement. */
class GasChemConsumer final : public catchem::test::RecordingProcess {
public:
    /** @brief Construct a consumer that optionally requires the test rate. */
    GasChemConsumer(const std::string& name, std::vector<std::string>& events, bool uses_photo)
        : RecordingProcess(name, events), uses_photo_(uses_photo) {}

    /** @brief Declare the matching J-rate dependency only for PHOTO mechanisms. */
    catchem::ProcessContract get_contract() const override {
        catchem::ProcessContract contract;
        contract.process_name = get_name();
        if (uses_photo_) contract.products.push_back({"photolysis_rate_jfoo", "photolysis", true});
        return contract;
    }

private:
    bool uses_photo_;
};

} // namespace

/** @brief Check representative tropospheric and stratospheric ozone columns and day/night geometry. */
void verify_ozone_profile_units_and_solar_geometry() {
    constexpr std::array<double, 5> ozone_profile_ppmv = {0.03, 0.04, 0.08, 1.0, 8.0};
    constexpr std::array<double, 5> dry_air_density_profile_kg_m3 = {1.2, 0.9, 0.4, 0.1, 0.015};
    constexpr double dry_air_molar_mass_kg_mol = 0.0289644;
    constexpr double avogadro_mol_inv = 6.02214076e23;
    for (std::size_t level = 0; level < ozone_profile_ppmv.size(); ++level) {
        const double dry_air_number_density_cm3 =
            dry_air_density_profile_kg_m3[level] / dry_air_molar_mass_kg_mol * avogadro_mol_inv * 1.0e-6;
        const double expected_ozone_number_density_cm3 = ozone_profile_ppmv[level] * 1.0e-6 * dry_air_number_density_cm3;
        const double ozone_number_density_cm3 = catchem::photolysis_ozone_ppmv_to_number_density(
            ozone_profile_ppmv[level], dry_air_number_density_cm3);
        assert(std::abs(ozone_number_density_cm3 - expected_ozone_number_density_cm3) <=
               expected_ozone_number_density_cm3 * 1.0e-14);
        assert(ozone_number_density_cm3 > 1.0e10);
        assert(ozone_number_density_cm3 != ozone_profile_ppmv[level]);
    }

    catchem::TimeState solar_time;
    solar_time.year = 2026;
    solar_time.month = 7;
    solar_time.day = 13;
    solar_time.hour = 19; // UTC noon at 105 degrees west.
    solar_time.calculate_derived_fields();
    const double daytime_cosine = solar_time.get_cos_sza(40.0, -105.0);
    solar_time.hour = 6; // Local nighttime at the same location and date.
    solar_time.calculate_derived_fields();
    const double nighttime_cosine = solar_time.get_cos_sza(40.0, -105.0);
    assert(daytime_cosine > 0.0);
    assert(nighttime_cosine < 0.0);
}

/** @brief Verify required J-rate producers and product provenance validation. */
int main() {
    verify_ozone_profile_units_and_solar_geometry();
    std::vector<std::string> events;
    catchem::ExecutionPlan plan;

    const auto photo_consumer = std::make_shared<GasChemConsumer>("gaschem", events, true);
    std::vector<std::shared_ptr<catchem::ProcessInterface>> missing_producer{photo_consumer};
    plan.compile(missing_producer, nullptr);
    assert(plan.validation().has_errors());
    assert(plan.validation().format().find("missing-product-producer") != std::string::npos);

    std::vector<std::shared_ptr<catchem::ProcessInterface>> mismatched_schedule{
        std::make_shared<RateProducer>("alternate_photo", events), photo_consumer};
    plan.compile(mismatched_schedule, nullptr);
    assert(plan.validation().has_errors());
    assert(plan.validation().format().find("product-producer-mismatch") != std::string::npos);

    std::vector<std::shared_ptr<catchem::ProcessInterface>> photo_schedule{
        std::make_shared<RateProducer>("photolysis", events), photo_consumer};
    plan.compile(photo_schedule, nullptr);
    assert(!plan.validation().has_errors());
    assert(plan.product_provenance("photolysis_rate_jfoo") != nullptr);
    catchem::StateManager state(1, 1, 1);
    double temperature = 280.0;
    state.bind_field_3d("T", &temperature);
    const auto original_inputs = plan.input_fingerprint(0, state);
    assert(!plan.product_is_current("photolysis_rate_jfoo", "chapman", 7, 11, original_inputs));
    plan.publish_products(0, "chapman", 7, 11, original_inputs);
    assert(plan.product_is_current("photolysis_rate_jfoo", "chapman", 7, 11, original_inputs));
    assert(!plan.product_is_current("photolysis_rate_jfoo", "other-mechanism", 7, 11));
    assert(!plan.product_is_current("photolysis_rate_jfoo", "chapman", 8, 11));
    // Host wrappers may re-import identical interval inputs between physics calls.
    state.begin_import_generation();
    assert(plan.product_is_current("photolysis_rate_jfoo", "chapman", 7, 12, original_inputs));
    temperature += 1.0;
    const auto changed_inputs = plan.input_fingerprint(0, state);
    assert(changed_inputs != original_inputs);
    assert(!plan.product_is_current("photolysis_rate_jfoo", "chapman", 7, 12, changed_inputs));
    temperature -= 1.0;
    ++state.clock().hour;
    const auto changed_clock = plan.input_fingerprint(0, state);
    assert(changed_clock != original_inputs);
    assert(!plan.product_is_current("photolysis_rate_jfoo", "chapman", 7, 12, changed_clock));
    plan.invalidate_products(0);
    assert(!plan.product_is_current("photolysis_rate_jfoo", "chapman", 7, 11));

    // Mechanisms without PHOTO rate consumers must not acquire a false dependency.
    std::vector<std::shared_ptr<catchem::ProcessInterface>> no_photo_schedule{
        std::make_shared<GasChemConsumer>("gaschem", events, false)};
    plan.compile(no_photo_schedule, nullptr);
    assert(!plan.validation().has_errors());
    assert(plan.product_provenance("photolysis_rate_jfoo") == nullptr);
    return 0;
}
