/**
 * @file catchem_emission_manager.cpp
 * @brief Out-of-line definitions for the EmissionManager registry.
 *
 * @details Documentation is inherited from catchem_emission_manager.hpp; the
 * manager only stores and looks up category copies, so there are no
 * cpp-local helpers to document here.
 */
#include "catchem_emission_manager.hpp"

namespace catchem {

    void EmissionManager::add_category(const EmissionCategory& category) {
        categories[category.category_name] = category;
        is_loaded = true;
    }

    bool EmissionManager::has_category(const std::string& name) const {
        return categories.find(name) != categories.end();
    }

    const EmissionCategory* EmissionManager::get_category(const std::string& name) const {
        auto it = categories.find(name);
        return it != categories.end() ? &it->second : nullptr;
    }

} // namespace catchem
