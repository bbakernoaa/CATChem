// src/core/catchem_diagnostic.cpp
#include "catchem_diagnostic_manager.hpp"
#include <algorithm>
#include <stdexcept>

namespace catchem {

#ifdef CATCHEM_ENABLE_KOKKOS

    DiagnosticField::DiagnosticField(const std::string& name_val, const std::string& desc_val,
                                     const std::string& units_val, DiagType type_val, const std::vector<int>& dims,
                                     DiagnosticPolicy policy, double reset, std::vector<SemanticAxis> semantic_axes)
        : name(name_val), description(desc_val), units(units_val), type(type_val), dimensions(dims),
          axes(std::move(semantic_axes)), reset_policy(policy), reset_value(reset) {
        is_gpu_target = !std::is_same_v<HostSpace, DeviceSpace>;

        if (type == DiagType::FIELD_2D) {
            if (dims.size() != 2)
                throw std::invalid_argument("2D field requires 2 dimensions");
            if (is_gpu_target)
                device_view_2d = View2D("dev_" + name, dims[0], dims[1]);
            host_view_2d = HostView2D("host_" + name, dims[0], dims[1]);
            if (!is_gpu_target)
                device_view_2d = host_view_2d;
        } else if (type == DiagType::FIELD_3D) {
            if (dims.size() != 3)
                throw std::invalid_argument("3D field requires 3 dimensions");
            if (is_gpu_target)
                device_view_3d = View3D("dev_" + name, dims[0], dims[1], dims[2]);
            host_view_3d = HostView3D("host_" + name, dims[0], dims[1], dims[2]);
            if (!is_gpu_target)
                device_view_3d = host_view_3d;
        } else {
            throw std::invalid_argument("Unsupported DiagType");
        }
    }

    void DiagnosticField::sync_to_host() {
        if (latest_writer != LatestWriter::DeviceCurrent)
            return;
        if constexpr (!std::is_same_v<HostSpace, DeviceSpace>) {
            if (type == DiagType::FIELD_2D) {
                Kokkos::deep_copy(host_view_2d, device_view_2d);
            } else if (type == DiagType::FIELD_3D) {
                Kokkos::deep_copy(host_view_3d, device_view_3d);
            }
        }
        latest_writer = LatestWriter::Synchronized;
    }

    void DiagnosticField::sync_to_device() {
        if (latest_writer != LatestWriter::HostCurrent)
            return;
        if constexpr (!std::is_same_v<HostSpace, DeviceSpace>) {
            if (type == DiagType::FIELD_2D) {
                Kokkos::deep_copy(device_view_2d, host_view_2d);
            } else if (type == DiagType::FIELD_3D) {
                Kokkos::deep_copy(device_view_3d, host_view_3d);
            }
        }
        latest_writer = LatestWriter::Synchronized;
    }

    void DiagnosticField::reset() {
        if (type == DiagType::FIELD_2D) {
            Kokkos::deep_copy(device_view_2d, reset_value);
            Kokkos::deep_copy(host_view_2d, reset_value);
        } else if (type == DiagType::FIELD_3D) {
            Kokkos::deep_copy(device_view_3d, reset_value);
            Kokkos::deep_copy(host_view_3d, reset_value);
        }
        latest_writer = LatestWriter::Synchronized;
    }

    Kokkos::View<double**, Kokkos::LayoutLeft, Kokkos::DefaultExecutionSpace::memory_space>
    DiagnosticManager::get_device_view_2d(const std::string& name) {
        auto field = get_field(name);
        if (field->type != DiagType::FIELD_2D)
            throw std::invalid_argument("Field is not 2D: " + name);
        field->sync_to_device();
        field->mark_device_modified();
        return field->device_view_2d;
    }

    Kokkos::View<double***, Kokkos::LayoutLeft, Kokkos::DefaultExecutionSpace::memory_space>
    DiagnosticManager::get_device_view_3d(const std::string& name) {
        auto field = get_field(name);
        if (field->type != DiagType::FIELD_3D)
            throw std::invalid_argument("Field is not 3D: " + name);
        field->sync_to_device();
        field->mark_device_modified();
        return field->device_view_3d;
    }

    const void* DiagnosticManager::get_host_read_pointer(const std::string& name) {
        auto field = get_field(name);
        field->sync_to_host();
        if (field->type == DiagType::FIELD_2D) {
            return static_cast<void*>(field->host_view_2d.data());
        } else if (field->type == DiagType::FIELD_3D) {
            return static_cast<void*>(field->host_view_3d.data());
        }
        return nullptr;
    }

#else // host-only build

    DiagnosticField::DiagnosticField(const std::string& name_val, const std::string& desc_val,
                                     const std::string& units_val, DiagType type_val, const std::vector<int>& dims,
                                     DiagnosticPolicy policy, double reset, std::vector<SemanticAxis> semantic_axes)
        : name(name_val), description(desc_val), units(units_val), type(type_val), dimensions(dims),
          axes(std::move(semantic_axes)), reset_policy(policy), reset_value(reset) {
        is_gpu_target = false;

        if (type == DiagType::FIELD_2D) {
            if (dims.size() != 2)
                throw std::invalid_argument("2D field requires 2 dimensions");
            storage.assign(static_cast<size_t>(dims[0]) * dims[1], 0.0);
        } else if (type == DiagType::FIELD_3D) {
            if (dims.size() != 3)
                throw std::invalid_argument("3D field requires 3 dimensions");
            storage.assign(static_cast<size_t>(dims[0]) * dims[1] * dims[2], 0.0);
        } else {
            throw std::invalid_argument("Unsupported DiagType");
        }
    }

    void DiagnosticField::sync_to_host() {}
    void DiagnosticField::sync_to_device() {
        latest_writer = LatestWriter::Synchronized;
    }

    void DiagnosticField::reset() {
        std::fill(storage.begin(), storage.end(), reset_value);
        latest_writer = LatestWriter::Synchronized;
    }

    DiagnosticField::Mdspan2D DiagnosticManager::get_device_view_2d(const std::string& name) {
        auto field = get_field(name);
        if (field->type != DiagType::FIELD_2D)
            throw std::invalid_argument("Field is not 2D: " + name);
        field->sync_to_device();
        field->mark_device_modified();
        return DiagnosticField::Mdspan2D(field->storage.data(), field->dimensions[0], field->dimensions[1]);
    }

    DiagnosticField::Mdspan3D DiagnosticManager::get_device_view_3d(const std::string& name) {
        auto field = get_field(name);
        if (field->type != DiagType::FIELD_3D)
            throw std::invalid_argument("Field is not 3D: " + name);
        field->sync_to_device();
        field->mark_device_modified();
        return DiagnosticField::Mdspan3D(field->storage.data(), field->dimensions[0], field->dimensions[1],
                                         field->dimensions[2]);
    }

    const void* DiagnosticManager::get_host_read_pointer(const std::string& name) {
        auto field = get_field(name);
        field->sync_to_host();
        if (field->type == DiagType::FIELD_2D || field->type == DiagType::FIELD_3D) {
            return static_cast<void*>(field->storage.data());
        }
        return nullptr;
    }

#endif

    void* DiagnosticManager::get_host_write_pointer(const std::string& name) {
        auto field = get_field(name);
        const void* pointer = get_host_read_pointer(name);
        field->mark_host_modified();
        return const_cast<void*>(pointer);
    }

    void DiagnosticField::advance_generation(std::size_t value) {
        generation = value;
        generation_failed = false;
        if (reset_policy != DiagnosticPolicy::Persistent)
            reset();
        availability = AvailabilityState::Current;
    }

    void DiagnosticManager::register_field(const std::string& name, const std::string& desc, const std::string& units,
                                           DiagType type, const std::vector<int>& dims) {
        std::vector<SemanticAxis> axes;
        if (dims.size() >= 1)
            axes.push_back(SemanticAxis::Column);
        if (dims.size() >= 2)
            axes.push_back(dims[1] == 1 ? SemanticAxis::Singleton : SemanticAxis::Level);
        if (dims.size() >= 3)
            axes.push_back(SemanticAxis::Species);
        register_field_contract(name, desc, units, type, dims, DiagnosticPolicy::Instantaneous, 0.0, axes);
    }

    void DiagnosticManager::register_field_contract(const std::string& name, const std::string& desc,
                                                    const std::string& units, DiagType type,
                                                    const std::vector<int>& dims, DiagnosticPolicy policy,
                                                    double reset_value, const std::vector<SemanticAxis>& axes) {
        if (name.empty() || desc.empty() || units.empty() || dims.empty() || dims.size() != axes.size() ||
            std::any_of(dims.begin(), dims.end(), [](int d) { return d <= 0; }))
            throw std::invalid_argument("Invalid diagnostic contract: " + name);
        auto existing = fields.find(name);
        if (existing != fields.end()) {
            const auto& field = *existing->second;
            if (field.type != type || field.dimensions != dims || field.units != units || field.description != desc ||
                field.axes != axes || field.reset_policy != policy || field.reset_value != reset_value)
                throw std::invalid_argument("Incompatible diagnostic re-registration: " + name);
            return;
        }
        auto field = std::make_shared<DiagnosticField>(name, desc, units, type, dims, policy, reset_value, axes);
        field->registration_generation = generation_;
        fields[name] = std::move(field);
    }

    bool DiagnosticManager::has_field(const std::string& name) const {
        return fields.find(name) != fields.end();
    }

    std::shared_ptr<DiagnosticField> DiagnosticManager::get_field(const std::string& name) {
        if (!has_field(name))
            throw std::invalid_argument("Field not found: " + name);
        return fields.at(name);
    }

    void DiagnosticManager::sync_to_host() {
        for (auto& [key, field] : fields) {
            field->sync_to_host();
        }
    }

    void DiagnosticManager::sync_to_device() {
        for (auto& [key, field] : fields) {
            field->sync_to_device();
        }
    }

    void DiagnosticManager::reset_all() {
        for (auto& [key, field] : fields) {
            field->reset();
        }
    }

    void DiagnosticManager::begin_timestep() {
        ++generation_;
        for (auto& [key, field] : fields)
            field->advance_generation(generation_);
    }

    void DiagnosticManager::mark_generation_failed() {
        for (auto& [key, field] : fields) {
            if (field->generation == generation_)
                field->generation_failed = true;
        }
    }

    std::vector<std::string> DiagnosticManager::get_registered_names() const {
        std::vector<std::string> names;
        for (const auto& [name, field] : fields) {
            names.push_back(name);
        }
        return names;
    }

    void DiagnosticManager::mark_host_modified(const std::string& name) {
        get_field(name)->mark_host_modified();
    }
    void DiagnosticManager::mark_device_modified(const std::string& name) {
        get_field(name)->mark_device_modified();
    }

} // namespace catchem
