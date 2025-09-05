#include "yaml_interface.h"
#include <yaml-cpp/yaml.h>
#include <iostream>
#include <fstream>
#include <string>
#include <cstring>
#include <memory>

// Internal node management
struct YamlNodeWrapper {
    YAML::Node node;
    YamlNodeWrapper(const YAML::Node& n) : node(n) {}
};

extern "C" {

// Node management functions
void* yaml_load_file(const char* filename) {
    try {
        YAML::Node node = YAML::LoadFile(filename);
        return new YamlNodeWrapper(node);
    } catch (const std::exception& e) {
        std::cerr << "Error loading YAML file: " << e.what() << std::endl;
        return nullptr;
    }
}

void* yaml_load_string(const char* yaml_string) {
    try {
        YAML::Node node = YAML::Load(yaml_string);
        return new YamlNodeWrapper(node);
    } catch (const std::exception& e) {
        std::cerr << "Error loading YAML string: " << e.what() << std::endl;
        return nullptr;
    }
}

void yaml_destroy_node(void* node_ptr) {
    if (node_ptr) {
        delete static_cast<YamlNodeWrapper*>(node_ptr);
    }
}

// Getter functions
bool yaml_get_string(void* node_ptr, const char* key, char* value, int max_len) {
    if (!node_ptr || !key || !value) return false;

    try {
        YamlNodeWrapper* wrapper = static_cast<YamlNodeWrapper*>(node_ptr);
        if (wrapper->node[key]) {
            std::string result = wrapper->node[key].as<std::string>();
            strncpy(value, result.c_str(), max_len - 1);
            value[max_len - 1] = '\0';
            return true;
        }
    } catch (const std::exception& e) {
        std::cerr << "Error getting string value: " << e.what() << std::endl;
    }
    return false;
}

bool yaml_get_integer(void* node_ptr, const char* key, int* value) {
    if (!node_ptr || !key || !value) return false;

    try {
        YamlNodeWrapper* wrapper = static_cast<YamlNodeWrapper*>(node_ptr);
        if (wrapper->node[key]) {
            *value = wrapper->node[key].as<int>();
            return true;
        }
    } catch (const std::exception& e) {
        std::cerr << "Error getting integer value: " << e.what() << std::endl;
    }
    return false;
}

bool yaml_get_real(void* node_ptr, const char* key, double* value) {
    if (!node_ptr || !key || !value) return false;

    try {
        YamlNodeWrapper* wrapper = static_cast<YamlNodeWrapper*>(node_ptr);
        if (wrapper->node[key]) {
            *value = wrapper->node[key].as<double>();
            return true;
        }
    } catch (const std::exception& e) {
        std::cerr << "Error getting real value: " << e.what() << std::endl;
    }
    return false;
}

bool yaml_get_logical(void* node_ptr, const char* key, bool* value) {
    if (!node_ptr || !key || !value) return false;

    try {
        YamlNodeWrapper* wrapper = static_cast<YamlNodeWrapper*>(node_ptr);
        if (wrapper->node[key]) {
            *value = wrapper->node[key].as<bool>();
            return true;
        }
    } catch (const std::exception& e) {
        std::cerr << "Error getting logical value: " << e.what() << std::endl;
    }
    return false;
}

// Array getter functions
bool yaml_get_real_array(void* node_ptr, const char* key, double* values, int max_size, int* actual_size) {
    if (!node_ptr || !key || !values || !actual_size) return false;

    try {
        YamlNodeWrapper* wrapper = static_cast<YamlNodeWrapper*>(node_ptr);
        if (wrapper->node[key] && wrapper->node[key].IsSequence()) {
            const YAML::Node& seq = wrapper->node[key];
            *actual_size = std::min(static_cast<int>(seq.size()), max_size);

            for (int i = 0; i < *actual_size; ++i) {
                values[i] = seq[i].as<double>();
            }
            return true;
        }
    } catch (const std::exception& e) {
        std::cerr << "Error getting real array: " << e.what() << std::endl;
    }
    return false;
}

bool yaml_get_integer_array(void* node_ptr, const char* key, int* values, int max_size, int* actual_size) {
    if (!node_ptr || !key || !values || !actual_size) return false;

    try {
        YamlNodeWrapper* wrapper = static_cast<YamlNodeWrapper*>(node_ptr);
        if (wrapper->node[key] && wrapper->node[key].IsSequence()) {
            const YAML::Node& seq = wrapper->node[key];
            *actual_size = std::min(static_cast<int>(seq.size()), max_size);

            for (int i = 0; i < *actual_size; ++i) {
                values[i] = seq[i].as<int>();
            }
            return true;
        }
    } catch (const std::exception& e) {
        std::cerr << "Error getting integer array: " << e.what() << std::endl;
    }
    return false;
}

bool yaml_get_string_array(void* node_ptr, const char* key, char* values, int max_strings, int max_len, int* actual_size) {
    if (!node_ptr || !key || !values || !actual_size) return false;

    try {
        YamlNodeWrapper* wrapper = static_cast<YamlNodeWrapper*>(node_ptr);
        if (wrapper->node[key] && wrapper->node[key].IsSequence()) {
            const YAML::Node& seq = wrapper->node[key];
            *actual_size = std::min(static_cast<int>(seq.size()), max_strings);

            for (int i = 0; i < *actual_size; ++i) {
                std::string str = seq[i].as<std::string>();
                char* dest = values + i * max_len;
                strncpy(dest, str.c_str(), max_len - 1);
                dest[max_len - 1] = '\0';
            }
            return true;
        }
    } catch (const std::exception& e) {
        std::cerr << "Error getting string array: " << e.what() << std::endl;
    }
    return false;
}

// Utility functions
bool yaml_has_key(void* node_ptr, const char* key) {
    if (!node_ptr || !key) return false;

    try {
        YamlNodeWrapper* wrapper = static_cast<YamlNodeWrapper*>(node_ptr);
        return wrapper->node[key].IsDefined();
    } catch (const std::exception& e) {
        std::cerr << "Error checking key existence: " << e.what() << std::endl;
    }
    return false;
}

int yaml_get_size(void* node_ptr) {
    if (!node_ptr) return 0;

    try {
        YamlNodeWrapper* wrapper = static_cast<YamlNodeWrapper*>(node_ptr);
        return static_cast<int>(wrapper->node.size());
    } catch (const std::exception& e) {
        std::cerr << "Error getting node size: " << e.what() << std::endl;
    }
    return 0;
}

bool yaml_is_sequence(void* node_ptr) {
    if (!node_ptr) return false;

    try {
        YamlNodeWrapper* wrapper = static_cast<YamlNodeWrapper*>(node_ptr);
        return wrapper->node.IsSequence();
    } catch (const std::exception& e) {
        std::cerr << "Error checking if sequence: " << e.what() << std::endl;
    }
    return false;
}

bool yaml_is_map(void* node_ptr) {
    if (!node_ptr) return false;

    try {
        YamlNodeWrapper* wrapper = static_cast<YamlNodeWrapper*>(node_ptr);
        return wrapper->node.IsMap();
    } catch (const std::exception& e) {
        std::cerr << "Error checking if map: " << e.what() << std::endl;
    }
    return false;
}

// Setter functions
bool yaml_set_string(void* node_ptr, const char* key, const char* value) {
    if (!node_ptr || !key || !value) return false;

    try {
        YamlNodeWrapper* wrapper = static_cast<YamlNodeWrapper*>(node_ptr);
        wrapper->node[key] = std::string(value);
        return true;
    } catch (const std::exception& e) {
        std::cerr << "Error setting string value: " << e.what() << std::endl;
    }
    return false;
}

bool yaml_set_integer(void* node_ptr, const char* key, int value) {
    if (!node_ptr || !key) return false;

    try {
        YamlNodeWrapper* wrapper = static_cast<YamlNodeWrapper*>(node_ptr);
        wrapper->node[key] = value;
        return true;
    } catch (const std::exception& e) {
        std::cerr << "Error setting integer value: " << e.what() << std::endl;
    }
    return false;
}

bool yaml_set_real(void* node_ptr, const char* key, double value) {
    if (!node_ptr || !key) return false;

    try {
        YamlNodeWrapper* wrapper = static_cast<YamlNodeWrapper*>(node_ptr);
        wrapper->node[key] = value;
        return true;
    } catch (const std::exception& e) {
        std::cerr << "Error setting real value: " << e.what() << std::endl;
    }
    return false;
}

bool yaml_set_logical(void* node_ptr, const char* key, bool value) {
    if (!node_ptr || !key) return false;

    try {
        YamlNodeWrapper* wrapper = static_cast<YamlNodeWrapper*>(node_ptr);
        wrapper->node[key] = value;
        return true;
    } catch (const std::exception& e) {
        std::cerr << "Error setting logical value: " << e.what() << std::endl;
    }
    return false;
}

// File operations
bool yaml_save_file(void* node_ptr, const char* filename) {
    if (!node_ptr || !filename) return false;

    try {
        YamlNodeWrapper* wrapper = static_cast<YamlNodeWrapper*>(node_ptr);
        std::ofstream fout(filename);
        fout << wrapper->node;
        return true;
    } catch (const std::exception& e) {
        std::cerr << "Error saving YAML file: " << e.what() << std::endl;
    }
    return false;
}

// Get all keys from a YAML map
bool yaml_get_all_keys(void* node_ptr, char* keys, int max_keys, int max_key_len, int* actual_count) {
    if (!node_ptr || !keys || !actual_count) return false;

    try {
        YamlNodeWrapper* wrapper = static_cast<YamlNodeWrapper*>(node_ptr);
        if (!wrapper->node.IsMap()) {
            *actual_count = 0;
            return false;
        }

        *actual_count = 0;
        for (const auto& pair : wrapper->node) {
            if (*actual_count >= max_keys) break;
            
            std::string key_str = pair.first.as<std::string>();
            if (key_str.length() < max_key_len) {
                // Copy key to the output array
                char* dest = keys + (*actual_count) * max_key_len;
                strncpy(dest, key_str.c_str(), max_key_len - 1);
                dest[max_key_len - 1] = '\0';  // Ensure null termination
                (*actual_count)++;
            }
        }
        return true;
    } catch (const std::exception& e) {
        std::cerr << "Error getting all keys: " << e.what() << std::endl;
        *actual_count = 0;
    }
    return false;
}

} // extern "C"
