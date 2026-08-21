#pragma once

#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

namespace catchem {

    enum ErrorCode { SUCCESS = 0, FAILURE = -1, INVALID_INPUT = 1001, INVALID_STATE = 1003, MEMORY_ALLOCATION = 1007 };

    class ErrorManager {
    private:
        std::vector<std::string> context_stack;

    public:
        void push_context(const std::string& ctx) { context_stack.push_back(ctx); }
        void pop_context() {
            if (!context_stack.empty()) {
                context_stack.pop_back();
            }
        }
        void report_error(ErrorCode code, const std::string& msg) {
            std::cerr << "[CATChem C++ Error " << code << "] " << msg << " | Context: ";
            for (const auto& ctx : context_stack) {
                std::cerr << ctx << " -> ";
            }
            std::cerr << "End\n";
        }
    };

    inline void require_field_pointer(const char* process_name, const char* field_name, const void* ptr) {
        if (ptr == nullptr) {
            throw std::runtime_error(std::string("FATAL ERROR: ") + process_name + " process missing required field " +
                                     field_name);
        }
    }

} // namespace catchem
