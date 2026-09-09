#pragma once
#include "catchem_state_manager.hpp"
#include <initializer_list>
#include <string_view>
#include <utility>

namespace catchem {

    class Logger {
    public:
        using ContextList = std::initializer_list<std::pair<std::string_view, std::string_view>>;

        // Minimum severity actually emitted.  Controlled by the
        // CATCHEM_LOG_LEVEL environment variable (DEBUG | INFO | WARN | ERROR,
        // case-insensitive); anything unrecognized or unset selects INFO so
        // production and operational runs stay quiet by default.
        enum class Level { Debug = 0, Info = 1, Warn = 2, Error = 3 };

        static void debug(const StateManager* state, std::string_view message, ContextList context = {});
        static void info(const StateManager* state, std::string_view message, ContextList context = {});
        static void warn(const StateManager* state, std::string_view message, ContextList context = {});
        static void error(const StateManager* state, std::string_view message, ContextList context = {});

        // Returns true when a message of the given level would be emitted.
        // Call sites with expensive context preparation should check this
        // first and skip the whole block when it returns false.
        static bool enabled(Level level);

    private:
        static void log(const StateManager* state, std::string_view level, Level level_id, std::string_view message,
                        ContextList context);
        static bool should_color(int fd);
        static Level threshold();
    };

} // namespace catchem
