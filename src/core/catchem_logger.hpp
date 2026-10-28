/**
 * @file catchem_logger.hpp
 * @brief Severity-filtered, optionally site-annotated logging for the CATChem core.
 *
 * @details Declares the Logger facade used throughout the C++ core and process
 * layer. Emission is controlled by a runtime threshold: an explicit
 * Logger::set_level() override (the YAML configuration path,
 * simulation/verbose/log_level) always wins; otherwise the CATCHEM_LOG_LEVEL
 * environment variable (DEBUG | INFO | WARN | ERROR, case-insensitive) selects
 * the level, and an unset or unrecognized value selects Info so production
 * runs stay quiet. Records carry a UTC timestamp, a fixed-width service tag,
 * and an 8-character trace ID taken from the state when available.
 */
#pragma once
#include "catchem_state_manager.hpp"
#include <initializer_list>
#include <optional>
#include <string_view>
#include <utility>

namespace catchem {

    /**
     * @brief Static logging facade for the CATChem core.
     *
     * @details All members are static; the class is a namespace, not an
     * instance. The emission threshold is process-wide state, so
     * set_level()/clear_level() affect every caller. Records are written
     * synchronously to the standard streams (ERROR to std::cerr, the rest to
     * std::clog) with ANSI color when the destination is a TTY and NO_COLOR
     * is unset.
     */
    class Logger {
    public:
        /// Key/value pairs attached to one log record, rendered as `key=value`.
        using ContextList = std::initializer_list<std::pair<std::string_view, std::string_view>>;

        /**
         * @brief Severity levels, ordered from most to least verbose.
         *
         * @details A record is emitted when its level is numerically greater
         * than or equal to the current threshold. Names are accepted
         * case-insensitively by level_from_string(); "warning" is an alias
         * for "warn".
         */
        enum class Level {
            Debug = 0, ///< Detailed diagnostic output; below the default threshold.
            Info = 1,  ///< Normal progress messages; the default threshold.
            Warn = 2,  ///< Recoverable anomalies worth operator attention.
            Error = 3, ///< Failures that abort or corrupt an operation.
        };

        /**
         * @brief Emit a record at Debug level.
         *
         * @param state   Simulation state supplying the trace ID for the
         *                record prefix, or nullptr for the "global" trace tag.
         *                Not retained after the call.
         * @param message Record text.
         * @param context Optional key/value pairs appended after the message.
         */
        static void debug(const StateManager* state, std::string_view message, ContextList context = {});
        /// @copydoc debug
        static void info(const StateManager* state, std::string_view message, ContextList context = {});
        /// @copydoc debug
        static void warn(const StateManager* state, std::string_view message, ContextList context = {});
        /// @copydoc debug
        static void error(const StateManager* state, std::string_view message, ContextList context = {});

        /**
         * @brief Report whether a message at the given level would be emitted.
         *
         * @param level Severity to test against the current threshold.
         * @return true when level is at or above the threshold.
         *
         * @note Call sites with expensive context preparation should check
         * this first and skip the whole block when it returns false.
         */
        static bool enabled(Level level);

        /**
         * @brief Parse a level name case-insensitively.
         *
         * @param name Level text; "warning" is accepted as an alias for "warn".
         * @return The matching Level, or std::nullopt for unrecognized names.
         */
        static std::optional<Level> level_from_string(std::string_view name);

        /**
         * @brief Force the emission threshold, overriding CATCHEM_LOG_LEVEL.
         * @param level Minimum severity that will be emitted from now on.
         */
        static void set_level(Level level);

        /// Drop the explicit override so CATCHEM_LOG_LEVEL applies again.
        static void clear_level();

    private:
        /// Shared implementation for debug()/info()/warn()/error().
        static void log(const StateManager* state, std::string_view level, Level level_id, std::string_view message,
                        ContextList context);
        /// True when the stream on the given file descriptor should be ANSI-colored.
        static bool should_color(int fd);
        /// The active threshold: explicit override if set, else cached CATCHEM_LOG_LEVEL.
        static Level threshold();
        /// Reference to the process-wide override slot (-1 means "no override").
        static int& override_level();
    };

} // namespace catchem
