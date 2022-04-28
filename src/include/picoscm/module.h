/*********************************************************************************/
/**
 * @file module.cpp
 *
 * @version   0.1
 * @date      2022-
 * @author    PikachuHy
 * @copyright MIT License
 *************************************************************************************/

#ifndef PICOSCHEME_MODULE_H
#define PICOSCHEME_MODULE_H
#include "types.hpp"
#include <memory>

namespace pscm {
struct ModulePrivate;

class Module {
public:
    Module() {
    }

    Module(Cell name, SymenvPtr env);
    bool operator==(const Module& rhs) const;
    bool operator!=(const Module& rhs) const;
    const Cell& name() const noexcept;
    const SymenvPtr& env() const noexcept;
    SymenvPtr& env() noexcept;

    struct hash : private std::hash<ModulePrivate *> {
        using argument_type = Module;
        using result_type = std::size_t;

        result_type operator()(const Module& module) const noexcept;
    };

private:
    std::shared_ptr<ModulePrivate> m;
};
} // namespace pscm
#endif // PICOSCHEME_MODULE_H
