/*********************************************************************************/
/**
 * @file module.cpp
 *
 * @version   0.1
 * @date      2022-
 * @author    PikachuHy
 * @copyright MIT License
 *************************************************************************************/
#include "picoscm/module.h"
#include "picoscm/cell.hpp"
#include "picoscm/symbol.hpp"

namespace pscm {
struct ModulePrivate {
    ModulePrivate(Cell name, SymenvPtr env)
        : name(name)
        , env(env) {
    }

    bool operator==(const ModulePrivate& rhs) const {
        return name == rhs.name;
    }

    bool operator!=(const ModulePrivate& rhs) const {
        return !(rhs == *this);
    }

    Cell name;
    SymenvPtr env;
};

Module::Module(Cell name, SymenvPtr env)
    : m(std::make_shared<ModulePrivate>(name, env)) {
}

const Cell& Module::name() const noexcept {
    return m->name;
}

const SymenvPtr& Module::env() const noexcept {
    return m->env;
}

SymenvPtr& Module::env() noexcept {
    return m->env;
}

bool Module::operator==(const Module& rhs) const {
    return m == rhs.m;
}

bool Module::operator!=(const Module& rhs) const {
    return !(rhs == *this);
}

std::size_t Module::hash::operator()(const Module& module) const noexcept {
    return std::hash<ModulePrivate *>::operator()(module.m.get());
}
} // namespace pscm