/********************************************************************************/
/**
 * @file cobj.hpp
 *
 * @version   0.1
 * @date      2022-
 * @author    PikachuHy
 * @copyright MIT License
 *************************************************************************************/

#ifndef PICOSCHEME_COBJ_H
#define PICOSCHEME_COBJ_H

namespace pscm {
class CObj {
    void *ptr;
    int m_tag;

public:
    CObj(void *ptr, int tag)
        : ptr(ptr),
          m_tag(tag) {
    }

    CObj(const CObj&) = default;
    CObj& operator=(const CObj&) = default;
    CObj& operator=(CObj&&) = default;

    [[nodiscard]] int tag() const {
        return m_tag;
    }

    [[nodiscard]] void *value() const {
        return ptr;
    }

    bool operator==(const CObj& rhs) const {
        return ptr == rhs.ptr;
    }

    bool operator!=(const CObj& rhs) const {
        return !(rhs == *this);
    }

    struct hash {
        using argument_type = pscm::CObj;
        using result_type = std::size_t;

        result_type operator()(const CObj& cobj) const {
            return ptrdiff_t(cobj.ptr);
        }
    };

    template <typename OStream>
    friend OStream& operator<<(OStream& os, const CObj& cobj) {
        return os << "cobj: " << cobj.ptr;
    }
};
} // namespace pscm
#endif // PICOSCHEME_COBJ_H
