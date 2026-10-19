# CATChem C++ Doxygen Style Contract

## 1. Purpose

Doxygen comments in the CATChem C++ sources (`src/core`, `src/process`) are
the source of truth for the generated API reference. They must let a host-model
integrator (UFS/WRF/MPAS driver author, CCPP-SCM user) understand every public
symbol — its purpose, parameters, units, ownership, thread-safety, and failure
modes — without reading the implementation.

## 2. Block Markers

- Every file starts with a `@file` block: one-sentence `@brief` plus a
  `@details` paragraph for non-obvious modules (thread-safety model,
  ownership rules, relation to the Fortran/CCPP boundary).
- Multi-line docs use `/** ... */`; one-liners use `///`. Trailing member or
  enumerator docs use `//!<`.
- Each public symbol is documented **exactly once**, at its declaration in
  the header. A `.cpp` definition of a header-declared symbol gets no doc
  block (Doxygen inherits it); use `@copydoc` only when a definition must
  carry the doc and the header cannot hold it.
- Symbols that exist only inside a `.cpp` (static functions, anonymous
  namespaces) are documented in place with the same tag discipline.
- Preserve every existing explanatory comment: convert or augment it into
  Doxygen blocks; never delete substantive prose.

## 3. Required Tags

| Element                    | Required tags                                                          |
|----------------------------|------------------------------------------------------------------------|
| File                       | `@file`, `@brief` (+ `@details` when the module has real subtlety)     |
| Public class / struct      | `@brief` (+ `@details` for lifetime, ownership, thread-safety)         |
| Public function / method   | `@brief`, `@param` for **every** parameter, `@return` if non-void      |
| Failure/ownership nuance   | `@note` on the function                                                |
| Cross-layer pointer        | `@see` (e.g. header ↔ Fortran bridge ↔ CCPP context API)               |
| Private / internal helper  | one-line `@brief`                                                      |
| Enumerator / public member | trailing `//!<` brief                                                  |
| Template                   | `@tparam` per parameter; concept explained in the type's `@details`    |

## 4. Quality Bar (hard requirements)

- **No vacuous filler.** `@param x x` and `@param nc the nc` are prohibited.
  Every description must add information the name does not carry.
- **Physical parameters state units** (kg m-3, m s-1, s, K, …) and valid
  range where the code enforces one.
- **Pointer and handle parameters state ownership and lifetime**
  ("caller retains", "must outlive the call", "returned handle must be
  released with `catchem_core_destroy`").
- **Error-returning functions name the enum constants** they can return and
  the condition for each.
- **Never invent behavior.** Document what the code does, citing the
  implementation where a claim is non-obvious. If intent is unclear,
  describe observed behavior and add a `@note`, not an aspiration.
- Kokkos view parameters state their assumed layout (LayoutRight /
  LayoutLeft / default) and whether the view is copied or referenced.

## 5. Verification

The build gates run from the repository root:

```bash
# Undocumented-member and missing-param gate (Doxyfile sets
# WARN_IF_UNDOCUMENTED / WARN_NO_PARAMDOC = YES):
cd docs && doxygen Doxyfile 2>&1 | grep 'warning:' | grep -E 'src/(core|process)'
# expected: no output

# Vacuous-comment audit:
grep -rnE '@param +([A-Za-z_][A-Za-z0-9_]*) +(\1|the \1)[.!]? *$' \
  src/core src/process --include='*.hpp' --include='*.cpp'
# expected: no output
```

Comment-only edits are confirmed with:

```bash
git diff -U0 <paths> | grep '^+' | grep -vE '^\+\s*(/\*\*|\*|///|//|$)'
# expected: no output
```

## 6. Reference Example

`src/core/catchem_logger.hpp` is the canonical worked example: file block,
class block, documented enum with `//!<` values, a fully tagged public method,
`@copydoc` aliases, and one-line private helper briefs. New code should look
like it.
