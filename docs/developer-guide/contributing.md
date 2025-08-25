# Contributing Guide to CATChem

This guide will help you get started with learning the basics on how to contribute to the CATChem library and modeling component. Thank you for your interest in contributing!

## Overview

CATChem is a community-driven project that welcomes contributions from:

- **Atmospheric scientists** contributing process implementations
- **Software developers** improving infrastructure and tooling
- **Computational scientists** optimizing performance
- **Documentation writers** improving user and developer guides
- **Students and researchers** reporting issues and testing features

## Ways to Contribute

### Code Contributions

- **New atmospheric processes** (chemistry, emissions, transport, losses)
- **Performance optimizations** and algorithm improvements
- **Bug fixes** and error handling improvements
- **Testing infrastructure** and validation tools
- **Build system** and deployment improvements

### Documentation Contributions

- **User guides** and tutorials
- **Developer documentation** and API references
- **Process documentation** and scientific validation
- **Installation guides** for different systems
- **Examples** and case studies

### Community Contributions

- **Issue reporting** and bug reproduction
- **Feature requests** and use case documentation
- **Code reviews** and testing
- **Community support** and discussions
- **Presentations** and outreach

## Getting Started

### 1. Development Environment Setup

**Prerequisites:**

- Modern Fortran compiler (Intel 2019+, GNU 9+)
- MPI implementation (OpenMPI, Intel MPI)
- NetCDF-Fortran and HDF5 libraries
- CMake 3.15+
- Git

**Environment Setup:**
```bash
# Create a fork of the CATChem repository: https://github.com/UFS-Community/CATChem.git
# Clone the develop branch from your fork
git clone https://github.com/Your-Fork/CATChem.git
cd CATChem

# Create development branch
git checkout -b feature/your-feature-name

# Set up build environment
module load intel/2021.3.0 impi/2021.3.0 netcdf/4.7.4
export FC=mpiifort CC=mpiicc CXX=mpiicpc

# Build in debug mode
mkdir build-debug && cd build-debug
cmake .. -DCMAKE_BUILD_TYPE=Debug -DTESTING=ON
make -j 8
```

### 2. Code Style and Standards

Review our [**Coding Standards Documentation**](coding-standards.md) for more details.

**Fortran Coding Standards:**
```fortran
! Module naming: PascalCase with _Mod suffix
module ProcessInterface_Mod
  use precision_mod, only: wp
  implicit none
  private

  ! Type naming: PascalCase
  type, public :: ProcessInterface
    private
    character(len=:), allocatable :: name
    logical :: initialized = .false.
  contains
    ! Procedure naming: snake_case
    procedure :: init => process_init
    procedure :: run => process_run
    procedure :: finalize => process_finalize
  end type

contains

  ! Subroutine/function naming: snake_case
  subroutine process_init(this, config, error_msg)
    class(ProcessInterface), intent(inout) :: this
    type(config_type), intent(in) :: config
    character(len=*), intent(out) :: error_msg

    ! Variable naming: snake_case
    integer :: status_code
    logical :: is_valid

    ! Constants: UPPER_CASE
    real(wp), parameter :: DEFAULT_TOLERANCE = 1.0e-6_wp

  end subroutine process_init

end module ProcessInterface_Mod
```

**Documentation Standards:**
```fortran
!> @brief Process interface for atmospheric chemistry processes
!>
!> This module provides the base interface that all atmospheric processes
!> must implement. It defines the lifecycle methods (init, run, finalize)
!> and provides common functionality.
!>
!> @author Your Name
!> @date 2024-01-15
!> @version 2.1.0
module ProcessInterface_Mod

  !> @brief Base type for all atmospheric processes
  !>
  !> All processes must extend this type and implement the required
  !> procedures. The type provides:
  !> - Lifecycle management (initialization, execution, cleanup)
  !> - Configuration handling
  !> - Error reporting
  !> - Diagnostic output
  type, public :: ProcessInterface

    !> Process name for identification and logging
    character(len=:), allocatable :: name

  contains

    !> @brief Initialize the process
    !> @param[in] config Configuration object
    !> @param[out] error_msg Error message if initialization fails
    procedure :: init => process_init

  end type

end module ProcessInterface_Mod
```

Learn more about contributing to CATChem documentation in the [**Developer Documentation Guide**](documentation.md)

### 3. Testing Requirements

**Unit Tests:**
```fortran
! All new code must include unit tests
program test_my_process
  use MyProcess_Mod
  use testing_mod
  implicit none

  call test_init()
  call test_run()
  call test_edge_cases()
  call test_error_handling()

contains

  subroutine test_init()
    type(MyProcess) :: process
    character(len=256) :: error_msg

    call process%init(default_config, error_msg)
    call assert_true(process%initialized, "Process should be initialized")
    call assert_equals("", trim(error_msg), "No error message expected")
  end subroutine

end program
```

**Integration Tests:**
```bash
# Tests must pass on multiple systems
cd tests
./run_integration_tests.sh
```

Learn more about testing requirements in the [**Developer Testing Guide**](testing.md)

## Development Workflow

### 1. Issue-Driven Development

**Before Starting:**

1. **Check existing issues** - Look for related work
2. **Create or comment on issue** - Describe your planned contribution
3. **Get feedback** - Discuss approach with maintainers
4. **Plan implementation** - Break down into manageable pieces

### 2. Branch Strategy

**Branch Naming:**

- `feature/descriptive-name` - New features
- `bugfix/issue-number-description` - Bug fixes
- `docs/topic-name` - Documentation updates
- `refactor/component-name` - Code refactoring

**Example:**
```bash
# Feature branch
git checkout -b feature/multiphase-chemistry

# Bug fix branch
git checkout -b bugfix/issue-123-memory-leak

# Documentation branch
git checkout -b docs/process-generator-tutorial
```

### 3. Commit Standards

**Commit Message Format:**
```
<type>(<scope>): <subject>

<body>

<footer>
```

**Types:**

- `feat` - New feature
- `fix` - Bug fix
- `docs` - Documentation changes
- `style` - Code style changes (no logic changes)
- `refactor` - Code refactoring
- `test` - Adding or updating tests
- `chore` - Build process or auxiliary tool changes

**Examples:**
```bash
git commit -m "feat(chemistry): add multiphase chemistry process

- Implement gas-liquid partitioning
- Add Henry's law constants database
- Include mass conservation checks
- Add comprehensive unit tests

Closes #234"

git commit -m "fix(emissions): resolve memory leak in emission reader

The emission data reader was not properly deallocating temporary
arrays, causing memory usage to grow during long runs.

Fixes #456"

git commit -m "docs(processes): add process generator tutorial

Add comprehensive tutorial covering:
- Basic usage examples
- Advanced configuration options
- Best practices and troubleshooting

Addresses #789"
```

### 4. Pull Request Process

**Before Submitting:**

1. **Rebase on main** - Ensure clean history
2. **Run all tests** - Unit, integration, and performance tests
3. **Update documentation** - Include relevant docs updates
4. **Self-review** - Check your own changes carefully

**Pull Request Template:**
```markdown
## Description
Brief description of changes and motivation.

## Type of Change
- [ ] Bug fix (non-breaking change that fixes an issue)
- [ ] New feature (non-breaking change that adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to not work as expected)
- [ ] Documentation update

## Testing
- [ ] Unit tests pass
- [ ] Integration tests pass
- [ ] Performance tests pass (if applicable)
- [ ] Manual testing completed

## Checklist
- [ ] Code follows project style guidelines
- [ ] Self-review completed
- [ ] Code is commented and documented
- [ ] Tests added/updated for changes
- [ ] Documentation updated if needed

## Related Issues
Closes #123
Addresses #456
```

**Review Process:**

1. **Automated checks** - CI/CD pipeline runs automatically
2. **Code review** - At least one maintainer review required
3. **Testing** - Reviewers may run additional tests
4. **Approval** - Changes must be approved before merging

## Contribution Guidelines

### Process Development

**New Process Checklist:**

- [x] Use the [**process generator**](processes/generator-tutorial.md)
- [x] Extend `ProcessInterface` base class
- [x] Implement all required lifecycle methods
- [x] Add configuration validation
- [x] Include comprehensive error handling
- [x] Write unit and integration tests
- [x] Document the process thoroughly
- [x] Add examples and usage guides

**Process Documentation:**
```markdown
# My New Process

## Overview
Scientific description of the process and its importance.

## Implementation
Technical details of the implementation approach.

## Configuration
```yaml
processes:
  - name: my_process
    scheme: best_scheme
    parameters:
      param1: value1
```

## Validation
Comparison with observations or other models.

## References
Scientific literature and validation sources.

### Performance Contributions

**Performance Guidelines:**

- **Profile before optimizing** - Use built-in profiling tools
- **Benchmark changes** - Document performance improvements
- **Consider all architectures** - Test on Intel, AMD, ARM systems
- **Maintain accuracy** - Don't sacrifice scientific accuracy for speed

**Performance Testing:**
```bash
# Benchmark your changes
cd tests/performance
./benchmark_before_after.sh feature-branch main
```

### Documentation Contributions

**Documentation Standards:**

- **Clear and concise** - Write for your target audience
- **Complete examples** - Include working code/config examples
- **Cross-references** - Link to related documentation
- **Version-aware** - Update version-specific information

**Documentation Structure:**
```markdown
# Title

## Overview
What this document covers and who should read it.

## Prerequisites
What users need to know before starting.

## Step-by-Step Guide
Detailed instructions with examples.

## Troubleshooting
Common issues and solutions.

## References
Related documentation and external resources.
```

## Review Process {#review_process}

### Code Review Guidelines

**For Authors:**

- **Small, focused PRs** - Easier to review and understand
- **Clear descriptions** - Explain what and why, not just how
- **Respond promptly** - Address reviewer comments quickly
- **Test thoroughly** - Don't rely solely on CI/CD

**For Reviewers:**

- **Be constructive** - Provide helpful suggestions
- **Focus on important issues** - Don't nitpick style if tools handle it
- **Test if needed** - Run code locally for complex changes
- **Approve promptly** - Don't delay good contributions

### Review Criteria

**Pre-Review Checklist**

- [x] Code compiles without warnings
- [x] All tests pass
- [x] Documentation updated
- [x] Style checks pass
- [x] Performance regression tests

**Code Quality:**

- [x] Follows coding standards and conventions
- [x] Includes appropriate error handling
- [x] Has adequate test coverage
- [x] Is well-documented and commented

**Scientific Accuracy:**

- [x] Implements processes correctly
- [x] Includes proper validation
- [x] Maintains physical consistency
- [x] References appropriate literature

**Performance:**

- [x] Doesn't introduce significant performance regressions
- [x] Uses efficient algorithms and data structures
- [x] Considers memory usage and scalability

**Integration:**

- [x] Works with existing codebase
- [x] Doesn't break existing functionality
- [x] Follows established patterns and interfaces

## Release Process

### Version Numbering

CATChem follows semantic versioning (MAJOR.MINOR.PATCH):

- **MAJOR** - Incompatible API changes
- **MINOR** - New functionality (backward compatible)
- **PATCH** - Bug fixes (backward compatible)

### Release Schedule

- **Major releases** - Annual (with deprecation warnings)
- **Minor releases** - Quarterly (new features)
- **Patch releases** - As needed (critical bug fixes)

### Contributing to Releases

**Feature Freeze:**

- No new features after feature freeze date
- Bug fixes and documentation updates only
- Testing and validation focus

**Release Candidates:**

- Community testing period
- Issue reporting and fixing
- Documentation finalization

## Community Guidelines

### Code of Conduct

CATChem follows the [**Contributor Covenant Code of Conduct**](https://www.contributor-covenant.org/). All community members are expected to uphold these standards.

### Communication Channels

- [**GitHub Issues**](https://github.com/UFS-Community/CATChem/issues) - Bug reports and feature requests
- [**GitHub Discussions**](https://github.com/UFS-Community/CATChem/discussions) - General questions and community discussion
- **Developer Meetings**: Bi-weekly virtual meetings (contact team for details)
- **Community Meetings**: Bi-monthly virtual meetings (contact team for details)

### Getting Help

**For Contributors:**

- Check existing documentation and issues first
- Use [**GitHub Discussions**](https://github.com/UFS-Community/CATChem/discussions) for general questions
- Join developer meetings for complex discussions
- Contact maintainers directly for urgent issues

**For Users:**

- Start with user documentation and tutorials
- Search existing issues for known problems
- Create new issues with detailed problem descriptions
- Participate in community discussions

## Recognition

### Contributor Recognition

- **Code contributors** - Listed in AUTHORS file and release notes
- **Documentation contributors** - Acknowledged in documentation
- **Issue reporters** - Thanked in issue resolutions
- **Community supporters** - Recognized in community channels

### Maintainer Path

Active contributors may be invited to become maintainers:

- **Regular contributions** - Consistent, high-quality contributions
- **Community involvement** - Helping other contributors and users
- **Technical expertise** - Deep understanding of CATChem architecture
- **Time commitment** - Ability to review PRs and guide development

## Resources

### Development Resources

- [**Developer Guide**](index.md)
- [**Build System Documentation**](core/build-system.md)
- [**Testing Guide**](testing.md)
- [**Performance Guide**](performance.md)
- [**Process Generator Tutorial**](processes/generator-tutorial.md)

### Community Resources

- [**CATChem GitHub Repository**](https://github.com/NOAA-GSL/CATChem)
- [**Issue Tracker**](https://github.com/NOAA-GSL/CATChem/issues)
- [**Discussions Forum**](https://github.com/NOAA-GSL/CATChem/discussions)
- [**Developer Documentation**](https://catchem.readthedocs.io/en/latest/developer-guide/)

### Scientific Resources

- [**Process Documentation**](../processes/index.md)
- [**Evaluation Studies**](../evaluation/index.md)
- [**Scientific References**](../references.md)

---

Thank you for contributing to CATChem! Your contributions help advance atmospheric chemistry modeling for the entire community.
