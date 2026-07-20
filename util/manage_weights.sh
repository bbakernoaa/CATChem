#!/bin/bash
# Weight File Management Script for CATChem NUOPC
# This script demonstrates how to manage ESMF weight files for optimal performance

set -e

# Configuration
WEIGHTS_DIR="./weights"
INPUT_CONFIG="catchem_input_config.yml"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Print colored output
print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Help function
show_help() {
    cat << EOF
CATChem NUOPC Weight File Management Script

USAGE:
    $0 [COMMAND] [OPTIONS]

COMMANDS:
    setup       Create weights directory and initial configuration
    check       Check status of weight files
    clean       Remove all weight files
    validate    Validate weight file configuration
    generate    Generate weight files (requires model run)
    help        Show this help message

OPTIONS:
    -d DIR      Specify weights directory (default: ./weights)
    -c CONFIG   Specify input config file (default: catchem_input_config.yml)
    -v          Verbose output

EXAMPLES:
    $0 setup                    # Create weights directory
    $0 check                    # Check weight file status
    $0 clean                    # Remove all weight files
    $0 validate -v              # Validate configuration with verbose output
    $0 generate                 # Instructions for generating weight files

EOF
}

# Setup weights directory
setup_weights() {
    print_info "Setting up weights directory: $WEIGHTS_DIR"

    if [ ! -d "$WEIGHTS_DIR" ]; then
        mkdir -p "$WEIGHTS_DIR"
        print_success "Created weights directory: $WEIGHTS_DIR"
    else
        print_warning "Weights directory already exists: $WEIGHTS_DIR"
    fi

    # Create a README file in the weights directory
    cat > "$WEIGHTS_DIR/README.md" << 'EOF'
# ESMF Weight Files Directory

This directory contains pre-computed ESMF regridding weight files for CATChem NUOPC.

## File Naming Convention

Weight files should follow the template pattern specified in `catchem_input_config.yml`.

Common patterns:
- `{DATASET}_{METHOD}_weights.nc` - Basic pattern
- `{DATASET}_{METHOD}_{POLE_METHOD}_weights.nc` - With pole method
- `{DATASET}_{FIELD}_{METHOD}_weights.nc` - Field-specific weights

## File Management

- Weight files are automatically generated when `save_weights: true` is set
- Pre-computed weight files can be placed here manually
- Files are loaded automatically if they exist and match the template

## Performance

Using pre-computed weight files can significantly reduce model startup time,
especially for high-resolution grids and complex regridding operations.
EOF

    print_success "Created weights directory README"
}

# Check weight file status
check_weights() {
    print_info "Checking weight file status..."

    if [ ! -d "$WEIGHTS_DIR" ]; then
        print_error "Weights directory does not exist: $WEIGHTS_DIR"
        print_info "Run '$0 setup' to create the directory"
        return 1
    fi

    weight_count=$(find "$WEIGHTS_DIR" -name "*.nc" -type f | wc -l)

    if [ "$weight_count" -eq 0 ]; then
        print_warning "No weight files found in $WEIGHTS_DIR"
        print_info "Weight files will be computed at runtime"
    else
        print_success "Found $weight_count weight file(s) in $WEIGHTS_DIR"

        if [ "$VERBOSE" = "true" ]; then
            print_info "Weight files:"
            find "$WEIGHTS_DIR" -name "*.nc" -type f -exec basename {} \; | sort
        fi
    fi

    # Check configuration file
    if [ -f "$INPUT_CONFIG" ]; then
        print_success "Input configuration file found: $INPUT_CONFIG"

        # Check for weight file settings
        if grep -q "save_weights.*true" "$INPUT_CONFIG"; then
            print_success "Automatic weight saving is enabled"
        else
            print_warning "Automatic weight saving is not enabled"
        fi

        if grep -q "weight_file" "$INPUT_CONFIG"; then
            print_success "Weight file configuration found"
        else
            print_warning "No weight file configuration found"
        fi
    else
        print_error "Input configuration file not found: $INPUT_CONFIG"
    fi
}

# Clean weight files
clean_weights() {
    print_info "Cleaning weight files from $WEIGHTS_DIR"

    if [ ! -d "$WEIGHTS_DIR" ]; then
        print_warning "Weights directory does not exist: $WEIGHTS_DIR"
        return 0
    fi

    weight_count=$(find "$WEIGHTS_DIR" -name "*.nc" -type f | wc -l)

    if [ "$weight_count" -eq 0 ]; then
        print_info "No weight files to clean"
        return 0
    fi

    print_warning "This will remove $weight_count weight file(s)"
    read -p "Continue? (y/N): " -n 1 -r
    echo

    if [[ $REPLY =~ ^[Yy]$ ]]; then
        find "$WEIGHTS_DIR" -name "*.nc" -type f -delete
        print_success "Removed all weight files"
    else
        print_info "Cancelled"
    fi
}

# Validate configuration
validate_config() {
    print_info "Validating weight file configuration..."

    if [ ! -f "$INPUT_CONFIG" ]; then
        print_error "Configuration file not found: $INPUT_CONFIG"
        return 1
    fi

    # Check YAML syntax (basic check)
    if command -v python3 &> /dev/null; then
        if ! python3 -c "import yaml; yaml.safe_load(open('$INPUT_CONFIG'))" 2>/dev/null; then
            print_error "Invalid YAML syntax in $INPUT_CONFIG"
            return 1
        fi
        print_success "YAML syntax is valid"
    else
        print_warning "Python3 not available, skipping YAML syntax check"
    fi

    # Check for required sections
    if grep -q "datasets:" "$INPUT_CONFIG"; then
        print_success "Datasets section found"
    else
        print_error "No datasets section found in configuration"
        return 1
    fi

    # Check weight file configuration
    if grep -q "weight_file" "$INPUT_CONFIG"; then
        print_success "Weight file configuration found"

        if [ "$VERBOSE" = "true" ]; then
            print_info "Weight file settings:"
            grep -n "weight_file" "$INPUT_CONFIG" || true
        fi
    else
        print_warning "No weight file configuration found"
        print_info "Add weight_file or weight_file_template to dataset regrid_options"
    fi

    print_success "Configuration validation complete"
}

# Generate weight files
generate_weights() {
    print_info "Weight file generation requires running the model"

    cat << EOF

To generate weight files automatically:

1. Ensure your configuration has save_weights: true
   Example:
   regrid_options:
     save_weights: true
     weight_file_template: "./weights/{DATASET}_{METHOD}_weights.nc"

2. Run the model for a short period
   The weight files will be generated during initialization

3. For subsequent runs, the weight files will be loaded automatically

Alternative: Use ESMF utilities to pre-generate weights
- ESMF_RegridWeightGen utility
- Custom scripts using ESMF libraries

EOF

    if [ -f "$INPUT_CONFIG" ] && grep -q "save_weights.*true" "$INPUT_CONFIG"; then
        print_success "Your configuration is set up for automatic weight generation"
    else
        print_warning "Your configuration needs save_weights: true for automatic generation"
    fi
}

# Parse command line arguments
VERBOSE=false
while [[ $# -gt 0 ]]; do
    case $1 in
        -d|--dir)
            WEIGHTS_DIR="$2"
            shift 2
            ;;
        -c|--config)
            INPUT_CONFIG="$2"
            shift 2
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -h|--help)
            show_help
            exit 0
            ;;
        setup|check|clean|validate|generate|help)
            COMMAND="$1"
            shift
            ;;
        *)
            print_error "Unknown option: $1"
            show_help
            exit 1
            ;;
    esac
done

# Default command
if [ -z "${COMMAND:-}" ]; then
    COMMAND="help"
fi

# Execute command
case $COMMAND in
    setup)
        setup_weights
        ;;
    check)
        check_weights
        ;;
    clean)
        clean_weights
        ;;
    validate)
        validate_config
        ;;
    generate)
        generate_weights
        ;;
    help)
        show_help
        ;;
    *)
        print_error "Unknown command: $COMMAND"
        show_help
        exit 1
        ;;
esac
