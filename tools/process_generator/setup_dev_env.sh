#!/bin/bash
# CATChem Process Generator Development Environment Setup
# Usage: ./setup_dev_env.sh [env_name] [python_version] [minimal|full]
#   env_name: Name for the virtual environment (default: catchem-process-gen)
#   python_version: Python version to use (default: 3.10)
#   install_type: 'minimal' for basic deps, 'full' for dev environment (default: full)

set -e

ENV_NAME="${1:-catchem-process-gen}"
PYTHON_VERSION="${2:-3.10}"
INSTALL_TYPE="${3:-full}"

echo "🚀 Setting up CATChem Process Generator development environment..."
echo "   Environment name: $ENV_NAME"
echo "   Python version: $PYTHON_VERSION"
echo "   Install type: $INSTALL_TYPE"

# Check if python3 is available (prioritize venv over conda)
if command -v python3 &> /dev/null; then
    echo "🐍 Using venv for environment management"

    # Create virtual environment
    echo "Creating virtual environment: $ENV_NAME"
    python3 -m venv "$ENV_NAME"

    # Activate environment
    echo "Activating environment..."
    source "$ENV_NAME/bin/activate"

    # Upgrade pip
    echo "Upgrading pip..."
    pip install --upgrade pip setuptools wheel

    # Install dependencies
    echo "Installing dependencies..."
    if [[ "${3:-full}" == "minimal" ]]; then
        echo "  → Installing minimal dependencies from requirements.txt"
        pip install -r requirements.txt
    else
        echo "  → Installing full development environment"
        pip install -e ".[dev,test,docs,validation]"
    fi

    echo "✅ Virtual environment '$ENV_NAME' created and configured!"
    echo "   To activate: source $ENV_NAME/bin/activate"

elif command -v conda &> /dev/null; then
    echo "📦 Using conda for environment management"

    # Create conda environment
    echo "Creating conda environment: $ENV_NAME"
    conda create -n "$ENV_NAME" python="$PYTHON_VERSION" -y

    # Activate environment
    echo "Activating environment..."
    source "$(conda info --base)/etc/profile.d/conda.sh"
    conda activate "$ENV_NAME"

    # Install dependencies
    echo "Installing dependencies..."
    if [[ "${3:-full}" == "minimal" ]]; then
        echo "  → Installing minimal dependencies from requirements.txt"
        pip install -r requirements.txt
    else
        echo "  → Installing full development environment"
        pip install -e ".[dev,test,docs,validation]"
    fi

    echo "✅ Conda environment '$ENV_NAME' created and configured!"
    echo "   To activate: conda activate $ENV_NAME"

else
    echo "❌ Error: Neither python3 nor conda found!"
    echo "   Please install Python 3.8+ or Anaconda/Miniconda"
    exit 1
fi

echo ""
echo "🔧 Development environment ready!"
echo ""
echo "Next steps:"
echo "  1. Activate the environment:"
if command -v python3 &> /dev/null; then
    echo "     source $ENV_NAME/bin/activate"
elif command -v conda &> /dev/null; then
    echo "     conda activate $ENV_NAME"
fi
echo "  2. Test the installation:"
echo "     python process_generator.py --help"
echo "  3. Run tests:"
echo "     pytest"
echo "  4. Generate a process:"
echo "     python process_generator.py configs/example_process.yaml"
echo ""
echo "📚 Documentation:"
echo "   - Process Generator Guide: ../docs/PROCESS_GENERATOR_GUIDE.md"
echo "   - Template Documentation: templates/README.md"
echo "   - API Reference: docs/api/"
