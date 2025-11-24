# User Guide

Welcome to the CATChem User Guide. This comprehensive guide covers everything you need to know to effectively use the CATChem library and modeling component for atmospheric chemistry modeling.

## Getting Started

- **[HPC Installation](hpc-installation.md)** - Learn how to install CATChem on HPC systems
- **[Build System](build-system.md)** - Learn how to build CATChem
- **[Overview](overview.md)** - Understanding CATChem's architecture and capabilities
- **[Configuration System](configuration.md)** - How to configure CATChem for your needs
- **[Input Files](input-files.md)** - Required input data and formats

## Process Documentation

CATChem uses a modular process-based architecture. Each atmospheric process is implemented as a separate module. Process descriptions are automatically generated based on the notes in the code files to ensure efficient and accurate documentation. For information on how to add a new process to CATChem see the **[Process Development](../developer-guide/processes/index.md)** section of the CATChem Developer Guide.

- **[Process Overview](../processes/index.md)** - Introduction to CATChem processes

### Transport Processes
- **[Settling](../processes/settling_process.md)** - Gravitational settling processes
- **[Plumerise](../processes/plume_rise/plume_rise.md)** - Plume rise process for fires, industrial stacks, and other elevated point sources

### Chemical Processes

More information coming soon!

### Emission Processes
- **[Biogenic Emissions](../processes/biogenic_emission/biogenic_emission.md)** - Biogenic emissions from vegetation
- **[Dust Emissions](../processes/dust/dust.md)** - Windblown dust emissions
- **[Sea Salt Emissions](../processes/seasalt/index.md)** - Marine aerosol processes
- **[External Emission Plume Rise](../processes/EXTERNAL_DATA_PLUME_RISE_SOLUTION.md)** - Complete solution for external emission data and plume rise
- **[External Emissions](../processes/external_emission_data/external_emission_data.md)** - Data management for external emissions in CATChem
     -  **[Integration Guide](../processes/external_emission_data/INTEGRATION_GUIDE.md)** - Integration Guide for external emissions

### Loss Processes

More information coming soon!

## Advanced Topics

This collection provides in-depth explanations of key concepts, architecture patterns, and advanced topics for CATChem users and developers.

### 📚 Available Guides

#### Core Architecture
- **[Process Infrastructure](advanced_topics/process-infrastructure.md)** - Understanding the process framework and lifecycle
- **[Column Virtualization](advanced_topics/column-virtualization.md)** - How CATChem handles column-based data access
- **[StateContainer](advanced_topics/statecontainer.md)** - Central state management and data flow
- **[Diagnostic System](advanced_topics/diagnostic-system.md)** - Process monitoring and output management

#### Configuration & Integration
- **[Configuration Management](advanced_topics/configuration-management.md)** - YAML-based configuration system
- **[Integration Patterns](advanced_topics/integration-patterns.md)** - Common patterns for model coupling

#### Performance & Optimization
- **[Performance Optimization](advanced_topics/performance.md)** - Tuning CATChem for optimal performance

### 🎯 Guide Categories

#### **Conceptual Guides**
High-level explanations of how CATChem works and why it's designed the way it is.

#### **How-To Guides**
Step-by-step instructions for specific tasks and common workflows.

#### **Reference Guides**
Technical reference material for advanced users and developers.

### 🚀 Getting Started

If you're new to CATChem:

1. Start with **[Process Infrastructure](advanced_topics/process-infrastructure.md)** to understand the core architecture
2. Read **[Column Virtualization](advanced_topics/column-virtualization.md)** to learn about data management
3. Explore **[StateContainer](advanced_topics/statecontainer.md)** for state management concepts

For developers:

1. Review this User Guide for basic concepts
2. Study the **[Developer Guide](../developer-guide/index.md)** for implementation details
3. Use this Advanced Topics section for deep dives into specific topics

### 🤝 Contributing to Guides

We welcome contributions to improve and expand these guides:

- **Report Issues** - Found something confusing or incorrect?
- **Suggest Topics** - What guides would be helpful?
- **Contribute Content** - Share your expertise with the community

Visit our **[Contributing Guide](../developer-guide/contributing.md)** to get started.

*These guides complement the **[API Reference](../api/index.md)** and provide context for understanding CATChem's design and implementation.*


## Next Steps

- **Developers**: See the **[Developer Guide](../developer-guide/index.md)**
- **API Users**: Check the **[API Reference](../api/index.md)**
