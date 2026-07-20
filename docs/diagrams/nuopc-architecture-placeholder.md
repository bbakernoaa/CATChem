# NUOPC Architecture Diagram

This is a placeholder for the NUOPC architecture diagram. The actual diagram should show:

## Components:
- NUOPC Driver/Mediator at the top
- Connected model components: ATM, CHM (CATChem), OCN, ICE, LND
- Field exchange and coupling between components
- ESMF infrastructure layer
- Parallel decomposition and communication

## Key Elements:
- Standard NUOPC phases (Initialize, Run, Finalize)
- Field advertisement and realization
- Time management and synchronization
- Grid and regridding capabilities
- Performance monitoring

To create the actual PNG diagram, use a tool like:
- Draw.io/Lucidchart for professional diagrams
- Mermaid with PNG export
- PowerPoint/Keynote with PNG export
- Python matplotlib/seaborn for programmatic generation

The diagram should be saved as `docs/diagrams/nuopc-architecture.png`
