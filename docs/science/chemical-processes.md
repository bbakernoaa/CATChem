# Chemical Processes

Detailed description of chemical processes and photochemistry in the Canopy-App model.

## Overview

The Canopy-App includes comprehensive treatment of chemical processes within and above forest canopies, focusing on:

- **Biogenic emission chemistry**
- **Photolysis rate calculations**
- **Gas-phase chemical reactions**
- **Dry deposition of chemical species**

## Biogenic Emission Chemistry

### Volatile Organic Compounds (VOCs)

#### Isoprene Emissions

Isoprene (C₅H₈) is the most abundant biogenic VOC, following the Guenther et al. (2012) algorithms:

```fortran
! Isoprene emission rate
E_iso = ε_iso × γ_T × γ_P × γ_SM × ρ_foliage
```

**Where:**
- `ε_iso`: Base emission factor (μg g⁻¹ h⁻¹)
- `γ_T`: Temperature activity factor
- `γ_P`: PAR (photosynthetically active radiation) activity factor
- `γ_SM`: Soil moisture activity factor
- `ρ_foliage`: Foliar density (g m⁻³)

#### Temperature Dependence

```fortran
! Temperature activity factor
γ_T = E_opt × C_T2 × exp(C_T1 × (T - T_s)) /
      (C_T2 - C_T1 × (1 - exp(C_T2 × (T - T_s))))
```

**Parameters:**
- `E_opt`: Maximum normalized emission capacity
- `C_T1`: Empirical coefficient (95,000 J mol⁻¹)
- `C_T2`: Empirical coefficient (230,000 J mol⁻¹)
- `T_s`: Standard temperature (303 K)

#### Light Dependence

```fortran
! PAR activity factor
γ_P = α × PAR / √(1 + α² × PAR²)
```

**Where:**
- `α`: Empirical coefficient (0.0027 mol⁻¹ m² s)
- `PAR`: Photosynthetically active radiation (μmol m⁻² s⁻¹)

#### Implementation

See module `canopy_bioemi_mod.F90`:
- `calc_isoprene_emission()` - Main isoprene calculation
- `temperature_activity()` - Temperature dependence
- `light_activity()` - Light dependence

### Monoterpene Emissions

#### Temperature-Only Dependence

Monoterpenes (α-pinene, β-pinene, limonene) depend only on temperature:

```fortran
! Monoterpene emission rate
E_mono = ε_mono × exp(β × (T - T_s)) × ρ_foliage
```

**Where:**
- `β`: Temperature coefficient (0.09 K⁻¹)
- Other variables as defined for isoprene

#### Species-Specific Factors

```fortran
! Individual monoterpene species
E_α_pinene = 0.5 × E_mono    ! 50% of total
E_β_pinene = 0.3 × E_mono    ! 30% of total
E_limonene = 0.2 × E_mono    ! 20% of total
```

### Other Biogenic VOCs

#### Methanol and Acetone

```fortran
! Light-independent emissions
E_methanol = ε_methanol × γ_T × ρ_foliage
E_acetone = ε_acetone × γ_T × ρ_foliage
```

#### Sesquiterpenes

```fortran
! High-molecular-weight terpenes
E_sesqui = ε_sesqui × exp(β_sesqui × (T - T_s)) × ρ_foliage
```

## Photolysis Rate Calculations

### Actinic Flux

#### Above-Canopy Calculation

Solar actinic flux above the canopy:

```fortran
! Clear-sky actinic flux
F_clear(λ) = F_0(λ) × cos(SZA) × τ_atm(λ)
```

**Where:**
- `F_0(λ)`: Extraterrestrial solar flux
- `SZA`: Solar zenith angle
- `τ_atm(λ)`: Atmospheric transmission

#### Within-Canopy Attenuation

```fortran
! Canopy attenuation
F_canopy(λ,z) = F_clear(λ) × [f_direct(λ,z) + f_diffuse(λ,z)]
```

**Direct beam component:**
```fortran
f_direct(λ,z) = exp(-k_direct(λ) × LAI_cumulative(z))
```

**Diffuse component:**
```fortran
f_diffuse(λ,z) = f_sky × exp(-k_diffuse(λ) × LAI_cumulative(z)) +
                 f_scattered(λ,z)
```

### Photolysis Rate Constants

#### J-Value Calculation

```fortran
! Photolysis rate constant
J(z) = ∫ σ(λ,T) × φ(λ,T) × F(λ,z) dλ
```

**Where:**
- `σ(λ,T)`: Absorption cross-section (cm²)
- `φ(λ,T)`: Quantum yield
- `F(λ,z)`: Actinic flux (photons cm⁻² s⁻¹ nm⁻¹)

#### Key Photolysis Reactions

**Ozone photolysis:**
```fortran
! O₃ + hν → O₂ + O(¹D)    (λ < 320 nm)
J_O3_O1D = ∫₂₈₀³²⁰ σ_O3(λ,T) × φ_O1D(λ,T) × F(λ,z) dλ
```

**NO₂ photolysis:**
```fortran
! NO₂ + hν → NO + O(³P)   (λ < 420 nm)
J_NO2 = ∫₂₈₀⁴²⁰ σ_NO2(λ,T) × φ_NO2(λ,T) × F(λ,z) dλ
```

**Formaldehyde photolysis:**
```fortran
! HCHO + hν → H₂ + CO     (λ < 370 nm)
! HCHO + hν → H + HCO     (λ < 370 nm)
J_HCHO_H2 = ∫₂₈₀³⁷⁰ σ_HCHO(λ,T) × φ_H2(λ,T) × F(λ,z) dλ
J_HCHO_H = ∫₂₈₀³⁷⁰ σ_HCHO(λ,T) × φ_H(λ,T) × F(λ,z) dλ
```

#### Implementation

See module `canopy_phot_mod.F90`:
- `calc_photolysis_rates()` - Main photolysis routine
- `actinic_flux_profile()` - Actinic flux calculations
- `read_cross_sections()` - Spectroscopic data

## Gas-Phase Chemical Reactions

### Chemical Mechanisms

#### Simplified Hydrocarbon Chemistry

**Isoprene oxidation initiation:**
```fortran
! OH-initiated
C5H8 + OH → RO2 + H2O                    ! k = 1.0×10⁻¹⁰ cm³ s⁻¹

! O₃-initiated
C5H8 + O3 → products                     ! k = 1.3×10⁻¹⁷ cm³ s⁻¹

! NO₃-initiated (nighttime)
C5H8 + NO3 → RO2 + HNO3                 ! k = 3.2×10⁻¹³ cm³ s⁻¹
```

**Monoterpene oxidation:**
```fortran
! α-Pinene + OH
C10H16 + OH → RO2                       ! k = 5.3×10⁻¹¹ cm³ s⁻¹

! α-Pinene + O₃
C10H16 + O3 → products                  ! k = 8.7×10⁻¹⁷ cm³ s⁻¹
```

#### Secondary Organic Aerosol (SOA) Formation

```fortran
! Low-volatility products
RO2 + NO → RONO2 + LV_products          ! Nitrate pathway
RO2 + HO2 → ROOH + LV_products          ! Peroxide pathway
RO2 + RO2 → products + LV_products      ! Self-reaction
```

### Nitrogen Oxide Chemistry

#### NOₓ Cycle

```fortran
! Basic NOₓ reactions
NO + O3 → NO2 + O2                      ! k = 1.9×10⁻¹⁴ cm³ s⁻¹
NO2 + hν → NO + O(³P)                   ! J_NO2
O(³P) + O2 + M → O3 + M                 ! k = 6.0×10⁻³⁴ cm⁶ s⁻¹
```

#### Organic Nitrate Formation

```fortran
! RO₂ + NO reactions
RO2 + NO → RO + NO2                     ! (1-α) pathway
RO2 + NO → RONO2                        ! α pathway (branching ratio)
```

### Radical Chemistry

#### HOₓ Reactions

```fortran
! OH production
O(¹D) + H2O → 2OH                       ! k = 1.6×10⁻¹⁰ cm³ s⁻¹
HO2 + NO → OH + NO2                     ! k = 3.3×10⁻¹² cm³ s⁻¹

! OH consumption
OH + CO → H + CO2                       ! k = 2.3×10⁻¹³ cm³ s⁻¹
OH + VOC → RO2 + H2O                    ! Variable rates
```

#### Peroxy Radical Reactions

```fortran
! HO₂ formation and loss
H + O2 + M → HO2 + M                    ! k = 4.4×10⁻³² cm⁶ s⁻¹
HO2 + HO2 → H2O2 + O2                   ! k = 1.9×10⁻¹² cm³ s⁻¹
```

## Dry Deposition Chemistry

### Species-Specific Deposition

#### Ozone Deposition

```fortran
! O₃ deposition velocity
v_d(O3) = 1 / (R_a + R_b + R_c)
```

**Surface resistance components:**
```fortran
! Stomatal pathway
R_s = R_s_min × (1 + (D_s/D_0)^n) × f(T) × f(Ψ_l)

! Non-stomatal pathway
R_ns = R_cut + R_soil + R_water
```

#### NO₂ Deposition

```fortran
! NO₂ surface resistance
R_c(NO2) = 1 / (1/R_s + 1/R_cut)
```

**Where:**
- Stomatal uptake dominates during day
- Cuticular uptake important at night

#### SO₂ Deposition

```fortran
! SO₂ high solubility
R_c(SO2) = R_s × f_0 / (1 + (D_s/D_0))
```

#### NH₃ Bidirectional Exchange

```fortran
! NH₃ can have emission or deposition
F_NH3 = v_d × (C_atm - C_comp)
```

**Where:**
- `C_comp`: Compensation point concentration
- Positive flux = emission, negative = deposition

### Henry's Law Constants

Aqueous-phase partitioning:

```fortran
! Dimensionless Henry's law constant
H_cc = H_cp × R × T
```

**Species values:**
- SO₂: H_cp = 1.2 M atm⁻¹
- NH₃: H_cp = 58 M atm⁻¹
- HNO₃: H_cp = 2.1×10⁵ M atm⁻¹

## Chemical Kinetics Implementation

### Rate Constant Calculations

#### Temperature Dependence

```fortran
! Arrhenius equation
k(T) = A × exp(-E_a / (R × T))
```

#### Pressure Dependence

```fortran
! Three-body reactions
k(T,P) = k_0 × [M] / (1 + k_0 × [M] / k_∞) × F_c^x

! Where x = {1 + [log₁₀(k_0×[M]/k_∞)]²}⁻¹
```

### Numerical Integration

#### Stiff Solver

For chemical ODEs:

```fortran
! Implicit Euler method
C(t+dt) = C(t) + dt × P(C(t+dt)) - dt × L(C(t+dt)) × C(t+dt)
```

#### Operator Splitting

```fortran
! Sequence for each time step:
! 1. Emissions
! 2. Chemistry
! 3. Deposition
! 4. Vertical mixing
```

## Model Validation

### Chamber Studies

Comparison with environmental chamber experiments:
- **Isoprene + OH**: SOA yields within 20%
- **α-Pinene + O₃**: Product distributions match
- **NOₓ photochemistry**: Ozone production rates validated

### Field Measurements

Validation against flux tower and aircraft data:
- **Emission fluxes**: Within factor of 2
- **Concentration profiles**: R² > 0.8
- **Photolysis rates**: ±30% of measured values

## Future Developments

### Enhanced Chemistry

- **Detailed SOA mechanisms** (VBS framework)
- **Aqueous-phase chemistry** in cloud droplets
- **Heterogeneous reactions** on aerosol surfaces
- **Halogen chemistry** in coastal regions

### Numerical Improvements

- **Adaptive time stepping** for stiff chemistry
- **Higher-order integration** schemes
- **Vectorized chemistry** for computational efficiency

## References

### Key Chemical Papers

1. **Guenther, A.B., et al. (2012)**. "MEGAN2.1: Model of Emissions of Gases and Aerosols from Nature." *Geosci. Model Dev.*, 5, 1471-1492.

2. **Sander, S.P., et al. (2011)**. "Chemical Kinetics and Photochemical Data for Use in Atmospheric Studies." *JPL Publication 10-6*.

3. **Wild, O., et al. (2000)**. "Fast-J: Accurate simulation of in- and below-cloud photolysis in tropospheric chemical models." *J. Atmos. Chem.*, 37, 245-282.

4. **Atkinson, R., and J. Arey (2003)**. "Atmospheric degradation of volatile organic compounds." *Chem. Rev.*, 103, 4605-4638.

## Navigation

- **[Model Description](model-description.md)** - Overall model framework
- **[Physical Processes](physical-processes.md)** - Meteorology and turbulence
- **[Parameterizations](parameterizations.md)** - Mathematical formulations
- **[API Reference](../api/overview.md)** - Implementation details
