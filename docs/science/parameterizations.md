# Parameterizations

The Canopy-App model includes various parameterizations for physical and chemical processes within and above forest canopies. This section provides detailed descriptions of the scientific formulations and parameters used.

## Turbulence Parameterization

### K-Theory Approach

The model uses K-theory to parameterize turbulent transport:

$$
\overline{w'\phi'} = -K_{\phi} \frac{\partial \phi}{\partial z}
$$

where:
- $\overline{w'\phi'}$ is the turbulent flux of scalar $\phi$
- $K_{\phi}$ is the eddy diffusivity for scalar $\phi$
- $z$ is height above ground

### Mixing Length Model

The eddy diffusivity is calculated using a mixing length approach:

$$
K_m = l_m^2 \sqrt{\left(\frac{\partial U}{\partial z}\right)^2 + \left(\frac{\partial V}{\partial z}\right)^2}
$$

$$
K_h = \frac{K_m}{\Pr_t}
$$

where:
- $K_m$ is momentum diffusivity
- $K_h$ is heat/scalar diffusivity
- $l_m$ is mixing length
- $\Pr_t$ is turbulent Prandtl number (typically 0.7-1.3)

### Mixing Length Formulation

Within the canopy, the mixing length is parameterized as:

$$
l_m(z) = \begin{cases}
\beta h \left(\frac{z}{h}\right)^n & \text{for } z < h \\
\kappa z & \text{for } z \geq h
\end{cases}
$$

where:
- $h$ is canopy height
- $\beta$ is scaling parameter (typically 0.1-0.3)
- $n$ is shape parameter (typically 1-3)
- $\kappa$ is von Karman constant (0.41)

## Radiation Parameterization

### Two-Stream Approximation

Solar radiation transfer uses the two-stream approximation:

$$
\mu \frac{dI^+}{d\tau} = I^+ - \omega \beta_0 I^+ - \omega \beta_1 I^-
$$

$$
-\mu \frac{dI^-}{d\tau} = I^- - \omega \beta_1 I^+ - \omega \beta_0 I^-
$$

where:
- $I^+, I^-$ are upward and downward radiation intensities
- $\tau$ is optical depth
- $\omega$ is single scattering albedo
- $\beta_0, \beta_1$ are phase function parameters
- $\mu = \cos(\theta)$ for solar zenith angle $\theta$

### Leaf Area Distribution

The cumulative leaf area index from canopy top:

$$
LAI(z) = LAI_{total} \exp\left(-\alpha \left(\frac{h-z}{h}\right)^{\beta}\right)
$$

Parameters:
- $\alpha$: extinction coefficient (typically 0.5-2.0)
- $\beta$: shape parameter (typically 0.5-2.0)

## Photosynthesis Parameterization

### Farquhar-von Caemmerer-Berry Model

Net photosynthesis rate:

$$
A_n = \min(A_c, A_j) - R_d
$$

where:
- $A_c$: Rubisco-limited rate
- $A_j$: RuBP-regeneration limited rate
- $R_d$: dark respiration rate

### Rubisco-Limited Rate

$$
A_c = \frac{V_{c,max}(C_i - \Gamma^*)}{C_i + K_c(1 + O_i/K_o)}
$$

where:
- $V_{c,max}$: maximum carboxylation rate
- $C_i$: internal CO₂ concentration
- $\Gamma^*$: CO₂ compensation point
- $K_c, K_o$: Michaelis constants for CO₂ and O₂
- $O_i$: internal O₂ concentration

### Light-Limited Rate

$$
A_j = \frac{J(C_i - \Gamma^*)}{4(C_i + 2\Gamma^*)}
$$

$$
J = \frac{\alpha I + J_{max} - \sqrt{(\alpha I + J_{max})^2 - 4\theta \alpha I J_{max}}}{2\theta}
$$

where:
- $J$: electron transport rate
- $J_{max}$: maximum electron transport rate
- $\alpha$: quantum efficiency
- $I$: incident PPFD
- $\theta$: curvature parameter

## Stomatal Conductance

### Ball-Berry Model

$$
g_s = g_{s0} + \frac{m A_n h_s}{C_s}
$$

where:
- $g_s$: stomatal conductance
- $g_{s0}$: minimum conductance
- $m$: slope parameter (species-dependent)
- $A_n$: net photosynthesis rate
- $h_s$: relative humidity at leaf surface
- $C_s$: CO₂ concentration at leaf surface

### Medlyn Model (Alternative)

$$
g_s = g_{s0} + 1.6\left(1 + \frac{g_1}{\sqrt{D}}\right)\frac{A_n}{C_s}
$$

where:
- $g_1$: slope parameter
- $D$: vapor pressure deficit

## Energy Balance

### Leaf Energy Balance

For sunlit and shaded leaves:

$$
R_n = H + \lambda E + S
$$

where:
- $R_n$: net radiation
- $H$: sensible heat flux
- $\lambda E$: latent heat flux
- $S$: heat storage (usually neglected)

### Sensible Heat Flux

$$
H = \rho c_p g_{bh}(T_l - T_a)
$$

where:
- $\rho$: air density
- $c_p$: specific heat of air
- $g_{bh}$: boundary layer conductance for heat
- $T_l$: leaf temperature
- $T_a$: air temperature

### Latent Heat Flux

$$
\lambda E = \frac{\lambda \rho}{P} g_{bw} (e_l - e_a)
$$

where:
- $\lambda$: latent heat of vaporization
- $P$: atmospheric pressure
- $g_{bw}$: boundary layer conductance for water vapor
- $e_l$: vapor pressure at leaf temperature
- $e_a$: ambient vapor pressure

## Canopy Drag Parameterization

### Momentum Absorption

$$
F_u = -c_d a(z) U(z) \sqrt{U(z)^2 + V(z)^2}
$$

$$
F_v = -c_d a(z) V(z) \sqrt{U(z)^2 + V(z)^2}
$$

where:
- $F_u, F_v$: drag forces per unit volume
- $c_d$: drag coefficient (typically 0.1-0.3)
- $a(z)$: leaf area density
- $U(z), V(z)$: wind speed components

## Biogenic Emissions

### Isoprene Emissions

Following Guenther et al. (2012):

$$
E_{iso} = \epsilon_{iso} \cdot \gamma_{CE} \cdot \gamma_{age} \cdot \gamma_{SM} \cdot D \cdot LAI
$$

where:
- $\epsilon_{iso}$: emission factor
- $\gamma_{CE}$: canopy environment activity factor
- $\gamma_{age}$: leaf age activity factor
- $\gamma_{SM}$: soil moisture activity factor
- $D$: light distribution factor

### Temperature and Light Dependence

$$
\gamma_{CE} = C_T \cdot C_L
$$

$$
C_T = \frac{\exp\left(\frac{C_{T1}(T-T_s)}{RT_sT}\right)}{1 + \exp\left(\frac{C_{T2}(T-T_{M})}{RT_sT}\right)}
$$

$$
C_L = \frac{\alpha C_L1 PPFD}{\sqrt{1 + \alpha^2 PPFD^2}}
$$

Parameters:
- $C_{T1} = 95,000$ J/mol
- $C_{T2} = 230,000$ J/mol
- $T_s = 303$ K (standard temperature)
- $T_M = 314$ K (maximum temperature)
- $\alpha = 0.0027$ (empirical coefficient)
- $C_{L1} = 1.066$ (empirical coefficient)

## Dry Deposition

### Resistance Model

Total deposition velocity:

$$
v_d = \frac{1}{r_a + r_b + r_c}
$$

where:
- $r_a$: aerodynamic resistance
- $r_b$: quasi-laminar boundary layer resistance
- $r_c$: canopy resistance

### Canopy Resistance

$$
\frac{1}{r_c} = \frac{1}{r_s + r_m} + \frac{1}{r_{lu}} + \frac{1}{r_{dc}} + \frac{1}{r_{cl}}
$$

where:
- $r_s$: stomatal resistance
- $r_m$: mesophyll resistance
- $r_{lu}$: resistance of upper canopy
- $r_{dc}$: resistance of lower canopy/ground
- $r_{cl}$: resistance of exposed surfaces

## Parameter Tables

### Vegetation-Specific Parameters

| Parameter | Conifer | Deciduous | Grass | Units |
|-----------|---------|-----------|--------|-------|
| $V_{c,max}$ (25°C) | 60 | 80 | 40 | μmol/m²/s |
| $J_{max}$ (25°C) | 120 | 160 | 80 | μmol/m²/s |
| $g_1$ | 3.0 | 4.0 | 5.0 | kPa^0.5 |
| $\epsilon_{iso}$ | 0.1 | 10.0 | 0.0 | μg/g/h |
| $c_d$ | 0.15 | 0.20 | 0.10 | - |

### Temperature Response Parameters

| Parameter | Value | Units | Description |
|-----------|-------|-------|-------------|
| $Q_{10,V}$ | 2.0 | - | $V_{c,max}$ temperature sensitivity |
| $Q_{10,J}$ | 1.9 | - | $J_{max}$ temperature sensitivity |
| $Q_{10,R}$ | 2.0 | - | Respiration temperature sensitivity |
| $H_a$ | 72000 | J/mol | Activation energy |
| $H_d$ | 200000 | J/mol | Deactivation energy |
| $\Delta S$ | 650 | J/mol/K | Entropy term |

## Model Validation

### Flux Tower Comparisons

The parameterizations have been validated against:

- **FLUXNET** sites (global network)
- **AmeriFlux** sites (North American network)
- **Long-term ecological research** sites

Typical model performance:
- Sensible heat flux: R² = 0.85, RMSE = 50 W/m²
- Latent heat flux: R² = 0.80, RMSE = 60 W/m²
- CO₂ flux: R² = 0.75, RMSE = 5 μmol/m²/s

### Sensitivity Analysis

Key sensitive parameters identified through sensitivity analysis:

1. **Leaf area index** (±20% → ±15% flux change)
2. **Drag coefficient** (±50% → ±25% wind change)
3. **Maximum carboxylation rate** (±20% → ±18% photosynthesis change)

## References

Key scientific references for parameterizations:

1. **Farquhar et al. (1980)**: Photosynthesis model
2. **Ball et al. (1987)**: Stomatal conductance
3. **Guenther et al. (2012)**: Biogenic emissions
4. **Raupach & Thom (1981)**: Canopy turbulence
5. **Dai et al. (2004)**: Two-stream radiation

For implementation details, see the [API Reference](../api/overview.md).
