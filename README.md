# PIA-Protocol: Psychometric Integrity Architecture
### Computational Diagnostic Engine for Latent-Class Response Distortion

[![OSF](https://img.shields.io/badge/OSF-Pre--registration-blue.svg)](https://osf.io/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Language: R](https://img.shields.io/badge/Language-R-1565C0.svg)](https://www.r-project.org/)
[![Target: HU Berlin](https://img.shields.io/badge/Target-Ziegler--Lab%40HU--Berlin-black.svg)](https://www.psychologie.hu-berlin.de/de/professuren/psychologische-diagnostik)

---

## 🧬 Academic Overview & Theoretical Architecture

The `PIA-Protocol` is an open-science computational framework built for the **R** ecosystem. It isolates, models, and flags non-random human variance—specifically response distortion (faking) and anomalous profiles—within unproctored online assessments. 

By unifying **Multi-Parameter Item Response Theory (IRT)** with **Stochastic Latency Distributions**, this architecture decomposes standard assessment arrays into explicit item-person validity indices, replacing black-box analytics with mathematically verifiable diagnostics.

### Core Psychometric Operations
1. **Latent Trait Estimation:** Isolates applicant trait fields from structural guessing parameters.
2. **Anomalous Latency Decomposition:** Evaluates response velocity vectors ($\tau$) to separate automated manipulation from natural human cognitive delays.
3. **Person-Fit Optimization:** Calculates structural residuals between observed responses and parametric expectations to catch profile contradictions.

---

## 🧮 Mathematical Foundations

The architecture models response outcomes ($X_{pi}$) and response times ($RT_{pi}$) simultaneously as interdependent functions.

### 1. Multi-Parameter Item Response Theory (4PL Integration)
The probability $P$ of an affirmative response from participant $p$ on item $i$ is formulated as:

$$P(X_{pi} = 1 \mid \theta_p, a_i, b_i, c_i, d_i) = c_i + \frac{d_i - c_i}{1 + e^{-1.702 \cdot a_i (\theta_p - b_i)}}$$

*   $\theta_p$: True latent trait location.
*   $a_i, b_i$: Item discrimination and item difficulty parameters.
*   $c_i, d_i$: Lower (guessing) and upper (inattention/faking caps) asymptotes.

### 2. Stochastic Joint Response-Velocity Modeling
To isolate latency distortions caused by active faking or item tampering, response times are modeled via an Ex-Gaussian distribution to isolate the exponential parameter $\tau_{pi}$:

$$f(RT_{pi} \mid \mu_{pi}, \sigma_{pi}, \tau_{pi}) = \frac{1}{\tau_{pi}} \exp \left( \frac{\mu_{pi} - RT_{pi}}{\tau_{pi}} + \frac{\sigma_{pi}^2}{2\tau_{pi}^2} \right) \Phi \left( \frac{RT_{pi} - \mu_{pi}}{\sigma_{pi}} - \frac{\sigma_{pi}}{\tau_{pi}} \right)$$

---

## 📂 System Architecture

```text
sarbah-fred/
├── LICENSE         <- MIT open-source academic preservation license
├── README.md       <- High-density repository portal and equations
├── src/            <- Core computational engines
    ├── core_irt.R  <- 2PL/3PL/4PL parameter estimation loops
    ├── hddm_lat.R  <- Ex-Gaussian and drift-diffusion response models
    └── validation.R<- CFA and measurement invariance evaluation
