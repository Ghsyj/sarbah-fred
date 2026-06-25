# NEWS: Psychometric Integrity Architecture (PIA-Protocol)

## PIA-Protocol 1.0.0 (2026-06-26)
### Formal Statistical Validation & Parameter Constraints
* **Four-Stage Factorial Invariance Protocol:** Upgraded the structural validation matrix (`src/validation.R`) from a basic three-stage model to a rigorous four-stage nested verification framework. The engine now sequentially evaluates Configural, Metric, Scalar, and Strict Invariance constraints by restricting item factor loadings ($\Lambda$), intercepts ($\nu$), and unique residual variances ($\Theta$). This allows the system to pinpoint exactly where strategic faking compresses or alters item-level measurement error structures across testing groups.
* **Numerical Invertibility Safeguards:** Implemented a robust optimization check utilizing the inverse of the implied covariance matrix ($\Sigma^{-1}$) within individual log-likelihood gradient scoring loops. This safeguard prevents model evaluation crashes caused by non-positive definite or singular matrices when analyzing highly skewed or artificial response profiles.
* **Algorithmic Convergence Architecture:** Standardized robust estimator pathways (`MLR` and `WLSMV`) along with Full Information Maximum Likelihood (`FIML`) integration to protect parameters from bias when processing missing data blocks or extreme ceiling responses.

## PIA-Protocol 0.2.0 (2026-04-12)
### Stochastic Latency Decompositions & Cross-Platform Formats
* **Ex-Gaussian Density Optimization:** Deployed a bounded Maximum Likelihood Estimation (MLE) engine within `src/hddm_lat.R` to isolate the exponential tail parameter ($\tau$). This parameter captures the cognitive friction and delayed processing signatures indicative of active profile distortion.
* **Multi-Subject Data Transformation Arrays:** Created a structured data translation layer to map item responses, trial latencies, and subject grouping vectors directly into clean long-format schemas. This allows your datasets to interface cleanly with Python-based hierarchical drift-diffusion models (`HDDM`).
* **Parameter Space Boundaries:** Injected custom PORT optimization boundaries to prevent local parameter drift and safeguard solver convergence when processing fast, automated response streams.

## PIA-Protocol 0.1.0 (2026-02-18)
### Core Latent Estimation & Structural Safeties
* **Multi-Parameter IRT Engine Architecture:** Implemented a high-performance estimation engine (`src/core_irt.R`) supporting 2PL, 3PL, and 4PL Item Response Theory models. It resolves item discrimination ($a_i$), difficulty ($b_i$), guessing ($c_i$), and inattention/faking ($d_i$) parameters using stabilized expectation-maximization (EM) iterations.
* **Standardized Person-Fit Diagnostics:** Integrated Drasgow's parametric person-fit statistic ($l_z$) to evaluate individual response vectors against expected latent trait distributions ($\theta$), identifying anomalous or contradictory answering profiles.
* **Defensive Matrix Inversion Scrubbing:** Added an automated filter to remove zero-variance items, protecting the model's Hessian matrix from division-by-zero failures during standard error estimation.
