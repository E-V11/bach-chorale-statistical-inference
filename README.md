# Bach Chorale Cadence Analysis: Exact Statistical Inference

Statistical analysis evaluating whether J. S. Bach's 371 Riemenschneider chorales end on soprano scale degree 1 at rates exceeding historical 90% benchmarks.

## Overview
* **Data Pipeline:** Parsed Humdrum `**kern` encodings in R, filtered duplicate harmonizations and key conflicts to construct a clean sample ($n = 343$).
* **Methodology:** Modeled $X \sim \text{Binomial}(n, p)$. Derived unbiasedness, variance, MSE, and consistency ($\hat{p} \xrightarrow{P} p$).
* **Inference:** Used Clopper-Pearson exact confidence intervals and exact binomial testing to handle boundary behavior ($p \approx 1$).

---

## Results

| Metric | Value |
| :--- | :--- |
| **Sample Size ($n$)** | 343 unique chorales |
| **Successes ($X$)** | 316 |
| **Sample Proportion ($\hat{p}$)** | 92.13% ($\text{SE} = 0.0145$) |
| **95% Clopper-Pearson CI** | (0.888, 0.948) |
| **Exact $p$-value ($H_0: p = 0.90$)** | 0.108 |
| **Conclusion ($\alpha = 0.05$)** | Fail to reject $H_0$ |

> **Takeaway:** While 92.1% of chorales end on scale degree 1, the observed rate does not statistically exceed the 90% baseline ($p = 0.108$).

---

## Code Base Structure

```text
├── data/       # Parsed CSV datasets
├── src/        # R scripts for parsing, deduplication, and testing
└── paper/      # LaTeX source and compiled report PDF
