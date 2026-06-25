# 📘 User Guide — Plant Breeding Analytics Suite

**Dr. Vijay Kamal Meena** | Agriculture University Jodhpur | v3.0 (2025)

---

## Table of Contents

1. [Getting Started](#1-getting-started)
2. [Data Format Requirements](#2-data-format-requirements)
3. [Module 1 — D² Genetic Diversity Analyser](#3-module-1--d²-genetic-diversity-analyser)
4. [Module 2 — MET Analysis Suite](#4-module-2--met-analysis-suite)
5. [Module 3 — Multi-Trait Selection Suite](#5-module-3--multi-trait-selection-suite)
6. [Export Options](#6-export-options)
7. [Troubleshooting](#7-troubleshooting)

---

## 1. Getting Started

### 1.1 Launch the App

```r
# From RStudio — open project and run:
shiny::runApp("app.R")

# Or source directly:
source("app.R")
```

### 1.2 First-Time Setup

Run the installer script to check and install all dependencies:

```r
source("install_packages.R")
```

---

## 2. Data Format Requirements

### Module 1 — D² Diversity (two files needed)

#### Raw Data File (`DWR_raw_data.csv`)
```
GEN, REP, Trait1, Trait2, ..., TraitN
Genotype1, 1, 45.2, 12.3, ...
Genotype1, 2, 44.8, 12.1, ...
```
- Column 1: Genotype name (`GEN`)
- Column 2: Replication number (`REP`)
- Columns 3+: Numeric trait measurements
- Set **"Trait start column"** in the app to `3` (default)

#### Genotype Means File (`DWR_genotype_means.csv`)
```
GEN, Trait1, Trait2, ..., TraitN
Genotype1, 45.0, 12.2, ...
```
- Column 1: Genotype name (`GEN`)
- Columns 2+: Pre-computed trait means

### Module 2 & 3 — MET Data (`MET_wheat_data.csv`)
```
ENV, REP, GEN, Trait1, Trait2, ..., TraitN
E1, 1, Genotype1, 90, 132, ...
```
- Requires at minimum 3 identifier columns: environment, replication, genotype
- Column names are flexible — you map them in the app

---

## 3. Module 1 — D² Genetic Diversity Analyser

### Step 1: Data Upload
1. Navigate to **D² — Data Upload**
2. Upload your **Raw Data** CSV (format: GEN, REP, traits)
3. Upload your **Genotype Means** CSV (format: GEN, traits)
4. Set **Trait start column** (default = 3 for raw data)
5. Click **▶ Load & Validate Data**
6. Check the data preview tables for correct loading

### Step 2: MANOVA
1. Go to **D² — MANOVA**
2. Click **▶ Run MANOVA**
3. View the multivariate test statistics (Pillai, Wilks λ, Hotelling, Roy)
4. The univariate ANOVA table per trait appears automatically below

### Step 3: D² Distances
1. Go to **D² — Mahalanobis Distances**
2. Click **▶ Compute D² Distances**
3. View the distance matrix and interactive heatmap
4. Higher D² values indicate greater genetic divergence between genotypes

### Step 4: Tocher Clustering
1. Go to **D² — Tocher Clustering**
2. Click **▶ Run Tocher**
3. View:
   - **Cluster Membership** — which genotypes belong to which cluster
   - **Inter/Intra-cluster Distances** — within-cluster (diagonal) vs between-cluster distances
   - **Dendrogram** — hierarchical representation
   - **Network Distance Plot** — graph-based cluster visualization
   - **Cluster-wise Trait Means** — mean value of each trait per cluster

> **Interpretation:** Genotypes within the same cluster are less divergent. For hybrid development, choose parents from distant clusters (high inter-cluster D²).

### Step 5: PCA
1. Go to **D² — Principal Component Analysis**
2. Click **▶ Run PCA**
3. View:
   - **Scree Plot** — variance explained by each PC
   - **PCA Biplot** — genotypes (points) + traits (vectors)
   - **Eigenvalues Table** — % variance and cumulative % per PC
   - **Variable Loadings** — contribution of each trait to each PC
   - **PCA with Tocher Cluster Overlay** — genotypes coloured by cluster

> **Tip:** PCs with eigenvalue > 1 (Kaiser criterion) or explaining ≥ 5% variance are generally retained.

### Step 6: Correlation
1. Go to **D² — Correlation Analysis**
2. Choose colour scheme (Red-White-Blue, Pastel, or Viridis)
3. Click **▶ Compute Correlation**
4. View the heatmap and the table of significant pairs (p ≤ 0.05)

---

## 4. Module 2 — MET Analysis Suite

### Data Upload
1. Go to **MET — Data Upload**
2. Upload your MET CSV file
3. Map column names: ENV, GEN, REP
4. Click **▶ Load Data**
5. The value boxes show environment count, genotype count, and trait count

### Descriptive Statistics
- Select a trait from the dropdown
- View summary statistics and distribution plots (histogram, boxplot, GxE heatmap)

### ANOVA
- **Individual ANOVA:** per-environment ANOVA for each trait
- **Pooled ANOVA:** combined analysis across environments
- Bartlett test for variance homogeneity is included

### Stability Analysis

#### ANOVA-based Stability
- **Ecovalence (Wricke's W²i):** low W²i = more stable
- **Shukla's σ²i:** variance component-based; lower = more stable

#### Regression-based (Eberhart–Russell)
- **bi (regression coefficient):** bi = 1 → average response; bi > 1 → above-average response
- **S²di (deviation from regression):** low S²di = more predictable

#### Non-parametric Stability
- **Lin & Binns Superiority (Pi):** lower Pi = more stable AND high-yielding
- **Fox Top-Third:** % environments in which a genotype ranked in top 1/3

#### AMMI Analysis
1. Select trait and click **▶ Run AMMI**
2. Available biplots:
   - **AMMI1:** Genotype/environment means vs PC1 scores
   - **AMMI2:** PC1 vs PC2 scores
   - **AMMI biplot:** Combined biplot
3. **ASV** (AMMI Stability Value) — lower = more stable
4. **WAAS** (Weighted Average of Absolute Scores) — lower = more stable

#### GGE Biplot
1. Select trait, biplot type (7 options), and SVP (1=genotype, 2=environment, 3=symmetric)
2. Biplot types: Basic, Discriminativeness, Representativeness, Mean vs Stability, Ranking, Comparing, which-won-where

---

## 5. Module 3 — Multi-Trait Selection Suite

### Step 1: Data & Settings
1. Go to **MT — Data & Settings**
2. Upload MET data CSV
3. Map ENV, GEN, REP columns
4. Select **yield trait** (used for direct selection comparison)
5. Set **Selection Intensity (%)** (default = 15%)
6. For each trait, set **goal** (↑ increase = `h` / ↓ decrease = `l`)
7. Click **⚙️ Process Data**

### Step 2: Fit Models
1. Go to **MT — Fit Models**
2. Click **▶ Fit gamem_met + waasb**
3. ⚠️ This step fits mixed models — may take 1–5 minutes depending on data size
4. Progress notifications appear when complete

### Step 3: Variance Components
- Click **▶ Compute Variance Components**
- View BLUP-based variance decomposition per trait

### MTSI — Multi-Trait Stability Index
- Click **▶ Run MTSI**
- Based on the `waasb` model (stability-weighted)
- Tabs:
  - **MTSI Index** — ranked values for all genotypes
  - **Selected** — list of selected genotypes
  - **Circular Plot** — visual ranking (orange = selected)
  - **Strengths & Weaknesses** — per-trait selection differentials
  - **Contribution** — factor contribution plot
  - **metan Default** — metan's built-in index plot

### MGIDI — Multi-trait Genotype-Ideotype Distance Index
- Click **▶ Run MGIDI**
- Based on the `gamem_met` model (means-based)
- Lower MGIDI = closer to ideotype
- Same plot tabs as MTSI

### FAI-BLUP — Factor Analysis and Ideotype-Design
- Click **▶ Run FAI-BLUP**
- Uses factor scores from BLUP predictions
- Higher ID1 score = closer to ideotype

### Smith-Hazel Index
- Click **▶ Run Smith-Hazel**
- Classical economic weights-based selection index
- Weights are derived from phenotypic and genotypic covariance matrices

### Direct Selection
- Click **▶ Run Direct Selection**
- Selects top % genotypes based solely on the designated yield trait

### Selection Summary
- **Table 3** — selection differentials and genetic gains for all methods
- **Coincidence Index** — pairwise overlap between selection methods
- **Venn Diagram** — 4-way Venn showing overlap of selected sets (Direct, MTSI, MGIDI, Smith-Hazel)

### GT / GYT Biplots
- **GT biplot** — Genotype × Trait biplot from means
- **GYT biplot** — Genotype × Yield×Trait biplot

### Radar Chart
- Compare selected genotypes across all traits
- Selectable method and genotype subset

---

## 6. Export Options

### D² Module
| Button | Output |
|---|---|
| Genotype Means (.csv) | Mean values per genotype per trait |
| D² Matrix (.csv) | Full Mahalanobis distance matrix |
| Tocher Clusters (.csv) | Genotype-cluster assignments |
| Inter/Intra Cluster Distances (.csv) | Cluster distance matrix |
| Cluster-wise Trait Means (.csv) | Mean trait values per cluster |
| Correlation Matrix (.csv) | Pearson r values |
| PCA Eigenvalues (.csv) | Eigenvalues and % variance |
| PCA Loadings (.csv) | Variable loadings on each PC |
| PCA Scores (.csv) | Genotype scores on each PC |
| ANOVA per Trait (.csv) | ANOVA table for each trait |
| Dendrogram (.pdf) | High-resolution dendrogram |
| Network Plot (.pdf) | Cluster network graph |
| D² Heatmap (.pdf) | Distance heatmap |
| Scree Plot (.pdf) | PCA scree plot |
| PCA Biplot (.pdf) | PCA biplot |
| Corr. Heatmap (.pdf) | Correlation heatmap |

### MET & MT Modules
- Each analysis has its own **⬇ Download** button for XLSX tables and PDF plots

---

## 7. Troubleshooting

| Problem | Solution |
|---|---|
| App does not start | Run `source("install_packages.R")` to install missing packages |
| Data not loading | Check column names — GEN, REP columns are case-sensitive |
| Tocher shows no clusters | Run D² distances first before clicking Run Tocher |
| PCA cluster overlay is empty | Run both PCA and Tocher before viewing the overlay |
| Model fitting hangs | Reduce number of traits; ensure ≥ 3 replications per environment |
| PDF download is blank | Ensure the analysis has been run first; plots require computed results |
| MGIDI error about ideotype | Check that trait goals are set for all selected traits |

---

*User Guide v3.0 | Plant Breeding Analytics Suite | Dr. Vijay Kamal Meena | Agriculture University Jodhpur | 2025*
