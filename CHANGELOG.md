# Changelog

All notable changes to the Plant Breeding Analytics Suite are documented here.

---

## [3.0] — 2025

### Added
- Unified three separate Shiny apps into a single `shinydashboard` application using Shiny modules
- **D² Module:** Inter/intra-cluster distances CSV export; Cluster-wise Trait Means CSV export
- **MT Module:** Strengths & Weaknesses plots (MTSI, MGIDI) using `plot(..., type="contribution")`
- Real wheat dataset (6 environments × 36 genotypes × 3 replications × 11 traits)
- Comprehensive README, User Guide, CITATION.cff, and install helper script

### Fixed
- `tabItems()` crash: module `tagList` outputs unwrapped via `do.call(tabItems, c(as.list(...)))`
- Tocher `cluster_vec` NA bug: `tocher()$clusters` stores row indices, not genotype names
- Genotype Means CSV download: `round()` applied only to numeric columns
- Circular index plots: dynamic font sizing + staggered radii to prevent label overlap
- Non-parametric superiority plot: case-insensitive GEN column detection
- All metan `plot()` PDF downloads: explicit `print()` required for ggplot/patchwork objects

---

## [2.0] — 2024

### Added
- Multi-Trait Selection Suite (MTSI, MGIDI, FAI-BLUP, Smith-Hazel, GT/GYT, Venn)
- MET Analysis Suite with AMMI and GGE biplots

---

## [1.0] — 2023

### Added
- D² Genetic Diversity Analyser (initial standalone app)
