# ============================================================
#  Plant Breeding Analytics Suite — Package Installer
#  Dr. Vijay Kamal Meena | Agriculture University Jodhpur
# ============================================================

cat("Installing required packages for Plant Breeding Analytics Suite...\n\n")

pkgs <- c(
  # Shiny framework
  "shiny", "shinydashboard", "shinyWidgets", "shinyjs", "shinycssloaders",

  # Tables & interactivity
  "DT", "plotly",

  # Data wrangling
  "dplyr", "tidyr", "tibble", "purrr", "reshape2", "readxl", "writexl",

  # Visualisation
  "ggplot2", "ggrepel", "scales", "viridis", "RColorBrewer",
  "ggforce", "patchwork", "fmsb", "corrplot", "ggdendro",

  # Multivariate & diversity
  "factoextra", "FactoMineR", "Hmisc", "biotools", "dendextend",

  # Plant breeding / MET
  "metan"
)

# Check which are missing
missing_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]

if (length(missing_pkgs) == 0) {
  cat("✅ All packages are already installed.\n")
} else {
  cat(sprintf("📦 Installing %d missing package(s): %s\n\n",
              length(missing_pkgs), paste(missing_pkgs, collapse=", ")))
  install.packages(missing_pkgs, dependencies=TRUE)
  cat("\n✅ Installation complete.\n")
}

cat("\n🚀 To launch the app, run:\n")
cat('   shiny::runApp("app.R")\n\n')
