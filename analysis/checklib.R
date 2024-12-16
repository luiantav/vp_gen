packages <- c("ggplot2", "dplyr", "stats", "lme4", "here", "renv", "gtools",
              "see", "plotrix", "modelbased", "emmeans", "performance", "wesanderson",
              "tidyverse", "ggsignif", "broom", "quickpsy", "ggpubr", "performance",
              "glmmTMB", "loo", "bayesplot", "rlang", "bayestestR", "rstan", "brms",
              "tidybayes", "sjPlot", "sjmisc", "ghibli", "yardstick", "effectsize",
              "datawizard", "esc", "grid", "svglite", "lsr", "gridExtra", "boot")

# Read the script file
script <- readLines("your_script.R")

# Function to find package usage
check_package_usage <- function(packages, script) {
  used_packages <- c()
  
  for (pkg in packages) {
    # Get functions exported by the package
    if (requireNamespace(pkg, quietly = TRUE)) {
      exported_funcs <- getNamespaceExports(pkg)
      
      # Check if any function is used in the script
      for (func in exported_funcs) {
        if (any(grepl(paste0("\\b", func, "\\b"), script))) {
          used_packages <- c(used_packages, pkg)
          break
        }
      }
    }
  }
  
  return(unique(used_packages))
}

# Get used packages
used_packages <- check_package_usage(packages, script)
unused_packages <- setdiff(packages, used_packages)

# Print results
cat("Used packages:\n", paste(used_packages, collapse = ", "), "\n")
cat("Unused packages:\n", paste(unused_packages, collapse = ", "), "\n")