# TODO: Rename this file
# ANOVA
# Student’s t-test
# Fisher’s exact test,
# Kruskal-Wallis test
# Wilcoxon signed-rank test
# “We used a combination of parametric, non-parametric, and categorical statistical tests to analyze the data.”

library(ggplot2)
library(dplyr)
source("functions/utils.r")
source("config.r")

# Prepare data by removing blacklisted specimens and specimens without a clade.
caloplaca_data_raw <- read.csv("data/caloplaca_exsecuta.csv")

# ------------------------------------------------------------------------------
#
# Tests running on all vs all
#
# ------------------------------------------------------------------------------

caloplaca_data_subset <- caloplaca_data_raw |>
  subset(!(ID %in% blacklisted_id_list)) |>
  subset(clade %in% c(1, 2, 3))

# Convert booleans and remove missing values for a specific set of columns.
data_anova <- prepare_pca_data(
  dataframe = caloplaca_data_subset,
  columns = numeric_columns,
  convert = TRUE
)
data_fishers <- prepare_pca_data(
  dataframe = caloplaca_data_subset,
  columns = binary_columns,
  convert = TRUE
)
data_kruskal <- prepare_pca_data(
  dataframe = caloplaca_data_subset,
  columns = ordinal_columns,
  convert = FALSE
)

# Run tests
anova_pvalues <- run_anova(pca_data_anova)
fisher_pvalues <- run_fisher(pca_data_fishers)
kruskal_pvalues <- run_kruskal(pca_data_kruskal)

# ------------------------------------------------------------------------------
#
# Tests running one vs one
#
# ------------------------------------------------------------------------------

# Run tests on the different clades against each other
pairwise_results <- list()
for (pair in group_pairs) {
  # Subset only the two groups
  pair_data <- caloplaca_data_subset %>%
    subset(clade %in% pair)

  pair_name <- paste0("clade_", pair[1], "_vs_", pair[2]) # Formatting purposes

  # Convert booleans and remove missing values for a specific set of columns.
  data_ttest <- prepare_pca_data(
    dataframe = pair_data,
    columns = numeric_columns,
    convert = TRUE
  )
  data_wilcox <- prepare_pca_data(
    dataframe = pair_data,
    columns = ordinal_columns,
    convert = FALSE
  )
  data_fisher <- prepare_pca_data(
    dataframe = pair_data,
    columns = binary_columns,
    convert = TRUE
  )

  # Run tests
  pairwise_results[[pair_name]] <- list(
    ttest = run_ttest(data_ttest, group_col = "clade"),
    wilcox = run_wilcox(data_wilcox, group_col = "clade"),
    fisher = run_fisher(data_fisher, group_col = "clade")
  )
}

# ------------------------------------------------------------------------------
#
# Tests running one vs rest
#
# ------------------------------------------------------------------------------

 