# ANOVA
# Student’s t-test
# Fisher’s exact test,
# Kruskal-Wallis test
# Wilcoxon signed-rank test

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
data_fisher <- prepare_pca_data(
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
anova_pvalues <- run_anova(data_anova)
fisher_pvalues <- run_fisher(data_fisher)
kruskal_pvalues <- run_kruskal(data_kruskal)


# ------------------------------------------------------------------------------
#
# Tests running one vs one (pairwise)
#
# ------------------------------------------------------------------------------

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
  data_fishers <- prepare_pca_data(
    dataframe = pair_data,
    columns = binary_columns,
    convert = TRUE
  )
  data_wilcox <- prepare_pca_data(
    dataframe = pair_data,
    columns = ordinal_columns,
    convert = FALSE
  )

  # Run tests
  pairwise_results[[pair_name]] <- list(
    ttest = run_ttest(data_ttest, group_col = "clade"),
    fisher = run_fisher(data_fishers, group_col = "clade"),
    wilcox = run_wilcox(data_wilcox, group_col = "clade")
  )
}

# ------------------------------------------------------------------------------
#
# Tests running one vs rest
#
# ------------------------------------------------------------------------------


one_vs_rest_results <- list()
all_clades <- unique(caloplaca_data_subset$clade)

for (current_clade in all_clades) {
  one_vs_rest_data <- caloplaca_data_subset %>%
    mutate(group_vs_rest = ifelse(clade == current_clade,
                                  paste0("clade_", current_clade),
                                  "rest"))

  # Keep group_vs_rest during prepare_pca_data
  data_ttest <- prepare_pca_data(one_vs_rest_data,
                                 columns = c(numeric_columns, "group_vs_rest"),
                                 convert = TRUE)
  data_ttest$clade <- NULL # Remove clade column so it’s not tested

  data_fishers <- prepare_pca_data(one_vs_rest_data,
                                   columns = c(binary_columns, "group_vs_rest"),
                                   convert = TRUE)
  data_fishers$clade <- NULL

  data_wilcox <- prepare_pca_data(one_vs_rest_data,
                                  columns = c(ordinal_columns, "group_vs_rest"),
                                  convert = FALSE)
  data_wilcox$clade <- NULL

  # Run tests
  one_vs_rest_results[[paste0("clade_", current_clade, "_vs_rest")]] <- list(
    ttest = run_ttest(data_ttest, group_col = "group_vs_rest"),
    fisher = run_fisher(data_fishers, group_col = "group_vs_rest"),
    wilcox = run_wilcox(data_wilcox, group_col = "group_vs_rest")
  )
}
one_vs_rest_results

# ------------------------------------------------------------------------------
#
# Saving results
#
# ------------------------------------------------------------------------------

# Format and save results 
# All vs all results
all_vs_all_df <- data.frame(
  Variable = c(names(anova_pvalues), names(fisher_pvalues), names(kruskal_pvalues)),
  Test = c(
    rep("ANOVA", length(anova_pvalues)),
    rep("Fisher", length(fisher_pvalues)),
    rep("Kruskal-Wallis", length(kruskal_pvalues))
  ),
  P_value = c(unlist(anova_pvalues), unlist(fisher_pvalues), unlist(kruskal_pvalues))
)

all_vs_all_df <- all_vs_all_df[order(all_vs_all_df$P_value), ]

all_vs_all_df$Variable <- format(all_vs_all_df$Variable, width = 25, justify = "left")
all_vs_all_df$Test <- format(all_vs_all_df$Test, width = 15, justify = "left")
all_vs_all_df$P_value <- format(all_vs_all_df$P_value, width = 8, justify = "right")

# Save to text file
write.table(
  all_vs_all_df,
  file = file.path("output/hypothesis_tests", "all_vs_all_results.txt"),
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

# Pairwise results
pairwise_list <- list()
for (pair_name in names(pairwise_results)) {
  tests <- pairwise_results[[pair_name]]

  for (test_name in names(tests)) {
    df <- data.frame(
      Variable = names(tests[[test_name]]),
      Test = test_name,
      P_value = unlist(tests[[test_name]]),
      Pair = pair_name
    )
    pairwise_list[[paste(pair_name, test_name, sep = "_")]] <- df
  }
}

# Combine all pairwise results into one data frame
pairwise_df <- do.call(rbind, pairwise_list)

# Sort by ascending P-value
pairwise_df <- pairwise_df[order(pairwise_df$P_value), ]

# Format columns
pairwise_df$Variable <- format(pairwise_df$Variable, width = 25, justify = "left")
pairwise_df$Test <- format(pairwise_df$Test, width = 15, justify = "left")
pairwise_df$Pair <- format(pairwise_df$Pair, width = 15, justify = "left")
pairwise_df$P_value <- format(pairwise_df$P_value, width = 8, justify = "right")

# Save to text file
write.table(
  pairwise_df,
  file = file.path("output/hypothesis_tests", "pairwise_results.txt"),
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)

# One vs rest
one_vs_rest_list <- list()

for (comparison_name in names(one_vs_rest_results)) {
  tests <- one_vs_rest_results[[comparison_name]]
  
  for (test_name in names(tests)) {
    df <- data.frame(
      Variable = names(tests[[test_name]]),
      Test = test_name,
      P_value = unlist(tests[[test_name]]),
      Comparison = comparison_name
    )
    one_vs_rest_list[[paste(comparison_name, test_name, sep = "_")]] <- df
  }
}

# Combine all 1 vs rest results into one data frame
one_vs_rest_df <- do.call(rbind, one_vs_rest_list)

# Sort by ascending P-value
one_vs_rest_df <- one_vs_rest_df[order(one_vs_rest_df$P_value), ]

# Format columns
one_vs_rest_df$Variable <- format(one_vs_rest_df$Variable, width = 25, justify = "left")
one_vs_rest_df$Test <- format(one_vs_rest_df$Test, width = 15, justify = "left")
one_vs_rest_df$Comparison <- format(one_vs_rest_df$Comparison, width = 25, justify = "left")
one_vs_rest_df$P_value <- format(one_vs_rest_df$P_value, width = 8, justify = "right")

# Format P-values as decimals (e.g., 6 digits after decimal)
one_vs_rest_df$P_value <- sprintf("%.6f", as.numeric(one_vs_rest_df$P_value))

# Save to text file
write.table(
  one_vs_rest_df,
  file = file.path("output/hypothesis_tests", "one_vs_rest_results.txt"),
  sep = "\t",
  row.names = FALSE,
  quote = FALSE
)
