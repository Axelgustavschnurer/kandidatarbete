# TODO: Rename this file

library(ggplot2)
library(dplyr)
source("scripts/utils.R")
source("config.r")

df <- read.csv("data/caloplaca_exsecuta.csv")

# Filter out clades 1, 2, 3
# Do we want to be doing this ?
df <- df[df$clade %in% c(1, 2, 3), ]

pca_data_anova <- df[pca_columns_anova] |>
  convert_boolean_numeric()
pca_data_fishers <- df[pca_columns_fishers] |>
  convert_boolean_numeric()
pca_data_kruskal <- df[pca_columns_kruskal]

# Add the clade column back
# TODO: Does this only remove missing clades?
pca_data_anova$clade <- df$clade |> na.omit()
pca_data_fishers$clade <- df$clade |> na.omit()
pca_data_kruskal$clade <- df$clade |> na.omit()

anova_pvalues <- run_anova(pca_data_anova)
fisher_pvalues <- run_fisher(pca_data_fishers)
kruskal_pvalues <- run_kruskal(pca_data_kruskal)

print_pvalues_table <- function(pvalues, title = "P-values") {

  if (is.list(pvalues)) {
    pvalues <- unlist(pvalues)
  }

  # Sort by p-value
  sorted <- sort(pvalues)

  cat("\n", title, "\n", sep = "")
  cat(strrep("-", 40), "\n")
  cat(sprintf("%-30s %8s\n", "Variable", "P-Value all"))
  cat(strrep("-", 40), "\n")

  for (i in seq_along(sorted)) {
    cat(sprintf("%-30s %8.4f\n", names(sorted)[i], sorted[[i]]))
  }
  cat(strrep("-", 40), "\n")
}

print_pvalues_table(anova_pvalues, "ANOVA P-Values")
print_pvalues_table(fisher_pvalues, "Fisher's Exact Test P-Values")
print_pvalues_table(kruskal_pvalues, "Kruskal-Wallis P-Values")



# 1 vs rest
df$clade_group <- ifelse(df$clade == 1, "Clade1", "Other")
df_filtered <- df[df$clade %in% c(1, 2, 3), ]

run_ttest <- function(data) {
  variables <- intersect(names(data), pca_columns_anova)
  pvalues <- sapply(variables, function(var) {
    t.test(data[[var]] ~ data$clade_group)$p.value
  })
  return(pvalues)
}

pca_data_anova$clade_group <- df$clade_group
ttest_pvalues <- run_ttest(pca_data_anova)

run_fisher <- function(data, group_col = "clade") {
  variables <- setdiff(names(data), c(group_col, "clade"))  # exclude both clade_group and clade

  pvalues <- sapply(variables, function(var) {
    tbl <- table(data[[var]], data[[group_col]])
    fisher.test(tbl)$p.value
  })

  return(pvalues)
}

pca_data_fishers$clade_group <- df$clade_group
fisher_pvalues_bin <- run_fisher(pca_data_fishers, group_col = "clade_group")

run_wilcox <- function(data) {
  variables <- intersect(names(data), pca_columns_kruskal)
  pvalues <- sapply(variables, function(var) {
    wilcox.test(data[[var]] ~ data$clade_group)$p.value
  })
  return(pvalues)
}

pca_data_kruskal$clade_group <- df$clade_group
wilcox_pvalues <- run_wilcox(pca_data_kruskal)

print_pvalues_table(ttest_pvalues, "T-Test P-Values (Clade 1 vs Others)")
print_pvalues_table(fisher_pvalues_bin, "Fisher's Exact Test P-Values (Clade 1 vs Others)")
print_pvalues_table(wilcox_pvalues, "Wilcoxon P-Values (Clade 1 vs Others)")

#2 vs 3
# Filter data to only include clade 2 and clade 3
df_clade_2_3 <- df[df$clade %in% c(2, 3), ]

# Create a clade_group variable for 2 vs 3 comparison
df_clade_2_3$clade_group <- ifelse(df_clade_2_3$clade == 2, "Clade2", "Clade3")

# Subset PCA datasets accordingly
pca_data_anova_2_3 <- df_clade_2_3[pca_columns_anova] |> convert_boolean_numeric()
pca_data_fishers_2_3 <- df_clade_2_3[pca_columns_fishers] |> convert_boolean_numeric()
pca_data_kruskal_2_3 <- df_clade_2_3[pca_columns_kruskal]

# Add clade_group to PCA datasets
pca_data_anova_2_3$clade_group <- df_clade_2_3$clade_group
pca_data_fishers_2_3$clade_group <- df_clade_2_3$clade_group
pca_data_kruskal_2_3$clade_group <- df_clade_2_3$clade_group

# Run t-tests for clade 2 vs clade 3
ttest_pvalues_2_3 <- run_ttest(pca_data_anova_2_3)

# Run Fisher's exact test for clade 2 vs clade 3
fisher_pvalues_bin_2_3 <- run_fisher(pca_data_fishers_2_3, group_col = "clade_group")

# Run Wilcoxon test for clade 2 vs clade 3
wilcox_pvalues_2_3 <- run_wilcox(pca_data_kruskal_2_3)

# Print the p-values tables
print_pvalues_table(ttest_pvalues_2_3, "T-Test P-Values (Clade 2 vs Clade 3)")
print_pvalues_table(fisher_pvalues_bin_2_3, "Fisher's Exact Test P-Values (Clade 2 vs Clade 3)")
print_pvalues_table(wilcox_pvalues_2_3, "Wilcoxon P-Values (Clade 2 vs Clade 3)")
