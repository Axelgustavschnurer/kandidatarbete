# Output directories
directories <- c(
  "output",
  "output/initial_pca",
  "output/hypothesis_tests",
  "output/secondary_pca",
  "output/boxplots"
)

# Codes for shapes representing different clades
shape_codes <- c(
  "1" = 16,
  "2" = 15,
  "3" = 17,
  "Unknown" = 5
)

# ID's of specimens excluded from all tests
blacklisted_id_list <- c("L20483", "L04230", "L22236")

# Possible clade combinations
# TODO: Rename to clade combinations
group_pairs <- list(
  c(1, 2),
  c(1, 3),
  c(2, 3)
)

# Characters used in our inital PCA
initial_pca_columns <- c(
  "hymenium_length", "hypothecium_length", "paraphyse_branch_frequency",
  "paraphyse_top_width_max", "excipulum_cell_width_max",
  "excipulum_cell_length_max", "spore_length_avg", "spore_width_avg",
  "septa_length_avg", "length_over_width", "septa_over_length",
  "diameter_max", "disc_max_convex", "amount", "form_regular",
  "form_aggregated", "proper_exciple_width_max", "proper_exciple_flattened",
  "coherency", "areole_crack_regularity", "areole_thickness_regularity",
  "partially_thin_or_immersed", "thallus_size", "areole_diamater_max",
  "thallus_thickness_max", "colour", "cyanobacteria", "prothallus",
  "hypothecium_pale", "parasitised"
)

# Characters used in our secondary PCA
secondary_pca_columns <- c(
  "spore_width_avg", "septa_over_length", "length_over_width",
  "spore_length_avg", "partially_thin_or_immersed", "areole_diamater_max",
  "areole_crack_regularity", "form_regular", "form_aggregated",
  "amount"
)

# Numerical variables used in ANOVA and t-test
numeric_columns <- c(
  "hymenium_length", "hypothecium_length",
  "paraphyse_top_width_max", "excipulum_cell_width_max",
  "excipulum_cell_length_max", "spore_length_avg", "spore_width_avg",
  "septa_length_avg", "length_over_width", "septa_over_length",
  "diameter_max", "proper_exciple_width_max", "thallus_size",
  "areole_diamater_max", "thallus_thickness_max"
)

# Binary variables used in Fisher’s
binary_columns <- c(
  "form_regular", "form_aggregated", "proper_exciple_flattened",
  "coherency", "partially_thin_or_immersed", "cyanobacteria", "prothallus",
  "hypothecium_pale", "parasitised", "areole_crack_regularity", "colour"
)

# Ordinal variables used in Kruskal-Wallis and Wilcoxon
ordinal_columns <- c(
  "paraphyse_branch_frequency", "disc_max_convex", "amount",
  "areole_thickness_regularity"
)

# Boxplot booleans
boxplot_booleans <- c(
  "partially_thin_or_immersed", "areole_crack_regularity",
  "form_regular", "form_aggregated"
)