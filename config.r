# Codes for shapes representing different clades
shape_codes <- c(
  "1" = 16,
  "2" = 15,
  "3" = 17,
  "Unknown" = 5
)

# Output directories
directories <- c(
  "output",
  "output/initial_pca",
  "output/tests",
  "output/secondary_pca",
  "output/boxplots"
)

# ID's of specimens excluded from all tests
blacklisted_id_list <- c("L20483", "L04230", "L22236")

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

# Characters used in our secondary PCA, TODO: We should the characters here :)
# Form regular instead of hypothecium length might be a contender...
# septa_length_avg seems to do good in place of amount
secondary_pca_columns <- c(
  "septa_over_length", "coherency", "spore_width_avg", "coherency",
  "partially_thin_or_immersed", "hypothecium_length", "length_over_width",
  "form_aggregated", "diameter_max", "spore_length_avg", "amount"
)

# Columns used in varying tests
pca_columns_anova <- c(
  "hymenium_length", "hypothecium_length",
  "paraphyse_top_width_max", "excipulum_cell_width_max",
  "excipulum_cell_length_max", "spore_length_avg", "spore_width_avg",
  "septa_length_avg", "length_over_width", "septa_over_length",
  "diameter_max", "proper_exciple_width_max", "thallus_size",
  "areole_diamater_max", "thallus_thickness_max"
)

pca_columns_fishers <- c(
  "form_regular", "form_aggregated", "proper_exciple_flattened",
  "coherency", "partially_thin_or_immersed", "cyanobacteria", "prothallus",
  "hypothecium_pale", "parasitised", "areole_crack_regularity", "colour"
)

pca_columns_kruskal <- c(
  "paraphyse_branch_frequency", "disc_max_convex", "amount",
  "areole_thickness_regularity"
)
