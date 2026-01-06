# 1. Filter data
# 2. Run intial PCA (Save graph to /output/intial_pca)
# 3. Run parallel analysis (Save graph to /output/intial_pca)
# 4. Generate output (Save text to /output/intial_pca)

library(psych)
source("functions/utils.r")
source("config.r")

generate_directories()

caloplaca_data_raw <- read.csv("data/caloplaca_exsecuta.csv")

caloplaca_data_subset <- caloplaca_data_raw |>
  subset(!(ID %in% blacklisted_id_list))

caloplaca_data_filtered <- caloplaca_data_subset |>
  (\(df) df[initial_pca_columns])() |>
  convert_boolean_numeric() |>
  convert_na_mean()

png("output/initial_pca/parallel_analysis_scree.png", width = 8000, height = 6000, res = 900)
fa.parallel(
  caloplaca_data_filtered,
  fa = "pc",
  n.iter = 100,
  show.legend = TRUE,
  main = "Parallel analysis scree plot",
)
dev.off()

pca_result <- prcomp(caloplaca_data_filtered, center = TRUE, scale. = TRUE)

png("output/initial_pca/pca_biplot.png", width = 8000, height = 8000, res = 900)
generate_biplot(
  title = "Initial PCA - PC1 against PC3",
  df = caloplaca_data_subset,
  pca_result = pca_result,
  pc_x = "PC1",
  pc_y = "PC3",
  group_by_title = "Clade",
  group_by = caloplaca_data_subset$clade,
  group_shape_codes = shape_codes,
  grouped_only = FALSE,
  show_labels = FALSE,
  show_loadings = FALSE,
  show_loading_labels = FALSE
)
dev.off()

contrib_output <- generate_pca_contributions(pca_result, c("PC1", "PC2", "PC3"))
capture.output(
  contrib_output,
  file = "output/initial_pca/pca_contributions.txt"
)
