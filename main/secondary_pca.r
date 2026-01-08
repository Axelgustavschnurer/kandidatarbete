# 1. Filter data
# 2. Run intial PCA (Save graph to /output/secondary_pca)
# 3. Run parallel analysis (Save graph to /output/secondary_pca)
# 4. Generate output (Save text to /output/secondary_pca)

library(psych)
source("functions/utils.r")
source("config.r")

generate_directories()

caloplaca_data_raw <- read.csv("data/caloplaca_exsecuta.csv")

caloplaca_data_subset <- caloplaca_data_raw |>
  subset(!(ID %in% blacklisted_id_list))

caloplaca_data_filtered <- caloplaca_data_subset |>
  (\(df) df[secondary_pca_columns])() |>
  convert_boolean_numeric() |>
  convert_na_mean()

pca_result <- prcomp(caloplaca_data_filtered, center = TRUE, scale. = TRUE)

png("output/secondary_pca/pca_biplot.png",  width = 8000, height = 8000, res = 900)
generate_biplot(
  title = "Secondary PCA - PC1 against PC2",
  df = caloplaca_data_subset,
  pca_result = pca_result,
  pc_x = "PC1",
  pc_y = "PC2",
  group_by_title = "Clade",
  group_by = caloplaca_data_subset$clade,
  group_shape_codes = shape_codes,
  grouped_only = FALSE,
  show_labels = FALSE,
  show_loadings = TRUE,
  show_loading_labels = TRUE
)
dev.off()

contrib_output <- generate_pca_contributions(pca_result, c("PC1", "PC2"))
capture.output(
  contrib_output,
  file = "output/secondary_pca/pca_contributions.txt"
)
