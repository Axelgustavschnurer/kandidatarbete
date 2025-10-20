source("functions/utils.r")
source("config.r")

generate_directories()

spore_data_raw <- read.csv("data/spores.csv")
spore_data_filtered <- subset(
  spore_data_raw,
  !(ID %in% blacklisted_id_list) & !is.na(clade)
)

width_columns <- c("spore_1_width", "spore_2_width", "spore_3_width",
                   "spore_4_width", "spore_5_width", "spore_6_width",
                   "spore_7_width", "spore_8_width", "spore_9_width",
                   "spore_10_width")

spore_widths <- spore_data_filtered[, c("clade", width_columns)]

spore_widths_long <- data.frame( # TODO: What does "long" even mean
  clade = rep(spore_widths$clade, each = length(width_columns)),
  width = as.vector(t(spore_widths[, width_columns]))
)

png(
  "output/boxplots/jittered_boxplot_spore_width.png",
  width = 800,
  height = 800
)
generate_jittered_boxplot(
  spore_widths_long,
  clade,
  width,
  "Clade",
  "Spore widths",
  "Spore widths by clade"
)
dev.off()

caloplaca_data_raw <- read.csv("data/caloplaca_exsecuta.csv")

caloplaca_data_subset <- subset(
  caloplaca_data_raw,
  !(ID %in% blacklisted_id_list) & !is.na(clade)
)

caloplaca_data_filtered <- caloplaca_data_subset |>
  convert_boolean_numeric() |>
  subset(!is.na(partially_thin_or_immersed))

# Example data
df <- data.frame(
  clade = caloplaca_data_filtered$clade,
  value = caloplaca_data_filtered$partially_thin_or_immersed
)
df
# Save output
png("output/boxplots/barchart.png",
    width = 800, height = 800)

generate_true_false_barchart(
  data = df,
  group_var = clade,
  value = value,
  x_axis_label = "Clade",
  y_axis_label = "Count of Values",
  title = "Presence/Absence of partially thin or immersed thallus by Clade"
)

dev.off()
