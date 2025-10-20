source("functions/utils.r")
source("config.r")

generate_directories()

#--------------------------------------
# Generate boxplots from spore data
#--------------------------------------

spore_data_raw <- read.csv("data/spores.csv")
spore_data_filtered <- subset(
  spore_data_raw,
  !(ID %in% blacklisted_id_list) & !is.na(clade)
)

width_columns <- c("spore_1_width", "spore_2_width", "spore_3_width",
                   "spore_4_width", "spore_5_width", "spore_6_width",
                   "spore_7_width", "spore_8_width", "spore_9_width",
                   "spore_10_width")
length_columns <- c("spore_1_length", "spore_2_length", "spore_3_length",
                    "spore_4_length", "spore_5_length", "spore_6_length",
                    "spore_7_length", "spore_8_length", "spore_9_length",
                    "spore_10_length")

spore_widths <- spore_data_filtered[, c("clade", width_columns)]
spore_lengths <- spore_data_filtered[, c("clade", length_columns)]

spore_widths_long <- data.frame( # TODO: What does "long" even mean
  clade = rep(spore_widths$clade, each = length(width_columns)),
  width = as.vector(t(spore_widths[, width_columns]))
)
spore_lengths_long <- data.frame( # TODO: What does "long" even mean
  clade = rep(spore_lengths$clade, each = length(length_columns)),
  width = as.vector(t(spore_lengths[, length_columns]))
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
png(
  "output/boxplots/jittered_boxplot_spore_length.png",
  width = 800,
  height = 800
)
generate_jittered_boxplot(
  spore_lengths_long,
  clade,
  width,
  "Clade",
  "Spore lengths",
  "Spore lengths by clade"
)
dev.off()


caloplaca_data_raw <- read.csv("data/caloplaca_exsecuta.csv")

caloplaca_data_subset <- subset(
  caloplaca_data_raw,
  !(ID %in% blacklisted_id_list) & !is.na(clade)
)

png(
  "output/boxplots/jittered_boxplot_amount.png",
  width = 800,
  height = 800
)
generate_jittered_boxplot(
  caloplaca_data_subset,
  clade,
  amount,
  "Clade",
  "Amount",
  "Amount of apothecia by clade"
)
dev.off()
png(
  "output/boxplots/jittered_boxplot_max_areole_diamater.png",
  width = 800,
  height = 800
)
generate_jittered_boxplot(
  caloplaca_data_subset,
  clade,
  areole_diamater_max,
  "Clade",
  "Max areole diameter",
  "Max areole diameter by clade"
)
dev.off()

#--------------------------------------
# Generate barcharts from caloplaca data
#--------------------------------------

caloplaca_data_raw <- read.csv("data/caloplaca_exsecuta.csv")

caloplaca_data_subset <- subset(
  caloplaca_data_raw,
  !(ID %in% blacklisted_id_list) & !is.na(clade)
)

caloplaca_data_filtered <- caloplaca_data_subset |>
  convert_boolean_numeric()

# Example data
for (col_name in boxplot_booleans) {

  # Remove rows where current column is NA
  df <- caloplaca_data_filtered[!is.na(caloplaca_data_filtered[[col_name]]), ]

  # Create a small dataframe with clade + current column
  plot_df <- data.frame(
    clade = df$clade,
    value = df[[col_name]]
  )

  # Construct output filename
  filename <- paste0("output/boxplots/barchart_", col_name, ".png")

  # Save plot
  png(filename, width = 800, height = 800)
  generate_true_false_barchart(
    data = plot_df,
    group_var = clade,
    value = value,
    x_axis_label = "Clade",
    y_axis_label = "Count",
    title = paste("Presence/Absence of", col_name, "by Clade")
  )
  dev.off()
}
