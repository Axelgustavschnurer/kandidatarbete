source("functions/utils.r")

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

spore_widths_long <- data.frame(
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
