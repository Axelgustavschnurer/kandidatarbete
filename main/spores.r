source("functions/utils.r")
source("config.r")
library(ggplot2)

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
length_columns <- c("spore_1_length", "spore_2_length", "spore_3_length",
                    "spore_4_length", "spore_5_length", "spore_6_length",
                    "spore_7_length", "spore_8_length", "spore_9_length",
                    "spore_10_length")

spore_widths <- spore_data_filtered[, c("clade", width_columns)]
spore_lengths <- spore_data_filtered[, c("clade", length_columns)]

spores_long <- data.frame(
  width = numeric(),
  length = numeric(),
  clade = character(),
  stringsAsFactors = FALSE
)

# Loop through spore 1–10 and extract width/length pairs
for (i in 1:10) {
  w_col <- paste0("spore_", i, "_width")
  l_col <- paste0("spore_", i, "_length")

  tmp <- data.frame(
    width = spore_data_filtered[[w_col]],
    length = spore_data_filtered[[l_col]],
    clade = spore_data_filtered$clade,
    stringsAsFactors = FALSE
  )

  spores_long <- rbind(spores_long, tmp)
}

spores_long$clade <- as.character(spores_long$clade)
spores_long$clade[is.na(spores_long$clade)] <- "Unknown"
spores_long <- spores_long[spores_long$clade != "Unknown", ]
unique(spores_long$clade)

# Now plot width vs length
ggplot(spores_long, aes(x = length, y = width, shape = clade)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_shape_manual(values = shape_codes) +
  labs(
    x = "Spore length",
    y = "Spore width",
    title = "Spore Length vs Width"
  ) +
  theme_minimal() +
  coord_fixed(ratio = 1)
