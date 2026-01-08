# Various functions used throughout other files

library("ggplot2")
source("config.r")

# Generate output directories
generate_directories <- function() {
  for (directory in directories) {
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
      message("Created directory: ", directory)
    }
  }
}

# Convert boolean values in a dataframe to numeric values.
convert_boolean_numeric <- function(dataframe) {
  dataframe[] <- lapply(dataframe, function(column) {
    if (is.logical(column)) as.numeric(column) else column
  })

  return(dataframe)
}

# Convert missing values in a dataframe to the mean value for that column.
convert_na_mean <- function(dataframe) {
  dataframe[] <- lapply(dataframe, function(column) {
    if (is.numeric(column)) {
      column[is.na(column)] <- mean(column, na.rm = TRUE)
    }

    return(column)
  })

  return(dataframe)
}

# Generates a boxplot with jittered points
generate_jittered_boxplot <- function(
  data,
  x_axis,
  y_axis,
  x_axis_label,
  y_axis_label,
  title
) {
  formula <- as.formula(paste(
    deparse(substitute(y_axis)),
    "~",
    deparse(substitute(x_axis))
  ))

  boxplot(
    formula,
    data = data,
    main = title,
    xlab = x_axis_label,
    ylab = y_axis_label,
    border = "gray",
    col = "#ebebeb"
  )

  stripchart(
    formula,
    data = data,
    vertical = TRUE,
    method = "jitter",
    pch = 16,
    col = rgb(0, 0, 0),
    add = TRUE
  )
}

# Generates a barchart plot for different clades
generate_true_false_barchart <- function(
  data,
  group_var,
  value_var,
  x_axis_label,
  y_axis_label,
  title
) {
  group_name <- deparse(substitute(group_var))
  value_name <- deparse(substitute(value_var))

  true_counts <- tapply(data[[value_name]] == 1, data[[group_name]], sum, na.rm = TRUE)
  false_counts <- tapply(data[[value_name]] == 0, data[[group_name]], sum, na.rm = TRUE)

  counts <- rbind(`True` = true_counts, `False` = false_counts)

  # Generate barplot
  barplot(
    counts,
    beside = TRUE,
    col = c("#4CAF50", "#F44336"),
    border = "gray30",
    main = title,
    xlab = x_axis_label,
    ylab = y_axis_label,
    legend.text = TRUE,
    args.legend = list(x = "topright", bty = "n")
  )
}

# Generates a biplot based on PCA results
generate_biplot <- function(
  title,
  df,
  pca_result,
  pc_x,
  pc_y,
  group_by,
  group_by_title,
  group_shape_codes,
  grouped_only = FALSE,
  show_labels = TRUE,
  show_loadings = TRUE,
  show_loading_labels = TRUE
) {

  # Get scores from PCA
  plot_data <- as.data.frame(pca_result$x)

  # Create labels
  id_array <- df$ID
  dna_id_array <- df$DNA
  combined_label <- ifelse(
    is.na(dna_id_array) | dna_id_array == "",
    id_array,
    paste0(id_array, " (", dna_id_array, ")")
  )
  plot_data$label <- combined_label

  # Grouping
  plot_data$group <- factor(ifelse(is.na(group_by), "Unknown", group_by))
  if (grouped_only) {
    plot_data <- plot_data[plot_data$group != "Unknown", ]
  }

  # Extract numeric PC indices
  pc_x_num <- as.numeric(sub("PC", "", pc_x))
  pc_y_num <- as.numeric(sub("PC", "", pc_y))

  # Loadings
  loadings <- as.data.frame(pca_result$rotation[, c(pc_x_num, pc_y_num)])
  colnames(loadings) <- c("PCx", "PCy")
  loadings$variable <- rownames(loadings)

  # Scale loadings
  arrow_scale <- 4
  loadings$PCx <- loadings$PCx * pca_result$sdev[pc_x_num] * arrow_scale
  loadings$PCy <- loadings$PCy * pca_result$sdev[pc_y_num] * arrow_scale

  plot <- ggplot() +
    scale_shape_manual(
      values = group_shape_codes,
      name = group_by_title
    )

  # Add loading arrows
  if (show_loadings) {
    plot <- plot + geom_segment(
      data = loadings,
      aes(x = 0, y = 0, xend = .data$PCx, yend = .data$PCy),
      arrow = arrow(length = unit(0.25, "cm")),
      color = "gray60"
    )
  }

  # Add loading labels
  if (show_loading_labels) {
    plot <- plot + geom_text(
      data = loadings,
      aes(
        x = .data$PCx * 1.1,
        y = .data$PCy * 1.1,
        label = .data$variable
      ),
      color = "gray20",
      size = 2
    )
  }

  # Add point labels
  if (show_labels) {
    plot <- plot + geom_text(
      data = plot_data,
      aes(
        x = .data[[pc_x]],
        y = .data[[pc_y]],
        label = .data$label
      ),
      vjust = -1.2,
      size = 2
    )
  }

  # Add points
  plot <- plot + geom_point(
    data = plot_data,
    aes(
      x = .data[[pc_x]],
      y = .data[[pc_y]],
      shape = .data$group
    ),
    size = 3
  )

  # Center at 0,0
  x_range <- range(plot_data[[pc_x]], 0)
  y_range <- range(plot_data[[pc_y]], 0)
  x_max <- max(abs(x_range))
  y_max <- max(abs(y_range))

  # Add margins
  margin_abs <- 0  
  x_lim_expanded <- c(-x_max - margin_abs, x_max + margin_abs)
  y_lim_expanded <- c(-y_max - margin_abs, y_max + margin_abs)

  # Final plot
  plot +
    ggtitle(title) +
    xlab(paste0(
      pc_x,
      " (",
      round(100 * summary(pca_result)$importance[2, pc_x_num], 1),
      "%)"
    )) +
    ylab(paste0(
      pc_y,
      " (",
      round(100 * summary(pca_result)$importance[2, pc_y_num], 1),
      "%)"
    )) +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "transparent"),
      plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 24)),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      panel.border = element_rect(color = "gray70", fill = NA, linewidth = 1)
    ) +
    coord_fixed(xlim = x_lim_expanded, ylim = y_lim_expanded)
}

# Calculate contribution of different variables
# within a given set of principal components.
generate_pca_contributions <- function(
  pca_result,
  principal_components
) {
  # Extract specified principal components
  loadings <- as.data.frame(
    pca_result$rotation[, principal_components, drop = FALSE]
  )

  # Square loadings
  squared_loadings <- loadings^2

  # Calculate percentage contributions
  contrib_percent <- sweep(
    squared_loadings,
    2,
    colSums(squared_loadings),
    FUN = "/"
  ) * 100

  # Add variable names
  contrib_percent$variable <- rownames(contrib_percent)

  # Calculate total contribution across selected components
  contrib_percent$total <- rowSums(
    contrib_percent[, principal_components, drop = FALSE]
  )

  # Sort by total descending
  contrib_percent[
    order(-contrib_percent$total),
    c(principal_components, "total")
  ]
}

# Tests
run_anova <- function(data, group_col = "clade") {
  cols_to_test <- setdiff(names(data), group_col)

  res <- lapply(cols_to_test, function(col) {
    formula <- as.formula(paste(col, "~", group_col))
    model <- aov(formula, data = data)
    summary(model)[[1]][["Pr(>F)"]][1]  # extract p-value
  })

  names(res) <- cols_to_test
  return(res)
}

# Only the fisher function deals with invalid values.
# This is because it was the only relevant place to deal with them
# for this project.
run_fisher <- function(data, group_col = "clade") {
  cols_to_test <- setdiff(names(data), group_col)

  res <- lapply(cols_to_test, function(col) {
    tab <- table(data[[col]], data[[group_col]])

    # Check if the table has at least 2 rows and 2 columns
    if (all(dim(tab) >= 2)) {
      fisher.test(tab)$p.value
    } else {
      NA  # not enough data to run test
    }
  })

  names(res) <- cols_to_test
  return(res)
}

run_kruskal <- function(data, group_col = "clade") {
  cols_to_test <- setdiff(names(data), group_col)

  res <- lapply(cols_to_test, function(col) {
    formula <- as.formula(paste(col, "~", group_col))
    kruskal.test(formula, data = data)$p.value
  })

  names(res) <- cols_to_test
  return(res)
}

run_ttest <- function(data, group_col = "clade") {
  cols_to_test <- setdiff(names(data), group_col)

  res <- lapply(cols_to_test, function(col) {
    formula <- as.formula(paste(col, "~", group_col))
    t.test(formula, data = data)$p.value
  })

  names(res) <- cols_to_test
  return(res)
}

run_wilcox <- function(data, group_col = "clade") {
  cols_to_test <- setdiff(names(data), group_col)

  res <- lapply(cols_to_test, function(col) {
    formula <- as.formula(paste(col, "~", group_col))
    wilcox.test(formula, data = data)$p.value
  })

  names(res) <- cols_to_test
  return(res)
}


# TODO: Rename this function (It is currently a bit
# missleading as it has nothing to do with a PCA).
# We use it for statistical tests to remove missing values
# and convert values to booleans if needed
prepare_pca_data <- function(dataframe, columns, convert = TRUE) {
  data <- dataframe[columns]
  if (convert) data <- convert_boolean_numeric(data)
  data$clade <- dataframe$clade
  na.omit(data)
}