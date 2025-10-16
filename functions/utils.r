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

  return(dataframe) # nolint
}

# Convert missing values in a dataframe to the mean value for that column.
convert_na_mean <- function(dataframe) {
  dataframe[] <- lapply(dataframe, function(column) {
    if (is.numeric(column)) {
      column[is.na(column)] <- mean(column, na.rm = TRUE)
    }

    return(column) # nolint
  })

  return(dataframe) # nolint
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

# Generate biplot based on a PCA result.
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

  # Get scores from our PCA
  plot_data <- as.data.frame(pca_result$x)

  # Extract ID's aswell as DNA_ID's and use them to label graphs
  # TODO: Do not use df here, instead allow passing an argument "labeled_by"
  # as an array of df columns
  id_array <- df$ID
  dna_id_array <- df$DNA
  combined_label <- ifelse(
    is.na(dna_id_array) | dna_id_array == "",
    id_array,
    paste0(id_array, " (", dna_id_array, ")")
  )
  plot_data$label <- combined_label

 
  # Allow showing group (in our clase CLADE)
  # TODO: Might make more sense to just force data to be "group" or "clade"
  # instead of allowing all groupings
  plot_data$group <- factor(ifelse(is.na(group_by), "Unknown", group_by))
  if (grouped_only) {
    plot_data <- plot_data[plot_data$group != "Unknown", ]
  }

  # Extract numeric PC indices from the pc_x and pc_y strings, e.g. "PC2" -> 2
  pc_x_num <- as.numeric(sub("PC", "", pc_x))
  pc_y_num <- as.numeric(sub("PC", "", pc_y))

  # Dynamically get loadings for the two PCs
  loadings <- as.data.frame(pca_result$rotation[, c(pc_x_num, pc_y_num)])
  colnames(loadings) <- c("PCx", "PCy")
  loadings$variable <- rownames(loadings)

  # Scale loadings
  arrow_scale <- 5
  loadings$PCx <- loadings$PCx * pca_result$sdev[pc_x_num] * arrow_scale
  loadings$PCy <- loadings$PCy * pca_result$sdev[pc_y_num] * arrow_scale

  plot <- ggplot()

  plot <- plot + scale_shape_manual(
    values = group_shape_codes,
    name = group_by_title
  )

  # Conditionally add loading arrows
  if (show_loadings) {
    plot <- plot + geom_segment(
      data = loadings,
      aes(x = 0, y = 0, xend = .data$PCx, yend = .data$PCy),
      arrow = arrow(length = unit(0.25, "cm")),
      color = "gray60"
    )
  }

  # Conditionally add loading labels
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

  # Conditionally add point labels
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

  # Add group labels
  plot <- plot + geom_point(
    data = plot_data,
    aes(
      x = .data[[pc_x]],
      y = .data[[pc_y]],
      shape = .data$group
    ),
    size = 3
  )


  # Final plot adjustments
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
      panel.grid.minor = element_line(color = "transparent")
    ) +
    coord_fixed()
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
# For future use all functions should be written to handle this case.
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

# TODO: Probably remove this function :)
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

# TODO: Rename this function
# We use it for statistical tests to remove missing values
# and convert values to booleans if needed
prepare_pca_data <- function(dataframe, columns, convert = TRUE) {
  data <- dataframe[columns]
  if (convert) data <- convert_boolean_numeric(data)
  data$clade <- dataframe$clade
  na.omit(data)
}