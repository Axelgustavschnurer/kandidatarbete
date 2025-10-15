# ID's of specimens excluded from all tests
blacklisted_id_list <- c("L20483", "L04230", "L22236")

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