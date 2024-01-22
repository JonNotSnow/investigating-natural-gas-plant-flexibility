# functions.R

# Function to filter plants data based on inputs
filter_plants <- function(data, A_range, B_range, C_range, D_range) {
  data %>%
    filter(
      A >= A_range[1] & A <= A_range[2],
      B >= B_range[1] & B <= B_range[2],
      C >= C_range[1] & C <= C_range[2],
      D >= D_range[1] & D <= D_range[2]
    )
}

# Function to remove outliers
removeOutliers <- function(df, variable, iqrMultiplier) {
  Q1 <- quantile(df[[variable]], 0.25)
  Q3 <- quantile(df[[variable]], 0.75)
  IQR <- Q3 - Q1
  lowerBound <- Q1 - iqrMultiplier * IQR
  upperBound <- Q3 + iqrMultiplier * IQR
  df[df[[variable]] >= lowerBound & df[[variable]] <= upperBound, ]
}
