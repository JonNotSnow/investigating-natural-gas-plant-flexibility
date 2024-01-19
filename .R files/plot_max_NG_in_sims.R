library(dplyr)
library(ggplot2)
library(corrplot)

NG_max_df <- read.csv(file = "NG_capacity_sum_each_sim.csv", header = TRUE) 
head(NG_max_df)
BAs <- c("CISO", "ERCO", "MISO", "PJM", "SWPP")

margin <- 0.1
for (BA in BAs) {
  col1 <- paste0(BA, "_infea")
  col2 <- paste0(BA, "_infea_w_margin")
  
  sum_col <- paste0(BA, "_sum")
  max_col <- paste0(BA, "_NG_max")
  
  NG_max_df[, col1] <- ifelse(NG_max_df[, sum_col] < NG_max_df[, max_col], 1, 0)
  NG_max_df[, col2] <- ifelse(NG_max_df[, sum_col] < NG_max_df[, max_col] * (1+margin), 1, 0)
}

# sum(NG_max_df$CISO_infea)
# sum(NG_max_df$CISO_infea_w_margin)

CISO_sim_inputs <- read.csv(file = "C:/Users/Achaochao/Desktop/CISO_Results_2024-01-12/CISO_inputs.csv", header = TRUE)
infea_inputs <- CISO_sim_inputs[which(NG_max_df$CISO_infea == 1), ]
fea_inputs <- CISO_sim_inputs[which(NG_max_df$CISO_infea == 0), ]
corrplot(cor(infea_inputs), method = 'square', order = 'alphabet')
summary(infea_inputs)
summary(fea_inputs)



