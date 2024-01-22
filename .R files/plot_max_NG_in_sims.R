library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)

NG_max_df <- read.csv(file = "NG_capacity_sum_sim_1_to_1000.csv", header = TRUE) 
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


### Plot 1
# Convert to data frame in long format
data_df <- as.data.frame(t(colSums(NG_max_df)[12:21]))
data_df <- tibble::rownames_to_column(data_df, "Category")
data_df <- pivot_longer(data_df, cols = -Category, names_to = "BA_case", values_to = "num_of_sims")
data_df$Category <- rep(c("Not enough capacity without margin", "Not enough capacity with 10% margin"), 5)
data_df$BA <- rep(BAs, each = 2)

# Create the bar plot
ggplot(data_df, aes(x = BA, y = num_of_sims, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  scale_fill_brewer(palette = "Blues") + 
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 200)) +
  theme_minimal() +
  labs(x = element_blank(), y = "# of Simulations", 
       title = "Number of simulations (out of 1000) where current NG fleet lacks capacity", fill = element_blank()) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "bottom", legend.text = element_text(size = 14),
        plot.title = element_text(size = 18))


### Plot 2
# Use this to see  if NG can form a base load
NG_max_df <- read.csv(file = "NG_capacity_sum_sim_1_to_1000.csv", header = TRUE) 
df_sorted <- as.data.frame(lapply(NG_max_df, function(x) sort(x, decreasing = TRUE))) %>% 
  select(ends_with("max"))
df_sorted_long <- pivot_longer(df_sorted, col = ends_with("max"), names_to = "BA_NG_max", values_to = "Maximum_NG_in_Simulation")
df_sorted_long$x <- rep(1:1000, each = 5)
df_sorted_long$BA <- rep(c("CISO", "PJM", "SWPP", "ERCO", "MISO"), 1000)

ggplot(data = df_sorted_long, aes(x = x, y = Maximum_NG_in_Simulation, color = BA)) +
  geom_line(size = 1.1) +
  theme_classic(base_size = 14) +
  scale_y_continuous(limits = c(10000, 1.4e5), breaks = seq(0, 1.4e5, by = 20000)) +
  labs(x = element_blank(), y = "Maximun NG Capacity (MW)", color = element_blank(),
       title = "Simulated Maximum Aggregated Natural Gas Plant Capacity",
       subtitle = "Sorted from High to Low") +
  theme(legend.text = element_text(size = 15), legend.position = "bottom")
