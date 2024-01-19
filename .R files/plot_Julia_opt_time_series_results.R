library(ggplot2)
library(dplyr)
library(tidyr)

# op_by_hour <- read.csv('Julia Codes/Gurobi_test_results.csv', header = T)
# 
# op_by_hour <- op_by_hour %>%
#   mutate(Power = if_else(Type == "Renewable Curtailment", -Power, Power)) %>%
#   mutate(Power = if_else(Type == "Battery Charge", -Power, Power))
# 
# demands  <- read.csv('Julia Codes/Gurobi_test_demands.csv', header = T)
# demands$Net_Demand <- demands$Demand - demands$Renewable_Total
# 
# ggplot() +
#   geom_bar(data = op_by_hour, aes(x = Time, y = Power, fill = Type), stat = "identity") +
#   theme_minimal() +
#   labs(y = "Power (MW)", x = "Hour", fill = "Type") +
#   geom_point(data = demands, aes(x = Time, y = Demand, color = "Demand"), size = 2) +
#   geom_line(data = demands, aes(x = Time, y = Demand, color = "Demand")) +
#   geom_point(data = demands, aes(x = Time, y = Renewable_Total, color = "Total RE"), size = 2) +
#   geom_line(data = demands, aes(x = Time, y = Renewable_Total, color = "Total RE")) +
#   geom_point(data = demands, aes(x = Time, y = State_of_Charge, color = "State of Charge"), size = 2) +
#   geom_line(data = demands, aes(x = Time, y = State_of_Charge, color = "State of Charge")) +
#   scale_color_manual(values = c("Demand" = "blue", "Total RE" = "red", "State of Charge" = "black"),
#                      name = "Time Series",
#                      breaks = c("Demand", "Total RE", "State of Charge"),
#                      labels = c("Demand", "Total RE", "State of Charge")) +
#   ggtitle("Test Results")

             


# ERCO results, as example
myBA <- "SWPP"

myPath <- paste0('../Julia Codes/Single_Sim_Test_Results/', myBA, '_test_results.csv')
op_by_hour <- read.csv(myPath, header = T)

op_by_hour <- op_by_hour %>% 
  mutate(Power = if_else(Type == "Solar_Curtailment", -Power, Power)) %>%
  mutate(Power = if_else(Type == "Wind_Curtailment", -Power, Power)) %>%
  mutate(Power = if_else(Type == "Renewable_Curtailment", -Power, Power)) %>% 
  mutate(Power = if_else(Type == "Battery_Charge", -Power, Power)) %>% 
  mutate(Power = if_else(Type == "Effective_Battery_Charge", -Power, Power))
op_by_hour_reshaped <- pivot_wider(op_by_hour, names_from = Type, values_from = Power)
glimpse(op_by_hour_reshaped)

myPath <- paste0('../Julia Codes/Single_Sim_Test_Results/', myBA, '_test_demands.csv')
demands  <- read.csv(myPath, header = T)

# Combine into big df
big_df <- merge(op_by_hour_reshaped, demands, by = c("Time"))
sum(big_df$Solar_Curtailment)
sum(big_df$Wind_Curtailment)
# which(big_df$`Battery Charge` * 0.9 != big_df$`Effective Battery Charge`)
# which((big_df$`Battery Charge` * 0.9 - big_df$`Effective Battery Charge`) < -0.1)
# which(big_df$`Battery Discharge` / 0.9 != big_df$`Effective Battery Discharge`)
# which((big_df$`Effective Battery Discharge` * 0.9 - big_df$`Battery Discharge`) > 0.1)

# Slice the data
k <- 300
k_plus <- 5
T_range <- c((24*k+1) : (24*(k+k_plus)))
# T_range <- c(8675:8685)
op_by_hour_plot <- op_by_hour %>% 
  filter(Time %in% T_range) %>%
  filter(!(Type %in% c('Effective_Battery_Charge', 'Effective_Battery_Discharge')))
demands_plot <- demands[T_range, ]

# First graph: Hourly operation
ggplot() +
  geom_bar(data = op_by_hour_plot, aes(x = Time, y = Power, fill = Type), stat = "identity") +
  theme_minimal() +
  labs(y = "Power (MW)", x = "Hour", fill = "Type") +
  geom_point(data = demands_plot, aes(x = Time, y = Demand, color = "Demand"), size = 2) +
  geom_line(data = demands_plot, aes(x = Time, y = Demand, color = "Demand"), linewidth = 1) +
  geom_point(data = demands_plot, aes(x = Time, y = Renewable_Total, color = "Total RE Produced"), size = 2) +
  geom_line(data = demands_plot, aes(x = Time, y = Renewable_Total, color = "Total RE Produced"), linewidth = 1) +
  geom_point(data = demands_plot, aes(x = Time, y = State_of_Charge, color = "State of Charge"), size = 2) +
  geom_line(data = demands_plot, aes(x = Time, y = State_of_Charge, color = "State of Charge"), linewidth = 1) +
  scale_color_manual(values = c("Demand" = "red", "Total RE Produced" = "blue", "State of Charge" = "darkslateblue"),
                     name = "Time Series",
                     breaks = c("Demand", "Total RE Produced", "State of Charge"),
                     labels = c("Demand", "Total RE Produced", "State of Charge")) +
  ggtitle(paste0(myBA, " Balacing Authority Results for Hour ", T_range[1], "-", T_range[length(T_range)])) +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(n.breaks=10) + scale_y_continuous(n.breaks=10)


# ggplot() +
#   geom_bar(data = op_by_hour_plot, aes(x = Time, y = Power, fill = Type), stat = "identity") +
#   theme_minimal() +
#   labs(y = "Power (MW)", x = "Hour", fill = "Type") +
#   geom_step(data = demands_plot, aes(x = Time, y = Demand, color = "Demand"), size = 1) +
#   # geom_line(data = demands_plot, aes(x = Time, y = Demand, color = "Demand"), linewidth = 1) +
#   geom_step(data = demands_plot, aes(x = Time, y = Renewable_Total, color = "Total RE Produced"), size = 1) +
#   # geom_line(data = demands_plot, aes(x = Time, y = Renewable_Total, color = "Total RE Produced"), linewidth = 1) +
#   geom_step(data = demands_plot, aes(x = Time, y = State_of_Charge, color = "State of Charge"), size = 1) +
#   # geom_line(data = demands_plot, aes(x = Time, y = State_of_Charge, color = "State of Charge"), linewidth = 1) +
#   scale_color_manual(values = c("Demand" = "red", "Total RE Produced" = "blue", "State of Charge" = "darkslateblue"),
#                      name = "Time Series",
#                      breaks = c("Demand", "Total RE Produced", "State of Charge"),
#                      labels = c("Demand", "Total RE Produced", "State of Charge")) +
#   ggtitle(paste0(myBA, " Balacing Authority Results for Hour ", T_range[1], "-", T_range[length(T_range)]))


# Second graph: Check residual demand
demand_resi <- demands$Demand -                 # This one to check when NG is used to charge battery ahead of peak
  op_by_hour_reshaped$Solar_Generation -
  op_by_hour_reshaped$Wind_Generation
plot(T_range, demands$Demand[T_range], type = "s", ylim = c(0, ceiling(max(demands_plot$Demand)/10000)*10000+15000),
     col = "red", lwd = 2,
     xlab = "Hour", ylab = "Demand (MW)", main = paste(myBA, "Residual Demand"))
lines(T_range, demand_resi[T_range], type = "s", lty = 1, col = "blue", lwd = 2)
lines(T_range, op_by_hour_reshaped$Gas_Power[T_range], type = "s", col = "green", lwd = 2)
legend("topleft", legend=c("Demand", "Residual Demand", "NG"), 
       col=c("red", "blue", "green"), 
       pch = c(NA,NA,NA), lty = c(1,1,2), cex = 1)





# # Long to wide
# library(tidyr)
# data <- data.frame(
#   id = c(1, 2),
#   treatment1 = c(5, 4),
#   treatment2 = c(6, 5)
# )
# 
# # Reshaping from wide to long
# long <- pivot_longer(data, cols = -id, names_to = "treatment", values_to = "result")
# 
# # Reshaping from long to wide
# wide <- pivot_wider(long, names_from = treatment, values_from = result)
# 
# # View the data
# print("Long format:")
# print(long)
# print("Wide format:")
# print(wide)
# 
# pivot_wider(op_by_hour_s, names_from = Type, values_from = Power)