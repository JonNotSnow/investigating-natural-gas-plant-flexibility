library(dplyr)
library(ggplot2)
library(readxl)

source("Redo_column_names.R")

emissions_df <- read_excel(path = "emissions_2019_EIA.xlsx", sheet = "CO2", skip = 1)
glimpse(emissions_df)

emissions_df_new <- redo_col_names(emissions_df)
glimpse(emissions_df_new)
emissions_df_new <- emissions_df_new %>% 
  rename(Emission_Rate_Short_Ton_Per_Unit_Fuel = Emission_Rate_17) %>% # These are rates 
  rename(Emission_Rate_Metric_Ton_Per_Unit_Fuel = Emission_Rate_19) %>% 
  mutate(Generation_MWh = Generation_kWh / 1000) %>% 
  mutate(Emission_Rate_Metric_Ton_Per_MWh = Metric_Tonnes_of_CO2_Emissions / Generation_MWh)
glimpse(emissions_df_new)

emissions_grp <- emissions_df_new %>% 
  group_by(Plant_Code, Plant_Name, Fuel_Code) %>% 
  summarise(Generation_MWh = sum(Generation_MWh, na.rm = TRUE),
            Total_Fuel_Consumption_MMBtu = sum(Total_Fuel_Consumption_MMBtu, na.rm = TRUE),
            Quantity_of_Fuel_Consumed = sum(Quantity_of_Fuel_Consumed, na.rm = TRUE),
            Metric_Tonnes_of_CO2_Emissions = sum(Metric_Tonnes_of_CO2_Emissions, na.rm = TRUE),
            Emission_Rate_Metric_Ton_Per_Unit_Fuel = mean(Emission_Rate_Metric_Ton_Per_Unit_Fuel, na.rm = TRUE),
            Emission_Rate_Metric_Ton_Per_MWh = mean(Emission_Rate_Metric_Ton_Per_MWh, na.rm = TRUE))

write.csv(emissions_grp, "plant_emission_rate.csv", row.names = FALSE)
