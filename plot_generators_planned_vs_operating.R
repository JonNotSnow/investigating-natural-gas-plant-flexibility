library(readxl)
library(ggplot2)
library(dplyr)
library(scales)

planned_fleet <- read_excel(path = "generator_december_2019.xlsx",
                            sheet = "Planned",
                            skip = 1, guess_max = 10^3)
planned_fleet <- planned_fleet %>% 
  rename_with(~ gsub("\r\n", "_", .x, fixed = TRUE)) %>%
  rename_with(~ gsub(" ", "_", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("__", "_", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("(MW)", "MW", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("(MWh)", "MWh", .x, fixed = TRUE)) %>% 
  filter(!is.na(Plant_ID)) %>% 
  filter(Planned_Operation_Year <= 2023) # Not looking at plants planned beyond 2023
glimpse(planned_fleet)

current_fleet <- read_excel(path = "generator_november_2023.xlsx", # As of November 2023
                            sheet = "Operating",
                            skip = 2, guess_max = 10^3)
current_fleet <- current_fleet %>% 
  rename_with(~ gsub("\r\n", "_", .x, fixed = TRUE)) %>%
  rename_with(~ gsub(" ", "_", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("__", "_", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("(MW)", "MW", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("(MWh)", "MWh", .x, fixed = TRUE)) %>% 
  filter(!is.na(Plant_ID))
glimpse(current_fleet)

colnames(planned_fleet)[colnames(planned_fleet) %in% colnames(current_fleet)]
# Find the rows in df.x that are also in df.y
result <- semi_join(planned_fleet, current_fleet,
                    by = c("Plant_ID", "Generator_ID", "Balancing_Authority_Code"))

# Group by technology types and sum total nameplate capacity
planned_fleet_grp <- planned_fleet %>% 
  group_by(Technology) %>% 
  summarise(Total_Nameplate_Capacity_MW = sum(Nameplate_Capacity_MW))
planned_fleet_grp

result_grp <- result %>% 
  group_by(Technology) %>% 
  summarise(Total_Nameplate_Capacity_MW = sum(Nameplate_Capacity_MW))
result_grp

# Final df showing the difference between planned and operating
comparison_df <- merge(planned_fleet_grp, result_grp, by = c("Technology"), all.x = TRUE)
comparison_df 
comparison_df <- comparison_df %>% # Rename columns
  rename(Dec_2019_Planned = Total_Nameplate_Capacity_MW.x) %>% 
  rename(Nov_2023_Operating = Total_Nameplate_Capacity_MW.y) %>% 
  mutate(Nov_2023_Operating = if_else(is.na(Nov_2023_Operating), 0, Nov_2023_Operating)) %>% 
  mutate(Pct_OP = Nov_2023_Operating / Dec_2019_Planned)
comparison_df
comparison_df <- comparison_df %>% # Add another "Energy_Source" column for plot color coding
  mutate(Energy_Source = if_else(grepl("Natural Gas", Technology, fixed = TRUE), "NG", "Other")) %>% 
  mutate(Energy_Source = if_else(grepl("Wind", Technology, fixed = TRUE), "WND", Energy_Source)) %>% 
  mutate(Energy_Source = if_else(grepl("Photovoltaic", Technology, fixed = TRUE), "SUN", Energy_Source)) 
comparison_df

### Plots
# Planned Capacity vs. Realized Operating Capacity
ggplot(comparison_df) +
  geom_bar(aes(x = Technology, y = Dec_2019_Planned, fill = "Planned"), stat = "identity") +
  geom_bar(aes(x = Technology, y = Nov_2023_Operating, fill = "Operating"), stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("Planned" = "grey70", "Operating" = "grey30")) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title = element_text(size = 14), axis.title.y = element_text(vjust = 2.5),
        legend.title = element_blank(), legend.text = element_text(size = 14),
        legend.position = "top") +
  labs(x = "Technology", y = "Capacity (MW)", 
       title = "Planned Capacity (Dec. 2019) vs. Realized Operating Capacity (Nov. 2023)",
       caption = "Source: EIA-860M") +
  guides(fill = guide_legend(reverse = TRUE))

# Operational Percentage of Planned Plant/Generator Capacity
ggplot(comparison_df, aes(x = Technology, y = Pct_OP, fill = Energy_Source)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(Pct_OP, accuracy = 1)), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 4) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 14) +
  scale_fill_manual(values = c("Other" = "grey85", "SUN" = "gold2", "NG" = "#7570b3", "WND" = "cornflowerblue")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        legend.position = "none",
        plot.title = element_text(hjust = 0)) +
  labs(x = "Technology", y = "% Operating as of Nov 2023", 
       title = "Operational Percentage of Planned Plant/Generator Capacity (Dec. 2019) as of Nov. 2023",
       caption = "Source: EIA-860M")

