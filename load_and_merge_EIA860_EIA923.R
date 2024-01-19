library(readxl)
library(dplyr)
library(ggplot2)
library(zoo)



# Net Actual Generation excluding imports (NAAGxi) = Gross Actual Generation (GAAG) - Auxiliary Loads
# In the power station after the electricity is generated, some of the electricity is used onsite, 
# for example in lighting and other electrical equipment. These are called auxiliary loads. 



####################################
# 
# PART 1:
# READ EIA 860 DATA
#
####################################

# (1) PLANT DATA, Schedule 2
# location, BA code
eia860_plant <- read_excel(path = "eia860_2019/2___Plant_Y2019.xlsx", skip = 1, guess_max = 10^3)
colnames(eia860_plant) <- gsub(" ", "_", colnames(eia860_plant))
str(eia860_plant)
# colnames(eia860_plant)


# (2) GENERATOR DATA, Schedule 3
# nameplate capacity, energy source(s)
eia860_generator <- read_excel(path = "eia860_2019/3_1_Generator_Y2019.xlsx", 
                               sheet = "Operable", skip = 1, guess_max = 1048576) 
# guess_max to increase the number of rows used to "guess" the appropriate data type of the columns
# 1,048,576 is the maximum number of lines supported by Excel currently

colnames(eia860_generator) <- gsub(" ", "_", colnames(eia860_generator))
str(eia860_generator)
# colnames(eia860_generator)


# (3) Merge (1) & (2)
eia860_merged <- merge(eia860_plant, eia860_generator,
                       by = c("Plant_Code", "Plant_Name", "Utility_ID", "Utility_Name", "State", "County"),
                       all.y = TRUE)
eia860_merged <- arrange(eia860_merged, Plant_Code)
colnames(eia860_merged)


# eia860_merged %>%
#   filter(Energy_Source_1 == "NG") %>% # this may include/exclude some generators that could run with gas/oil
#   count() # %>% # find # of rows



####################################
# 
# PART 2:
# READ EIA 923 DATA
#
####################################

# ===> (1) Generator data with monthly net generation

# cleaning function
clean_generator_eia923 <- function(eia923_data_sheet) {
  generator <- eia923_data_sheet
  
  # rename columns
  generator <- generator %>%
    rename_with(~ gsub("\r\n", "_", .x, fixed = TRUE)) %>%
    rename_with(~ gsub(" ", "_", .x, fixed = TRUE)) %>%
    rename_with(~ gsub("__", "_", .x, fixed = TRUE)) %>%
    rename(Plant_Code = Plant_Id) %>%  # new_name = old_name
    rename(Generator_ID = Generator_Id) # This doesn't match yet
  
  # modify some values
  generator <- generator %>% 
    mutate(across(where(is.character), ~ na_if(., "."))) %>% 
    mutate(Combined_Heat_And_Power_Plant = if_else(Combined_Heat_And_Power_Plant == "Y", TRUE, FALSE))
  
  # remove Temp_Helper column
  if("Temp_Helper" %in% colnames(generator)) {
    generator <- generator %>% 
      select(-Temp_Helper)
  }
  
  return(generator)
}


# read spreadsheet
raw_eia923_generator <- read_excel(path = "eiaf923_2019/EIA923_Schedules_2_3_4_5_M_12_2019_Final_Revision.xlsx", 
                               sheet = "Page 4 Generator Data", 
                               skip = 5, guess_max = 1048576)

cleaned_generator <- clean_generator_eia923(raw_eia923_generator)
glimpse(cleaned_generator)

remove(raw_eia923_generator)


# ===> (2) Generation and fuel consumption <===
# This data can't be easily linked to individual boilers, generators, and generation units, 
# but it provides the most complete coverage of fuel consumption and electricity generation for the entire generation fleet.

# cleaning function
clean_gen_fuel_eia923 <- function(eia923_data_sheet) {
  gen_fuel <- eia923_data_sheet
  
  gen_fuel <- gen_fuel %>%
    
    # rename columns
    rename_with(~ gsub("\r\n", "_", .x, fixed = TRUE)) %>%
    rename_with(~ gsub(" ", "_", .x, fixed = TRUE)) %>%
    rename(Plant_Code = Plant_Id) %>% # new_name = old_name
    
    # modify values in a few columns
    mutate(Combined_Heat_And_Power_Plant = if_else(Combined_Heat_And_Power_Plant == "Y", TRUE, FALSE)) %>% 
    mutate(Nuclear_Unit_Id = if_else(Nuclear_Unit_Id == ".", NA_character_, Nuclear_Unit_Id)) %>%
    mutate(Nuclear_Unit_Id = as.numeric(Nuclear_Unit_Id))
  
  # remove Temp_Helper column
  if("Temp_Helper" %in% colnames(gen_fuel)) {
    gen_fuel <- gen_fuel %>% 
      select(-Temp_Helper)
  }
  
  return(gen_fuel)
}


# read spreadsheet
raw_eia923_gen_plus_fuel <- read_excel(path = "eiaf923_2019/EIA923_Schedules_2_3_4_5_M_12_2019_Final_Revision.xlsx", 
                                   sheet = "Page 1 Generation and Fuel Data", # <=====
                                   skip = 5, guess_max = 1048576)

# run the cleaning function
cleaned_gen_fuel <- clean_gen_fuel_eia923(raw_eia923_gen_plus_fuel)

cleaned_gen_fuel_grouped <- cleaned_gen_fuel %>%
  group_by(Plant_Code, Plant_Name, Reported_Fuel_Type_Code, Physical_Unit_Label) %>%
  summarise(Total_Fuel_Consumption_Physical_Quantity_by_Type = sum(Total_Fuel_Consumption_Quantity, na.rm = TRUE),
            Total_Fuel_Consumption_MMBtu_by_Type = sum(Total_Fuel_Consumption_MMBtu, na.rm = TRUE),
            Total_Generation_by_Type_BACKUP = sum(Net_Generation_Megawatthours, na.rm = TRUE)) %>% # sum of generation from this 923 data
  rename(Fuel_Type_Code = Reported_Fuel_Type_Code)

remove(raw_eia923_gen_plus_fuel)


############################################################################################
# Compare net generation number in the two imported sheets above

# ***** For now, use the net generation by generators. *****

# # From (1):
# # This is the net generation summarized over generation by each generator (with generator ID that is also in EIA 860)
# net_gen_1 <- cleaned_generator %>%
#   group_by(Plant_Code) %>%
#   summarise(Net_Plant_Gen = sum(Net_Generation_Year_To_Date)) %>%
#   mutate(Net_Plant_Gen = round(Net_Plant_Gen)) %>%
#   glimpse()
# 
# 
# # From (2):
# # Based on "Page 7 File Layout":
# # This is total electrical output net of station service.  
# # In the case of combined heat and power plants, this value is intended to include internal consumption of electricity 
# # for the purposes of a production process, as well as power put on the grid.
# net_gen_2 <- cleaned_gen_fuel %>%
#   group_by(Plant_Code) %>%
#   summarise(Net_Plant_Gen = sum(Net_Generation_Megawatthours)) %>%
#   mutate(Net_Plant_Gen = round(Net_Plant_Gen)) %>%
#   glimpse() %>%
#   filter(Plant_Code %in% net_gen_1$Plant_Code) %>% # choose plants that are only included in (1)
#   glimpse()
# 
# 
# # Net generation need to be cross-checked with (1)
# net_gen_1$Plant_Code[which(net_gen_1$Net_Plant_Gen < net_gen_2$Net_Plant_Gen)]
# sum(net_gen_1$Net_Plant_Gen < net_gen_2$Net_Plant_Gen)
# 
# net_gen_1$Plant_Code[which(net_gen_1$Net_Plant_Gen > net_gen_2$Net_Plant_Gen)]
# sum(net_gen_1$Net_Plant_Gen > net_gen_2$Net_Plant_Gen)
# 
# temp_df <- net_gen_1
# temp_df$Net_Plant_Gen_2 <- net_gen_2$Net_Plant_Gen # as reported in "Page 1 Generation and Fuel Data"
# temp_df <- temp_df %>% 
#   rename(generator_net_gen = Net_Plant_Gen) %>% 
#   rename(overall_net_gen = Net_Plant_Gen_2)
# temp_df$flag <- ifelse(temp_df$generator_net_gen > temp_df$overall_net_gen, ">>>", 
#                        ifelse(temp_df$generator_net_gen < temp_df$overall_net_gen, "<<<", 0))
# temp_df$flag <- ifelse(temp_df$generator_net_gen == temp_df$overall_net_gen, "Same", temp_df$flag)


############################################################################################



# ===> (3) Financial (primarily annual O&M reported)
# Per the instructions, Parts A through F are required for plants 100 MW and above, 
# and only Parts C, E and F are required for plants from 10 megawatts to less than 100 MW.
# Here Part B is imported

# cleaning functions
clean_OM <- function(eia923_data_sheet) {
  OM_data <- eia923_data_sheet
  
  # change column names
  OM_data <- OM_data %>% 
    rename_with(~ gsub("\r\n", "_", .x, fixed = TRUE)) %>%
    rename_with(~ gsub(" ", "_", .x, fixed = TRUE)) %>%
    rename_with(~ gsub("/", "_", .x, fixed = TRUE)) %>% 
    rename(Plant_Code = Plant_ID) 
  
  # pick columns with totals
  OM_data <- OM_data %>%
    select(Year, Plant_Code, starts_with("Total") & ends_with("Expense")) %>%
    
    # replace "." with "0" (still char type)
    mutate(across(c(Total_Collection_Abatement_O_and_M_Expense, Total_Disposal_Abatement_O_and_M_Expense, Total_Other_O_and_M_Expense), 
                  ~ ifelse(. == ".", "0", .))) %>%
    
    # change char type column to numeric
    mutate(across(c(Total_Collection_Abatement_O_and_M_Expense, Total_Disposal_Abatement_O_and_M_Expense, Total_Other_O_and_M_Expense), 
                  as.numeric)) %>% 
  
    # calculate final sum
    mutate(Sum_O_and_M = Total_Collection_Abatement_O_and_M_Expense + 
             Total_Disposal_Abatement_O_and_M_Expense + Total_Other_O_and_M_Expense)
    
  return(OM_data)
}

clean_fuel_cost <- function(eia923_data_sheet) {
  OM_data <- eia923_data_sheet
  
  # change column names
  OM_data <- OM_data %>% 
    rename_with(~ gsub("\r\n", "_", .x, fixed = TRUE)) %>%
    rename_with(~ gsub(" ", "_", .x, fixed = TRUE)) %>%
    rename_with(~ gsub("/", "_", .x, fixed = TRUE)) %>% 
    rename(Plant_Code = Plant_Id) %>% 
    mutate(FUEL_COST = as.numeric(FUEL_COST ))
  
  return(OM_data)
}


# read spreadsheet: O&M cost
raw_eia923_OM <- read_excel(path = "eiaf923_2019/EIA923_Schedule_8_Annual_Environmental_Information_2019_Final_Revision.xlsx", 
                        sheet = 2, # 8B Financial Info
                        skip = 4, guess_max = 1048576)

cleaned_OM <- clean_OM(raw_eia923_OM) 
glimpse(cleaned_OM) # note that cost numbers are in THOUSAND dollars

remove(raw_eia923_OM)


# read spreadsheet: fuel cost
raw_eia923_fuel_cost <- read_excel(path = "eiaf923_2019/EIA923_Schedules_2_3_4_5_M_12_2019_Final_Revision.xlsx", 
                                   sheet = "Page 5 Fuel Receipts and Costs", # <=====
                                   skip = 4, guess_max = 1048576)
cleaned_fuel_cost <- clean_fuel_cost(raw_eia923_fuel_cost)
# glimpse(cleaned_fuel_cost)
cleaned_fuel_cost_grouped <- cleaned_fuel_cost %>%
  rename(Fuel_Type_Code = ENERGY_SOURCE) %>% 
  group_by(Plant_Code, Plant_Name, Fuel_Type_Code) %>%
  summarise(Avg_Fuel_Cost_by_Type = mean(FUEL_COST, na.rm = TRUE), # in cents per million Btu (MMBtu)
            Total_Fuel_Quantity_Delivered = sum(QUANTITY, na.rm = TRUE)) # total quantity delivered, not consumed (for reference)
which(cleaned_fuel_cost_grouped$Fuel_Type_Code == "NG" & is.na(cleaned_fuel_cost_grouped$Avg_Fuel_Cost_by_Type)) # check missing values

cleaned_fuel_cost_grouped <- cleaned_fuel_cost_grouped %>%  
  mutate(Avg_Fuel_Cost_by_Type = Avg_Fuel_Cost_by_Type/100) %>%  # change to $/MMBTU
  mutate(Avg_Fuel_Cost_by_Type = if_else(Fuel_Type_Code == "NG" & is.na(Avg_Fuel_Cost_by_Type), 
                                         2.57, Avg_Fuel_Cost_by_Type)) # $2.57/MMBTU as 2019 average for NG, update if needed
which(cleaned_fuel_cost_grouped$Fuel_Type_Code == "NG" & is.na(cleaned_fuel_cost_grouped$Avg_Fuel_Cost_by_Type)) # should return 0 now

remove(raw_eia923_fuel_cost)



####################################
# 
# PART 3:
# Combine EIA 860 and 923
#
####################################
glimpse(eia860_merged)
glimpse(cleaned_generator)

### df1: Operational data
# Attach EIA923 generator to EIA860 merged main
df1 <- merge(eia860_merged, cleaned_generator, by = c("Plant_Code", "Plant_Name", "Generator_ID"), 
             all.x = TRUE)  # only include those "Operable" generators

df1 <- df1 %>% 
  rename_with(~ gsub("(", "", .x, fixed = TRUE)) %>%
  rename_with(~ gsub(")", "", .x, fixed = TRUE)) %>%
  rename_with(~ gsub("?", "", .x, fixed = TRUE)) %>% 
  mutate(YEAR = 2019) %>% 
  mutate(Balancing_Authority_Code = coalesce(Balancing_Authority_Code.x, Balancing_Authority_Code.y)) %>% 
  select(-Balancing_Authority_Code.x) %>% 
  select(-Balancing_Authority_Code.y) %>% 
  select(Plant_Code, Plant_Name, Generator_ID, Balancing_Authority_Code, Balancing_Authority_Name,
         Technology, Prime_Mover, Reported_Prime_Mover, Nameplate_Capacity_MW, Minimum_Load_MW,
         Status, Operating_Month, Operating_Year, Planned_Retirement_Month, Planned_Retirement_Year,
         Energy_Source_1, Time_from_Cold_Shutdown_to_Full_Load, Combined_Heat_And_Power_Plant,
         Net_Generation_Year_To_Date) %>% 
  rename(Fuel_Type_Code = Energy_Source_1)

# Group by plant and fuel type, calculate total generation by fuel type
df1_1 <- df1 %>% 
  group_by(Plant_Code, Plant_Name, Fuel_Type_Code) %>% 
  summarise(Total_Generation_by_Type = sum(Net_Generation_Year_To_Date, na.rm = TRUE))
  

### df2: Fuel and cost data
# Merge those two grouped dfs from above
df2 <- merge(cleaned_gen_fuel_grouped, cleaned_fuel_cost_grouped, by = c("Plant_Code", "Plant_Name", "Fuel_Type_Code"), all = TRUE)
  
# Add O&M
df2_1 <- merge(df2, cleaned_OM, by = c("Plant_Code"), all = TRUE) %>% 
  select(-Year, -Total_Collection_Abatement_O_and_M_Expense, -Total_Disposal_Abatement_O_and_M_Expense, -Total_Other_O_and_M_Expense) %>%
  mutate(Avg_Fuel_Cost_by_Type = if_else(Fuel_Type_Code == "NG" & is.na(Avg_Fuel_Cost_by_Type), 
                                         2.57, Avg_Fuel_Cost_by_Type)) %>% # $2.57/MMBTU as 2019 average for NG, update if needed
  mutate(Total_Fuel_Cost_by_Type = Total_Fuel_Consumption_MMBtu_by_Type * Avg_Fuel_Cost_by_Type) %>% 
  # mutate(Sum_O_and_M = coalesce(Sum_O_and_M, 0)) %>%
  mutate(Sum_O_and_M = Sum_O_and_M * 1000)# from 1000$/year to $/year
  # filter(Fuel_Type_Code == "NG")
glimpse(df2_1)


# Cost data frame containing all that are available
# Cost is assumed the same for all generators in a plant using the same primary fuel
# Check units: Generation in MWh/year, Total O&M in $/year, Fuel cost in $/year
cost_final_df <- merge(df1_1, df2_1, by = c("Plant_Code", "Plant_Name", "Fuel_Type_Code"), all = TRUE)

cost_final_df <- cost_final_df %>% 
  select(Plant_Code, Plant_Name, Fuel_Type_Code, 
         Total_Fuel_Consumption_MMBtu_by_Type, Avg_Fuel_Cost_by_Type, Total_Fuel_Cost_by_Type,
         Total_Generation_by_Type, # from Generator data sheet
         Total_Generation_by_Type_BACKUP, # from Gen and Fuel data sheet
         Sum_O_and_M) %>% 
  filter(Fuel_Type_Code == "NG")

cost_final_df <- cost_final_df %>%
  mutate(Total_Generation_by_Type = if_else(Total_Generation_by_Type == 0, Total_Generation_by_Type_BACKUP, Total_Generation_by_Type)) %>% 
  mutate(Total_Generation_by_Type = if_else(is.na(Total_Generation_by_Type), Total_Generation_by_Type_BACKUP, Total_Generation_by_Type)) %>% 
  filter(Total_Fuel_Cost_by_Type > 0 & Total_Generation_by_Type > 0 & Total_Generation_by_Type_BACKUP > 0)

# fill missing O&M values using linear regression based on generation
# cost_final_df$Sum_O_and_M[cost_final_df$Sum_O_and_M == 0] <- NA
# train_set <- cost_final_df[!is.na(cost_final_df$Sum_O_and_M), ]
# predict_set <- cost_final_df[is.na(cost_final_df$Sum_O_and_M), ]
# model <- lm(log(Sum_O_and_M) ~ log(Total_Generation_by_Type_BACKUP), data = train_set) # doesn't look linear

# For now, assume those not reporting O&M cost as having none. <===
cost_final_df <- cost_final_df %>%
  mutate(Operating_Cost = if_else(!is.na(Sum_O_and_M), 
                                  (Total_Fuel_Cost_by_Type + Sum_O_and_M) / Total_Generation_by_Type_BACKUP, # Use 923 generation
                                  (Total_Fuel_Cost_by_Type) / Total_Generation_by_Type_BACKUP)
  )

# Input data frame for Julia second step
input_for_Julia_df <- merge(df1, cost_final_df, by = c("Plant_Code", "Plant_Name", "Fuel_Type_Code"), all.x = TRUE)
glimpse(input_for_Julia_df)
input_for_Julia_df <- input_for_Julia_df %>% 
  select(Plant_Code, Plant_Name, Generator_ID, Balancing_Authority_Code, Fuel_Type_Code,
         Nameplate_Capacity_MW, Minimum_Load_MW, Operating_Cost,
         Planned_Retirement_Month, Planned_Retirement_Year) %>%
  filter(Fuel_Type_Code == "NG") %>% 
  filter(!is.na(Balancing_Authority_Code))

input_df_with_NA <- input_for_Julia_df %>% 
  filter(is.na(Operating_Cost))
unique(input_df_with_NA$Plant_Code)

input_for_Julia_df_final <- input_for_Julia_df %>% 
  filter(!is.na(Operating_Cost))

# Check minimum load column. For now, set problematic values to 10% of nameplate capacity
idx <- which(is.na(input_for_Julia_df_final$Minimum_Load_MW))
input_for_Julia_df_final$Minimum_Load_MW[idx] <- 0.1 * input_for_Julia_df_final$Nameplate_Capacity_MW[idx]
idx <- which(input_for_Julia_df_final$Nameplate_Capacity_MW < input_for_Julia_df_final$Minimum_Load_MW)
input_for_Julia_df_final$Minimum_Load_MW[idx] <- 0.1 * input_for_Julia_df_final$Nameplate_Capacity_MW[idx]

input_for_Julia_df_final$Generator_ID <- gsub("-", "_", input_for_Julia_df_final$Generator_ID, fixed = TRUE)

which(duplicated(paste0(input_for_Julia_df_final$Plant_Code, "_", input_for_Julia_df_final$Generator_ID)))
which(duplicated(input_for_Julia_df_final))
input_for_Julia_df_final <- unique(input_for_Julia_df_final)

# write.csv(input_for_Julia_df_final, "R codes/Plant_Op_Cost.csv", row.names = FALSE)

