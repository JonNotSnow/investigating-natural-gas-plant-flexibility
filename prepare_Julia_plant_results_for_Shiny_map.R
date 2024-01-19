library(readxl)
library(dplyr)
library(ggplot2)


### Read EIA860 plant info
eia860_plant <- read_excel(path = "../eia860_2019/2___Plant_Y2019.xlsx", skip = 1, guess_max = 10^3)
colnames(eia860_plant) <- gsub(" ", "_", colnames(eia860_plant))
head(eia860_plant, 10)

plant_op_cost <- read.csv(file = "Plant_Op_Cost.csv")
plant_op_cost_grped <- plant_op_cost %>% 
    mutate(Plant_Code = as.numeric(Plant_Code)) %>%
    group_by(Plant_Code) %>%
    summarise(Total_Namaplate_Capacity = sum(Nameplate_Capacity_MW),
              Min_Load = min(Minimum_Load_MW),
              Avg_Op_Cost = mean(Operating_Cost),
              Number_of_Generators = n())

### Read simulation results

BA <- "CISO"

# Read simulation inputs
parent_dir <- "C:/Users/Achaochao/Desktop"
directory_names <- readLines("directory_list.txt")
folder_dir <- directory_names[grep(BA, directory_names)]

paste0(parent_dir, "/", folder_dir, "/", BA, "_inputs.csv")
sim_inputs <- read.csv(paste0(parent_dir, "/", folder_dir, "/", BA, "_inputs.csv"))
sim_inputs <- sim_inputs %>% 
  select(-Combined_Pre_Pene)
summary(sim_inputs)

# Choose the simulation based on manually entered point using Euclidean distances
my_input <- c(Battery_Hour = 4,
              Battery_Rate = 40000,
              Solar_Pre_Pene = 0.3,
              Wind_Pre_Pene = 0.2)

sim_id <- find_sim_file(my_input_vector = my_input, sim_input_df = sim_inputs)
input_row <- sim_inputs[sim_id, ] # The four parameters used in this particular simulation

# Read the corresponding results file
paste0(parent_dir, "/", folder_dir, "/", BA, "_Sim_", sim_id, "_Plant_Asgmt_Results.csv")
NG_assignment_df <- read.csv(paste0(parent_dir, "/", folder_dir, "/", BA, "_Sim_", sim_id, "_Plant_Asgmt_Results.csv"))
glimpse(NG_assignment_df)


### Merge with 860 data
plant_code <- c()
for (i in 1:nrow(NG_assignment_df)) {
  plant_code[i] <- as.integer(strsplit(NG_assignment_df$Plant_Generator, split = "_")[[i]][2])
}
NG_assignment_df$Plant_Code <- plant_code
remove(plant_code)

# Merge to add other plant info
output_to_map <- merge(NG_assignment_df, eia860_plant, by = c("Plant_Code"), all.x = T)
output_to_map <- merge(output_to_map, plant_op_cost_grped, by = c("Plant_Code"), all.x = T)
output_to_map$Simulation_id <- sim_id
output_to_map <- output_to_map %>%
  select(-Plant_Generator) %>% 
  select(Simulation_id, Plant_Code:Balancing_Authority_Name, Total_Namaplate_Capacity, Min_Load, Avg_Op_Cost, Number_of_Generators) %>% 
  mutate(Cap_Factor = Cumulative_Output / (Total_Namaplate_Capacity * 8760))
  # mutate(Cap_Factor = Generator_Output / (Total_Namaplate_Capacity * 8760))
glimpse(output_to_map)

# Save summarized results to CSV
write_path = paste0(parent_dir, "/", folder_dir, "/Map_Input_", BA, "_Sim_", sim_id, "_Plant_Asgmt.csv")
write_path

if (file.exists(write_path)) { # Check if the file already exists
  # Prompt the user for confirmation
  response <- readline(prompt = "File already exists. Do you want to overwrite it? (yes/no): ")
  # Convert response to lower case
  response <- tolower(response)

  # Check the response
  if (response == "yes") {
    # Write to the file (overwrite)
    write.csv(output_to_map, write_path, row.names = FALSE)
    cat("File has been overwritten.\n")
  } else {
    cat("Operation cancelled. File not overwritten.\n")
  }
} else {
  # If the file does not exist, write the data to a new file
  write.csv(output_to_map, write_path, row.names = FALSE)
  cat("File has been created and written.\n")
}

# write.csv(input_row, paste0(parent_dir, "/", folder_dir, "/Map_Input_", BA, "_Sim_", sim_id, "_Parameter.csv"))
write.table(input_row, paste0(parent_dir, "/", folder_dir, "/Map_Input_", BA, "_Sim_", sim_id, "_Parameter.txt"),
            sep=",", row.names = FALSE)




#####################################################################################################################################################
# Functions

find_sim_file <- function(my_input_vector, sim_input_df) {
  
  # Find the simulation file based on Euclidean distances
  
  ED_distances <- apply(sim_inputs, 1, function(each_row) {sqrt(sum((each_row - my_input)^2))} ) # MARGIN=1: the manipulation is performed on rows
  
  sim_inputs_ranked <- sim_inputs %>% 
    mutate(Distance = ED_distances,
           Simulation_ID = seq(1, nrow(sim_inputs))) %>% 
    arrange(Distance) # rearrange rows based on distances
  # glimpse(sim_inputs_ranked)
  
  for (i in 1:nrow(sim_inputs_ranked)) {
    NG_assignment_df <- read.csv(paste0(parent_dir, "/", folder_dir, "/", BA, "_Sim_", sim_inputs_ranked$Simulation_ID[i], "_Plant_Asgmt_Results.csv"))
    if (sum(is.na(NG_assignment_df$Cumulative_Output)) == 0) {   # Keep looking until one with no NA is located
      closest_row_idx <- sim_inputs_ranked$Simulation_ID[i]
      closest_input <- sim_inputs[closest_row_idx, ]
      
      print(paste0("Found simulation #", closest_row_idx))
      print(closest_input)
      
      break
    }
  }
  
  return(closest_row_idx)
}




# closest_row_idx <- 370
# closest_input <- sim_inputs[closest_row_idx, ]
# NG_assignment_df <- read.csv(paste0(parent_dir, "/", folder_dir, "/", BA, "_Sim_", closest_row_idx, "_Plant_Asgmt_Results.csv"))
# glimpse(NG_assignment_df)
# sum(is.na(NG_assignment_df$Cumulative_Output)) == 0






# read_path = paste0("Julia Codes/", BA, "_plant_output.csv")
# 
# plant_output <- read.csv(file = read_path, header = TRUE)
# head(plant_output, 10)
# glimpse(plant_output)
# 
# plant_code <- c()
# for (i in 1:nrow(plant_output)) {
#   plant_code[i] <- strsplit(plant_output$Plant_Generater, split = "_")[[i]][2]
# }
# plant_output$Plant_Code <- plant_code
# head(plant_output, 10)
# 
# plant_output_grped <- plant_output %>% 
#   mutate(Plant_Code = as.numeric(Plant_Code)) %>% 
#   group_by(Plant_Code, Operating_Cost_per_MWh) %>%
#   summarise(Total_Namaplate_Capacity = sum(Nameplate_Capacity),
#             Total_Generator_Output = sum(Generator_Output),
#             Number_of_Generators = n())
# head(plant_output_grped, 20)
# 
# plant_output_grped %>%
#   arrange(Operating_Cost_per_MWh)
# 
# 
# 
# # Read EIA860 plant info
# eia860_plant <- read_excel(path = "eia860_2019/2___Plant_Y2019.xlsx", skip = 1, guess_max = 10^3)
# colnames(eia860_plant) <- gsub(" ", "_", colnames(eia860_plant))
# head(eia860_plant, 10)
# 
# # Merge to send to R shiny map app
# plant_output_to_map <- merge(plant_output_grped, eia860_plant, by = c("Plant_Code"), all.x = T)
# glimpse(plant_output_to_map)
# plant_output_to_map <- plant_output_to_map %>% 
#   select(Plant_Code:Balancing_Authority_Name)
# glimpse(plant_output_to_map)
# 
# 
# 
# # Save summarized results to CSV
# write_path = paste0("R codes/", BA, "_plant_NG_output_to_map.csv")
# 
# # Check if the file already exists
# if (file.exists(write_path)) {
#   # Prompt the user for confirmation
#   response <- readline(prompt = "File already exists. Do you want to overwrite it? (yes/no): ")
#   # Convert response to lower case
#   response <- tolower(response)
#   
#   # Check the response
#   if (response == "yes") {
#     # Write to the file (overwrite)
#     write.csv(plant_output_to_map, write_path, row.names = FALSE)
#     cat("File has been overwritten.\n")
#   } else {
#     cat("Operation cancelled. File not overwritten.\n")
#   }
# } else {
#   # If the file does not exist, write the data to a new file
#   write.csv(plant_output_to_map, write_path, row.names = FALSE)
#   cat("File has been created and written.\n")
# }


