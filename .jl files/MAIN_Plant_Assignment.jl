using DataFrames, Random, CSV, Distributions
using Gurobi, JuMP

include("Assign_NG_plants.jl")
include("Lookup_Dict_and_Functions.jl")

# Determine balancing authority
BALANCING_AUTHORITY = "ERCO"
# folder_path = "C:\\Users\\Achaochao\\Desktop\\CISO_Results_2024-01-12"  # Change path accordingly
# folder_path = "C:\\Users\\Achaochao\\Desktop\\PJM_Results_2024-01-11"
# folder_path = "C:\\Users\\Achaochao\\Desktop\\SWPP_Results_2024-01-12"
# folder_path = "C:\\Users\\Achaochao\\Desktop\\ERCO_Results_2024-01-11"
folder_path = raw"C:\Users\Achaochao\Desktop\ERCO_Results_2024-01-18"
# folder_path = find_path_in_file("Result_Dir_List.txt", BALANCING_AUTHORITY)
println(folder_path)
if ispath(folder_path) && occursin(BALANCING_AUTHORITY, folder_path)
    println()
    println("----- Folder for ", BALANCING_AUTHORITY, " has been located. -----")
else
    println()
    println("----- Check folder path. -----")
end

# Read cost data output from R
plant_cost_df = CSV.read("../R codes/Plant_Op_Cost.csv", DataFrame)
names(plant_cost_df)
plant_cost_df = plant_cost_df[plant_cost_df.Balancing_Authority_Code.==BALANCING_AUTHORITY, :] # filter plants by BA
unique(plant_cost_df.Plant_Code)

# Group plants by their plant IDs
gdf = groupby(plant_cost_df, :Plant_Code)
aggregated_plant_df = combine(gdf,
    :Nameplate_Capacity_MW => sum => :Total_Nameplate_Capacity,
    :Minimum_Load_MW => minimum => :Total_Min_Capacity,  # The minimum as the total. This is might not be realistic. Possible revision later. ***
    :Operating_Cost => mean => :Avg_Operating_Cost   # Since each generator within a plant has same cost, this doesn't change anything.
)

# Define the plants as "Symbols"
plants = Symbol.(string.("PL_", aggregated_plant_df[!, "Plant_Code"])) # For now, consider each plant as a whole
# plants = Symbol.(string.("GE_", plant_cost_df[!, "Plant_Code"], "_", plant_cost_df[!, "Generator_ID"]))  # This treats each generator separately 

# Plants data
max_capacity_MW = Dict(plants[i] => aggregated_plant_df[i, "Total_Nameplate_Capacity"] for i in eachindex(plants))
min_capacity_MW = Dict(plants[i] => aggregated_plant_df[i, "Total_Min_Capacity"] for i in eachindex(plants))
cost_per_MWh = Dict(plants[i] => aggregated_plant_df[i, "Avg_Operating_Cost"] for i in eachindex(plants))
# for p in plants[1:10]
#     println(p, "  Max: ", max_capacity_MW[p], "  Min: ", min_capacity_MW[p], "  Cost: ", cost_per_MWh[p])
# end
sum_capacity = sum(values(max_capacity_MW))

# Other data
reserve_margin = 0.1 # Extra capacity (%)
time_periods = 1:8760 # https://docs.juliahub.com/CalculusWithJulia/AZHbv/0.0.5/precalc/ranges.html

# To save result dataframes
dfs = Dict{String,DataFrame}();


#################################################
# Loop through each simulation result file
# include("Assign_NG_plants.jl")

# sum_capacity
# infeasible_count = 0
# for i in 1:1000
#     println("------------------ Looking at $i-th file now. ------------------")

#     dfs["Sim_$i"] = assign_NG_plants(i, folder_path, BALANCING_AUTHORITY, plants, max_capacity_MW, min_capacity_MW, cost_per_MWh, reserve_margin, time_periods)

#     if any(ismissing, dfs["Sim_$i"][!, "Cumulative_Output"])
#         infeasible_count = infeasible_count + 1
#     end
# end

# infeasible_count

# Result quick check
# i = 999
# display(dfs["Sim_$i"])
# get_cost = plant_id -> get(cost_per_MWh, plant_id, NaN) # Function to get cost from the dictionary. Returns NaN if plant_id is not in the dictionary
# sum(map(get_cost, dfs["Sim_$i"][!, "Plant_Generator"]) .* dfs["Sim_$i"][!, "Cumulative_Output"])
# assign_NG_plants(i, folder_path, BALANCING_AUTHORITY, plants, max_capacity_MW, min_capacity_MW, cost_per_MWh, reserve_margin, time_periods)


#################################################
# Save results to CSV
# folder_path

# for (name, df) in dfs
#     filename = joinpath(folder_path, string(BALANCING_AUTHORITY, "_", "$name", "_Plant_Asgmt_Results.csv"))  # Generate a filename based on the DataFrame's name
#     println(filename)
#     CSV.write(filename, df)
# end


##########################################
# Just to check max demand of each file
# println("Looking at: ", BALANCING_AUTHORITY, "'s simulation result files.")

# num_files = 5940
# NG_max_vector = zeros(0)
# for i in 1:num_files
#     push!(NG_max_vector, read_max_NG_demand(i, folder_path, BALANCING_AUTHORITY))
# end
# sum_capacity
# sum(sum_capacity .< NG_max_vector)
# sum(sum_capacity .< NG_max_vector .* (1 + reserve_margin))

# temp_df = DataFrame()
# temp_df.Sim_id = collect(1:num_files)

# col1 = BALANCING_AUTHORITY * "_sum"
# col2 = BALANCING_AUTHORITY * "_NG_max"
# temp_df[!, col1]= fill(sum_capacity, num_files)
# temp_df[!, col2]= NG_max_vector
# temp_df

# filename = "NG_capacity_sum_2024-01-18.csv"
# CSV.write(filename, temp_df)