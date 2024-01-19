using DataFrames, Random, CSV, Distributions
using Gurobi, JuMP
using Dates, TimeZones
# using JLD2
# using Gadfly

include("Lookup_Dict_and_Functions.jl");
include("Simple_Optmization.jl");
include("Solution_Exporting.jl");


################################################################################
# Import hourly data
################################################################################

#
# Choose balancing authority
# >.>
BALANCING_AUTHORITY = "PJM"
# get_iana_time_zone(BALANCING_AUTHORITY, BA_to_time_zone, us_time_zones)
# <.<
#


############################
# 1.
# read hourly demand data
DEMAND_PATH = string("Cleaned_Data_Oct_2020_Update/", BALANCING_AUTHORITY, ".csv") # file path to read hourly demand

raw_demand = CSV.read(DEMAND_PATH, DataFrame, dateformat="yyyy-mm-dd HH:MM:SS");
raw_demand.date_time = raw_demand.date_time - Hour(1); # substract 1 hour to show the start of each hour (EIA uses the end of each hour)
raw_demand.local_date_time = ZonedDateTime.(raw_demand.date_time, tz"UTC"); # time zone was set to UTC in the data
raw_demand.local_date_time = astimezone.(raw_demand.local_date_time, get_iana_time_zone(BALANCING_AUTHORITY, BA_to_time_zone, us_time_zones)); # then change to BA's respective time zone
# first(raw_demand, 10)

# ***** filter only 2019 demand ***** 
raw_demand_2019 = raw_demand[Dates.year.(raw_demand.local_date_time).==2019, :]# look up Julia broadcasting if needed
first(raw_demand_2019, 10)

#############################
# 2.
# read concurrent hourly solar and wind generation data
SOLAR_PATH = "imputed_solar_2019.csv"
WIND_PATH = "imputed_wind_2019.csv"

solar_df = CSV.read(SOLAR_PATH, DataFrame);
wind_df = CSV.read(WIND_PATH, DataFrame);

BA_solar = solar_df[:, BALANCING_AUTHORITY];
BA_wind = wind_df[:, BALANCING_AUTHORITY];
# describe(BA_solar)


################################################################################
# Prepare the model
################################################################################

### Variable/Vectors defined glovbally
# Demand for each time period
demands = raw_demand_2019[:, "cleaned demand (MW)"]

# Define the data
max_demand = maximum(demands)                     # Maximum demand throughout
step_size = 10000                                  # Representing a dummy plant. Smaller takes longer to solve.
n_steps = ceil(Int, max_demand / step_size)       # Number of dummy NG plants
plants = [Symbol("Plant", i) for i in 1:n_steps]  # Gas plant identifiers (Julia symbols as keys instead of strings)
time_periods = 1:length(demands)                  # Example: 8760 hours
renewables = [:Wind, :Solar]                      # Renewable energy sources (symbols to be used in dictionaries later)

# Existing power plant data
max_capacity = Dict(plants .=> step_size)                                             # Maximum capacity of each plant in MW
cost_per_mw = Dict(Symbol("Plant", i) => Float64(1 * i) for i in 1:n_steps)           # Cost per MW of electricity produced per hour
# emission_factor = Dict(plants .=> 0.5)                                              # Tons of CO2 per MW per hour

### Renewable generation data (changed to Float64 to avoid "InexactError" in Julia).
generation_def = Dict(
    :Wind => Float64.(BA_wind),   # Variable wind generation
    :Solar => Float64.(BA_solar)  # Solar generation
)

# Renewable pre-peneration levels: sum of renewable generation / sum of demand
solar_pre_pene = sum(BA_solar) / sum(demands)   # SOLAR pre-curtailment penetration level (%)
wind_pre_pene = sum(BA_wind) / sum(demands)     # WIND pre-curtailment penetration level (%)

# Curtailment cost (placeholder for future revision)
# cost_curt =

### Vectors for saving input parameters and results. Need to be reset before each MC run.
STG_HR_VEC = zeros(0); # storage hour for each run
STG_PWR_VEC = zeros(0); # storage rated power for each run
# NG_MAX_VEC = zeros(0); # max NG power
# NG_SUM_VEC = zeros(0); # sum of NG generation
SOLAR_PRE_VEC = zeros(0); # solar pre-penetration
WIND_PRE_VEC = zeros(0); # wind pre-penetration
# SOLAR_MAX_VEC = zeros(0); # max solar generation after scaling
# WIND_MAX_VEC = zeros(0); # max wind generation after scaling
# SOLAR_POST_VEC = zeros(0); # solar post penetration
# WIND_POST_VEC = zeros(0); # wind post penetration
COMB_PRE_VEC = zeros(0); # combined pre penetration
# COMB_POST_VEC = zeros(0); # combined post penetration
# LOAD_MAX_VEC = zeros(0); # max of net demand
dfs = Dict{String,DataFrame}(); # to save result dataframes

println("----------------------------------------------------------------")
println("*** Deterministic model inputs loaded. ***")
println("Currently running simulations for:")
println(BALANCING_AUTHORITY)
println("----------------------------------------------------------------")


################################################################################
# ***** FOR loop for simple Monte Carlo here *****
################################################################################
include("Simple_Optmization.jl");
include("Solution_Exporting.jl");

### Battery data ###
charge_rate_range = floor.(collect(0.2:0.1:1) * max_demand / 1000) * 1000     # MW, max rate of charging/discharging
battery_hour_range = collect(2:2:8)                                              # Random storage hour rating
efficiency = 0.9                                                          # Charge efficiency
efficiency_dis = 0.9                                                      # Discharge efficiency

### Renewable penetration levels
total_pre_pene_range = collect(0.5:0.05:1.2)        # Total pre-penetration
solar_pct_range = collect(0:0.1:1)                  # % from solar

# Cost coefficient in obj function
alpha = 1e-3

i = 1
for charge_rate in charge_rate_range
    for battery_hour in battery_hour_range
        for total_pre_pene in total_pre_pene_range
            for solar_pct in solar_pct_range

                battery_capacity = charge_rate * battery_hour  # MWh capacity

                wind_pct = 1 - solar_pct   # % from wind

                println("-------------------------------------- This is $i-th simulation. --------------------------------------")

                # Scale renewable generation
                solar_scale = total_pre_pene * solar_pct / solar_pre_pene
                wind_scale = total_pre_pene * wind_pct / wind_pre_pene
                scaleFactors = Dict(:Wind => wind_scale, :Solar => solar_scale)    # Scale factor for solar and wind respectively
                # scaleFactors = Dict(:Wind => 1.11, :Solar => 7)  # Manual scale factors
                generation = RE_scaling(generation_def, scaleFactors)

                # # ***** Run the model function ******
                solution = simple_opt(demands, battery_capacity, charge_rate, efficiency, efficiency_dis, generation, alpha)
                # # ***** Run the model function ******

                # ### Update input parameter vectors
                push!(STG_HR_VEC, battery_hour) # storage hour for each run
                push!(STG_PWR_VEC, charge_rate) # storage rated power for each run
                push!(SOLAR_PRE_VEC, total_pre_pene * solar_pct) # SCALED solar pre-penetration
                push!(WIND_PRE_VEC, total_pre_pene * wind_pct) # SCALED wind pre-penetration
                push!(COMB_PRE_VEC, total_pre_pene) # combined pre penetration of renewables

                # ### Save model results to df's
                solution_df = solution_to_df(demands, time_periods, solution, generation)

                # # Attach generated df to data-saving dictionary (with unique name)
                dfs["Sim_$i"] = solution_df
                i = i + 1

                println(charge_rate, " MW, ", battery_hour, " hours, ", total_pre_pene * solar_pct, " solar, ", total_pre_pene * wind_pct, " wind.")
            end
        end
    end
end # *** Don't forget to reset the vectors before running the for loop again. ***


################################################################################
# Write to CSVs
################################################################################
# Create directory to save the results
prefix = BALANCING_AUTHORITY * "_Results_"
formatted_date = Dates.format(Dates.today(), "yyyy-mm-dd")
# directory = "$prefix$formatted_date"  # Using string interpolation
# directory = "C:\\Users\\Achaochao\\Desktop\\$prefix$formatted_date"  # Save to local desktop instead of Google Drive
if !isdir(directory)
    println()
    println("New directory is created.")
    println()
    mkdir(directory)  # Create the directory if it doesn't exist
end

# Export model results
for (name, df) in dfs
    filename = joinpath(directory, string(BALANCING_AUTHORITY, "_", "$name.csv"))  # Generate a filename based on the DataFrame's name
    CSV.write(filename, df)
end

# Export input df
input_df = DataFrame(
    Battery_Hour=STG_HR_VEC, # storage hour for each run
    Battery_Rate=STG_PWR_VEC, # storage rated power for each run
    Solar_Pre_Pene=SOLAR_PRE_VEC, # SCALED solar pre-penetration
    Wind_Pre_Pene=WIND_PRE_VEC, # SCALED wind pre-penetration
    # Solar_Max_Out = SOLAR_MAX_VEC, # max solar generation
    # Wind_Max_Out = WIND_MAX_VEC, # max wind generation
    Combined_Pre_Pene=COMB_PRE_VEC # combined pre penetration
)
filename = joinpath(directory, string(BALANCING_AUTHORITY, "_inputs.csv"))
CSV.write(filename, input_df)
