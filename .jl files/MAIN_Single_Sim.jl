using DataFrames, Random, CSV, Distributions
using Gurobi, JuMP
using Dates, TimeZones
# using Gadfly


include("Lookup_Dict_and_Functions.jl");
include("Solution_Exporting.jl");

################################################################################
# Import hourly data
################################################################################

#
# Choose balancing authority
# >.>
BALANCING_AUTHORITY = "SWPP"
# get_iana_time_zone(BALANCING_AUTHORITY, BA_to_time_zone, us_time_zones)
# <.<
#

############################
# 1.
# read hourly demand data
DEMAND_PATH = string("Cleaned_Data_Oct_2020_Update/", BALANCING_AUTHORITY, ".csv")

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

solar_df = CSV.read(SOLAR_PATH, DataFrame)
wind_df = CSV.read(WIND_PATH, DataFrame)

BA_solar = solar_df[:, BALANCING_AUTHORITY];
BA_wind = wind_df[:, BALANCING_AUTHORITY];
# describe(BA_solar)


################################################################################
# Prepare the model
################################################################################

# Demand for each time period
demands = raw_demand_2019[:, "cleaned demand (MW)"];

# Define the data
max_demand = maximum(demands)                     # Maximum demand throughout
step_size = 5000                              # Representing a dummy plant. Smaller takes longer to solve.
n_steps = ceil(Int, max_demand / step_size)       # Number of dummy NG plants
plants = [Symbol("Plant", i) for i in 1:n_steps]  # Gas plant identifiers (Julia symbols as keys instead of strings)
time_periods = 1:length(demands)                  # Example: 8760 hours
renewables = [:Wind, :Solar]                      # Renewable energy sources

# Existing power plant data
max_capacity = Dict(plants .=> step_size)                       # Maximum capacity of each plant in MW
cost_per_mw = Dict(Symbol("Plant", i) => Float64(100 * i) for i in 1:n_steps)  # Cost per MW of electricity produced per hour
emission_factor = Dict(plants .=> 0.5)                          # Tons of CO2 per MW per hour

# Battery data
charge_rate = 20000                               # MW, max rate of charging/discharging
battery_hour = 4                                    # Storage hour rating
battery_capacity = charge_rate * battery_hour            # MWh
efficiency = 0.9                                   # Charge efficiency
efficiency_dis = 0.9                               # Discharge efficiency

# Renewable generation data
generation_def = Dict(
    :Wind => Float64.(BA_wind),   # Variable wind generation
    :Solar => Float64.(BA_solar)  # Solar generation
)
scaleFactors = Dict(:Wind => 2, :Solar => 2);
generation = RE_scaling(generation_def, scaleFactors)

# Cost coefficient in obj function
alpha = 1e-3

################################################################################
# Run the optimization
################################################################################

# ***** Run the model function ******
include("Lookup_Dict_and_Functions.jl");
solution = simple_opt(demands, battery_capacity, charge_rate, efficiency, efficiency_dis, generation, alpha)
# solution["power"]
# solution["charge"]
# solution["gen"]


##############################################################################
# Prepare visualization for R
##############################################################################

println(BALANCING_AUTHORITY)

include("Solution_Exporting.jl");
folder = "Single_Sim_Test_Results"
single_sim_to_R(BALANCING_AUTHORITY, demands, time_periods, solution, generation, folder)

println()
println("Currently running simulations for:")
println(BALANCING_AUTHORITY)
println("-------------- Model Inputs: --------------")
println("Charging/Discharging Rate: $charge_rate")
println("Duration: $battery_hour")
println("Battery Capacity: $battery_capacity")
println("ChargingEfficiency: $efficiency")
println("Discharging Efficiency: $efficiency_dis")
println("Alpha: $alpha")
