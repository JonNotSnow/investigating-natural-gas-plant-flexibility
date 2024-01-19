##################################################
# Dictionaries for timezones. Function to look up.
using Dates, TimeZones

us_time_zones = Dict(
    "Eastern" => tz"America/New_York",
    "Central" => tz"America/Chicago",
    "Mountain" => tz"America/Denver",
    "Mountain (no DST)" => tz"America/Phoenix",
    "Pacific" => tz"America/Los_Angeles",
    "Alaska" => tz"America/Anchorage",
    "Hawaii" => tz"Pacific/Honolulu",
    "Samoa" => tz"Pacific/Pago_Pago",
    "Chamorro" => tz"Pacific/Guam"
)

BA_to_time_zone = Dict(
    "CISO" => "Pacific",
    "ERCO" => "Central",
    "MISO" => "Eastern",
    "PJM" => "Eastern",
    "SWPP" => "Central"
    # Add more cities as needed
)

function get_iana_time_zone(BA::String, BA_to_tz::Dict{String,String}, tz_dict::Dict{String,<:TimeZone})
    # BA_to_tz: Dictionary linking each BA to its reporting time zone 
    # tz_dict: Dictionary linking time zone to representive city in IANA database

    # Find the time zone name from the BA
    tz_name = get(BA_to_tz, BA, nothing)

    # If the city is found, then find the IANA time zone
    if tz_name !== nothing
        return get(tz_dict, tz_name, nothing)
    else
        return nothing  # or some error handling
    end
end


#########################################
# Function to scale renewable generation
function RE_scaling(generation_in::Dict{Symbol,Vector{Float64}}, scale_factors::Dict{Symbol,<:Number})
    scaled_generation = Dict{Symbol,Vector{Float64}}()

    for (key, value) in generation_in
        scaleFactor = get(scale_factors, key, 1)  # Default to 1 if key not found
        scaled_generation[key] = value .* scaleFactor
    end

    return scaled_generation
end


#########################################
# Function to locate result directory
function find_path_in_file(file_path, key)
    found_path = nothing  # Default to `nothing` if the path is not found
    open(file_path, "r") do file
        for line in eachline(file)
            if occursin(key, line)
                found_path = strip(line) # Remove any white spaces
                found_path = replace(found_path, "\\\\" => "/") # Replace double backslashes with single ones if needed
                break
            end
        end
    end
    return found_path
end


# ##############################################
# # ***** Main function for optimization *****

# # Understand MIP Logging: https://www.gurobi.com/documentation/9.1/refman/mip_logging.html

# function simple_opt(demands, battery_capacity, charge_rate, efficiency, efficiency_dis, generation, alpha)

#     #--------------------------------------------------
#     # Model start
#     #--------------------------------------------------

#     # Initialize the model
#     model = Model(Gurobi.Optimizer)
#     set_optimizer_attribute(model, "TimeLimit", 30)  # Set a time limit of 60 seconds
#     set_optimizer_attribute(model, "FeasibilityTol", 1e-4)
#     set_optimizer_attribute(model, "OptimalityTol", 1e-6)
#     set_optimizer_attribute(model, "MIPGap", 0.5e-3)

#     ######## 
#     # Decision variables for power plants
#     @variable(model, power[plants, time_periods] >= 0)

#     # Decision variables for battery
#     @variable(model, charge[time_periods] >= 0)   # Charging power in each period
#     @variable(model, discharge[time_periods] >= 0)   # Discharging power in each period
#     @variable(model, state_of_charge[time_periods] >= 0)  # State of charge at the end of each period
#     @variable(model, charge_mode[time_periods], Bin)  # Binary variable for charge mode

#     # @variable(model, actual_charge[time_periods] >= 0)  # Effective charged energy in each period
#     # @variable(model, actual_discharge[time_periods] >= 0)  # Effective discharged energy in each period

#     @variable(model, cycle_cost[time_periods] >= 0) # Introduce battery cycling cost

#     # Decision variables for renewable generation and curtailment
#     @variable(model, gen[renewables, time_periods] >= 0)  # Generation from renewables INTO grid (including meeting demand and charging battery)
#     @variable(model, curt[renewables, time_periods] >= 0)  # Curtailment of renewables

#     ########  
#     # Constraints
#     for t in time_periods
#         # Demand must be met by plants, battery discharge, and renewable generation minus any curtailment
#         @constraint(model, sum(power[p, t] for p in plants) + discharge[t] + sum(gen[r, t] for r in renewables) == demands[t] + charge[t])

#         # Power output must not exceed plant capacity
#         for p in plants
#             @constraint(model, power[p, t] <= max_capacity[p])
#         end

#         # Renewable generation limits
#         for r in renewables
#             @constraint(model, gen[r, t] <= generation[r][t])  # Generation cannot exceed available potential
#         end

#         # Curtailments can only max out up to what is produced
#         for r in renewables
#             @constraint(model, curt[r, t] <= generation[r][t])
#         end

#         # Curtailment is now any excess generation that isn't used or stored
#         @constraint(model, sum(curt[r, t] for r in renewables) == sum(generation[r][t] for r in renewables) - (demands[t] - sum(power[p, t] for p in plants) - discharge[t] + charge[t]))

#         # Battery operation limits and priority use of renewable for charging
#         @constraint(model, charge[t] <= charge_rate * charge_mode[t])
#         @constraint(model, discharge[t] <= charge_rate * (1 - charge_mode[t]))
#         @constraint(model, state_of_charge[t] >= 0.1 * battery_capacity) # 10% minimum SOC
#         @constraint(model, state_of_charge[t] <= 0.9 * battery_capacity) # 90% maximum SOC
#         # @constraint(model, actual_charge[t] <= charge_rate * charge_mode[t] * efficiency)
#         # @constraint(model, actual_discharge[t] * efficiency_dis <= charge_rate * (1 - charge_mode[t]))

#         # Battery efficiency constraints
#         # @constraint(model, actual_charge[t] <= charge[t] * efficiency) # For charging efficiency
#         # @constraint(model, discharge[t] <= actual_discharge[t] * efficiency_dis) # For discharging efficiency

#         # Battery state of charge update
#         # if t == 1
#         #     @constraint(model, state_of_charge[t] == battery_capacity / 2 + actual_charge[t] - actual_discharge[t]) # Assume half-full charge to start with
#         # else
#         #     @constraint(model, state_of_charge[t] == state_of_charge[t-1] + actual_charge[t] - actual_discharge[t])
#         # end
#         if t == 1
#             @constraint(model, state_of_charge[t] == battery_capacity / 2 + charge[t] * efficiency - discharge[t] * (1/efficiency_dis)) # Assume half-full charge to start with
#         else
#             @constraint(model, state_of_charge[t] == state_of_charge[t-1] + charge[t] * efficiency - discharge[t] * (1/efficiency_dis))
#         end


#         # *** The following constraints still need testing. ***
#         # *** Comment in and out along with their associated decision variables, if any. ***
#         # Encourage battery charging from renewables before curtailment
#         # @constraint(model, sum(gen[r, t] for r in renewables) + charge[t] >= sum(generation[r][t] for r in renewables))

#         # Encourage battery charging using excess grid capacity
#         # @constraint(model, sum(power[p, t] for p in plants) + charge[t] <= sum(max_capacity[p] for p in plants))

#         # Introduce battery cycleing cost
#         @constraint(model, cycle_cost[t] >= (charge[t] + discharge[t]))
#     end

#     ######## 
#     # Objective: Minimize total cost (including potential costs and (huge) penalties for curtailment)
#     @objective(
#         model,
#         Min,
#         sum(cost_per_mw[p] * power[p, t] for p in plants, t in time_periods)
#         + alpha * sum(curt[r, t] for r in renewables, t in time_periods)  # Minimize curtailment cost might lead to more battery cycles
#         # - (0.0001) * sum(charge[t] for t in time_periods)  # Incentivizes battery charging
#         + alpha * 1e2 * sum(cycle_cost[t] for t in time_periods)  # Add a cost for each charging and discharging cycle
#     )  # Modify as needed for costs

#     ######## 
#     # Solve the model
#     optimize!(model)

#     #--------------------------------------------------
#     # End of model
#     #--------------------------------------------------


#     # Collect results
#     solution = Dict()

#     solution["power"] = value.(power)

#     solution["charge"] = value.(charge)
#     solution["discharge"] = value.(discharge)
#     # solution["actual_charge"] = value.(actual_charge)
#     # solution["actual_discharge"] = value.(actual_discharge)
#     solution["state_of_charge"] = value.(state_of_charge)

#     solution["gen"] = value.(gen)
#     solution["curt"] = value.(curt)

#     solution["total_cost"] = objective_value(model)
#     # solution["total_emissions"] = sum(value(power[p, t]) * emission_factor[p] for p in plants, t in time_periods)

#     return solution
# end