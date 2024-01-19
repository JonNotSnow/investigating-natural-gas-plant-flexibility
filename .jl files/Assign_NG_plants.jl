#######################################################################################################################################
# Function to assignment OPERATING plants' capacity to simulation results, based on average operating costs of each plant/generator.
function assign_NG_plants(simulation_id, folder_path, balancing_authority, plants, max_capacity_MW, min_capacity_MW, cost_per_MWh, reserve_margin, time_periods)

    # "Demands" for NG
    df_path = joinpath(folder_path, "$(balancing_authority)_Sim_$simulation_id.csv")
    if !isfile(df_path)
        error("File not found: $df_path")
    end
    sim_df = CSV.read(df_path, DataFrame)
    demands = sim_df[!, "Gas_Power"]
    max_demand = maximum(demands) * (1 + reserve_margin) # Max demand to be met considering reserve margin

    println()
    println("==> Max demand with %", reserve_margin*100, " reserve margin in this file is: $max_demand <==")
    println()

    if sum(values(max_capacity_MW)) < max_demand
        generator_output = Vector{Union{Float64,Missing}}(missing, length(plants))
        println("----- Not enough plant capacity for maximum demand. -----")
        println()
    else
        ##########################
        # Initialize the model
        model = Model(Gurobi.Optimizer)
        @variable(model, power[plants, time_periods] >= 0)

        # Constraints for each time period
        for t in time_periods
            # Demand must be met in each time period
            @constraint(model, sum(power[p, t] for p in plants) >= demands[t] * (1 + reserve_margin))

            # Power output must be within plant/generator capacity
            for p in plants
                @constraint(model, power[p, t] <= max_capacity_MW[p])
                @constraint(model, power[p, t] >= min_capacity_MW[p])
            end
        end

        # Objective: Minimize total operating cost
        @objective(model, Min, sum(cost_per_MWh[p] * power[p, t] for p in plants, t in time_periods))

        optimize!(model)

        # Check if the model has a feasible solution 
        if termination_status(model) == MOI.OPTIMAL # Check: https://github.com/jump-dev/JuMPTutorials.jl/blob/master/notebook/introduction/solvers_and_solutions.ipynb
            println("Termination Statue:: ", termination_status(model))
            generator_output = [sum(value(power[p, t]) for t in time_periods) for p in plants]
        else
            generator_output = Vector{Union{Float64,Missing}}(missing, length(plants))
            println("Termination Statue:: ", termination_status(model))
            println("----- No feasible solution found. -----")
        end
    end

    plant_out_dict = Dict(
        "Plant_Generator" => plants,
        "Cumulative_Output" => generator_output
    )

    return DataFrame(plant_out_dict)
end


#######################################################################################################################################
# Read the maximum NG power output from the simulation result files
function read_max_NG_demand(simulation_id, folder_path, balancing_authority)

    # "Demands" for NG
    df_path = joinpath(folder_path, "$(balancing_authority)_Sim_$simulation_id.csv")
    if !isfile(df_path)
        error("File not found: $df_path")
    end
    sim_df = CSV.read(df_path, DataFrame)
    demands = sim_df[!, "Gas_Power"]
    max_demand = maximum(demands)  # Max demand to be met

    return max_demand
end