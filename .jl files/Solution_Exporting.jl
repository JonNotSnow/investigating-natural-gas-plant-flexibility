###########################################################################
# Function to construct solution data frame in each Monte Carlo simulation

function solution_to_df(demands, time_periods, solution_in, generation_in)

    # Initialize arrays to store data
    gas_power = zeros(length(demands)) # total output from NG plants
    battery_charge = zeros(length(demands)) # apparant charging rate
    battery_discharge = zeros(length(demands)) # apparant discharging rate
    battery_actual_charge = Vector{Union{Float64,Missing}}(missing, length(demands)) # effective energy flow into battery
    battery_actual_discharge = Vector{Union{Float64,Missing}}(missing, length(demands)) # effective energy flow out of battery
    renewable_gen_solar = zeros(length(demands)) # solar USED by the grid (load + charging)
    renewable_gen_wind = zeros(length(demands))  # wind USED by the grid (load + charging)
    renewable_curt_solar = zeros(length(demands))  # solar curtailments
    renewable_curt_wind = zeros(length(demands))  # wind curtailments

    renewable_tot = zeros(length(demands)) # total renewable energy PRODUCED each hour
    SOC = zeros(length(demands)) # state of charge of battery

    # Fill in the arrays with data from the model
    for t in time_periods
        gas_power[t] = sum(solution_in["power"][p, t] for p in plants)

        battery_charge[t] = solution_in["charge"][t]
        battery_discharge[t] = solution_in["discharge"][t]  # Keep positive for plotting
        if haskey(solution_in, "actual_charge")
            battery_actual_charge[t] = solution_in["actual_charge"][t]
        end
        if haskey(solution_in, "actual_discharge")
            battery_actual_discharge[t] = solution_in["actual_discharge"][t]
        end

        renewable_gen_solar[t] = solution_in["gen"][:Solar, t]
        renewable_gen_wind[t] = solution_in["gen"][:Wind, t]
        renewable_curt_solar[t] = solution_in["curt"][:Solar, t]  # Keep positive for plotting
        renewable_curt_wind[t] = solution_in["curt"][:Wind, t]  # Keep positive for plotting

        renewable_tot[t] = sum(generation_in[r][t] for r in renewables)
        SOC[t] = solution_in["state_of_charge"][t]
    end

    # Create a DataFrame for plotting
    # Wide formate
    solution_df_out = DataFrame(
        # Time = time_periods,
        Demand=demands,
        Gas_Power=gas_power,
        Solar_to_Grid=renewable_gen_solar,
        Wind_to_Grid=renewable_gen_wind,
        Battery_Apparant_Charge=battery_charge,
        Battery_Apparant_Discharge=battery_discharge,
        Battery_Effective_Charge=battery_actual_charge,
        Battery_Effective_Discharge=battery_actual_discharge,
        Solar_Curtailment=renewable_curt_solar,
        Wind_Curtailment=renewable_curt_wind,
        Total_Renewable_Produced=renewable_tot,
        State_of_Charge=SOC
    )
    # Long format
    # solution_df = DataFrame(
    #     Time=repeat(time_periods, outer=10), # 10 columns of data to be saved
    #     Power=vcat(
    #         demands,
    #         gas_power,
    #         renewable_gen_solar, renewable_gen_wind,
    #         battery_charge, battery_discharge,
    #         renewable_curt_solar, renewable_curt_wind,
    #         renewable_tot,
    #         SOC
    #     ),
    #     Type=repeat(
    #         [
    #             "Demand",
    #             "Gas Power",
    #             "Solar to Grid ", "Wind to Grid",
    #             "Battery Charge", "Battery Discharge",
    #             "Solar Curtailment", "Wind Curtailment",
    #             "Total Renewable Produced",
    #             "State of Charge"
    #         ],
    #         inner=length(demands)
    #     )
    # )

    return solution_df_out
end


####################################################################################################
# Function to save a single simulation result (CSV) for plotting and other analysis/validation in R
function single_sim_to_R(BALANCING_AUTHORITY, demands, time_periods, solution_in, generation_in, folder_path)

    # Initialize arrays to store data
    gas_power = zeros(length(demands)) # total output from NG plants
    battery_charge = zeros(length(demands)) # apparant charging rate
    battery_discharge = zeros(length(demands)) # apparant discharging rate
    battery_actual_charge = Vector{Union{Float64,Missing}}(missing, length(demands)) # effective energy flow into battery
    battery_actual_discharge = Vector{Union{Float64,Missing}}(missing, length(demands)) # effective energy flow out of battery
    renewable_gen_solar = zeros(length(demands)) # solar USED by the grid (load + charging)
    renewable_gen_wind = zeros(length(demands))  # wind USED by the grid (load + charging)
    renewable_curt_solar = zeros(length(demands))  # solar curtailments
    renewable_curt_wind = zeros(length(demands))  # wind curtailments

    # Fill in the arrays with data from the model
    for t in time_periods
        gas_power[t] = sum(solution_in["power"][p, t] for p in plants)

        battery_charge[t] = solution_in["charge"][t]
        battery_discharge[t] = solution_in["discharge"][t]  # Keep positive for plotting
        if haskey(solution_in, "actual_charge")
            battery_actual_charge[t] = solution_in["actual_charge"][t]
        end
        if haskey(solution_in, "actual_discharge")
            battery_actual_discharge[t] = solution_in["actual_discharge"][t]
        end

        renewable_gen_solar[t] = solution_in["gen"][:Solar, t]
        renewable_gen_wind[t] = solution_in["gen"][:Wind, t]
        renewable_curt_solar[t] = solution_in["curt"][:Solar, t]  # Keep positive for plotting
        renewable_curt_wind[t] = solution_in["curt"][:Wind, t]  # Keep positive for plotting
    end

    # Create a DataFrame for plotting
    data1 = DataFrame(
        Time=repeat(time_periods, outer=9),
        Power=vcat(gas_power,
            battery_charge,
            battery_discharge,
            battery_actual_charge,
            battery_actual_discharge,
            renewable_gen_solar,
            renewable_gen_wind,
            renewable_curt_solar,
            renewable_curt_wind,
        ),
        Type=repeat(["Gas_Power",
                "Battery_Charge",
                "Battery_Discharge",
                "Effective_Battery_Charge",
                "Effective_Battery_Discharge",
                "Solar_Generation",
                "Wind_Generation",
                "Solar_Curtailment",
                "Wind_Curtailment"],
            inner=8760)
    )

    # Write to CSV
    file1 = string(BALANCING_AUTHORITY, "_test_results.csv")
    path1 = joinpath(folder_path, file1)
    CSV.write(path1, data1)

    # Initialize another DataFrame to store demand, total renewable generation, and SOC
    renewable_tot = zeros(length(demands)) # total renewable energy PRODUCED each hour
    SOC = zeros(length(demands)) # state of charge of battery

    for t in time_periods
        renewable_tot[t] = sum(generation_in[r][t] for r in renewables)
        SOC[t] = solution_in["state_of_charge"][t]
    end

    data2 = DataFrame(
        Time=time_periods,
        Demand=demands,
        Renewable_Total=renewable_tot,
        State_of_Charge=SOC
    )

    file2 = string(BALANCING_AUTHORITY, "_test_demands.csv")
    path2 = joinpath(folder_path, file2)
    CSV.write(path2, data2)

end
