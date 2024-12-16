import pandas as pd
import os

base_path = os.path.expanduser('~/Documents/Portucrazy/Renewable/Lab/')

file_names = [
    'RE24_Laboratory_MarketPrice_TimeSeries.txt',
    'RE24_Laboratory_Demand_TimeSeries.txt',
    'RE24_Laboratory_Dimensionless_solar_power_production_TimeSeries.txt',
    'RE24_Laboratory_Dimensionless_wind_power_production_TimeSeries.txt',
    'RE24_Laboratory_Dimensionless_net_income_flowrate_TimeSeries.txt'
]

for file_name in file_names:
    if not os.path.isfile(os.path.join(base_path, file_name)):
        print(f"File not found: {file_name}")
        exit(1)
        
#load time series
market_price = pd.read_csv(os.path.join(base_path, file_names[0]), header=None, names=['price'], dtype={'price': float})
demand = pd.read_csv(os.path.join(base_path, file_names[1]), skiprows=1, header=None, names=['demand'], dtype={'demand': float})
solar_production = pd.read_csv(os.path.join(base_path, file_names[2]), header=None, names=['dimensionless'], dtype={'dimensionless': float})
wind_production = pd.read_csv(os.path.join(base_path, file_names[3]), header=None, names=['dimensionless'], dtype={'dimensionless': float})
net_income_flow_rate = pd.read_csv(os.path.join(base_path, file_names[4]), header=None, names=['flow_rate'], delimiter=r"\s+")
net_income_flow_rate['flow_rate'] = net_income_flow_rate['flow_rate'].str.replace(',', '.').astype(float)


#inputs hydropower plant
max_volume_upper = 1.060e9  # m^3
max_level_upper = 170  # m
min_level_upper = 167  # m
max_volume_lower = 3.834e7  # m^3
max_level_lower = 143.5  # m
min_level_lower = 140.5  # m
installed_capacity_solar = 4.03  # MW 
installed_capacity_wind = 5.63  # MW
q_factor = 100.29  # m^3/s

#convert dimensionless prodction to actual values
solar_energy = solar_production['dimensionless'] * installed_capacity_solar
wind_energy = wind_production['dimensionless'] * installed_capacity_wind

#set measured constants
efficiency = 0.9
gravity = 9.81  # m/s^2
head = (max_level_upper - min_level_upper)  # m
flow_rate = net_income_flow_rate['flow_rate']  # m^3/s

hydropower_energy = efficiency * q_factor * gravity * head * flow_rate / 1e6  # convert to MW

#total energy produced for each energy source
total_solar_energy = solar_energy.sum()
total_wind_energy = wind_energy.sum()
total_hydropower_energy = hydropower_energy.sum()

print(total_wind_energy)
print(total_solar_energy)
print(total_hydropower_energy)

#total energy produced across all sources
total_energy = total_solar_energy + total_wind_energy + total_hydropower_energy

#combine all energy production into a single DataFrame
energy_production = solar_energy + wind_energy + hydropower_energy
energy_production = energy_production.reset_index(drop=True)

#thresholds
price_threshold = market_price['price'].quantile(0.8)  
demand_threshold = demand['demand'].quantile(0.9)  

#initialize variables
stored_energy = 0.0  # MWh
storage_capacity = 1000.0  # MWh
sold_energy = 0.0  # MWh
hoarded_energy = 0.0  # MWh

#decision logic
sell_decisions = []
for i in range(len(market_price)):
    current_price = market_price['price'][i]
    current_demand = demand['demand'][i]
    current_production = energy_production[i]
    
    #determine if we should sell or store energy
    if current_price >= price_threshold and current_demand >= demand_threshold:
        sell_amount = min(current_production + stored_energy, current_demand)  #sell up to the current demand
        sold_energy += sell_amount
        stored_energy -= sell_amount
        sell_decisions.append('Sell')
    else:
        store_amount = current_production
        if stored_energy + store_amount <= storage_capacity:
            stored_energy += store_amount
            hoarded_energy += store_amount
            sell_decisions.append('Hoard')
        else:
            excess_energy = (stored_energy + store_amount) - storage_capacity
            stored_energy = storage_capacity
            sold_energy += excess_energy
            sell_decisions.append('Sell Excess')

#add decisions to DataFrame for analysis
results = pd.DataFrame({
    'price': market_price['price'],
    'demand': demand['demand'],
    'production': energy_production,
    'decision': sell_decisions
})

#print summary
print(f"Total sold energy: {sold_energy} MWh")
print(f"Total hoarded energy: {hoarded_energy} MWh")
print(results)

#calculate revenue for each time period
solar_revenue = solar_energy * market_price['price'] 
wind_revenue = wind_energy * market_price['price']
hydropower_revenue = hydropower_energy * market_price['price']

#total revenue
total_revenue = solar_revenue.sum() + wind_revenue.sum() + hydropower_revenue.sum()

discount_rate = 0.07
project_lifetime = len(net_income_flow_rate)

#estimated costs (example values)
capital_cost_solar = 800 * installed_capacity_solar  # €/MW
capital_cost_wind = 1000 * installed_capacity_wind  # €/MW
capital_cost_hydro = 20 * (max_volume_upper + max_volume_lower) / 1e6  # €/m^3

operational_cost_annual = 0.02 * (capital_cost_solar + capital_cost_wind + capital_cost_hydro)  # 2% annual O&M cost

#total costs
total_capital_cost = capital_cost_solar + capital_cost_wind + capital_cost_hydro
total_operational_cost = operational_cost_annual * project_lifetime
total_cost = total_capital_cost + total_operational_cost

#calculate ROI
roi = (total_revenue - total_cost) / total_cost

#calculate LCOE
lcoe = total_cost / total_energy

print(f"Total revenue: {total_revenue} €")
print(f"Total cost: {total_cost} €")
print(f"ROI: {roi}")
print(f"LCOE: {lcoe} €/MWh")