library(tidyverse)
library(fpp2)

#notes -- why not 284?
  ## a few not incorporated til post 2000 but still in with missing data
  ## add exponential smoothing
  ## exponential could be average of two years or something 

df <- readxl::read_excel("./muni_data.xlsx", skip = 1) |> 
  janitor::clean_names() 

df_trim <- df |> 
  select(name = x1, x1950:x2020)


# Linear A -- 2010 - 2020 ------------------------------------------------

df_trim <- df_trim |> 
  mutate(growth_rate_2010_2020 = x2020 - x2010,
         lin_a_2020 = x2020,
         lin_a_2030 = x2020 + growth_rate_2010_2020,
         lin_a_2040 = x2020 + (growth_rate_2010_2020*2),
         lin_a_2050 = x2020 + (growth_rate_2010_2020*3))|> 
  select(!growth_rate_2010_2020)

# Linear B -- 2000  - 2020 ---------------------------------------------------

df_trim <- df_trim |> 
  mutate(growth_rate_2000_2020 = (x2020 - x2000)/2,
         lin_b_2020 = x2020,
         lin_b_2030 = x2020 + growth_rate_2000_2020,
         lin_b_2040 = x2020 + (growth_rate_2000_2020*2),
         lin_b_2050 = x2020 + (growth_rate_2000_2020*3)) |> 
  select(!growth_rate_2000_2020)

# Expo C  -- 2010 - 2020 ---------------------------------------------------

df_trim <- df_trim |> 
  mutate(growth_rate_2010_2020_exp = x2020/x2010,
         exp_c_2020 = x2020,
         exp_c_2030 = x2020 * growth_rate_2010_2020_exp,
         exp_c_2040 = x2020 * (growth_rate_2010_2020_exp^2),
         exp_c_2050 = x2020 * (growth_rate_2010_2020_exp^3)) |> 
  select(!growth_rate_2010_2020_exp)

# Expo D -- 2000 - 2020 ---------------------------------------------------

df_trim <- df_trim |> 
  mutate(growth_rate_2000_2020_exp = x2020/x2000,
         exp_d_2020 = x2020,
         exp_d_2030 = x2020 * growth_rate_2000_2020_exp,
         exp_d_2040 = x2020 * (growth_rate_2000_2020_exp^2),
         exp_d_2050 = x2020 * (growth_rate_2000_2020_exp^3)) |> 
  select(!growth_rate_2000_2020_exp)


# Expo E (Smoothing) ------------------------------------------------------
ets



saveRDS(df_trim,"trends.rdata")