library(tidyverse)

#notes -- why not 284?
  ## a few not incorporated til post 2000 but still in with misisng data
  ## add exponential smoothing
  ## exponential could be average of two years or something 

df <- readxl::read_excel("./muni_data.xlsx", skip = 1) |> 
  janitor::clean_names() 

df_trim <- df |> 
  select(name = x1, x1950:x2020)


# Linear A -- 2000 - 2010 ------------------------------------------------

df_trim <- df_trim |> 
  mutate(growth_rate_2000_2010 = x2010 - x2000,
         lin_a_2020 = x2010 + growth_rate_2000_2010,
         lin_a_2030 = x2010 + (growth_rate_2000_2010*2),
         lin_a_2040 = x2010 + (growth_rate_2000_2010*3),
         lin_a_2050 = x2010 + (growth_rate_2000_2010*4)) |> 
  select(!growth_rate_2000_2010)

# Linear B -- 1990  -2010 ---------------------------------------------------

df_trim <- df_trim |> 
  mutate(growth_rate_1990_2010 = (x2010 - x1990)/2,
         lin_b_2020 = x2010 + growth_rate_1990_2010,
         lin_b_2030 = x2010 + (growth_rate_1990_2010*2),
         lin_b_2040 = x2010 + (growth_rate_1990_2010*3),
         lin_b_2050 = x2010 + (growth_rate_1990_2010*4)) |> 
  select(!growth_rate_1990_2010)

# Expo C  -- 2000 - 2010 ---------------------------------------------------

df_trim <- df_trim |> 
  mutate(growth_rate_2000_2010_exp = x2010/x2000,
         exp_c_2020 = x2010 * growth_rate_2000_2010_exp,
         exp_c_2030 = x2010 * (growth_rate_2000_2010_exp^2),
         exp_c_2040 = x2010 * (growth_rate_2000_2010_exp^3),
         exp_c_2050 = x2010 * (growth_rate_2000_2010_exp^4)) |> 
  select(!growth_rate_2000_2010_exp)

# Expo D -- 1990 - 2010 ---------------------------------------------------

df_trim <- df_trim |> 
  mutate(growth_rate_1990_2010_exp = x2010/x1990,
         exp_d_2020 = x2010 * growth_rate_1990_2010_exp,
         exp_d_2030 = x2010 * (growth_rate_1990_2010_exp^2),
         exp_d_2040 = x2010 * (growth_rate_1990_2010_exp^3),
         exp_d_2050 = x2010 * (growth_rate_1990_2010_exp^4)) |> 
  select(!growth_rate_1990_2010_exp)

saveRDS(df_trim,"trends.rdata")

# Expo E (Smoothing) ------------------------------------------------------


