library(tidyverse)
library(plotly)
library(cmapplot)
library(ggtext)

options(scipen = 999)

df <- read_rds("trends.rdata")

df %>%
  filter(if_any(lin_a_2020:exp_d_2050, is.na))

# data frame with census data
df1 <- df %>%
  pivot_longer(x1950:x2020,
               names_to = "year",
               values_to = "value") %>%
  select("name", "year", "value") %>%
  mutate("trend" = "census") %>%
  rename(muni = name)

df1$year <- as.numeric(gsub("x", "", df1$year)) # remove x's from year column

# dataframe of forecast data
df2 <- df %>%
  pivot_longer(lin_a_2020:exp_d_2050,
               names_to = "forecast_method_year",
               values_to = "value") %>%
  separate(forecast_method_year,
           into = c("trend", "trend_type", "year"),
           sep ="_") %>%
  unite("trend", "trend":"trend_type", sep = "_",
        remove = T) %>%
  select("name", "trend", "year", "value") %>%
  rename(muni = name)

df2$year <- as.numeric(df2$year) # convert year to numeric data type

df_tidy <- rbind(df1, df2) # combine dataframes


df_tidy %>%
  group_by(muni) %>%
  arrange("year") 


# rename trends
df_tidy[ df_tidy == "census"] <- "Census Data 2000 - 2020"
df_tidy[ df_tidy == "exp_c"] <- "Exponential Trend: 2010 - 2020"
df_tidy[ df_tidy == "exp_d"] <- "Exponential Trend: 2000 - 2020"
df_tidy[ df_tidy == "lin_a"] <- "Linear Trend: 2010 - 2020"
df_tidy[ df_tidy == "lin_b"] <- "Linear Trend: 2000 - 2020"

df_tidy <- df_tidy %>%
  mutate(trend = as_factor(trend))



na <- df_tidy %>%
  filter(is.na(value))
  

palette <- (get_cmapplot_global("palettes"))

str(palette$colors)

saveRDS(df_tidy, "trendstidy.rdata")
