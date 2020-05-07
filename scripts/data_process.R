library(tidyverse)
library(lubridate)
library(rvest)
library(stringdist)

# Function to read the raw CSV files. The files are aggregated to the country
# level and then converted to long format

clean_jhd_to_long <- function(df) {
  df_str <- deparse(substitute(df))
  var_str <- substr(df_str, 1, str_length(df_str) - 4)

  df %>%
    group_by(`Country/Region`) %>%
    filter(`Country/Region` != "Cruise Ship") %>%
    select(-`Province/State`, -Lat, -Long) %>%
    mutate_at(vars(-group_cols()), sum) %>%
    distinct() %>%
    ungroup() %>%
    rename(country = `Country/Region`) %>%
    pivot_longer(
      -country,
      names_to = "date_str",
      values_to = var_str
    ) %>%
    mutate(date = mdy(date_str)) %>%
    select(country, date, !!sym(var_str))
}

confirmed_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")


jh_covid19_data <- clean_jhd_to_long(confirmed_raw) %>%
  full_join(clean_jhd_to_long(deaths_raw))


# Next, I pull official country level indicators from the UN Statstics Division
# to get country level identifiers.

jhd_countries <- tibble(country = unique(jh_covid19_data$country)) %>% arrange(country)

ctry_ids <- read_html("https://unstats.un.org/unsd/methodology/m49/") %>%
  html_table()
un_m49 <- ctry_ids[[1]]
colnames(un_m49) <- c("country", "un_m49", "iso3c")


# Merging by country name is messy. I start with a fuzzy matching approach
# using the {stringdist} package

ctry_names_dist <- matrix(NA, nrow = nrow(jhd_countries), ncol = nrow(un_m49))
for (i in 1:length(jhd_countries$country)) {
  for (j in 1:length(un_m49$country)) {
    ctry_names_dist[i, j] <- stringdist(
      tolower(jhd_countries$country[i]),
      tolower(un_m49$country[j])
    )
  }
}

min_ctry_name_dist <- apply(ctry_names_dist, 1, min)

matched_ctry_names <- NULL

for (i in 1:nrow(jhd_countries)) {
  un_m49_row <- match(min_ctry_name_dist[i], ctry_names_dist[i, ])
  if (length(which(ctry_names_dist[i, ] %in% min_ctry_name_dist[i])) > 1) un_m49_row <- NA
  matched_ctry_names <- rbind(
    matched_ctry_names,
    tibble(
      jhd_countries_row = i,
      un_m49_row = un_m49_row,
      jhd_ctry_name = jhd_countries$country[i],
      un_m49_name = ifelse(is.na(un_m49_row), NA,
        un_m49$country[un_m49_row]
      )
    )
  )
}

# This matches most cases well but some cases need to be adjusted by hand.
# In addition there are two jurisdictions (Kosovo, Taiwan)
# that cannot be matched as they are no 'country' as far as the U.N.
# Statistics Devision is concerned.

# WATCH OUT: The data from JHU is subject to change without notice.
# New countries are being added and names/spelling might change.
# Also, in the long run, the data provided by the UNSD might change.
# Inspect 'matched_ctry_names' before using the data.

matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Bolivia"] <- 27
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Brunei"] <- 35
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Congo (Brazzaville)"] <- 54
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Congo (Kinshasa)"] <- 64
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "East Timor"] <- 222
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Iran"] <- 109
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Korea, South"] <- 180
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Kosovo"] <- NA
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Moldova"] <- 181
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Russia"] <- 184
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Taiwan*"] <- NA
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Tanzania"] <- 236
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "United Kingdom"] <- 235
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "US"] <- 238
matched_ctry_names$un_m49_row[matched_ctry_names$jhd_ctry_name == "Venezuela"] <- 243

# Last Step: Match country identifier data and save file (commented out here)
jhd_countries %>%
  left_join(matched_ctry_names %>%
    select(jhd_ctry_name, un_m49_row),
  by = c(country = "jhd_ctry_name")
  ) %>%
  left_join(un_m49 %>% mutate(un_m49_row = row_number()), by = "un_m49_row") %>%
  rename(country = country.x) %>%
  select(country, iso3c) -> jhd_countries

jh_covid19_data <- jh_covid19_data %>%
  left_join(jhd_countries) %>%
  select(country, iso3c, date, confirmed, deaths)

jh_covid19_data[jh_covid19_data$country == "Taiwan*", "iso3c"] <- "TWN"
jh_covid19_data[jh_covid19_data$country == "US", "country"] <- "USA"

write_csv(jh_covid19_data, sprintf("data/jh_covid19_data_%s.csv", Sys.Date()))
