#import Libraries ----------------------------------------------------------

library(tidyverse)

#import all files ----------------------------------------------------------

advertisers <- read_csv("advertiser.csv")
campaigns <- read_csv("campaigns.csv")
clicks <- read_tsv("clicks.tsv")
impressions <- read_tsv("impressions.tsv")


#rename columns in campaigns -----------------------------------------------

campaigns_selected <- campaigns %>%
  rename(campaign_id = id, campaign_name = name) %>%
  select(campaign_id, campaign_name, advertiser_id, budget)

# Rename columns in advertisers --------------------------------------------

advertiser_selected <- advertisers %>%
  rename(advertiser_id = ID,advertiser_name = name) %>%
  select(advertiser_id, advertiser_name)

# Join the data on advertiser_id -------------------------------------------

campaigns_advertisers <- campaigns_selected %>%
  left_join(advertiser_selected, by = "advertiser_id")


# Define timezone offsets --------------------------------------------------

timezone_offsets <- c(
  "UTC" = 0,
  "Eastern time" = 5,
  "Pacific time" = 8
)


# Function to manually adjust timezones ------------------------------------

adjust_to_utc <- function(data) {
  data %>%
    rowwise() %>%
    mutate(
      # Convert date to components
      day = as.numeric(substr(date, 1, 2)),
      month = as.numeric(substr(date, 4, 5)),
      year = as.numeric(substr(date, 7, 10)),

      # Split time into hours, minutes, and seconds
      hour = as.numeric(substr(time, 1, 2)),
      minute = as.numeric(substr(time, 4, 5)),
      second = as.numeric(substr(time, 7, 8)),
      
      # Adjust time by timezone offset
      utc_hour = hour + timezone_offsets[timezone],

      # Handle hour overflows and underflows
      carry_day = ifelse(utc_hour < 0, -1, ifelse(utc_hour >= 24, 1, 0)),
      adjusted_hour = utc_hour %% 24,
      
      # Adjust day, month, and year for rollovers
      adjusted_day = day + carry_day,
      days_in_month = c(31, 28 + ifelse((year %% 4 == 0) & (year %% 100 != 0 | year %% 400 == 0), 1, 0),
                        31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[month],

      final_day = ifelse(adjusted_day < 1, days_in_month[month - 1], 
                         ifelse(adjusted_day > days_in_month, 1, adjusted_day)),

      carry_month = ifelse(adjusted_day < 1, -1, ifelse(adjusted_day > days_in_month, 1, 0)),
      final_month = (month + carry_month - 1) %% 12 + 1,
      carry_year = (month + carry_month - 1) %/% 12,
      final_year = year + carry_year,
      
      # Final adjustments
      final_time = sprintf("%02d:%02d:%02d", adjusted_hour, minute, second),
      final_date = sprintf("%02d/%02d/%04d", final_day, final_month, final_year),
      
      # Update timezone to UTC
      timezone = "UTC"
      
    ) %>%
    ungroup() %>%
    select(-c(day, month, year, hour, minute, second, utc_hour, carry_day, 
              adjusted_day, carry_month, final_day, days_in_month, carry_year))
}


# Process clicks.csv -------------------------------------------------------
clicks_processed <- clicks %>%
  adjust_to_utc() %>%
  left_join(campaigns_advertisers, by = "campaign_id") %>%
  select(
    campaign_id, campaign_name, advertiser_id, advertiser_name,
    budget, date=final_date, time=final_time, timezone
  )

# Process impressions.csv --------------------------------------------------
impressions_processed <- impressions %>%
  adjust_to_utc() %>%
  left_join(campaigns_advertisers, by = "campaign_id") %>%
  select(
    campaign_id, campaign_name, advertiser_id, advertiser_name,
    budget, date=final_date, time=final_time, timezone
  )

# Save the processed data --------------------------------------------------
write_csv(clicks_processed, "clicks_processed.csv")
write_csv(impressions_processed, "impressions_processed.csv")