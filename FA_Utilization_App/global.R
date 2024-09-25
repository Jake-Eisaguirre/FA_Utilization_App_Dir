library(writexl)
library(shinyWidgets)
library(here)
library(shinycssloaders)
library(lubridate)
library(plotly)
library(DT)
library(padr)
library(tidyverse)
library(odbc)
library(DBI)
library(shiny)
library(shinymanager)

#source(here("FA_Utilization_App/creds.R"))

source(here("creds.R"))

bases <- c("HNL", "LAX")



# bid_periods <- read_csv(here("FA_Utilization_App", "data", "bid_periods.csv"))
# 
# fa_rsk_sop <- read_csv(here("FA_Utilization_App", "data", "fa_rsk_sop.csv"))%>%
#   mutate(weekday = wday(PAIRING_DATE, label = T)) %>%
#   rename(DATE = PAIRING_DATE)
# 
# hline_data <- read_csv(here("FA_Utilization_App", "data", "hline_data.csv")) %>%
#   mutate(weekday = wday(DATE, label = T))
# 
# label_data <- read_csv(here("FA_Utilization_App", "data", "label_data.csv"))%>%
#   mutate(weekday = wday(DATE, label = T))
# 
# label_data_used <- read_csv(here("FA_Utilization_App", "data", "label_data_used.csv"))%>%
#   mutate(weekday = wday(DATE, label = T))
# 
# long_data <- read_csv(here("FA_Utilization_App", "data", "long_data.csv"))%>%
#   mutate(weekday = wday(DATE, label = T))
# 
# long_data_deg_fa <- read_csv(here("FA_Utilization_App", "data", "long_data_deg_fa.csv"))%>%
#   mutate(weekday = wday(PAIRING_DATE, label = T)) %>%
#   rename(DATE = PAIRING_DATE)
# 
# sum_fa_rsk_sop_t <- read_csv(here("FA_Utilization_App", "data", "sum_fa_rsk_sop_t.csv"))%>%
#   mutate(weekday = wday(PAIRING_DATE, label = T)) %>%
#   rename(DATE = PAIRING_DATE)
# 
# 
# tot_label <- read_csv(here("FA_Utilization_App", "data", "tot_label.csv"))%>%
#   mutate(weekday = wday(DATE, label = T))
# 
# utl_df <- read_csv(here("FA_Utilization_App", "data", "utl_df.csv")) %>%
#   mutate(weekday = wday(DATE, label = T))


bid_periods <- read_csv(here("data", "bid_periods.csv"))

fa_rsk_sop <- read_csv(here("data", "fa_rsk_sop.csv"))%>%
  mutate(weekday = wday(PAIRING_DATE, label = T)) %>%
  rename(DATE = PAIRING_DATE)

hline_data <- read_csv(here("data", "hline_data.csv")) %>%
  mutate(weekday = wday(DATE, label = T))

label_data <- read_csv(here("data", "label_data.csv"))%>%
  mutate(weekday = wday(DATE, label = T))

label_data_used <- read_csv(here("data", "label_data_used.csv"))%>%
  mutate(weekday = wday(DATE, label = T))

long_data <- read_csv(here("data", "long_data.csv"))%>%
  mutate(weekday = wday(DATE, label = T))

long_data_deg_fa <- read_csv(here("data", "long_data_deg_fa.csv"))%>%
  mutate(weekday = wday(PAIRING_DATE, label = T)) %>%
  rename(DATE = PAIRING_DATE)

sum_fa_rsk_sop_t <- read_csv(here("data", "sum_fa_rsk_sop_t.csv"))%>%
  mutate(weekday = wday(PAIRING_DATE, label = T)) %>%
  rename(DATE = PAIRING_DATE)


tot_label <- read_csv(here("data", "tot_label.csv"))%>%
  mutate(weekday = wday(DATE, label = T))

utl_df <- read_csv(here("data", "utl_df.csv")) %>%
  mutate(weekday = wday(DATE, label = T))

dynamic_text_size <- function(x_range, num_points, base_size = 5) {
  # Calculate a scaling factor that works better for larger date ranges and few points
  scaling_factor <- (x_range / num_points) * 0.05  # Adjust multiplier to make text smaller for large ranges
  
  # Set an upper and lower bound for text size to prevent extreme values
  text_size <- base_size * scaling_factor
  text_size <- max(min(text_size, 7), 2)  # Ensure text size stays between 2 and 10
  
  return(text_size)
}
