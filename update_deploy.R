if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}

# librarian downloads, if not already downloaded, and reads in needed packages
librarian::shelf(writexl, shinyWidgets, here, shinycssloaders, lubridate, plotly, DT, padr, 
                 tidyverse, odbc, DBI, shiny)

raw_date <- Sys.Date()

fut_bid_period <- substr(as.character((raw_date +30)), 1, 7)
# Database connection
# 
db_connection <- dbConnect(odbc::odbc(),
                           Driver = "SnowflakeDSIIDriver",
                           Server = "hawaiianair.west-us-2.azure.snowflakecomputing.com",
                           WAREHOUSE = "DATA_LAKE_READER",
                           Database = "ENTERPRISE",
                           UID = "jacob.eisaguirre@hawaiianair.com",
                           authenticator = "externalbrowser")

dbExecute(db_connection, "USE SCHEMA CREW_ANALYTICS")


# Fetch bid periods for selectInput
q_bid_periods <- "SELECT DISTINCT BID_PERIOD FROM CT_MASTER_HISTORY ORDER BY BID_PERIOD DESC;"
bid_periods <- dbGetQuery(db_connection, q_bid_periods) %>% 
  filter(!BID_PERIOD == fut_bid_period) %>% 
  filter(BID_PERIOD > "2021-03")

write_csv(bid_periods, here("FA_Utilization_App", "data", "bid_periods.csv"))


utl_df <- data.frame()

long_data <- data.frame()

label_data <- data.frame()

label_data_used <- data.frame()

hline_data <- data.frame()

tot_label <- data.frame()


# Define the bases
bases <- c("LAX", "HNL")

# Loop over bid periods
for (j in seq_along(bases)) {
  
  # Loop over bases
  for (i in seq_along(bid_periods$BID_PERIOD)) {
    
    q_master_history <- paste0("SELECT * FROM CT_MASTER_HISTORY WHERE BID_PERIOD = '", bid_periods$BID_PERIOD[i], "' 
                               AND CREW_INDICATOR = 'FA' AND BASE = '", bases[j], "';")
    
    master_history_raw <- dbGetQuery(db_connection, q_master_history) %>%
      mutate(UPDATE_TIME = as.character(UPDATE_TIME),
             UPDATE_DATE = as.character(UPDATE_DATE))
    
    update_dt_rlv <- paste0((as_datetime(paste0(bid_periods$BID_PERIOD[i], "-24 00:00:00")) - months(1)), " 00:00:00") 
    
    fa_ut_rlv <- master_history_raw %>% 
      ungroup() %>% 
      filter(TRANSACTION_CODE %in% c("RLV")) %>%
      mutate(update_dt = as_datetime(paste(UPDATE_DATE, UPDATE_TIME, sep = " "))) %>%
      filter(update_dt < as_datetime(update_dt_rlv)) %>%
      group_by(CREW_ID, PAIRING_DATE, TRANSACTION_CODE) %>%
      mutate(temp_id = cur_group_id()) %>%
      filter(!duplicated(temp_id)) %>%
      ungroup() %>% 
      select(CREW_INDICATOR, CREW_ID, TRANSACTION_CODE, PAIRING_NO,
             PAIRING_DATE, TO_DATE, PAIRING_POSITION, BID_PERIOD, BASE, update_dt)
    
    fa_ut_asn <- master_history_raw %>% 
      ungroup() %>% 
      filter(TRANSACTION_CODE %in% c("ACR", "RSV", "RLV", "ASN", "BSN", "BRD")) %>% 
      mutate(update_dt = as_datetime(paste(UPDATE_DATE, UPDATE_TIME, sep = " "))) %>%
      group_by(CREW_ID, PAIRING_DATE, TRANSACTION_CODE) %>%
      mutate(temp_id = cur_group_id()) %>%
      filter(!duplicated(temp_id)) %>%
      ungroup() %>% 
      select(CREW_INDICATOR, CREW_ID, TRANSACTION_CODE, PAIRING_NO,
             PAIRING_DATE, TO_DATE, PAIRING_POSITION, BID_PERIOD, BASE, update_dt)
    
    fa_ut_single <- fa_ut_asn %>% 
      group_by(CREW_ID, PAIRING_NO) %>% 
      mutate(single = if_else(PAIRING_DATE == TO_DATE, 1, 0)) %>% 
      filter(single == 1) %>% 
      filter(TRANSACTION_CODE %in% c("ASN", "BSN", "BRD")) %>% 
      pivot_longer(cols = c("PAIRING_DATE", "TO_DATE"),
                   values_to = "DATE") %>% 
      group_by(CREW_ID, TRANSACTION_CODE, DATE, PAIRING_NO) %>% 
      mutate(temp_id = cur_group_id()) %>% 
      filter(!duplicated(temp_id)) %>%
      ungroup() 
    
    fa_ut_double <- fa_ut_asn %>% 
      group_by(CREW_ID, PAIRING_NO) %>% 
      mutate(single = if_else(PAIRING_DATE == TO_DATE, 1, 0)) %>% 
      filter(single == 0) %>% 
      filter(TRANSACTION_CODE %in% c("ASN", "BSN", "BRD")) %>% 
      pivot_longer(cols = c("PAIRING_DATE", "TO_DATE"),
                   values_to = "DATE") %>% 
      group_by(CREW_ID, TRANSACTION_CODE, DATE, PAIRING_NO) %>% 
      mutate(temp_id = cur_group_id()) %>%
      filter(!duplicated(temp_id)) %>%
      ungroup() %>% 
      group_by(CREW_ID, BASE, PAIRING_NO) %>% 
      pad(by="DATE") %>% 
      ungroup() 
    
    piv_emp_hist_fa_asn <- rbind(fa_ut_double, fa_ut_single) %>% 
      group_by(DATE, BASE) %>% 
      mutate(rlv_used = n()) %>% 
      ungroup() %>% 
      select(DATE, BASE, rlv_used)
    
    piv_emp_hist_fa_rlv <- fa_ut_rlv %>% 
      mutate(update_dt = as_datetime(update_dt)) %>% 
      filter(TRANSACTION_CODE %in% c("RLV")) %>%
      rename(DATE = PAIRING_DATE) %>% 
      group_by(DATE, BASE) %>% 
      mutate(net_rlv_available = length(unique(CREW_ID))) %>% 
      ungroup() %>% 
      select(DATE, BASE, net_rlv_available)
    
    comb_fa <- left_join(piv_emp_hist_fa_rlv, piv_emp_hist_fa_asn, relationship = "many-to-many",
                         by = join_by(DATE, BASE)) %>% 
      group_by(DATE, BASE) %>% 
      mutate(temp_id = cur_group_id()) %>% 
      filter(!duplicated(temp_id)) %>% 
      select(-temp_id) %>%
      ungroup() %>% 
      mutate(rlv_used = if_else(is.na(rlv_used), 0, rlv_used)) %>% 
      mutate(rlv_remaining = net_rlv_available - rlv_used) %>% 
      mutate(rlv_remaining = if_else(rlv_remaining < 0, 0, rlv_remaining)) %>% 
      mutate(perc_ut = round((rlv_used/net_rlv_available)*100, 0)) %>% 
      group_by(DATE) %>% 
      mutate(m_avg_utl = round(mean(rlv_used), 0),
             m_perc_utl = paste0(round(mean(perc_ut), 0)),
             perc_ut = paste0(perc_ut, "%"),
             BID_PERIOD = bid_periods$BID_PERIOD[i])
    
    # Append results
    utl_df <- rbind(utl_df, comb_fa)
    
    
    # Reshape data to long format
    long_data_fa_hnl <- comb_fa %>%
      select(DATE, BASE, rlv_used, rlv_remaining) %>%
      pivot_longer(cols = c(rlv_used, rlv_remaining),
                   names_to = "rlv_type",
                   values_to = "Head_Count") %>%
      mutate(rlv_type=if_else(rlv_type == "rlv_remaining", "RLV Available", "RLV Utilized"))%>% 
      mutate(BID_PERIOD = bid_periods$BID_PERIOD[i])
    
    long_data <- rbind(long_data_fa_hnl, long_data)
    
    
    label_data_hnl <- comb_fa %>%
      select(DATE, BASE, net_rlv_available) %>% 
      mutate(BID_PERIOD = bid_periods$BID_PERIOD[i])
    
    label_data <- rbind(label_data_hnl, label_data)
    
    label_data_hnl_used <- comb_fa %>%
      select(DATE, BASE, rlv_used, perc_ut)%>% 
      mutate(BID_PERIOD = bid_periods$BID_PERIOD[i])
    
    label_data_used <- rbind(label_data_used, label_data_hnl_used)
    
    hline_data_hnl <- data.frame(
      yintercept = utl_df$m_avg_utl,
      BASE = utl_df$BASE,
      BID_PERIOD = utl_df$BID_PERIOD,
      DATE = utl_df$DATE)
    
    hline_data <- rbind(hline_data_hnl, hline_data)
    
    
    tot_rlv_label <- fa_ut_rlv %>%
      reframe(tot = length(unique(CREW_ID)),
              tot_no_lab = length(unique(CREW_ID)),
              BID_PERIOD = BID_PERIOD,
              BASE = BASE,
              DATE = PAIRING_DATE)
    
    tot_label <- rbind(tot_rlv_label, tot_label)
    
    
    
    # Print status
    print(paste0("Completed BID_PERIOD: ", bid_periods$BID_PERIOD[i], " BASE: ", bases[j]))
    
  } # End of base loop
  
} # End of bid period loop


write_csv(tot_label, here("FA_Utilization_App", "data", "tot_label.csv"))

write_csv(hline_data, here("FA_Utilization_App", "data", "hline_data.csv"))

write_csv(label_data_used, here("FA_Utilization_App", "data", "label_data_used.csv"))

write_csv(long_data, here("FA_Utilization_App", "data", "long_data.csv"))

write_csv(utl_df, here("FA_Utilization_App", "data", "utl_df.csv"))

write_csv(label_data, here("FA_Utilization_App", "data", "label_data.csv"))


fa_rsk_sop <- data.frame()

long_data_deg_fa <- data.frame()

sum_fa_rsk_sop_t <- data.frame()

# Define the bases
bases <- c("LAX", "HNL")

# Loop over bid periods
for (j in seq_along(bases)) {
  
  # Loop over bases
  for (i in seq_along(bid_periods$BID_PERIOD)) {
    
    q_master_history <- paste0("SELECT * FROM CT_MASTER_HISTORY WHERE BID_PERIOD = '", bid_periods$BID_PERIOD[i], "' 
                               AND CREW_INDICATOR = 'FA' AND BASE = '", bases[j], "';")
    
    master_history_raw <- dbGetQuery(db_connection, q_master_history) %>%
      mutate(UPDATE_TIME = as.character(UPDATE_TIME),
             UPDATE_DATE = as.character(UPDATE_DATE))
    
    fa_trx_hist <- master_history_raw %>% 
      mutate(update_dt = paste(UPDATE_DATE, UPDATE_TIME, sep = " ")) %>% 
      group_by(CREW_ID, PAIRING_DATE, TRANSACTION_CODE) %>% 
      mutate(temp_id = cur_group_id()) %>% 
      filter(!duplicated(temp_id)) %>% 
      select(!temp_id) %>% 
      ungroup() %>% 
      group_by(CREW_ID, PAIRING_DATE) %>% 
      mutate(keep = ifelse(any(TRANSACTION_CODE %in% c("SCR", "ASN", "RSV", "ARC", "RLV")), 1, 0)) %>% 
      filter(keep == 1)
    
    
    sick_follow_asn <- fa_trx_hist %>% 
      group_by(CREW_ID, PAIRING_DATE) %>% 
      mutate(keep = if_else(any(TRANSACTION_CODE %in% c("SOP", "2SK", "FLV", "FLP", "UNA", "FLS", "FLU",
                                                        "N/S", "PER", "MGR")), 1, 0)) %>% 
      filter(keep == 1)
    
    
    sum_fa_rsk_sop <- sick_follow_asn %>% 
      group_by(CREW_ID, PAIRING_DATE) %>% 
      mutate(keep = if_else(any(TRANSACTION_CODE %in% c("ASN")), 1, 0)) %>%
      filter(keep == 1,
             TRANSACTION_CODE %in% c("SOP", "2SK", "FLV", "FLP", "UNA", "FLS", "FLU",
                                     "N/S", "PER", "MGR")) %>% 
      dplyr::arrange(CREW_ID, PAIRING_DATE, update_dt) %>% 
      filter(update_dt == max(update_dt)) %>% 
      group_by(PAIRING_POSITION, BASE, PAIRING_DATE, TRANSACTION_CODE, BID_PERIOD) %>% 
      reframe(rlv_asn_2sk = if_else(TRANSACTION_CODE == "2SK", length(unique(CREW_ID)), NA),
              rlv_asn_sop = if_else(TRANSACTION_CODE == "SOP", length(unique(CREW_ID)), NA),
              rlv_asn_flv = if_else(TRANSACTION_CODE == "FLV", length(unique(CREW_ID)), NA),
              rlv_asn_flp = if_else(TRANSACTION_CODE == "FLP", length(unique(CREW_ID)), NA),
              rlv_asn_una = if_else(TRANSACTION_CODE == "UNA", length(unique(CREW_ID)), NA),
              rlv_asn_fls = if_else(TRANSACTION_CODE == "FLS", length(unique(CREW_ID)), NA),
              rlv_asn_flu = if_else(TRANSACTION_CODE == "FLU", length(unique(CREW_ID)), NA),
              rlv_asn_ns = if_else(TRANSACTION_CODE == "N/S", length(unique(CREW_ID)), NA),
              rlv_asn_per = if_else(TRANSACTION_CODE == "PER", length(unique(CREW_ID)), NA),
              rlv_asn_mgr = if_else(TRANSACTION_CODE == "MGR", length(unique(CREW_ID)), NA)) %>% 
      ungroup() %>% 
      select(PAIRING_POSITION, BASE, BID_PERIOD, PAIRING_DATE, rlv_asn_2sk, rlv_asn_sop, rlv_asn_flv, rlv_asn_flp,
             rlv_asn_una, rlv_asn_fls, rlv_asn_flu, rlv_asn_ns, rlv_asn_per, rlv_asn_mgr ) 
    
    fa_rsk_sop <- rbind(sum_fa_rsk_sop, fa_rsk_sop)
    
    
    # Reshape data to long format
    long_data_deg_fa_hnl <- sum_fa_rsk_sop %>%
      select(PAIRING_DATE, BASE, BID_PERIOD, rlv_asn_2sk, rlv_asn_sop, rlv_asn_flv, rlv_asn_flp,
             rlv_asn_una, rlv_asn_fls, rlv_asn_flu, rlv_asn_ns, rlv_asn_per, rlv_asn_mgr) %>%
      pivot_longer(cols = c(rlv_asn_2sk, rlv_asn_sop, rlv_asn_flv, rlv_asn_flp,
                            rlv_asn_una, rlv_asn_fls, rlv_asn_flu, rlv_asn_ns, rlv_asn_per, rlv_asn_mgr),
                   names_to = "sick_type",
                   values_to = "Head_Count") %>%
      drop_na(Head_Count) %>%
      mutate(sick_type=case_when(sick_type == "rlv_asn_2sk" ~ "2SK",
                                 sick_type == "rlv_asn_sop" ~ "SOP",
                                 sick_type == "rlv_asn_flv" ~ "FLV",
                                 sick_type == "rlv_asn_flp" ~ "FLP",
                                 sick_type == "rlv_asn_una" ~ "UNA",
                                 sick_type == "rlv_asn_fls" ~ "FLS",
                                 sick_type == "rlv_asn_flu" ~ "FLU",
                                 sick_type == "rlv_asn_ns" ~ "N/S",
                                 sick_type == "rlv_asn_per" ~ "PER",
                                 sick_type == "rlv_asn_mgr" ~ "MGR")) %>%
      group_by(PAIRING_DATE, BASE, sick_type) %>%
      mutate(temp_id = cur_group_id()) %>%
      filter(!duplicated(temp_id))
    
    long_data_deg_fa <- rbind(long_data_deg_fa_hnl ,long_data_deg_fa)
    
    
    
    sum_fa_rsk_sop_t_hnl <- sick_follow_asn %>% 
      group_by(CREW_ID, PAIRING_DATE) %>% 
      mutate(keep = if_else(any(TRANSACTION_CODE %in% c("ASN")), 1, 0)) %>%
      filter(keep == 1) %>% 
      filter(TRANSACTION_CODE %in% c("ASN", "2SK", "SOP", "FLV", "FLP", "UNA", "FLS", "FLU",
                                     "N/S", "PER", "MGR")) %>% 
      select(CREW_ID, TRANSACTION_CODE, PAIRING_DATE, update_dt, BASE, BID_PERIOD) %>% 
      arrange(PAIRING_DATE, CREW_ID, update_dt) %>% 
      rename(UPDATE_DATE_TIME = update_dt)
    
    wrap_time <- master_history_raw %>% 
      filter(TRANSACTION_CODE %in% c("RLV"),
             is.na(PAIRING_NO)) %>% 
      mutate(update_dt = paste(UPDATE_DATE, UPDATE_TIME, sep = " "),
             wrap_dif = TO_TIME - FROM_TIME) %>% 
      group_by(CREW_ID, PAIRING_DATE) %>% 
      filter(wrap_dif == max(wrap_dif)) %>% 
      ungroup() %>% 
      group_by(CREW_ID, PAIRING_DATE, TRANSACTION_CODE) %>% 
      mutate(temp_id = cur_group_id()) %>% 
      filter(!duplicated(temp_id)) %>% 
      ungroup() %>% 
      select(CREW_ID, PAIRING_DATE, FROM_TIME, TO_TIME) 
    
    sum_fa_rsk_sop_t_hnl <- left_join(sum_fa_rsk_sop_t_hnl, wrap_time, by = c("CREW_ID", "PAIRING_DATE"))
    
    sum_fa_rsk_sop_t <- rbind(sum_fa_rsk_sop_t_hnl, sum_fa_rsk_sop_t)
    
    
    #         # Print status
    print(paste0("Completed BID_PERIOD: ", bid_periods$BID_PERIOD[i], " BASE: ", bases[j]))
    
  }
}

write_csv(fa_rsk_sop, here("FA_Utilization_App", "data", "fa_rsk_sop.csv"))

write_csv(long_data_deg_fa, here("FA_Utilization_App", "data", "long_data_deg_fa.csv"))

write_csv(sum_fa_rsk_sop_t, here("FA_Utilization_App", "data", "sum_fa_rsk_sop_t.csv"))


rsconnect::deployApp(here("FA_Utilization_App"), appName = "FA_Utilization")

