source(here("global.R"))
source(here("ui.R"))

#source(here("FA_Utilization_App/global.R"))
#source(here("FA_Utilization_App/ui.R"))

# Define server logic
server <- function(input, output, session) {
  
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  
  

  reserve_utl <- reactive({
    utl_df %>% 
      filter(BID_PERIOD %in% c(input$bid_periods_input),
             BASE == input$bases,
             weekday %in% c(input$weekday)) %>% 
      mutate(m_perc_utl = round(mean(m_perc_utl), 0),
             m_avg_utl = round(mean(m_avg_utl), 0)) 
  })
  
  # Download handler for Excel file
  output$download_utl_table <- downloadHandler(
    filename = function() {
      paste("utl_table_", input$bid_periods_input, ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(reserve_utl(), path = file)
    }
  )
  
  
  
  
  h_line_label_p_utl <- reactive({
    utl_df %>% 
      filter(BID_PERIOD %in% c(input$bid_periods_input),
             BASE == input$bases,
             weekday %in% c(input$weekday)) %>% 
      group_by(BASE) %>% 
      reframe(m_perc_utl = round(mean(m_perc_utl), 0),
              m_avg_utl = round(mean(m_avg_utl), 0))
  })
  
  tot_label_rec <- reactive({
    tot_label %>% 
      filter(BID_PERIOD %in% c(input$bid_periods_input),
             BASE == input$bases,
             weekday %in% c(input$weekday)) %>% 
      group_by(BASE, BID_PERIOD) %>%
      mutate(temp_id = cur_group_id()) %>% 
      filter(!duplicated(temp_id)) %>% 
      ungroup() %>% 
      reframe(tot = sum(tot))
  })
  
  hline_data_rec <- reactive({
    hline_data %>% 
      filter(BID_PERIOD %in% c(input$bid_periods_input),
             BASE == input$bases,
             weekday %in% c(input$weekday)) %>% 
      group_by(BASE, BID_PERIOD) %>% 
      distinct() %>% 
      ungroup() %>% 
      reframe(yintercept = mean(yintercept))
  })
  
  label_data_used_rec <- reactive({
    label_data_used %>% 
      filter(BID_PERIOD %in% c(input$bid_periods_input),
             BASE == input$bases,
             weekday %in% c(input$weekday))
  })
  
  long_data_rec <- reactive({
    long_data %>% 
      filter(BID_PERIOD %in% c(input$bid_periods_input),
             BASE == input$bases,
             weekday %in% c(input$weekday))
  })
  
  label_data_rec <- reactive({
    label_data %>% 
      filter(BID_PERIOD %in% c(input$bid_periods_input),
             BASE == input$bases,
             weekday %in% c(input$weekday))
  })
  
  fa_rsk_sop_rec <- reactive({
    fa_rsk_sop %>% 
      filter(BID_PERIOD %in% c(input$bid_periods_input),
             BASE == input$bases,
             weekday %in% c(input$weekday))
  })
  
  long_data_deg_fa_rec <- reactive({
    long_data_deg_fa %>% 
      filter(BID_PERIOD %in% c(input$bid_periods_input),
             BASE == input$bases,
             sick_type %in% c(input$sick_input),
             weekday %in% c(input$weekday))
  })
  
  sum_fa_rsk_sop_t_rec <- reactive({
    sum_fa_rsk_sop_t %>% 
      filter(BID_PERIOD %in% c(input$bid_periods_input),
             BASE == input$bases,
             weekday %in% c(input$weekday))
  })
  
  
  
  
  output$plot_utl <- renderPlotly({
    tryCatch({
      reserve_data <- reserve_utl()
      
      # Calculate the range of the x-axis (DATE) based on the data
      min_date <- min(reserve_utl()$DATE)
      max_date <- max(reserve_utl()$DATE)
      
      # Convert dates to character strings, but first sort them by the original DATE order
      sorted_dates <- reserve_utl()$DATE[order(reserve_utl()$DATE)]
      date_strings <- format(sorted_dates, "%Y-%m-%d")
      
      # Select every 3rd date from the sorted date strings
      seq_dates <- date_strings[seq(1, length(date_strings), by = 3)]
      
      # Calculate the range of the x-axis (DATE) and the number of data points
      x_range <- as.numeric(max(reserve_utl()$DATE) - min(reserve_utl()$DATE))
      num_points <- nrow(reserve_utl())
      
      # Calculate the dynamic text size based on x-axis range and number of points
      dynamic_size <- dynamic_text_size(x_range, num_points)
      
      # Create the ggplot object
      utl_p <- ggplot(reserve_utl()) +
        geom_bar(aes(x = as.character(DATE), y = net_rlv_available,
                     text = paste("Date:", DATE, "<br>Weekday:", weekday, "<br>RLV Available:", net_rlv_available)), 
                 stat = "identity", alpha = 0.3, width = 0.85) +  # Adjust width as needed
        geom_bar(data = long_data_rec(), 
                 aes(x = as.character(DATE), y = Head_Count, fill = rlv_type, 
                     text = paste0("Date:", DATE, "<br>Weekday:", weekday, "<br>", rlv_type, ": ", Head_Count)), 
                 stat = "identity", position = "stack", width = 0.85) +  # Adjust width as needed
        scale_x_discrete(breaks = seq_dates, labels = seq_dates) +  # Conditional labeling
        scale_fill_manual(values = c("RLV Available" = "#413691", 
                                     "RLV Utilized" = "#D2058A")) +
        labs(x = "Date", y = "Primary RLV Head Count", fill = "RLV Type") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.text = element_text(size = 9),
              legend.key.size = unit(0.35, "cm"),
              legend.title = element_text(size = 5),
              legend.box.spacing = unit(0.35, "cm"),
              plot.title = element_text(hjust = 1.5)) +
        ggtitle(paste0(input$bases, " - ", tot_label_rec()$tot, " Total Reserves Across Bid Periods")) +
        geom_text(data = label_data_rec(),
                  aes(x = as.character(DATE), y = net_rlv_available + (0.025 * max(net_rlv_available)), 
                      label = net_rlv_available),
                  size = dynamic_size, vjust = -0.75, hjust = 0.5) +
        geom_text(data = label_data_used_rec(),
                  aes(x = as.character(DATE), y = (rlv_used + (0.025 * max(rlv_used))), 
                      label = perc_ut),
                  size = dynamic_size, vjust = -0.75, color = "lightgrey") +
        geom_hline(aes(yintercept = hline_data_rec()$yintercept), linetype = "dashed", color = "#00A5BA") +
        geom_text(aes(x = as.character(max(reserve_utl()$DATE)+1),
                      y = (h_line_label_p_utl()$m_avg_utl + 5)), label = paste(h_line_label_p_utl()$m_perc_utl, "%"),
                  vjust = -1, hjust = 0, color = "#00A5BA", size = 2.5) +
        coord_cartesian(ylim = c(0, NA))
      
      # Convert to plotly with the tooltip containing the weekday
      ggplotly(utl_p, tooltip = "text")
      
    }, error = function(e) {
      plotly_empty() %>%
        layout(
          title = list(text = "Please Make a Selection",
                       font = list(size = 24, color = "red"),
                       x = 0.5,  # Centered horizontally
                       y = 0.95  # Vertical positioning
          ),
          xaxis = list(
            title = "Date", 
            range = c(1, 10),  # Dummy x-axis range
            showgrid = TRUE,
            zeroline = FALSE,
            tickvals = 1:10,  # Dummy tick values
            ticktext = format(seq(Sys.Date() - 9, Sys.Date(), by = 1), "%Y-%m-%d")  # Dummy dates
          ),
          yaxis = list(
            title = "Head Count", 
            range = c(0, 100),  # Dummy y-axis range
            showgrid = TRUE,
            zeroline = FALSE
          ),
          annotations = list(
            list(
              x = 0.5, y = 0.5, 
              text = "Please Make a Selection", 
              showarrow = FALSE, 
              font = list(size = 16, color = "grey")
            )
          ),
          margin = list(l = 50, r = 50, t = 80, b = 60),
          paper_bgcolor = "grey95",  # Background color
          plot_bgcolor = "white"        # Plot area background
        )
      
    })
  })
  
  
  

  custom_colors_rec  <- reactive({

    c(colorRampPalette(c("#413691", "#D2058A"))(length(unique(long_data_deg_fa_rec()$sick_type))))

  })


  output$plotly_sic <- renderPlotly({

    code_p <- ggplot(sum_fa_rsk_sop_t_rec()) +
      geom_bar(data = long_data_deg_fa_rec(),
               aes(x = DATE, y = Head_Count, fill = sick_type),
               stat = "identity", position = "stack") +
      scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "3 days") +
      scale_fill_manual(values = custom_colors_rec(), name = "RLV Sick Type") +
      labs(x = "Date", y = "Primary Head Count", fill = "RLV Sick Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.text = element_text(size = 6),  # Adjust legend text size
            legend.key.size = unit(0.25, "cm"),  # Adjust size of legend keys
            legend.title = element_text(size = 5),  # Adjust legend title size
            legend.box.spacing = unit(0.25, "cm"),
            plot.title = element_text(hjust = 0.5)# Adjust spacing around the legend box
      ) +
      ggtitle(paste(input$bases))

    ggplotly(code_p)
  })
 
  
  output$asn_table <- renderDataTable({
    
    sum_fa_rsk_sop_t_rec() %>%
      datatable(options = list(
        columnDefs = list(
          list(visible = FALSE, targets = c(5, 6, 9))  # hide columns 1 and 3
        )
      ))
    
  })

  # Download handler for Excel file
  output$download_asn_table <- downloadHandler(
    filename = function() {
      paste("asn_table_", input$bid_periods_input, ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(sum_fa_rsk_sop_t_rec(), path = file)
    }
  )


  
}
