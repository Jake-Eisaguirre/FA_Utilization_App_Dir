
#source(here("FA_Utilization_App/global.R"))

source(here("global.R"))


# Define UI
ui <- secure_app(fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        margin: 0;
      }
      
      .title-banner {
        background-color: #413691;
        color: white;
        padding: 15px;
        position: relative;
      }
      
      .title-banner img {
        position: absolute;
        top: 7px;
        right: 60px;
        width: 190px;
        height: 85px; /* Adjust the size of the image */
      }
      
      .main-panel {
        background-color: #f0f0f0; /* Neutral professional grey for the main content area */
        padding: 15px; /* Padding for spacing inside the main panel */
      }
      
      .sidebar-panel {
        background-color: #d0d0d0; /* Darker grey for sidebar panel */
        padding: 15px; /* Optional padding */
      }
      
      .solid-line {
        border-top: 2px solid #cccccc; /* Solid line separator */
        margin: 20px 0; /* Space above and below the line */
      }
      
            /* Custom styles for spinners */
      .shiny-spinner {
        border: 4px solid #f0f0f0; /* Light grey border for spinner */
        border-top-color: #ff66b2; /* Pink color for spinner top */
      }

      .spinner-border-sm {
        border: 2px solid #f0f0f0; /* Light grey border for small spinner */
        border-top-color: #ff66b2; /* Pink color for spinner top */
      }
      
      .spinner-border-xl {
        border: 6px solid #f0f0f0; /* Light grey border for extra-large spinner */
        border-top-color: #ff66b2; /* Pink color for spinner top */
      }
      
      .selectize-dropdown-content .active {
        background-color: #D2058A;
      
      }
      
      .shiny-input-select {
      
       color: #D2058A;
      
      }
      
      .nav-tabs > li > a {
        color: #D2058A;
      }
      



  .bootstrap-select .dropdown-menu .active {
    background-color: #D2058A !important; /* Active (hover) background color */
    color: white !important; /* White text for active items */
  }

  .bootstrap-select .btn {
    color: #D2058A !important; /* Text color for the selected text */
    background-color: white !important; /* Button background color */
    border-color: #D2058A !important; /* Border color */
  }

  .bootstrap-select .dropdown-toggle {
    color: #D2058A !important; /* Text color for the dropdown */
    border: none !important; /* Remove border for a cleaner look */
  }
      

    "))
  ),
  
  # Title banner with image
  div(class = "title-banner",
      tags$h1("Flight Attendant Reserve Utilization"),
      img(src = "OIP.png", alt = "Image", class = "banner-image")
  ),
  
  div(
    h3("Data Background"),
    HTML('
    Data presented in this report is aggregated across the <a href="https://app.snowflake.com/halorg/hawaiianair/#/data/databases/ENTERPRISE/schemas/CREW_ANALYTICS/view/CT_MASTER_HISTORY/data-preview" target="_blank" style="color:#413691"><strong>CT_MASTER_HISTORY</strong></a> table from the Snowflake Database within the <a href="https://app.snowflake.com/halorg/hawaiianair/#/data/databases/ENTERPRISE/schemas/CREW_ANALYTICS" target="_blank" style="color:#413691"><strong>CREW_ANALYTICS</strong></a> Schema.
    For the purpose of this report, reserve utilization is defined as an employee having an <strong title = "Reserve">RLV</strong> code and receiving an <strong title="Reserve Assignment">ASN</strong>, <strong title="International Boarding Assignment">BSN</strong>, or <strong title="Neighbor Island Boarding Assignment">BRD</strong>
    code for a given day.
    The <strong title = "Reserve">RLV</strong> head count per day is determined by the count of distinct employees per day on the 25th of the preceding bid period; This is prior to when FAs can trade RLV days.
    The black dashed line presented on the first figure indicates the average utilization for the given bid period.
    For the sick code figure and data; sick codes are defined as <strong title = "Sick w/ Out Pay">SOP</strong>, <strong title="Sick Leave">2SK</strong>, <strong title="Family Leave Vacation">FLV</strong>, <strong title="Family Leave PER">FLP</strong>, <strong title="Unavailable">UNA</strong>, <strong title="Family Leave Sick">FLS</strong>, <strong title="FMLA No Sick">FLU</strong>, <strong title="No Show">N/S</strong>, <strong title="PLWOP">PER</strong>, <strong title="Medical Grounding">MGR</strong>.
    The sick code figure visualizes the final sick code associated with the employee following an assignment determined by the greatest value associated for Update Date and Time column per day per employee.
    The sick code table provides all the sick codes within the transaction following the assignment.
  '),
    tags$hr(class = "solid-line")),
  
  div(h3("Daily Reserve Utilization and Sick Codes")),
  
  sidebarLayout(
    sidebarPanel(width=2,
                 class = "sidebar-panel",
                 pickerInput("bid_periods_input", "Bid Period",
                             options = pickerOptions(
                               actionsBox = TRUE, 
                               size = 10,
                               selectedTextFormat = "count > 3"
                             ), 
                             choices =bid_periods$BID_PERIOD, multiple = T,
                             selected = max(bid_periods$BID_PERIOD)),
                 pickerInput("bases", "Base", choices = bases, 
                             selected = c("HNL")),
                 pickerInput("weekday", "Weekday", choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), 
                             selected = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                             multiple = T,
                             options = pickerOptions(
                               actionsBox = TRUE, 
                               size = 10,
                               selectedTextFormat = "count > 3"
                             )),
                 pickerInput("sick_input", "Sick Code", choices = c("SOP", "2SK", "FLV", "FLP", 
                                                                    "UNA", "FLS", "FLU", "N/S", 
                                                                    "PER", "MGR"), 
                             multiple = T,
                             options = pickerOptions(
                               actionsBox = TRUE, 
                               size = 10,
                               selectedTextFormat = "count > 3"
                             ), 
                             selected = c("SOP", "2SK", "FLV", "FLP", 
                                          "UNA", "FLS", "FLU", "N/S", 
                                          "PER", "MGR")),
                 pickerInput("asn_codes", "Assignment Codes", choices = c("ASN", "BRD", "BSN"), 
                             selected = c("ASN", "BRD", "BSN"),
                             multiple = T,
                             options = pickerOptions(
                               actionsBox = TRUE, 
                               size = 10,
                               selectedTextFormat = "count > 3"
                             ))
    ),

# -------------------------------------------------------------------------

    
    mainPanel(width = 9,
              class = "main-panel",
              tabsetPanel(
                tabPanel("Reserve Utilization", 
                         withSpinner(plotlyOutput('plot_utl', height = "600px"), color = getOption("spinner.color", default = "#D2058A")),
                         tags$hr(class = "solid-line"),
                         tags$br(),
                         downloadButton("download_utl_table", "Download as Excel File",
                                        style = "background-color: #217346; color: white; border: none; padding: 10px 20px;")
                ),
                tabPanel("Sick After ASN", 
                         withSpinner(plotlyOutput('plotly_sic', height = "400px"), color = getOption("spinner.color", default = "#D2058A")),
                         tags$hr(class = "solid-line"),
                         withSpinner(dataTableOutput('asn_table'), color = getOption("spinner.color", default = "#D2058A")),
                         tags$br(),
                         downloadButton("download_asn_table", "Download as Excel File",
                                        style = "background-color: #217346; color: white; border: none; padding: 10px 20px;")
                )
              )
    )
  )
))

