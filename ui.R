# Define UI for application that plots COVID-19 epi by LRFs
ui <- fluidPage(
  
  # Application title
  titlePanel(paste("PHREE Local COVID-19 epidemiology in England (version 0.7) updated", Sys.Date())),
  
  # geographies
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("radio_type", h3("Plot type"),
                   choices = list("Cumulative cases"=1, "Cumulative cases per 100,000 persons"=2, "Cumulative cases per 100,000 persons aged over 60"=3, 
                                  "New cases"=4, "New cases per 100,000 persons"=5,"New cases per 100,000 persons aged over 60"=6),
                   selected = 4),
      # checkboxGroupInput("check_ref",
      #                    h3("Reference line"),
      #                    choices = c("England"=2, "London"=3),
      #                    selected = 1),
      # actionButton("reset", "Reset"),
      # radioButtons("radio_national", h3("Country"),
      #              choices = list("England" = 1),selected = 1),
      radioButtons("radio_regional", h3("Region"),
                   choices = reg,selected = 3),
      conditionalPanel(
        condition = "input.radio_regional == 2" ,
        radioButtons("radio_lrf2", h3("LRF"),
                     choices = lrf[[2]],selected = 1)
      ),
      conditionalPanel(
        condition = "input.radio_regional == 3" ,
        radioButtons("radio_lrf3", h3("LRF"),
                     choices = lrf[[3]],selected = 1)
      ),
      conditionalPanel(
        condition = "input.radio_regional == 4" ,
        radioButtons("radio_lrf4", h3("LRF"),
                     choices = lrf[[4]],selected = 1)
      ),
      conditionalPanel(
        condition = "input.radio_regional == 5" ,
        radioButtons("radio_lrf5", h3("LRF"),
                     choices = lrf[[5]],selected = 1)
      ),
      conditionalPanel(
        condition = "input.radio_regional == 6" ,
        radioButtons("radio_lrf6", h3("LRF"),
                     choices = lrf[[6]],selected = 1)
      ),
      conditionalPanel(
        condition = "input.radio_regional == 7" ,
        radioButtons("radio_lrf7", h3("LRF"),
                     choices = lrf[[7]],selected = 1)
      ),
      conditionalPanel(
        condition = "input.radio_regional == 8" ,
        radioButtons("radio_lrf8", h3("LRF"),
                     choices = lrf[[8]],selected = 1)
      ),
      conditionalPanel(
        condition = "input.radio_regional == 9" ,
        radioButtons("radio_lrf9", h3("LRF"),
                     choices = lrf[[9]],selected = 1)
      ),
      conditionalPanel(
        condition = "input.radio_regional == 10" ,
        radioButtons("radio_lrf10", h3("LRF"),
                     choices = lrf[[10]],selected = 1)
      )#,
      # sliderInput("date", label = h3("Date Range"), min = as.Date("2020-01-01"), 
      #             max = Sys.Date(), value = c(as.Date("2020-03-01"), Sys.Date())),
      # conditionalPanel(
      #   condition = "input.check_y == true & input.radio_type < 3" ,
      #   sliderInput("y", label = h3("y-axis min & max"), min = 0, 
      #               max = 1e5, value = c(0,5e3))
      # ),
      # conditionalPanel(
      #   condition = "input.check_y == true & input.radio_type > 2" ,
      #   sliderInput("y2", label = h3("y-axis max & min"), min = 0, 
      #               max = 2000, value = c(0,500))
      # )#,
      #checkboxInput("check_y", "Custom y-axis", value = FALSE)    
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Cases", p(" "), plotlyOutput("ltlaPlot"), p(" "),
                           plotlyOutput("utlaPlot"), p(" "),
                           #downloadButton(outputId = "down1", label = "Download the plot"),
                           plotlyOutput("lrfPlot"), p(" "),
                           plotlyOutput("regionalPlot"), p(" "),
                           plotlyOutput("nationalPlot"), p(" ")),
                  tabPanel("Cases (log scale)", p(" "), plotlyOutput("ltlaLogPlot"), p(" "),
                           (" "), plotlyOutput("utlaLogPlot"), p(" "),
                           plotlyOutput("lrfLogPlot"), p(" "),
                           plotlyOutput("regionalLogPlot"), p(" ")),
                  tabPanel("COVID Deaths", p(" "), plotlyOutput("ltladeathPlot"), p(" "), 
                           plotlyOutput("utladeathPlot"), p(" "),
                           plotlyOutput("lrfdeathPlot"), p(" "),
                           plotlyOutput("regdeathPlot"), p(" ")), 
                  tabPanel("All deaths", p(" "), plotlyOutput("ltladeathallPlot"), p(" "), 
                           plotlyOutput("utladeathallPlot"), p(" "), 
                           plotlyOutput("lrfdeathallPlot"), p(" "),
                           plotlyOutput("regdeathallPlot"), p(" ")),
                  # tabPanel("Non-COVID deaths", p(" "), plotlyOutput("ltladeathnonPlot"), p(" "), 
                  #          plotlyOutput("utladeathnonPlot"), p(" "), 
                  #          plotlyOutput("lrfdeathnonPlot"), p(" "),
                  #          plotlyOutput("regdeathnonPlot"), p(" ")),
                  tabPanel("Text",
                           h4("Interpretation"),
                           p("Numbers should be interpreted with caution for several reasons."),
                           p("1. The date is the date of a sample being taken for cases or the date of occurence of death. There are lags in reporting so the presented numbers will be underestimates (most significant for the past few days of tests)"),
                           p("2. Differences in the testing strategy over time. Initially testing was targeted and travellers from overseas and their contacts. Testing then shifted to patients admitted to hospital. Now, testing is being extended to heathcare workers."),
                           p("3. COVID deaths are where COVID is included anywhere on the death certificate."),
                           h4("Data sources"),
                           tags$a(href="https://coronavirus.data.gov.uk/", "PHE/NHSX Total and daily UK cases"),
                           br(),
                           tags$a(href="https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard", "ONS Death registrations and occurrences by local authority and health board,  © Crown copyright and database right 2020"),
                           br(),
                           tags$a(href="https://www.ons.gov.uk/personspopulationandcommunity/populationandmigration/populationprojections/datasets/localAuthoritiesinenglandtable2", "ONS population projections, © Crown copyright and database right 2020"),
                           h4("Code"),
                           tags$a(href="https://github.com/drserajames/covid-local-epi", "GitHub"),
                           h4("Author"),
                           p("Sarah James, Public Health doctor"),
                           p(tags$a(href="mailto:slj38@cam.ac.uk", "Email"),tags$a(href="https://twitter.com/DrSeraJames", "Twitter")),
                           h4("Supported by PHREE"),
                           p(tags$a(href="https://heeoe.hee.nhs.uk/public_health/specialty-training-programme/public-health-registrars-east-england-phree", "PHREE"), "(Public Health Registrars in the East of England) represents public health trainees in the East of England. They have supported the development and dissemination of this tool, to provide up-to-date local information on COVID-19 epidemiology. The responsibility for any errors or ommisions lies with Sarah James (contact details above).")
                  )
                  #tabPanel("Table", tableOutput("table"))
      )
    )
  )
)