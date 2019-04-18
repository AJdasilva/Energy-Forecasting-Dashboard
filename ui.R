# Smart Grid Data Exploration Shiny Application ---------------------------------------


# set up -----------------------------------------------------------------------
# load packages that will be used for the application
library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)
library(markdown)
library(shinycssloaders)

# initialize state of dashboard to something that works 
view_window_start = "2016-01-30"
view_window_end = "2016-04-05"
forcast_horizon = 7
start_date = "2014-01-07" # start late due to ewa needing time for its last week trend
end_date = "2016-11-05"
train_end_date = "2016-01-01"
level_of_data = "daily"
time_period = "late evening"
peak_time = "yes"

# Set up the application ui
shinyUI(navbarPage("FORECAST", fluid = "TRUE", position = "static-top",

                   # define the tabs to be used in the app ----------------------------------------
                   tabPanel("Information",
                            includeMarkdown("./instruct.Rmd"),
                            hr()),
                   # SUMMARY STATISTICS TAB
                   tabPanel("Summary Statistics",
                            div(class="summary",
                                tags$head(
                                  includeCSS("./styles.css"))),
                            mainPanel(h3("Summary Statistics"),
                                      width = 12,
                                      
                                      # Output: Tabset w/ plot, summary, and table ----
                                      tabsetPanel(type = "tabs",
                                                  tabPanel("Variable Summaries",
                                                           fluidRow(width = 12,
                                                             box(width = 12, br(),
                                                                 selectInput(width = '100%', "variable_to_plot", "Choose Variable to Plot:", 
                                                                             choices = c('Power Use [kW]'='use','Solar Generation [kW]'='gen','Temperature'='temp',
                                                                                         'Humidity'='hum',
                                                                                         'Visibility'='vis','Pressure'='press','Wind Bearing'='windBearing',
                                                                                         'Precipitation Intensity'='pricipIntensity'))
                                                             )),
                                                           fluidRow(
                                                             column(width = 12, class = "well",
                                                                    h4("Brush, double-click to zoom, triple-click to reset"),
                                                                    plotOutput("plot3", height = 300,
                                                                               dblclick = "plot3_dblclick",
                                                                               brush = brushOpts(
                                                                                 id = "plot3_brush",
                                                                                 resetOnNew = TRUE
                                                                               )
                                                                    )
                                                             )),
                                                  fluidRow(width = 12,
                                                  box(width = 8,
                                                            column(width = 10, class = "well",
                                                            plotlyOutput("plot4")
                                                           )
                                                            ),
                                                  box(width = 4, 
                                                      selectInput(width = '100%', "month", "Select Month for Statistics:", 
                                                                  choices = c('January'= 1,'February' = 2, 'March' = 3, 'April' = 4,
                                                                              'May' = 5, 'June' = 6, 'July' = 7, 'August' = 8,
                                                                              'September'= 9,'October' = 10, 'November' = 11,
                                                                              'December' = 12)),
                                                      dataTableOutput('table3')
                                                      )
                                                 ) # end fluid row
                                                 ),
                                                  tabPanel("Cat Photos",
                                                           imageOutput("cat1"),hr())
                                      ), # END TABSET PANEL
                            hr())), # END MAIN PANEL FOR DISPLAYING OUTPUTS
                   # POWER CONSUMPTION TAB
                   tabPanel("Power Consumption",
                            # set up styling for map
                            div(class="power",
                                tags$head(
                                  includeCSS("./styles.css"))),
                            fluidRow(
                              box(title = "Power Consumption Forecasting", width = 12)
                            ),
                            # Boxes need to be put in a row (or column)
                            fluidRow(
                              box(h4("Error of Use Model (mse):",
                                  textOutput("text1"))),
                              width = 12
                            ),
                            br(),
                            fluidRow(
                                 box(
                                     width = 8,
                                     withSpinner(plotlyOutput("plot1", width = '100%'),type=5) 
                                     ) ,
                                 box(
                                    width = 4,
                                    sliderInput("slider", "Forecast Horizon (days)", 1, 50, 7, width = '100%'),
                                    dateRangeInput("date_range", "Analysis Range: Start Date to End Date", start = start_date, end = end_date, width = '100%'),
                                    dateInput("train_date","Date to End Training",value = train_end_date, width = '100%'),
                                    selectInput(width = '100%', "time_period", "Choose time period:", 
                                                choices = c('Entire Day'='whole_day','Early Morning 1:00am-8:00am'='early morning','Afternoon 8:00am-11:00am'='morning',
                                                            'Late Afternoon noon-6:00pm'='afternoon',
                                                            'Evening 6:00pm-8:00pm'='evening','Late Evening 8:00pm-midnight'='late evening')),
                                    dateRangeInput(width = '100%', "window_range", "Viewing Window Range: Start Date to End Date", start = view_window_start, end = view_window_end)
                                 )
                            ),
                            fluidRow(
                                box(dataTableOutput('table'),width = 12)
                            ),
                            hr()),
                   # SOLAR POWER GENERATION TAB
                   tabPanel("Solar Power Generation",
                            # set up styling for map
                            div(class="solar",
                                tags$head(
                                  includeCSS("./styles.css"))),
                            fluidRow(
                              box(title = "Solar Power Generation Forecasting", width = 12)
                            ),
                            # Boxes need to be put in a row (or column)
                            fluidRow(
                              box(h4("Error of Use Model (mse):",textOutput("text2")))
                            ),
                            br(),
                            fluidRow(
                              box(
                                width = 8,
                                withSpinner(plotlyOutput("plot2", width = '100%'),type=5) # loading spinner which shows until the plot is loaded
                              ) ,
                              box(
                                width = 4,
                                sliderInput(width = '100%', "solslide", "Forecast Horizon (days)", 1, 50, 3),
                                dateRangeInput(width = '100%', "sol_date_range", "Analysis Range: Start Date to End Date", start = start_date, end = end_date),
                                dateInput(width = '100%', "sol_train_date","Date to End Training",value = train_end_date),
                                selectInput(width = '100%', "peak_time", "Choose time period:", 
                                            choices = c('Peak Solar 9:00am-5:00pm'='yes','Non-Peak Solar 5:00pm-9:00am'='no')),
                                dateRangeInput(width = '100%', "sol_win_range", "Viewing Window Range: Start Date to End Date", start = view_window_start, end = view_window_end)
                              )
                            ),
                            fluidRow(
                              box(dataTableOutput('table2'), width = 12)
                            ),
                            hr())
                   
                   
        # close the UI definition ...
                   ) # end navbarPage
        
        )# end shinyUI

