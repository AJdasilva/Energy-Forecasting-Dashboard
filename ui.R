# Smart Grid Data Exploration Shiny Application ---------------------------------------


# set up -----------------------------------------------------------------------
# load packages that will be used for the application
library(shiny)
library(leaflet)
library(shinydashboard)
library(DT)
library(markdown)

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
shinyUI(navbarPage("Forecasting Dashboard",
                   
                   # define the tabs to be used in the app ----------------------------------------
                   tabPanel("Dashboard Instructions",
                            includeMarkdown("./instruct.Rmd"),
                            hr()),
                   tabPanel("Summary Statistics",
                            hr()),
                   # POWER CONSUMPTION TAB
                   tabPanel("Power Consumption",
                            # set up styling for map
                            div(class="outer",
                                tags$head(
                                  includeCSS("./styles.css"))),
                            fluidRow(
                              box(title = "Power Consumption Forecasting")
                            ),
                            # Boxes need to be put in a row (or column)
                            fluidRow(
                              box("Error of Use Model (mse):",textOutput("text1"))
                            ),
                            fluidRow(
                              
                              box(plotOutput("plot1", height = 400)),
                              box(
                                title = "Controls",
                                sliderInput("slider", "Forecast Horizon (days)", 1, 50, 7),
                                dateRangeInput("date_range", "Analysis Range: Start Date to End Date", start = start_date, end = end_date),
                                dateInput("train_date","Date to End Training",value = train_end_date),
                                selectInput("time_period", "Choose time period:", 
                                            choices = c('Entire Day'='whole_day','Early Morning 1:00am-8:00am'='early morning','Afternoon 8:00am-11:00am'='morning',
                                                        'Late Afternoon noon-6:00pm'='afternoon',
                                                        'Evening 6:00pm-8:00pm'='evening','Late Evening 8:00pm-midnight'='late evening')),
                                dateRangeInput("window_range", "Viewing Window Range: Start Date to End Date", start = view_window_start, end = view_window_end)
                              ),
                              box(dataTableOutput('table', width = 850))
                            ),
                            hr()),
                   # SOLAR POWER GENERATION TAB
                   tabPanel("Solar Power Generation",
                            # set up styling for map
                            div(class="outer",
                                tags$head(
                                  includeCSS("./styles.css"))),
                            fluidRow(
                              box(title = "Solar Power Generation Forecasting")
                            ),
                            # Boxes need to be put in a row (or column)
                            fluidRow(
                              box("Error of Use Model (mse):",textOutput("text2"))
                            ),
                            fluidRow(
                              
                              box(plotOutput("plot2", height = 400)),
                              box(
                                title = "Controls",
                                sliderInput("solslide", "Forecast Horizon (days)", 1, 50, 3),
                                dateRangeInput("sol_date_range", "Analysis Range: Start Date to End Date", start = start_date, end = end_date),
                                dateInput("sol_train_date","Date to End Training",value = train_end_date),
                                selectInput("peak_time", "Choose time period:", 
                                            choices = c('Peak Solar 9:00am-5:00pm'='yes','Non-Peak Solar 5:00pm-9:00am'='no')),
                                dateRangeInput("sol_win_range", "Viewing Window Range: Start Date to End Date", start = view_window_start, end = view_window_end)
                              ),
                              box(dataTableOutput('table2', width = 850))
                            ),
                            hr())
                   
                   
        # close the UI definition ...
                   ) # end navbarPage
        ) # end shinyUI

