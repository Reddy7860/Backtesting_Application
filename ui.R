options(shiny.sanitize.errors = FALSE)

library("shinyWidgets")
library(jsonlite)
library(dplyr)
library(DT)
library(quantmod)
library("sqldf")
require("sqldf")
library(anytime)
library(lubridate)
library("taRifx")

library("shinybusy")
library("shinycssloaders")
library("shinycustomloader")
library(shinyjs)
library(shinythemes)
library(shinyjs)
library(rintrojs)
library(shinyalert)
library(shinydashboard)
library(highcharter)
library(quantmod)
library("RJSONIO")
library(magrittr)
library(ggplot2)
library(slickR)
library(plotly)
require('tidyverse')
library(tidyverse)
library(tidyquant)

js <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #d73925;
}"'

ui <- tagList(
  
  navbarPage("Intraday Backtracker", 
             selected = "Home", 
             
             collapsible = TRUE, 
             inverse = TRUE, 
             theme = shinytheme("cosmo"),
             useShinyjs(),
             introjsUI(),
             useShinyalert(),
             # footer = includeHTML("footer.html"),
             header = tagList(
               useShinydashboard()
             ),
             tags$style(js),
             tabBox(id = "tabs",
                    side = "right", width = 12,
                    height = "45px",
                    selected = "Home",
                    tabPanel("About us",
                             fluidRow(
                               column(width = 6,
                                      tags$div(class = "box box-solid",
                                               tags$div(class = "box-header with-border collapse",
                                                        tags$i(class = "fas fa-book-open"),
                                                        tags$h3(class = "box-title custom-box-header","The Short")
                                               ),
                                               tags$div(class = "box-body",
                                                        tags$p("Stocks Analysis is an innovative trading idea to make trading technology accessible to everyone. This is a one stop application to having a full trading platform in a web browser."),
                                                        tags$p("Short"),
                                                        tags$p("Sai Teja Reddy is currently working as a Data Scientist at one of the most reputed technology organization. Prior to that, worked as Sales Operations Analyst and Business Analyst at Uber with almost 2 years of experience. He began his carrer as a Developer and has good hands on experience with microsoft technologies. He is an Electrical graduate from National Insititute of Technology Durgapur."),
                                                        tags$p("Long"),
                                                        tags$p("I am a technology lover and good writer. The purpose was building programmed software which allows to automate the trading of Indian stock trades by acting upon high volumes, creating strategies and doing backtest to generate more profits."),
                                                        br()
                                                        
                                               )
                                      )
                               )
                             ),
                             includeHTML("about.html"),
                             shinyjs::useShinyjs(),
                             tags$head(
                               tags$link(rel = "stylesheet", 
                                         type = "text/css", 
                                         href = "plugins/carousel.css"),
                               tags$script(src = "plugins/holder.js")
                             ),
                             tags$style(type="text/css",
                                        ".shiny-output-error { visibility: hidden; }",
                                        ".shiny-output-error:before { visibility: hidden; }"
                             )      
                    ),
                    tabPanel("Strategy Backtest",
                             box(width=12,status="primary",solidHeader=TRUE,
                                 fluidRow(
                                   column(3,
                                          dateRangeInput("backtest_range", "Date range:", start = "2020-12-28", end = Sys.Date(),format = "yyyy-mm-dd")
                                   ),
                                   column(2,
                                          selectizeInput("backtest_stock", label = "Ticker :", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","TITAN.NS"), selected = "RELIANCE.NS", multiple = FALSE,
                                                         options = list(create = TRUE,maxOptions = 10)),
                                          selectizeInput("backtest_timeframe", label = "Range :", choices=c("1 min"="1m","2 min"="2m","5 min"="5m","15 min"="15m","1 hour"="1h","4 hour"="4h","1 Day"="1d"), selected = "5 min", multiple = FALSE,
                                                         options = list(maxOptions = 10))
                                          
                                   ),
                                   column(2,
                                          numericInput("initial_cap", label=h6("Initial Capital:"), value=100000, min =1000,max = 10000000000),
                                          # selectizeInput("target_selection", label = "Ticker :", choices=c("Percentage","Points","Trail Stoploss"), selected = "Percentage", multiple = FALSE,
                                          #                options = list(create = FALSE,maxOptions = 10)),
                                          # numericInput(inputId = "ip_risk", label = "Risk %", value = 1, 
                                          #              min = 0.0, max = 5, step = 0.25),
                                          # numericInput(inputId = "ip_reward", label = "Reward %", value = 1, 
                                          #              min = 0.0, max = 5, step = 0.01)
                                          
                                          selectizeInput("target_selection", label = "Ticker :", choices=c("Percentage","Points","Trail Stoploss"), selected = "Percentage", multiple = FALSE,
                                                         options = list(create = FALSE,maxOptions = 10)),
                                          
                                          conditionalPanel(
                                            condition = "input.target_selection == 'Percentage'",
                                            numericInput(inputId = "percentage_ip_risk", label = "Risk %", value = 1, min = 0.0, max = 5, step = 0.25),
                                            numericInput(inputId = "percentage_ip_reward", label = "Reward %", value = 1, min = 0.0, max = 5, step = 0.01)
                                          ),
                                          
                                          conditionalPanel(
                                            condition = "input.target_selection == 'Points'",
                                            numericInput(inputId = "points_ip_risk", label = "Risk Points", value = 200, min = 10, max = 2000, step = 25),
                                            numericInput(inputId = "points_ip_reward", label = "Reward Points", value = 200, min = 10, max = 2000, step = 25)
                                          ),
                                          
                                          conditionalPanel(
                                            condition = "input.target_selection == 'Trail Stoploss'",
                                            numericInput(inputId = "trail_ip_risk", label = "Trail Risk by", value = 1, min = 0.0, max = 5, step = 0.25),
                                            numericInput(inputId = "trail_ip_reward",  label = "Reward reach", value = 0.1, min = 0.0, max = 10, step = 0.25)
                                          )
                                   ),
                                   column(2,
                                          radioButtons("bot_strategy", "Strategy:",
                                                       c("Cowboy" = "cowboy",
                                                         "Sweths Violation" = "sweths_violation",
                                                         "Reds Rocket"="reds_rocket",
                                                         "Reds Brahmos"="reds_brahmos",
                                                         "Blackout" = "blackout",
                                                         "Gap Up"="gap_up",
                                                         "Gap Down"= "gap_down",
                                                         "Moving Average" = "ma"),
                                                       selected = "cowboy")
                                   ),
                                   column(2,
                                          radioButtons("eod_square_off", "EOD Square Off:",
                                                       c("Yes" = "yes",
                                                         "No" = "no"),
                                                       selected = "no")
                                   ),
                                   column(1,
                                          actionButton(inputId = "backtest_action",label = "Run Backtest",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                   )
                                 ),
                                 fluidRow(
                                   conditionalPanel(
                                     condition = "input.bot_strategy == 'ma'",
                                     box(width=12,status="primary",solidHeader=TRUE,height = 100,
                                     fluidRow(
                                       column(2,
                                         selectizeInput("ma_long_1", label = "Moving Average Type :", choices=c("SMA","EMA"), selected = "SMA", multiple = FALSE)
                                       ),
                                       column(1,
                                              numericInput("ma_long_1_numeric", label="Line 1:", value=10, min =1,max = 100)
                                      ),
                                     column(2,
                                      selectizeInput("ma_long_cond", label = "Long Condition :", choices=c("Crosses Above","Crosses Below"), selected = "Crosses Above", multiple = FALSE) 
                                     ),
                                     column(2,
                                       selectizeInput("ma_long_2", label = "Moving Average Type :", choices=c("SMA","EMA"), selected = "SMA", multiple = FALSE)
                                     ),
                                     column(1,
                                       numericInput("ma_long_2_numeric", label="Line 2:", value=30, min =1,max = 100),
                                      )
                                     )
                                     ),
                                     HTML('<hr style="color: purple;">'),
                                     box(width=12,status="primary",solidHeader=TRUE,height = 100,
                                     fluidRow(
                                     column(2,
                                            selectizeInput("ma_short_1", label = "Moving Average Type :", choices=c("SMA","EMA"), selected = "SMA", multiple = FALSE)
                                            
                                     ),
                                     column(1,
                                            numericInput("ma_short_1_numeric", label="Line 1:", value=10, min =1,max = 100)
                                     ),
                                     column(2,
                                            selectizeInput("ma_short_cond", label = "Short Condition :", choices=c("Crosses Above","Crosses Below"), selected = "Crosses Above", multiple = FALSE)
                                     ),
                                     column(2,
                                            selectizeInput("ma_short_2", label = "Moving Average Type :", choices=c("SMA","EMA"), selected = "SMA", multiple = FALSE)
                                     ),
                                     column(1,
                                            numericInput("ma_short_2_numeric", label="Line 2:", value=30, min =1,max = 100)
                                     )
                                     )
                                     )
                                   ),
                                 )
                                 
                             ),
                             fluidPage(
                               tabsetPanel(
                                 tabPanel("Overview", 
                                          box(width=12,status="primary",solidHeader=TRUE,
                                         fluidRow(
                                             infoBoxOutput("total_pnl",width = 4),
                                             infoBoxOutput("total_signals",width = 4),
                                             infoBoxOutput("total_wins",width = 4)
                                          ),
                                         fluidRow(
                                           infoBoxOutput("total_losses",width = 4),
                                           infoBoxOutput("total_closed_eod",width = 4)
                                         )
                                         ,
                                         fluidRow(
                                           infoBoxOutput("time_hit_target",width = 4),
                                           infoBoxOutput("time_hit_stoploss",width = 4)
                                         )
                                          ),
                                         box(width=12,title= "MOM Performance",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
                                             column(6,
                                                    plotlyOutput("returns_plot")
                                             )
                                             
                                         )
                                 ),
                                 tabPanel("Transaction Details", 
                                          fluidRow(
                                            box(width=12,title= "BackTest Results :",status="primary",solidHeader=TRUE,
                                                div(style = 'overflow-y:scroll;',
                                                    withSpinner( DT::dataTableOutput("backtest_results")))
                                            )
                                          ),
                                          fluidRow(
                                            box(width=6,title= "Weekly vs Hourly Results :",status="primary",solidHeader=TRUE,
                                                div(style = 'overflow-y:scroll;',
                                                    plotlyOutput("weekly_and_hourly_chart"))
                                            ),
                                            box(width=6,title= "Monthly vs Hourly Results :",status="primary",solidHeader=TRUE,
                                                div(style = 'overflow-y:scroll;',
                                                    plotlyOutput("monthly_and_hourly_chart"))
                                            )
                                          )
                                 ),
                                 tabPanel("Optimiser", 
                                          fluidRow(
                                            numericInput("ml_stoploss_1", label="Stop Loss 1:", value=1, min =0,max = 10),
                                            numericInput("ml_stoploss_2", label="Stop Loss 2:", value=2, min =0,max = 10),
                                            numericInput("ml_stoploss_3", label="Stop Loss 3:", value=3, min =0,max = 10)
                                          ) , 
                                          fluidRow(width=12,title= "Results 1:",status="primary",solidHeader=TRUE,
                                                   div(style = 'overflow-y:scroll;',
                                                       withSpinner( DT::dataTableOutput("backtest_results_1")))
                                          ),
                                          fluidRow(width=12,title= "Results 2:",status="primary",solidHeader=TRUE,
                                                   div(style = 'overflow-y:scroll;',
                                                       withSpinner( DT::dataTableOutput("backtest_results_2")))
                                          ),
                                          fluidRow(width=12,title= "Results 3:",status="primary",solidHeader=TRUE,
                                                   div(style = 'overflow-y:scroll;',
                                                       withSpinner( DT::dataTableOutput("backtest_results_3")))
                                          )
                                          
                                 )
                                 # tabPanel("Daily Voided/Obeyed",
                                 #          fluidRow( 
                                 #            box(width=12,title= "Voided :",status="primary",solidHeader=TRUE , collapsible = TRUE, collapsed = TRUE ,
                                 #                div(style = 'overflow-y:scroll;',
                                 #                    withSpinner( DT::dataTableOutput("daily_voided")))
                                 #            )
                                 #          )  
                                 #          
                                 #          
                                 # )
                                 
                               )
                             )        
                    ),
                    tabPanel("Home",
                             
                             includeHTML("home.html"),
                             tags$script(src = "plugins/scripts.js"),
                             tags$head(
                               tags$link(rel = "stylesheet", 
                                         type = "text/css", 
                                         href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                               tags$link(rel = "icon", 
                                         type = "image/png", 
                                         href = "images/logo_icon.png")
                             )
                    )
                    
             )
  )
  
)




# shinyUI(navbarPage(title = "Intraday Backtracker",
#                    theme = "style/style.css",
#                    footer = includeHTML("footer.html"),
#                    fluid = TRUE, 
#                    # collapsible = TRUE,
#                    # ----------------------------------
#                    # tab panel 1 - Home
#                    tabPanel("Home",
#                             
#                             includeHTML("home.html"),
#                             tags$script(src = "plugins/scripts.js"),
#                             tags$head(
#                               tags$link(rel = "stylesheet", 
#                                         type = "text/css", 
#                                         href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
#                               tags$link(rel = "icon", 
#                                         type = "image/png", 
#                                         href = "images/logo_icon.png")
#                             )
#                    ),
#                    tabPanel("Strategy Backtest",
#                             box(width=12,status="primary",solidHeader=TRUE,
#                                 fluidRow(
#                                   column(3,
#                                     dateRangeInput("backtest_range", "Date range:", start = "2020-12-28", end = Sys.Date(),format = "yyyy-mm-dd")
#                                   ),
#                                   column(2,
#                                     selectizeInput("backtest_stock", label = "Ticker :", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","TITAN.NS"), selected = "RELIANCE.NS", multiple = FALSE,
#                                                  options = list(create = TRUE,maxOptions = 10))
#                                   ),
#                                   column(2,
#                                     numericInput("initial_cap", label=h6("Initial Capital:"), value=100000, min =1000,max = 10000000000)
#                                   ),
#                                   column(2,
#                                     radioButtons("bot_strategy", "Strategy:",
#                                                c("Cowboy" = "cowboy",
#                                                  "Sweths Violation" = "sweths_violation",
#                                                  "Reds Rocket"="reds_rocket",
#                                                  "Reds Brahmos"="reds_brahmos",
#                                                  "Blackout" = "blackout",
#                                                  "Gap Up"="gap_up",
#                                                  "Gap Down"= "gap_down"),
#                                                selected = "cowboy")
#                                   ),
#                                   column(1,
#                                   actionButton(inputId = "backtest_action",label = "Run Backtest",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
#                                   )
#                                 )
#                                 
#                             ),
#                             fluidPage(
#                               tabsetPanel(
#                                 tabPanel("Transaction Details", 
#                                          fluidRow(
#                                            box(width=12,title= "BackTest Results :",status="primary",solidHeader=TRUE,
#                                                div(style = 'overflow-y:scroll;',
#                                                    withSpinner( DT::dataTableOutput("backtest_results")))
#                                            )
#                                          )
#                                 )
#                                 # tabPanel("Daily Voided/Obeyed",
#                                 #          fluidRow( 
#                                 #            box(width=12,title= "Voided :",status="primary",solidHeader=TRUE , collapsible = TRUE, collapsed = TRUE ,
#                                 #                div(style = 'overflow-y:scroll;',
#                                 #                    withSpinner( DT::dataTableOutput("daily_voided")))
#                                 #            )
#                                 #          )  
#                                 #          
#                                 #          
#                                 # )
#                                 
#                               )
#                             )
#                             
#                    ),
#                    tabPanel("About us",
#                             fluidRow(
#                               column(width = 6,
#                                      tags$div(class = "box box-solid",
#                                               tags$div(class = "box-header with-border collapse",
#                                                        tags$i(class = "fas fa-book-open"),
#                                                        tags$h3(class = "box-title custom-box-header","The Short")
#                                               ),
#                                               tags$div(class = "box-body",
#                                                        tags$p("Stocks Analysis is an innovative trading idea to make trading technology accessible to everyone. This is a one stop application to having a full trading platform in a web browser."),
#                                                        tags$p("Short"),
#                                                        tags$p("Sai Teja Reddy is currently working as a Data Scientist at one of the most reputed technology organization. Prior to that, worked as Sales Operations Analyst and Business Analyst at Uber with almost 2 years of experience. He began his carrer as a Developer and has good hands on experience with microsoft technologies. He is an Electrical graduate from National Insititute of Technology Durgapur."),
#                                                        tags$p("Long"),
#                                                        tags$p("I am a technology lover and good writer. The purpose was building programmed software which allows to automate the trading of Indian stock trades by acting upon high volumes, creating strategies and doing backtest to generate more profits."),
#                                                        br()
#                                                        
#                                               )
#                                      )
#                               )
#                             ),
#                             includeHTML("about.html"),
#                             shinyjs::useShinyjs(),
#                             tags$head(
#                               tags$link(rel = "stylesheet", 
#                                         type = "text/css", 
#                                         href = "plugins/carousel.css"),
#                               tags$script(src = "plugins/holder.js")
#                             ),
#                             tags$style(type="text/css",
#                                        ".shiny-output-error { visibility: hidden; }",
#                                        ".shiny-output-error:before { visibility: hidden; }"
#                             )
#                             
#                    )
#                    
#                    
#                    
#                    
# ))

# ui <- fluidPage(
#   
#   # Application title
#   titlePanel("Backtesting Application"),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#       dateRangeInput("backtest_range", "Date range:", start = "2020-12-28", end = Sys.Date(),format = "yyyy-mm-dd"),
#       selectizeInput("backtest_stock", label = "Ticker :", choices=c("RELIANCE.NS","SBIN.NS","DRREDDY.NS","UBER","GOOG","AAPL","TITAN.NS"), selected = "RELIANCE.NS", multiple = FALSE,
#                      options = list(create = TRUE,maxOptions = 10)),
#       numericInput("initial_cap", label=h6("Initial Capital:"), value=100000, min =1000,max = 10000000000),
#       radioButtons("bot_strategy", "Strategy:",
#                           c("Cowboy" = "cowboy",
#                             "Sweths Violation" = "sweths_violation",
#                             "Reds Rocket"="reds_rocket",
#                             "Reds Brahmos"="reds_brahmos",
#                             "Blackout" = "blackout",
#                             "Gap Up"="gap_up",
#                             "Gap Down"= "gap_down",
#                             "5 Candle ABC"="abc_5_cand",
#                             "3 Candle ABC"= "abc_3_cand",
#                             "Demand And Supply Zone" = "demand_and_supply"),
#                           selected = "cowboy"),
#       actionButton(inputId = "backtest_action",label = "Run Backtest",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#       infoBoxOutput("total_pnl"),
#       withSpinner( DT::dataTableOutput("backtest_results"))
#       
#     )
#   )
# )