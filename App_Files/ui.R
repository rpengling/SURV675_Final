library(shiny)
library(shinydashboard)
library(tidyverse) 
library(vroom) 
library(plotly)
library(DT)
library(here)

shinyUI(
  dashboardPage(skin = "blue",
                dashboardHeader(title = "Exploring Attitudes towards Gender Roles and Immigration", titleWidth = 500, disable = FALSE), 
                
                
                dashboardSidebar(collapsed = FALSE, disable = FALSE,
                                 sidebarMenu( id = 'sidebarmenu', 
                                              
                                              
                                              #About Page                      
                                              menuItem(text = "About", tabName = "about", icon = icon("earth-americas")), 
                                              
                                              
                                              #charts          
                                              menuItem("Graphs & Tables", tabName = "charts", icon = icon("square-poll-vertical")),
                                              
                                              
                                              
                                              #Data         
                                              menuItem("Data", tabName = "data", icon = icon("database")), 
                                              
                                              
                                              
                                              #Github        
                                              menuItem("Click for Github Repository", href = "https://github.com/rpengling/SURV675_Final.git", icon = icon("link"))
                                 )
                ), 
                
                
                
                
                
                
                
                dashboardBody(
                  tabItems(
                    
                    #About        
                    tabItem(tabName = "about", 
                            h2("Welcome!"), 
                            h4("This app is designed to let you explore data from the World Value Study's 2006 Wave."), 
                            p("Please use the sidebar menu to navigate through the different pages. Within the 'Graphs & Tables' page, you will find Graphs, Tables, and All Countries. The Graphs and the Tables sections will show you three different options: the average attitudes towards democracy, average news consumption, and the average attitudes towards science for the country you select. The All Countries section will show you the same information but for all countries in the dataset. Please note that some countries may not produce graphs and tables for all three options due to one or more question not being asked in that county's survey."), 
                            p("The 'Data' page will display the full dataset used for this app. To differentiate between the different sections, variables starting with 'D' are the democracy questions, 'N' are news consumption, and 'S' are science."), 
                            p("'Click for Github Repository' will immediately open and navigate to the Github repository for this project where more information on the creation of this app can be located."), 
                            h4("Citation for data used:"),
                            p("Inglehart, R., C. Haerpfer, A. Moreno, C. Welzel, K. Kizilova, J. Diez-Medrano, M. Lagos, P. Norris, E. Ponarin & B. Puranen et al. (eds.). 2018. World Values Survey: Round Six - Country-Pooled Datafile. Madrid, Spain & Vienna, Austria: JD Systems Institute & WVSA Secretariat. doi.org/10.14281/18241.8")),
                    
                    
                    #Charts(Graphs)         
                    tabItem(tabName = "charts", 
                            fluidRow(
                              box(title = "Country", status = "primary", solidHeader = T, uiOutput("inputwidget"), width = 3),
                              box(title = "Outcome Variable", status = "primary", solidHeader = T, uiOutput("outcomewidget"), width = 3),
                              box(title = "Control Variable(s)", status = "primary", solidHeader = T, uiOutput("controlswidget"), width = 3),
                              box(title = "Ploynomial Value", status = "primary", solidHeader = T, uiOutput("polywidget"), width = 3)),
                            
                            
                            fluidRow(
                              tabBox(id = "tabchart1", title = "Graphs",
                                     tabPanel("Attitudes Towards Democracy", plotlyOutput("dem")), 
                                     tabPanel("News Consumption", plotlyOutput("new")), 
                                     tabPanel("Attitudes Towards Science", plotlyOutput("sci"))
                              ), 
                              tabBox(id = "tabchart3", side = "right", title = "Tables",
                                     tabPanel("Attitudes Towards Democracy", DT::dataTableOutput("demo")), 
                                     tabPanel("News Consumption", DT::dataTableOutput("news")),
                                     tabPanel("Attitudes Towards Science", DT::dataTableOutput("science"))
                              )
                            ),
                            
                            fluidRow(
                              tabBox(id = "tabchart2", title = "All Countries",
                                     tabPanel("Attitudes Towards Democracy", DT::dataTableOutput("bigdem")), 
                                     tabPanel("News Consumption", DT::dataTableOutput("bignew")), 
                                     tabPanel("Attitudes Towards Science", DT::dataTableOutput("bigsci"))
                              )
                            ), 
                            
                            
                            fluidRow(
                              box(title = "Click to Download Report", status = "danger", solidHeader = T, downloadButton("download_report", "Download Report"), width = 4))
                            
                            
                            
                            
                            
                            
                            
                    ),
                    
                    
                    #Data
                    tabItem(tabName = "data", DT::dataTableOutput("mydatatable"))
                    
                    
                  )
                )
  )
)