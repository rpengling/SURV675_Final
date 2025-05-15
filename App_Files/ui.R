library(shiny)
library(shinydashboard)
library(tidyverse) 
library(vroom) 
library(plotly)
library(DT)
library(here)

shinyUI(
  dashboardPage(skin = "blue",
                dashboardHeader(title = "Exploring Attitudes towards Gender Roles and Immigration: EVS Data", titleWidth = 700, disable = FALSE), 
                
                
                dashboardSidebar(collapsed = FALSE, disable = FALSE,
                                 sidebarMenu(id = 'sidebarmenu', 
                                              
                                              
                                              #About Page                      
                                              menuItem(text = "About", tabName = "about", icon = icon("earth-americas")), 
                                              
                                              
                                              #explore          
                                              menuItem("Exploration & Regression", tabName = "charts", icon = icon("line-chart")),
                                              
                                              
                                              
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
                            h1("Welcome!"), 
                            h4("This app is designed to let you explore data from the European Value Study's 2017 Wave."),
                            p("Please use the sidebar menu to navigate through the different pages. Continue reading for more information about the pages."), 
                            
                            h2("Exploration & Regression"), 
                            h4("Here you will find four boxes where you can select different variables and manipulate the graphs, tables, and regression formula."), 
                            
                            h3("Selection Boxes"), 
                            p("The 'Country' box contains a dropdown where you can select a specific country you are interested in. The default is to show you all countries contain within the dataset. The 'Outcome Variable' box lets you select which of the outcome variables you are interested in investigating. The options are 'GenRol' which represents the attitudes towards traditional gender roles where a higher values reported indicates more positive views on traditional roles, such as believing children suffer if the mother works. The second option, 'Immig, is representative of attitudes towards immigration where higher values reported indicate more positive views towards immigration, such as believing people of the nation should not be given priority over immigrants if jobs are scarce. The 'Control Variable(s)' box lets you select how many control variables you would like considered in the regression calculation. You can select 'Sex' and/or 'Edu', for level of education, or leave the box empty. 'Age' is automatically a part of the regression formula. 'Polynomial Value' also influences the regression formula by changing the number of polynomials of age in the regression, with the options of 1, 2, 3, 4, and 5. If you choose to enter 3, the regression will contain 'Age + Age^2^ + Age^3^'."), 
                            
                            h3("Graphs"), 
                            p("The 'Exploration' tab depicts the average value reported of the outcome variable of your choosing, based on respondents' age group, sex, and education level. The 'Regression' tab depicts a scatter plot of the the predicted versus the residuals from the regression."), 
                            
                            h3("Regression Results"), 
                            p("This section depicts the results from the linear regression, influenced by which control and outcome variables you choose. By default, the regression formula predicts attitudes towards traditional gender roles on age."), 
                            
                            h2("Data"), 
                            h4("You will see the display of the full dataset used for this app."), 
                            
                            h2("Click for Github Repository"), 
                            h4("Click this tab will immediately open and navigate to the Github repository for this project where more information on the creation of this app can be located.")
                    ),
                    
                    
                    #Charts(Graphs)         
                    tabItem(tabName = "charts", 
                          #Widgets
                            fluidRow(
                              box(title = "Country", status = "primary", solidHeader = T, uiOutput("inputwidget"), width = 3),
                              box(title = "Outcome Variable", status = "primary", solidHeader = T, uiOutput("outcomewidget"), width = 3),
                              box(title = "Control Variable(s)", status = "primary", solidHeader = T, uiOutput("controlswidget"), width = 3),
                              box(title = "Ploynomial Value", status = "primary", solidHeader = T, uiOutput("polywidget"), width = 3)),
                            
                          
                          
                          
                          #Exploration
                            fluidRow(
                              tabBox(id = "tabchart1", title = "Graphs",
                                     tabPanel("Gender Role Exploration", plotlyOutput("GenExp")), 
                                     tabPanel("Immigration Exploration", plotlyOutput("ImmigExp")), 
                                     tabPanel("Gender Role Regression", plotlyOutput("GenReg")), 
                                     tabPanel("Immigration Regression", plotlyOutput("ImmigReg"))
                              ), 

                          #Regression   
                            fluidRow(
                              tabBox(id = "tabchart1", title = "Regression Results",
                                     tabPanel("Gender Roles", DT::dataTableOutput("GenModel")), 
                                     tabPanel("Immigration", DT::dataTableOutput("ImmigModel"))
                              )
                            ), 
                            

                          
                          
                        #Download widget    
                            fluidRow(
                              box(title = "Click to Download Report", status = "danger", solidHeader = T, downloadButton("download_report", "Download Report"), width = 4))
                            
                            
                            
                            
                            
                            
                            
                    )),
                    
                    
                    #Data
                    tabItem(tabName = "data", DT::dataTableOutput("mydatatable"))
                    
                    
                  )
                )
  )
)