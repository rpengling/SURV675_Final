library(shiny)
library(shinydashboard)
library(tidyverse) 
library(vroom) 
library(plotly)
library(DT)
library(here) 

shinyServer(function(input, output, session){
  
  #Datatable
  output$mydatatable <- DT::renderDT({
    Dat %>%
      DT::datatable(options = list(scrollX = T))
  })
  
  
  #Graphs
  
  
  #Dem by Country 
  output$dem <- renderPlotly({
    Dem %>%
      dplyr::filter(Country == input$selected) %>%
      dplyr::select(-Country) %>%
      summarise_all(~ round(mean(.x, na.rm = TRUE), 2)) %>%
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Average_Rating") %>% 
      mutate(Variable = stringr::str_remove(Variable, "^[DNS]_")) %>%
      plot_ly(
        x = ~Variable, 
        y = ~Average_Rating, 
        type = "bar"
      )
  })
  
  
  
  #News by Country 
  output$new <- renderPlotly({
    plot_data <- AllComb_Fin %>%
      dplyr::filter(Country == input$selected) %>% 
      ungroup() %>%
      dplyr::select(-Country) %>%
      pivot_longer(
        cols = 2:9, 
        names_to = "Variable", 
        values_to = "Proportion") %>%
      mutate(
        Variable = stringr::str_remove(Variable, "^[N]_"))
    plot_ly(
      data = plot_data,
      x = ~Variable,                
      y = ~Proportion,              
      color = ~factor(Response),    
      type = "bar", 
      name = ~factor(Response)      
    ) %>%
      layout(
        barmode = "stack",           
        xaxis = list(title = "Variables"),
        yaxis = list(title = "Proportion")
      )
  })
  
  #Science 
  output$sci <- renderPlotly({
    Sci %>% 
      dplyr::filter(Country == input$selected) %>%
      dplyr::select(-Country) %>%
      summarise_all(~ round(mean(.x, na.rm = TRUE), 2)) %>%
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Average_Rating") %>% 
      mutate(Variable = stringr::str_remove(Variable, "^[DNS]_")) %>%
      plot_ly(
        x = ~Variable, 
        y = ~Average_Rating, 
        type = "bar") 
    
  })
  
  
  
  
  
  
  
  
  
  
  #Tables 
  
  output$demo <- DT::renderDT({
    Dem %>% 
      dplyr::filter(Country == input$selected) %>% 
      dplyr::select(-Country) %>% 
      summarise_all(mean, na.rm = TRUE) %>% 
      round(2) %>% 
      rename_with(~ stringr::str_remove(., "^[DNS]_")) %>%
      DT::datatable(options = list(scrollX = T))
  })
  
  output$news <- DT::renderDT({
    AllComb_Fin %>% 
      dplyr::filter(Country == input$selected) %>% 
      rename_with(~ stringr::str_remove(., "^[DNS]_")) %>%
      DT::datatable(options = list(scrollX = T))
  })
  
  output$science <- DT::renderDT({
    Sci %>% 
      dplyr::filter(Country == input$selected) %>% 
      dplyr::select(-Country) %>% 
      summarise_all(mean, na.rm = TRUE) %>% 
      round(2) %>% 
      rename_with(~ stringr::str_remove(., "^[DNS]_")) %>%
      DT::datatable(options = list(scrollX = T))
  })
  
  
  #Full data
  output$bigdem <- DT::renderDT({
    Dem %>% 
      group_by(Country) %>%
      summarise_all(~ round(mean(.x, na.rm = TRUE), 2)) %>% 
      rename_with(~ stringr::str_remove(., "^[DNS]_")) %>%
      DT::datatable(options = list(scrollX = T))
  })
  
  
  output$bignew <- DT::renderDT({
    AllComb_Fin %>%
      rename_with(~ stringr::str_remove(., "^[DNS]_")) %>%
      DT::datatable(options = list(scrollX = TRUE)) 
  })
  
  
  output$bigsci <- DT::renderDT({
    Sci %>% 
      group_by(Country) %>%
      summarise_all(~ round(mean(.x, na.rm = TRUE), 2)) %>% 
      rename_with(~ stringr::str_remove(., "^[DNS]_")) %>%
      DT::datatable(options = list(scrollX = T))
  })
  
  
  
  
  #Input widget 
  output$inputwidget <- renderUI({
    selectInput("selected", "Select a Country:", choices = unique(Dat$Country), selected = unique(Dat$Country)[1])
  })
  
  
}
)