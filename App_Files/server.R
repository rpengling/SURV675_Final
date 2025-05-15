library(shiny)
library(shinydashboard)
library(tidyverse) 
library(vroom) 
library(plotly)
library(DT)
library(here) 

shinyServer(function(input, output, session){
  
#########################################################
  
### Datatable
  output$mydatatable <- DT::renderDT({
    Dat %>%
      DT::datatable(options = list(scrollX = T))
  })
  
  
  
### Input Widgets 
  #Country widget 
  All <- unique(Dat$Country)
  
  output$inputwidget <- renderUI({
    selectInput("selected", "Select a Country:", 
                choices = c("All", as.character(All)), 
                selected = "All")
  })
  
  observeEvent(input$selected, {
    selected_countries <- if (input$selected == "All") All else input$selected
  })
  

  
  #Outcome widget 
  output$outcomewidget <- renderUI({
    selectInput("chosen", "Select an Outcome Variable:", 
                choices = c("GenRol", "Immig"), 
                selected = "GenRol")
  })
  #Controls widget 
  output$controlswidget <- renderUI({
    selectInput("controler", 
                "Select Control Variable(s):", 
                choices = c("Sex", "Edu"), 
                selected = NULL,  
                multiple = TRUE)
  })
  #Poly widget 
  output$polywidget <- renderUI({
    numericInput("num_input", "Enter a Numeric Value (1-5):", value = NULL, min = 1, max = 5)
  })
  
  
  
  
### Explore Graphs 
  #GenRol
  output$GenExp <- renderPlotly({
    Filt <- if (input$selected == "All") {
      AllExpGenDat
    } else {
      SelectedExpGenDat %>% dplyr::filter(Country == input$selected)
    }
    
    plot_ly(Filt, 
            x = ~Variable, 
            y = ~Average, 
            color = ~Group, 
            type = 'bar', 
            text = ~paste("Group:", Group, "<br>Average:", Average),
            hoverinfo = 'text') %>%
      layout(title = "Average Attitude Towards Gender Roles by Group",
             xaxis = list(title = "Variable Groups"),
             yaxis = list(title = "Average Attitude Towards Gender Roles", range = c(1, 4)),
             barmode = 'group')
  })
  
  #Immig
  output$ImmigExp <- renderPlotly({
    Filt <- if (input$selected == "All") {
      AllExpImmigDat
    } else {
      SelectedExpImmigDat %>% dplyr::filter(Country == input$selected)
    }
    
    plot_ly(Filt,
            x = ~Variable, 
            y = ~Average, 
            color = ~Group, 
            type = 'bar', 
            text = ~paste("Group:", Group, "<br>Average:", Average),
            hoverinfo = 'text') %>%
      layout(title = "Average Attitude Towards Immigration by Group",
             xaxis = list(title = "Variable Groups"),
             yaxis = list(title = "Average Attitude Towards Immigration", range = c(1, 4)),
             barmode = 'group')
  })
  
  
  
  
  
### Regression Graphs
  #GenRol
  output$GenReg <- renderPlotly({
    Filt <- if (input$selected == "All") {
      Dat
    } else {
      Dat %>% dplyr::filter(Country == input$selected)
    }
    
    GenMod <- lm(GenRol ~ Age + Edu + Sex, data = Filt)  
    model_df <- broom::augment(GenMod) %>%
      dplyr::select(.fitted, .resid)
    
    plotly::plot_ly(
      data = model_df,
      x = ~.fitted,
      y = ~.resid,
      type = 'scatter',
      mode = 'markers'
      ) %>%
      plotly::layout(
        title = "Predicted Values vs Residuals for Gender Roles",
        xaxis = list(title = "Predicted Values"),
        yaxis = list(title = "Residuals")
      )
  })
  
  #Immig
  output$ImmigReg <- renderPlotly({
    Filt <- if (input$selected == "All") {
      Dat
    } else {
      Dat %>% dplyr::filter(Country == input$selected)
    }
    
    ImmigMod <- lm(Immig ~ Age + Edu + Sex, data = Filt)  
    model_df <- broom::augment(ImmigMod) %>%
      dplyr::select(.fitted, .resid)
    
    plotly::plot_ly(
      data = model_df,
      x = ~.fitted,
      y = ~.resid,
      type = 'scatter',
      mode = 'markers'
    ) %>%
      plotly::layout(
        title = "Predicted Values vs Residuals for Immigration",
        xaxis = list(title = "Predicted Values"),
        yaxis = list(title = "Residuals")
      )
  })
  

  

    
    
### Regression Table
  output$GenModel <-DT::renderDT({
    Filt <- if (input$selected == "All") {
      Dat
    } else {
      Dat %>% dplyr::filter(Country == input$selected)
    }
    GenMod <- lm(GenRol ~ Age + Edu + Sex, data = Filt)  
    broom::tidy(GenMod) %>%
      dplyr::mutate(across(
        where(is.numeric), 
        ~ ifelse(round(., 4) == 0, "<0.0001", format(round(., 4), nsmall = 4))
      )) %>%
      DT::datatable(options = list(scrollX = TRUE))
  })
  
  output$ImmigModel <-DT::renderDT({ 
    Filt <- if (input$selected == "All") {
      Dat
    } else {
      Dat %>% dplyr::filter(Country == input$selected)
    } 
    ImmMod <- lm(Immig ~ Age + Edu + Sex, data = Filt)  
    broom::tidy(ImmMod) %>%
      dplyr::mutate(across(
        where(is.numeric), 
        ~ ifelse(round(., 4) == 0, "<0.0001", format(round(., 4), nsmall = 4))
      )) %>%
      DT::datatable(options = list(scrollX = TRUE))
  })
  
  
  
  
  
  
### Report widget 
  output$download_report <- downloadHandler(
    filename = function() {
      paste("Report_for_", input$country, ".pdf", sep = "")  
    },
    content = function(file) {
      report_content <- generate_report(input$country)
      
      write(report_content, file)
    }
  )
  
  
}
)