library(shiny)
library(shinydashboard)
library(tidyverse) 
library(vroom) 
library(plotly)
library(DT)
library(here) 

shinyServer(function(input, output, session){
  
#########################################################
  
  
  #Datatable
  output$mydatatable <- DT::renderDT({
    Dat %>%
      DT::datatable(options = list(scrollX = T))
  })
  
  
  
  
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
  
  
####Explore Graphs 
  output$GenExp <- renderPlotly({
    plot_ly(AllExpGenDat, 
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
  
  
  output$ImmigExp <- renderPlotly({
    plot_ly(AllExpImmigDat, 
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
  
  
  
  
  
####Regression Graphs
  output$GenReg <- renderPlotly({
    Filt <- if (input$selected == "All") {
      Dat
    } else {
      Dat %>% dplyr::filter(Country == input$selected)
    }
    GenMod <- lm(GenRol ~ Age + Edu + Sex, data = Filt)  
    model_df <- broom::tidy(GenMod) 
    
    plotly::plot_ly(
      data = model_df,
      x = ~term,
      y = ~estimate,
      type = 'scatter',
      mode = 'markers+errorbars',
      error_y = list(
        type = 'data',
        array = ~std.error,
        visible = TRUE
      )
    ) %>%
      plotly::layout(
        title = "Scatter Plot of Model Coefficients",
        xaxis = list(title = "Term"),
        yaxis = list(title = "Estimate")
      )
  })
  
  output$ImmigReg <- renderPlotly({
    Filt <- if (input$selected == "All") {
      Dat
    } else {
      Dat %>% dplyr::filter(Country == input$selected)
    }
    ImmigMod <- lm(Immig ~ Age + Edu + Sex, data = Filt)  
    model_df <- broom::tidy(ImmigMod) 
    
    plotly::plot_ly(
      data = model_df,
      x = ~term,
      y = ~estimate,
      type = 'scatter',
      mode = 'markers+errorbars',
      error_y = list(
        type = 'data',
        array = ~std.error,
        visible = TRUE
      )
    ) %>%
      plotly::layout(
        title = "Scatter Plot of Model Coefficients",
        xaxis = list(title = "Term"),
        yaxis = list(title = "Estimate")
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
  
  
  
  
  #Report widget 
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