library(shiny)
library(shinydashboard)
library(tidyverse) 
library(vroom) 
library(plotly)
library(DT)
library(here) 
library(pander) 
library(RColorBrewer)

shinyServer(function(input, output, session){
  
  
  
  
  
  
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
    numericInput("num_input", "Enter a Numeric Value (1-5):", value = 1, min = 1, max = 5)
  })
  
  
  
  
  
  
### Explore Graphs 
  output$exp <- renderPlotly({
    if (input$chosen == "GenRol") {
      Filt <- if (input$selected == "All") {
        AllExpGenDat
      } else {
        SelectedExpGenDat %>% dplyr::filter(Country == input$selected)
      }
      plot_title <- "Average Attitude Towards Gender Roles by Group"
      y_title <- "Average Attitude Towards Gender Roles"
    } else if (input$chosen == "Immig") {
      Filt <- if (input$selected == "All") {
        AllExpImmigDat
      } else {
        SelectedExpImmigDat %>% dplyr::filter(Country == input$selected)
      }
      plot_title <- "Average Attitude Towards Immigration by Group"
      y_title <- "Average Attitude Towards Immigration"
    }
    
    plot_ly(Filt,
            x = ~Variable, 
            y = ~Average, 
            color = ~Group, 
            type = 'bar', 
            text = ~paste("Group:", Group, "<br>Average:", Average),
            hoverinfo = 'text') %>%
      layout(
        title = plot_title,
        xaxis = list(title = "Variable Groups"),
        yaxis = list(title = y_title, range = c(1, 4)),
        barmode = 'group'
      )
  })
 
  
  
  
  
  
### Regression Graphs
  output$regplot <- renderPlotly({
    Filt <- if (input$selected == "All") {
      Dat
    } else {
      Dat %>% dplyr::filter(Country == input$selected)
    } 
    
    Filt <- Filt %>% dplyr::filter(!is.na(Age))
    
    outcome_var <- input$chosen
    selected_vars <- input$controler 
    poly_degree <- input$num_input
    
    age_term <- if (poly_degree == 1) {
      "Age"
    } else {
      paste0("poly(Age, ", poly_degree, ")")
    }
    
    if (length(selected_vars) > 0) {
      formula <- paste(outcome_var, "~", age_term, "+", paste(selected_vars, collapse = " + "))
    } else {
      formula <- paste(outcome_var, "~", age_term)
    }
    
    Mod <- lm(as.formula(formula), data = Filt)
  
    model_df <- broom::augment(Mod) %>%
      dplyr::select(.fitted, .resid)
    
    plotly::plot_ly(
      data = model_df,
      x = ~.fitted,
      y = ~.resid,
      type = 'scatter',
      mode = 'markers'
      ) %>%
      plotly::layout(
        title = "Predicted Values vs Residuals",
        xaxis = list(title = "Predicted Values"),
        yaxis = list(title = "Residuals")
      )
  })
  
  
 
  
    
    
### Regression Table
  output$reg <-DT::renderDT({
    Filt <- if (input$selected == "All") {
      Dat
    } else {
      Dat %>% dplyr::filter(Country == input$selected)
    } 
    
    Filt <- Filt %>% dplyr::filter(!is.na(Age))
    
    outcome_var <- input$chosen
    selected_vars <- input$controler 
    poly_degree <- input$num_input
    
    age_term <- if (poly_degree == 1) {
      "Age"
    } else {
      paste0("poly(Age, ", poly_degree, ")")
    }
    
    if (length(selected_vars) > 0) {
      formula <- paste(outcome_var, "~", age_term, "+", paste(selected_vars, collapse = " + "))
    } else {
      formula <- paste(outcome_var, "~", age_term)
    }
    
    Mod <- lm(as.formula(formula), data = Filt)
    
    broom::tidy(Mod) %>%
      dplyr::mutate(across(
        where(is.numeric), 
        ~ ifelse(round(., 4) == 0, "<0.0001", format(round(., 4), nsmall = 4))
      )) %>%
      dplyr::mutate(term = case_when(
        grepl("poly\\(Age, [0-9]+\\)1", term) ~ "Age",
        grepl("poly\\(Age, [0-9]+\\)2", term) ~ "Age\u00B2",
        grepl("poly\\(Age, [0-9]+\\)3", term) ~ "Age\u00B3", 
        grepl("poly\\(Age, [0-9]+\\)4", term) ~ "Age\u2074", 
        grepl("poly\\(Age, [0-9]+\\)5", term) ~ "Age\u2075",
        TRUE ~ term
      )) %>%
      dplyr::rename(
        Term = term,
        `Reg Coef` = estimate,
        `SE` = std.error,
        `t-Statistic` = statistic,
        `p-Value` = p.value
      ) %>%
      DT::datatable(
        options = list(scrollX = TRUE), 
        rownames = FALSE, 
        escape = FALSE
      )
  })

##############################################################################################################
  
### Report widget 
  output$download_report <- downloadHandler(
    filename = function() {
      paste("Report_for_", input$selected, ".html", sep = "")  
    },
    content = function(file) {
      user_temp <- "C:/Users/Owner/SURV675_Final/Reports"
      
      reg_plot_path <- "C:/Users/Owner/SURV675_Final/Reports/reg_plot.png"
      exp_plot_path <- "C:/Users/Owner/SURV675_Final/Reports/exp_plot.png"
      reg_table_path <- file.path(user_temp, "reg_table.csv")
      
      Filt <- if (input$selected == "All") {
        Dat
      } else {
        Dat %>% dplyr::filter(Country == input$selected)
      }
      Filt <- Filt %>% dplyr::filter(!is.na(Age))
      
      outcome_var <- input$chosen
      selected_vars <- input$controler
      poly_degree <- input$num_input
      age_term <- if (poly_degree == 1) {
        "Age"
      } else {
        paste0("poly(Age, ", poly_degree, ")")
      }
      
      if (length(selected_vars) > 0) {
        formula <- paste(outcome_var, "~", age_term, "+", paste(selected_vars, collapse = " + "))
      } else {
        formula <- paste(outcome_var, " ~ ", age_term)
      }
      
      Mod <- lm(as.formula(formula), data = Filt)
      
      reg_plot <- ggplot(broom::augment(Mod), aes(x = .fitted, y = .resid)) +
        geom_point() +
        labs(title = "Predicted Values vs Residuals", x = "Predicted Values", y = "Residuals") +
        theme_minimal()
      
      ggsave(filename = reg_plot_path, plot = reg_plot, device = "png", width = 5, height = 5)
      
      exp_data <- if (input$selected == "All") {
        AllExpGenDat 
      } else {
        SelectedExpGenDat %>% dplyr::filter(Country == input$selected) 
      }
      num_groups <- length(unique(exp_data$Group))
      
      palette1 <- brewer.pal(12, "Set3") 
      palette2 <- brewer.pal(8, "Set2")      
      
      if (num_groups <= length(palette1)) {
        colors_to_use <- palette1[1:num_groups]
      } else {
        colors_to_use <- c(palette1, palette2)[1:num_groups]
      }
      
      exp_plot <- ggplot(exp_data, aes(x = Variable, y = Average, fill = Group)) +
        geom_bar(stat = "identity", position = "dodge") + 
        scale_fill_manual(values = colors_to_use) +
        labs(title = "Average Attitude by Group", x = "Variable Groups", y = "Average", fill = "Groups", caption = "Data from EVS 2017 Wave.") + 
        scale_y_continuous(limits = c(0, 4)) +
        theme_minimal()
      
        ggsave(filename = exp_plot_path, plot = exp_plot, device = "png", width = 5, height = 5)
      
      
      reg_table <- broom::tidy(Mod)
      write.csv(reg_table, reg_table_path, row.names = FALSE)
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = list(
          country = input$selected,
          outcome = input$chosen,
          controls = input$controler,
          poly_degree = input$num_input,
          reg_plot_path = reg_plot_path,
          exp_plot_path = exp_plot_path,
          reg_table_path = reg_table_path
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
#######################################################################
  
}
)