library(shiny)
library(tidyverse)
library(broom)
library(bslib)
library(DT)


ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,
    base_font = font_google("Montserrat")
  ),
  
  navbarPage(
    title = "Statistical Test App",
    tabPanel(
      title = "Home",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            inputId = "testname",
            label = "Select a Statistical Test",
            choices = c(
              "One Sample t-Test", 
              "Two Samples t-Test", 
              "Wilcoxon Signed Rank Test", 
              "Shapiro Test",
              "Kolmogorov And Smirnov Test",
              "Fisher’s F-Test",
              "Correlation Test"
            ),
            selected = "One Sample t-Test"
          ),
          textInput(
            inputId = "firstvector",
            label = "Type first vector"
          ),
          uiOutput("vector"),
          uiOutput("samplemean"),
          uiOutput("confidencelevel"),
          actionButton(
            inputId = "generate",
            label = "Generate"
          )
        ),
        mainPanel(
          tableOutput("testresult"),
          plotOutput("hist")
        )
      )
    ),
    tabPanel("About"),
    tabPanel(
      title = "Source Code",
      pre(includeText("app.R"))
    )
  )
  
)


server <- function(input, output) {
  
  output$vector <- renderUI({
    onevector <- c("One Sample t-Test", "Wilcoxon Signed Rank Test", "Shapiro Test")
    
    if(!input$testname %in% onevector){
      textInput(
        inputId = "secondvector",
        label = "Type second vector"
      )
    }
  })
  
  output$samplemean <- renderUI({
    samplemean <- c("One Sample t-Test", "Wilcoxon Signed Rank Test", "Two Samples t-Test")
    
    if(input$testname %in% samplemean){
      numericInput(
        inputId = "mu",
        label = "Input Sample Mean",
        value = 0
      )
    }
  })
  
  output$confidencelevel <- renderUI({
    confidencelevel <- c("One Sample t-Test", "Wilcoxon Signed Rank Test", "Two Samples t-Test")
    
    if(input$testname %in% confidencelevel){
      selectInput(
        inputId = "conf.level",
        label = "Select Confidence Level",
        choices = list("90%" = 0.90, "95%" = 0.95, "99%" = 0.99),
        selected = 0.90
      )
    }
  })
  
  
  stat_test <- eventReactive(input$generate, {
    
    firstvector <- as.numeric(unlist(str_split(input$firstvector, pattern = ",")))
    secondvector <- as.numeric(unlist(str_split(input$secondvector, pattern = ",")))
    conf.level <- as.numeric(input$conf.level)
    
    if(input$testname == "One Sample t-Test") {
      test_result <- t.test(firstvector, mu = input$mu, conf.level = conf.level) %>% 
        tidy() 
    } else if (input$testname == "Two Samples t-Test") {
      test_result <- t.test(x = firstvector, y = secondvector, mu = input$mu, conf.level = conf.level) %>% 
        tidy()
    } else if (input$testname == "Wilcoxon Signed Rank Test") {
      test_result <- wilcox.test(firstvector, mu = input$mu, conf.level = conf.level) %>% 
        tidy()
    } else if (input$testname == "Shapiro Test") {
      test_result <- shapiro.test(firstvector) %>% tidy()
    } else if (input$testname == "Kolmogorov And Smirnov Test") {
      test_result <- ks.test(x = firstvector, y = secondvector) %>% tidy()
    } else if (input$testname == "Fisher’s F-Test") {
      test_result <- var.test(x = firstvector, y = secondvector) %>% tidy()
    } else if (input$testname == "Correlation Test") {
      test_result <- cor.test(x = firstvector, y = secondvector) %>% tidy()
    }
    
      test_result_tidy <- test_result %>% 
        mutate(result = ifelse(p.value <= 0.05, "Statistically Significant, Reject H0", "Statistically Insignificant, Accept H0")) %>% 
        t() %>% 
        tibble(Parameter = rownames(.), Value = .[,1]) %>% 
        select(-1) %>% 
        mutate(Parameter = str_to_title(Parameter))
    
    return(test_result_tidy)
    
  })
  
  output$testresult <- renderTable(stat_test())
  
  
  hist_vector <- eventReactive(input$generate, {
    
    firstvector <- as.numeric(unlist(str_split(input$firstvector, pattern = ",")))
    return(firstvector)
    
  })
  
  output$hist <- renderPlot({
    hist(hist_vector())
  })

}


shinyApp(ui = ui, server = server)
