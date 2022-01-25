library(shiny)
library(shinyvalidate)
library(tidyverse)
library(broom)
library(bslib)
library(thematic)
library(DT)
library(plotly)


testnamedesctable <- read.csv("data/testnamedesc.csv")


# User Interface----

ui <- navbarPage(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#284F79",
    secondary = "#86919A",
    success = "#86919A",
    base_font = font_google("IBM Plex Sans"),
    code_font = font_google("JetBrains Mono")
  ),
  tags$style(type='text/css', '#testnamedesc {white-space: pre-wrap;}'),
  title = "Statistical Test Learning App",
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
            "Fisher’s F-Test"
          ),
          selected = "One Sample t-Test"
        ),
        textInput(
          inputId = "firstvector",
          label = "Type First Vector"
        ),
        uiOutput("vector"),
        h5(
          actionButton(
            inputId = "randomnum",
            label = "Generate Random Vector"
          ),
          align = "center"
        ),
        uiOutput("samplemean"),
        uiOutput("confidencelevel"),
        h5(
          actionButton(
            inputId = "generate",
            label = "Calculate"
          ),
          align = "center"
        )
      ),
      mainPanel(
        fluidRow(
          column(width = 6, h4(textOutput("testresulttitle")), align = "center"),
          column(width = 6, h4(textOutput("histogramtitle")), align = "center")
        ),
        fluidRow(
          column(width = 6, DTOutput("testresult"), align = "center"),
          column(width = 6, plotlyOutput("hist", width = "100%"), align = "center")
        ),
        fluidRow(br()),
        fluidRow(h4(textOutput("descriptiontitle")), align = "center"),
        fluidRow(
          verbatimTextOutput("testnamedesc")
        )
      )
    )
  ),
  tabPanel(
    title = "Source Code",
    pre(includeText("app.R"))
  )
)

# Server----
server <- function(input, output, session) {
  
  #bslib::bs_themer()
  
  thematic::thematic_shiny()
  
  randomnumx <- eventReactive(input$randomnum, {
    randomnum <- rnorm(n = 20)
  })
  
  randomnumy <- eventReactive(input$randomnum, {
    randomnum <- rnorm(n = 20)
  })
  
  output$vector <- renderUI({
    onevector <- c("One Sample t-Test", "Wilcoxon Signed Rank Test", "Shapiro Test")
    
    if(!input$testname %in% onevector){
      textInput(
        inputId = "secondvector",
        label = "Type Second Vector"
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
  
  observe({
    updateTextInput(session, "firstvector", value = paste(randomnumx(), collapse = ", "))
  })
  
  observe({
    updateTextInput(session, "secondvector", value = paste(randomnumy(), collapse = ", "))
  })
  
  
  iv <- InputValidator$new()
  iv$add_rule("firstvector", sv_required())
  iv$add_rule("secondvector", sv_required())
  iv$add_rule(
    "firstvector",
    function(value) {
      if(is.na(sum(as.numeric(unlist(str_split(value, pattern = ",")))))) {
        "Input have to be numeric, separated by comma"
      }
  })
  iv$add_rule(
    "secondvector",
    function(value) {
      if(is.na(sum(as.numeric(unlist(str_split(value, pattern = ",")))))) {
        "Input have to be numeric, separated by comma"
      }
    })
  iv$enable()
  
  
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
    } 
    
      test_result_tidy <- test_result %>% 
        mutate(result = ifelse(p.value <= 0.05, "Statistically Significant, Reject H0", "Statistically Insignificant, Accept H0")) %>% 
        t() %>% 
        tibble(Parameter = rownames(.), Value = .[,1]) %>% 
        select(-1) %>% 
        mutate(Parameter = str_to_title(Parameter))
    
    return(test_result_tidy)
    
  })
  
  output$testresult <- renderDT({
    datatable(
      stat_test(), 
      options = list(
        dom = 't',
        columnDefs = list(
          list(
            className = "dt-center",
            targets = "_all"
          )
        )
      )
    )
  })
  
  
  hist_vector <- eventReactive(input$generate, {
    
    firstvector <- density(as.numeric(unlist(str_split(input$firstvector, pattern = ","))))
    return(firstvector)
    
  })
  
  output$hist <- renderPlotly({
    hist_vector <- hist_vector()
    plot_ly(x = ~hist_vector$x, y = ~hist_vector$y, type = "scatter", mode = "lines", fill = "tozeroy") %>%  
      layout(xaxis = list(title = "Vector"), yaxis = list(title = "Density"))
  })

  testresulttitle <- eventReactive(input$generate, {
    "Test Result"
  })
  
  histogramtitle <- eventReactive(input$generate, {
    "Histogram Plot"
  })
  
  output$testresulttitle <- renderText({
    paste(testresulttitle())
  })
  
  output$histogramtitle <- renderText({
    paste(histogramtitle())
  })
  
  testdescription <- eventReactive(input$generate, {
    "Test Description"
  })
  
  output$descriptiontitle <- renderText({
    paste(testdescription())
  })
  
  testnamedesc <- eventReactive(input$generate, {
    testnamedesc <- testnamedesctable %>% 
      dplyr::filter(testname == input$testname) %>% 
      dplyr::select(description)
  })
  
  output$testnamedesc <- renderText({
    paste(testnamedesc())
  })
  
}


shinyApp(ui = ui, server = server)
