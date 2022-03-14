library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)


not_sel <- "Not Selected"

about_page <- tabPanel(
  title = "About",
  titlePanel("About"),
  "Statistical Consultancy",
  br(),
  "2022 March",
  br(),
  "Ramindu de Silva - AS2017342"
)


main_page <- tabPanel(
  title = "Analysis",
  titlePanel("Inputs"),
  sidebarLayout(
    sidebarPanel(
      title = "Inputs",
      fileInput("csv_input", "Select CSV File to Import", accept = ".csv"),
      selectInput("num_var_1", "Numerical Variable 1", choices = c(not_sel)),
      selectInput("num_var_2", "Numerical Variable 2", choices = c(not_sel)),
      selectInput("fact_var", "Factor Variable", choices = c(not_sel)),
      br(),
      actionButton("run_button", "Run Analysis", icon = icon("play"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "One numerical variable",
          plotOutput("plot_1")
        ),
        
        tabPanel(
          title = "Association of two numerical variables",
          plotOutput("plot_2")
        ),
        
        tabPanel(
          title = "Association of a numerical and factor variable",
          plotOutput("plot_3")
        ),
        
        tabPanel(
          title = "One factor variable",
          plotOutput("plot_4")
        ),
        
        tabPanel(
          title = "Summary statistics",
          fluidRow(
            column(width = 4, strong(textOutput("num_var_1_title"))),
            column(width = 4, strong(textOutput("num_var_2_title"))),
            column(width = 4, strong(textOutput("fact_var_title")))
          ),
          fluidRow(
            column(width = 4, tableOutput("num_var_1_summary_table")),
            column(width = 4, tableOutput("num_var_2_summary_table")),
            column(width = 4, tableOutput("fact_var_summary_table"))
          ),
          fluidRow(
            column(width = 12, strong("Combined Statistics"))
          ),
          fluidRow(
            column(width = 12, tableOutput("combined_summary_table"))
          )
        )
      )
    )
  )
)



#One numerical variable
draw_plot_1 <- function(data_input, num_var_1, num_var_2, fact_var){
  if(fact_var!=not_sel){
    data_input[,(fact_var):= as.factor(data_input[,get(fact_var)])]
  }
  
  if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1)) +
      geom_histogram()
  }
  else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_2)) +
      geom_histogram()
  }
}


#Association of two numerical variables
draw_plot_2 <- function(data_input, num_var_1, num_var_2, fact_var){
  if(fact_var!=not_sel){
    data_input[,(fact_var):= as.factor(data_input[,get(fact_var)])]
  }
  if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2, color = fact_var)) +
      geom_point()
  }
  else if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2)) +
      geom_point()
  }
}


#Association of a numerical and factor variable
draw_plot_3 <- function(data_input, num_var_1, num_var_2, fact_var){
  
  if(fact_var!=not_sel){
    data_input[,(fact_var):= as.factor(data_input[,get(fact_var)])]
  }
  
  if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var, y = num_var_1)) +
      geom_boxplot()
  }
  else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var, y = num_var_2)) +
      geom_boxplot()
  }
}

#One factor variable
draw_plot_4 <- function(data_input, num_var_1, num_var_2, fact_var){
  if(fact_var!=not_sel){
    data_input[,(fact_var):= as.factor(data_input[,get(fact_var)])]
  }
  
  if(num_var_1 == not_sel & num_var_2 == not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var)) +
      geom_bar()
  }
}


create_num_var_table <- function(data_input, num_var){
  if(num_var != not_sel){
    col <- data_input[,get(num_var)]
    if (length(col)>5000) col_norm <- sample(col,5000) else col_norm <- col
    norm_test <- shapiro.test(col_norm)
    statistic <- c("mean", "median", "Standard deviation","Q1", "Q3", "IQR",
                   "Shapiro p-value")
    value <- c(round(mean(col),2), round(median(col),2), round(sd(col),2),
               round(quantile(col, 0.25),2), round(quantile(col, 0.75),2),
               round(IQR(col),2), norm_test$p.value)
    data.table(statistic, value)
  }
}




create_fact_var_table <- function(data_input, fact_var){
  if(fact_var != not_sel){
    freq_tbl <- data_input[,.N, by = get(fact_var)]
    freq_tbl <- setnames(freq_tbl,c("factor_value", "count"))
    freq_tbl
  }
}

create_combined_table <- function(data_input, num_var_1, num_var_2, fact_var){
  if(fact_var != not_sel){
    if(num_var_1 != not_sel & num_var_2 != not_sel){
      res_tbl <- data_input[,.(correlation = cor(get(num_var_1), get(num_var_2))), by = fact_var]
    }
    else if(num_var_1 != not_sel & num_var_2 == not_sel){
      res_tbl <- data_input[,.(mean = mean(get(num_var_1))), by = fact_var]
    }
    else if(num_var_1 == not_sel & num_var_2 != not_sel){
      res_tbl <- data_input[,.(mean = mean(get(num_var_2))), by = fact_var]
    }
  }
  else if(num_var_1 != not_sel & num_var_2 != not_sel){
    res_tbl <- data.table(
      statistic = c("correlation"),
      value = c(cor(
        data_input[,get(num_var_1)],
        data_input[,get(num_var_2)])))
  }
  return(res_tbl)
}

ui <- navbarPage(
  title = "Exploratory Data Analysis",
  main_page,
  about_page
)

server <- function(input, output){
  
  options(shiny.maxRequestSize=10*1024^2) 
  
  data_input <- reactive({
    req(input$csv_input)
    fread(input$csv_input$datapath)
  })
  
  observeEvent(data_input(),{
    choices <- c(not_sel,names(data_input()))
    updateSelectInput(inputId = "num_var_1", choices = choices)
    updateSelectInput(inputId = "num_var_2", choices = choices)
    updateSelectInput(inputId = "fact_var", choices = choices)
  })
  
  num_var_1 <- eventReactive(input$run_button,input$num_var_1)
  num_var_2 <- eventReactive(input$run_button,input$num_var_2)
  fact_var <- eventReactive(input$run_button,input$fact_var)
  
  # plot
  
  plot_1 <- eventReactive(input$run_button,{
    draw_plot_1(data_input(), num_var_1(), num_var_2(), fact_var())
  })
  
  output$plot_1 <- renderPlot(plot_1())
  
  plot_2 <- eventReactive(input$run_button,{
    draw_plot_2(data_input(), num_var_1(), num_var_2(), fact_var())
  })
  
  output$plot_2 <- renderPlot(plot_2())
  
  plot_3 <- eventReactive(input$run_button,{
    draw_plot_3(data_input(), num_var_1(), num_var_2(), fact_var())
  })
  
  output$plot_3 <- renderPlot(plot_3())
  
  plot_4 <- eventReactive(input$run_button,{
    draw_plot_4(data_input(), num_var_1(), num_var_2(), fact_var())
  })
  
  output$plot_4 <- renderPlot(plot_4())
  
  
  
  # 1-d summary tables
  
  output$num_var_1_title <- renderText(paste("Num Var 1:",num_var_1()))
  
  num_var_1_summary_table <- eventReactive(input$run_button,{
    create_num_var_table(data_input(), num_var_1())
  })
  
  output$num_var_1_summary_table <- renderTable(num_var_1_summary_table(),colnames = FALSE)
  
  output$num_var_2_title <- renderText(paste("Num Var 2:",num_var_2()))
  
  num_var_2_summary_table <- eventReactive(input$run_button,{
    create_num_var_table(data_input(), num_var_2())
  })
  
  output$num_var_2_summary_table <- renderTable(num_var_2_summary_table(),colnames = FALSE)
  
  output$fact_var_title <- renderText(paste("Factor Var:",fact_var()))
  
  fact_var_summary_table <- eventReactive(input$run_button,{
    create_fact_var_table(data_input(), fact_var())
  })
  
  output$fact_var_summary_table <- renderTable(fact_var_summary_table(),colnames = FALSE)
  
  # multi-d summary table
  
  combined_summary_table <- eventReactive(input$run_button,{
    create_combined_table(data_input(), num_var_1(), num_var_2(), fact_var())
  })
  
  output$combined_summary_table <- renderTable(combined_summary_table())
  
}

shinyApp(ui = ui, server = server)
