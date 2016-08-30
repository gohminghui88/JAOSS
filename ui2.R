#This scripts uses shiny and R to help doing descriptive and influential statistics
#
#Author: Eric Goh
#Department: Institutional Statistics
#Company: Nanyang Technological University
#
#License: LGPL, All rights reserved. 
#
#Reference: 
#1. http://blog.rstudio.org/2014/11/24/rvest-easy-web-scraping-with-r/
#2. http://shiny.rstudio.com/reference/shiny/latest/tableOutput.html
#3. http://www.statmethods.net/stats/rdiagnostics.html
#
#hosted at: https://gohminghui88.shinyapps.io/JAOSS/


#import "shiny" library for easy web interface development
library(shiny);

# Define UI for application that scrap ARWU University Ranking Page
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Just Another Online Statistics System (JAOSS) v1.0 alpha"),
  
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      
      #Data Input
      h3("Data (CSV format, at least 2 columns, Numerical)"),
      tags$br(),
      
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ), 
      
      tags$hr(),
                
      checkboxInput('header', 'Header', TRUE),
                
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      
      tags$hr(), 
      
      #Descriptive Statistics
      tags$br(),
      h3("Descriptives"),
      tags$br(),
      
      selectInput("varSelect", label = h3("Variables"), 
                  choices = list("Choice 1" = 1, "Choice 2" = 2,
                                 "Choice 3" = 3), selected = 1), 
  
      tags$hr(), 
      
      #Correlations Analysis
      tags$br(),
      h3("Correlation Analysis"),
      tags$br(),
      
      selectInput("corrSelect", label = h3("Methods"), 
                  choices = list("kendall" = 1, "pearson" = 2,
                                 "spearman" = 3), selected = 1), 
      
      selectInput("varSelect1", label = h3("Variables"), 
                  choices = list("Choice 1" = 1, "Choice 2" = 2,
                                 "Choice 3" = 3), selected = 1), 
      
      selectInput("varSelect2", label = h3("Variables"), 
                  choices = list("Choice 1" = 1, "Choice 2" = 2,
                                 "Choice 3" = 3), selected = 1), 
      tags$br(),
      tags$p("For Correlation Matrix, use Plots. "),
      
      
      #Plots
      tags$br(),
      h3("Plots"),
      tags$br(),
      
      selectInput("plotSelect", label = h3("Plot Type"), 
                  choices = list("histogram" = 1, "scatter" = 2,
                                 "pie" = 3, "bar" = 4, "boxplot" = 5, "Correlations Matrix" = 6), selected = 1), 
      
      selectInput("varSelect3", label = h3("Variables"), 
                  choices = list("Choice 1" = 1, "Choice 2" = 2,
                                 "Choice 3" = 3), selected = 1), 
      
      selectInput("varSelect4", label = h3("Variables"), 
                  choices = list("Choice 1" = 1, "Choice 2" = 2,
                                 "Choice 3" = 3), selected = 1), 
      
      
      #T-Test
      tags$br(),
      h3("T-Test"),
      tags$br(),
      
      selectInput("ttestSelect", label = h3("Paired"), 
                  choices = list("No" = 1, "Yes" = 2), selected = 1), 
      
      selectInput("varSelect5", label = h3("Variables"), 
                  choices = list("Choice 1" = 1, "Choice 2" = 2,
                                 "Choice 3" = 3), selected = 1), 
      
      selectInput("varSelect6", label = h3("Variables"), 
                  choices = list("Choice 1" = 1, "Choice 2" = 2,
                                 "Choice 3" = 3), selected = 1), 
      
      
      #ANOVA AND REGRESSION
      tags$br(),
      h3("ANOVA & Regression"),
      tags$br(),
      
      selectInput("methodSelect", label = h3("Method"), 
                  choices = list("ANOVA" = 1, "LINEAR REGRESSION" = 2), selected = 1),
      
      selectInput("varSelect7", label = h3("Response Variable (Compare with All Variables)"), 
                  choices = list("Choice 1" = 1, "Choice 2" = 2,
                                 "Choice 3" = 3), selected = 1), 
      
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br()
    ),
    
    
    #show tab panels, each with different components
    mainPanel(
      
      tabsetPanel(
        
        tabPanel('Data Table', dataTableOutput("dataTable")), 
        tabPanel('Descriptives', tableOutput("desTableOutput")), 
        tabPanel('Inferential', tableOutput("infTableOutput")), 
        tabPanel('Plots', plotOutput("plot1", click = "plot_click")), 
        tabPanel('T-Test', tableOutput("ttestTableOutput")), 
        tabPanel('ANOVA & Regression', tableOutput("anovaTableOutput"))
      )
      
    )
  )
))