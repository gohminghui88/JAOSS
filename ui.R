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
#4. https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/SVM
#
#hosted at: https://gohminghui88.shinyapps.io/JAOSS/


#import "shiny" library for easy web interface development
library(shiny);


shinyUI(navbarPage("JAOSS v1.0 beta", 
      
      #Data Input
      tabPanel("Data", 
            sidebarLayout(
              sidebarPanel(
                 
                
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
                
                tags$hr() 
                                
              ), 
                            
            mainPanel(
                 
              dataTableOutput("dataTable")  
              #downloadButton("dataFileOutput", "Download")
                              
            )
        )
                            
      ), 
      
                   
                   
      #Descriptive Statistics
      tabPanel("Descriptives", 
               
               sidebarLayout(
                 sidebarPanel(
                   
                   
                   tags$br(),
                   h3("Descriptives"),
                   tags$br(),
                   
                   selectInput("varSelect", label = h3("Variables"), 
                               choices = list("Choice 1" = 1, "Choice 2" = 2,
                                              "Choice 3" = 3), selected = 1), 
                   
                   tags$hr()
                   
                 ), 
               
               mainPanel(
                 
                 tableOutput("desTableOutput")
                 
               )
                 
          )
               
               
      ), 
      
      
      
      #Correlations Analysis
      tabPanel("Correlations", 
          sidebarLayout(
            sidebarPanel(     
            
              
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
              
              tags$hr()
            ),  
          
          mainPanel(
                 
            tableOutput("infTableOutput")
                 
          )
        )
      ), 
      
      
      
      #Plots
      tabPanel("Plots", 
         
               sidebarLayout(
                 sidebarPanel(     
                   
                   
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
                                              "Choice 3" = 3), selected = 1)
                   
                   
                 ),  
               
               mainPanel(
                 
                 plotOutput("plot1", click = "plot_click")
                 
               )  
            )              
      ),
      
      
      #T-Test
      tabPanel("T-Test", 
                            
               sidebarLayout(
                 sidebarPanel(     
                   
                   
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
                                              "Choice 3" = 3), selected = 1)
                   
                 ), 
               
               mainPanel(
                 
                 tableOutput("ttestTableOutput")
                 
               )
          )
      ), 
                   
      #ANOVA AND REGRESSION             
      tabPanel("ANOVA/Regression", 
            
               sidebarLayout(
                 sidebarPanel(     
                   
                   tags$br(),
                   h3("ANOVA & Regression"),
                   tags$br(),
                   
                   selectInput("methodSelect", label = h3("Method"), 
                               choices = list("ANOVA" = 1, "LINEAR REGRESSION" = 2), selected = 1),
                   
                   selectInput("varSelect7", label = h3("Response Variable (Compare with All Variables)"), 
                               choices = list("Choice 1" = 1, "Choice 2" = 2,
                                              "Choice 3" = 3), selected = 1), 
                   
                   tags$br()
                 ), 
               
               mainPanel(
                 
                 tableOutput("anovaTableOutput")
                 
               )
          )
                            
      ), 
    
    
      #Predictive Analytics             
      tabPanel("Predictive Analytics", 
                            
            sidebarLayout(
                sidebarPanel(     
                                
                    tags$br(),
                    h3("Predictive Analytics"),
                    tags$br(),
                                
                    selectInput("predSelect", label = h3("Classifiers"), 
                        choices = list("Naive Bayes" = 1, "SVM" = 2, "Neural Network" = 3), selected = 1),
                                
                    selectInput("varSelect8", label = h3("Response Variable (Compare with All Variables)"), 
                        choices = list("Choice 1" = 1, "Choice 2" = 2,
                            "Choice 3" = 3), selected = 1), 
                                
                    tags$br(), 
                    tags$hr(),
                    
                    fileInput('file2', 'upload file to predict',
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
                    
                    checkboxInput('header2', 'Header', TRUE),
                    
                    radioButtons('sep2', 'Separator',
                                 c(Comma=',',
                                   Semicolon=';',
                                   Tab='\t'),
                                 ','),
                    
                    radioButtons('quote2', 'Quote',
                                 c(None='',
                                   'Double Quote'='"',
                                   'Single Quote'="'"),
                                 '"'),
                    
                    tags$hr() 
                ), 
                              
                mainPanel(
                    h3("Model Evaluation (trainset=66.67%, testset=33.33%)"), 
                    tags$br(), 
                    tags$p("Confusion Matrix: Predict vs Actual"), 
                    tableOutput("evalTableOutput"),
                    tags$br(),  
                    tags$hr(),
                    tags$br(),  
                    h3("Prediction"),
                    tags$br(),
                    dataTableOutput("predTableOutput")
                                
                )
          )
                            
    ),                
                   
    #About
    tabPanel("About", 
      
             #verbatimTextOutput("summary", 
              
                  h3("Just Another Online Statistics System (JAOSS) v1.0 beta"),
                  tags$br(),
                  tags$p("Created by Eric Goh M. H., Institutional Statistics, Nanyang Technological University"),
                  tags$p("License: GNU GPL"),
                  tags$p("Description: JAOSS is an online statistical system that enables quick statistics analysis and visualization. It is not meant to replace softwares like SPSS and SAS. System is in beta version and has been tested with IRIS Dataset. "),
                  tags$br(), 
                  tags$p("All Rights Reserved. Copyrighted 2016. ")
                                
              #)
             
    )
))



