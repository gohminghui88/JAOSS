#This scripts uses shiny and R to help doing descriptive and influential statistics
#This is the server side logic scripts for intefacing with ui.R. 
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

library(corrgram);
library(nnet);
library(e1071); #naive bayes and svm
library(rpart); #decision trees


# Define server logic required to scrape data
shinyServer(function(input, output, session) {
  
  #Data Input
  output$dataTable = renderDataTable({ 
    
    data <- readData(input);
    
    updateSelectInput(session, "varSelect", label = "variables",
                      choices = colnames(data), selected = colnames(data)[1]
    );
    
    updateSelectInput(session, "varSelect1", label = "variables",
                      choices = colnames(data), selected = colnames(data)[1]
    );
    
    updateSelectInput(session, "varSelect2", label = "variables",
                      choices = colnames(data), selected = colnames(data)[2]
    );
    
    updateSelectInput(session, "varSelect3", label = "variables",
                      choices = colnames(data), selected = colnames(data)[1]
    );
    
    updateSelectInput(session, "varSelect4", label = "variables",
                      choices = colnames(data), selected = colnames(data)[2]
    );
    
    updateSelectInput(session, "varSelect5", label = "variables",
                      choices = colnames(data), selected = colnames(data)[1]
    );
    
    updateSelectInput(session, "varSelect6", label = "variables",
                      choices = colnames(data), selected = colnames(data)[2]
    );
    
    updateSelectInput(session, "varSelect7", label = "Response Variable",
                      choices = colnames(data), selected = colnames(data)[1]
    );
    
    updateSelectInput(session, "varSelect8", label = "Response Variable",
                      choices = colnames(data), selected = colnames(data)[1]
    );
    
    
    dataResults <- data;
    
    data
    
  })
  
  #Descriptives Output
  output$desTableOutput = renderTable({
    
    data <- readData(input);
    selData <- data[, input$varSelect];
    
    desc <- summary(selData);
    variance <- var(selData);
    stdDev <- sd(selData);
    
    res <- data.frame(c("Variable", "Min", "1st Qu", "Median", "Mean", "3rd Qu", "Max", "Var.", "St. Dev."), c(toString(input$varSelect), desc[1], desc[2], desc[3], desc[4], desc[5], desc[6], variance, stdDev));
    
    res
    
    #paste("Column: ", toString(input$varSelect), " | Min: ", desc[1], " | 1st Qu: ", desc[2], " | Median: ", desc[3], " | Mean: ", desc[4], " | 3rd Qu: ", desc[5], " | Max: ", desc[6], " | Var.: ", variance, " | Std. Dev.: ", stdDev)
  })
  
  #Inferential Output
  output$infTableOutput = renderTable({
    
    data <- readData(input);
    cor_Method <- input$corrSelect;
    selData1 <- data[, input$varSelect1];
    selData2 <- data[, input$varSelect2];
    
    str_cor_Method <- "";
    corr <- 0;
    
    if(cor_Method == 1) { 
      str_cor_Method <- "kendall"; 
      corr <- cor(selData1, selData2, method="kendall");
    }
    
    else if(cor_Method == 2) { 
      str_cor_Method <- "pearson"; 
      corr <- cor(selData1, selData2, method="pearson");
    }
    
    else { 
      str_cor_Method <- "spearman"; 
      corr <- cor(selData1, selData2, method="spearman");
    }
    
    column_str <- paste(toString(input$varSelect1), " & ", toString(input$varSelect2));
    
    res <- data.frame(c("Variables", "Correlations", "Method"), c(column_str, toString(corr), str_cor_Method));
    #paste("Columns: ", toString(input$varSelect1), " & ", toString(input$varSelect2), " | Correlation: ", corr, " | Method: ", str_cor_Method)
    
    res
  })
  
  #plotOutput
  output$plot1 <- renderPlot({
    
    data <- readData(input);
    plot_Method <- input$plotSelect;
    selData1 <- data[, input$varSelect3];
    selData2 <- data[, input$varSelect4];
    
    
    if(plot_Method == 1) {
      hist(selData1, main=toString(input$varSelect3));  
    }
    
    else if(plot_Method == 2) {
      plot(selData1, selData2, main=paste(toString(input$varSelect3), " vs ", toString(input$varSelect4)));
    }
    
    else if(plot_Method == 3) {
      mytable <- table(selData1);
      pie(mytable, main=toString(input$varSelect3));
    }
    
    else if(plot_Method == 4) {
      mytable <- table(selData1);
      barplot(selData1, horiz=TRUE, main=toString(input$varSelect3));
    }
    
    else if(plot_Method == 5) {
      boxplot(selData1, main=toString(input$varSelect3));
    }
    
    else if(plot_Method == 6) {
      
      corrgram(data);
    }
  })
  
  #T-Test Output
  output$ttestTableOutput = renderTable({
    
    data <- readData(input);
    ttest_Method <- input$ttestSelect;
    selData1 <- data[, input$varSelect5];
    selData2 <- data[, input$varSelect6];
    
    str_ttest_Method <- "";
    res <- "";
    
    if(ttest_Method == 1) { 
      str_ttest_Method <- "No"; 
      res <- t.test(selData1, selData2, paired=FALSE);
    }
    
    else { 
      str_ttest_Method <- "Yes"; 
      res <- t.test(selData1, selData2, paired=TRUE);
    }
    
    str_Columns <- paste(toString(input$varSelect5), " & ", toString(input$varSelect6));
    
    resTable <- data.frame(c("Variables", "Paired?", "t", "df", "P-Value", "95% Confidence Interval"), c(str_Columns, str_ttest_Method, toString(res[1]), toString(res[2]), toString(res[3]), toString(res[4])));
    
    resTable
    #paste("Columns: ", toString(input$varSelect5), " & ", toString(input$varSelect6), "Test Paired: ", str_ttest_Method, " | t: ", res[1], " | df: ", res[2], " | p-Value: ", res[3], " | 95% Confidence Interval: ", res[4])
    #paste(res)
  })
  
  #ANOVA Output
  output$anovaTableOutput = renderTable({
    
    dataInput <- readData(input);
    method <- input$methodSelect;
    selData1 <- dataInput[, input$varSelect7];
    
    str_Method <- "";
    res <- "";
    
    if(method == 1) { 
      str_Method <- "ANOVA"; 
      res <- aov(selData1 ~ ., data=dataInput);
      res <- summary(res);
      res <- as.data.frame(res[[1]]);
    }
    
    else {
      str_Method <- "Linear Regression";
      
      coefficients <- lm(selData1 ~ ., data=dataInput);
      res <- as.data.frame(coefficients[[1]]);
    }
    
    res;
    #paste("Response Variable: ", toString(input$varSelect7), "Method: ", str_Method, " | results: ", res)
    #paste(res)
  })
  
  
  
  #PREDICTIVE Output
  
  #Model Evaluation
  output$evalTableOutput = renderTable({
  
    #Get data
    data <- readData(input);
    
    #Get testData and trainData from data
    index <- 1:nrow(data)
    testindex <- sample(index, trunc(length(index)/3))
    testset <- data[testindex,]
    trainset <- data[-testindex,]
    
    #Get Response Variable and Classifier Type
    classifier_type <- input$predSelect;
    selResponseVar <- trainset[, input$varSelect8];
    
    pred_model <- NULL;
    res <- NULL;
    
    
    #Neural Network
    if(classifier_type == 3)
    {
      
      pred_model <- nnet(selResponseVar ~ ., data=trainset, size=2, maxit=500);
      pred_res <- predict(pred_model, testset, type="class");
      
      res <- table(pred = pred_res, true = testset[,input$varSelect8]);
    }
    
    #Naive Bayes
    else if(classifier_type == 1)
    { 
      pred_model <- naiveBayes(selResponseVar ~ ., data=trainset);
      pred_res <- predict(pred_model, testset);
      
      res <- table(pred = pred_res, true = testset[,input$varSelect8]);
    }
    
    #SVM
    else if(classifier_type == 2)
    {
      pred_model <- svm(selResponseVar ~ ., data=trainset);
      pred_res <- predict(pred_model, testset);
      
      res <- table(pred = pred_res, true = testset[,input$varSelect8]);
    }
    
    res
    
  })
  
  #Prediction
  output$predTableOutput = renderDataTable({
  
    #input data for prediction
    inFile2 <- input$file2;
    
    if (is.null(inFile2))
      return(NULL);
    
    data2Pred <- read.csv(inFile2$datapath, header = input$header2,
                     sep = input$sep2, quote = input$quote2);
  
    #Get training data
    trainData <- readData(input);
    
    #Get Response Variable and Classifier Type
    classifier_type <- input$predSelect;
    selResponseVar <- trainData[, input$varSelect8];
    
    pred_model <- NULL;
    res <- NULL;
    
    
    #Neural Network
    if(classifier_type == 3)
    {
      
      pred_model <- nnet(selResponseVar ~ ., data=trainData, size=2, maxit=500);
      pred_res <- data.frame(predict(pred_model, data2Pred, type="class"));
      
      res <- cbind(data2Pred, pred_res);
    }
    
    #Naive Bayes
    else if(classifier_type == 1)
    {
      pred_model <- naiveBayes(selResponseVar ~ ., data=trainData);
      pred_res <- data.frame(predict(pred_model, data2Pred));
      
      res <- cbind(data2Pred, pred_res);
    }
    
    #SVM
    else if(classifier_type == 2)
    {
      pred_model <- svm(selResponseVar ~ ., data=trainData);
      pred_res <- data.frame(predict(pred_model, data2Pred));
      
      res <- cbind(data2Pred, pred_res);
    }
    
    res
  })
  
})


#Global Variables
dataResults <- "";


#function to extract data from extracted html table codes
initData <- function(){
  
  Example_A <- c(1, 2, 3);
  Example_B <- c(1, 3, 4);
  Example_C <- c(5, 6, 7);
  
  data <- data.frame(Example_A, Example_B, Example_C);
  
  return(data);
}

readData <- function(input) {
  
  inFile <- input$file1;
  
  if (is.null(inFile))
    return(NULL);
  
  data <- read.csv(inFile$datapath, header = input$header,
                   sep = input$sep, quote = input$quote);
  
  return(data);
  
}

getVariables <- function(input) {
  data <- readData(input);
  
  return (colnames(data));
}





