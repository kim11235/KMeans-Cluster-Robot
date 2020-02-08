#list.of.packages <- c("dplyr", "shiny", "tidyr", "stringr", "lubridate")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)
#install.packages("shiny", dependencies = TRUE)
#install.packages("shinydashboards")

library(tidyr)
library(stringr)
library(dplyr)
library(lubridate)
library(shiny)
library(readr)
library(ggplot2)

source('data/getClusters.R')

ui<-fluidPage(
  titlePanel("K-Means Cluster Bot"),
  sidebarLayout(
    sidebarPanel(
      p("Upload the data you want to cluster, and select the number of clusters you want to use based on the plot of within-clusters sum of squares."),
        p("Non-numeric columns will be excluded from the clustering analysis."),
      p("Data must be cleaned with no NA values.  The first column must contain a unique ID for each observation."),
      p("The app scales each numeric column between 0 and 1, runs k-means clustering for up to 15 clusters, and displays the results in a plot."),
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      sliderInput(inputId = "num_clusters", label = "Number of Clusters", min = 1, max = 15, value = 3, round = T)
    ),
    mainPanel(
      plotOutput("wss_plot"),

      # Button
      fluidRow(column(width = 4, br(), uiOutput("download_button")),
               column(width = 4, uiOutput("var_x_select")),
               column(width = 4, uiOutput("var_y_select"))
      ),
      plotOutput("cluster_plot"),
      tableOutput(outputId = 'contents')
    )
  )
)

server<-function(input, output){
  #Read input file from user and save in reactive list contents

  contents <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath)
  })
  
  #Call the function to calculate the k-means clusters
  df<-reactive({
    req(contents())
    getClusters(contents())
  })

  #Read the within-clusters sum of squares calculations from the clustering results
  wss<-reactive({
    req(df())
    df()[[2]]
  })

  #Plot the wss data
  output$wss_plot<-renderPlot({
    plot(1:15, wss(),
                 type="b", pch = 19, frame = FALSE, 
                 xlab="Number of clusters K",
                 ylab="Total within-clusters sum of squares")
  })
  
  #Read the cluster data, based on the user-specified number of clusters
  cluster_results<-reactive({
    num<-input$num_clusters
    
    cluster_results<-df()[[1]]
    
    #Count how many columns belong to the original data frame
    training_data_columns<-ncol(cluster_results)-15
    
    #Create a dataframe containing the original data and the column of cluster assignments
    #, based on the user's desired # of clusters
    cluster_results<- cluster_results[,c(1:training_data_columns, (training_data_columns+num))]
  })
  
  #Plot the results of the cluster analysis
  output$cluster_plot<-renderPlot({
    req(input$var_x_col)
    if(input$var_x_col != input$var_y_col) {
        plot_data<-cluster_results() %>%
          select(input$var_x_col, input$var_y_col, contains('Clusters_'))
      
        colnames(plot_data)<-c("X", "Y", "Clusters")
        
        plot_data<-plot_data %>% mutate(Clusters = as.factor(Clusters))
        
        plot_data %>%
          ggplot(aes(X, Y, colour = Clusters)) +
         geom_point(size = 3) +
         labs(x = input$var_x_col, y = input$var_y_col) +
        theme(axis.title = element_text(size = 16),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 14),
              legend.title = element_text(size = 16))
    }
    #plot(cluster_results()[,1]~cluster_results()[,2], col = cluster_results()[,ncol(cluster_results())], pch = 16)
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Cluster Results", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(cluster_results(), file, row.names = FALSE)
    }
  )
  output$download_button<-renderUI({
    req(cluster_results())
    downloadButton(label = "Download Cluster Results", 
                   inputId = "downloadData",
                   outputId = "downloadData")
  })
  output$var_x_select<-renderUI({
    req(cluster_results())
    vars<-colnames(cluster_results())[2:ncol(cluster_results())-1]
    selectInput(inputId = "var_x_col", label = "Plot Variable 1", choices = vars)
  })
  output$var_y_select<-renderUI({
    req(cluster_results())
    vars<-colnames(cluster_results())[2:ncol(cluster_results())-1]
    vars<-vars[!(vars %in% input$var_x_col)]
    selectInput(inputId = "var_y_col", label = "Plot Variable 2", choices = vars, selected = TRUE)
  })
}

shinyApp(ui, server)
