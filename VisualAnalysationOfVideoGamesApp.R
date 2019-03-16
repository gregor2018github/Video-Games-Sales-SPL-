#First of all you have to download the .csv data file "Video.csv". Put it in a folder of your choice.
#Change the direction in line 3 to the location of this data file.
#Please note that the program will check for the required packages. You might need an internet connection to download them.

setwd("D:/Uni/SPL") #change this direction to the location of "Video.csv" on your machine

#loading and cleaning the data

fullData = read.csv("Video.csv", header = TRUE)

fullData$PlatformGroup = fullData$Platform
levels(fullData$PlatformGroup) = c("Other", "Other", "Nintendo", "Other", "Nintendo", "Nintendo", "Nintendo", "Nintendo", 
                                   "Other", "Other", "Nintendo", "Nintendo", "Other", "PC","Other", "Playstation", 
                                   "Playstation","Playstation", "Playstation","Playstation","Playstation", "Other", 
                                   "Other", "Nintendo", "Other", "Nintendo", "Nintendo", "Other", "Microsoft", "Microsoft", "Microsoft")


reviewData = (fullData[!is.na(fullData$Critic_Count)&!is.na(fullData$User_Count),]) #create a second dataframe called 'reviewData'
reviewData$User_Score = as.numeric(reviewData$User_Score)
reviewData$Disparity_Score = (reviewData$Critic_Score - reviewData$User_Score)

salesData = fullData[,c(1:10,17)] #create a third dataframe called 'salesData'

#Shiny App starts here

if (!require("shiny")) install.packages('shiny') #check for required packages
if (!require("DT")) install.packages('DT')
if (!require("ggplot2")) install.packages('ggplot2')
if (!require("hexbin")) install.packages('hexbin')
if (!require("stargazer")) install.packages('stargazer')
  
library(shiny) #load packages
library(DT)
library(ggplot2)
library(hexbin)
library(stargazer)

ui <- #define user interface
  navbarPage(title = "Video Games - Sales and Reviews", #navigation bar at the top of the window
    
    tabPanel(title = "General Plots", # adding of several tabs to the main navigation bar
             
             verbatimTextOutput(outputId = "text1"), # adding output plots to the tabs
             plotOutput(outputId = "soldYearGenre"),
             plotOutput(outputId = "soldYearPlatform"),
             verbatimTextOutput(outputId = "text2"),
             plotOutput(outputId = "disparityYearBars"),
             plotOutput(outputId = "reviewHexScores"),
             plotOutput(outputId = "reviewHexCritics"),
             verbatimTextOutput(outputId = "text4"),
             plotOutput(outputId = "absPublishYearLine"),
             plotOutput(outputId = "relPublishYearLine"),
             plotOutput(outputId = "pieChart"),
             verbatimTextOutput(outputId = "text5"),
             verbatimTextOutput(outputId = "regression")),
    
    tabPanel(title = "Explore - Review Data",
             fluidRow(column(4,selectInput("reviewDataHistType", "Choose a Data Type:", #defining rows and columns to keep elements (for example sliders) in line
                                           list("Critic_Score", "User_Score", "Critic_Count", "User_Count", "Disparity_Score"
                                           ))),
                      column(4,selectInput("reviewDataHistGenreFilter", "Choose a Genre-Filter:",
                                           list("All Data",
                                                `Genre` = list("Action", "Adventure", "Fighting", "Misc", "Platform", "Puzzle", "Racing", "Role-Playing", "Shooter", "Simulation", "Sports", "Strategy")
                                           ))),
                      column(4,selectInput("reviewDataHistPlatformFilter", "Choose a Platform-Filter:",
                                           list("All Data",
                                                `Platform` = list("3DS","DS","GBA","GC","PC","PS","PS2","PS3","PS4","PSP","PSV","Wii","WiiU","X360","XB","XOne")
                                           )))
                      ),
             plotOutput(outputId = "reviewDataHist"),
             verbatimTextOutput("reviewDataHistInfo")),
    
    tabPanel(title = "Explore - Sales Data",
             fluidRow(column(4,selectInput("selectSalesData1", "Choose Sales Area 1:",
                                          list("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales"),
                                          selected = "NA_sales")),
                      column(4,selectInput("selectSalesData2", "Choose Sales Area 2:",
                                          list("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales"),
                                          selected = "Global_Sales")),
                      column(4,selectInput("salesDataPlotPlatformFilter", "Choose a Platform-Filter:",
                                           list("All Data",
                                                `Platform` = list("3DS","DS","GBA","GC","PC","PS","PS2","PS3","PS4","PSP","PSV","Wii","WiiU","X360","XB","XOne")
                                           )))
                      ),
             fluidRow(column(4,sliderInput(inputId = "SliderXPlot", label = "Choose a Range for the X-Axis", 
                                          value = c(0,11), max = 85, min = 0, step = 1)),
                      column(4,sliderInput(inputId = "SliderYPlot", label = "Choose a Range for the Y-Axis", 
                                          value = c(0,30), max = 85, min = 0, step = 1))),
             plotOutput("salesDataPlot", click = "plot_click"),
             verbatimTextOutput("salesDataPlotInfo")),
    
    tabPanel(title = "Data Browser",
             selectInput("dataset", "Choose a data set:",
                         list("Full Dataset", "Sales Dataset", "Review Dataset")),
             DT::dataTableOutput("mytable")) #data browser 
    
  )

server <- function(input, output){ # server function renders the outputs that are displayed in the user interface
  
  output$absPublishYearLine = renderPlot({#define contingency table to calculate the number of published games by year and by genre
    
    absFreqTable <- table(salesData$Year_of_Release, salesData$Genre)[1:37,2:13]
    
    absFreqTable <- as.data.frame.matrix(absFreqTable) #convert table into dataframe to use them with ggplot
    
    Year <- c(1980:2016) #display graphs with the package ggplot
    ggplot(absFreqTable, aes(Year, Absolute_Frequency_of_Games_Published)) + 
      geom_line(aes(y = Action, colour = "Action")) +      
      geom_line(aes(y = Adventure, colour = "Adventure")) +
      geom_line(aes(y = Fighting, colour = "Fighting")) + 
      geom_line(aes(y = Misc, colour = "Miscellaneous")) +
      geom_line(aes(y = Platform, colour = "Platform")) +
      geom_line(aes(y = Puzzle, colour = "Puzzle")) + 
      #geom_line(aes(y = Racing, colour = "Racing")) +
      #geom_line(aes(y = Role-Playing, colour = "Role-Playing")) +
      geom_line(aes(y = Shooter, colour = "Shooter")) +
      geom_line(aes(y = Simulation, colour = "Simulation")) + 
      #geom_line(aes(y = Strategy, colour = "Strategy")) + 
      geom_line(aes(y = Sports, colour = "Sports")) + xlab("Year") + ylab("Absolute Number of Games Published") + ggtitle("Number of Games Published per Year")
      
    
  })
  
  output$relPublishYearLine = renderPlot({ #define contingency table to calculate the number of published games by year and by genre
    
    relFreqTable = table(salesData$Year_of_Release, salesData$Genre)[1:37,2:13] #absolute frequencys

    for(i in 1:11){
      relFreqTable[,i] = relFreqTable[,i]/table(salesData$Year_of_Release)[1:37]
    }
    
    relFreqTable = as.data.frame.matrix(relFreqTable) #convert table into dataframe to use it with the package ggplot
    
    Year = c(1980:2016) #display graph with the package ggplot
    ggplot(relFreqTable, aes(Year)) + 
      geom_line(aes(y = Action, colour = "Action")) +
      geom_line(aes(y = Shooter, colour = "Shooter")) +
      geom_line(aes(y = Sports, colour = "Sports")) + 
      xlab("Year") + ylab("Relative Number of Games Published") + ggtitle("Share of Games Published in a Certain Genre")
    
  })
  
  output$soldYearGenre = renderPlot({
    
    ggplot(salesData, aes(fill = Genre, y = as.numeric(Global_Sales), x = as.numeric(as.character(Year_of_Release)))) + geom_bar(stat = "identity") + xlab("Year") + xlim(1980,2016) + ylab("Global Sales in Million Units") + ggtitle("Global Sales by Year and Genre") 
    
  })
  
  output$soldYearPlatform = renderPlot({
    
    ggplot(salesData, aes(fill = PlatformGroup, y = as.numeric(Global_Sales), x = as.numeric(as.character(Year_of_Release)))) + geom_bar(stat = "identity") + xlab("Year") + xlim(1980,2016) + ylab("Global Sales in Million Units") + ggtitle("Global Sales by Year and Platform Group") 
    
  })
  
  output$disparityYearBars = renderPlot({
    
    ggplot(reviewData, aes(fill = PlatformGroup, y = as.numeric(Disparity_Score), x = as.numeric(as.character(Year_of_Release)))) + geom_bar(stat = "identity") + xlim(1995, 2016) + xlab("Year") + ylab("Disparity Score") + ggtitle("Disparity Score - Positive and Negative Sum for a given Year")
    
  })
  
  output$pieChart = renderPlot({
    
    slices <- colSums(Filter(is.numeric, salesData[,-10]))
    
    
    lbls <- c("NA-Sales","EU-Sales","JP-Sales","Other Sales")
    lbls <- paste(lbls,slices, sep = " - ")
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct, sep =" - ") # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # ad % to labels 
    
    pie(slices,
        main="Sales by Region in Million Units", labels = lbls)
    
  })
  
  output$regression = renderPrint({
    
    mixedLinearModel <- lm(as.numeric(Global_Sales) ~ 	
                               as.numeric(User_Score) + 
                               as.numeric(Critic_Score) + 
                               as.numeric(User_Count) + 
                               as.numeric(Critic_Count) + 
                               as.numeric(as.character(Year_of_Release)) +
                               PlatformGroup, data = reviewData)
    summary(mixedLinearModel)
    
  })
  
  output$reviewDataHist = renderPlot({ #showing a Kernel Density Estimation for filtered or unfiltered review data
    
    #if user wants to see all data
    if(input$reviewDataHistGenreFilter=="All Data" && input$reviewDataHistPlatformFilter=="All Data"){
      
      hist(reviewData[,input$reviewDataHistType],
           freq = FALSE, 
           main = c("Number of Observations: ", as.character(length(reviewData[,input$reviewDataHistType]))),
           xlab = "Value", breaks = 80)
    
      lines(density(reviewData[,input$reviewDataHistType]), col = "red")
      
    #if user wants to see a special genre  
    }else if(input$reviewDataHistGenreFilter!="All Data" && input$reviewDataHistPlatformFilter=="All Data"){
      hist(reviewData[reviewData$Genre==input$reviewDataHistGenreFilter,input$reviewDataHistType],
           freq = FALSE, 
           main = c("Number of Observations: ", as.character(length(reviewData[reviewData$Genre==input$reviewDataHistGenreFilter,input$reviewDataHistType]))),
           xlab = "Value", breaks = 50)
      
      lines(density(reviewData[reviewData$Genre==input$reviewDataHistGenreFilter,input$reviewDataHistType]), col = "red")
    
    #if user wants to see special Platform
    }else if(input$reviewDataHistGenreFilter=="All Data" && input$reviewDataHistPlatformFilter!="All Data"){
      hist(reviewData[reviewData$Platform==input$reviewDataHistPlatformFilter,input$reviewDataHistType],
           freq = FALSE, 
           main = c("Number of Observations: ", as.character(length(reviewData[reviewData$Platform==input$reviewDataHistPlatformFilter,input$reviewDataHistType]))),
           xlab = "Value", breaks = 50)
      
      lines(density(reviewData[reviewData$Platform==input$reviewDataHistPlatformFilter,input$reviewDataHistType]), col = "red")
    
    #if user wants to apply two special filters  
    }else{
      
      hist(reviewData[reviewData$Genre==input$reviewDataHistGenreFilter & reviewData$Platform==input$reviewDataHistPlatformFilter,input$reviewDataHistType],
           freq = FALSE, 
           main = c("Number of Observations: ", as.character(length(reviewData[reviewData$Genre==input$reviewDataHistGenreFilter & reviewData$Platform==input$reviewDataHistPlatformFilter,input$reviewDataHistType]))),
           xlab = "Value")
      
      lines(density(reviewData[reviewData$Genre==input$reviewDataHistGenreFilter & reviewData$Platform==input$reviewDataHistPlatformFilter,input$reviewDataHistType]), col = "red")
    }
    
    })
  
  output$reviewDataHistInfo = renderPrint({
    #if all data
    if(input$reviewDataHistGenreFilter=="All Data" && input$reviewDataHistPlatformFilter=="All Data"){
      
      summary(reviewData[,input$reviewDataHistType])
      
    #if special genre  
    }else if(input$reviewDataHistGenreFilter!="All Data" && input$reviewDataHistPlatformFilter=="All Data"){
      
      summary(reviewData[reviewData$Genre==input$reviewDataHistGenreFilter,input$reviewDataHistType])
    
    #if special Platform  
    }else if(input$reviewDataHistGenreFilter=="All Data" && input$reviewDataHistPlatformFilter!="All Data"){
      
      summary(reviewData[reviewData$Platform==input$reviewDataHistPlatformFilter,input$reviewDataHistType])
    
    #if two special filters  
    }else{
      
      summary(reviewData[reviewData$Genre==input$reviewDataHistGenreFilter & reviewData$Platform==input$reviewDataHistPlatformFilter,input$reviewDataHistType])
    }
    
  })
  
  output$reviewHexScores = renderPlot({
    
    ggplot(reviewData, aes(User_Score, Critic_Score)) + geom_hex(colour = "darkblue") + xlab("Average Score given by Users") + ylab("Average Score given by Critics") + ggtitle("Comparison of Review Scores")
    
  })
  
  output$reviewHexCritics = renderPlot({
    
    ggplot(reviewData, aes(Critic_Count, Critic_Score)) + geom_hex(colour = "darkblue") + xlab("Number of Critic Reviews") + ylab("Average Score given by Critics") + ggtitle("Overview of Critic's Reviews")
    
  })
  
  output$salesDataPlot = renderPlot({ #create a scatterplot, user can define x-axis range, y-axis range, 
    
    #if user wants to see all data
    if(input$salesDataPlotPlatformFilter=="All Data"){ 
      
      plot(salesData[,input$selectSalesData1],salesData[,input$selectSalesData2],
           xlim=input$SliderXPlot, ylim=input$SliderYPlot,
           xlab = c(input$selectSalesData1,"(in mio Units)"), ylab = c(input$selectSalesData2,"(in mio Units)"), col=salesData$PlatformGroup)
      
      fit <- lm(salesData[,input$selectSalesData2] ~ salesData[,input$selectSalesData1])
      abline(fit, col = "orange")
      
      legend("bottomright",legend=c("Microsoft", "Playstation", "PC", "Nintendo", "Other", "Linear Regression"),
             col=c("cyan", "blue", "green", "red", "black", "orange"), lty=1:2, cex=0.8)
    
      #if user wants to see special platform
    } else{ 
      
      plot(salesData[salesData$Platform==input$salesDataPlotPlatformFilter,input$selectSalesData1],salesData[salesData$Platform==input$salesDataPlotPlatformFilter,input$selectSalesData2],
           xlim=input$SliderXPlot, ylim=input$SliderYPlot,
           xlab = c(input$selectSalesData1,"(in mio Units)"), ylab = c(input$selectSalesData2,"(in mio Units)"))
      
      fit <- lm(salesData[salesData$Platform==input$salesDataPlotPlatformFilter,input$selectSalesData2] ~ salesData[salesData$Platform==input$salesDataPlotPlatformFilter,input$selectSalesData1])
      abline(fit, col = "orange")
      
    }
    
  })
  
  output$salesDataPlotInfo = renderText({
    
    paste0("Click in the graph to receive the coordinates here.","\nx=", input$plot_click$x, "\ny=", input$plot_click$y)
    
  })
  
  output$mytable = DT::renderDataTable({
    
    if(input$dataset=="Full Dataset"){
      fullData
    }else if(input$dataset=="Review Dataset"){
      reviewData
    }else {
      salesData
    }
      
  })
  
  output$text1 <-renderText({
    
    paste0("This application will perform a visual and descriptive analysis of a data set called 'Video Games Sales with Ratings'.","\nThe data set contains 16719 video games that were published between 1980 and 2016.","\nThe respective data comes from a data collection of the website 'www.VGchartz.com'.","\nIn this tab you may see some general information and graphs. Click on the other tabs to explore the data on your own.")
    
  })
  
  output$text2 = renderText({
    
    paste0("The following three graphs depict evaluation data by critic's and users' of different games.","\nThe 'Disparity score' is the difference between the critics and the users average evaltuations for each game.","\nA positive value means, that critics evaluated the game better than the players and vice versa.")
    
  })
  
  output$text4 = renderText({
    
    paste0("The next two outputs examine the number of games published.","\nYou can see the absolute and relative number of games published by year for selected genres.")
    
  })
  
  output$text5 = renderText({
    
    paste0("The following text output summarises a linear regression.","\nThe global sales are regressed to different numeric variables and the platformGroup.")
    
  })
  
}
shinyApp(ui = ui, server = server)