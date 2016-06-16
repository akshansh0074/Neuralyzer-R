shinyUI(fluidPage( 
  titlePanel("Sentiment Analysis"), #Title
  textOutput("currentTime"),   #Here, I show a real time clock
  h4("Tweets:"),   #Sidebar title
  sidebarLayout(
    sidebarPanel(
      dataTableOutput('tweets_table') #Here I show the users and the sentiment
    ),
    #     Show a plot of the generated distribution:
    #       
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("distPlot2"),#Here I will show the bars graph
      sidebarPanel(
        plotOutput("positive_wordcloud") #Cloud for positive words
      ),
      sidebarPanel(
        plotOutput("negative_wordcloud") #Cloud for negative words
      ),
      sidebarPanel(
        plotOutput("neutral_wordcloud") #Cloud for neutral words
      )))))