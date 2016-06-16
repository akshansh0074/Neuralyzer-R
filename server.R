library(shiny) 
library(tm)
library(wordcloud)
library(ROAuth)
library(twitteR)

library(sentiment)
library(plyr)
library(ggplot2)

library(RColorBrewer)
shinyServer(function(input, output, session) {
  # download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
  setup_twitter_oauth("LJeW6VWSn16ThejU3ZAkK1xwH", "YtQ8Zh3Y4t5IgJ9cD2ulaYLjFNbvnnk2GnhngFYR8lClL7Abej",access_token="516433587-sy05O9ndz3N7zmpf5ACNCrcMhphDb60p8vRQ6GrK",  access_secret="Cb2SYKIwMnX0cAzK5yI5Xs8h9bIGkB0PmP35oi9rfjt7f")
  # token <- get("oauth_token", twitteR:::oauth_cache) #Save the credentials info
  # token$cache()
  # output$currentTime <- renderText({invalidateLater(1000, session) #Here I will show the current time
  #                                   paste("Current time is: ",Sys.time())})
  observe({
    invalidateLater(60000,session)
    # count_positive = 0
    # count_negative = 0
    # count_neutral = 0
    # positive_text <- vector()
    # negative_text <- vector()
    # neutral_text <- vector()
    # vector_users <- vector()
    # vector_sentiments <- vector()
    # tweets_result = ""
    some_tweets = searchTwitter(searchString ="akshansh", n=15, lang="en")
    
    some_txt = sapply(some_tweets, function(x) x$getText())
    # remove retweet entities
    some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
    # remove at people
    some_txt = gsub("@\\w+", "", some_txt)
    # remove punctuation
    some_txt = gsub("[[:punct:]]", "", some_txt)
    # remove numbers
    some_txt = gsub("[[:digit:]]", "", some_txt)
    # remove html links
    some_txt = gsub("http\\w+", "", some_txt)
    # remove unnecessary spaces
    some_txt = gsub("[ \t]{2,}", "", some_txt)
    some_txt = gsub("^\\s+|\\s+$", "", some_txt)
    
    # define "tolower error handling" function 
    try.error = function(x)
    {
      # create missing value
      y = NA
      # tryCatch error
      try_error = tryCatch(tolower(x), error=function(e) e)
      # if not an error
      if (!inherits(try_error, "error"))
        y = tolower(x)
      # result
      return(y)
    }
    # lower case using try.error with sapply 
    some_txt = sapply(some_txt, try.error)
    
    # remove NAs in some_txt
    some_txt = some_txt[!is.na(some_txt)]
    names(some_txt) = NULL
    
    
    
    # classify emotion
    class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
    # get emotion best fit
    emotion = class_emo[,7]
    # substitute NA's by "unknown"
    emotion[is.na(emotion)] = "unknown"
    
    # classify polarity
    class_pol = classify_polarity(some_txt, algorithm="bayes")
    # get polarity best fit
    polarity = class_pol[,4]
    
    
    
    # data frame with results
    sent_df = data.frame(text=some_txt, emotion=emotion,
                         polarity=polarity, stringsAsFactors=FALSE)
    
    # sort data frame
    sent_df = within(sent_df,
                     emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
    
    # tweets_result = searchTwitter("sucks") #Here I use the searchTwitter function to extract the tweets
    # for (tweet in tweets_result){
    #   print(paste(tweet$screenName, ":", tweet$text))
    #   vector_users <- c(vector_users, as.character(tweet$screenName)); #save the user name
    #   if (grepl("I love it", tweet$text, ignore.case = TRUE) == TRUE | grepl("Wonderful", tweet$text, ignore.case = TRUE) | grepl("Awesome", tweet$text, ignore.case = TRUE)){ #if positive words match...
    #     count_positive = count_positive + 1 # Add the positive counts
    #     vector_sentiments <- c(vector_sentiments, "Positive") #Add the positive sentiment
    #     positive_text <- c(positive_text, as.character(tweet$text)) # Add the positive text
    #   } else if (grepl("Boring", tweet$text, ignore.case = TRUE) | grepl("I'm sleeping", tweet$text, ignore.case = TRUE) |grepl("suicidal", tweet$text, ignore.case = TRUE)) { # Do the same for negatives 
    #     count_negative = count_negative + 1
    #     vector_sentiments <- c(vector_sentiments, "Negative")
    #     negative_text <- c(negative_text, as.character(tweet$text))
    #   } else { #Do the same for neutrals
    #     count_neutral = count_neutral + 1
    #     print("neutral")
    #     vector_sentiments <- c(vector_sentiments, "Neutral")
    #     neutral_text <- c(neutral_text, as.character(neutral_text))
    #   }
    # }
    # df_users_sentiment <- data.frame(vector_users, vector_sentiments) 
    # emotion_count<-as.data.frame(table(sent_df['emotion']))
    
    
    
    output$tweets_table = renderDataTable({
      sent_df
    })
    
    
     output$distPlot <- renderPlot({
    #   results = data.frame(tweets = c("Positive", "Negative", "Neutral"), numbers = c(count_positive,count_negative,count_neutral))
    #   barplot(results$numbers, names = results$tweets, xlab = "Sentiment", ylab = "Counts", col = c("Green","Red","Blue"))
       emotion_count<-data.frame(table(sent_df['emotion']))
       barplot(emotion_count$Freq, names = emotion_count$Var1, xlab = "Emotion", ylab = "Counts")
     })
     output$distPlot2 <- renderPlot({
       #   results = data.frame(tweets = c("Positive", "Negative", "Neutral"), numbers = c(count_positive,count_negative,count_neutral))
       #   barplot(results$numbers, names = results$tweets, xlab = "Sentiment", ylab = "Counts", col = c("Green","Red","Blue"))
       sentiment_count<-data.frame(table(sent_df['polarity']))
       barplot(sentiment_count$Freq, names = sentiment_count$Var1, xlab = "polarity", ylab = "Counts")
     })
  })
})