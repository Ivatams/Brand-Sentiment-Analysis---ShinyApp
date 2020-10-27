# Creating Server functions
################################################################################
#SERVER.R
# Server is a function used to render the objects created in the User Interface function of the shiny Application
# It takes the input and output as an argument
################################################################################

server <- function(input, output) {# Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
 
  # Set-up twitter authentication key
  
  data1 = eventReactive(input$goButton, {
    
    # Conditional data extraction based on user input, if the InputID selected by user = username
    if (input$typeInput == "username") 
    {
      #tweetOutput <- userTL(user.name = input$usernameInput,number.of.tweets = input$numberInput)
      
      tweetOutput <- searchTwitter(input$companyInput, input$numberInput, lang="en", since="2014-08-20")
    }
    
    #Cleans the tweet
    df.tweets = cleanTweets(tweetOutput)
    #Get sentiments
    nrc.lexicons = get_nrc_sentiment(df.tweets$text_clean)
  })
  
  # ## Rendering TM plots ##
  # renderPlot - Renders a reactive plot that is suitable for assigning to an output slot.
  # In this case the the objects used are plot1 and plot2
  output$plot1 = renderPlot({
    
    # ## creating Barplot for emotions ##
    barplot(
      #Sort  - (or order) a vector or factor (partially) into ascending or descending order
      #prop.table - Express Table Entries as Fraction of Marginal Table
      sort(colSums(prop.table(data1()[, 1:8]))), 
      horiz = TRUE, 
      cex.names = 0.8, 
      las = 1, 
      main = "Emotions in tweets", xlab="Percentage", xlim = c(0,.4))}, 
    width = 700, height = 500)
  
  output$plot2 = renderPlot({
    
    # ## Creating barplot for positive vs negative ##
    barplot(
      sort(colSums(prop.table(data1()[, 9:10]))), 
      horiz = TRUE, 
      cex.names = 0.75, 
      las = 1, 
      main = "Ratio of positive to negative tweets",xlab="Percentage", xlim = c(0,1))},
    width = 700, height = 500)
  
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data2 = eventReactive(input$goButton, {
    
    if (input$typeInput == "username") 
    {
      searchQuery = input$companyInput;
      
      
      if(grepl(',',input$problemInput,fixed=TRUE) == "TRUE") {
        
        problemList <- strsplit(input$problemInput,",")[[1]]
        
        for (problem in problemList) {
          searchQuery <- paste(searchQuery, problem, sep = " AND ")
        }
        
      } else {
        searchQuery <- paste(searchQuery, input$problemInput, sep = " AND ")
      }
      
      if(input$latInput == "") {
        
        tweetOutput <- searchTwitter(searchQuery, input$numberInput, lang="en")  
      } else {
        geoString = input$latInput
        
        geoString <- paste(geoString, input$longInput, sep = ",")
        geoString <- paste(geoString, input$rediuslemInput, sep = ",")
        geoString <- paste(geoString, "mi")
        
        tweetOutput <- searchTwitter(searchQuery, input$numberInput, lang="en", geocode = geoString)
      }
    }
    
    searchtweet.clean = cleanTweets(tweetOutput)
    
    searchtweet.tdm.tfidf = tdm.TFIDF(searchtweet.clean)
    
    nrc.lex = getSentiments.TF_IDF.nrc(searchtweet.tdm.tfidf)
    
  })
  
  # ## Creating a Render plots for TFIDF ##
  # renderPlot - Renders a reactive plot that is suitable for assigning to an output slot.
  # In this case the the objects used are plot3 and plot4
  output$plot3 = renderPlot({
    barplot(
      sort(colSums(prop.table(data2()[, 1:8]))), 
      horiz = TRUE, 
      cex.names = 0.75, 
      las = 1, 
      main = "Emotions in tweets", xlab="Percentage",xlim = c(0,.4))}, width = 700, height = 500)
  
  output$plot4 = renderPlot({
    barplot(
      sort(colSums(prop.table(data2()[, 9:10]))), 
      horiz = TRUE, 
      cex.names = 0.8,
      las = 1, 
      main = "Polarity in tweets", xlab="Percentage", xlim = c(0,1))}, width = 700, height = 500)
  
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data3 = eventReactive(input$goButton, {
    
    
    if (input$typeInput == "username") 
    {
      searchQuery = input$companyInput;
      
      
      if(grepl(',',input$problemInput,fixed=TRUE) == "TRUE") {
        
        problemList <- strsplit(input$problemInput,",")[[1]]
        
        for (problem in problemList) {
          searchQuery <- paste(searchQuery, problem, sep = " AND ")
        }
        
      } else {
        searchQuery <- paste(searchQuery, input$problemInput, sep = " AND ")
      }
      
      if(input$latInput == "") {
        
        tweetOutput <- searchTwitter(searchQuery, input$numberInput, lang="en")  
      } else {
        geoString = input$latInput
        
        geoString <- paste(geoString, input$longInput, sep = ",")
        geoString <- paste(geoString, input$rediuslemInput, sep = ",")
        geoString <- paste(geoString, "mi")
        
        tweetOutput <- searchTwitter(searchQuery, input$numberInput, lang="en", geocode = geoString)
      }
    }
    
    df.tweets <- cleanTweets(tweetOutput)
    
    
    searchtweet.tdm.tm.stopword = tdm.tmStopWord(df.tweets)
    
    tweets.positive = generateWordCloud.positive.tmStopWords(searchtweet.tdm.tm.stopword)
    
  })
  
  # ## Render Positive Wordcloud TM ##
  # renderWordcloud2 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  # In this case the the object used is wordCloud1
  output$wordCloud1 = renderWordcloud2({wordcloud2(data = data3())})
  
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data4 = eventReactive(input$goButton, {
    
    if (input$typeInput == "username") 
    {
      searchQuery = input$companyInput;
      
      
      if(grepl(',',input$problemInput,fixed=TRUE) == "TRUE") {
        
        problemList <- strsplit(input$problemInput,",")[[1]]
        
        for (problem in problemList) {
          searchQuery <- paste(searchQuery, problem, sep = " AND ")
        }
        
      } else {
        searchQuery <- paste(searchQuery, input$problemInput, sep = " AND ")
      }
      
      if(input$latInput == "") {
        
        tweetOutput <- searchTwitter(searchQuery, input$numberInput, lang="en")  
      } else {
        geoString = input$latInput
        
        geoString <- paste(geoString, input$longInput, sep = ",")
        geoString <- paste(geoString, input$rediuslemInput, sep = ",")
        geoString <- paste(geoString, "mi")
        
        tweetOutput <- searchTwitter(searchQuery, input$numberInput, lang="en", geocode = geoString)
      }
    }
    
    df.tweets = cleanTweets(tweetOutput)
    
    searchtweet.tdm.tm.stopword = tdm.tmStopWord(df.tweets)
    
    tweets.negative = generateWordCloud.negative.tmStopWords(searchtweet.tdm.tm.stopword)
    
  })
  
  # ## Render negative wordcloud TM ##
  # renderWordcloud2 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  # In this case the the object used is wordCloud2
  output$wordCloud2 = renderWordcloud2({wordcloud2(data = data4())})
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data5 = eventReactive(input$goButton, {
    
    if (input$typeInput == "username") 
    {
      searchQuery = input$companyInput;
      
      
      if(grepl(',',input$problemInput,fixed=TRUE) == "TRUE") {
        
        problemList <- strsplit(input$problemInput,",")[[1]]
        
        for (problem in problemList) {
          searchQuery <- paste(searchQuery, problem, sep = " AND ")
        }
        
      } else {
        searchQuery <- paste(searchQuery, input$problemInput, sep = " AND ")
      }
      
      if(input$latInput == "") {
        
        tweetOutput <- searchTwitter(searchQuery, input$numberInput, lang="en")  
      } else {
        geoString = input$latInput
        
        geoString <- paste(geoString, input$longInput, sep = ",")
        geoString <- paste(geoString, input$rediuslemInput, sep = ",")
        geoString <- paste(geoString, "mi")
        
        tweetOutput <- searchTwitter(searchQuery, input$numberInput, lang="en", geocode = geoString)
      }
    }
    
    df.tweets = cleanTweets(tweetOutput)
    
    tdm.tfidf = tdm.TFIDF(df.tweets)
    
    tdm.tm.nostop = tdm.tm(df.tweets)
    
    tweets.positive = generateWordCloud.positive.TF_IDF(tdm.tfidf, tdm.tm.nostop)
    
  })
  
  # ##Rendering positive wordcloud for TFIDF ## #
  # renderWordcloud2 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  # In this case the the object used is wordCloud3
  output$wordCloud3 = renderWordcloud2({wordcloud2(data = data5())})
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data6 = eventReactive(input$goButton, {
    
    if (input$typeInput == "username") 
    {
      searchQuery = input$companyInput;
      
      
      if(grepl(',',input$problemInput,fixed=TRUE) == "TRUE") {
        
        problemList <- strsplit(input$problemInput,",")[[1]]
        
        for (problem in problemList) {
          searchQuery <- paste(searchQuery, problem, sep = " AND ")
        }
        
      } else {
        searchQuery <- paste(searchQuery, input$problemInput, sep = " AND ")
      }
      
      if(input$latInput == "") {
        
        tweetOutput <- searchTwitter(searchQuery, input$numberInput, lang="en")  
      } else {
        geoString = input$latInput
        
        geoString <- paste(geoString, input$longInput, sep = ",")
        geoString <- paste(geoString, input$rediuslemInput, sep = ",")
        geoString <- paste(geoString, "mi")
        
        tweetOutput <- searchTwitter(searchQuery, input$numberInput, lang="en", geocode = geoString)
      }
    }
    
    df.tweets = cleanTweets(tweetOutput)
    
    tdm.tfidf = tdm.TFIDF(df.tweets)
    
    tdm.tm.nostop = tdm.tm(df.tweets)
    
    tweets.negative = generateWordCloud.negative.TF_IDF(tdm.tfidf, tdm.tm.nostop)
    
    tweets.negative
  })
  
  # ##Render negative wordcloud TFIDF## #
  # renderWordcloud2 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  # In this case the the object used is wordCloud4
  output$wordCloud4 = renderWordcloud2({wordcloud2(data = data6())})
  
  
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function 
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data7 = eventReactive(input$goButton, {
    
    
    if (input$typeInput == "username") 
    {
      searchQuery = input$companyInput;
      
      
      if(grepl(',',input$problemInput,fixed=TRUE) == "TRUE") {
        
        problemList <- strsplit(input$problemInput,",")[[1]]
        
        for (problem in problemList) {
          searchQuery <- paste(searchQuery, problem, sep = " AND ")
        }
        
      } else {
        searchQuery <- paste(searchQuery, input$problemInput, sep = " AND ")
      }
      
      if(input$latInput == "") {
        
        tweetOutput <- searchTwitter(searchQuery, input$numberInput, lang="en")  
      } else {
        geoString = input$latInput
        
        geoString <- paste(geoString, input$longInput, sep = ",")
        geoString <- paste(geoString, input$rediuslemInput, sep = ",")
        geoString <- paste(geoString, "mi")
        
        tweetOutput <- searchTwitter(searchQuery, input$numberInput, lang="en", geocode = geoString)
      }
    }
    #Converting the Tweets into data frame
    #df.tweets = twListToDF(tweetOutput)
    
    df.tweets = cleanTweets(tweetOutput)
    
    
    #only displaying Text, Created, Screen Name, RT count, and Location
    
    # Remove all nongraphical characters
    text = str_replace_all(df.tweets$text,"[^[:graph:]]", " ")
    df.tweets = cbind(text, df.tweets[c(5,11,3,12,17)])
    
    #Changing column names
    colnames(df.tweets) = c("Tweets", "Date", "Username", "Fav Count", "RT Count", "Location")
    
    tweetOutput = df.tweets
    #grep('/'+input$companyInput+'/', ignore.case = FALSE, rownames(tweetOutput));
    #tweetOutput <- tweetOutput[grep('/'+input$companyInput+'/', rownames(tweetOutput)), ]
  })
  
  # ##Render tweets## #
  # renderDataTable - Renders a reactive data table that is suitable for assigning to an output slot.
  # In this case the the object used is tweetTable
  output$tweetTable = renderDataTable({data7()}, options = list(lengthMenu = c(10, 30, 50), pageLength = 5))
  
}
