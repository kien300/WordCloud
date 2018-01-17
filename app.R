library(tm)
library(wordcloud)
library(memoise)

# Using "memoise" to automatically cache the results
#test email correction
getTermMatrix <- memoise(function(book) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  # if (!(book %in% books))
  #   stop("Unknown book")
  
  text <- readLines(book)
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

terms <- getTermMatrix('Abstract_Intro.txt')

# a <- readLines(sprintf('Abstract_Intro.txt'))
# b <- readLines('Abstract_Intro.txt')
# c <- VectorSource(a)
# d <- Corpus(c)
# e <- tm_map(d, content_transformer(tolower))


#define UI----
ui <- fluidPage(
  titlePanel("CIAT peer reviewed articles-2017"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput('freq', 'Minimum Frequency',
                  min = 1, max = 50, value = 15),
      sliderInput('max', 'Maximum Number of Words',
                  min = 1, max = 200, value = 50)
    ),
    
    mainPanel(
      plotOutput('plot')
    )
  )
)

#Define server function----
server <- function(input, output){
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)