library(XML)
library(feedeR)
library(dplyr)
library(DT)
library(rvest)
library(tm)
library(SnowballC)
library(plotrix)
library(RWeka)
library(RColorBrewer)

#Function to extract links and news feed content
extractNewsFeeds <- function(queries = list('trump','clinton')){
   
        output_df = data.frame(Query = character(),
                               Headlines = character(),
                                 Dates = character(),
                                 Links = character(),
                                 text = character()
                                 )
        for(query in queries){
                url = paste('https://news.google.com/news/feeds?q=',query,'&output=rss&num=50"',sep='')
                
                #Extract RSS Data
                news_raw <- feed.extract(url)
                news_titles = news_raw$items$title
                news_dates = news_raw$items$date
                news_links = news_raw$items$link
                
                #Build into dataframe
                df <- data.frame(Headlines = news_titles,
                                 Dates = news_dates,
                                 Links = news_links)
                #Extract news daya
                df$Links <- as.character(df$Links)
                links <- df$Links
                df$text <- "" #initialize
                #For loop to loop through each link, get document text 
                for(i in 1:length(links)){
                        linkContent <- read_html(x = links[i])
                        linkText <- linkContent %>% html_nodes("p") %>%html_text()
                        linkParagraph <- paste0(linkText,collapse = '')
                        df$text[i] <- linkParagraph
                }
                output_df = rbind(output_df,df)
                
                
        }
        return(output_df)
        
}

#Function used by TDM To Clean corpus
clean_vector <- function(vector){
        vector <- removePunctuation(vector)
        vector <- removeNumbers(vector)
        vector <- removeWords(vector,c(stopwords("en"),stopwords("SMART"),
                                       "report","learn","click","bulletin","metal","dec","download","source","download",
                                       "free","freeget","get","freeclick","trial","address","website","visitor",
                                       "visit","update","user","reportclick",
                                       "information","success","investing","copper"))
        vector <- stripWhitespace(vector)
        return(vector)
}

#Create Term-document-matrix
createTDM <- function(df){
        df$text[1] <- clean_vector(df$text[1])
        df$text[2] <- clean_vector(df$text[2])
        
        dfSource <- VectorSource(df$text)
        dfCorpus <- VCorpus(dfSource)
        
        df$text[1] <- stemDocument(as.character(df$text[1]))
        df$text[2] <- stemDocument(as.character(df$text[2]))
        
        dfSource2 <- VectorSource(df$text)
        dfCorpus2 <- Corpus(dfSource2)
        
        options(mc.cores=1)
        tokenizer <- function(x){
                NGramTokenizer(x,Weka_control(min=2,max=2))
        }
        
        tdm <- TermDocumentMatrix(dfCorpus2, control = list(tokenize=tokenizer))
        
        completed = stemCompletion(rownames(tdm), dfCorpus)
        rownames(tdm) <- names(completed)
        return(tdm)   
        
}

#Using the TDM Function
df <- extractNewsFeeds()
tdm_test <- createTDM(df)
tdm_test <- as.matrix(tdm_test)




