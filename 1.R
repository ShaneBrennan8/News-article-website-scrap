####DATA COLLECTION

###Used Packages####
library(purrr)
library(rvest)
library(dplyr)
library(rlang)
library(rvest)
library(tidyverse)
library(stringr)
library(purrr)
library(dplyr)
library(Rcrawler)
library(data.table)
library(tidytext)
library(tibble)
library(ggplot2)
library(tidyr)
library(textdata)
library(wordcloud)
library(reshape2)
library(scales)
library(xlsx)

system.time({ sleep_for_a_minute() })


####CNP (Covid News Paper Data Base) Collection ####
##Date Range: 300d
##Search Terms: corona / covid-19


###### Ireland - Irish Times #####
### Full scrap achieved
ITNP <-'https://news.google.com/rss/search?q=corona%20covid-19%20www.irishtimes.com%20when%3A300d&hl=en-IE&gl=IE&ceid=IE%3Aen'
nodes <- ITNP%>%read_html()%>%html_nodes('item')
g_ITNP <- map_df(nodes, function(item) {
  
  data.frame(title = str_squish(item%>%html_node('title') %>% html_text()),
             link = str_squish(item%>%html_node(xpath="*/following-sibling::text()") %>%
                                 html_text()),
             date = str_squish(item%>%html_node('pubdate') %>% html_text()),
             source = str_squish(item%>%html_node('source') %>% html_text()),
             stringsAsFactors=FALSE)
})

g_ITNP <-g_ITNP [!(g_ITNP $source!="The Irish Times"),]
g_ITNP$id <-seq.int(nrow(g_ITNP))

df <- NULL

urls<-g_ITNP$link
DataITNP<-ContentScraper(urls, CssPatterns=c(".genre-current"))
df<- rbindlist(DataITNP)
df$id <- seq.int(nrow(df))
DataITNP<-merge(g_ITNP, df, by=c("id"))
DataITNP<-dplyr::rename(DataITNP, c("content"="V1"))
df <- NULL




# Ireland - Journal.ie
#Able to get a full scrap
JNP <-'https://news.google.com/rss/search?q=corona%20covid-19%20www.thejournal.ie%20when%3A300d&hl=en-IE&gl=IE&ceid=IE%3Aen'
nodes <- JNP%>%read_html()%>%html_nodes('item')
g_JNP <- map_df(nodes, function(item) {
  
  data.frame(title = str_squish(item%>%html_node('title') %>% html_text()),
             link = str_squish(item%>%html_node(xpath="*/following-sibling::text()") %>%
                                 html_text()),
             date = str_squish(item%>%html_node('pubdate') %>% html_text()),
             source = str_squish(item%>%html_node('source') %>% html_text()),
             stringsAsFactors=FALSE)
})

g_JNP<-g_JNP [!(g_JNP$source!="TheJournal.ie" & g_JNP$source!="thejournal.ie"  ),]
g_JNP$id <-seq.int(nrow(g_JNP))

df <- NULL

urls<-g_JNP$link
DataJNP<-ContentScraper(urls, CssPatterns=c(".postMainLandscape"))
df<- rbindlist(DataJNP)
df$id <- seq.int(nrow(df))
DataJNP<-merge(g_JNP, df, by=c("id"))
DataJNP<-dplyr::rename(DataJNP, c("content"="V1"))
df <- NULL


######  UK #####

# UK - BBC
# Full scrape
BBCNP <-'https://news.google.com/rss/search?q=corona%20covid-19%20www.bbc.com%20when%3A300d&hl=en-IE&gl=IE&ceid=IE%3Aen'
nodes <- BBCNP%>%read_html()%>%html_nodes('item')
g_BBCNP <- map_df(nodes, function(item) {
  
  data.frame(title = str_squish(item%>%html_node('title') %>% html_text()),
             link = str_squish(item%>%html_node(xpath="*/following-sibling::text()") %>%
                                 html_text()),
             date = str_squish(item%>%html_node('pubdate') %>% html_text()),
             source = str_squish(item%>%html_node('source') %>% html_text()),
             stringsAsFactors=FALSE)
})


g_BBCNP <-g_BBCNP [!(g_BBCNP  $source!="BBC News"),]
g_BBCNP$id <-seq.int(nrow(g_BBCNP))


df <- NULL

urls<-g_BBCNP$link
DataBBCNP<-ContentScraper(urls, CssPatterns=c(".e1nh2i2l6"))
df<- rbindlist(DataBBCNP)
df$id <- seq.int(nrow(df))
DataBBCNP<-merge(g_BBCNP, df, by=c("id"))
DataBBCNP<-dplyr::rename(DataBBCNP, c("content"="V1"))
df <- NULL







# UK - The Guardian
# Full scrape
GRDNP <-'https://news.google.com/rss/search?q=corona%20covid-19%20www.theguardian.com%20when%3A300d&hl=en-IE&gl=IE&ceid=IE%3Aen'
nodes <- GRDNP%>%read_html()%>%html_nodes('item')
g_GRDNP <- map_df(nodes, function(item) {
  
  data.frame(title = str_squish(item%>%html_node('title') %>% html_text()),
             link = str_squish(item%>%html_node(xpath="*/following-sibling::text()") %>%
                                 html_text()),
             date = str_squish(item%>%html_node('pubdate') %>% html_text()),
             source = str_squish(item%>%html_node('source') %>% html_text()),
             stringsAsFactors=FALSE)
})

g_GRDNP <-g_GRDNP [!(g_GRDNP $source!="The Guardian"),]
g_GRDNP$id <-seq.int(nrow(g_GRDNP))

df <- NULL

urls<-g_GRDNP$link
DataGRDNP<-ContentScraper(urls, CssPatterns=c(".dcr-1c5ohp"))
df<- rbindlist(DataGRDNP)
df$id <- seq.int(nrow(df))
DataGRDNP<-merge(g_GRDNP, df, by=c("id"))
DataGRDNP<-dplyr::rename(DataGRDNP, c("content"="V1"))
df <- NULL



###### USA #####

#USA Today

USTNP <-'https://news.google.com/rss/search?q=corona%20covid-19%20www.usatoday.comwhen%3A300d&hl=en-IE&gl=IE&ceid=IE%3Aen'
nodes <- USTNP%>%read_html()%>%html_nodes('item')
g_USTNP<- map_df(nodes, function(item) {
  
  data.frame(title = str_squish(item%>%html_node('title') %>% html_text()),
             link = str_squish(item%>%html_node(xpath="*/following-sibling::text()") %>%
                                 html_text()),
             date = str_squish(item%>%html_node('pubdate') %>% html_text()),
             source = str_squish(item%>%html_node('source') %>% html_text()),
             stringsAsFactors=FALSE)
})

g_USTNP<-g_USTNP[!(g_USTNP$source!="USA TODAY"),]
g_USTNP$id <-seq.int(nrow(g_USTNP))


df <- NULL

urls<-g_USTNP$link
DataUSTNP<-ContentScraper(urls, CssPatterns=c(".row"))
df<- rbindlist(DataUSTNP)
df$id <- seq.int(nrow(df))
DataUSTNP<-merge(g_USTNP, df, by=c("id"))
DataUSTNP<-dplyr::rename(DataUSTNP, c("content"="V1"))
df <- NULL




# USA - LA Times
LATNP <-'https://news.google.com/rss/search?q=corona%20covid-19%20www.latimes.com%20when%3A300d&hl=en-IE&gl=IE&ceid=IE%3Aen'
nodes <- LATNP%>%read_html()%>%html_nodes('item')
g_LATNP<- map_df(nodes, function(item) {
  
  data.frame(title = str_squish(item%>%html_node('title') %>% html_text()),
             link = str_squish(item%>%html_node(xpath="*/following-sibling::text()") %>%
                                 html_text()),
             date = str_squish(item%>%html_node('pubdate') %>% html_text()),
             source = str_squish(item%>%html_node('source') %>% html_text()),
             stringsAsFactors=FALSE)
})

g_LATNP<-g_LATNP[!(g_LATNP$source!="Los Angeles Times"),]
g_LATNP$id <-seq.int(nrow(g_LATNP))


df <- NULL

urls<-g_LATNP$link
DataLATNP<-ContentScraper(urls, CssPatterns=c(".story"))
df<- rbindlist(DataLATNP)
df$id <- seq.int(nrow(df))
DataLATNP<-merge(g_LATNP, df, by=c("id"))
DataLATNP<-dplyr::rename(DataLATNP, c("content"="V1"))
df <- NULL



###### New Zealand ######

# NZ - Stuff.co,uk
STFNP <-'https://news.google.com/rss/search?q=corona%20covid-19%20%22www.stuff.co.nz%22%20when%3A30d&hl=en-IE&gl=IE&ceid=IE%3Aen'
nodes <- STFNP%>%read_html()%>%html_nodes('item')
g_STFNP<- map_df(nodes, function(item) {
  
  data.frame(title = str_squish(item%>%html_node('title') %>% html_text()),
             link = str_squish(item%>%html_node(xpath="*/following-sibling::text()") %>%
                                 html_text()),
             date = str_squish(item%>%html_node('pubdate') %>% html_text()),
             source = str_squish(item%>%html_node('source') %>% html_text()),
             stringsAsFactors=FALSE)
})

g_STFNP<-g_STFNP[!(g_STFNP$source!="Stuff.co.nz"),]
g_STFNP$id <-seq.int(nrow(g_STFNP))


df <- NULL

urls<-g_STFNP$link
DataSTFNP<-ContentScraper(urls, CssPatterns=c(".sics-component__news-page__container"))
df<- rbindlist(DataSTFNP)
df$id <- seq.int(nrow(df))
DataSTFNP<-merge(g_STFNP, df, by=c("id"))
DataSTFNP<-dplyr::rename(DataSTFNP, c("content"="V1"))
df <- NULL




# NZ - NZherald
NZHNP <-'https://news.google.com/rss/search?q=corona%20covid-19%20"www.nzherald.co.nz"%20when%3A300d&hl=en-IE&gl=IE&ceid=IE%3Aen'
nodes <- NZHNP%>%read_html()%>%html_nodes('item')
g_NZHNP<- map_df(nodes, function(item) {
  
  data.frame(title = str_squish(item%>%html_node('title') %>% html_text()),
             link = str_squish(item%>%html_node(xpath="*/following-sibling::text()") %>%
                                 html_text()),
             date = str_squish(item%>%html_node('pubdate') %>% html_text()),
             source = str_squish(item%>%html_node('source') %>% html_text()),
             stringsAsFactors=FALSE)
})

g_NZHNP<-g_NZHNP[!(g_NZHNP$source!="New Zealand Herald"),]
g_NZHNP$id <-seq.int(nrow(g_NZHNP))


df <- NULL

urls<-g_NZHNP$link
DataNZHNP<-ContentScraper(urls, CssPatterns=c(".article__body"))
df<- rbindlist(DataNZHNP)
df$id <- seq.int(nrow(df))
DataNZHNP<-merge(g_NZHNP, df, by=c("id"))
DataNZHNP<-dplyr::rename(DataNZHNP, c("content"="V1"))
df <- NULL



###### Australia ######
#Sydney Morning Herald
SMHNP <-'https://news.google.com/rss/search?q=corona%20covid-19%20www.smh.com.au%20when%3A300d&hl=en-IE&gl=IE&ceid=IE%3Aen'
nodes <- SMHNP%>%read_html()%>%html_nodes('item')
g_SMHNP<- map_df(nodes, function(item) {
  
  data.frame(title = str_squish(item%>%html_node('title') %>% html_text()),
             link = str_squish(item%>%html_node(xpath="*/following-sibling::text()") %>%
                                 html_text()),
             date = str_squish(item%>%html_node('pubdate') %>% html_text()),
             source = str_squish(item%>%html_node('source') %>% html_text()),
             stringsAsFactors=FALSE)
})

g_SMHNP<-g_SMHNP[!(g_SMHNP$source!="The Sydney Morning Herald"),]
g_SMHNP$id <-seq.int(nrow(g_SMHNP))


df <- NULL

urls<-g_SMHNP$link
DataSMHNP<-ContentScraper(urls, CssPatterns=c("._2yRSr"))
df<- rbindlist(DataSMHNP)
df$id <- seq.int(nrow(df))
DataSMHNP<-merge(g_SMHNP, df, by=c("id"))
DataSMHNP<-dplyr::rename(DataSMHNP, c("content"="V1"))
df <- NULL


# Nine
NINENP <-'https://news.google.com/rss/search?q=corona%20covid-19%20www.9news.com.au%20when%3A300d&hl=en-IE&gl=IE&ceid=IE%3Aen'
nodes <- NINENP%>%read_html()%>%html_nodes('item')
g_NINENP<- map_df(nodes, function(item) {
  
  data.frame(title = str_squish(item%>%html_node('title') %>% html_text()),
             link = str_squish(item%>%html_node(xpath="*/following-sibling::text()") %>%
                                 html_text()),
             date = str_squish(item%>%html_node('pubdate') %>% html_text()),
             source = str_squish(item%>%html_node('source') %>% html_text()),
             stringsAsFactors=FALSE)
})

g_NINENP<-g_NINENP[!(g_NINENP$source!="9News"),]
g_NINENP$id <-seq.int(nrow(g_NINENP))


df <- NULL

urls<-g_NINENP$link
DataNINENP<-ContentScraper(urls, CssPatterns=c(".article__body-croppable"))
df<- rbindlist(DataNINENP)
df$id <- seq.int(nrow(df))
DataNINENP<-merge(g_NINENP, df, by=c("id"))
DataNINENP<-dplyr::rename(DataNINENP, c("content"="V1"))
df <- NULL

###### Canada ######

#The Toronto Star
STRNP <-'https://news.google.com/rss/search?q=corona%20covid-19%20%2Fwww.thestar.com%20when%3A300d&hl=en-IE&gl=IE&ceid=IE%3Aen'
nodes <- STRNP%>%read_html()%>%html_nodes('item')
g_STRNP<- map_df(nodes, function(item) {
  
  data.frame(title = str_squish(item%>%html_node('title') %>% html_text()),
             link = str_squish(item%>%html_node(xpath="*/following-sibling::text()") %>%
                                 html_text()),
             date = str_squish(item%>%html_node('pubdate') %>% html_text()),
             source = str_squish(item%>%html_node('source') %>% html_text()),
             stringsAsFactors=FALSE)
})

g_STRNP<-g_STRNP[!(g_STRNP$source!="Toronto Star"),]
g_STRNP$id <-seq.int(nrow(g_STRNP))


df <- NULL

urls<-g_STRNP$link
DataSTRNP<-ContentScraper(urls, CssPatterns=c(".c-article-body__content"))
df<- rbindlist(DataSTRNP)
df$id <- seq.int(nrow(df))
DataSTRNP<-merge(g_STRNP, df, by=c("id"))
DataSTRNP<-dplyr::rename(DataSTRNP, c("content"="V1"))
df <- NULL




# National Post

NPNP <-'https://news.google.com/rss/search?q=corona%20covid-19%20www.nationalpost.com%20when%3A300d&hl=en-IE&gl=IE&ceid=IE%3Aen'
nodes <- NPNP%>%read_html()%>%html_nodes('item')
g_NPNP<- map_df(nodes, function(item) {
  
  data.frame(title = str_squish(item%>%html_node('title') %>% html_text()),
             link = str_squish(item%>%html_node(xpath="*/following-sibling::text()") %>%
                                 html_text()),
             date = str_squish(item%>%html_node('pubdate') %>% html_text()),
             source = str_squish(item%>%html_node('source') %>% html_text()),
             stringsAsFactors=FALSE)
})

g_NPNP<-g_NPNP[!(g_NPNP$source!="National Post"),]
g_NPNP$id <-seq.int(nrow(g_NPNP))


df <- NULL

urls<-g_NPNP$link
DataNPNP<-ContentScraper(urls, CssPatterns=c(".article-content-story--story"))
df<- rbindlist(DataNPNP)
df$id <- seq.int(nrow(df))
DataNPNP<-merge(g_NPNP, df, by=c("id"))
DataNPNP<-dplyr::rename(DataNPNP, c("content"="V1"))
df <- NULL






###### India #####

#The Indian Express

TIENP <-'https://news.google.com/rss/search?q=corona%20covid-19%20www.indianexpress.com%20when%3A300d&hl=en-IE&gl=IE&ceid=IE%3Aen'
nodes <- TIENP%>%read_html()%>%html_nodes('item')
g_TIENP<- map_df(nodes, function(item) {
  
  data.frame(title = str_squish(item%>%html_node('title') %>% html_text()),
             link = str_squish(item%>%html_node(xpath="*/following-sibling::text()") %>%
                                 html_text()),
             date = str_squish(item%>%html_node('pubdate') %>% html_text()),
             source = str_squish(item%>%html_node('source') %>% html_text()),
             stringsAsFactors=FALSE)
})

g_TIENP<-g_TIENP[!(g_TIENP$source!="The Indian Express"),]
g_TIENP$id <-seq.int(nrow(g_TIENP))


df <- NULL

urls<-g_TIENP$link
DataTIENP<-ContentScraper(urls, CssPatterns=c(".leftpanel"))
df<- rbindlist(DataTIENP)
df$id <- seq.int(nrow(df))
DataTIENP<-merge(g_TIENP, df, by=c("id"))
DataTIENP<-dplyr::rename(DataTIENP, c("content"="V1"))
df <- NULL


#The Hindu English

THENP <-'https://news.google.com/rss/search?q=corona%20covid-19%20www.thehindu.com%20when%3A300d&hl=en-IE&gl=IE&ceid=IE%3Aen'
nodes <- THENP%>%read_html()%>%html_nodes('item')
g_THENP<- map_df(nodes, function(item) {
  
  data.frame(title = str_squish(item%>%html_node('title') %>% html_text()),
             link = str_squish(item%>%html_node(xpath="*/following-sibling::text()") %>%
                                 html_text()),
             date = str_squish(item%>%html_node('pubdate') %>% html_text()),
             source = str_squish(item%>%html_node('source') %>% html_text()),
             stringsAsFactors=FALSE)
})

g_THENP<-g_THENP[!(g_THENP$source!="The Hindu"),]
g_THENP$id <-seq.int(nrow(g_THENP))


df <- NULL

urls<-g_THENP$link
DataTHENP<-ContentScraper(urls, CssPatterns=c(".col-xs-12"))
df<- rbindlist(DataTHENP)
df$id <- seq.int(nrow(df))
DataTHENP<-merge(g_THENP, df, by=c("id"))
DataTHENP<-dplyr::rename(DataTHENP, c("content"="V1"))
df <- NULL



#### News paper Data Base Cleaning####

#### My newspaper Data Base Cleaning####

# Bind all my corpus together
GN <- rbind(DataBBCNP,DataGRDNP,DataITNP,DataJNP,DataLATNP,DataNINENP,DataNPNP,DataNZHNP,DataSMHNP,DataSTFNP,DataSTRNP,DataTHENP,DataTIENP,DataUSTNP)
# Set a local time
Sys.setlocale("LC_TIME", "C")
GN$realDate <- as.Date(substr(GN$date, 6, 16), tryFormats = c("%d %b %Y"))
#GN<-GN[complete.cases(GN),]
# Remove duplicate titles and article bodies
GN<-GN[!duplicated(GN$title,),]
GN<-GN[!duplicated(substr(GN$title, 1, 40)),]
GN<-GN[!duplicated(substr(GN$content, 1, 50)),]
# Remove blanks
GN<-GN[!(GN$content==""),]

# Text Cleaning The corpus
Clean_String <- function(string){
  # Turn everything lowercase
  temp <- tolower(string)
  # Remove everything that is not a number or letter . 
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  # Split it
  temp <- stringr::str_split(temp, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  return(temp)
}

allWords <- function(data){
  sent <- data.frame()
  idx <- (1:nrow(data))
  for (i in idx) {
    text <- data$content[i]
    tidytext <- Clean_String(text)
    text2 <- as.data.frame(tidytext)
    text_df <- tibble(line=1:nrow(text2), text=text2)
    text_df <- mutate(text_df, text=text2$tidytext)
    text_df %>% unnest_tokens(word, text)
    text_df <- dplyr::rename(text_df, c("word"="text"))
    text_df$source <- data$source[i]
    text_df$date <- data$realDate[i]
    text_df$id <- data$id[i]
    sent <- rbind(sent, text_df)
  }
  return(sent)
}
 

allTitles <- function(data){
  sent <- data.frame()
  idx <- (1:nrow(data))
  for (i in idx) {
    text <- data$title[i]
    tidytext <- Clean_String(text) 
    text2 <- as.data.frame(tidytext)
    text_df <- tibble(line=1:nrow(text2), text=text2)
    text_df <- mutate(text_df, text=text2$tidytext)
    text_df %>% unnest_tokens(word, text)
    text_df <- dplyr::rename(text_df, c("word"="text"))
    text_df$source <- data$source[i]
    text_df$date <- data$realDate[i]
    text_df$id <- data$id[i]
    sent <- rbind(sent, text_df)
  }
  return(sent)
}


#### make lists/tibbles of words & take only pandemic era relevant articles  ####

GNWords <- allWords(GN)
GNTitles <- allTitles(GN)

##### Split word corpus into dates based on world vaccination administration progress

GNFirst <- subset(GN, GN$realDate < as.Date("2021-05-31"))
GNSecond <- subset(GN, GN$realDate >= as.Date("2021-06-01") & GN$realDate <= as.Date("2021-08-31") )
GNThird <- subset(GN, GN$realDate >= as.Date("2021-06-01"))

GNFirstWords <- allWords(GNFirst)
GNSecondWords <- allWords(GNSecond)
GNThirdWords <- allWords(GNThird)

##### Data Summary and topline work overview ####
summary(GN)

GN %>% group_by(realDate) %>% count()  %>% ggplot() + geom_bar(aes(realDate,n), stat ='identity')


#save(list = c("GN",   "GNWords", "GNTitles"), file="C:\Users\shane\OneDrive\Desktop\H Dip Data Analytics - NCI\Project\Project work\My own r code for project")

# Saved as
write.xlsx(GN,'GNdataAll.xlsx')



