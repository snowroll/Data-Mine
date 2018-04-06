#!/usr/bin/env Rscript
library("XML")
library("methods")
library("tm")
#library("qdap")  #stem
library("parallel")
library("SnowballC")

cleanFun <- function(xmlString){  #remove tag
  return (gsub("<.*?>", "", xmlString))
}

GetAttr <- function(xmlString){  #match attr
  tmp = gsub("Top/.*?/", "", xmlString)
  result = gsub("/.*?$", "", tmp)
  return (result)
}

stem_text <- function(text, language = "porter", mc.cores = 1){  #stem word function
   # stem each word in a block of text
  stem_string <- function(str, language) {
    str <- strsplit(x = str, split = " ")
    str <- wordStem(unlist(str), language = language)
    str <- paste(str, collapse = " ")
    return(str)
  }
  # stem each text block in turn
  x <- mclapply(X = text, FUN = stem_string, language, mc.cores = mc.cores)
  
  # return stemed text blocks
  return(unlist(x))
}

file = list.files("./samples_500/")
file_num = length(file)
my.dataframe <- data.frame("title" = character(),  "year" = character(), "month" = character(), "day" = character(), "Attr"=character(), "full_text"=character())
word_set <- list()

for(i in 1:file_num){
  if(i == 6)
    break
  doc_name = paste("./samples_500/", file[i], sep = "")
  doc <- xmlParse(doc_name)  #loading xml file
  text_node = getNodeSet(doc, "//block[@class='full_text']")  #get full_text
  size = xmlSize(text_node)
  text_vector <- c()
  for(i in 1 : size){
    if(size == 0){
      text_vector <- c(NA)
      break
    }
    text = cleanFun(toString.XMLNode(text_node[[i]]))
    text_vector <- c(text_vector, text)
  }

  text_vector = paste(text_vector, collapse = " ")  #merge paragraph
  text_vector = gsub('[[:punct:] ]+', ' ' ,text_vector)  #remove punct digit and tolower
  text_vector = gsub('\n', '', text_vector)
  text_vector = tolower(text_vector)  #lower word
  text_vector = gsub('[[:digit:]]+', '', text_vector)
  stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')  #remove stopwords
  stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
  text_vector = stringr::str_replace_all(text_vector, stopwords_regex, '')
  text_vector = gsub("^\\s+|\\s+$", "", text_vector)  #remove head and tail space
  text_vector = gsub("\\s+", " ", text_vector)  #remove more space 
  stem_news = stem_text(text_vector, language = 'en')  #stem the news

  word <- strsplit(stem_news, split=" ")  #split word  
  unlist_word <- unlist(word)
  word_set <- c(word_set, list(unlist_word))
  
  title_node = getNodeSet(doc, "//title")  #get title 
  title = toString.XMLNode(title_node[[1]])
  title = cleanFun(title)
  
  classifier_node = getNodeSet(doc, "//classifier")
  size = xmlSize(classifier_node)
  AttrVector <- c()  #get Attr  maybe not only one attr
  for(i in 1 : size){
    classifier = cleanFun(toString.XMLNode(classifier_node[[i]]))
    if(substring(classifier, 1, 9)  == "Top/News/" || substring(classifier, 1, 13) == "Top/Features/"){
      Attr = GetAttr(classifier)
      if((Attr %in% AttrVector) == FALSE){
         AttrVector <- c(AttrVector, Attr)
      }
    }
  }
  AttrVector = paste(AttrVector, collapse = " ")
  
  year_node = getNodeSet(doc, "//meta[@name='publication_year']")  #find year node
  year = sapply(year_node, xmlGetAttr, "content")  #get the real year
  month_node = getNodeSet(doc, "//meta[@name='publication_month']")
  month = sapply(month_node, xmlGetAttr, "content")
  day_node = getNodeSet(doc, "//meta[@name='publication_day_of_month']")
  day = sapply(day_node, xmlGetAttr, "content")
  
  my.dataframe <- rbind(my.dataframe, data.frame( year, month, day, AttrVector, stem_news))  #first question
}

unlist_set <- unlist(word_set)  #bulid Bag Of Word Vector
unique_word_set <- unique(unlist_set)  #Duplicate removal
bag_of_word <- list()
for(i in 1 : 5){
  vector_num <- c()
  single_set <- unlist(word_set[i])  #the words of signal news
  for(j in 1 : length(unique_word_set)){ 
    patter <- paste("^", unique_word_set[j], "$", sep = "");  #match unique word in signal news
    occur_num <- length(grep(patter, single_set))  #get the number of occur
    #print(occur_num)
    vector_num <- c(vector_num, occur_num)
  }
  bag_of_word <- c(bag_of_word, list(vector_num))
}
print(bag_of_word)