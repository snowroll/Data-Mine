#!/usr/bin/env Rscript
library("XML")
library("methods")

cleanFun <- function(xmlString){
  return (gsub("<.*?>", "", xmlString))
}

GetAttr <- function(xmlString){
  tmp = gsub("Top/.*?/", "", xmlString)
  result = gsub("/.*?$", "", tmp)
  return (result)
}

file = list.files("./samples_500/")
file_num = length(file)
my.dataframe <- data.frame("title" = character(),  "year" = character(), "month" = character(), "day" = character(), "Attr"=character(), "full_text"=character())
print(my.dataframe)
for(i in 1:file_num){
  doc_name = paste("./samples_500/", file[i], sep = "")
  #print(doc_name)
  doc <- xmlParse(doc_name)
  text_node = getNodeSet(doc, "//block[@class='full_text']")  #get text
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
  text_vector = paste(text_vector, collapse = " ")
  
  title_node = getNodeSet(doc, "//title")
  title = toString.XMLNode(title_node[[1]])
  title = cleanFun(title)
  
  classifier_node = getNodeSet(doc, "//classifier")
  size = xmlSize(classifier_node)
  AttrVector <- c()  #get Attr
  for(i in 1 : size){
    classifier = cleanFun(toString.XMLNode(classifier_node[[i]]))
    if(substring(classifier, 1, 9)  == "Top/News/" || substring(classifier, 1, 13) == "Top/Features/"){
      Attr = GetAttr(classifier)
      if((Attr %in% AttrVector) == FALSE){
         AttrVector <- c(AttrVector, Attr)
      }
    }
  }
  AttrVector = paste(AttrVector, collapse = " ** ")
  
  year_node = getNodeSet(doc, "//meta[@name='publication_year']")  #find year node
  year = sapply(year_node, xmlGetAttr, "content")  #get the real year
  month_node = getNodeSet(doc, "//meta[@name='publication_month']")
  month = sapply(month_node, xmlGetAttr, "content")
  day_node = getNodeSet(doc, "//meta[@name='publication_day_of_month']")
  day = sapply(day_node, xmlGetAttr, "content")
  
  my.dataframe <- rbind(my.dataframe, data.frame( year, month, day, AttrVector, text_vector))
  #print(my.dataframe)
}
print(my.dataframe)
