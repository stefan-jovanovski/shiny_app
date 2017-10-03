
library(pdftools)
library(tidytext)
library(readxl)
library(dplyr)
library(RODBC)
 
##############################
#
# SECTION 0: PDF INPUT (MANUAL)
#
##############################
 
Guazzete <- "s3.pdf"
 
##############################
#
# SECTION 1: TEXT MINNING
#
##############################
 
setwd("~/Automation - Official Guazzete")
path <- getwd()
 
pdf_file <- file.path(path, Guazzete)
test <- pdf_text(pdf_file)
 
### Two Sided
 
test = gsub("[\r\n]", " ", test)
list = strsplit(test, " {2,}") #split anywhere where there are 2 or more consecutive spaces - hopefully only between two paragraphs (if not the output wont make much sense)
 
resi = lapply(list, function(x) {
  unl = unlist(x)
  len = length(x)
  uneven = seq(from = 1, to = len , by = 2)
  even = seq(from = 2, to = len , by = 2)
  uneven = unl[uneven]
  even = unl[even]
  uneven = paste(uneven, collapse = " ")
  even = paste(even, collapse = " ") #intentionally leave a space between them, one could even use something that is not expected to occur in the document like "frafrafra" and use that in the gsub call later as gsub("(\\d)-frafrafra(\\d)", "\\1\\2", resi)
  return(cbind(uneven, even))
}) #separate even from uneven rows
 
resi = unlist(resi)
 
resi = gsub("(\\d)- (\\d)", "\\1\\2", resi) #clean numbers
resi = gsub("(\\b)- (\\b)", "\\1\\2", resi) #clean words
 
resi <- data_frame(line = 1:length(resi), text = resi) #change class/type - vector to dataframe
 
count <- resi %>%
  unnest_tokens(word, text)  %>% #split columns into word like elements (tokens)
  count(word, sort = TRUE)       #count frequency and sort in desc order
 
count$word <- gsub("[^0-9]", NA, count$word)
count$num_char <- nchar(count$word)
 
two_cols <- count %>%
            filter(!is.na(word)) %>%
            filter(n == 1) %>%
            filter(num_char == 7 | num_char == 13 | num_char == 15)
 
### One Sided
 
test <- pdf_text(pdf_file)
test = gsub("[\r\n]", "", test) #replace new line
test = gsub("(\\d)-  {0,}(\\d)", "\\1\\2", test) #replace - followed by any amount of white spaces between numbers with just the numbers.
 
test = gsub("[\r\n]", "", test)
test = gsub("(\\d)-.*?(\\d)", "\\1\\2", test)
 
test <- data_frame(line = 1:length(test), text = test) #change class/type - vector to dataframe
 
count <- test %>%
  unnest_tokens(word, text)  %>% #split columns into word like elements (tokens)
  count(word, sort = TRUE)       #count frequency and sort in desc order
 
count$word <- gsub("[^0-9]", NA, count$word)
count$num_char <- nchar(count$word)
 
one_col <- count %>%
  filter(!is.na(word)) %>%
  filter(n == 1) %>%
  filter(num_char == 7 | num_char == 13 | num_char == 15)
 
rm(list=setdiff(ls(), c("one_col", "two_cols"))) #remove all but two files
 
test <- rbind(one_col, two_cols) #combine One sided and Two sided results
test <- test[!duplicated(test),] #Remove duplicates
 
rm(one_col); rm(two_cols) #remove unnecesary dataframes
 
#Assign number identifier
test <- test %>%
  mutate(id_type =
           ifelse(num_char == 13, "tax_number",
           ifelse(num_char == 7,  "id_number",
           ifelse(num_char == 15, "account_number", "error"))))
 