library(stringr)

setwd("D:/code/Parser/Parser_for_Biodiversity_Checklists_Formal")

getSciname <- function(x){
  sciName_index <- grep("^[0-9]", x[, 1])
  sciName <- x[sciName_index,]
  sciName <- gsub("^[0-9]+. ", "", sciName)
  return(sciName)
}

getDis <- function(x,length){
  Distribution = c()
  n = 0
  cur_dis = ""
  for (i in 1:length) {
    if (str_detect(x[i, 1], "Distribution:") &&
        (str_detect(x[i, 1], "[*. ]$"))) {
      n = n + 1
      cur_dis = str_split(x[i, 1], "Distribution: ")[[1]][2]
    }
    Distribution[n] = cur_dis
    if ((str_detect(x[i, 1], "Distribution:")) &&
        (str_detect(x[i, 1], "[*. ]$") == FALSE)) {
      n = n + 1
      dstr_line = ""
      dstr_line = x[i, 1]
      if (i < length - 1) {
        j = i + 1
      }
      while (j < length - 1) {
        if (grepl("^[0-9]", x[j + 1, 1]) |
            grepl("^[[:alpha:]]*$", x[j + 1, 1])) {
          dstr_line <- paste(dstr_line, x[j, 1])
          break
        } else{
          dstr_line <- paste(dstr_line, x[j, 1])
          j = j + 1
        }
      }
      cur_dis <- str_split(dstr_line, "Distribution: ")[[1]][2]
      dstr_line <- ""
    }
    Distribution[n] <- cur_dis
  }
  return(Distribution)
}

getFamily <- function(x,sciName){
  family_index = grep("^[[:alpha:]]*$", x[, 1])
  family_name = x[family_index,]
  total_num = length(sciName)
  cur_index = c()
  cur_num = c()
  for (i in 1:length(family_index)) {
    cur_index[i] <-
      (str_match(as.character(x[family_index[i] + 1, 1]), "^[0-9]+. "))
    cur_index[i] <- str_split(cur_index[i], "[\\.]")[[1]][1]
    cur_num[i] <- as.integer(cur_index[i])
  }
  cur_num <- c(cur_num, total_num + 1)
  Family <- rep(family_name, diff(cur_num))
  return(Family)
}

parse_taxolist <- function(input,type,sep,output){if (type == "txt") {file = read.csv(input, sep = sep , header = FALSE)}
  if (type == "csv") {file = read.csv(input, sep = sep , header = FALSE)}
  
  length = length(file[, 1])
  sciName = getSciname(file)
  Distribution = getDis(file,length)
  Family = getFamily(file,sciName)
  full_scientific_name = sciName
  genus = str_extract_all(full_scientific_name,"^[[:blank:]]?[A-z]+")
  rest1 = gsub("^[[:blank:]]?[A-z]+","",full_scientific_name)
  author = str_extract_all(rest1,"[A-Z]{1}.?\\s?&?\\s?[A-z]+.?(.*?)?,")
  year = str_extract_all(full_scientific_name,"\\s?[0-9]+")
  
  table = cbind(as.data.frame(Family),as.data.frame(Distribution))
  table$genus = 1
  table$author = 1
  table$year = 1
  for (i in 1:nrow(table)){
    table[i,3] = genus[i]
    table[i,4] = author[i]
    table[i,5] = year[i]
  }
  write.csv(table, file = output, row.names = F)
  return(table)
}

b = parse_taxolist("taxo01.txt","txt","\t","taxo_out01.csv")
c = parse_taxolist("testest.csv","csv","\t","taxo_out02.csv")
