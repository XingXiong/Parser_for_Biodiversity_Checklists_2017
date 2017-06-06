library(stringr)

# Situation 1:author name starts with a special symbol

a <- readLines("author example1.txt")
author <- str_extract_all(a[1],"\\([A-Z]{1}.*[A-z]+[[:blank:]]*[A-z]*[[:blank:]]*[A-z]*[[:blank:]]*[A-z]*[[:blank:]]*[A-z]*[[:blank:]]*[A-z]*[[:blank:]]*[A-z]*[[:blank:]]*[A-z]*[[:blank:]]*[A-z]*,*[:blank:]*[0-9]*[[:blank:]]*;*[A-z]*[[:blank:]]*[A-z]*[[:blank:]]*[A-z]*[[:blank:]]*[A-z]*[[:blank:]]*[A-z]*[[:blank:]]*[A-z]*[[:blank:]]*[A-z]*[[:blank:]]*[A-z]*[[:blank:]]*[A-z]*,*[:blank:]*[0-9]*")
author
year <- str_extract_all(author, "[0-9]*")
author <- gsub("[0-9]*","",author)
author <- gsub("\\("," ",author)


# Situation 2:get author name in binomial nomenclature structure
b <- readLines("author example2.txt")
author <- str_extract_all(b[1],"[[:blank:]]+[A-Z]{1}[A-z]*[[:blank:]]*[A-z]*[[:blank:]]*[A-z]*[[:blank:]]*[A-z]*.*;{1}")
