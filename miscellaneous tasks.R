

library(easypackages)
libraries("tidyverse","jsonlite","here", "purrr", "magrittr","gtools")

install.packages("stargazer")



library(jsonlite)

relevant_ex_17 %>% 
  toJSON() %>%
  write_lines("relevant_ex_17.json")

json_data_raw<-fromJSON("mydata.txt")


#convert json into dataframe.
json_file <- lapply(json_data_raw, function(x) {)

output <-- do.call("rbind", json_file)
write.csv(a, file="json.csv",row.names = FALSE)
file.show("json.csv")




#how to do a random sample from 52 datasets



#how to filter rows from 52 datasets.




#key word lists
SCS_keywords <- c("黄岩岛", "斯卡伯勒浅滩", "南海", "南沙", "中沙群岛")
ECS_keywords <- c("钓鱼岛", "尖阁诸岛", "保钓")
Both_keywords<- c("黄岩岛", "斯卡伯勒浅滩", "南海", "南沙", "中沙群岛", "钓鱼岛", "尖阁诸岛", "保钓")

Both_keywords %<>%
  paste(collapse = "|")

#write  a keyword extraction function

extract_relevant <- function(df) {
  df %>%
    filter(str_detect(text, Both_keywords))
}


#forloop for rule-based data filtering and merging
datafiles <- list.files(here("Data"))

datafiles <- mixedsort(sort(datafiles))

Week1 <- read_csv(here("Data/week1.csv"),guess_max = min(100000, Inf))

relevant <- extract_relevant(Week1)

i=1

for (file in datafiles[2:52]) {
  dfn <- read_csv(here("Data",file), guess_max = min(100000, Inf))
  extracted <- extract_relevant(dfn)
  relevant <- full_join(relevant,extracted)
  i=i+1
  print(i)
}

relevantposts <- relevant

head(relevantposts$text, 100)
