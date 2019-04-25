###################
#RHC Example

#install packages
install.packages("tableone")
install.packages("Matching")

#load packages
library(tableone)
library(Matching)


# user matching
length(unique(dfplot$uid))

group_by(dfplot, crisis) %>%
  summarise(
    count = n(),
    mean = mean(as.numeric(Anger), na.rm = TRUE),
    sd = sd(as.numeric(Anger), na.rm = TRUE))

# Subset weight data before treatment
before <- dfplot %>%
  filter(crisis == 0) %>%
  dplyr::select(uid,Anger,day)

# subset weight data after treatment
after <- dfplot %>%
  filter(crisis == 1) %>%
  dplyr::select(uid,Anger,day)

# pair user posts
within_subject_diff <- inner_join(before,after,by= "uid")

within_subject_diff$within_sub_diff <- as.integer(within_subject_diff$Anger.y)- as.integer(within_subject_diff$Anger.x)

t.test(within_subject_diff$within_sub_diff)

ggplot(within_subject_diff, aes(within_sub_diff))+geom_histogram()

userposts <- within_subject_diff %>%
  group_by(uid)%>%
  summarise(number = n())

summary(userposts$number)
  
