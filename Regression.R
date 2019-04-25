library(tidyverse)
library(margins)
library(mfx)
library(stargazer)
library(PairedData)
library(bife)
library(margins)
#


filtered_df <- All_coded_423 %>%
  filter(News ==0, Irrelevant ==0)

filtered_df 

hist(filtered_df$day, breaks = "days")

exclude_protest <- filtered_df %>%
  filter(day < as.Date("2012-8-15") | day > as.Date("2012-09-20"))

summer_of_protest <- filtered_df %>%
  filter(day >= as.Date("2012-8-15") & day <= as.Date("2012-09-20"))

hist(exclude_protest$day, breaks = "days")

exclde_anger <- filtered_df %>%
  filter(Anger == 0)

hist(exclde_anger$day, breaks = "days")

SCS_only <- filtered_df %>%
  filter(SCS ==1)

Exclude_SCS_Standoff <- filtered_df %>%
  filter(day < as.Date("2012-04-08") | day > as.Date("2012-06-25"))

Exclude_crisis <- filtered_df %>%
  filter(day < as.Date("2012-04-08") | day > as.Date("2012-06-25") & day < as.Date("2012-8-15") | day > as.Date("2012-09-20") )

News_only <- All_coded_423 %>%
  filter(News ==1)

hist(Exclude_crisis$day, breaks = "days")


m1 <- glm(formula = Hawk~ Anger, data = filtered_df, family = "binomial") 
m2 <- glm(formula = Hawk~ Anger + HC_SCS + HC_ECS + Information + Power + ECS, data = filtered_df, family = "binomial") #main
m4 <- glm(formula = Hawk~ Anger + HC_SCS + HC_ECS + Information + Anger_cue + day + Power + ECS, data = filtered_df, family = "binomial") 
m5 <- glm(formula = Hawk~ Anger + HC_SCS + HC_ECS + Information + Anger_cue  + day + Power + ECS, data = exclude_protest, family = "binomial")
m6 <- glm(formula = Hawk~ Anger + HC_SCS + HC_ECS + Information + Anger_cue  + day + Power + ECS, data = Exclude_crisis, family = "binomial")

 



stargazer(m1, m2, m4, m5, m6, out = "Logit.html",
          title            = "Associations With Hawkish Posts",
          dep.var.labels   = "Hawkishness (Advocating the use of force)",
          covariate.labels = c("Anger", "Hawkish Cue (SCS)", "Hawkish Cue (ECS)",
                               "High Information", "Anger Cue", "Day",
                               "Confidence in National Power", "Post on the East China Sea Dispute"))

fe <- bife(Hawk ~ Anger + HC_SCS + HC_ECS + Information  + day + Power |as.factor(uid), data = filtered_df, bias_corr = "ana")


m_a1<- glm(formula = Anger ~  HC_SCS + HC_ECS + DC_SCS + DC_ECS  + Anger_cue + day  + ECS, data = filtered_df, family = "binomial") 
m_a2<- glm(formula = Anger ~  crisis + HC_SCS + HC_ECS + DC_SCS + DC_ECS  + Anger_cue + day  + ECS, data = dfplot, family = "binomial") 

stargazer(m_a1, m_a2, out = "Robust.html")

#margianl effect
margins(m2)

#Conditional predicted value and average marginal effect plots for
cplot(m2, what = "effect")

cplot(m2, dx="HC_SCS" ,what = "effect")
cplot(m2, dx="HC_ECS" ,what = "effect")
cplot(m2, dx="Power" ,what = "effect")
cplot(m2, dx="Information" ,what = "effect")


################################################################################################
  #                                        otther models
################################################################################################

# reverse causation models
m9 <- glm(formula = HC_SCS ~ Anger + Hawk + Protest, data = SCS_only, family = "binomial") 
m10 <- glm(formula = DC_SCS ~ Anger + Hawk + Protest, data = SCS_only, family = "binomial") 
m11 <- glm(formula = HC_ECS ~ Anger + Hawk + Protest, data = summer_of_protest, family = "binomial") 
m12 <- glm(formula = DC_ECS ~ Anger + Hawk + Protest, data = summer_of_protest, family = "binomial") 

stargazer(m9, m10, m11, m12, out = "Reverse.html")


#information 
m13 <- glm(formula = Information ~ Anger+ Hawk + Power + day, data = filtered_df, family = "binomial") 
stargazer(m13, out = "Information.html")


#news only 
m14 <- glm(formula = Hawk ~  HC_SCS + HC_ECS + DC_SCS + DC_ECS  + Anger_cue + day  + ECS, data = News_only, family = "binomial") 

stargazer(m14, out = "News.html")


values <- filtered_df %>% 
  expand(Information=as.factor(0), Anger, HC_SCS, ECS = 0, Power = as.factor(0), day)

predicted <- augment(m3,  
                     type.predict = "response",
                     newdata = values) 

predicted %>% 
  ggplot() + 
  aes(x = Anger, y = .fitted, color = Anger) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("HC_SCS", ncol = 1) +
  labs(y = "Probability", x = "", title = "Facet by Hawkish Cue SCS") + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +scale_color_manual(values=c("#377eb8","#e41a1c"))

values_ECS <- filtered_df %>% 
  expand(Information=as.factor(0), Anger, HC_SCS, ECS = 1, Power = as.factor(0), day)

predicted_ECS <- augment(m3,  
                     type.predict = "response",
                     newdata = values_ECS) 


predicted_ECS %>% 
  ggplot() + 
  aes(x = Anger, y = .fitted, color = Anger) + 
  geom_pointrange(aes(ymin = .fitted - 1.96*.se.fit,
                      ymax = .fitted + 1.96*.se.fit) )  + 
  coord_flip() +
  facet_wrap("HC_SCS", ncol = 1) +
  labs(y = "Probability", x = "",title= "Facet by Hawkish Cue ECS" ) + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +scale_color_manual(values=c("#377eb8","#e41a1c"))



