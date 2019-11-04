# Alexander Agadjanian and G. Elliott Morris
# 
# last update: 11/21/17

# Notes:
# november update to cces bot -- expanding to use of 280 characters
# example:
# "I'm a 21 year old white man. I'm a very conservative Republican who supports concealed carry, 
# never supports abortion, and support immigration. I voted for Jill Stein"
#
# using: age, race, gender, ideology, party, 3-4 policies, vote choice (options/didn't vote)
#
#

rm(list = ls())
library(matrixStats) 
library(ggrepel)
library(maps)
library(gridExtra)
library(utils)
library(geofacet)
library(pollstR)
library(lubridate)
library(ggthemes)
library(MASS)
library(haven)
library(foreign)
library(stringr)
library(broom)
library(stargazer)
library(survey)
library(tidyverse)
options(stringsAsFactors = FALSE)

# reading in data ----

# set your working directory here
#setwd("/Users/Victor/Desktop/Blog-data/cces 2016/")
cces <- read.dta("CCES16_Common_OUTPUT_Feb2018_VV.dta")

# selecting variables 
df <- cces %>% 
  select(V101, inputstate_post, commonweight, commonweight_post,commonweight_vv_post, pid7, ideo5, # party id and ideo from pre
         gender, race, hispanic, birthyr, educ, CL_E2016GVM, CC16_410a,
         CC16_330e, CC16_331_7, CC16_332a, CC16_333d, CC16_334a,
         CC16_351I, CC16_351K) #%>% # CC16_337_1, CC16_337_2, CC16_337_3, 
# removing not sure/skipped/not asked in vote choice, but keeping those we know didn't vote -- see/uncomment line 57
# deleting more observations than it should -- why?
# run table(cces$CC16_410a, cces$CL_E2016GVM) and table(df$novoterecorded)
# mutate(novoterecorded = ifelse(CC16_410a=="I'm not sure" & CL_E2016GVM!="", 1,
#                                ifelse(CC16_410a=="Skipped" & CL_E2016GVM!="", 1,
#                                       ifelse(CC16_410a=="Not Asked" & CL_E2016GVM!="", 1, 
#                                              ifelse(is.na(CC16_410a) & CL_E2016GVM!="", 1, 0)))))


unique(df$CC16_410a)
unique(df$CL_E2016GVM)
table(df$CC16_410a, df$CL_E2016GVM)

# filtering -- maybe do this after new variables are created? - uncommented for now 
# df <- df %>% 
#   filter(pid7 != "Skipped", pid7 != "Not Asked", pid7 != "Not sure",
#          ideo5 != "Skipped", ideo5 != "Not Asked", ideo5 != "Not sure",
#          novoterecorded != 1,
#          CC16_330e != "Skipped", CC16_330e != "Not Asked", 
#          CC16_331_7 != "Skipped", CC16_331_7 != "Not Asked",
#          CC16_332a != "Skipped", CC16_332a != "Not Asked",
#          CC16_333d != "Skipped", CC16_333d != "Not Asked",
#          CC16_334a != "Skipped", CC16_334a != "Not Asked",
#          CC16_351I != "Skipped", CC16_351I != "Not Asked",
#          CC16_351K != "Skipped", CC16_351K != "Not Asked")

# ----

# variable adjustments and additions ----

df1 <- df %>%
  mutate(race = ifelse(race == "White", "white",
                       ifelse(race == "Black", "black",
                              ifelse(race == "Hispanic" | hispanic == "Yes", "Hispanic",
                                     ifelse(race == "Asian", "Asian",
                                            ifelse(race == "Native American", "Native American", 
                                                   ifelse(race == "Mixed", "mixed race", 
                                                          ifelse(race == "Middle Eastern", "Middle Eastern",
                                                                 ifelse(race == "Other", "other race", NA)))))))),
         gender = ifelse(gender == "Male", "man", 
                         ifelse(gender == "Female", "woman", NA)),
         age = 2017 - birthyr,
         educ2 = ifelse(educ %in% c("No HS", "High school graduate", "Some college"), "non-college-educated", "college-educated"),
         pid7.pre = ifelse(pid7 == "Not sure" | pid7 == "Skipped" | pid7 == "Not Asked", NA, 
                           ifelse(pid7 == "Strong Democrat", "Strong Democrat",
                                  ifelse(pid7 == "Not very strong Democrat", "Not very strong Democrat",
                                         ifelse(pid7 == "Lean Democrat", "Lean Democrat",
                                                ifelse(pid7 == "Independent", "Independent",
                                                       ifelse(pid7 == "Lean Republican", "Lean Republican",
                                                              ifelse(pid7 == "Not very strong Republican", "Not very strong Republican",
                                                                     ifelse(pid7 == "Strong Republican", "Strong Republican", NA)))))))), 
         pid3.pre = ifelse(pid7.pre == "Strong Democrat" | pid7.pre == "Not very strong Democrat" | pid7.pre == "Lean Democrat", "Democrat",
                           ifelse(pid7.pre == "Strong Republican" | pid7.pre == "Not very strong Republican" | pid7.pre == "Lean Republican", "Republican",
                                  ifelse(pid7.pre == "Independent", "Independent", NA))),
         ideo5.pre = ifelse(ideo5 == "Not sure" | ideo5 == "Skipped" | ideo5 == "Not Asked", NA, 
                            ifelse(ideo5 == "Very liberal", "very liberal",
                                   ifelse(ideo5 == "Liberal", "liberal",
                                          ifelse(ideo5 == "Moderate", "moderate",
                                                 ifelse(ideo5 == "Conservative", "conservative",
                                                        ifelse(ideo5 == "Very conservative", "very conservative", NA)))))),
         ideo3.pre = ifelse(ideo5.pre == "Very liberal" | ideo5.pre == "Liberal", "Liberal",
                            ifelse(ideo5.pre == "Very conservative" | ideo5.pre == "Conservative", "Conservative",
                                   ifelse(ideo5.pre == "Moderate", "Moderate", NA))))#,


# votechoice = ifelse(CC16_410a=="Donald Trump (Republican)" & CL_E2016GVM!="", "I voted for Donald Trump",
#                     ifelse(CC16_410a=="Hillary Clinton (Democrat)" & CL_E2016GVM!="", "I voted for Hillary Clinton",
#                            ifelse(CC16_410a=="Gary Johnson (Libertarian)" & CL_E2016GVM!="", "I voted for Gary Johnson",
#                                   ifelse(CC16_410a=="Jill Stein (Green)" & CL_E2016GVM!="", "I voted for Jill Stein",
#                                          ifelse(CC16_410a=="Evan McMullin (Independent)" & CL_E2016GVM!="", "I voted for Evan McMullin",
#                                                 ifelse(CC16_410a=="Other" & CL_E2016GVM!="", "I voted for another candidate",
#                                                        ifelse((CC16_410a %in% c("I didn't vote in this election", "I'm not sure", "Skipped", "Not Asked")) & CL_E2016GVM!="", "I didn't vote in 2016",
#                                                               ifelse(CL_E2016GVM=="", "I didn't vote in 2016", vote))))))))) 

# why are there still NA's for vote choice? -- this is for above code chunk (changing votechoice with ifelse)
#unique(df1$votechoice)
#temp <- df1 %>% filter(is.na(votechoice)) %>% select(CL_E2016GVM)

df1$votechoice <- "I voted another candidate in 2016"
df1$votechoice[df1$CC16_410a=="Donald Trump (Republican)" & df1$CL_E2016GVM!=""] <- "I voted Donald Trump in 2016"
df1$votechoice[df1$CC16_410a=="Hillary Clinton (Democrat)" & df1$CL_E2016GVM!=""] <- "I voted Hillary Clinton in 2016"
df1$votechoice[df1$CC16_410a=="Gary Johnson (Libertarian)" & df1$CL_E2016GVM!=""] <- "I voted Gary Johnson in 2016"
df1$votechoice[df1$CC16_410a=="Jill Stein (Green)" & df1$CL_E2016GVM!=""] <- "I voted Jill Stein in 2016"
df1$votechoice[df1$CC16_410a=="Evan McMullin (Independent)" & df1$CL_E2016GVM!=""] <- "I voted Evan McMullin in 2016"
df1$votechoice[df1$CC16_410a=="Other" & df1$CL_E2016GVM!=""] <- "I voted another candidate in 2016"
df1$votechoice[(df1$CC16_410a %in% c("I didn't vote in this election", "I'm not sure", "Skipped", "Not Asked")) & df1$CL_E2016GVM!=""] <- "I didn't vote in 2016"
df1$votechoice[df1$CL_E2016GVM==""] <- "I didn't vote in 2016"

table(df1$votechoice)
unique(df1$votechoice)

table(df1$CC16_410a, df1$CL_E2016GVM)

#~~~

df2 <- df1 %>%
  select(V101, inputstate_post, commonweight_vv_post, race, gender, age, educ2, votechoice, pid3.pre, ideo5.pre, 
         "concealed" = CC16_330e, "deport" = CC16_331_7, "prochoice" = CC16_332a, "cleanair" = CC16_333d,
         "mandmin" = CC16_334a, "aca" = CC16_351I, "minwage" = CC16_351K) %>%
  mutate(concealed = ifelse(concealed =="Support", "supports concealed-carry", "opposes concealed-carry"), # simplified
         deport = ifelse(deport == "Yes", "supports deporting illegal immigrants", "opposes deporting illegal immigrants"),
         prochoice = ifelse(prochoice == "Support", "always supports abortion", "sometimes opposes abortion"),
         cleanair = ifelse(cleanair == "Support", "supports the Clean Air Act", "opposes the Clean Air Act"), # simplified
         mandmin = ifelse(mandmin == "Support", "opposes mandatory minimums", "supports mandatory minimums"), # simplified (check q -- reversed)
         aca = ifelse(aca == "For", "opposes the ACA", "supports the ACA"), # simplified
         minwage = ifelse(minwage == "For", "supports raising the min. wage", "opposes raising the min. wage")) %>% # simplified
  # filtering here
  filter(!is.na(pid3.pre), !is.na(ideo5.pre), !is.na(inputstate_post))

# manage state
data.frame(inputstate_post = c(state.name, "District of Columbia"), abb = c(state.abb, "DC"))

states <- data.frame(inputstate_post = c(state.name, "District of Columbia"), abb = c(state.abb, "DC"))

df2 <- df2 %>% 
  left_join(states, by = "inputstate_post") %>%
  mutate(state = ifelse(is.na(abb),"the U.S.",abb))

# if weight is 0, just set to the median of all weights (~0.7)
df2 <- df2 %>%
  mutate(commonweight_vv_post = ifelse(is.na(commonweight_vv_post), median(df2$commonweight_vv_post,na.rm=T),commonweight_vv_post))

# ----

# WEIGHTED random selection ---- 

# names of issue opinion to sample from (choose 3 out of 7)
opinions <- names(df2[,11:17])

# randomly selecting an observation (individual)
df3 <- df2[sample(nrow(df2), size = 1, prob = df2$commonweight_vv_post),]

# randomly selecting 3 issue positions
df3 <- df3 %>% select(sample(opinions, 4), "pid" = pid3.pre, "ideo" = ideo5.pre, race, gender, age, educ2, votechoice)

result <- paste0("I'm a ", df3$age, " year old, ", df3$educ2, " ", df3$race, " ", df3$gender, #19
                 ". I'm a ", df3$ideo, " ", df3$pid, " who ", df3[,1], ", ", df3[,2], ", ", # 18
                 df3[,3], ", and ", df3[,4], ". ", df3$votechoice, ".") # 9

result
nchar(result)

# constant character count = 46
# unique values for each var
nchar(unique(df2$race)) # max = 15
nchar(unique(df2$gender)) # max = 5
nchar(unique(df2$age)) # max = 2
nchar(unique(df2$educ2)) # max = 20
nchar(unique(df2$votechoice)) # max = 37
nchar(unique(df2$pid3.pre)) # max = 11
nchar(unique(df2$ideo5.pre)) # max = 17

# pick 3 of 7 = 94
# pick 4 of 7 = 120
# pick 5 of 7 = 146
nchar(unique(df2$concealed)) # max = 24
nchar(unique(df2$deport)) # max = 37
nchar(unique(df2$prochoice)) # max = 26
nchar(unique(df2$cleanair)) # max = 26
nchar(unique(df2$mandmin)) # max = 27
nchar(unique(df2$aca)) # max = 16
nchar(unique(df2$minwage)) # max = 30
# 37, 30, 27, 26, 26, 24, 16

46+15+5+2+20+37+11+17+94 # pick 3 = 247
46+15+5+2+20+37+11+17+120 # pick 4 = 273
46+15+5+2+20+37+11+17+146 # pick 5 = 299

# ******* go with 4 issue postions ************

# ----

# final sampling and writing the file ----
# function for sampling and making response
sample_obvs <- function(obvsdf){
  # get a row via weighted sampling
  respondentx <- obvsdf[sample(nrow(obvsdf), size = 1, prob = obvsdf$commonweight_vv_post),]
  
  #
  indiv <- respondentx %>% select(sample(opinions, 4), "pid" = pid3.pre, "ideo" = ideo5.pre, race, gender, age, educ2, votechoice,state)
  
  result <- data.frame("text" =sprintf("I'm a %s year old, %s, %s %s from %s.

I'm a %s %s who %s, %s, %s, and %s. 
                    
%s.",indiv$age, indiv$educ2, indiv$race, indiv$gender,indiv$state, indiv$ideo, indiv$pid, indiv[,1], indiv[,2], indiv[,3], indiv[,4], indiv$votechoice)
  )
  
  # get id to strip this observation from file 
  obvs_id <<- respondentx$V101
  
  return(result)
  
}

# testing it out
loop_df <- df2 %>% na.omit()
sample_obvs(loop_df)

# sample 60,000 respondents by weight, make sure not to grab people we already used

# setup parallel backend to use many processors
## Calculate the number of cores
library(doParallel)
num_cores <- ifelse(detectCores()>1,detectCores() - 1,detectCores()) # get available cores
registerDoParallel(cores=num_cores)  
cl <- parallel::makeCluster(num_cores)

# do the loop in parallel -- shouldn't take more than 10-15 minutes with 7 cores
system.time(
  parallel_output <- foreach(i=1:nrow(loop_df),
                            .combine = 'rbind') %dopar% {
  result <-  sample_obvs(loop_df)
  
  while(nchar(result)>279){
    result <- sample_obvs(loop_df)
  }
  
  
  loop_df <- loop_df[loop_df$V101 != obvs_id,]
  
  return(result)

}

)
stopCluster(cl)

output <- parallel_output %>%
  filter(!str_detect("NA",text))

output <- output %>% 
  filter(!duplicated(text))

# save as csv

write.csv(parallel_output,"CCES_ANNOTATED.csv",row.names = FALSE)

#----

# distribution of constraint ----
# ----

df3 <- df2 %>%
  mutate(pid = ifelse(pid3.pre == "Democrat", 1, 
                      ifelse(pid3.pre == "Republican", -1, 0)),
         ideo = ifelse(ideo5.pre == "liberal" | ideo5.pre == "very liberal", 1,
                       ifelse(ideo5.pre == "conservative" | ideo5.pre == "very conservative", -1, 0)),
         conc = ifelse(concealed == "opposes concealed-carry", 1, -1),
         dep = ifelse(deport == "opposes deporting illegal immigrants", 1, -1),
         choice = ifelse(prochoice == "always supports abortion", 1, -1),
         clean = ifelse(cleanair == "supports the Clean Air Act", 1, -1),
         mand = ifelse(mandmin == "opposes mandatory minimums", 1, -1),
         affcare = ifelse(aca == "supports the ACA", 1, -1),
         wage = ifelse(minwage == "supports raising the min. wage", 1, -1)) %>%
  mutate(constraint = pid + ideo + conc + dep + choice + clean + mand + affcare + wage,
         abs.constraint = abs(constraint))

# total distribution

ggplot() + 
  geom_bar(data = df3, aes(x = abs.constraint)) +
  scale_x_continuous(breaks = seq(0, 9, 1))

# percentage distribution

df4 <- df3 %>%
  group_by(abs.constraint) %>%
  summarize(total = sum(n())) %>%
  mutate(pct = 100 * total / sum(total),
         pos = ifelse(pct < 2, -1.15, 0.5), 
         col = ifelse(pct < 2, "black", "white"),
         pct1 = paste0(round(pct), "%")) 

ggplot() + 
  geom_bar(data = df4, aes(x = abs.constraint, y = pct), stat = "identity") +
  geom_text(data = df4, aes(x = abs.constraint, y = pct / 2, label = pct1), 
            size = 4.5, color = "white") + 
  scale_x_continuous(breaks = seq(0, 9, 1)) +
  theme_few() +
  labs(title = "Ideological Constraint Among Americans",
       subtitle = "Percentage Holding Consistently Democratic/Liberal or Republican/Conservative Political Positions",
       x = "0 = Most Inconsistent, 9 = Most Consistent") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
        plot.caption = element_text(size = 10, face = "italic"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        strip.text = element_text(size = 11),
        legend.position = "bottom") 

# looping through: get only 3 of the 7 policy positions

df.num <- df3 %>% select(-c(pid3.pre:minwage))
opinions <- names(df.num[,4:9])
constraint <- data.frame(V101 = character(), total = numeric())

for (i in unique(df.num$V101)) {
  
  df5 <- df.num %>% 
    filter(V101 == i) %>%
    select(V101, pid, ideo, sample(opinions, 3)) 
  names(df5) <- c("V101", "var1", "var2", "var3", "var4", "var5")
  df5 <- df5 %>% 
    mutate(total = abs(var1 + var2 + var3 + var4 + var5)) %>%
    select(V101, total)
  constraint <- rbind(constraint, df5)
  rm(df5)
  
}

# link new data frame 
df3 <- left_join(df3, constraint, by = "V101")

# percentage distribution

df4 <- df3 %>%
  filter(!is.na(total.y)) %>%
  group_by(total.y) %>%
  summarize(x = sum(n())) %>%
  mutate(pct = 100 * x / sum(x),
         pct1 = paste0(round(pct), "%")) 

ggplot() + 
  geom_bar(data = df4, aes(x = total.y, y = pct), 
           stat = "identity", fill = "midnightblue") +
  geom_text(data = df4, aes(x = total.y, y = pct / 2, label = pct1), 
            size = 5, color = "white") + 
  scale_x_continuous(breaks = seq(0, 5, 1)) +
  theme_few() +
  labs(title = "Ideological Constraint Among Americans",
       subtitle = "Percentage Holding Consistently Democratic/Liberal or Republican/Conservative Political Positions",
       x = "0 = Least Consistent, 5 = Most Consistent") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
        plot.caption = element_text(size = 10, face = "italic"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size = 11),
        strip.text = element_text(size = 11),
        legend.position = "bottom") 

ggsave("/Users/Victor/Desktop/Blog-data/visualization/non-election season 2016-17 (2)/constraint090517.png", width = 10, height = 7.5)

# ----

