# Alexander Agadjanian
# 
# last update: 3/16/18

# Notes:
# updating cces bot with updated cces data (fixed turnout data)
# checking for mistake with aca support/oppose question coding
#
# using: age, race, gender, ideology, party, 3-4 policies, vote choice (options/didn't vote)
#

# loading packages ----
rm(list = ls())
# library(matrixStats) 
library(ggrepel)
# library(maps)
library(ggthemes)
library(stringr)
# library(gridExtra)
library(survey)
library(haven)
library(foreign)
library(lubridate)
library(broom)
library(stargazer)
# library(utils)
# library(pollstR)
# library(margins)
# library(MASS)
# library(gsheet)
library(tidyverse)
options(stringsAsFactors = FALSE)
# ----

# reading in data ----

file <- "/Users/Victor/Desktop/Blog-data/cces 2016/" # set working directory here
setwd(file)
cces <- read.dta("CCES16_Common_OUTPUT_Feb2018_VV.dta")

# selecting variables 
df <- cces %>% 
  select(V101, commonweight, commonweight_post, pid7, ideo5, # party id and ideo from pre
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

df1$votechoice <- "I voted for another candidate in 2016"
df1$votechoice[df1$CC16_410a=="Donald Trump (Republican)" & df1$CL_E2016GVM!=""] <- "I voted for Donald Trump in 2016"
df1$votechoice[df1$CC16_410a=="Hillary Clinton (Democrat)" & df1$CL_E2016GVM!=""] <- "I voted Hillary Clinton in 2016"
df1$votechoice[df1$CC16_410a=="Gary Johnson (Libertarian)" & df1$CL_E2016GVM!=""] <- "I voted Gary Johnson in 2016"
df1$votechoice[df1$CC16_410a=="Jill Stein (Green)" & df1$CL_E2016GVM!=""] <- "I voted Jill Stein in 2016"
df1$votechoice[df1$CC16_410a=="Evan McMullin (Independent)" & df1$CL_E2016GVM!=""] <- "I voted Evan McMullin in 2016"
df1$votechoice[df1$CC16_410a=="Other" & df1$CL_E2016GVM!=""] <- "I voted for another candidate in 2016"
df1$votechoice[(df1$CC16_410a %in% c("I didn't vote in this election", "I'm not sure", "Skipped", "Not Asked")) & df1$CL_E2016GVM!=""] <- "I didn't vote in 2016"
df1$votechoice[df1$CL_E2016GVM==""] <- "I didn't vote in 2016"

table(df1$votechoice)
unique(df1$votechoice)

table(df1$CC16_410a, df1$CL_E2016GVM)

#~~~

df2 <- df1 %>%
  select(V101, race, gender, age, educ2, votechoice, pid3.pre, ideo5.pre, 
         "concealed" = CC16_330e, "deport" = CC16_331_7, "prochoice" = CC16_332a, "cleanair" = CC16_333d,
         "mandmin" = CC16_334a, "aca" = CC16_351I, "minwage" = CC16_351K) %>%
  mutate(concealed = ifelse(concealed =="Support", "supports concealed-carry", "opposes concealed-carry"), # simplified
         deport = ifelse(deport == "Yes", "supports deporting illegal immigrants", "opposes deporting illegal immigrants"),
         prochoice = ifelse(prochoice == "Support", "always supports abortion", "sometimes opposes abortion"),
         cleanair = ifelse(cleanair == "Support", "supports the Clean Air Act", "opposes the Clean Air Act"), # simplified
         mandmin = ifelse(mandmin == "Support", "opposes mandatory minimums", "supports mandatory minimums"), # simplified (check q -- reversed)
         aca = ifelse(aca == "For", "opposes the ACA", "supports the ACA"), # simplified -- switched to reflect "repeal"
         minwage = ifelse(minwage == "For", "supports raising the min. wage", "opposes raising the min. wage")) %>% # simplified
  # filtering here
  filter(!is.na(pid3.pre), !is.na(ideo5.pre))

# ----

# random selection ---- 

# names of issue opinion to sample from (choose 3 out of 7)
opinions <- names(df2[,9:14])

# randomly selecting an observation (individual)
df3 <- df2[sample(nrow(df2), 1),]

# randomly selecting 3 issue positions
df3 <- df3 %>% select(sample(opinions, 4), "pid" = pid3.pre, "ideo" = ideo5.pre, race, gender, age, educ2, votechoice)

# old
# displaying statement
# result <- paste0("I'm a ", df3$ideo, " ", df3$pid, " who ", df3[,1], ", ", df3[,2], ", & ", df3[,3], ".")
# result

#trying to write if/else statement for a/an before age

# if(df3$age == 18 | (df3$age<=80 & df3$age<=89))
#   result <- paste0("I'm a ", df3$age, " year old, ", df3$educ2, " ", df3$race, " ", df3$gender, #19
#                    ". I'm a ", df3$ideo, " ", df3$pid, " who ", df3[,1], ", ", df3[,2], ", ", # 18 
#                    df3[,3], ", and ", df3[,4], ". ", df3$votechoice, ".")
# else()

# most concise:
# result <- paste0("I'm a ", df3$race, " ", df3$gender, ", ", df3$age, " ", df3$educ2,  #10
#                  ". I'm a ", df3$ideo, " ", df3$pid, " who ", df3[,1], ", ", df3[,2], ", ", # 18 
#                  df3[,3], ", & ", df3[,4], ". ", df3$votechoice, ".") #7


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

# final----
opinions <- names(df2[,9:14])
df3 <- df2[sample(nrow(df2), 1),]
df3 <- df3 %>% select(sample(opinions, 4), "pid" = pid3.pre, "ideo" = ideo5.pre, race, gender, age, educ2, votechoice)
result <- paste0("I'm a ", df3$age, " year old, ", df3$educ2, " ", df3$race, " ", df3$gender, #19
                 ". I'm a ", df3$ideo, " ", df3$pid, " who ", df3[,1], ", ", df3[,2], ", ", # 18
                 df3[,3], ", and ", df3[,4], ". ", df3$votechoice, ".") # 9

result
nchar(result)
#----

# need to discard observation after being used for a tweet -- reuse same data file each time?

# ----

