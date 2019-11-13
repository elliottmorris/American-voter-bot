# Alexander Agadjanian and G. Elliott Morris
# 
# created: 11/21/17
# updated: 11/12/19
#
# This script parses the 2016 cooperative congressional election study
# and extracts data for a Twitter bot.
# 
# example:
# "I'm a 21 year old white man. I'm a very conservative Republican who 
# supports concealed carry, never supports abortion, and support 
# immigration. I voted for Jill Stein"
#
# Vars used areage, race, gender, ideology, party, 4 (of 7) policies, 
# and vote choice (options/didn't vote)
#
#

rm(list = ls())
library(foreign)
library(stringr)
library(survey)
library(tidyverse)
library(pbapply)
library(parallel)
options(stringsAsFactors = FALSE)


# reading in data  --------------------------------------------------------
# read in the cces -- I have saved a binary file with the vars we need to avoid GitHub size 
# otherwise, download the CCES and run the commented line below
# cces <- read.dta("CCES16_Common_OUTPUT_Feb2018_VV.dta") 
cces <- read_rds('cces_2016.rds')

# selecting variables 
cces <- cces %>% 
  select(V101, inputstate_post, commonweight, commonweight_post,commonweight_vv_post, 
         pid7, ideo5, # party id and ideo from pre
         gender, race, hispanic, birthyr, educ, CL_E2016GVM, CC16_410a,
         CC16_330e, CC16_331_7, CC16_332a, CC16_333d, CC16_334a,
         CC16_351I, CC16_351K)


# mutate demographic and  partisan id variables  --------------------------
# race, gender, age, education, party identitication and ideology
demog <- cces %>%
  mutate(race = case_when(race == 'White' ~ 'white',
                          race == 'Black' ~ 'black',
                          race == 'Hispanic' | hispanic == 'Yes' ~ 'hispanic',
                          race == 'Asian' ~ 'Asian',
                          race == 'Native American' ~ 'Native American',
                          race == 'Mixed' ~ 'mixed race',
                          race == 'Middle Eastern' ~ 'Middle Eastern',
                          race == 'Other' ~ 'other race',
                          TRUE ~ NA_character_),
         
         gender = case_when(gender == 'Male' ~ 'man',
                            gender == 'Female' ~ 'woman',
                            TRUE ~ NA_character_),
         
         age = 2017 - birthyr,
         
         educ2 = case_when(educ %in% c('No HS', 'High school graduate', 'Some college') ~ 'non-college-educated',
                           !is.na(educ) ~ 'college-educated',
                           TRUE ~ NA_character_),
         
         pid7.pre = case_when(pid7 == 'Strong Democrat' ~ 'Strong Democrat',
                              pid7 == 'Not very strong Democrat' ~ 'Not very strong Democrat',
                              pid7 == 'Lean Democrat' ~ 'Lean Democrat',
                              pid7 == 'Independent' ~ 'Independent',
                              pid7 == 'Lean Republican' ~ 'Lean Republican',
                              pid7 == 'Not very strong Republican' ~ 'Not very strong Republican',
                              pid7 == 'Strong Republican' ~ 'Strong Republican',
                              TRUE ~ NA_character_),
         
         pid3.pre = case_when(pid7.pre == 'Strong Democrat' | 
                                pid7.pre == 'Not very strong Democrat' | 
                                pid7.pre == 'Lean Democrat' ~ 'Democrat',
                              pid7.pre == 'Strong Republican' | 
                                pid7.pre == 'Not very strong Republican' | 
                                pid7.pre == 'Lean Republican' ~ 'Republican',
                              pid7.pre == 'Independent' ~ 'Independent',
                              TRUE ~ NA_character_),
         
         ideo5.pre = case_when(ideo5 == "Very liberal" ~ "very liberal",
                               ideo5 == "Liberal" ~ "liberal",
                               ideo5 == "Moderate" ~ "moderate",
                               ideo5 == "Conservative" ~ "conservative",
                               ideo5 == "Very conservative" ~ "very conservative",
                               TRUE ~ NA_character_),
                                         
         ideo3.pre = case_when(ideo5.pre == "Very liberal" | ideo5.pre == "Liberal" ~ "Liberal",
                               ideo5.pre == "Very conservative" | ideo5.pre == "Conservative" ~ "Conservative",
                               ideo5.pre == "Moderate" ~ "Moderate",
                               TRUE ~ NA_character_)
  )

# code vote variables -----------------------------------------------------
# new approach to coding vote var; avoid miscoding of non-post-takers as "voted for another candidate" -- make sure the true NAs are assigned
# only a problem for miscoding some people who 1) didn't take post & 2) weren't identified as non-voters from vote val as "I voted for another candidate in 2016"; not too large, but an error
demog_a <- demog %>% 
  mutate(votechoice = case_when(CC16_410a=="Donald Trump (Republican)" ~ "I voted for Donald Trump in 2016",
                                CC16_410a=="Hillary Clinton (Democrat)" ~ "I voted Hillary Clinton in 2016",
                                CC16_410a=="Gary Johnson (Libertarian)" ~ "I voted Gary Johnson in 2016",
                                CC16_410a=="Jill Stein (Green)" ~ "I voted Jill Stein in 2016",
                                CC16_410a=="Evan McMullin (Independent)" ~ "I voted Evan McMullin in 2016",
                                CC16_410a=="Other" ~ "I voted for another candidate in 2016",
                                CC16_410a %in% c("I didn't vote in this election", "I'm not sure",
                                                 "Skipped", "Not Asked") ~ "I didn't vote in 2016",
                                TRUE ~ NA_character_)) # make sure everyone else is NA (namely non-post-takers)

# being careful, just start a new pipe to integrate vote val turnout
# just overwrite those as "" in CL_E2016GVM as non-voters, otherwise leave votechoice var alone -- could be people with a choice or NA (non-post-takers)
demog_b <- demog_a %>% 
  mutate(votechoice = case_when(CL_E2016GVM=="" ~ "I didn't vote in 2016",
                                TRUE ~ votechoice))



# recode attitudes --------------------------------------------------------
# simplify codings
attitudes <- demog_b %>%
  select(V101, inputstate_post, commonweight_vv_post, 
         race, gender, age, educ2, votechoice, pid3.pre, ideo5.pre, 
         concealed = CC16_330e, 
         deport = CC16_331_7, 
         prochoice = CC16_332a, 
         cleanair = CC16_333d,
         mandmin = CC16_334a, 
         aca = CC16_351I, 
         minwage = CC16_351K) %>%
  mutate(concealed = case_when(concealed =="Support" ~ "supports concealed-carry", 
                               !is.na(concealed) ~ "opposes concealed-carry",
                               TRUE ~ NA_character_),
         
         deport = case_when(deport == "Yes" ~ "supports deporting illegal immigrants", 
                            !is.na(deport) ~ "opposes deporting illegal immigrants",
                            TRUE ~ NA_character_),
         
         prochoice = case_when(prochoice == "Support" ~ "always supports abortion", 
                               !is.na(prochoice) ~ "sometimes opposes abortion",
                               TRUE ~ NA_character_),
         
         cleanair = case_when(cleanair == "Support" ~ "supports the Clean Air Act", 
                              !is.na(cleanair) ~ "opposes the Clean Air Act",
                              TRUE ~ NA_character_), 
         
         mandmin = case_when(mandmin == "Support" ~ "opposes mandatory minimums",
                             !is.na(mandmin) ~ "supports mandatory minimums",
                             TRUE ~ NA_character_), 
         
         aca = case_when(aca == "For" ~ "opposes the ACA", 
                         !is.na(aca) ~ "supports the ACA",
                         TRUE ~ NA_character_),
         
         minwage = case_when(minwage == "For" ~ "supports raising the min. wage",
                             !is.na(minwage) ~ "opposes raising the min. wage",
                             TRUE ~ NA_character_)) %>% 
  # filtering here
  filter(!is.na(pid3.pre), !is.na(ideo5.pre), !is.na(inputstate_post))

# manage state
data.frame(inputstate_post = c(state.name, "District of Columbia"), abb = c(state.abb, "DC"))

states <- data.frame(inputstate_post = c(state.name, "District of Columbia"), abb = c(state.abb, "DC"))

attitudes <- attitudes %>% 
  left_join(states, by = "inputstate_post") %>%
  mutate(state = ifelse(is.na(abb),"the U.S.",abb)) %>%
  select(-abb)

# remove NA
full_data <- attitudes %>% na.omit()

# checking..... -----------------------------------------------------------
# names of issue opinion to sample from (choose 3 out of 7)
lapply(names(full_data)[4:18],
       function(x){
         full_data %>%
           group_by_at(x) %>%
           summarise(prop = sum(commonweight_vv_post)) %>%
           ungroup() %>%
           mutate(prop = round(prop/sum(prop)*100))  %>%
           arrange(desc(prop))
       })

# weighted random selection and profile generation ------------------------
# function for sampling and making response
create_profile <- function(df, row_index){
  # get a row via weighted sampling
  respondentx <- df[row_index,]
  
  indiv <- respondentx %>% 
    select(sample(names(respondentx)[11:17], 4), 
           "pid" = pid3.pre, 
           "ideo" = ideo5.pre, race, gender, age, educ2, votechoice, state)
  
  result <- data.frame("text" = sprintf("I'm a %s year old, %s, %s %s from %s.

I'm a %s %s who %s, %s, %s, and %s. 
                    
%s.",indiv$age, indiv$educ2, indiv$race, indiv$gender,indiv$state, indiv$ideo, indiv$pid, indiv[,1], indiv[,2], indiv[,3], indiv[,4], indiv$votechoice)
  )
  
  nchar(result)
  
  # try again if nchar won't fit in tweet
  while(nchar(result)>279){
    indiv <- respondentx %>% 
      select(sample(names(respondentx)[11:17], 4), 
             "pid" = pid3.pre, 
             "ideo" = ideo5.pre, race, gender, age, educ2, votechoice, state)
    
    result <- data.frame("text" = sprintf("I'm a %s year old, %s, %s %s from %s.

I'm a %s %s who %s, %s, %s, and %s. 
                    
%s.",indiv$age, indiv$educ2, indiv$race, indiv$gender,indiv$state, indiv$ideo, indiv$pid, indiv[,1], indiv[,2], indiv[,3], indiv[,4], indiv$votechoice)
    )
    
  }
  
  # return once done
  return(result)
  
}

create_profile(full_data,10)

# sample respondents by weight, make sure not to grab people we already used
full_data <- full_data %>%
  arrange(desc(commonweight_vv_post))

# go
num_cores <- max(1,parallel::detectCores()-1)

# do the loop in parallel -- shouldn't take more than a minute with 7 cores
parallel_output <- pblapply(1:nrow(full_data),
                            cl=num_cores,
                            function(x){
                              
                              return(create_profile(full_data,x))
                             
                           })
# collapse list
output <- parallel_output %>% do.call('bind_rows',.)

# make sure there are no NAs (even though there really, really shouldn't be....)
output <- output %>%
  filter(!str_detect("NA",text))

# save as csv
write_csv(output,"CCES_ANNOTATED.csv")
