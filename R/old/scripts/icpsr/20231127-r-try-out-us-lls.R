library(psych)
library(tidyverse)
library(haven)
library(labelled)
library(todor)
library(codebook)

# this script is a try-out
# categories of parenting need to be checked again
# based on this file, make an empty file with all categories that need to be coded and part of the script

######################
# SETTINGS
######################

# QUESTION NEHA change the date when working in GITHUB?

options(max.print=100000) 

######################
# FUNCTIONS
######################

# recode and rename function
# provide: 
# dataset = dataset with new variables
# dataset_old = original dataset
# new_name = name of the new variable
# names_variables = the names of the variables that need to be recoded
# the value at which the counting should start
names_variables<-c("zses")
dataset_new<-df.basic
# QUESTION : can you give feedback on this function?
# it works for some variables but not for other (education)
recode_and_rename <- function(dataset, dataset_old, new_name, names_variables,start_value){
  dataset_new<-dataset
  x_columns <- grep("X", colnames(dataset_new), value = TRUE)   # distill al X columns
  print(c(paste("columns left:"),x_columns))
  #select the first empty X column and add variables from old dataset here
  dataset_new[,which(names(dataset_new)%in%c(x_columns[1:(length(names_variables))]))]<-dataset_old[,c(which(names(dataset_old)%in%c(names_variables)))]
  col<-which(names(dataset_new)%in%c(x_columns[1:(length(names_variables))]))
  newcol<-1:(length(names_variables)) # starts at 1; same length as col
  print(c(paste("recoded columns in new dataset:"),col))
  print(c(paste("n variables:"),newcol))
  # Rename each column in a loop 
  for (i in newcol) {
    old_column_name <- paste("X", col[i], sep = "")
    print(old_column_name)
    new_column_name <- paste(new_name, ((start_value-1)+newcol[i]), sep = "") 
    print(c(paste("names new variables:"),new_column_name))
    colnames(dataset_new)[colnames(dataset_new) == old_column_name] <- new_column_name
  }
  return(dataset_new)
}

# Function for adding variables to the excluded dataset
# Provide: name of the dataset in which the variables are stored, names of the variables
# all variables will be stored in df.excluded

exclude <- function(dataset_old, names_variables){
  dataset_new<-df.excluded
  x_columns <- grep("X", colnames(dataset_new), value = TRUE)   # distill al X columns
  #select the first empty X column and add variables from old dataset here
  dataset_new[,which(names(dataset_new)%in%c(x_columns[1:(length(names_variables))]))]<-dataset_old[,c(which(names(dataset_old)%in%c(names_variables)))]
  colnames(dataset_new)[which(names(dataset_new)%in%c(x_columns[1:(length(names_variables))]))]<-names_variables
      return(dataset_new)
}

######################
# NOTES
######################

# All notes will be saved in a seperate file, depending on the tag (e.g., NOTES.todo).
# start each note with the relevant tag, followed by date and the code of the dataset, the variable, and note

# Get today's date
today_date <- Sys.Date() # QUESTION NEHA add this date to notes?

# NOTES.todo  - everything that needs to be done (e.g., ask for more info) and discussed 
# NOTES.demographics - all notes about the samples demographics
# NOTES.parenting - all notes about the G1 and G2 parenting measures
# NOTES.moderators - all notes about the samples demographics
# NOTE.ID - info needed to interpret the ID numbers 

######################
# Read data
######################

df<-read_sav(file='O:/Research/FSW/Research_data/JG/SanneG/ICITOP IPD_MA/102.us-lls/2.data-checks/ICITOP/Lehigh_ICITOP.sav')

######################
# Data check
######################

# QUESTION NEHA can we keep the original and new variable name in a codebook is variables are not in the same order?

# 1. check if data is labelled 
haven::is.labelled(df) # No doesn't mean that a codebook cannot be generated
#data.dictionary.1 <- labelled::generate_dictionary(df)

data.dictionary.2 <- codebook::codebook_table(df) # this one is very slow but use this one

data.dictionary<-data.dictionary.2[,c(1,2,4,5,6,9,10,11,12,13)]

write.csv(data.dictionary,'O:/Research/FSW/Research_data/JG/SanneG/ICITOP IPD_MA/102.us-lls/2.data-checks/20231127-102.us-lls-datadictionary-original.csv', row.names=FALSE)

#2.recode and check data
# make an empty dataframe for variables that will be kept (basic) and excluded
# ncol = length original file + 1 (ID)

df.basic <- data.frame(matrix(ncol = 1317, nrow = 457))
df.excluded<-data.frame(matrix(ncol = 1317, nrow = 457))

# 2a. determine ID's
# NOTE.ID code: us-lls
# NOTE.ID number: 14
# NOTE.ID How many G1 parents? 2
# NOTE.ID How many G2 children/parents? 1
# NOTE.ID How many G3 children? - all children
# NOTE.ID structure data = wide, so only 1 ID needed

# 1. Study ID can be found in bta-overview (OSF)
colnames(df.basic)[1]<-"studyid"
df.basic$studyid<-14

# 2. subcohort ID, use info in dataset
# NOTE.ID lehigh has a variable on subcohort
# another option is that two datasets have to be merged and an ID number needs to be added
colnames(df.basic)[2]<-"subcohortid"
df.basic$subcohortid<-df$group_baseline

# 3. family ID - only relevant if multiple G2 participants per family
# not applicable for Lehigh 
# NOTE.question for 14.us-lls is family ID neccesary 

# 4. participant ID (G2 focal participant)
colnames(df.basic)[3]<-"id"
df.basic$id<-df$ID

# 5. code all the answers to the quesions in data request
# copy info directly from what has been provided
# Country and region where study took place
# I WILL CHANGE THIS INTO A LOOP
colnames(df.basic)[4]<-"country"
df.basic$country<-"USA"

colnames(df.basic)[5]<-"region"
df.basic$region<-"eastern Pennsylvania"

# Level of representativeness of the sample 
colnames(df.basic)[6]<-"representativeness"
df.basic$representativeness<-"The sample reflects the demographic makeup of the two-county area from which it was drawn but is not representative of the U.S. population."

# Recruitment strategy
colnames(df.basic)[7]<-"recruitment"
df.basic$recruitment<-"Families/children were recruited from child welfare agencies in the two counties and from other group settings (day care programs, nursery school programs, Head Start programs) in that same area."

# Is this sample specifically urban or rural?
colnames(df.basic)[8]<-"urban"
df.basic$urban<-"The area from which the participants were drawn is considered suburban. It sits outside of two relatively large urban centers (New York and Philadelphia)."

# Year at start of the study 
# NOTE.demo 20231127 102. us-lls year start was when G1 was 18 months old
colnames(df.basic)[9]<-"year.start"
df.basic$year.start<-1976

# Year at which Generation 3 (G3) assessments started
colnames(df.basic)[10]<-"year.start.g3"
df.basic$`year.start.g3`<-2008

# Original research focus when setting up study
colnames(df.basic)[11]<-"original.focus"
df$original.focus<-"correlates and consequences of child abuse and neglect, as well as resilience in child victims."

# Criteria for parents to participate
# NOTE.todo 20231127 102. us-lls critieria for participants ask if this is a criterium or a description of the data -Most children (G2) were from two-parent households when the study began.-
colnames(df.basic)[12]<-"inclusion.criteria"

# Any important information about the sample/study that is typically provided to understand the data 
colnames(df.basic)[13]<-"additional.info"

# 6. go through dataset and code
# go through the send dataset from the top to the bottom and assign the right codes to each variable.
# NOTE developed a function through multiple rounds, but now that I try to change the code with the lasts function, it doesn't seem to work on 
# all variables

# G1 Moderators- multiple measures

# SES 
# vinc	SES - family income	g1w[1,2,etc][r]vinc
# occ	SES - occupation	g1w[1,2,etc][r][m/f/p]occ
# edu	SES - highest education level g1w[1,2,etc][r][m/f/p]edu
#NOTE.todo 20231127 102.us-lls edu SES check if fedco is mom and medco is father + which wave (presumed preschool)?

df.basic<-recode_and_rename(df.basic, df, "g1w0medu","fedcom21", 1)
df.basic<-recode_and_rename(df.basic, df, "g1w0fedu","medcom21", 1)

# vses	SES - global	g1w[1,2,etc][r]vses

df.basic<-recode_and_rename(df.basic, df, "g1w1vses","zses", 1)
head(df.basic)
colnames(df.basic)[11]<-"g1w1vses"
df.basic$g1w1ses<-df$zses

colnames(df.basic)[14]<-"g1w0vinc"
df.basic$g1w0vinc<-df$atxinc28

colnames(df.basic)[15]<-"g1w0vadd"
df.basic$g1w0vadd<-df$totrm33

# G1 sex
colnames(df.basic)[4]<-"g1w1sex"
df.basic$g1w1sex<-df$sex
describe(df.basic$g1w1sex)

# years birthdates G1 mother and father
#NOTE.request How to calculate age G1 with birthdates and info about waves
# NOTE.waves Data from parents (G1) were collected in 1976-1977, 1980-1982, and 1990-1992. 
# NOTE.waves Data from children (G2) were collected in 1990-1992, 2008-2010 and 2018-2020. 
# NOTE.waves G1 provided detailed information about G2 in earlier assessments dating back to the start of the study.  
# NOTE.waves Data from G2 about G3 (children of the original children, G2) were collected in 2008-2010 and 2019-2020. 
#NOTE.demo G1 age is calculated based on year and month
#NOTE.demo The longitudinal study began in 1976-1977 when G1 were 18 month to six years of age.

#NOTE.ID subgroups: Group Assigned at Baseline 2 and 4 represent individuals recruited from social services or welfare systems, known to 
#NOTE.ID have official reports of child abuse. All other numbers represent a community sample.

colnames(df.basic)[5]<-"w1year"
colnames(df.basic)[6]<-"w2year"
colnames(df.basic)[7]<-"w3year"
colnames(df.basic)[8]<-"v1year"
colnames(df.basic)[9]<-"v2year"
colnames(df.basic)[10]<-"start"


df.basic$w1year<-"1990-1992"
df.basic$w2year<-"2008-2010"
df.basic$w3year<-"2018-2020"

df.basic$v1year<-"2008-2010"
df.basic$v2year<-"2019-2020"

# various discipline techniques at preschool
# various loops; integrate in 1 loop?
# made loop; replace text below for loop

# CHANGE THIS TO THE NEW FUNCTION 
# 11 to 17, 29 32-46 are physical discipline

df.basic[,c(16:38)]<-df[,c(11:17,29,32:46)]

col<-16:38
newcol<-1:23


# 18  19 20 22 30 31 is psych control

df.basic[,c(39:44)]<-df[,c(18 , 19, 20, 22, 30, 31)]

col<-39:44
newcol<-1:6 # starts at 1; same length as col

# Rename each column in a loop
#this only works once. if you make a mistake, clear session and run again)

for (i in newcol) {
  old_column_name <- paste("X", col[i], sep = "")
  new_column_name <- paste("g1w1mmqupsy", i, sep = "")
  colnames(df.basic)[colnames(df.basic) == old_column_name] <- new_column_name
}

head(df.basic)[col] #check
head(df[,c(18 , 19, 20, 22, 30, 31)])

rm(i, col,newcol, new_column_name,old_column_name) # remove all objects related to loop

# 27 is consistency

df.basic[,c(45)]<-df[,27]

col<-45
newcol<-1 # starts at 1; same length as col

# Rename each column in a loop 
#this only works once. if you make a mistake, clear session and run again)
for (i in newcol) {
  old_column_name <- paste("X", col[i], sep = "")
  new_column_name <- paste("g1w1mmqucon", i, sep = "")
  colnames(df.basic)[colnames(df.basic) == old_column_name] <- new_column_name
}

names(df.basic)[col] #check

rm(i, col,newcol, new_column_name,old_column_name) # remove all objects related to loop

# 28 is rewards

df.basic[,c(46)]<-df[,28]

col<-46
newcol<-1 # starts at 1; same length as col

# Rename each column in a loop 
#this only works once. if you make a mistake, clear session and run again)
for (i in newcol) {
  old_column_name <- paste("X", col[i], sep = "")
  new_column_name <- paste("g1w1mmquppr", i, sep = "")
  colnames(df.basic)[colnames(df.basic) == old_column_name] <- new_column_name
}

names(df.basic)[col] #check

rm(i, col,newcol, new_column_name,old_column_name) # remove all objects related to loop

# 21  23 24 25 26 is other

df.basic[,47:52]<-df[,c(21,  23, 24, 25, 26)]

col<-47:52
newcol<-1:5 # starts at 1; same length as col

# Rename each column in a loop 
#this only works once. if you make a mistake, clear session and run again)
for (i in newcol) {
  old_column_name <- paste("X", col[i], sep = "")
  new_column_name <- paste("g1w1mmquoth", i, sep = "")
  colnames(df.basic)[colnames(df.basic) == old_column_name] <- new_column_name
}

names(df.basic)[col] #check

rm(i, col,newcol, new_column_name,old_column_name) # remove all objects related to loop

####
# 47 to 82 = father discipline

#  47:53, 65, 68:82 are physical discipline

df.basic[,53:75]<-df[,c(47:53, 65, 68:82)]

col<-53:75
newcol<-1:23 # starts at 1; same length as col

# Rename each column in a loop 
#this only works once. if you make a mistake, clear session and run again)
for (i in newcol) {
  old_column_name <- paste("X", col[i], sep = "")
  new_column_name <- paste("g1w1ffqupun", i, sep = "")
  colnames(df.basic)[colnames(df.basic) == old_column_name] <- new_column_name
}

names(df.basic)[col] #check

rm(i, col,newcol, new_column_name,old_column_name) # remove all objects related to loop

# 54,55,56,58,66,67 is psychologial

df.basic[,76:81]<-df[,c(54,55,56,58,66,67)]

col<-76:81
newcol<-1:6 # starts at 1; same length as col

# Rename each column in a loop 
#this only works once. if you make a mistake, clear session and run again)
for (i in newcol) {
  old_column_name <- paste("X", col[i], sep = "")
  new_column_name <- paste("g1w1ffqupsy", i, sep = "")
  colnames(df.basic)[colnames(df.basic) == old_column_name] <- new_column_name
}

names(df.basic)[col] #check

rm(i, col,newcol, new_column_name,old_column_name) # remove all objects related to loop

# 57 59-62 is other

df.basic[,82:86]<-df[,c(57, 59:62)]

col<-82:86
newcol<-1:5 # starts at 1; same length as col

# Rename each column in a loop 
#this only works once. if you make a mistake, clear session and run again)
for (i in newcol) {
  old_column_name <- paste("X", col[i], sep = "")
  new_column_name <- paste("g1w1ffquoth", i, sep = "")
  colnames(df.basic)[colnames(df.basic) == old_column_name] <- new_column_name
}

names(df.basic)[col] #check

rm(i, col,newcol, new_column_name,old_column_name) # remove all objects related to loop

# 63 is consistency

df.basic[,87]<-df[,c(63)]

col<-87
newcol<-1 # starts at 1; same length as col

# Rename each column in a loop 
#this only works once. if you make a mistake, clear session and run again)
for (i in newcol) {
  old_column_name <- paste("X", col[i], sep = "")
  new_column_name <- paste("g1w1ffcon", i, sep = "")
  colnames(df.basic)[colnames(df.basic) == old_column_name] <- new_column_name
}

names(df.basic)[col] #check

rm(i, col,newcol, new_column_name,old_column_name) # remove all objects related to loop

# 64 is rewards

df.basic[,88]<-df[,c(64)]

col<-88
newcol<-1 # starts at 1; same length as col

# Rename each column in a loop 
#this only works once. if you make a mistake, clear session and run again)
for (i in newcol) {
  old_column_name <- paste("X", col[i], sep = "")
  new_column_name <- paste("g1w1ffppr", i, sep = "")
  colnames(df.basic)[colnames(df.basic) == old_column_name] <- new_column_name
}

names(df.basic)[col] #check

rm(i, col,newcol, new_column_name,old_column_name) # remove all objects related to loop

# G1 displine preschool longer than 3 months ago

#mother
# item 83 autonomy support
# item 84,86,89 psychological control
# item 85 87, other
# item 88 cons
# item 90:104 physical pun

df.basic<-recode_and_rename(89,83,"g1w1mmaut") #NOTE.todo think about how to continue counting
df.basic<-recode_and_rename(c(90,91,92),c(84,86,89),"g1w1mmpsy") #NOTE.todo think about how to continue counting
df.basic<-recode_and_rename(c(93,94),c(85,87),"g1w1mmoth") #NOTE.todo think about how to continue counting
df.basic<-recode_and_rename(c(95),c(88),"g1w1mmcon") #NOTE.todo think about how to continue counting
df.basic<-recode_and_rename(96:110,90:104,"g1w1mmpun") #NOTE.todo think about how to continue counting

#father
# item 105 autonomy support
# item 106,108,111 psychological control
# item 107,109 other
# item 110 cons
# item 112:126 physical pun

df.basic<-recode_and_rename(111,105,"g1w1ffaut") #NOTE.todo think about how to continue counting
df.basic<-recode_and_rename(c(112,113,114),c(106,108,111),"g1w1ffpsy") #NOTE.todo think about how to continue counting
df.basic<-recode_and_rename(c(115,116),c(107,109),"g1w1ffoth") #NOTE.todo think about how to continue counting
df.basic<-recode_and_rename(c(117),c(110),"g1w1ffcon") #NOTE.todo think about how to continue counting
df.basic<-recode_and_rename(118:132,112:126,"g1w1ffpun") #NOTE.todo think about how to continue counting

# Mother & Father Discipline Practice in School-age, last three months
# Note.todo check whether these items concern last 3 months or last year

# mother wave 2
# item 134,135,136,138 psychological control
# item 137,139:142 other
# item  143 cons
# item 127:133, 144:161  physical pun

df.basic<-recode_and_rename(132:135,c(134,135,136,138),"g1w2mmpsy") #NOTE.todo think about how to continue counting
df.basic<-recode_and_rename(136:140,c(137,139:142),"g1w2mmoth") #NOTE.todo think about how to continue counting
df.basic<-recode_and_rename(141,143,"g1w2mmcon") #NOTE.todo think about how to continue counting
df.basic<-recode_and_rename(144:168,c(127:133, 144:161),"g1w2mmpun") #NOTE.todo think about how to continue counting

# father wave 2
# item 169:171,174 , psychological control
# item 172,175:178 other
# item  179 cons
# item 162:168, 180:197  physical pun

df.basic<-recode_and_rename(168:171,c(169:171,174),"g1w2ffpsy") #NOTE.todo think about how to continue counting
df.basic<-recode_and_rename(171:175,c(172,175:178),"g1w2ffoth") #NOTE.todo think about how to continue counting
df.basic<-recode_and_rename(176,179,"g1w2ffcon") #NOTE.todo think about how to continue counting
df.basic<-recode_and_rename(180:204,c(162:168, 180:197),"g1w2ffpun") #NOTE.todo think about how to continue counting

head(df.basic)[168:204]

########  
#Gen 2-3
######## 

# GP
# NOTES.todo ask what GP line 198 means

# Age of G2 

df.basic<-recode_and_rename_3(df.basic,df,"g2v1c2age","A_AGE",1)

# date of birth
#NOTE.to do us-lls 22-11-2023 calculate age per wave with DOB

# ethnicity
#NOTE.to do us-lls 22-11-2023 check if G2 ethnicity is  from wave 1

df.basic<-recode_and_rename_3(df.basic,df,"g2v1c2eth",c("A_ETHGRP", "A_ETHCAT", "A_ETHRACE"),1)

# G2 relationship status
#NOTE.todo us-lls 22-11-2023 check if G2 marital status is  from wave 1

df.basic<-recode_and_rename_3(df.basic,df,"g2v1c2rst",c("A_LVMAR", "A_LVMTIMS", "A_LVCURSP","B_PSBG", "B_PSCOMMIT", "B_PSLIV"),1)

# n G3 
#NOTE.todo us-lls 22-11-2023 check if G3 n children is  from wave 1

df.basic<-recode_and_rename_3(df.basic,df,"g2v1c2nch",c("A_CHNUM"),1)

# involvement
#NOTE.todo us-lls 22-11-2023 check if living with children is from wave 1

df.basic<-recode_and_rename(df.basic,df,"g2v1c2inv",c("A_LVCURCH", "A_LVOTHCH", "A_LVOTHNB","NOCONTACT",
                                                        "A_LVCHPY1","A_LVCHPY2","A_LVCHPY3","A_LVCHPY4","A_LVCHPY5",
                                                        "A_LVCHPY6","A_LVCHPC1","A_LVCHPC2","A_LVCHPC3","A_LVCHPC4",
                                                        "A_LVCHPC5","A_LVCHPC6","A_LVCHCON1","A_LVCHCON2","A_LVCHCON3",
                                                        "A_LVCHCON4","A_LVCHCON5","A_LVCHCON6","A_LVCHTIMA1","A_LVCHTIMA2",
                                                        "A_LVCHTIMA3","A_LVCHTIMA4","A_LVCHTIMA5","A_LVCHTIMA6","A_LVCHTIM1",
                                                        "A_LVCHTIM2","A_LVCHTIM3","A_LVCHTIM4","A_LVCHTIM5","A_LVCHTIM6"),1)

# type of G2 caregiver
#NOTE.todo us-lls 22-11-2023 check if G2 type of parent is from wave 1 and whether is involves all people in the household as opposed to only the children
# Note.todo us-lls 22-11-2023. G2 type of caregiver if this study only focusses on biological children, select only those cases in which household member is biological child
df.basic<-recode_and_rename_3(df.basic,df,"g2v1c2car",c("A_LVBIO1", "A_LVBIO2", "A_LVBIO3", "A_LVBIO4", "A_LVBIO5", "A_LVBIO6", "A_LVBIO7", "A_LVBIO8", "A_LVBIO9", "A_LVBIO10", "A_LVBIO11",
                                                        "A_LVCHBIO1","A_LVCHBIO2","A_LVCHBIO3","A_LVCHBIO4","A_LVCHBIO5","A_LVCHBIO6"),1)


# gender G3 
#NOTE.todo us-lls 22-11-2023 check if G3 gender is from wave 1 and whether is involves all people in the household as opposed to only the children
# Note.todo us-lls 22-11-2023. G3 gender if this study only focusses on biological children, select only those cases in which household member is biological child
df.basic<-recode_and_rename_3(df.basic,df,"g3v1c3gen",c("A_LVGNDR1", "A_LVGNDR2", "A_LVGNDR3", "A_LVGNDR4", "A_LVGNDR5", "A_LVGNDR6", "A_LVGNDR7", "A_LVGNDR8", "A_LVGNDR9", "A_LVGNDR10", "A_LVGNDR11",
                                                        "A_LVCHGDR1","A_LVCHGDR2","A_LVCHGDR3","A_LVCHGDR4","A_LVCHGDR5","A_LVCHGDR6"),1)

# Age G3
# #NOTE.todo us-lls 22-11-2023 check if G3 age is from wave 1 and whether is involves all people in the household as opposed to only the children
# Note.todo us-lls 22-11-2023. G3 age if this study only focusses on biological children, select only those cases in which household member is biological child

df.basic<-recode_and_rename(df.basic,df,"g3v1c3age",c("A_LVAGE1","A_LVAGE2","A_LVAGE3","A_LVAGE4","A_LVAGE5","A_LVAGE6","A_LVAGE7","A_LVAGE8","A_LVAGE9","A_LVAGE10","A_LVAGE11"),1)

# G2 relationship stability
# #NOTE.todo us-lls 22-11-2023  G2 relationship stability check which wave this is measured
df.basic<-recode_and_rename(df.basic,df,"g2v1c2res", c("B_PSBGMO", "B_PSINF", "B_PSIMP" ),1)

# G2 romantic relationship quality
# #NOTE.todo us-lls 22-11-2023 G2 relationship quality check which wave this is measured

df.basic<-recode_and_rename(df.basic,df,"g2v1c2req", c("B_PSLIKE","B_PSACT","B_PSWRO","B_PSWROTIM","B_PSAFF","B_PSSUP",
                                                       "B_PSLOY","B_PSSTR","B_PSSATSEX","B_PSSAT","B_SD","B_SDCHIL","B_SD_ARG",
                                                       "B_SD_DIS","B_SD_YOUDIS","B_SD_OTHDIS","B_SD_YOUEMB","B_SD_OTHEMB","B_SD_YOUCON",
                                                       "B_SD_OTHCON","B_SD_YOUCHG","B_SD_OTHCHG","B_SD_YOUSER","B_SD_OTHSER","B_SD_YOUAV",
                                                       "B_SD_OTHAV","B_SD_YOUGU","B_SD_OTHGU","B_SD_YOUREF","B_SD_OTHREF","B_SD_YOUTK",
                                                       "B_SD_OTHTK","B_SD_YOUWD","B_SD_OTHWD","B_SD_YOUCH","B_SD_OTHCH","B_SD_YOUTHR",
                                                       "B_SD_OTHTHR","B_SD_YOUWAL","B_SD_OTHWAL",
                                                       "H_RES_SP","H_PIS_SP","H_RIS_SP","H_SPS_CARE","H_SPS_UND","H_SPS_RELY","H_SPS_TALK","H_SPS_RELX",
                                                       "S_PV_ICARE","S_PV_PCARE","S_PV_IEXPL","S_PV_PEXPL","S_PV_ISWR","S_PV_PSWR","S_PV_ITHRW","S_PV_PTHRW",
                                                       "S_PV_ITWST","S_PV_PTWST","S_PV_IBRU","S_PV_PBRU","S_PV_IRESP","S_PV_PRESP","S_PV_ISXNOC","S_PV_PSXNOC",
                                                       "S_PV_ISHOV","S_PV_PSHOV","S_PV_IWEAP","S_PV_PWEAP","S_PV_IPASS","S_PV_PPASS","S_PV_ICALNM",
                                                       "S_PV_PCALNM","S_PV_IPUNCH","S_PV_PPUNCH","S_PV_IDEST","S_PV_PDEST","S_PV_IGODR","S_PV_PGODR","S_PV_ICHOK",
                                                       "S_PV_PCHOK","S_PV_ISHOUT","S_PV_PSHOUT","S_PV_ISLAM","S_PV_PSLAM","S_PV_ISAYWK","S_PV_PSAYWK","S_PV_INDDR",
                                                       "S_PV_PNDDR","S_PV_IBEAT","S_PV_PBEAT","S_PV_IGRAB","S_PV_PGRAB","S_PV_IFORSX","S_PV_PFORSX","S_PV_IFOASX",
                                                       "S_PV_PFOASX","S_PV_ISTMP","S_PV_PSTMP","S_PV_ISLAP","S_PV_PSLAP","S_PV_IBRKBN","S_PV_PBRKBN","S_PV_ITHRSX",
                                                       "S_PV_PTHRSX","S_PV_ICOMP","S_PV_PCOMP","S_PV_IBURN","S_PV_PBURN","S_PV_IINSSX","S_PV_PINSSX","S_PV_INOASK",
                                                       "S_PV_PNOASK","S_PV_IACCUS","S_PV_PACCUS","S_PV_ISPITE","S_PV_PSPITE","S_PV_ITHRHT","S_PV_PTHRHT","S_PV_IHRTND",
                                                       "S_PV_PHRTND","S_PV_IKICK","S_PV_PKICK","S_PV_ITHRSX2","S_PV_PTHRSX2","S_PV_IAGREE","S_PV_PAGREE","S_PVW_UNSF",
                                                       "S_PVW_ASHM","S_PVW_NORK","S_PVW_PROG","S_PVW_PRIS","S_PVW_NOPW","S_PVW_HIDTR","S_PVW_OWND","S_PVW_SCARE",
                                                       "S_PVW_LOOK"),1)

# G2 partner education
# #NOTE.todo us-lls 22-11-2023 G2 partner education check which wave this is measured
# G2  education
df.basic<-recode_and_rename(df.basic,df,"g2v1r2edu", c("C_PCSCHCM", "W_SCHLVCOM"),1)

# G2  occupation
df.basic<-recode_and_rename(df.basic,df,"g2v1r2occ", c("X_EMPCUR"),1)

# G2 income
df.basic<-recode_and_rename(df.basic,df,"g2v1vinc", c("Y_FINEARIN", "Y_FIN"),1)

# G2 partner work
# #NOTE.todo us-lls 22-11-2023 G2 partner work check which wave this is measured
df.basic<-recode_and_rename(df.basic,df,g2v1r2occ, c("C_PCEMP"),1)

# G2 parenting
# #NOTE.todo us-lls 22-11-2023 G2 parenting check which wave this is measured

# str<-Parenting stress
# pse<-parental self-esteem

df.basic<-recode_and_rename(df.basic,df,"g2v1g2qupse", c("I_RECH_OPP", "I_RECH_NEG", "I_RECH_SHAM", "I_RECH_PROV",
                                                             "I_RECH_PRID"),1)
# pcs<-parent-child conflicts
df.basic<-recode_and_rename(df.basic,df,"g2v1g2qupcs", c("I_CHSTR"),1)

# pcc<-parent-child closeness
# ppr<-praise/positive reinforcement
# paw<-positive affect/warmth 
df.basic<-recode_and_rename(df.basic,df,"g2v1g2qupaw", c("H_PES_CHIL","H_PIS_CHIL", "I_CHENJOY", "I_CHSAT"),1)

# hos<-hostility and aggression
# inv<-involvement (or indifference/neglect)
# sen<-sensitivity
# aut<-autonomy support
# psy<-psychological control
df.basic<-recode_and_rename(df.basic,df,"g2v1g2qupsy", c("Q__YELL","Q__RIDI","Q__EMB","Q__STUP","Q__SARC",
                                                             "Q__NOTK","Q__THLV","Q__THAW","Q__GILT","Q__SCREAM",
                                                             "Q__FYELL1","Q__FYELL2","Q__FYELL3","Q__FYELL4",
                                                             "Q__FYELL5","Q__FYELL6","Q__FYELL7","Q__FYELL8",
                                                             "Q__FYELL9","Q__FRIDI1","Q__FRIDI2","Q__FRIDI3",
                                                             "Q__FRIDI4","Q__FRIDI5","Q__FRIDI6","Q__FRIDI7",
                                                             "Q__FRIDI8","Q__FRIDI9","Q__FEMB1","Q__FEMB2",
                                                             "Q__FEMB3","Q__FEMB4","Q__FEMB5","Q__FEMB6",
                                                             "Q__FEMB7","Q__FEMB8","Q__FEMB9","Q__FSTUP1",
                                                             "Q__FSTUP2","Q__FSTUP3","Q__FSTUP4","Q__FSTUP5",
                                                             "Q__FSTUP6","Q__FSTUP7","Q__FSTUP8","Q__FSTUP9",
                                                             "Q__FSARC1","Q__FSARC2","Q__FSARC3","Q__FSARC4",
                                                             "Q__FSARC5","Q__FSARC6","Q__FSARC7","Q__FSARC8",
                                                             "Q__FSARC9","Q__FNOTK1","Q__FNOTK2","Q__FNOTK3",
                                                             "Q__FNOTK4","Q__FNOTK5","Q__FNOTK6","Q__FNOTK7",
                                                             "Q__FNOTK8","Q__FNOTK9","Q__FTHLV1","Q__FTHLV2",
                                                             "Q__FTHLV3","Q__FTHLV4","Q__FTHLV5","Q__FTHLV6",
                                                             "Q__FTHLV7","Q__FTHLV8","Q__FTHLV9","Q__FTHAW1",
                                                             "Q__FTHAW2","Q__FTHAW3","Q__FTHAW4","Q__FTHAW5",
                                                             "Q__FTHAW6","Q__FTHAW7","Q__FTHAW8","Q__FTHAW9",
                                                             "Q__FGILT1","Q__FGILT2","Q__FGILT3","Q__FGILT4",
                                                              "Q__FGILT5","Q__FGILT6","Q__FGILT7","Q__FGILT8",
                                                             "Q__FGILT9","Q__SCREAM1","Q__SCREAM2","Q__SCREAM3",
                                                             "Q__SCREAM4","Q__SCREAM5","Q__SCREAM6","Q__SCREAM7",
                                                             "Q__SCREAM8","Q__SCREAM9"),1)
# pun<-physical punishment
df.basic<-recode_and_rename(df.basic,df,"g2v1g2qupun", c("Q_CHFM","Q_ROFM","Q_CHHR","Q_RMHR","Q_DKRM","Q_GRND",
                                                             "Q_LKOU","Q_TKML","Q_THRT","Q_THWP","Q_SOAP","Q_BITE",
                                                             "Q_BTBR","Q_PEPR","Q_SPNK","Q_SLP","Q_SLPH","Q_SLPB",
                                                             "Q_SHAK","Q_PLHR","Q_PADL","Q_BELT","Q_PADB","Q_BRN",
                                                             "Q_BRNM","Q_FCHFM1","Q_FCHFM2","Q_FCHFM3","Q_FCHFM4",
                                                             "Q_FCHFM5","Q_FCHFM6","Q_FCHFM7","Q_FCHFM8","Q_FCHFM9",
                                                             "Q_FROFM1","Q_FROFM2","Q_FROFM3","Q_FROFM4","Q_FROFM5",
                                                             "Q_FROFM6","Q_FROFM7","Q_FROFM8","Q_FROFM9","Q_FCHHR1",
                                                             "Q_FCHHR2","Q_FCHHR3","Q_FCHHR4","Q_FCHHR5","Q_FCHHR6",
                                                             "Q_FCHHR7","Q_FCHHR8","Q_FCHHR9","Q_FRMHR1","Q_FRMHR2",
                                                             "Q_FRMHR3","Q_FRMHR4","Q_FRMHR5","Q_FRMHR6","Q_FRMHR7",
                                                             "Q_FRMHR8","Q_FRMHR9","Q_FDKRM1","Q_FDKRM2","Q_FDKRM3",
                                                             "Q_FDKRM4","Q_FDKRM5","Q_FDKRM6","Q_FDKRM7","Q_FDKRM8",
                                                             "Q_FDKRM9","Q_FGRND1","Q_FGRND2","Q_FGRND3","Q_FGRND4",
                                                             "Q_FGRND5","Q_FGRND6","Q_FGRND7","Q_FGRND8","Q_FGRND9",
                                                             "Q_FLKOU1","Q_FLKOU2","Q_FLKOU3","Q_FLKOU4","Q_FLKOU5",
                                                             "Q_FLKOU6","Q_FLKOU7","Q_FLKOU8","Q_FLKOU9","Q_FTHRT1",
                                                             "Q_FTHRT2","Q_FTHRT3","Q_FTHRT4","Q_FTHRT5","Q_FTHRT6",
                                                             "Q_FTHRT7","Q_FTHRT8","Q_FTHRT9","Q_FTHWP1","Q_FTHWP2",
                                                             "Q_FTHWP3","Q_FTHWP4","Q_FTHWP5","Q_FTHWP6","Q_FTHWP7",
                                                             "Q_FTHWP8","Q_FTHWP9","Q_FSOAP1","Q_FSOAP2","Q_FSOAP3",
                                                             "Q_FSOAP4","Q_FSOAP5","Q_FSOAP6","Q_FSOAP7","Q_FSOAP8",
                                                             "Q_FSOAP9","Q_FBITE1","Q_FBITE2","Q_FBITE3","Q_FBITE4",
                                                             "Q_FBITE5","Q_FBITE6","Q_FBITE7","Q_FBITE8","Q_FBITE9",
                                                             "Q_FBTBR1","Q_FBTBR2","Q_FBTBR3","Q_FBTBR4","Q_FBTBR5",
                                                             "Q_FBTBR6","Q_FBTBR7","Q_FBTBR8","Q_FPEPR1","Q_FPEPR2",
                                                             "Q_FPEPR3","Q_FPEPR4","Q_FPEPR5","Q_FPEPR6","Q_FPEPR7",
                                                             "Q_FPEPR8","Q_FPEPR9","Q_FSPNK1","Q_FSPNK2","Q_FSPNK3",
                                                             "Q_FSPNK4","Q_FSPNK5","Q_FSPNK6","Q_FSPNK7","Q_FSPNK8",
                                                             "Q_FSPNK9","Q_FSLP1","Q_FSLP2","Q_FSLP3","Q_FSLP4","Q_FSLP5",
                                                             "Q_FSLP6","Q_FSLP7","Q_FSLP8","Q_FSLP9","Q_FSLPH1","Q_FSLPH2",
                                                             "Q_FSLPH3","Q_FSLPH4","Q_FSLPH5","Q_FSLPH6","Q_FSLPH7","Q_FSLPH8"
                                                             ,"Q_FSLPH9","Q_FSLPB1","Q_FSLPB2","Q_FSLPB3","Q_FSLPB4","Q_FSLPB5",
                                                             "Q_FSLPB6","Q_FSLPB7","Q_FSLPB8","Q_FSLPB9","Q_FSHAK1","Q_FSHAK2",
                                                             "Q_FSHAK3","Q_FSHAK4","Q_FSHAK5","Q_FSHAK6","Q_FSHAK7","Q_FSHAK8",
                                                             "Q_FSHAK9","Q_FPLHR1","Q_FPLHR2","Q_FPLHR3","Q_FPLHR4","Q_FPLHR5",
                                                             "Q_FPLHR6","Q_FPLHR7","Q_FPLHR8","Q_FPLHR9","Q_FPADL1","Q_FPADL2",
                                                             "Q_FPADL3","Q_FPADL4","Q_FPADL5","Q_FPADL6","Q_FPADL7","Q_FPADL8",
                                                             "Q_FPADL9","Q_FBELT1","Q_FBELT2","Q_FBELT3","Q_FBELT4","Q_FBELT5",
                                                             "Q_FBELT6","Q_FBELT7","Q_FBELT8","Q_FBELT9","Q_FPADB1","Q_FPADB2",
                                                             "Q_FPADB3","Q_FPADB4","Q_FPADB5","Q_FPADB6","Q_FPADB7","Q_FPADB8",
                                                             "Q_FPADB9","Q_FBRN1","Q_FBRN2","Q_FBRN3","Q_FBRN4","Q_FBRN5","Q_FBRN6",
                                                             "Q_FBRN7","Q_FBRN8","Q_FBRN9","Q_FBRNM1","Q_FBRNM2","Q_FBRNM3",
                                                             "Q_FBRNM4","Q_FBRNM5","Q_FBRNM6","Q_FBRNM7","Q_FBRNM8","Q_FBRNM9"),1)


# mon<-monitoring/

# con<-consistency in setting rules 
df.basic<-recode_and_rename(df.basic,df,"g2v1g2qucon", c("Q_IGNO","Q_REAS","Q_FIGNO1","Q_FIGNO2","Q_FIGNO3",
                                                             "Q_FIGNO4","Q_FIGNO5","Q_FIGNO6","Q_FIGNO7","Q_FIGNO8",
                                                             "Q_FIGNO9","Q_FREAS1","Q_FREAS2","Q_FREAS3","Q_FREAS4",
                                                             "Q_FREAS5","Q_FREAS6","Q_FREAS7","Q_FREAS8","Q_FREAS9"),1)

# poth<- other parenting 
df.basic<-recode_and_rename(df.basic,df,"g2v1g2qupoth", c("Q_HUM","Q_TKDS","Q_TKTY","Q_TKPR","Q_FHUM1","Q_FHUM2",
                                                            "Q_FHUM3","Q_FHUM4","Q_FHUM5","Q_FHUM6","Q_FHUM7",
                                                            "Q_FHUM8","Q_FHUM9","Q_FTKDS1","Q_FTKDS2","Q_FTKDS3",
                                                            "Q_FTKDS4","Q_FTKDS5","Q_FTKDS6","Q_FTKDS7","Q_FTKDS8",
                                                            "Q_FTKDS9","Q_FTKML1","Q_FTKML2","Q_FTKML3","Q_FTKML4",
                                                            "Q_FTKML5","Q_FTKML6","Q_FTKML7","Q_FTKML8","Q_FTKML9",
                                                            "Q_FTKTY1","Q_FTKTY2","Q_FTKTY3","Q_FTKTY4","Q_FTKTY5",
                                                            "Q_FTKTY6","Q_FTKTY7","Q_FTKTY8","Q_FTKTY9","Q_FTKPR1",
                                                            "Q_FTKPR2","Q_FTKPR3","Q_FTKPR4","Q_FTKPR5","Q_FTKPR6",
                                                            "Q_FTKPR7","Q_FTKPR8","Q_FTKPR9"
),1)

# G2 substance use g2v[1,2,etc][r]c2alc & g2v[1,2,etc][r]c2dru g2v[1,2,etc][r]c2qusub
# #NOTE.todo us-lls 22-11-2023 G2 substance use check which wave this is measured

# G2 alcohol use
df.basic<-recode_and_rename(df.basic,df,"g2v2c2qualc", c("M_SUBALC", "M_SUBALCYR","M_SUBALCYR1","M_SUBWKDRK","M_SUBWKNDR",
                                                           "M_SUBBINGE","M_SUBALCMO","M_SUBINMO","M_SUBDRUNK"),1)

# G2 drug use
df.basic<-recode_and_rename(df.basic,df,"g2v2c2qudru", c("M_SUBMJ","M_SUBMJYR","M_SUBMJTM","M_SUBMJAMT","M_SUBMJMO",
                                                           "M_SCR_CRWL", "M_SCR_INJUR", "M_SCR_NEEDL"),1)

# G2 substance use
df.basic<-recode_and_rename(df.basic,df,"g2v2c2qusub",c("M_SCRUSAL","M_SCR_TOMU","M_SCR_CUT","M_SCR_HELP","M_SCR_HLTH",
                                            "M_SCR_HEAD","M_SCR_WD","M_SCR_FAM","M_SCR_WRK","M_SCR_FGHT",
                                            "M_SCR_MORE","M_SCR_THINK","M_SCR_ACT","M_SCR_GUILT","M_SCR2_PROB",
                                            "M_SCR2_PRBNW"
),1)






# Make a file with excluded variables
# NOTE.todo 2023-11-24 check if this function works well; seems to leave some columns open?

df.excluded<-exclude(df,c("C_PC_REL","C_PC_SCH","C_PC_VOL","C_PC_POL","C_PC_ACT","C_PC_SMOK","C_PC_ALC",
                          "C_PC_MARIJ","C_PC_DRUG","C_PC_FIGHT","C_PC_CRIME","C_PCARR","C_PCARRPY",
                          "H_RES_CHIL","H_RIS_CHIL","M_SCR_BLACKv ","M_SCR_DTS","M_SCR_LIVER","M_SCR_ARR",
                          "M_SCR2_FAMPR","Q_SPDIS", "Q_CHFMSP","Q_ROFMSP","Q_CHHRSP","Q_RMHRSP","Q_DKRMSP",
                          "Q_GRNDSP","Q_LKOUSP","Q_YELLSP","Q_RIDISP","Q_EMBSP","Q_STUPSP","Q_HUMSP","Q_SARCSP",
                          "Q_IGNOSP","Q_NOTKSP","Q_TKDSSP","Q_TKMLSP","Q_TKTYSP","Q_TKPRSP","Q_REASSP","Q_THRTSP",
                          "Q_THLVSP","Q_THAWSP","Q_THWPSP","Q_SOAPSP","Q_BITESP","Q_BTBRSP","Q_PEPRSP","Q_SPNKSP",
                          "Q_SLPSP","Q_SLPHSP","Q_SLPB","Q_SLPBSP","Q_SHAKSP","Q_PLHRSP","Q_PADLSP","Q_BELTSP",
                          "Q_PADBSP","Q_BRNSP","Q_BRNMSP","Q_GILTSP","Q_SCREAMSP","Q_FCHFMSP1","Q_FCHFMSP2",
                          "Q_FCHFMSP3","Q_FCHFMSP4","Q_FCHFMSP5","Q_FCHFMSP6","Q_FCHFMSP7","Q_FCHFMSP8",
                          "Q_FCHFMSP9","Q_FROFMSP1","Q_FROFMSP2","Q_FROFMSP3","Q_FROFMSP4","Q_FROFMSP5",
                          "Q_FROFMSP6","Q_FROFMSP7","Q_FROFMSP8","Q_FROFMSP9","Q_FCHHRSP1","Q_FCHHRSP2",
                          "Q_FCHHRSP3","Q_FCHHRSP4","Q_FCHHRSP5","Q_FCHHRSP6","Q_FCHHRSP7","Q_FCHHRSP8",
                          "Q_FCHHRSP9","Q_FRMHRSP1","Q_FRMHRSP2","Q_FRMHRSP3","Q_FRMHRSP4","Q_FRMHRSP5",
                          "Q_FRMHRSP6","Q_FRMHRSP7","Q_FRMHRSP8","Q_FRMHRSP9","Q_FDKRMSP1","Q_FDKRMSP2",
                          "Q_FDKRMSP3","Q_FDKRMSP4","Q_FDKRMSP5","Q_FDKRMSP6","Q_FDKRMSP7","Q_FDKRMSP8",
                          "Q_FDKRMSP9","Q_FGRNDSP1","Q_FGRNDSP2","Q_FGRNDSP3","Q_FGRNDSP4","Q_FGRNDSP5",
                          "Q_FGRNDSP6","Q_FGRNDSP7","Q_FGRNDSP8","Q_FGRNDSP9","Q_FLKOUSP1","Q_FLKOUSP2",
                          "Q_FLKOUSP3","Q_FLKOUSP4","Q_FLKOUSP5","Q_FLKOUSP6","Q_FLKOUSP7","Q_FLKOUSP8",
                          "Q_FLKOUSP9","Q_FYELLSP1","Q_FYELLSP2","Q_FYELLSP3","Q_FYELLSP4","Q_FYELLSP5",
                          "Q_FYELLSP6","Q_FYELLSP7","Q_FYELLSP8","Q_FYELLSP9","Q_FRIDISP1","Q_FRIDISP2",
                          "Q_FRIDISP3","Q_FRIDISP4","Q_FRIDISP5","Q_FRIDISP6","Q_FRIDISP7","Q_FRIDISP8",
                          "Q_FRIDISP9","Q_FEMBSP1","Q_FEMBSP2","Q_FEMBSP3","Q_FEMBSP4","Q_FEMBSP5","Q_FEMBSP6",
                          "Q_FEMBSP7","Q_FEMBSP8","Q_FEMBSP9","Q_FSTUPSP1","Q_FSTUPSP2","Q_FSTUPSP3","Q_FSTUPSP4",
                          "Q_FSTUPSP5","Q_FSTUPSP6","Q_FSTUPSP7","Q_FSTUPSP8","Q_FSTUPSP9","Q_FHUMSP1","Q_FHUMSP2",
                          "Q_FHUMSP3","Q_FHUMSP4","Q_FHUMSP5","Q_FHUMSP6","Q_FHUMSP7","Q_FHUMSP8","Q_FHUMSP9",
                          "Q_FSARCSP1","Q_FSARCSP2","Q_FSARCSP3","Q_FSARCSP4","Q_FSARCSP5","Q_FSARCSP6",
                          "Q_FSARCSP7","Q_FSARCSP8","Q_FSARCSP9","Q_FIGNOSP1","Q_FIGNOSP2","Q_FIGNOSP3","Q_FIGNOSP4",
                          "Q_FIGNOSP5","Q_FIGNOSP6","Q_FIGNOSP7","Q_FIGNOSP8","Q_FIGNOSP9","Q_FTKDSSP1","Q_FTKDSSP2",
                          "Q_FTKDSSP3","Q_FTKDSSP4","Q_FTKDSSP5","Q_FTKDSSP6","Q_FTKDSSP7","Q_FTKDSSP8","Q_FTKDSSP9",
                          "Q_FTKMLSP1","Q_FTKMLSP2","Q_FTKMLSP3","Q_FTKMLSP4","Q_FTKMLSP5","Q_FTKMLSP6","Q_FTKMLSP7",
                          "Q_FTKMLSP8","Q_FTKMLSP9","Q_FTKTYSP1","Q_FTKTYSP2","Q_FTKTYSP3","Q_FTKTYSP4","Q_FTKTYSP5",
                          "Q_FTKTYSP6","Q_FTKTYSP7","Q_FTKTYSP8","Q_FTKTYSP9","Q_FTKPRSP1","Q_FTKPRSP2","Q_FTKPRSP3",
                          "Q_FTKPRSP4","Q_FTKPRSP5","Q_FTKPRSP6","Q_FTKPRSP7","Q_FTKPRSP8","Q_FTKPRSP9","Q_FREASSP1",
                          "Q_FREASSP2","Q_FREASSP3","Q_FREASSP4","Q_FREASSP5","Q_FREASSP6","Q_FREASSP7","Q_FREASSP8",
                          "Q_FREASSP9","Q_FTHRTSP1","Q_FTHRTSP2","Q_FTHRTSP3","Q_FTHRTSP4","Q_FTHRTSP5","Q_FTHRTSP6",
                          "Q_FTHRTSP7","Q_FTHRTSP8","Q_FTHRTSP9","Q_FTHLVSP1","Q_FTHLVSP2","Q_FTHLVSP3","Q_FTHLVSP4",
                          "Q_FTHLVSP5","Q_FTHLVSP6","Q_FTHLVSP7","Q_FTHLVSP8","Q_FTHLVSP9","Q_FTHWPSP1","Q_FTHWPSP2",
                          "Q_FTHWPSP3","Q_FTHWPSP4","Q_FTHWPSP5","Q_FTHWPSP6","Q_FTHWPSP7","Q_FTHWPSP8","Q_FTHWPSP9",
                          "Q_FTHAWSP1","Q_FTHAWSP2","Q_FTHAWSP3","Q_FTHAWSP4","Q_FTHAWSP5","Q_FTHAWSP6","Q_FTHAWSP7",
                          "Q_FTHAWSP8","Q_FTHAWSP9","Q_FTHWPSP1","Q_FTHWPSP2","Q_FTHWPSP3","Q_FTHWPSP4","Q_FTHWPSP5",
                          "Q_FTHWPSP6","Q_FTHWPSP7","Q_FTHWPSP8","Q_FTHWPSP9","Q_FBITESP1","Q_FBITESP2","Q_FBITESP3",
                          "Q_FBITESP4","Q_FBITESP5","Q_FBITESP6","Q_FBITESP7","Q_FBITESP8","Q_FBITESP9","Q_FBTBRSP1",
                          "Q_FBTBRSP2","Q_FBTBRSP3","Q_FBTBRSP4","Q_FBTBRSP5","Q_FBTBRSP6","Q_FBTBRSP7","Q_FBTBRSP8",
                          "Q_FBTBRSP9","Q_FPEPRSP1","Q_FPEPRSP2","Q_FPEPRSP3","Q_FPEPRSP4","Q_FPEPRSP5","Q_FPEPRSP6",
                          "Q_FPEPRSP7","Q_FPEPRSP8","Q_FPEPRSP9","Q_FSPNKSP1","Q_FSPNKSP2","Q_FSPNKSP3","Q_FSPNKSP4",
                          "Q_FSPNKSP5","Q_FSPNKSP6","Q_FSPNKSP7","Q_FSPNKSP8","Q_FSPNKSP9","Q_FSLPSP1","Q_FSLPSP2",
                          "Q_FSLPSP3","Q_FSLPSP4","Q_FSLPSP5","Q_FSLPSP6","Q_FSLPSP7","Q_FSLPSP8","Q_FSLPSP9","Q_FSLPHSP1",
                          "Q_FSLPHSP2","Q_FSLPHSP3","Q_FSLPHSP4","Q_FSLPHSP5","Q_FSLPHSP6","Q_FSLPHSP7","Q_FSLPHSP8",
                          "Q_FSLPHSP9","Q_FSLPBSP1","Q_FSLPBSP2","Q_FSLPBSP3","Q_FSLPBSP4","Q_FSLPBSP5","Q_FSLPBSP6",
                          "Q_FSLPBSP7","Q_FSLPBSP8","Q_FSLPBSP9","Q_FSHAKSP1","Q_FSHAKSP2","Q_FSHAKSP3","Q_FSHAKSP4",
                          "Q_FSHAKSP5","Q_FSHAKSP6","Q_FSHAKSP7","Q_FSHAKSP8","Q_FSHAKSP9","Q_FPLHRSP1","Q_FPLHRSP2",
                          "Q_FPLHRSP3","Q_FPLHRSP4","Q_FPLHRSP5","Q_FPLHRSP6","Q_FPLHRSP7","Q_FPLHRSP8","Q_FPLHRSP9",
                          "Q_FPADLSP1","Q_FPADLSP2","Q_FPADLSP3","Q_FPADLSP4","Q_FPADLSP5","Q_FPADLSP6","Q_FPADLSP7",
                          "Q_FPADLSP8","Q_FPADLSP9","Q_FBELTSP1","Q_FBELTSP2","Q_FBELTSP3","Q_FBELTSP4","Q_FBELTSP5",
                          "Q_FBELTSP6","Q_FBELTSP7","Q_FBELTSP8","Q_FBELTSP9","Q_FPADBSP1","Q_FPADBSP2","Q_FPADBSP3",
                          "Q_FPADBSP4","Q_FPADBSP5","Q_FPADBSP6","Q_FPADBSP7","Q_FPADBSP8","Q_FPADBSP9","Q_FBRNSP1",
                          "Q_FBRNSP2","Q_FBRNSP3","Q_FBRNSP4","Q_FBRNSP5","Q_FBRNSP6","Q_FBRNSP7","Q_FBRNSP8","Q_FBRNSP9",
                          "Q_FBRNMSP1","Q_FBRNMSP2","Q_FBRNMSP3","Q_FBRNMSP4","Q_FBRNMSP5","Q_FBRNMSP6","Q_FBRNMSP7",
                          "Q_FBRNMSP8","Q_FBRNMSP9","Q_FGILTSP1","Q_FGILTSP2","Q_FGILTSP3","Q_FGILTSP4","Q_FGILTSP5",
                          "Q_FGILTSP6","Q_FGILTSP7","Q_FGILTSP8","Q_FGILTSP9","Q_SCREAMSP1","Q_SCREAMSP2","Q_SCREAMSP3",
                          "Q_SCREAMSP4","Q_SCREAMSP5","Q_SCREAMSP6","Q_SCREAMSP7","Q_SCREAMSP8","Q_SCREAMSP9"))



# 3 add labels (if necessary)
df.basic<-df.basic[,-which(colnames(df.basic)%in%grep("X", colnames(df.basic), value = TRUE))] # delete unused columns 
columnnames <- names(df.basic) # Create a vector of column names
col<-length(columnnames) # length 

# Loop through column names and check if labels are NULL
# this loop works, but for some reason the labels in df.basic are not recognized
 for (col_name in colnames(df)) {
  label <- attr(df[[col_name]], "label")
  
  if (is.null(label)) {
    cat("Column:", col_name, "has no label\n")
  }
}


# add labels that are missing 
attr(df.basic$studyid, "label") <- "study id" #do not change; used for merging
attr(df.basic$subcohortid, "label") <- "Group Assigned at Baseline"
attr(df.basic$id, "label") # already assigned
attr(df.basic$g1w1ses,"label")<-  "standardized score based on mothers educational level occupation and income at preschool age"

# NOTE.todo 20231127 us-lls for preschool discipline; make a distinction between past 3 months and longer ago in labels

# 4 add answer category labels if not added
# NOTE.todo 20231127 us-lls request for g1w0medu and g1w0fedu labels

attr(df.basic$subcohortid, "labels")<-c("community"=c(1,3,5,6,7,8,9), "social services/welfare"=c(2,4))
attr(df.basic$g1w0medu,"labels")
attr(df.basic$g1w0fedu,"labels")

# check data with Meta-analysis: any variables missing? 




# Update codebook

data.dictionary.3 <- rename(data.dictionary, item = variable)

new_codebook <- left_join(data.dictionary.1, data.dictionary.3[,c("pos","item")], by = "pos")



# select the observations that are applicable & change names variables to new ones?
data.dictionary[[1]][which(colnames(data)%in%x)]
data.dictionary[[2]][which(colnames(data)%in%x)]
data.dictionary[[3]][which(colnames(data)%in%x)]
data.dictionary[[4]][which(colnames(data)%in%x)]
data.dictionary[[5]][which(colnames(data)%in%x)]
data.dictionary[[6]][which(colnames(data)%in%x)]
data.dictionary[[7]][which(colnames(data)%in%x)]

# Write Notes to PDF add 

notes <- todor_file("scripts/icpsr/20231018-r-try-out.R", todo_types = NULL, output = "markdown") # request notes file
notes <- str_remove_all(notes, "NOTE")
notes <- str_remove_all(notes, "\\[|\\]")

# writeLines(notes, "notes.txt")
cat(notes, file = "notes.txt")
cat(notes, file = "harsh-discipline.txt", append = TRUE)


?todor_file
