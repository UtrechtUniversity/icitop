library(psych)
library(tidyverse)
library(haven)
library(labelled)
library(todor)
library(codebook)

# this script is a try-out
# categories of parenting need to be checked again

######################
# SETTINGS
######################

# how can we keep the original variable name in codebook as well?
# this is also important for checking if recoding went well

options(max.print=100000) 

######################
# FUNCTIONS
######################

# provide: position column in df.basic, position column in df, variable name without item number
# if you make a mistake, run full script again

recode_and_rename <- function(col_df.basic,col_df, name_variable){
  df.basic[,col_df.basic]<-df[,c(col_df)]
  col<-col_df.basic
  newcol<-1:length(col_df.basic) # starts at 1; same length as col
  # Rename each column in a loop 
  for (i in newcol) {
    old_column_name <- paste("X", col[i], sep = "")
    new_column_name <- paste(name_variable, i, sep = "")
    colnames(df.basic)[colnames(df.basic) == old_column_name] <- new_column_name
  }
  return(df.basic)
}

# see if I can make this script with ascending numbers
# see if I can make this script bases on items instead of position

######################
# NOTES
######################

# Get today's date
today_date <- Sys.Date() # add this date to notes?

# NOTES.todo 
# Notes.demo

######################
# Read data
######################

df<-read_sav(file='O:/Research/FSW/Research_data/JG/SanneG/ICITOP IPD_MA/102.us-lls/2.data-checks/ICITOP/Lehigh_ICITOP.sav')

######################
# Data check
######################

# 1. check if data is labelled 
haven::is.labelled(df) # NOTE No doesn't mean that a codebook cannot be generated
data.dictionary.1 <- labelled::generate_dictionary(df)
# data.dictionary.2 <- codebook::codebook_table(df) # this one is very slow
data.dictionary.1$levels # answer categories
data.dictionary.1$variable # variable code
data.dictionary.1$label # original question

#2.recode and check data
# from GPT lesson: Avoid creating an empty data frame with predefined column names:
# Instead of creating an empty data frame with predefined column names, you can create 
# a data frame with the appropriate number of rows and add the recoded variables as you 
# loop through them.

# problem: we don't know how many columns we'll need
# as this example should only have requested data, use the same N as the requested data
# + study id

df.basic <- data.frame(matrix(ncol = 1317, nrow = 457))

# 2a. determine ID's
# NOTE.ID code: us-lls
# NOTE.ID number: 14
# NOTE.ID How many G1 parents? 2
# NOTE.ID How many G2 children/parents? 1
# NOTE.ID How many G3 children? - all children

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

# go through dataset and code

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
df.basic$start<-1976

# SES
colnames(df.basic)[11]<-"g1w1vses"
df.basic$g1w1ses<-df$zses

colnames(df.basic)[12]<-"g1w0medu"
colnames(df.basic)[13]<-"g1w0fedu"

#NOTE.todo check if fedco is mom and medco is father + which wave (presumed preschool)?
df.basic$g1w0medu<-df$fedcom21
df.basic$g1w0fedu<-df$medco21a

colnames(df.basic)[14]<-"g1w0vinc"
df.basic$g1w0vinc<-df$atxinc28

colnames(df.basic)[15]<-"g1w0vadd"
df.basic$g1w0vadd<-df$totrm33

# various discipline techniques at preschool
# various loops; integrate in 1 loop?
# made loop; replace text below for loop

# 11 to 17, 29 32-46 are physical discipline

df.basic[,c(16:38)]<-df[,c(11:17,29,32:46)]

col<-16:38
newcol<-1:23

# Rename each column in a loop
#this only works once. if you make a mistake, clear session and run again)
for (i in newcol) {
  old_column_name <- paste("X", col[i], sep = "")
  new_column_name <- paste("g1w1mmqupun", i, sep = "")
  colnames(df.basic)[colnames(df.basic) == old_column_name] <- new_column_name
}

names(df.basic)[col] #check

rm(i, col,newcol, new_column_name,old_column_name) # remove all objects related to loop

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

#### Gen 2-3






# 3 add labels (if necessary)

# Create a vector of column names

columnnames <- names(df.basic)

# Loop through the columns and print the attribute label
for (col in columnnames) {
  print(attr(df.basic[[col]], "label"))
}

# add labels that are missing 
attr(df.basic$studyid, "label") <- "study id" #do not change; used for merging
attr(df.basic$subcohortid, "label") <- "Group Assigned at Baseline"
attr(df.basic$id, "label") # already assigned
attr(df.basic$g1w1ses,"label")<-  "standardized score based on mothers educational level occupation and income at preschool age"

# Note.do for preschool discipline; make a distinction between past 3 months and longer ago in labels


# 4 add answer category labels if not added
# NOTE.request for g1w0medu and g1w0fedu labels

attr(df.basic$subcohortid, "labels")<-c("community"=c(1,3,5,6,7,8,9), "social services/welfare"=c(2,4))
attr(df.basic$g1w0medu,"labels")
attr(df.basic$g1w0fedu,"labels")


str(df.basic)

# structure data: 1 row per G2 participant


# HD C811L1A: Parents response to fight with sibs
df$g1w1c2bhdi1<-recode(data$C811L1A, "(11) Scold,nag,criticize"=1, .default=0)
df$g1w1c2bhdi2<-recode(data$C811L1A, "(14) Threaten physical"=1, .default=0)
df$g1w1c2bhdi3<-recode(data$C811L1A, "(15) Wash mouth"=1, .default=0)
df$g1w1c2bhdi4<-recode(data$C811L1A, "(16) Hit with object"=1, .default=0)
df$g1w1c2bhdi5<-recode(data$C811L1A, "(17) Slap"=1, .default=0)
df$g1w1c2bhdi6<-recode(data$C811L1A, "(18) Spank"=1, .default=0)
 
# HD C811L1B: Parents #2 response to fight with sibs
df$g1w1c2bhdi7<-recode(data$C811L1B, "(11) Scold,nag,criticize"=1, .default=0)
df$g1w1c2bhdi8<-recode(data$C811L1B, "(14) Threaten physical"=1, .default=0)
df$g1w1c2bhdi9<-recode(data$C811L1B, "(15) Wash mouth"=1, .default=0)
df$g1w1c2bhdi10<-recode(data$C811L1B, "(16) Hit with object"=1, .default=0)
df$g1w1c2bhdi11<-recode(data$C811L1B, "(17) Slap"=1, .default=0)
df$g1w1c2bhdi12<-recode(data$C811L1B, "(18) Spank"=1, .default=0)

# NOTE C811L2A: Parent #1 response to sass others
df$g1w1c2bhdi13<-recode(data$C811L2A, "(11) Scold,nag,criticize"=1, .default=0)
df$g1w1c2bhdi14<-recode(data$C811L2A, "(14) Threaten physical"=1, .default=0)
df$g1w1c2bhdi15<-recode(data$C811L2A, "(15) Wash mouth"=1, .default=0)
df$g1w1c2bhdi16<-recode(data$C811L2A, "(16) Hit with object"=1, .default=0)
df$g1w1c2bhdi17<-recode(data$C811L2A, "(17) Slap"=1, .default=0)
df$g1w1c2bhdi18<-recode(data$C811L2A, "(18) Spank"=1, .default=0)

# C811L2B: Parent #2 response to sass others
df$g1w1c2bhdi19<-recode(data$C811L2B, "(11) Scold,nag,criticize"=1, .default=0)
df$g1w1c2bhdi20<-recode(data$C811L2B, "(14) Threaten physical"=1, .default=0)
df$g1w1c2bhdi21<-recode(data$C811L2B, "(15) Wash mouth"=1, .default=0)
df$g1w1c2bhdi22<-recode(data$C811L2B, "(16) Hit with object"=1, .default=0)
df$g1w1c2bhdi23<-recode(data$C811L2B, "(17) Slap"=1, .default=0)
df$g1w1c2bhdi24<-recode(data$C811L2B, "(18) Spank"=1, .default=0)

data.dictionary.3 <- labelled::generate_dictionary(df)
data.dictionary.4 <- codebook::codebook_table(df)

data.dictionary.3 <- rename(data.dictionary.3, item = variable)

new_codebook <- left_join(data.dictionary.1, data.dictionary.3[,c("pos","item")], by = "pos")

# try with a loop; doesn't work yet

x<-c(c(paste0("C811L", 1:15,"A")),c(paste0("C811L", 1:15,"B")))
x<-replicate(6,x)
y<-replicate(30,(c("(11) Scold,nag,criticize", "(14) Threaten physical",
                   "(15) Wash mouth", "(16) Hit with object", "(17) Slap",
                   "(18) Spank")))  
l<-1:180

for (i in seq_along(l)){
  category<-paste0(y[i])
  variable<-paste0("data$",x[i])
  df[,i]<-recode(variable,'category'=1, .default=0)
}

describe(df) # empty

# change data dictionary

# select the observations that are applicable & change names variables to new ones?
data.dictionary[[1]][which(colnames(data)%in%x)]
data.dictionary[[2]][which(colnames(data)%in%x)]
data.dictionary[[3]][which(colnames(data)%in%x)]
data.dictionary[[4]][which(colnames(data)%in%x)]
data.dictionary[[5]][which(colnames(data)%in%x)]
data.dictionary[[6]][which(colnames(data)%in%x)]
data.dictionary[[7]][which(colnames(data)%in%x)]

# add 

notes <- todor_file("scripts/icpsr/20231018-r-try-out.R", todo_types = NULL, output = "markdown")
notes <- str_remove_all(notes, "NOTE")
notes <- str_remove_all(notes, "\\[|\\]")

# writeLines(notes, "notes.txt")
cat(notes, file = "notes.txt")
cat(notes, file = "harsh-discipline.txt", append = TRUE)



