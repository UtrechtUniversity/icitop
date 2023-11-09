library(psych)
library(tidyverse)
library(haven)
library(labelled)
library(todor)

load(file='data/raw/icpsr/37939-0004-Data.rda')

data <- da37939.0004

# NOTE data contains the responses of children to the question on how parents would respond
# NOTE to certain actions. Up to two responses can be given

# NOTE the following responses are considered "harsh" for now:
# NOTE 11 Scold,nag,criticize
# NOTE 14 Threaten physical
# NOTE 15 Wash mouth
# NOTE 16 Hit with object
# NOTE 17 Slap
# NOTE 18 Spank
 
haven::is.labelled(data) # NOTE but it does have all the info to generate a data dictionary 
data.dictionary.1 <- labelled::generate_dictionary(data)
data.dictionary.2 <- codebook::codebook_table(data)
data.dictionary$levels
data.dictionary$variable
data.dictionary$label

# NOTE generation 1 wave 1 g2 child reporting about parents harsh discipline
# NOTE before (re-)coding, number ascending  
# 15 questions with 2 responses 
# for try-out: two questions
# probably use a sumscore of all scenario's later, but first step is just putting raw data
# under the right category
# 6 responses to 30 questions = 180 question; can probably do this with a loop

df <- data.frame(matrix(ncol = 180, nrow = 206))
colnames(df)<-c(paste0("g1w1c2bhdi", 1:180))

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



