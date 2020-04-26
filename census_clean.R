setwd("/Users/walden/Projects/dataiku_tests/us_census_full")

library(caret)
library(dplyr)
library(tidyr)

col.labs <- c("AAGE", 
              "ACLSWKR", 
              "ADTIND",
              "ADTOCC",
              "AHGA",
              "AHRSPAY",
              "AHSCOL",
              "AMARITL",
              "AMJIND",
              "AMJOCC",
              "ARACE",
              "AREORGN",
              "ASEX",
              "AUNMEM",
              "AUNTYPE",
              "AWKSTAT",
              "CAPGAIN",
              "CAPLOSS",
              "DIVVAL",
              "FILESTAT",
              "GRINREG",
              "GRINST",
              "HHDFMX",
              "HHDREL",
              "MARSUPWT",
              "MIGMTR1",
              "MIGMTR3",
              "MIGMTR4",
              "MIGSAME",
              "MIGSUN",
              "NOEMP",
              "PARENT",
              "PEFNTVTY",
              "PEMNTVTY",
              "PENATVTY",
              "PRCITSHP",
              "SEOTR",
              "VETQVA",
              "VETYN",
              "WKSWORK", 
              "YEAR",
              "AGI")

col.types <- c("numeric", 
              "character", 
              "character",
              "character",
              "character",
              "numeric",
              "character",
              "character",
              "character",
              "character",
              "character",
              "character",
              "character",
              "character",
              "character",
              "character",
              "numeric",
              "numeric",
              "numeric",
              "character",
              "character",
              "character",
              "character",
              "character",
              "numeric",
              "character",
              "character",
              "character",
              "character",
              "character",
              "numeric",
              "character",
              "character",
              "character",
              "character",
              "character",
              "character",
              "character",
              "character",
              "numeric", 
              "character",
              "character")

train.df <- read.csv("census_income_learn.csv", header = F, col.names = col.labs, colClasses = col.types)
valid.df <- read.csv("census_income_test.csv", header = F, col.names = col.labs, colClasses = col.types)


write.csv(train.df, file = "census_income_learn_labs.csv", row.names = F)
write.csv(valid.df, file = "census_income_test_labs.csv", row.names = F)



train.df.c <- train.df

# Clean dataset
train.df.c[train.df.c == " ?"] <- "Missing"

train.df.c <- train.df.c %>% 
  na.omit

train.df.scale <- train.df.c %>%
  mutate_if(is.numeric, scale)

train.df.scale.c <- train.df.scale %>%
  mutate_if(is.character, as.factor) %>% 
  select(-YEAR, -AWKSTAT)

# One hot encoding Categorical Data 
# Dummy Vars 
dummies <- dummyVars(~ ., data=train.df.scale.c[,-40])
c2 <- predict(dummies, train.df.scale.c[,-40])

d_training <- as.data.frame(c2)
train.df.scale.c.d <- cbind(d_training, AGI=train.df.scale.c[,40])

# Remove Sparse Data 
num.col <- nrow(train.df.scale.c.d) * .05 # 5 Percent complete
  
train.df.scale.c.d1 <- train.df.scale.c.d %>% 
  select(-contains("Missing"), -contains(". ")) 

train.df.scale.c.d2 <- train.df.scale.c.d[ , -which(names(train.df.scale.c.d) %in% colnames(train.df.scale.c.d1))]

train.df.scale.c.d2.a <- train.df.scale.c.d2 %>% 
  select(-contains("Missing")) %>% 
  select_if(function(col) is.numeric(col) && sum(col) >= num.col) %>% 
  bind_cols(train.df.scale.c.d1)

table(train.df.scale.c.d2.a$AGI)

write.csv(train.df.scale.c.d2.a, file = "census_income_learn_tidy.csv", row.names = F)


