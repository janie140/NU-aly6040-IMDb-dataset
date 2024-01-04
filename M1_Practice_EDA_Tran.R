# Trang Tran, M1, EDA, April 23

cat("\014")  # clears console
rm(list = ls())  # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session

library(pacman)  
p_load(tidyverse) 
library(gt)
library(glue)
library(corrplot)
p_load(skimr)

#Load the dataset
df <- read.csv("smaller_data_history_genre_only.csv")
colnames(df)

#Descriptive statistics
# summary statistics
skim(df) #data types, missing values, min, max, mean, sd, hist
anyDuplicated(df) #check for duplications

# check for outliers or suspicious data
# frequency of isAdult
table(df$isAdult)

# frequency of endYear
table(df$endYear)

# frequency of startYear
table(df$startYear)

# frequency of runtimeMinutes
table(df$runtimeMinutes)

# histogram of averageRating
ggplot(df, aes(x = averageRating)) +
  geom_histogram(stat = "count", fill = "orange")

# histogram of numVotes
ggplot(df, aes(x = numVotes)) +
  geom_histogram(fill = "steelblue")

# histogram of titleType
ggplot(df, aes(x = titleType)) +
  geom_histogram(stat = "count", fill = "brown")

#Data pre-processing
# change isAdult into character type
df$isAdult <- as.character(df$isAdult)

# change startYear into numeric type
df <- df |> 
  mutate(startYear = as.numeric(ifelse(startYear != "\\N", startYear, NA)))

# change runtimeMinutes into numeric type
df <- df |> 
  mutate(runtimeMinutes = as.numeric(ifelse(runtimeMinutes != "\\N", runtimeMinutes, NA)))

# impute mean in all numeric columns
nums <- colnames(df[, unlist(lapply(df, is.numeric)), drop = FALSE]) 
for (col_name in nums) {
  df[is.na(df[, col_name]), col_name] <- mean(df[, col_name], na.rm = TRUE)
}

#Correlation matrix
# Corrplot of numeric variables: #na.rm = TRUE
corrs <- round(cor(df[, unlist(lapply(df, is.numeric))], use = "complete.obs"), 2)
corrplot(corrs, method="circle", type="upper", tl.col = "black", tl.srt = 45)

