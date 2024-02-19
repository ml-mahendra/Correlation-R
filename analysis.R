# Reading Lib
library(readr)

# Loading the Dataset from same folder
heart_df <- read_csv("heart.csv")

# Removing all the rows with cholesterol values 0
df <- heart_df[heart_df$Cholesterol != 0, ]

cholesterol <- df$Cholesterol
age <- df$Age

cor.test(cholesterol, age, method="pearson") 

