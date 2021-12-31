install.packages("ggpubr")
library("ggpubr")

my_data <- read.csv('Data_for_analysis_in_r.csv')

res1 <- cor.test(my_data$Sentiment_analysis_score, my_data$Deaths_per_ML, 
                method = "pearson")

res

res2 <- cor.test(my_data$Sentiment_analysis_score, my_data$Cases_per_ML, 
                 method = "pearson")

res2