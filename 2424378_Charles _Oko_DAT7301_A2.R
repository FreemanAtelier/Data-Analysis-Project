#Loading required packages#
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(ggformula)
library(mosaicCore)
library(effects)
library(corrplot)
library(psych)
library(janitor)


#importing dataset
student_por <- read_csv("C:/Users/okoch/OneDrive/Desktop/student-por.csv")
View(student_por)

#We can get the dimensions of the dataframe. Number of rows and columns
dim(student_por)

#We can look at the names of the variables (column vectors)
colnames(student_por)

#We can display the structure of the dataframe
str(student_por)

#Creating a dataframe for required variables
set.seed(123)
# we use set.seed to ensure that we all get the same random sample
# of 1,000 NHANES subjects in our nh_data collection
#The sample_n() function in R is used to randomly select n rows from a data frame or tibble. It's part of 
#the dplyr package.

df_var <- sample_n(student_por, size = 649) %>%
  select(studytime, sex, internet, famsup, guardian, G3, G2, G1)

View(df_var)

##########################
##Cleaning the dataframe##
##########################

##Checking for Missing Data

sum(is.na(df_var))

###checking for Outliers using boxplot
boxplot(df_var$G3, main = "Outliers in Final Grade", col = "skyblue")
boxplot(df_var$studytime, main = "Outliers in Study Time", col = "darkgreen")
##from the boxplot of both G3 and study it can be seen that there are outliers 
##but before dealing I want to check the frequency to understand if there anomalies or not 
### using the function table() to view frequencies of each observation in each variable
table(df_var$studytime)
table(df_var$G3)
###the observation 0 in final grade is flagged as an outlier but I can't impute or delete because it since it appeared 15 times
###the observation 4 in study time is flagged as an outlier but I can't impute or delete because it since it appeared 33 times


####Dealing with duplicates using tidyverse function distinct() and pipe operator %>% 
 df_var %>% 
   distinct() %>% 
   View()

#Describing and Summarizing the Dataset, since the dataset consists of categorical and quantitative datatype
 #considered summarizing them separating
 
##Describing and Summarizing Quantitative Variables
 ###summarizing central tendencies for numeric variables using pipe operator
df_var %>% 
  select(G3, G2, G1, studytime) %>% 
  summary()
  
###visualizing with histogram and density plot to understand distribution and trends

##creating a histogram of the G3 using the gf_histogram() function.
##The first argument is a formula using the tilde operator (~) or modeler that identifies the attribute to be plotted
##The second argument, data =, refers to the dataset
gf_histogram(
  ~ G3, data = df_var, 
  color = "navy", 
  fill = "purple"
) +
  ggtitle("Histogram Distribution of Final Grade") +
  xlab("Final Grade") +
  ylab("Count") +
  theme(
    plot.background = element_rect(fill = "lightgray", color = NA)
  )

## creating a Density plots can be created with the gf_density() function which takes the same arguments as the other gf_ function.

gf_density(
  ~ G3, data = df_var,
  color = "navy", 
  fill = "purple"
) +
  geom_vline(aes(xintercept = mean(G3)),
             color = "blue", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(G3)),
             color = "red", linetype = "dotted", size = 1) +
  ggtitle("Density Plot of Final Grade with Mean and Median") +
  xlab("Final Grade") +
  ylab("Density") +
  theme(
    plot.background = element_rect(fill = "lightgray", color = NA)
  )

  

###Describing and Summarizing Categorical Variables

# count the categories in the sex attribute
df_stats(~sex, data = df_var, counts)

#  compute the proportion for each category in the sex attribute
df_stats(~sex, data = df_var, props)
#  compute the percentage for each category in the sex attribute
df_stats(~sex, data = df_var, percs)

gf_counts( 
  ~ sex, data = df_var,
  color = "navy", 
  fill = "purple"
) +
  ggtitle("Sex Bar Chart Count") +
  xlab("Sex") +
  ylab("Count") +
  theme(
    plot.background = element_rect(fill = "lightgray", color = NA)
  )




###################################
##Exploratory Data Analysis (EDA)##
###################################
##viewing the dimensions of the dataset
 dim(df_var)
###We can look at the names of the variables (column vectors)
 colnames(df_var)
 
####viewing the first 6 and last 6
 head(df_var)
 tail(df_var)

#####Using the tidyverse function glimpse() to view the structure of the dataset
 glimpse(df_var)
 
######Transformations
 #looking at the dataset G3, G2 and G1 are defined as double instead of integers
 # Converting G1, G2, G3 from double to integer
 df_var$G1 <- as.integer(df_var$G1)
 df_var$G2 <- as.integer(df_var$G2)
 df_var$G3 <- as.integer(df_var$G3)
 df_var$studytime <- as.integer(df_var$studytime)
 
 # creating a predicted G3 using G1 and G2
 perf_model <- lm(G3 ~ G1 + G2, data = df_var)
 
 # Predict expected G3
 df_var$G3_predicted <- round(predict(perf_model), 2)
 
 view(df_var)
 
 
 ##creating performance level variable using G3_predicted and making it a factor of "High", "Medium" and "Low"  
 df_var <- df_var %>%
   mutate(performance = case_when(
     G3_predicted >= 15 ~ "High",
     G3_predicted >= 10 ~ "Medium",
     G3_predicted <= 10 ~ "Low"
   )) %>%
   mutate(performance = factor(performance, levels = c("Low", "Medium", "High")))
 
 
  ##transforming sex observations from "M", and "F", to "male" and "female
 df_var <- df_var %>%
   mutate(sex = recode(sex, 
                       "M" = "male",
                       "F" = "female"))

 view(df_var)
 
 
 #Distributions and Trends of variables
 
 # Performance level distribution
 ggplot(df_var, aes(x = performance)) +
          geom_bar(fill = "navy") +
          labs(title = "Distribution of Student Performance Levels", x = "Performance", y = "Count") +
          theme_minimal()+
   theme(
     plot.background = element_rect(fill = "lightgray", color = "gold")
   )
 
        
 # Study time distribution
 ggplot(df_var, aes(x = studytime)) +
   geom_bar(fill = "blue") +
   labs(title = "Distribution of Study Time", x = "Study Time", y = "Frequency") +
   theme_minimal()+
   theme(
     plot.background = element_rect(fill = "lightgray", color = "navy")
   )
 
 
 # Guardian type distribution
 ggplot(df_var, aes(x = guardian)) +
   geom_bar(fill = "orange") +
   labs(title = "Distribution of Guardian Types", x = "Guardian", y = "count") +
   theme_minimal()+
   theme(
     plot.background = element_rect(fill = "lightgray", color = "navy")
   )
 
 
 # Internet access distribution
 ggplot(df_var, aes(x = internet)) +
   geom_bar(fill = "purple") +
   labs(title = "Distribution of Internet Access", x = "Internet Access", y = "count") +
   theme_minimal()+
   theme(
     plot.background = element_rect(fill = "lightgray", color = "navy")
   )
 
 
 # Gender distribution
 ggplot(df_var, aes(x = sex)) +
   geom_bar(fill = "forestgreen") +
   labs(title = "Distribution of Sex", x = "Sex", y = "Count") +
   theme_minimal()+
   theme(
     plot.background = element_rect(fill = "lightgray", color = "navy")
   )
 
 
 ###############################################################################################
 ##Application of advanced techniques and relevant statistical tests to support your analysis ##
 ###############################################################################################
 
 
 # Research Questions:
 
 #1. Is students' study time associated with their performance level, specifically for those who have internet access?
 #2. Is there an association between Performance level and gender?
 #3. Is there an association between Performance level and guardian?
 
 #1. Is there an association between Study Time and Performance Level among internet access?
 #...........................................................................................
 #Ho: There's no association between Study Time and Performance Level, specifically for those who have internet access  
 #H1: There's an association between Study Time and Performance Level, specifically for those who have internet access
 #I used t.test(quantitative~categorical, data=df_var)
 #comparing the means
 
 
 #selecting the variables and observations for data analysis
 test_vars <- df_var %>% select(studytime, internet, performance) %>% 
 filter(performance %in% c("High", "Medium"))
 head(test_vars)
 
 #a) by performance level
 #---------------------
 #summary statistics for studytime by performance level
 summary_stat_by_performance_level <- test_vars %>% select(studytime, performance) %>%
   #group by performance
   group_by(performance) %>%
   #Calculate summary statistics for household income by education level
   summarise(n = n(), mean(studytime), sd(studytime))
 print(summary_stat_by_performance_level)
 
 # conduct comparison of the means test
 t.test(studytime~performance, data=test_vars)
 
 #b) by internet
 #-----------
 #summary statistics for studytime income by internet
 summary_stat_by_internet <- test_vars %>% select(studytime, internet) %>%
   #group by race
   group_by(internet) %>%
   #Calculate summary statistics for studytime income by internet
   summarise(n = n(), mean(studytime), sd(studytime))
 print(summary_stat_by_internet)
 
 # conduct comparison of the means test
 t.test(studytime~internet, data=test_vars)
 
 
 ##2. Is there an association between performance level and gender?
 #We are basically going to compare the proportions
 #----------------------------------------------------------
 #Ho:There's no association between performance level and gender
 #H1:There's association between performance level and gender
 test_vars2 <- df_var %>% select(performance, sex, guardian) %>%
   filter(performance %in% c("High", "Medium"), guardian %in% c("father", "mother"))
 head(test_vars2)
 
 #here a table is needed to investigate the frequency.
 table(test_vars2$performance, test_vars2$sex)
 
 #Create a percentage table of education level by gender 
 tabyl(test_vars2, performance, sex) %>% 
   adorn_percentages("col") %>%
   adorn_pct_formatting(digits=2)
 
 #conduct a chi-squared Test of independence
 chi_square_test <- chisq.test(test_vars2$performance, test_vars2$sex)
 #Check expected values
 expected_values <-chi_square_test$expected
 print(expected_values) #all expected values are greater than 5.
 #In a Chi-Square test for independence, the expected values (or expected frequencies) represent the counts 
 #we would expect to observe in each category if the null hypothesis were true (i.e., no association between
 #variables or no deviation from the expected distribution)
 print(chi_square_test)
 
 #Interpretation: p-value = 0.09932 means there is very little evidence in this data for association between gender
 #and performance in the same population.

 #3 investigate association between performance level and guardian
 #...................................................................
 #Ho: There's no association between performance level and guardian
 #H1: There's no association between performance level and guardian
 #creating a table for investigating the frequency.
 
 table(test_vars2$performance, test_vars2$guardian)
 #Create a percentage table of education level by race 
 tabyl(test_vars2, performance, guardian) %>% 
   adorn_percentages("col") %>%
   adorn_pct_formatting(digits=2)
 #conduct a chi-squared Test of independence
 chi_square_test <- chisq.test(test_vars2$performance, test_vars2$guardian)
 
 expected_values <-chi_square_test$expected
 print(expected_values) #all expected values are greater than 5.
 
 print(chi_square_test)
 #Interpretation; Since p-value = 0.7894 there is little evidence of association between performance level
 #and guardian in the same population.
 
 ###################################
 ##Advanced Statistical Techniques##
 ###################################
 
 ##### Multiple Linear Regression
 #To model the effect of studytime + famsup + guardian + internet + sex on G3
 
 lm_model <- lm(G3 ~ studytime + famsup + guardian + internet + sex, data = df_var)
 summary(lm_model)
 
 # Plotting all main effects
 plot(allEffects(lm_model), multiline = TRUE, rug = FALSE, ci.style = "bands")
 
 ##Justification: To model the relationship between multiple predictors and the outcome, 
 ##allowing us to interpret coefficients and test significance.
 
 
 #########################
 ##Correlation Analysis###
 #########################
 #using function corr.test()
 corr.test(df_var[, c("G1", "G2", "G3")])


 #using function corrplot()
 cor_matrix <- cor(df_var[, c("G1", "G2", "G3")])
 corrplot(cor_matrix, method = "color", addCoef.col = "black")
 title(main = "Correlation Heat Map of G1, G2 & G3", line = 3)
 
   
 #using ggplot function to plot scatter plot
 # G1 vs G3
 ggplot(df_var, aes(x = G1, y = G3)) +
   geom_point(position = position_jitter(width = 0.1, height = 0.1), color = "limegreen") +
   geom_smooth(method = "lm", se = TRUE, color = "black") +
   labs(title = "Scatter Plot Correlation of G1 and G3",
        x = "G1", y = "G3") +
   theme_minimal()+
   theme(
     plot.background = element_rect(fill = "lightgray", color = "blue")
   )
 
 # G2 vs G3
 ggplot(df_var, aes(x = G2, y = G3)) +
   geom_point(position = position_jitter(width = 0.1, height = 0.1), color ="red") +
   geom_smooth(method = "lm", se = TRUE, color = "black") +
   labs(title = "Scatter Plot Correlation of G2 and G3",
        x = "G2", y = "G3") +
   theme_minimal()+
   theme(
     plot.background = element_rect(fill = "lightgray", color = "blue")
   )
 
 
 
 
 #Justification: To confirm the strength and direction of the strong correlation between G3, G2 and G1
 #As indicated in the dataset source
 
 
 
 #############################
 ##Visualisation of insights##
 #############################
 
 # Plot mean study time by performance level
 ggplot(summary_stat_by_performance_level, aes(x = performance, y = `mean(studytime)`, fill = performance)) +
   geom_bar(stat = "identity", width = 0.6) +
   geom_errorbar(aes(ymin = `mean(studytime)` - `sd(studytime)`,
                     ymax = `mean(studytime)` + `sd(studytime)`),
                 width = 0.2) +
   labs(title = "Mean Study Time by Performance Level",
        x = "Performance Level", y = "Mean Study Time") +
   theme_minimal()+
   theme(
     plot.background = element_rect(fill = "lightgray", color = "gold")
   )
  
 
 # Plot mean study time by internet access
 ggplot(summary_stat_by_internet, aes(x = internet, y = `mean(studytime)`, fill = internet)) +
   geom_bar(stat = "identity", width = 0.6) +
   geom_errorbar(aes(ymin = `mean(studytime)` - `sd(studytime)`,
                     ymax = `mean(studytime)` + `sd(studytime)`),
                 width = 0.2) +
   labs(title = "Mean Study Time by Internet Access",
        x = "Internet Access", y = "Mean Study Time") +
   theme_minimal() +
   theme(
     plot.background = element_rect(fill = "lightgray", color = "gold")
   )
 

 #Proportion Performance Level by Gender
 ggplot(test_vars2, aes(x = sex, fill = performance)) +
   geom_bar(position = "fill") +
   scale_y_continuous(labels = scales::percent) +
   labs(title = "Performance Level by Gender (Proportions)",
        x = "Gender",
        y = "Proportion of Students") +
   theme_minimal()+
   theme(
     plot.background = element_rect(fill = "lightgray", color = "gold")
   )
 
 ##Proportion Performance Level by Guardian
 ggplot(test_vars2, aes(x = guardian, fill = performance)) +
   geom_bar(position = "fill") +
   scale_y_continuous(labels = scales::percent_format()) +
   labs(title = "Performance Level by Guardian (Proportions)",
        x = "Guardian",
        y = "Proportion of Students") +
   theme_minimal()+
   theme(
     plot.background = element_rect(fill = "lightgray", color = "gold")
   )
 
 ##############################
 ##Interpretation of insights##
 ##############################
 
#Research Question 1
#1. Study Time by Performance Level
#Summary of test:
#Mean Study Time (Medium performers): 1.95
 
#Mean Study Time (High performers): 2.28
 
#p-value: 0.0005434
 
#Hypotheses:
#Ho: There is no difference in mean study time between medium and high performance levels.
#H1: There is a difference in mean study time between medium and high performance levels.
 
#Interpretation:
#Since the p-value (0.0005) is less than 0.05, so the null hypothesis should be rejected.
#Therefore there is a statistically significant difference in study time between students with medium and high performance levels.

#2. Study Time by Internet Access
#Summary of test:
#Mean Study Time (no internet): 1.88
 
#Mean Study Time (with internet): 2.05
 
#p-value: 0.07785
 
#Hypotheses:
#Ho: There is no difference in mean study time between students with and without internet.
#H1: There is a difference in mean study time between students with and without internet.

#Interpretation:
#since the p-value (0.078) is greater than 0.05, I fail to reject the null hypothesis.
#so it can be seen that the difference in study time between students with and without internet access is not statistically significant at the 5% level.
 
#Research Question 2
#Is there an association between performance level and gender?
   
#Here I compared proportions of students in each performance level (High vs Medium) across genders (male vs female) using a Chi-Square Test of Independence.
 
#Hypotheses:
#Ho: There is no association between performance level and gender (they are independent).
#H1): There is an association between performance level and gender (they are dependent).

#Chi-Square Test Result:
  
#Chi-square statistic (X²): 1.5561
 
#Degrees of freedom (df): 1
 
#p-value: 0.2122
 
#Interpretation:
#Since the p-value (0.2122) is greater than 0.05, I fail to reject the null hypothesis.
#Meaning  I do not have statistically significant association between performance level and gender in my sample
#or looking at it like in the real world performance level does not differ significantly between male and female students.
 
 
#Research Question 3:
#Is there an association between performance level and guardian?
   
#Hypotheses:
#Ho: There is no association between performance level and guardian type.
#H1: There is an association between performance level and guardian type.
 
#Chi-Square Test Results:
#Test statistic (X²): 0.098
 
#df: 1
 
#p-value: 0.7545
 
#All expected counts > 5 (assumption met)
 
#Interpretation:
#Since the p-value (0.7545) is much greater than 0.05, I fail to reject the null hypothesis. 
#There's no statistically significant association between performance level and whether the student's guardian is their mother or father.
 
 
 
 
 
 
 
 
 
 