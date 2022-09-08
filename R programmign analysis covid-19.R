# Hello , I will be conducting a statistical analysis on data that I have obtained from Kaggle.com
# I have completed this task with my udemy course, which gave myfoundations on R statistics along with using different pakcages
# For data visualizations, data manipulations as well as interactive data 
# I will be trying to assess COVID_19 symptoms based on gender and age


rm(list=ls())
library(Hmisc)

# Reading data into data frame

data <- read.csv("C:/Users/User/Desktop/capstone projects/R data analysis project/COVID19_line_list_data (1).csv")
describe(data)

# Cleaning the data 

data$death_dummy <- as.integer(data$death !=0)

# Calculating general death rate

sum(data$death_dummy) / nrow(data)

# My Hypothesis: People who die because of the disease are older than people infected

dead=subset(data,death_dummy==1)
alive=subset(data,death_dummy==0)
mean(dead$age, na.rm=TRUE)
mean(alive$age, na.rm =TRUE)

# Based on results found, I assessed whether or not the results are statistically significant
# By using t.test and setting the confidence level as 0.95 for better significanceof the

t.test(alive$age, dead$age, alternative="two.sided",conf.leve=0.95)


# I found the , p_value  is smaller than 0.005 meaning null-hypothesis is rejected
# Safe to conclude that older people are more susceptible to COVID-19, so age has an effect
# also, it is safe to conclude statistically



# Hypothesis: Gender plays no role in COVID-19 severity
#To test this fact, I need to do a t.test

men=subset(data,gender=='male')
women=subset(data,gender =='female')
mean(men$death_dummy, na.rm=TRUE) 
mean(women$death_dummy, na.rm=TRUE)

# I found 3.7% and 8.5%! for women and men respectively their mean difference
# To better test two gender , I have conducted a t.test with two side and a confidence interval of 0.99
# Higher confidence interval allows more certainty as well as more creditability 


t.test(men$death_dummy, women$death_dummy, alternative="two.sided",conf.level=0.99)


# As a result I have found p-value of 0.002 which is smaller than 0.05, therefore it is safe to assume the null-hypothesis is rejected

#To conclude, COVID-19 affected older people in a mroe severe way , but there is no gender affect observed or even neglibile to assume
#it is playing a key role.