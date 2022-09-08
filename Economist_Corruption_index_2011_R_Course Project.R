#Hello, I will be conducting an data analysis on the research done for corruption index by country by the Economist 2011
#I have used different packages for data visualziation, for visual themes for more compelx visuals
#

install.packages('ggplot2')
install.packages('ggthemes')
install.packages('dplyr')
install.packages('data.table')
library(dplyr)
library(ggplot2)
library(data.table)
library(ggthemes)

#Indicating the countries that I want to work on

pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

#Retrieving the data from the spreadsheet

df <- fread('Economist_Index_Data.csv',drop=1)

#Dataframe was plotted on different visuals, with first and two plots I have processed a condition with a formula
#This allowed me to analyze the index in a more statistical way

pl <- ggplot (df,aes(x=CPI, y=HDI, color=Region)) +geom_point(size=4,shape=1)

pl2 <- pl +geom_smooth(aes(group=1),method='lm', formula=y~log(x),se=F,color='red')

#Third plot allows for labeling and indicating about the countries to visualize on the plot

pl3 <- pl2 + geom_text(aes(label = Country), color = "gray20",
data = subset(df, Country %in% pointsToLabel), check_overlap = TRUE)

#Fourth plot was used for theme adjustment as well as adjusting the coordination of the elements

pl4 <-pl3 +scale_x_continuous(limits=c(.9,10.5),breaks=1:10)
print(pl4+theme_economist_white())


#To conlcude this project allowed me to improve my visualziations skills along data manipulation.
#Throughout the coruse I have focused more on these aspects of programming
#Along with focusing integrating more types of visualizations