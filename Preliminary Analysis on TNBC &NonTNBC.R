#This file is my solution I have created to an assessment given by Kains Lab at UHN, Princess Margaret Cancer Center.
# Simple Preliminary Analysis
# I have cleaned the data by deleting empty columns on the mRNA spreadsheet that would cause problem in correlation analysis
# Package installing. daff has been used for extra purposes to latter to be used to put on my personal website

install.packages('corrplot')
library(corrplot)
install.packages('ggplot2')
library(ggplot2)
install.packages('daff')
library(daff)

# Setting up the working directory where the csv files are stored

setwd("C:/Users/User/Documents") 

# Reading the data into data frame and detecting the missing characters

TCGA_BRCA_mRNA <- read.csv("TCGA_BRCA_mRNA.csv", na.strings = '.') 
TCGA_BRCA_subtypes <- read.csv("TCGA_BRCA_subtypes.csv", na.strings = '.') 

# Viewing the data

View(TCGA_BRCA_mRNA)
View(TCGA_BRCA_subtypes)

# Summarizing data 

summary(TCGA_BRCA_subtypes)
summary(TCGA_BRCA_mRNA)

# Missing value checking, used it on console, no values are missing ,verifying the pre-data cleaning

sum(is.na(TCGA_BRCA_subtypes))
sum(is.na(TCGA_BRCA_mRNA))

# Dropping the first columns of the mRNA data frame, because genes are not numeric 

TCGA_BRCA_mRNA_dropped=subset(TCGA_BRCA_mRNA, select = -c(Genes) )
View(TCGA_BRCA_mRNA_dropped)

# After verifying the data is clean and has no missing values , Let's start with analysis steps
# Plotting the mRNA data for TCGA_BRCA_mRNA_dropped, The genes columns was dropped ,but viewing tab still allows me to assess the order is consistent

corrplot(corr = cor(TCGA_BRCA_mRNA_dropped),
        method = "color",
        type = "full",
        tl.pos = "tl",
        order ="original",
        addCoef.col = 1, number.cex=0.5,
        main = "TNBC and NonTNBC patient Pairwise Correlation", line = -2, cex.main = 1)
       


# Selecting only the first 3 rows respectively genes ESR1, PGR, ERBB2,

Genes_data <- TCGA_BRCA_mRNA_dropped[1:3,]

# Separating  NonTNBC from the sliced data frame whole with gene expression of the genes interested (ESR1, PGR, ERBB2)


NonTNBC <- Genes_data[,11:20]
View(NonTNBC)

# Separating  TNBC from the sliced data frame whole with gene expression of the genes interested (ESR1, PGR, ERBB2)


TNBC <- Genes_data[,1:10]
View(TNBC)


# Creating a boxplot for NonTNBC to better illustrate the expression data


boxplot(NonTNBC, xlab = 'Patients of TNBC', ylab = 'Gene Expression ', main = "ESR1, PGR and  ERBB2 Gene Expression data of TNBC patients", cex.main = 0.7)


# Creating a boxplot for TNBC to better illustrate the expression data


boxplot(TNBC, xlab = 'Patients of NonTNBC', ylab = 'Gene Expression ', main = "ESR1, PGR and  ERBB2 Gene Expression data of TNBCs", cex.main = 0.7)



# Conducting t-test to see the similarity between NonTNBC and TNBC patients
# I have assigned the results into a  data frame to better illustrate and assess the difference statistically

T_Test_results <- t.test(NonTNBC , TNBC, alternative="two.sided")
View(T_Test_results)


# To sum up , I have illustrated the similarity between the gene expression of different patients on my correlation analysis. I wanted to illustrate how different genes including the same one
# show correlation in their gene expression patterns. Correlation plot allowed me to understand in patient scale
# Next let's focus on more gene types than the patient scale
# As you see there is low correlation at the low left and high right edges,where NBC and TNBC patients were compared
# Correlation coefficient values less than 0 also indicated a poor correlation in their expression patterns
# This means we have to specify on those stages and separate TNBC from NBC to better illustrate how they are different in their expression
# Starting from Line 52, I have used slicing methods to have different data frames, on which I had both aimed to understand only the first three genes gene expression pattern as indicated
# Plotting separated data allowed me to visualize and compare the NBC to TNBC or vice versa
# Continuing with more statistics, I have conducted a two sample t-test where I have assessed the different between the two types
# With a p-value less than 0.05 I can conclude that there is a statistically significant difference in how the ESR1, PGR and  ERBB2 genes express between TNBC and NBC patients




# This is extra !: I then  wanted to visualize  the difference we need a different package !attention to Viewer tab


diff_data(TNBC,NonTNBC)
render_diff(diff_data(TNBC,NonTNBC, id = "key", always_show_header = "TRUE", show_unchanged = "TRUE"))

#This packages allows me to view and illustrate the TNBC and NonTNBC on an HTML file where I will further edit the pages with my HTML and CSS skills

#Also for the edited version I have send a google drive link for you, pelase check the instructions to view it with proper preview




        
  


