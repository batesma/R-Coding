# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 
# Clear packages
detach("package:datasets", unload = TRUE)  # For base
# Clear plots
dev.off()  # But only if there IS a plot
# Clear console
cat("\014")  # ctrl+L

#####SET OPTIONS############################################

# Set the working directory
setwd("C:/Users/Mark/Desktop")

#Set library path
.libPaths("C:/Users/Mark/Documents/R/win-library/4.1") 

######Install and Load Packages###############################

#Install packaages
if (!require("pacman")) install.packages("pacman")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("data.table")) install.packages("data.table")
if (!require("dr4pl")) install.packages("dr4pl")
if (!require("car")) install.packages("car")
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggtext")) install.packages("ggtext")
if (!require("RColorBrewer")) install.packages("RColorBrewer")

#Load Packages
library(datasets)
pacman::p_load(pacman, rio)
library(ggplot2)
library(data.table)
library(dr4pl)
library(car)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggtext)
library(RColorBrewer)
library(Hmisc)

####Load Data###############################################
bp1 <- read_xlsx(file.choose())

# #If you need to load a particular sheet 
# from an excel file specify the file
# and sheet name
# e.g.bp1 <- read_xlsx("Test Data.xlsx", sheet = "Sheet1")
#Otherwise if readxl package loaded use the import dataset tab

#####Viewing Data###############################################

#head allows you to view the first 6 rows in the console
#tail allows you to view the last 6 rows in the console
#names allows you to view the column headings in each column
#dim lists the dimensions of your data(rows & columns)
#view allows you to view the whole data in a seperate window
#ls() lists the different data sets and values stored in R
#rm() can be used to remove data

head(bp1)
tail(bp1)
names(bp1)
dim (bp1)
View(bp1)
ls()


####Attaching Data#################################
#the attach command import the data and associated
#variables into R's memory to that they can be more
#easily called upon, otherwise columns can be called upon
#using the $ symbol
#the detatch command can be used to remove the data

attach(bp1)



########Changing Classes#############################
class(bp1)
class(Sample_Name)
Sample_Name <- as.factor (Sample_Name)
levels(Sample_Name)

#Create Table or Data frame from the Columns of Interest######

table(Sample_Name,MYD88)
table <- table(Sample_Name,MYD88)
table

#####Plotting the Data###########################
#$ is the vector or column selected
barplot (table, ylim=c(0,200),
        beside=TRUE,
        col = rainbow(7))

plot (bp1)
#ylim sets the y axis upper and lower limits
barplot(bp1$CD3, ylim=c(0,100))

#use the names function and $ to label the samples
barplot(bp1$CD3, ylim=c(0,100),names = bp1$CD3)

#use the main, xlab, ylab functions to label axis and give the plot a title
#use the las function to rotate the axis title

data <- diamonds

colors <-c("#blue","red", "green", "orangered",
           "pink","turquoise", "black")

colors <-c

barplot(table(diamonds$color, diamonds$clarity),
        beside=TRUE,
        col = rainbow,
        legend = levels)
       
        
        ylim=c(0,100),
        main="CD3 Expression",
        ylab="% Positivity", 
        col = rainbow(7))

barplot(bp1$CD3, ylim=c(0,100),main="Title",xlab="Gender",ylab="%", 
        las=1, names.arg = c("Female","Male"), col = c(2, 5, 7))

#use the col function to specify colours
barplot(table(bp1$MYD88, bp1$TLR4), 
beside=TRUE, 
ylim=c(0,100),
        main="CD3 Expression",
        ylab="% Positivity", 
        col = rainbow(7))
