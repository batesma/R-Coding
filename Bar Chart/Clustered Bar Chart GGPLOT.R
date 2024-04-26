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

#Install Packages
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
if (!require("broom")) install.packages("broom")
if (!require("glue")) install.packages("glue")
if (!require("naniar")) install.packages("naniar")
if (!require("gggap")) install.packages("gggap")
if (!require("gg.gap")) install.packages("gg.gap")

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
library(tidyverse)
library(readxl)
library(ggtext)
library(glue)
library(broom)
library(naniar)
library(gggap)
library(gg.gap)
library(plyr)
library(drc)


####Load Data###############################################
MyData <- read_xlsx(file.choose())
MyData2 <-MyData[ ,c(1,13:14)] #Subset Data if desired, rows and then columns 
colnames(MyData2) <- c ("Sample_Name",  #Rename Columns of Interest Data
                        "Percent_Gene_Expression", 
                        "Gene_Name") 
MyData2$`Sample_Name`[MyData2$`Sample_Name` == "MAD1"] <- "MAD2 siRNA" #rename samples 
MyData2$`Sample_Name`[MyData2$`Sample_Name` == "MAD2"] <- "MAD2 siRNA"
MyData2$`Sample_Name`[MyData2$`Sample_Name` == "MAD3"] <- "MAD2 siRNA"
MyData2$`Sample_Name`[MyData2$`Sample_Name` == "NEG1"] <- "Scr siRNA" #rename samples 
MyData2$`Sample_Name`[MyData2$`Sample_Name` == "NEG2"] <- "Scr siRNA"
MyData2$`Sample_Name`[MyData2$`Sample_Name` == "NEG3"] <- "Scr siRNA"
MyData2$`Sample_Name`[MyData2$`Sample_Name` == "UNT1"] <- "Untreated" #rename samples 
MyData2$`Sample_Name`[MyData2$`Sample_Name` == "UNT2"] <- "Untreated"
MyData2$`Sample_Name`[MyData2$`Sample_Name` == "UNT3"] <- "Untreated"
MyData2$`Percent_Gene_Expression`<- round(MyData2$`Percent_Gene_Expression`, digits=2)#round data 

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

head(MyData)
tail(MyData)
names(MyData)
dim (MyData)
View(MyData)
ls()


####Attaching Data#################################
#the attach command import the data and associated
#variables into R's memory to that they can be more
#easily called upon, otherwise columns can be called upon
#using the $ symbol
#the detatch command can be used to remove the data

attach(MyData)

######Load Data in Data Frame######
Plot_Data=data.frame(
  Sample_Name= (c(MyData2$Sample_Name)),
  Percent_Gene_Expression= (c(MyData2$Percent_Gene_Expression)),
  Gene_Name= (c(MyData2$Gene_Name)))

#Subset data if required
#Plot_Data2 <- Plot_Data[c(19:27), ]

########Changing Classes#############################
class(Plot_Data$Sample_Name) #Calls up the class of your data
class(Plot_Data$Gene_Name) #Calls up the class of your data
levels(Plot_Data$Sample_Name)# Calls up the different levels of a factor
levels(Plot_Data$Gene_Name)# Calls up the different levels of a factor
Plot_Data$Sample_Name <- as.factor (Plot_Data$Sample_Name) #Converts to a factor
Plot_Data$Gene_Name <- as.factor (Plot_Data$Gene_Name) #Converts to a factor
Plot_Data$Sample_Name <- factor(Plot_Data$Sample_Name, #Specifies the level order 
                                   levels=c("Untreated",
                                            "Scr siRNA",
                                            "MAD2 siRNA"))
Plot_Data$Gene_Name <- factor(Plot_Data$Gene_Name, #Specifies the level order 
                             levels=c("MyD88", 
                                      "TLR4",
                                      "MAD2"))
##Getting Mean, STD and Other Stats####
summary(Plot_Data) # Generates a summary of whole df in the console
table(Plot_Data$Gene_Name) # Frequency table for a categorical variable
length(Plot_Data$Gene_Name) # Total Number of Observations for a categorical variable
table(Plot_Data$Gene_Name)/length(Plot_Data$Gene_Name) #Percentage of each category
table(Plot_Data$Gene_Name,Plot_Data$Sample_Name) #Two way contigency table

mean(Plot_Data$Percent_Gene_Expression)
median(Plot_Data$Percent_Gene_Expression)
sd (Plot_Data$Percent_Gene_Expression)
min (Plot_Data$Percent_Gene_Expression)
max (Plot_Data$Percent_Gene_Expression)
range (Plot_Data$Percent_Gene_Expression)
quantile (Plot_Data$Percent_Gene_Expression, probs = c(0.9, 0.75, 0.5, 0.25, 0.1))
sum(Plot_Data$Percent_Gene_Expression)
cor(Plot_Data$Percent_Gene_Expression, Plot_Data$Percent_Gene_Expression )#Pearson correlation, note both variables must be numerical
cor(Plot_Data$Percent_Gene_Expression,Plot_Data$Percent_Gene_Expression, method="spearman")#Spearman correlation, note both variables must be numerical
summary(Plot_Data$Percent_Gene_Expression) # Gives mean, med, min, max, quartiles for numerical variable
summary(Plot_Data$Gene_Name) # Gives the frequencies for categorical variables

#Determine Mean and STD for each sample for each gene
Percent_Gene_Expresssion_Summary <- ddply(Plot_Data, c("Gene_Name", "Sample_Name"), summarize, Mean = mean(Percent_Gene_Expression), STD = sd(Percent_Gene_Expression))
Percent_Gene_Expresssion_Summary$Mean<- round(Percent_Gene_Expresssion_Summary$Mean, digits=0) #Rounds Mean
Percent_Gene_Expresssion_Summary$STD<- round(Percent_Gene_Expresssion_Summary$STD, digits=0)   #Rounds STD
Percent_Gene_Expresssion_Summary <- Percent_Gene_Expresssion_Summary%>% replace_with_na(replace = list(STD=0)) # Replaces Zero values with NA for untreated

#Kruskal-Wallis one-way analysis of variance#
kt <- kruskal.test (Percent_Gene_Expression~Sample_Name, data = Plot_Data)
kt$p.value

#Pairwise Wilcox Test#
pairwise.wilcox.test()

#####Plot Data with GGPLOT#######
strip_chart <- #Stores plot 
  ggplot(aes(x=Sample_Name, y=Mean,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    # 
             group=Gene_Name, fill=Gene_Name),data=Percent_Gene_Expresssion_Summary, ylim = c(0, 400))+
  geom_errorbar(aes(ymin = Mean-STD, ymax = Mean+STD), size =1, width=0.6, position=position_dodge(.9))+  #position_dodge needed for bars on multiple columns, putting this function before geom_col hides the lower error bar, width for the width of whiskers, size for error bar thickness
  geom_bar(stat = "identity", position = position_dodge()) + #Position, dodge puts columns side by side, 
                                                              #width and spacing between groups
                                                              # can be set here i.e. position_dodge(width=0.5), width=0.5)
  scale_fill_manual(name=NULL,
                    breaks=c("MyD88", "TLR4",
                             "MAD2"),
                    values = c("blue", "red","Green")) +
  scale_x_discrete(breaks=c("Untreated",
                            "Scr siRNA",
                            "MAD2 siRNA"),
                   labels=c("Untreated",
                            "Scr siRNA",
                            "MAD2 siRNA"),expand=c(0.2,0.2)) +
  labs(x=NULL,
       y="% Gene Expression") +
  theme_classic() +
  theme(axis.text.x = element_text(size=10,colour ="black"), #sample names
        axis.text.y = element_text(size=10,colour ="black", #y axis tick labels
                                   face="bold"),
        axis.title.y = element_text(size=12,colour ="black", 
                                    face="bold",vjust=+2), #y-axis title
        legend.text = element_text(size=10,colour ="black", face="bold"),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.size = unit(10, "pt"),
        legend.position = 'bottom')+ 
  scale_y_continuous(limits = c(0,900),breaks=seq(0,900,100), expand = c(0, 0))#expand 0,0 gets rid of space between axis line and columns

#######################  

  


########Hide  Tick Marks ####
# +theme(axis.text.x = element_blank(),
#     axis.text.y = element_blank(),
#     axis.ticks = element_blank())

strip_chart #recall plot

#Add lines and Asterisk for Significance
strip_chart + 
  geom_line(data=tibble(x=c(1.3,3.3), y=c(850,850)), 
            aes(x=x, y=y), inherit.aes = FALSE,size = 1.5) +
  geom_text(data=tibble(x=2.3, y=855), 
          aes(x=x, y=y, label ="***"), inherit.aes = FALSE,size = 12)
#####Split Axis
gg.gap(
  plot = strip_chart,
  segments = c(200, 700),
  ylim = c(0, 900)
)+
add.legend(plot = strip_chart,
           margin = c(top=1,right=1,bottom=-150,left=1))+

  geom_line(data=tibble(x=c(2,3), y=c(850,850)), 
            aes(x=x, y=y), inherit.aes = TRUE,size = 1.5) +
  geom_text(data=tibble(x=2.3, y=855), 
            aes(x=x, y=y, label ="***"), inherit.aes = FALSE,size = 12)
############Save Plot###########################
ggsave("My Graph.tiff", width=5, height=4)
