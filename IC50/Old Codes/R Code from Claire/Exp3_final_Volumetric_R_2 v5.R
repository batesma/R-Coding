#
# Exp3 Final - Volumetric pipeline
# Claire Hughes claire.hughes1@ucdconnect.ie
# March 2021
# *************************************************

# library
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(dplyr)

# Clear all objects from the workspace 
rm(list = ls()) 

# Set the working directory
 setwd("~/OneDrive - University College Dublin/4th Year/Final Year Project/Final R")


# Set the x axis labels for use in plots later
x_axis_labels_bar <- c("Neg. Control", "NP 24hr", "NP 48hr", "NP 72hr", "COOH-NP", "STS")
label_colors_bar = c("Control"="skyblue1", "24hr"="yellow1","48hr"="salmon2","72hr"="red2","COOH"="yellowgreen", "STS"="purple1")

x_axis_labels_jitter <- c("Neg. Control", "NP 24hr", "NP 48hr", "NP 72hr")

# Import data from the required csv file (comma seperated text file format) and save to a variable for further analysis

# Read in data and save to the dataframe / variable spheroid_master_data
spheroid_master_data <- read.csv ("~/OneDrive - University College Dublin/4th Year/Final Year Project/Final_Spheroid toxicity assay_exp3/Volumetric_final_exp3/Evaluation 13/R/Volmetric_final_exp3_spheroid level data_R.csv")

# Rename our columns for consistency
names(spheroid_master_data) <- c("Spheroid.Size", "Treatment", "PI", "Volume", "Surface.Area", "Sphericity")
view(spheroid_master_data)

# Create the master dataframe of all metrics, sizes and treatments for plotting later 
spheroid_master_bar <- subset(spheroid_master_data, select = c("Spheroid.Size", "Treatment", "PI", "Volume", "Surface.Area", "Sphericity"))

# Get rid of the other controls that we don't need to measure
#spheroid_master_bar_filtered<- subset(spheroid_master_bar, !(Treatment == "COOH" | Treatment == "STS"))

# Group each of the metrics by size and treatment and get the mean and standard deviation
spheroid_master_grouped = spheroid_master_bar %>%
  group_by(Spheroid.Size, Treatment) %>%
  summarise_each(funs(mean,sd))

# set the factor levels and order
spheroid_master_grouped$Treatment <- factor(spheroid_master_grouped$Treatment)
spheroid_master_grouped$Spheroid.Size <- factor(spheroid_master_grouped$Spheroid.Size)
# ensure levels of Treatment are in logical order
spheroid_master_grouped$Treatment <- ordered(spheroid_master_grouped$Treatment, levels = c("Control", "24hr", "48hr", "72hr", "COOH", "STS"))
spheroid_master_grouped$Spheroid.Size <- ordered(spheroid_master_grouped$Spheroid.Size, levels = c("Small Spheroid", "Medium Spheroid", "Large Spheroid"))



# Begin Analysis of variance tests for metric : PI 
# ------------------------------------------------------------------------------

# Testing group 1 : Small speheroids
# ---------------------------------------------------
# Step 1: Create the subset for small spehroids, treatments and PI 

PI_small <- subset(spheroid_master_data, Spheroid.Size == 'Small Spheroid', select = c("Spheroid.Size", "Treatment", "PI"))

# Get rid of the other controls that we don't need to measure
PI_small_filtered<- subset(PI_small, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(PI_small_filtered)

# check the factor levels of the treatment
PI_small_filtered$Treatment <- factor(PI_small_filtered$Treatment)
# ensure levels of Treatment are in logical order
PI_small_filtered$Treatment <- ordered(PI_small_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(PI_small_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average PI of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_small_PI <- aov(PI ~ Treatment, data = PI_small_filtered)

# Summary of the analysis
summary(anova_small_PI)
# Looks like significance here so we will do jitter plot.


# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_small_PI)
# Significance here with 24hr, 48hr, 72hr versus Control

# Get the maximum value for each spheroid size and add 10% for position of "n = " on graph
max_PI_small_ctrl = max(PI_small_filtered[PI_small_filtered$Treatment == 'Control',]$PI, na.rm = TRUE) * 1.1
max_PI_small_24 = max(PI_small_filtered[PI_small_filtered$Treatment == '24hr',]$PI, na.rm = TRUE) * 1.1
max_PI_small_48 = max(PI_small_filtered[PI_small_filtered$Treatment == '48hr',]$PI, na.rm = TRUE) * 1.1
max_PI_small_72 = max(PI_small_filtered[PI_small_filtered$Treatment == '72hr',]$PI, na.rm = TRUE) * 1.1

# Get the number of samples for each spheroid size group to go on the graph.
sample_size_small_ctrl = nrow(PI_small_filtered[PI_small_filtered$Treatment == 'Control',])
sample_size_small_24 = nrow(PI_small_filtered[PI_small_filtered$Treatment == '24hr',])
sample_size_small_48 = nrow(PI_small_filtered[PI_small_filtered$Treatment == '48hr',])
sample_size_small_72 = nrow(PI_small_filtered[PI_small_filtered$Treatment == '72hr',])

# Begin plotting significance levels
ggplot(PI_small_filtered, aes(x=Treatment, y=PI, colour=Treatment))+geom_boxplot(lwd=1,width = 0.5)+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Neg.Control", "NP 24hr", "NP 48hr", "NP 72hr"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "% PI Positive Nuclei per Small Spheroid as Impacted by Treatment Types", y= "% PI Positive Nuclei")+
  theme(plot.title = element_text(hjust = 1, size=12))+
  theme(axis.title.y = element_text(size=12.5))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size= 12))+
  theme(axis.text.x = element_text(size= 12))+
  #theme(axis.text.x =element_blank())+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(-10, max(PI_small_filtered$PI, na.rm = TRUE) * 1.1), breaks = seq(0, max(PI_small_filtered$PI, na.rm = TRUE) * 1, by = 10))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  scale_x_discrete(labels= x_axis_labels_jitter)
  # This is the code needed to write text onto the graph
  #annotate("text", x=1, y=max_PI_small_ctrl, label= paste("n =", sample_size_small_ctrl, sep=" ")) +
  #annotate("text", x=2, y=max_PI_small_24, label= paste("n =", sample_size_small_24, sep=" ")) +
  #annotate("text", x=3, y=max_PI_small_48, label= paste("n =", sample_size_small_48, sep=" ")) +
  #annotate("text", x=4, y=max_PI_small_72, label= paste("n =", sample_size_small_72, sep=" "))


# Black
ggplot(PI_small_filtered, aes(x=Treatment, y=PI, colour=Treatment))+geom_boxplot(lwd=1,width = 0.5, fill="black")+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Neg.Control", "NP 24hr", "NP 48hr", "NP 72hr"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "% PI Positive Nuclei per Small Spheroid as Impacted by Treatment Types", y= "% PI Positive Nuclei")+
  theme(plot.title = element_text(hjust = 1, size=12))+
  theme(axis.title.y = element_text(size=12.5))+
  theme(axis.title.x = element_blank())+
  #theme(axis.text.x =element_blank())+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(-10, max(PI_small_filtered$PI, na.rm = TRUE) * 1.1), breaks = seq(0, max(PI_small_filtered$PI, na.rm = TRUE) * 1, by = 10))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  scale_x_discrete(labels= x_axis_labels_jitter) +
# This is the code needed to write text onto the graph
#annotate("text", x=1, y=max_PI_small_ctrl, label= paste("n =", sample_size_small_ctrl, sep=" ")) +
#annotate("text", x=2, y=max_PI_small_24, label= paste("n =", sample_size_small_24, sep=" ")) +
#annotate("text", x=3, y=max_PI_small_48, label= paste("n =", sample_size_small_48, sep=" ")) +
#annotate("text", x=4, y=max_PI_small_72, label= paste("n =", sample_size_small_72, sep=" "))
theme(panel.grid = element_blank()) + 
  theme(panel.background = element_rect(fill = "black"))+
  theme(legend.title = element_blank())+
  theme(legend.key=element_rect(fill="black"))+
  theme(legend.background = element_rect(fill= "black"))+
  theme(legend.text = element_text(colour="white"))+
  theme(plot.background = element_rect(fill = "black"))+
  theme(axis.text.y = element_text(colour="white"))+
  theme(axis.text.x = element_text(colour="white"))+
  theme(title=element_text(colour ="white"))

# Testing group 2 : Medium speheroids
# ---------------------------------------------------
# Step 1: Create the subset for medium spehroids, treatments and PI


PI_medium <- subset(spheroid_master_data, Spheroid.Size == 'Medium Spheroid', select = c("Spheroid.Size", "Treatment", "PI"))

# Get rid of the other controls that we don't need to measure
PI_medium_filtered<- subset(PI_medium, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(PI_medium_filtered)

# check the factor levels of the treatment
PI_medium_filtered$Treatment <- factor(PI_medium_filtered$Treatment)
# ensure levels of Treatment are in logical order
PI_medium_filtered$Treatment <- ordered(PI_medium_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(PI_medium_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average PI of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_medium_PI <- aov(PI ~ Treatment, data = PI_medium_filtered)

# Summary of the analysis
summary(anova_medium_PI)
# Looks like significance here so we will do jitter plot.


# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_medium_PI)
# Significance here with 24hr, 48hr, 72hr versus Control

# Get the maximum value for each spheroid size and add 10% for position of "n = " on graph
max_PI_medium_ctrl = max(PI_medium_filtered[PI_medium_filtered$Treatment == 'Control',]$PI, na.rm = TRUE) * 1.1
max_PI_medium_24 = max(PI_medium_filtered[PI_medium_filtered$Treatment == '24hr',]$PI, na.rm = TRUE) * 1.1
max_PI_medium_48 = max(PI_medium_filtered[PI_medium_filtered$Treatment == '48hr',]$PI, na.rm = TRUE) * 1.1
max_PI_medium_72 = max(PI_medium_filtered[PI_medium_filtered$Treatment == '72hr',]$PI, na.rm = TRUE) * 1.1

# Get the number of samples for each spheroid size group to go on the graph.
sample_size_medium_ctrl = nrow(PI_medium_filtered[PI_medium_filtered$Treatment == 'Control',])
sample_size_medium_24 = nrow(PI_medium_filtered[PI_medium_filtered$Treatment == '24hr',])
sample_size_medium_48 = nrow(PI_medium_filtered[PI_medium_filtered$Treatment == '48hr',])
sample_size_medium_72 = nrow(PI_medium_filtered[PI_medium_filtered$Treatment == '72hr',])

# Begin plotting significance levels
ggplot(PI_medium_filtered, aes(x=Treatment, y=PI, colour=Treatment))+geom_boxplot(lwd=1,width = 0.5)+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Neg.Control", "NP 24hr", "NP 48hr", "NP 72hr"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "% PI Postive Nuclei per Medium Spheroid as Impacted by Treatment Types", y= "% PI Positive Nuclei")+
  theme(plot.title = element_text(hjust = 0.95, size=11.8))+
  theme(axis.title.y = element_text(size=12.5))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size= 12))+
  theme(axis.text.x = element_text(size= 12))+
  #theme(axis.text.x =element_blank())+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(-10, max(PI_medium_filtered$PI, na.rm = TRUE) * 1.1), breaks = seq(0, max(PI_medium_filtered$PI, na.rm = TRUE) * 1, by = 10))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  scale_x_discrete(labels= x_axis_labels_jitter)
 # This is the code needed to write text onto the graph
  #annotate("text", x=1, y=max_PI_medium_ctrl, label= paste("n =", sample_size_medium_ctrl, sep=" ")) +
  #annotate("text", x=2, y=max_PI_medium_24, label= paste("n =", sample_size_medium_24, sep=" ")) +
  #annotate("text", x=3, y=max_PI_medium_48, label= paste("n =", sample_size_medium_48, sep=" ")) +
  #annotate("text", x=4, y=max_PI_medium_72, label= paste("n =", sample_size_medium_72, sep=" "))


# Black
ggplot(PI_medium_filtered, aes(x=Treatment, y=PI, colour=Treatment))+geom_boxplot(lwd=1,width = 0.5, fill="black")+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Neg.Control", "NP 24hr", "NP 48hr", "NP 72hr"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "% PI Postive Nuclei per Medium Spheroid as Impacted by Treatment Types", y= "% PI Positive Nuclei")+
  theme(plot.title = element_text(hjust = 0.95, size=11.8))+
  theme(axis.title.y = element_text(size=12.5))+
  theme(axis.title.x = element_blank())+
  #theme(axis.text.x =element_blank())+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(-10, max(PI_medium_filtered$PI, na.rm = TRUE) * 1.1), breaks = seq(0, max(PI_medium_filtered$PI, na.rm = TRUE) * 1, by = 10))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  scale_x_discrete(labels= x_axis_labels_jitter) +
# This is the code needed to write text onto the graph
#annotate("text", x=1, y=max_PI_medium_ctrl, label= paste("n =", sample_size_medium_ctrl, sep=" ")) +
#annotate("text", x=2, y=max_PI_medium_24, label= paste("n =", sample_size_medium_24, sep=" ")) +
#annotate("text", x=3, y=max_PI_medium_48, label= paste("n =", sample_size_medium_48, sep=" ")) +
#annotate("text", x=4, y=max_PI_medium_72, label= paste("n =", sample_size_medium_72, sep=" "))
  theme(panel.grid = element_blank()) + 
  theme(panel.background = element_rect(fill = "black"))+
  theme(legend.title = element_blank())+
  theme(legend.key=element_rect(fill="black"))+
  theme(legend.background = element_rect(fill= "black"))+
  theme(legend.text = element_text(colour="white"))+
  theme(plot.background = element_rect(fill = "black"))+
  theme(axis.text.y = element_text(colour="white"))+
  theme(axis.text.x = element_text(colour="white"))+
  theme(title=element_text(colour ="white"))  
  
# Testing group 3 : Large speheroids
# ---------------------------------------------------
# Step 1: Create the subset for medium spehroids, treatments and PI

PI_large <- subset(spheroid_master_data, Spheroid.Size == 'Large Spheroid', select = c("Spheroid.Size", "Treatment", "PI"))

# Get rid of the other controls that we don't need to measure
PI_large_filtered<- subset(PI_large, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
 view(PI_large_filtered)

# check the factor levels of the treatment
PI_large_filtered$Treatment <- factor(PI_large_filtered$Treatment)
# ensure levels of Treatment are in logical order
PI_large_filtered$Treatment <- ordered(PI_large_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(PI_large_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average PI of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_large_PI <- aov(PI ~ Treatment, data = PI_large_filtered)

# Summary of the analysis
summary(anova_large_PI)
# Looks like significance here so we will do jitter plot.

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_large_PI)
# Significance here between 24hr and Control

# Get the maximum value for each spheroid size and add 10% for position of "n = " on graph
max_PI_large_ctrl = max(PI_large_filtered[PI_large_filtered$Treatment == 'Control',]$PI, na.rm = TRUE) * 1.1
max_PI_large_24 = max(PI_large_filtered[PI_large_filtered$Treatment == '24hr',]$PI, na.rm = TRUE) * 1.1
max_PI_large_48 = max(PI_large_filtered[PI_large_filtered$Treatment == '48hr',]$PI, na.rm = TRUE) * 1.1
max_PI_large_72 = max(PI_large_filtered[PI_large_filtered$Treatment == '72hr',]$PI, na.rm = TRUE) * 1.1

# Get the number of samples for each spheroid size group to go on the graph.
sample_size_large_ctrl = nrow(PI_large_filtered[PI_large_filtered$Treatment == 'Control',])
sample_size_large_24 = nrow(PI_large_filtered[PI_large_filtered$Treatment == '24hr',])
sample_size_large_48 = nrow(PI_large_filtered[PI_large_filtered$Treatment == '48hr',])
sample_size_large_72 = nrow(PI_large_filtered[PI_large_filtered$Treatment == '72hr',])

# Begin plotting significance levels
ggplot(PI_large_filtered, aes(x=Treatment, y=PI, colour=Treatment))+geom_boxplot(lwd=1,width = 0.5)+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Neg.Control", "NP 24hr", "NP 48hr", "NP 72hr"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "% PI Positive Nuclei per Large Spheroid as Impacted by Treatment Types", y= "% PI Positive Nuclei")+
  theme(plot.title = element_text(hjust = 1, size=12))+
  theme(axis.title.y = element_text(size=12.5))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size= 12))+
  theme(axis.text.x = element_text(size= 12))+
  #theme(axis.text.x =element_blank())+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(-10, max(PI_large_filtered$PI, na.rm = TRUE) * 1.1), breaks = seq(0, max(PI_large_filtered$PI, na.rm = TRUE) * 1, by = 10))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  scale_x_discrete(labels= x_axis_labels_jitter)
  # This is the code needed to write text onto the graph
  #annotate("text", x=1, y=max_PI_large_ctrl, label= paste("n =", sample_size_large_ctrl, sep=" ")) +
  #annotate("text", x=2, y=max_PI_large_24, label= paste("n =", sample_size_large_24, sep=" ")) +
  #annotate("text", x=3, y=max_PI_large_48, label= paste("n =", sample_size_large_48, sep=" ")) +
  #annotate("text", x=4, y=max_PI_large_72, label= paste("n =", sample_size_large_72, sep=" "))

# Black
ggplot(PI_large_filtered, aes(x=Treatment, y=PI, colour=Treatment))+geom_boxplot(lwd=1,width = 0.5, fill="black")+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Neg.Control", "NP 24hr", "NP 48hr", "NP 72hr"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "% PI Positive Nuclei per Large Spheroid as Impacted by Treatment Types", y= "% PI Positive Nuclei")+
  theme(plot.title = element_text(hjust = 1, size=12))+
  theme(axis.title.y = element_text(size=12.5))+
  theme(axis.title.x = element_blank())+
  #theme(axis.text.x =element_blank())+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(-10, max(PI_large_filtered$PI, na.rm = TRUE) * 1.1), breaks = seq(0, max(PI_large_filtered$PI, na.rm = TRUE) * 1, by = 10))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  scale_x_discrete(labels= x_axis_labels_jitter) +
# This is the code needed to write text onto the graph
#annotate("text", x=1, y=max_PI_large_ctrl, label= paste("n =", sample_size_large_ctrl, sep=" ")) +
#annotate("text", x=2, y=max_PI_large_24, label= paste("n =", sample_size_large_24, sep=" ")) +
#annotate("text", x=3, y=max_PI_large_48, label= paste("n =", sample_size_large_48, sep=" ")) +
#annotate("text", x=4, y=max_PI_large_72, label= paste("n =", sample_size_large_72, sep=" "))
  theme(panel.grid = element_blank()) + 
  theme(panel.background = element_rect(fill = "black"))+
  theme(legend.title = element_blank())+
  theme(legend.key=element_rect(fill="black"))+
  theme(legend.background = element_rect(fill= "black"))+
  theme(legend.text = element_text(colour="white"))+
  theme(plot.background = element_rect(fill = "black"))+
  theme(axis.text.y = element_text(colour="white"))+
  theme(axis.text.x = element_text(colour="white"))+
  theme(title=element_text(colour ="white"))    



# Creating the barplot facet grids for each spehroid size and each treatment for PI.Nuclei
ggplot(spheroid_master_grouped, aes(fill=Treatment, y=PI_mean, x=Treatment)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=PI_mean-PI_sd, ymax=PI_mean+PI_sd), width=.2,
                position=position_dodge(.9), color = "red") + 
  scale_fill_manual(breaks=x_axis_labels_bar, 
                    values=label_colors_bar) +
  # scale_fill_viridis(discrete = T, option = "D") +
  theme_bw() +
  ggtitle("Mean % PI Positive Nuclei of each Spheroid Size and Treatment Type") +
  facet_wrap(~Spheroid.Size) +
  theme(plot.title = element_text(hjust = 1, size=11.5))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.text.y = element_text(size= 12))+
  #  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12)) +
  ylab("Mean % PI Positive Nuclei") +
  scale_y_continuous(limits = c(0, max(spheroid_master_grouped$PI_mean, na.rm = TRUE) * 1.4), breaks = seq(0, max(spheroid_master_grouped$PI_mean, na.rm = TRUE) * 1.4, by = 10)) + #the scale is the largest value + 25% extra
  scale_x_discrete(labels= x_axis_labels_bar)


# Black
ggplot(spheroid_master_grouped, aes(fill=Treatment, y=PI_mean, x=Treatment)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=PI_mean-PI_sd, ymax=PI_mean+PI_sd), width=.2,
                position=position_dodge(.9), color = "red") + 
  scale_fill_manual(breaks=x_axis_labels_bar, 
                    values=label_colors_bar) +
  # scale_fill_viridis(discrete = T, option = "D") +
  theme_bw() +
  ggtitle("Mean % PI Positive Nuclei of each Spheroid Size and Treatment Type") +
  facet_wrap(~Spheroid.Size) +
  theme(plot.title = element_text(hjust = 1, size=11.5))+
  theme(axis.title.y = element_text(size=12))+
  #  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9)) +
  ylab("Mean % PI Positive Nuclei") +
  scale_y_continuous(limits = c(0, max(spheroid_master_grouped$PI_mean, na.rm = TRUE) * 1.4), breaks = seq(0, max(spheroid_master_grouped$PI_mean, na.rm = TRUE) * 1.4, by = 10)) + #the scale is the largest value + 25% extra
  scale_x_discrete(labels= x_axis_labels_bar) +
  # Black
  theme(panel.grid = element_blank()) + 
  theme(panel.background = element_rect(fill = "black"))+
  theme(legend.title = element_blank())+
  theme(legend.key=element_rect(fill="black"))+
  theme(legend.background = element_rect(fill= "black"))+
  theme(legend.text = element_text(colour="white"))+
  theme(plot.background = element_rect(fill = "black"))+
  theme(axis.text.y = element_text(colour="white"))+
  theme(axis.text.x = element_text(colour="white"))+
  theme(title=element_text(colour ="white"))

# End Analysis of variance tests for metric : PI
# -------------------------------------------------------------------------------------------------------------------


# Begin Analysis of variance tests for metric : Volume
# ------------------------------------------------------------------------------

# Testing group 1 : Small speheroids
# ---------------------------------------------------
# Step 1: Create the subset for small spehroids, treatments and Volume

Volume_small <- subset(spheroid_master_data, Spheroid.Size == 'Small Spheroid', select = c("Spheroid.Size", "Treatment", "Volume"))

# Get rid of the other controls that we don't need to measure
Volume_small_filtered<- subset(Volume_small, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(Volume_small_filtered)

# check the factor levels of the treatment
Volume_small_filtered$Treatment <- factor(Volume_small_filtered$Treatment)
# ensure levels of Treatment are in logical order
Volume_small_filtered$Treatment <- ordered(Volume_small_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(Volume_small_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average Volume of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_small_Volume <- aov(Volume ~ Treatment, data = Volume_small_filtered)

# Summary of the analysis
summary(anova_small_Volume)
# Looks like significance here so we will do jitter plot.

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_small_Volume)
# No significance 

# Testing group 2 : Medium speheroids
# ---------------------------------------------------
# Step 1: Create the subset for medium spehroids, treatments and Volume

Volume_medium <- subset(spheroid_master_data, Spheroid.Size == 'Medium Spheroid', select = c("Spheroid.Size", "Treatment", "Volume"))

# Get rid of the other controls that we don't need to measure
Volume_medium_filtered<- subset(Volume_medium, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(Volume_medium_filtered)

# check the factor levels of the treatment
Volume_medium_filtered$Treatment <- factor(Volume_medium_filtered$Treatment)
# ensure levels of Treatment are in logical order
Volume_medium_filtered$Treatment <- ordered(Volume_medium_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(Volume_medium_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average Volume of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_medium_Volume <- aov(Volume ~ Treatment, data = Volume_medium_filtered)

# Summary of the analysis
summary(anova_medium_Volume)
# No significance

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_medium_Volume)
# No significance

# Testing group 3 : Large speheroids
# ---------------------------------------------------
# Step 1: Create the subset for large spehroids, treatments and Volume

Volume_large <- subset(spheroid_master_data, Spheroid.Size == 'Large Spheroid', select = c("Spheroid.Size", "Treatment", "Volume"))

# Get rid of the other controls that we don't need to measure
Volume_large_filtered<- subset(Volume_large, !(Treatment == "COOH" | Treatment == "STS"))


# Quick check that data is ok
view(Volume_large_filtered)

# check the factor levels of the treatment
Volume_large_filtered$Treatment <- factor(Volume_large_filtered$Treatment)
# ensure levels of Treatment are in logical order
Volume_large_filtered$Treatment <- ordered(Volume_large_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(Volume_large_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average area of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_large_Volume <- aov(Volume ~ Treatment, data = Volume_large_filtered)

# Summary of the analysis
summary(anova_large_Volume)
# Looks like no significance here so we will not do jitter plot.

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_large_Volume)
# No significance here

# Creating the barplot facet grids for each spehroid size and each treatment for Volume
ggplot(spheroid_master_grouped, aes(fill=Treatment, y=Volume_mean, x=Treatment)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Volume_mean-Volume_sd, ymax=Volume_mean+Volume_sd), width=.2,
                position=position_dodge(.9), color = "red") + 
  scale_fill_manual(breaks=x_axis_labels_bar, 
                    values=label_colors_bar) +
  # scale_fill_viridis(discrete = T, option = "D") +
  theme_bw() +
  ggtitle("Mean Volume [µm³] of each Spheroid Size and Treatment Type") +
  facet_wrap(~Spheroid.Size) +
  #  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.45, size=11.5))+
  theme(axis.title.y = element_text(size=12.5))+
  theme(axis.text.y = element_text(size= 12))+
  theme(legend.position="none") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12)) +
  ylab("Mean Volume [µm³]") +
  scale_y_continuous(limits = c(0, max(spheroid_master_grouped$Volume_mean, na.rm = TRUE) * 1.4), breaks = seq(0, max(spheroid_master_grouped$Volume_mean, na.rm = TRUE) * 1.4, by = 50000)) + #the scale is the largest value + 25% extra
  scale_x_discrete(labels= x_axis_labels_bar)

# black
ggplot(spheroid_master_grouped, aes(fill=Treatment, y=Volume_mean, x=Treatment)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Volume_mean-Volume_sd, ymax=Volume_mean+Volume_sd), width=.2,
                position=position_dodge(.9), color = "red") + 
  scale_fill_manual(breaks=x_axis_labels_bar, 
                    values=label_colors_bar) +
  # scale_fill_viridis(discrete = T, option = "D") +
  theme_bw() +
  ggtitle("Mean Volume [µm³] of each Spheroid Size and Treatment Type") +
  facet_wrap(~Spheroid.Size) +
  #  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5, size=11.8))+
  theme(axis.title.y = element_text(size=12.5))+
  theme(legend.position="none") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9)) +
  ylab("Mean Volume [µm³]") +
  scale_y_continuous(limits = c(0, max(spheroid_master_grouped$Volume_mean, na.rm = TRUE) * 1.4), breaks = seq(0, max(spheroid_master_grouped$Volume_mean, na.rm = TRUE) * 1.4, by = 50000)) + #the scale is the largest value + 25% extra
  scale_x_discrete(labels= x_axis_labels_bar) +
  # black
theme(panel.grid = element_blank()) + 
  theme(panel.background = element_rect(fill = "black"))+
  theme(legend.title = element_blank())+
  theme(legend.key=element_rect(fill="black"))+
  theme(legend.background = element_rect(fill= "black"))+
  theme(legend.text = element_text(colour="white"))+
  theme(plot.background = element_rect(fill = "black"))+
  theme(axis.text.y = element_text(colour="white"))+
  theme(axis.text.x = element_text(colour="white"))+
  theme(title=element_text(colour ="white"))

# End Analysis of variance tests for metric : Volume
# -------------------------------------------------------------------------------------------------------------------


# Begin Analysis of variance tests for metric : Surface.Area
# ------------------------------------------------------------------------------

# Testing group 1 : Small speheroids
# ---------------------------------------------------
# Step 1: Create the subset for small spehroids, treatments and Surface.Area

Surface.Area_small <- subset(spheroid_master_data, Spheroid.Size == 'Small Spheroid', select = c("Spheroid.Size", "Treatment", "Surface.Area"))

# Get rid of the other controls that we don't need to measure
Surface.Area_small_filtered<- subset(Surface.Area_small, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(Surface.Area_small_filtered)

# check the factor levels of the treatment
Surface.Area_small_filtered$Treatment <- factor(Surface.Area_small_filtered$Treatment)
# ensure levels of Treatment are in logical order
Surface.Area_small_filtered$Treatment <- ordered(Surface.Area_small_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(Surface.Area_small_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average Surface.Area of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_small_Surface.Area <- aov(Surface.Area ~ Treatment, data = Surface.Area_small_filtered)

# Summary of the analysis
summary(anova_small_Surface.Area)
# No significance found 

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_small_Surface.Area)
# No significance found 

# Testing group 2 : Medium speheroids
# ---------------------------------------------------
# Step 1: Create the subset for medium spehroids, treatments and Surface.Area

Surface.Area_medium <- subset(spheroid_master_data, Spheroid.Size == 'Medium Spheroid', select = c("Spheroid.Size", "Treatment", "Surface.Area"))

# Get rid of the other controls that we don't need to measure
Surface.Area_medium_filtered<- subset(Surface.Area_medium, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(Surface.Area_medium_filtered)

# check the factor levels of the treatment
Surface.Area_medium_filtered$Treatment <- factor(Surface.Area_medium_filtered$Treatment)
# ensure levels of Treatment are in logical order
Surface.Area_medium_filtered$Treatment <- ordered(Surface.Area_medium_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(Surface.Area_medium_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average Surface.Area of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_medium_Surface.Area <- aov(Surface.Area ~ Treatment, data = Surface.Area_medium_filtered)

# Summary of the analysis
summary(anova_medium_Surface.Area)
# Looks like there is no significance here so we will not do jitter plot.

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_medium_Surface.Area)
# No significance found

# Testing group 3 : Large speheroids
# ---------------------------------------------------
# Step 1: Create the subset for medium spehroids, treatments and Surface.Area

Surface.Area_large <- subset(spheroid_master_data, Spheroid.Size == 'Large Spheroid', select = c("Spheroid.Size", "Treatment", "Surface.Area"))

# Get rid of the other controls that we don't need to measure
Surface.Area_large_filtered<- subset(Surface.Area_large, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(Surface.Area_medium_filtered)

# check the factor levels of the treatment
Surface.Area_large_filtered$Treatment <- factor(Surface.Area_large_filtered$Treatment)
# ensure levels of Treatment are in logical order
Surface.Area_large_filtered$Treatment <- ordered(Surface.Area_large_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(Surface.Area_large_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average area of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_large_Surface.Area <- aov(Surface.Area ~ Treatment, data = Surface.Area_large_filtered)

# Summary of the analysis
summary(anova_large_Surface.Area)
# Looks like no significance here so we will not do jitter plot.

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_large_Surface.Area)
# Again no significances here

# Creating the barplot facet grids for each spehroid size and each treatment for Area
ggplot(spheroid_master_grouped, aes(fill=Treatment, y=Surface.Area_mean, x=Treatment)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Surface.Area_mean-Surface.Area_sd, ymax=Surface.Area_mean+Surface.Area_sd), width=.2,
                position=position_dodge(.9), color = "red") + 
  scale_fill_manual(breaks=x_axis_labels_bar, 
                    values=label_colors_bar) +
  # scale_fill_viridis(discrete = T, option = "D") +
  theme_bw() +
  ggtitle("Mean Surface Area [µm²] of each Spheroid Size and Treatment Type") +
  facet_wrap(~Spheroid.Size) +
  theme(plot.title = element_text(hjust = 0.45, size=10.8))+
  theme(axis.title.y = element_text(size=12))+
  theme(axis.text.y = element_text(size= 12))+
  #  theme_ipsum() +
  theme(legend.position="none") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12)) +
  ylab("Mean Surface Area [µm²]") +
  scale_y_continuous(limits = c(0, max(spheroid_master_grouped$Surface.Area_mean, na.rm = TRUE) * 1.3), breaks = seq(0, max(spheroid_master_grouped$Surface.Area_mean, na.rm = TRUE) * 1.3, by = 10000)) + #the scale is the largest value + 25% extra
  scale_x_discrete(labels= x_axis_labels_bar)


# black
ggplot(spheroid_master_grouped, aes(fill=Treatment, y=Surface.Area_mean, x=Treatment)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Surface.Area_mean-Surface.Area_sd, ymax=Surface.Area_mean+Surface.Area_sd), width=.2,
                position=position_dodge(.9), color = "red") + 
  scale_fill_manual(breaks=x_axis_labels_bar, 
                    values=label_colors_bar) +
  # scale_fill_viridis(discrete = T, option = "D") +
  theme_bw() +
  ggtitle("Mean Surface Area [µm²] of each Spheroid Size and Treatment Type") +
  facet_wrap(~Spheroid.Size) +
  theme(plot.title = element_text(hjust = 0.55, size=11.4))+
  theme(axis.title.y = element_text(size=12))+
  #  theme_ipsum() +
  theme(legend.position="none") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9)) +
  ylab("Mean Surface Area [µm²]") +
  scale_y_continuous(limits = c(0, max(spheroid_master_grouped$Surface.Area_mean, na.rm = TRUE) * 1.3), breaks = seq(0, max(spheroid_master_grouped$Surface.Area_mean, na.rm = TRUE) * 1.3, by = 10000)) + #the scale is the largest value + 25% extra
  scale_x_discrete(labels= x_axis_labels_bar)+
  # black
  theme(panel.grid = element_blank()) + 
  theme(panel.background = element_rect(fill = "black"))+
  theme(legend.title = element_blank())+
  theme(legend.key=element_rect(fill="black"))+
  theme(legend.background = element_rect(fill= "black"))+
  theme(legend.text = element_text(colour="white"))+
  theme(plot.background = element_rect(fill = "black"))+
  theme(axis.text.y = element_text(colour="white"))+
  theme(axis.text.x = element_text(colour="white"))+
  theme(title=element_text(colour ="white"))

# End Analysis of variance tests for metric : Surface.Area
# -------------------------------------------------------------------------------------------------------------------


# Begin Analysis of variance tests for metric : Sphericity
# ------------------------------------------------------------------------------

# Testing group 1 : Small speheroids
# ---------------------------------------------------
# Step 1: Create the subset for small spehroids, treatments and Sphericity

Sphericity_small <- subset(spheroid_master_data, Spheroid.Size == 'Small Spheroid', select = c("Spheroid.Size", "Treatment", "Sphericity"))

# Get rid of the other controls that we don't need to measure
Sphericity_small_filtered<- subset(Sphericity_small, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(Sphericity_small_filtered)

# check the factor levels of the treatment
Sphericity_small_filtered$Treatment <- factor(Sphericity_small_filtered$Treatment)
# ensure levels of Treatment are in logical order
Sphericity_small_filtered$Treatment <- ordered(Sphericity_small_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(Sphericity_small_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average Sphericity of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_small_Sphericity <- aov(Sphericity ~ Treatment, data = Sphericity_small_filtered)

# Summary of the analysis
summary(anova_small_Sphericity)
# Significance Not found

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_small_Sphericity)
# Significance not found

# Testing group 2 : Medium speheroids
# ---------------------------------------------------
# Step 1: Create the subset for medium spehroids, treatments and Sphericity

Sphericity_medium <- subset(spheroid_master_data, Spheroid.Size == 'Medium Spheroid', select = c("Spheroid.Size", "Treatment", "Sphericity"))

# Get rid of the other controls that we don't need to measure
Sphericity_medium_filtered<- subset(Sphericity_medium, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(Sphericity_medium_filtered)

# check the factor levels of the treatment
Sphericity_medium_filtered$Treatment <- factor(Sphericity_medium_filtered$Treatment)
# ensure levels of Treatment are in logical order
Sphericity_medium_filtered$Treatment <- ordered(Sphericity_medium_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(Sphericity_medium_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average Sphericity of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_medium_Sphericity <- aov(Sphericity ~ Treatment, data = Sphericity_medium_filtered)

# Summary of the analysis
summary(anova_medium_Sphericity)
# Looks like there is no significance here so we will not do jitter plot.

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_medium_Sphericity)
# No significance found

# Testing group 3 : Large speheroids
# ---------------------------------------------------
# Step 1: Create the subset for large spehroids, treatments and Sphericity

Sphericity_large <- subset(spheroid_master_data, Spheroid.Size == 'Large Spheroid', select = c("Spheroid.Size", "Treatment", "Sphericity"))

# Get rid of the other controls that we don't need to measure
Sphericity_large_filtered<- subset(Sphericity_large, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(Sphericity_large_filtered)

# check the factor levels of the treatment
Sphericity_large_filtered$Treatment <- factor(Sphericity_large_filtered$Treatment)
# ensure levels of Treatment are in logical order
Sphericity_large_filtered$Treatment <- ordered(Sphericity_large_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(Sphericity_large_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average Sphericity of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_large_Sphericity <- aov(Sphericity ~ Treatment, data = Sphericity_large_filtered)

# Summary of the analysis
summary(anova_large_Sphericity)
# Looks like significance here so we will do jitter plot.

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_large_Sphericity)
 # Significance here between 24hr and Control

# Get the maximum value for each spheroid size and add 10% for position of "n = " on graph
max_Sphericity_large_ctrl = max(Sphericity_large_filtered[Sphericity_large_filtered$Treatment == 'Control',]$Sphericity, na.rm = TRUE) * 1.1
max_Sphericity_large_24 = max(Sphericity_large_filtered[Sphericity_large_filtered$Treatment == '24hr',]$Sphericity, na.rm = TRUE) * 1.1
max_Sphericity_large_48 = max(Sphericity_large_filtered[Sphericity_large_filtered$Treatment == '48hr',]$Sphericity, na.rm = TRUE) * 1.1
max_Sphericity_large_72 = max(Sphericity_large_filtered[Sphericity_large_filtered$Treatment == '72hr',]$Sphericity, na.rm = TRUE) * 1.1

# Get the number of samples for each spheroid size group to go on the graph.
sample_large_small_ctrl = nrow(Sphericity_large_filtered[Sphericity_large_filtered$Treatment == 'Control',])
sample_large_small_24 = nrow(Sphericity_large_filtered[Sphericity_large_filtered$Treatment == '24hr',])
sample_large_small_48 = nrow(Sphericity_large_filtered[Sphericity_large_filtered$Treatment == '48hr',])
sample_large_small_72 = nrow(Sphericity_large_filtered[Sphericity_large_filtered$Treatment == '72hr',])


# Begin plotting significance levels
ggplot(Sphericity_large_filtered, aes(x=Treatment, y=Sphericity, colour=Treatment))+geom_boxplot(lwd=1,width = 0.5)+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Neg.Control", "NP 24hr", "NP 48hr", "NP 72hr"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "Large Spheorid Sphericity as Impacted by Treatment Types", y= "Sphericity")+
  theme(plot.title = element_text(hjust = 2, size=14))+
  theme(axis.title.y = element_text(size=14))+
  theme(axis.text.y = element_text(size= 12))+
  theme(axis.text.x = element_text(size= 12))+
  theme(axis.title.x = element_blank())+
  #theme(axis.text.x =element_blank())+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(0, max(Sphericity_large_filtered$Sphericity, na.rm = TRUE) * 1.25), breaks = seq(0, max(Sphericity_large_filtered$Sphericity, na.rm = TRUE) * 1.25, by = 0.1))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  scale_x_discrete(labels= x_axis_labels_jitter)
# This is the code needed to write text onto the graph
  #annotate("text", x=1, y=max_Sphericity_large_ctrl, label= paste("n =", sample_size_large_ctrl, sep=" ")) +
  #annotate("text", x=2, y=max_Sphericity_large_24, label= paste("n =", sample_size_large_24, sep=" ")) +
  #annotate("text", x=3, y=max_Sphericity_large_48, label= paste("n =", sample_size_large_48, sep=" ")) +
  #annotate("text", x=4, y=max_Sphericity_large_72, label= paste("n =", sample_size_large_72, sep=" "))

# Black
ggplot(Sphericity_large_filtered, aes(x=Treatment, y=Sphericity, colour=Treatment))+geom_boxplot(lwd=1,width = 0.5, fill="black")+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Neg.Control", "NP 24hr", "NP 48hr", "NP 72hr"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "Large Spheorid Sphericity as Impacted by Treatment Types", y= "Sphericity")+
  theme(plot.title = element_text(hjust = 2, size=14))+
  theme(axis.title.y = element_text(size=14))+
  theme(axis.title.x = element_blank())+
  #theme(axis.text.x =element_blank())+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(0, max(Sphericity_large_filtered$Sphericity, na.rm = TRUE) * 1.25), breaks = seq(0, max(Sphericity_large_filtered$Sphericity, na.rm = TRUE) * 1.25, by = 0.1))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  scale_x_discrete(labels= x_axis_labels_jitter) +
# This is the code needed to write text onto the graph
#annotate("text", x=1, y=max_Sphericity_large_ctrl, label= paste("n =", sample_size_large_ctrl, sep=" ")) +
#annotate("text", x=2, y=max_Sphericity_large_24, label= paste("n =", sample_size_large_24, sep=" ")) +
#annotate("text", x=3, y=max_Sphericity_large_48, label= paste("n =", sample_size_large_48, sep=" ")) +
#annotate("text", x=4, y=max_Sphericity_large_72, label= paste("n =", sample_size_large_72, sep=" "))
  # black
  theme(panel.grid = element_blank()) + 
  theme(panel.background = element_rect(fill = "black"))+
  theme(legend.title = element_blank())+
  theme(legend.key=element_rect(fill="black"))+
  theme(legend.background = element_rect(fill= "black"))+
  theme(legend.text = element_text(colour="white"))+
  theme(plot.background = element_rect(fill = "black"))+
  theme(axis.text.y = element_text(colour="white"))+
  theme(axis.text.x = element_text(colour="white"))+
  theme(title=element_text(colour ="white"))
  
# Creating the barplot facet grids for each spehroid size and each treatment for Sphericity
ggplot(spheroid_master_grouped, aes(fill=Treatment, y=Sphericity_mean, x=Treatment)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Sphericity_mean-Sphericity_sd, ymax=Sphericity_mean+Sphericity_sd), width =.2,
                position=position_dodge(.9), color = "red") + 
  scale_fill_manual(breaks=x_axis_labels_bar, 
                    values=label_colors_bar) +
  # scale_fill_viridis(discrete = T, option = "D") +
  theme_bw() +
  ggtitle("Mean Sphericity of each Spheroid Size and Treatment Type") +
  facet_wrap(~Spheroid.Size) +
  theme(plot.title = element_text(hjust = 0.5, size=12.5))+
  theme(axis.title.y = element_text(size=12.5))+
  theme(axis.text.y = element_text(size= 12))+
  #  theme_ipsum() +
  theme(legend.position="none") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12)) +
  ylab("Mean Sphericity") +
  scale_y_continuous(limits = c(0, max(spheroid_master_grouped$Sphericity_mean, na.rm = TRUE) * 1.2), breaks = seq(0, max(spheroid_master_grouped$Sphericity_mean, na.rm = TRUE) * 1.2, by = 0.1)) + #the scale is the largest value + 25% extra
  scale_x_discrete(labels= x_axis_labels_bar)

# Black
ggplot(spheroid_master_grouped, aes(fill=Treatment, y=Sphericity_mean, x=Treatment)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Sphericity_mean-Sphericity_sd, ymax=Sphericity_mean+Sphericity_sd), width =.2,
                position=position_dodge(.9), color = "red") + 
  scale_fill_manual(breaks=x_axis_labels_bar, 
                    values=label_colors_bar) +
  # scale_fill_viridis(discrete = T, option = "D") +
  theme_bw() +
  ggtitle("Mean Sphericity of each Spheroid Size and Treatment Type") +
  facet_wrap(~Spheroid.Size) +
  theme(plot.title = element_text(hjust = 0.5, size=12.5))+
  theme(axis.title.y = element_text(size=12.5))+
  #  theme_ipsum() +
  theme(legend.position="none") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9)) +
  ylab("Mean Sphericity") +
  scale_y_continuous(limits = c(0, max(spheroid_master_grouped$Sphericity_mean, na.rm = TRUE) * 1.2), breaks = seq(0, max(spheroid_master_grouped$Sphericity_mean, na.rm = TRUE) * 1.2, by = 0.1)) + #the scale is the largest value + 25% extra
  scale_x_discrete(labels= x_axis_labels_bar) +
  # black
  theme(panel.grid = element_blank()) + 
  theme(panel.background = element_rect(fill = "black"))+
  theme(legend.title = element_blank())+
  theme(legend.key=element_rect(fill="black"))+
  theme(legend.background = element_rect(fill= "black"))+
  theme(legend.text = element_text(colour="white"))+
  theme(plot.background = element_rect(fill = "black"))+
  theme(axis.text.y = element_text(colour="white"))+
  theme(axis.text.x = element_text(colour="white"))+
  theme(title=element_text(colour ="white"))
