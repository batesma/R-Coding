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


# Import data from the required csv file (comma seperated text file format) and save to a variable for further analysis

# Read in data and save to the dataframe / variable 
spheroid_master_data <- read.csv("~/OneDrive - University College Dublin/4th Year/Final Year Project/Final_Spheroid toxicity assay_exp3/MaxPro_final_exp3/R/MaxPro_final_exp3_spheroid level data_R.csv")


# Set the x axis labels for use in plots later
x_axis_labels_bar <- c("Neg. Control", "NP 24hr", "NP 48hr", "NP 72hr", "COOH-NP", "STS")
label_colors_bar = c("Control"="skyblue1", "24hr"="yellow1","48hr"="salmon2","72hr"="red2","COOH"="yellowgreen", "STS"="purple1")

x_axis_labels_jitter <- c("Neg. Control", "NP 24hr", "NP 48hr", "NP 72hr")

# Rename our columns for consistency
names(spheroid_master_data) <- c("Spheroid.Size", "Treatment", "No. objects", "Area", "Roundness", "Width", "Length")
view(spheroid_master_data)

# Create the master dataframe of all metrics, sizes and treatments for plotting later 
spheroid_master_bar <- subset(spheroid_master_data, select = c("Spheroid.Size", "Treatment", "Area", "Roundness", "Width", "Length"))

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



# Begin Analysis of variance tests for metric : Area
# ------------------------------------------------------------------------------

# Testing group 1 : Small speheroids
# ---------------------------------------------------
# Step 1: Create the subset for small spehroids, treatments and Area

area_small <- subset(spheroid_master_data, Spheroid.Size == 'Small Spheroid', select = c("Spheroid.Size", "Treatment", "Area"))

# Get rid of the other controls that we don't need to measure
area_small_filtered<- subset(area_small, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(area_small_filtered)

# check the factor levels of the treatment
area_small_filtered$Treatment <- factor(area_small_filtered$Treatment)
# ensure levels of Treatment are in logical order
area_small_filtered$Treatment <- ordered(area_small_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(area_small_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average area of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_small_area <- aov(Area ~ Treatment, data = area_small_filtered)

# Summary of the analysis
summary(anova_small_area)
# Looks like no significance here so we will not do jitter plot.


# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_small_area)
# Again no significances here with 48hr being the closest to being significant but not making the cut

# Testing group 2 : Medium speheroids
# ---------------------------------------------------
# Step 1: Create the subset for medium spehroids, treatments and Area

area_medium <- subset(spheroid_master_data, Spheroid.Size == 'Medium Spheroid', select = c("Spheroid.Size", "Treatment", "Area"))

# Get rid of the other controls that we don't need to measure
area_medium_filtered<- subset(area_medium, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(area_medium_filtered)

# check the factor levels of the treatment
area_medium_filtered$Treatment <- factor(area_medium_filtered$Treatment)
# ensure levels of Treatment are in logical order
area_medium_filtered$Treatment <- ordered(area_medium_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(area_medium_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average area of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_medium_area <- aov(Area ~ Treatment, data = area_medium_filtered)

# Summary of the analysis
summary(anova_medium_area)
# Looks like no significance here so we will not do jitter plot.

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_medium_area)
# Again no significances here between the control and the treatments against one another

# Testing group 3 : Large speheroids
# ---------------------------------------------------
# Step 1: Create the subset for medium spehroids, treatments and Area

area_large <- subset(spheroid_master_data, Spheroid.Size == 'Large Spheroid', select = c("Spheroid.Size", "Treatment", "Area"))

# Get rid of the other controls that we don't need to measure
area_large_filtered<- subset(area_large, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(area_medium_filtered)

# check the factor levels of the treatment
area_large_filtered$Treatment <- factor(area_large_filtered$Treatment)
# ensure levels of Treatment are in logical order
area_large_filtered$Treatment <- ordered(area_large_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(area_large_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average area of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_large_area <- aov(Area ~ Treatment, data = area_large_filtered)

# Summary of the analysis
summary(anova_large_area)
# Looks like no significance here so we will not do jitter plot.

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_large_area)
# Again no significances here

# Creating the barplot facet grids for each spehroid size and each treatment for Area
ggplot(spheroid_master_grouped, aes(fill=Treatment, y=Area_mean, x=Treatment)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Area_mean-Area_sd, ymax=Area_mean+Area_sd), width=.2,
                position=position_dodge(.9), color = "red") + 
  scale_fill_manual(breaks=x_axis_labels_bar, 
                    values=label_colors_bar) +
  # scale_fill_viridis(discrete = T, option = "D") +
  theme_bw() +
  ggtitle("Mean Area [µm²] of each Spheroid Size and Treatment Type") +
  facet_wrap(~Spheroid.Size) +
  theme(plot.title = element_text(hjust = 0.5, size=12))+
  theme(axis.title.y = element_text(size=12.5))+
  theme(axis.text.y = element_text(size= 12))+
  #  theme_ipsum() +
  theme(legend.position="none") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12)) +
  ylab("Mean Area [µm²]") +
  scale_y_continuous(limits = c(0, max(spheroid_master_grouped$Area_mean, na.rm = TRUE) * 1.2), breaks = seq(0, max(spheroid_master_grouped$Area_mean, na.rm = TRUE) * 1.2, by = 2500)) + #the scale is the largest value + 25% extra
  scale_x_discrete(labels= x_axis_labels_bar)


# BLACK BACKGROUND
ggplot(spheroid_master_grouped, aes(fill=Treatment, y=Area_mean, x=Treatment)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Area_mean-Area_sd, ymax=Area_mean+Area_sd), width=.2,
                position=position_dodge(.9), color = "red") + 
  scale_fill_manual(breaks=x_axis_labels_bar, 
                    values=label_colors_bar) +
  # scale_fill_viridis(discrete = T, option = "D") +
  theme_bw() +
  ggtitle("Mean Area [µm²] of each Spheroid Size and Treatment Type") +
  facet_wrap(~Spheroid.Size) +
  theme(plot.title = element_text(hjust = 0.5, size=12))+
  theme(axis.title.y = element_text(size=12.5))+
  #  theme_ipsum() +
  theme(legend.position="none") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9, colour="white")) +
  ylab("Mean Area [µm²]") +
  scale_y_continuous(limits = c(0, max(spheroid_master_grouped$Area_mean, na.rm = TRUE) * 1.2), breaks = seq(0, max(spheroid_master_grouped$Area_mean, na.rm = TRUE) * 1.2, by = 2500)) + #the scale is the largest value + 25% extra
  scale_x_discrete(labels= x_axis_labels_bar) +
# This adds the black background
theme(panel.grid = element_blank()) + 
  theme(panel.background = element_rect(fill = "black"))+
  theme(legend.title = element_blank())+
  theme(legend.key=element_rect(fill="black"))+
  theme(legend.background = element_rect(fill= "black"))+
  theme(legend.text = element_text(colour="white"))+
  theme(plot.background = element_rect(fill = "black"))+
  theme(axis.text.y = element_text(colour="white"))+
  theme(title=element_text(colour ="white"))
# End Analysis of variance tests for metric : Area
# -------------------------------------------------------------------------------------------------------------------


# Begin Analysis of variance tests for metric : Roundness
# ------------------------------------------------------------------------------

# Testing group 1 : Small speheroids
# ---------------------------------------------------
# Step 1: Create the subset for small spehroids, treatments and Roundness

round_small <- subset(spheroid_master_data, Spheroid.Size == 'Small Spheroid', select = c("Spheroid.Size", "Treatment", "Roundness"))

# Get rid of the other controls that we don't need to measure
round_small_filtered<- subset(round_small, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(round_small_filtered)

# check the factor levels of the treatment
round_small_filtered$Treatment <- factor(round_small_filtered$Treatment)
# ensure levels of Treatment are in logical order
round_small_filtered$Treatment <- ordered(round_small_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(round_small_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average area of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_small_round <- aov(Roundness ~ Treatment, data = round_small_filtered)

# Summary of the analysis
summary(anova_small_round)
# Looks like no significance here so we will not do jitter plot.

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_small_round)
# Significance has been found between 72hr and control at alpha = 0.05 and alpha = 0.01 and 72hr and 24hr at alpha = 0.05 and alpha = 0.01

# Please check values for 24hr as looks like roundness has increased vs control????

# Get the maximum value for each spheroid size and add 10% for position of "n = " on graph
max_round_small_ctrl = max(round_small_filtered[round_small_filtered$Treatment == 'Control',]$Roundness, na.rm = TRUE) * 1.1
max_round_small_24 = max(round_small_filtered[round_small_filtered$Treatment == '24hr',]$Roundness, na.rm = TRUE) * 1.1
max_round_small_48 = max(round_small_filtered[round_small_filtered$Treatment == '48hr',]$Roundness, na.rm = TRUE) * 1.1
max_round_small_72 = max(round_small_filtered[round_small_filtered$Treatment == '72hr',]$Roundness, na.rm = TRUE) * 1.1

# Get the number of samples for each spheroid size group to go on the graph.
sample_size_small_ctrl = nrow(round_small_filtered[round_small_filtered$Treatment == 'Control',])
sample_size_small_24 = nrow(round_small_filtered[round_small_filtered$Treatment == '24hr',])
sample_size_small_48 = nrow(round_small_filtered[round_small_filtered$Treatment == '48hr',])
sample_size_small_72 = nrow(round_small_filtered[round_small_filtered$Treatment == '72hr',])


# Begin plotting significance levels

ggplot(round_small_filtered, aes(x=Treatment, y=Roundness, colour=Treatment))+geom_boxplot(lwd=1,width = 0.5)+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  #scale_color_viridis(discrete = TRUE, option = "D")+
  scale_color_discrete(breaks=c("Neg. Control", "NP 24hr", "NP 48hr", "NP 72hr"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "Small Spheorid Roundness as Impacted by Treatment Types", y= "Roundness")+
  theme(plot.title = element_text(hjust = 1.2, size=14))+
  theme(axis.title.y = element_text(size=14))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size= 12))+
  theme(axis.text.x = element_text(size= 12))+
  #theme(axis.text.x =element_blank())+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(0, max(round_small_filtered$Roundness, na.rm = TRUE) * 1.1), breaks = seq(0, max(round_small_filtered$Roundness, na.rm = TRUE) * 1.25, by = 0.2))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  scale_x_discrete(labels= x_axis_labels_jitter)
  # This is the code needed to write text onto the graph
  #annotate("text", x=1, y=max_round_small_ctrl, label= paste("n =", sample_size_small_ctrl, sep=" ")) +
  #annotate("text", x=2, y=max_round_small_24, label= paste("n =", sample_size_small_24, sep=" ")) +
  #annotate("text", x=3, y=max_round_small_48, label= paste("n =", sample_size_small_48, sep=" ")) +
  #annotate("text", x=4, y=max_round_small_72, label= paste("n =", sample_size_small_72, sep=" "))

# BLACK BACKGROUND
ggplot(round_small_filtered, aes(x=Treatment, y=Roundness, colour=Treatment))+geom_boxplot(lwd=1,width = 0.5, fill="black")+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Neg. Control", "NP 24hr", "NP 48hr", "NP 72hr"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "Small Spheorid Roundness as Impacted by Treatment Types", y= "Roundness")+
  theme(plot.title = element_text(hjust = 1.4, size=14, color='white'))+
  theme(axis.title.y = element_text(size=14))+
  theme(axis.title.x = element_blank())+
  #theme(axis.text.x =element_blank())+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(0, max(round_small_filtered$Roundness, na.rm = TRUE) * 1.1), breaks = seq(0, max(round_small_filtered$Roundness, na.rm = TRUE) * 1.25, by = 0.1))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  scale_x_discrete(labels= x_axis_labels_jitter) +
# This is the code needed to write text onto the graph
#annotate("text", x=1, y=max_round_small_ctrl, label= paste("n =", sample_size_small_ctrl, sep=" ")) +
#annotate("text", x=2, y=max_round_small_24, label= paste("n =", sample_size_small_24, sep=" ")) +
#annotate("text", x=3, y=max_round_small_48, label= paste("n =", sample_size_small_48, sep=" ")) +
#annotate("text", x=4, y=max_round_small_72, label= paste("n =", sample_size_small_72, sep=" "))
  # This adds the black background
  theme(panel.grid = element_blank()) + 
  theme(panel.background = element_rect(fill = "black"))+
  theme(legend.title = element_blank())+
  theme(legend.key=element_rect(fill="black"))+
  theme(legend.background = element_rect(fill= "black"))+
  theme(legend.text = element_text(colour="white"))+
  theme(plot.background = element_rect(fill = "black"))+
  theme(axis.text.y = element_text(colour="white"))+
  theme(axis.text.x = element_text(color='white')) +
  theme(title=element_text(colour ="white"))
  
# Testing group 2 : Medium speheroids
# ---------------------------------------------------
# Step 1: Create the subset for medium spehroids, treatments and Roundness

round_medium <- subset(spheroid_master_data, Spheroid.Size == 'Medium Spheroid', select = c("Spheroid.Size", "Treatment", "Roundness"))

# Get rid of the other controls that we don't need to measure
round_medium_filtered<- subset(round_medium, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(round_medium_filtered)

# check the factor levels of the treatment
round_medium_filtered$Treatment <- factor(round_medium_filtered$Treatment)
# ensure levels of Treatment are in logical order
round_medium_filtered$Treatment <- ordered(round_medium_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(round_medium_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average area of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_medium_round <- aov(Roundness ~ Treatment, data = round_medium_filtered)

# Summary of the analysis
summary(anova_medium_round)
# Looks like there is significance here so we will do jitter plot.

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_medium_round)
# Significance here actually lies between 48hr and 24hr but is only at the alpha 0.1 level.
# We will do a jitter anyway, but there is not enough evidence to say treatment is different from control.

# Get the maximum value for each spheroid size and add 10% for position of "n = " on graph
max_round_med_ctrl = max(round_medium_filtered[round_medium_filtered$Treatment == 'Control',]$Roundness, na.rm = TRUE) * 1.1
max_round_med_24 = max(round_medium_filtered[round_medium_filtered$Treatment == '24hr',]$Roundness, na.rm = TRUE) * 1.1
max_round_med_48 = max(round_medium_filtered[round_medium_filtered$Treatment == '48hr',]$Roundness, na.rm = TRUE) * 1.1
max_round_med_72 = max(round_medium_filtered[round_medium_filtered$Treatment == '72hr',]$Roundness, na.rm = TRUE) * 1.1

# Get the number of samples for each spheroid size group to go on the graph.
sample_size_med_ctrl = nrow(round_medium_filtered[round_medium_filtered$Treatment == 'Control',])
sample_size_med_24 = nrow(round_medium_filtered[round_medium_filtered$Treatment == '24hr',])
sample_size_med_48 = nrow(round_medium_filtered[round_medium_filtered$Treatment == '48hr',])
sample_size_med_72 = nrow(round_medium_filtered[round_medium_filtered$Treatment == '72hr',])


# Begin plotting significance levels
ggplot(round_medium_filtered, aes(x=Treatment, y=Roundness, colour=Treatment))+geom_boxplot(lwd=1,width = 0.5)+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Neg. Control", "NP 24Hr", "NP 48hr", "NP 72hr"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "Medium Spheorid Roundness as Impacted by Treatment Types", y= "Roundness")+
  theme(plot.title = element_text(hjust = 0.2, size=12))+
  theme(axis.title.x = element_blank())+
  #theme(axis.text.x =element_blank())+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(0, max(round_medium_filtered$Roundness, na.rm = TRUE) * 1.1), breaks = seq(0, max(round_medium_filtered$Roundness, na.rm = TRUE) * 1.25, by = 0.1))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  scale_x_discrete(labels= x_axis_labels_jitter)
# This is the code needed to write text onto the graph
  #annotate("text", x=1, y=max_round_med_ctrl, label= paste("n =", sample_size_med_ctrl, sep=" ")) +
  #annotate("text", x=2, y=max_round_med_24, label= paste("n =", sample_size_med_24, sep=" ")) +
  #annotate("text", x=3, y=max_round_med_48, label= paste("n =", sample_size_med_48, sep=" ")) +
  #annotate("text", x=4, y=max_round_med_72, label= paste("n =", sample_size_med_72, sep=" "))

# BLACK BACKGROUND
ggplot(round_medium_filtered, aes(x=Treatment, y=Roundness, colour=Treatment))+geom_boxplot(lwd=1,width = 0.5, fill="black")+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Neg. Control", "NP 24Hr", "NP 48hr", "NP 72hr"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "Medium Spheorid Roundness as Impacted by Treatment Types", y= "Roundness")+
  theme(plot.title = element_text(hjust = 0.2, size=12))+
  theme(axis.title.x = element_blank())+
  #theme(axis.text.x =element_blank())+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(0, max(round_medium_filtered$Roundness, na.rm = TRUE) * 1.1), breaks = seq(0, max(round_medium_filtered$Roundness, na.rm = TRUE) * 1.25, by = 0.1))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  scale_x_discrete(labels= x_axis_labels_jitter) +
# This is the code needed to write text onto the graph
#annotate("text", x=1, y=max_round_med_ctrl, label= paste("n =", sample_size_med_ctrl, sep=" ")) +
#annotate("text", x=2, y=max_round_med_24, label= paste("n =", sample_size_med_24, sep=" ")) +
#annotate("text", x=3, y=max_round_med_48, label= paste("n =", sample_size_med_48, sep=" ")) +
#annotate("text", x=4, y=max_round_med_72, label= paste("n =", sample_size_med_72, sep=" "))
# This adds the black background
theme(panel.grid = element_blank()) + 
  theme(panel.background = element_rect(fill = "black"))+
  theme(legend.title = element_blank())+
  theme(legend.key=element_rect(fill="black"))+
  theme(legend.background = element_rect(fill= "black"))+
  theme(legend.text = element_text(colour="white"))+
  theme(plot.background = element_rect(fill = "black"))+
  theme(axis.text.y = element_text(colour="white"))+
  theme(axis.text.x = element_text(color='white')) +
  theme(title=element_text(colour ="white"))

# Testing group 3 : Large speheroids
# ---------------------------------------------------
# Step 1: Create the subset for large spehroids, treatments and Roundness

round_large <- subset(spheroid_master_data, Spheroid.Size == 'Large Spheroid', select = c("Spheroid.Size", "Treatment", "Roundness"))

# Get rid of the other controls that we don't need to measure
round_large_filtered<- subset(round_large, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(area_medium_filtered)

# check the factor levels of the treatment
round_large_filtered$Treatment <- factor(round_large_filtered$Treatment)
# ensure levels of Treatment are in logical order
round_large_filtered$Treatment <- ordered(round_large_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(round_large_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average area of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_large_round <- aov(Roundness ~ Treatment, data = round_large_filtered)

# Summary of the analysis
summary(anova_large_round)
# Looks like no significance here so we will not do jitter plot.

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_large_round)
# Again no significances here


# Creating the barplot facet grids for each spehroid size and each treatment for Area
ggplot(spheroid_master_grouped, aes(fill=Treatment, y=Roundness_mean, x=Treatment)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Roundness_mean-Roundness_sd, ymax=Roundness_mean+Roundness_sd), width=.2,
                position=position_dodge(.9), color = "red") + 
  scale_fill_manual(breaks=x_axis_labels_bar, 
                    values=label_colors_bar) +
  # scale_fill_viridis(discrete = T, option = "D") +
  theme_bw() +
  ggtitle("Mean Roundness of each Spheroid Size and Treatment Type") +
  facet_wrap(~Spheroid.Size) +
  theme(plot.title = element_text(hjust = 0.5, size=12))+
  theme(axis.title.y = element_text(size=12.5))+
  theme(axis.text.y = element_text(size= 12))+
  #  theme_ipsum() +
  theme(legend.position="none") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12)) +
  ylab("Mean Roundness") +
  scale_y_continuous(limits = c(0, max(spheroid_master_grouped$Roundness_mean, na.rm = TRUE) * 1.25), breaks = seq(0, max(spheroid_master_grouped$Roundness_mean, na.rm = TRUE) * 1.2, by = 0.1)) + #the scale is the largest value + 25% extra
  scale_x_discrete(labels= x_axis_labels_bar)


# BLACK BACKGROUND
ggplot(spheroid_master_grouped, aes(fill=Treatment, y=Roundness_mean, x=Treatment)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Roundness_mean-Roundness_sd, ymax=Roundness_mean+Roundness_sd), width=.2,
                position=position_dodge(.9), color = "red") + 
  scale_fill_manual(breaks=x_axis_labels_bar, 
                    values=label_colors_bar) +
  # scale_fill_viridis(discrete = T, option = "D") +
  theme_bw() +
  ggtitle("Mean Roundness of each Spheroid Size and Treatment Type") +
  facet_wrap(~Spheroid.Size) +
  theme(plot.title = element_text(hjust = 0.5, size=12.5))+
  theme(axis.title.y = element_text(size=12.5))+
  #  theme_ipsum() +
  theme(legend.position="none") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9, color='white')) +
  ylab("Mean Roundness") +
  scale_y_continuous(limits = c(0, max(spheroid_master_grouped$Roundness_mean, na.rm = TRUE) * 1.25), breaks = seq(0, max(spheroid_master_grouped$Roundness_mean, na.rm = TRUE) * 1.2, by = 0.1)) + #the scale is the largest value + 25% extra
  scale_x_discrete(labels= x_axis_labels_bar) +
  # THis adds black background
  theme(panel.grid = element_blank()) + 
  theme(panel.background = element_rect(fill = "black"))+
  theme(legend.title = element_blank())+
  theme(legend.key=element_rect(fill="black"))+
  theme(legend.background = element_rect(fill= "black"))+
  theme(legend.text = element_text(colour="white"))+
  theme(plot.background = element_rect(fill = "black"))+
  theme(axis.text.y = element_text(colour="white"))+
  theme(title=element_text(colour ="white"))

# End Analysis of variance tests for metric : Roundness
# -------------------------------------------------------------------------------------------------------------------


# Begin Analysis of variance tests for metric : Width
# ------------------------------------------------------------------------------

# Testing group 1 : Small speheroids
# ---------------------------------------------------
# Step 1: Create the subset for small spehroids, treatments and Width

width_small <- subset(spheroid_master_data, Spheroid.Size == 'Small Spheroid', select = c("Spheroid.Size", "Treatment", "Width"))

# Get rid of the other controls that we don't need to measure
width_small_filtered<- subset(width_small, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(width_small_filtered)

# check the factor levels of the treatment
width_small_filtered$Treatment <- factor(width_small_filtered$Treatment)
# ensure levels of Treatment are in logical order
width_small_filtered$Treatment <- ordered(width_small_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(width_small_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average area of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_small_width <- aov(Width ~ Treatment, data = width_small_filtered)

# Summary of the analysis
summary(anova_small_width)
# Significance found 

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_small_width)
# Significance found at the level aplha = 0.05 between 48hr and 24hr and 0.05 between 72hr and 24hr 

# Get the maximum value for each spheroid size and add 10% for position of "n = " on graph
max_width_small_ctrl = max(width_small_filtered[width_small_filtered$Treatment == 'Control',]$Width, na.rm = TRUE) * 1.1
max_width_small_24 = max(width_small_filtered[width_small_filtered$Treatment == '24hr',]$Width, na.rm = TRUE) * 1.1
max_width_small_48 = max(width_small_filtered[width_small_filtered$Treatment == '48hr',]$Width, na.rm = TRUE) * 1.1
max_width_small_72 = max(width_small_filtered[width_small_filtered$Treatment == '72hr',]$Width, na.rm = TRUE) * 1.1

# Get the number of samples for each spheroid size group to go on the graph.
sample_size_small_ctrl = nrow(width_small_filtered[width_small_filtered$Treatment == 'Control',])
sample_size_small_24 = nrow(width_small_filtered[width_small_filtered$Treatment == '24hr',])
sample_size_small_48 = nrow(width_small_filtered[width_small_filtered$Treatment == '48hr',])
sample_size_small_72 = nrow(width_small_filtered[width_small_filtered$Treatment == '72hr',])

# Begin plotting significance levels
ggplot(width_small_filtered, aes(x=Treatment, y=Width, colour=Treatment))+geom_boxplot(lwd=1,width = 0.5)+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Neg. Control", "NP 24hr", "NP 48hr", "NP 72hr"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "Small Spheorid Width [µm] as Impacted by Treatment Types", y= "Width [µm]")+
  theme(plot.title = element_text(hjust = 1.7, size=14))+
  theme(axis.title.y = element_text(size=14))+
  theme(axis.text.y = element_text(size= 12))+
  theme(axis.text.x = element_text(size= 12))+
  theme(axis.title.x = element_blank())+
  #theme(axis.text.x =element_blank())+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(0, max(width_small_filtered$Width, na.rm = TRUE) * 1.1), breaks = seq(0, max(width_small_filtered$Width, na.rm = TRUE) * 1.25, by = 10))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  scale_x_discrete(labels= x_axis_labels_jitter)
  # This is the code needed to write text onto the graph
  #annotate("text", x=1, y=max_width_small_ctrl, label= paste("n =", sample_size_small_ctrl, sep=" ")) +
  #annotate("text", x=2, y=max_width_small_24, label= paste("n =", sample_size_small_24, sep=" ")) +
  #annotate("text", x=3, y=max_width_small_48, label= paste("n =", sample_size_small_48, sep=" ")) +
  #annotate("text", x=4, y=max_width_small_72, label= paste("n =", sample_size_small_72, sep=" "))

# BLACK BACKGROUND
ggplot(width_small_filtered, aes(x=Treatment, y=Width, colour=Treatment))+geom_boxplot(lwd=1,width = 0.5, fill="black")+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Neg. Control", "NP 24hr", "NP 48hr", "NP 72hr"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "Small Spheorid Width [µm] as Impacted by Treatment Types", y= "Width [µm]")+
  theme(plot.title = element_text(hjust = 1.7, size=14))+
  theme(axis.title.y = element_text(size=14))+
  theme(axis.title.x = element_blank())+
  #theme(axis.text.x =element_blank())+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(0, max(width_small_filtered$Width, na.rm = TRUE) * 1.1), breaks = seq(0, max(width_small_filtered$Width, na.rm = TRUE) * 1.25, by = 10))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  scale_x_discrete(labels= x_axis_labels_jitter) +
# This is the code needed to write text onto the graph
#annotate("text", x=1, y=max_width_small_ctrl, label= paste("n =", sample_size_small_ctrl, sep=" ")) +
#annotate("text", x=2, y=max_width_small_24, label= paste("n =", sample_size_small_24, sep=" ")) +
#annotate("text", x=3, y=max_width_small_48, label= paste("n =", sample_size_small_48, sep=" ")) +
#annotate("text", x=4, y=max_width_small_72, label= paste("n =", sample_size_small_72, sep=" "))
  # This adds the black background
  theme(panel.grid = element_blank()) + 
  theme(panel.background = element_rect(fill = "black"))+
  theme(legend.title = element_blank())+
  theme(legend.key=element_rect(fill="black"))+
  theme(legend.background = element_rect(fill= "black"))+
  theme(legend.text = element_text(colour="white"))+
  theme(plot.background = element_rect(fill = "black"))+
  theme(axis.text.y = element_text(colour="white"))+
  theme(axis.text.x = element_text(color='white')) +
  theme(title=element_text(colour ="white"))
  
# Testing group 2 : Medium speheroids
# ---------------------------------------------------
# Step 1: Create the subset for medium spehroids, treatments and Width

width_medium <- subset(spheroid_master_data, Spheroid.Size == 'Medium Spheroid', select = c("Spheroid.Size", "Treatment", "Width"))

# Get rid of the other controls that we don't need to measure
width_medium_filtered<- subset(width_medium, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(width_medium_filtered)

# check the factor levels of the treatment
width_medium_filtered$Treatment <- factor(width_medium_filtered$Treatment)
# ensure levels of Treatment are in logical order
width_medium_filtered$Treatment <- ordered(width_medium_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(width_medium_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average area of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_medium_width <- aov(Width ~ Treatment, data = width_medium_filtered)

# Summary of the analysis
summary(anova_medium_width)
# Looks like there is no significance here so we will not do jitter plot.

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_medium_width)
# No significance found

# Testing group 3 : Large speheroids
# ---------------------------------------------------
# Step 1: Create the subset for medium spehroids, treatments and Width

width_large <- subset(spheroid_master_data, Spheroid.Size == 'Large Spheroid', select = c("Spheroid.Size", "Treatment", "Width"))

# Get rid of the other controls that we don't need to measure
width_large_filtered<- subset(width_large, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(area_medium_filtered)

# check the factor levels of the treatment
width_large_filtered$Treatment <- factor(width_large_filtered$Treatment)
# ensure levels of Treatment are in logical order
width_large_filtered$Treatment <- ordered(width_large_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(width_large_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average area of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_large_width <- aov(Width ~ Treatment, data = width_large_filtered)

# Summary of the analysis
summary(anova_large_width)
# Looks like no significance here so we will not do jitter plot.

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_large_width)
# Again no significances here

# Creating the barplot facet grids for each spehroid size and each treatment for Width
ggplot(spheroid_master_grouped, aes(fill=Treatment, y=Width_mean, x=Treatment)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Width_mean-Width_sd, ymax=Width_mean+Width_sd), width=.2,
                position=position_dodge(.9), color = "red") + 
  scale_fill_manual(breaks=x_axis_labels_bar, 
                    values=label_colors_bar) +
  # scale_fill_viridis(discrete = T, option = "") +
  theme_bw() +
  ggtitle("Mean Width [µm] of each Spheroid Size and Treatment Type") +
  facet_wrap(~Spheroid.Size) +
  theme(plot.title = element_text(hjust = 0.5, size=12.5))+
  theme(axis.title.y = element_text(size=12.5))+
  theme(axis.text.y = element_text(size= 12))+
  #  theme_ipsum() +
  theme(legend.position="none") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12)) +
  ylab("Mean Width [µm]") +
  scale_y_continuous(limits = c(0, max(spheroid_master_grouped$Width_mean, na.rm = TRUE) * 1.2), breaks = seq(0, max(spheroid_master_grouped$Width_mean, na.rm = TRUE) * 1.2, by = 10)) + #the scale is the largest value + 25% extra
  scale_x_discrete(labels= x_axis_labels_bar)


# BLACK BACKGROUND
ggplot(spheroid_master_grouped, aes(fill=Treatment, y=Width_mean, x=Treatment)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Width_mean-Width_sd, ymax=Width_mean+Width_sd), width=.2,
                position=position_dodge(.9), color = "red") + 
  scale_fill_manual(breaks=x_axis_labels_bar, 
                    values=label_colors_bar) +
  # scale_fill_viridis(discrete = T, option = "D") +
  theme_bw() +
  ggtitle("Mean Width [µm] of each Spheroid Size and Treatment Type") +
  facet_wrap(~Spheroid.Size) +
  theme(plot.title = element_text(hjust = 0.5, size=12.5))+
  theme(axis.title.y = element_text(size=12.5))+
  #  theme_ipsum() +
  theme(legend.position="none") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9, color = 'white')) +
  ylab("Mean Width [µm]") +
  scale_y_continuous(limits = c(0, max(spheroid_master_grouped$Width_mean, na.rm = TRUE) * 1.2), breaks = seq(0, max(spheroid_master_grouped$Width_mean, na.rm = TRUE) * 1.2, by = 10)) + #the scale is the largest value + 25% extra
  scale_x_discrete(labels= x_axis_labels_bar) +
  # THis adds black background
  theme(panel.grid = element_blank()) + 
  theme(panel.background = element_rect(fill = "black"))+
  theme(legend.title = element_blank())+
  theme(legend.key=element_rect(fill="black"))+
  theme(legend.background = element_rect(fill= "black"))+
  theme(legend.text = element_text(colour="white"))+
  theme(plot.background = element_rect(fill = "black"))+
  theme(axis.text.y = element_text(colour="white"))+
  theme(title=element_text(colour ="white"))
# End Analysis of variance tests for metric : Width
# -------------------------------------------------------------------------------------------------------------------

## DONT NEED LENGTH****

# Begin Analysis of variance tests for metric : Length
# ------------------------------------------------------------------------------

# Testing group 1 : Small speheroids
# ---------------------------------------------------
# Step 1: Create the subset for small spehroids, treatments and Length

length_small <- subset(spheroid_master_data, Spheroid.Size == 'Small Spheroid', select = c("Spheroid.Size", "Treatment", "Length"))

# Get rid of the other controls that we don't need to measure
length_small_filtered<- subset(length_small, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(length_small_filtered)

# check the factor levels of the treatment
length_small_filtered$Treatment <- factor(length_small_filtered$Treatment)
# ensure levels of Treatment are in logical order
length_small_filtered$Treatment <- ordered(length_small_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(length_small_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average area of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_small_length <- aov(Length ~ Treatment, data = length_small_filtered)

# Summary of the analysis
summary(anova_small_length)
# Significance Not found

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_small_length)
# Significance not found

# Testing group 2 : Medium speheroids
# ---------------------------------------------------
# Step 1: Create the subset for medium spehroids, treatments and Length

length_medium <- subset(spheroid_master_data, Spheroid.Size == 'Medium Spheroid', select = c("Spheroid.Size", "Treatment", "Length"))

# Get rid of the other controls that we don't need to measure
length_medium_filtered<- subset(length_medium, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(length_medium_filtered)

# check the factor levels of the treatment
length_medium_filtered$Treatment <- factor(length_medium_filtered$Treatment)
# ensure levels of Treatment are in logical order
length_medium_filtered$Treatment <- ordered(length_medium_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(length_medium_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average area of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_medium_length <- aov(Length ~ Treatment, data = length_medium_filtered)

# Summary of the analysis
summary(anova_medium_length)
# Looks like there is no significance here so we will not do jitter plot.

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_medium_length)
# No significance found

# Testing group 3 : Large speheroids
# ---------------------------------------------------
# Step 1: Create the subset for medium spehroids, treatments and Length

length_large <- subset(spheroid_master_data, Spheroid.Size == 'Large Spheroid', select = c("Spheroid.Size", "Treatment", "Length"))

# Get rid of the other controls that we don't need to measure
length_large_filtered<- subset(length_large, !(Treatment == "COOH" | Treatment == "STS"))

# Quick check that data is ok
view(length_large_filtered)

# check the factor levels of the treatment
length_large_filtered$Treatment <- factor(length_large_filtered$Treatment)
# ensure levels of Treatment are in logical order
length_large_filtered$Treatment <- ordered(length_large_filtered$Treatment, levels = c("Control", "24hr", "48hr", "72hr"))
levels(length_large_filtered$Treatment)

# Step 2: We want to know if there is any significant difference between the average area of 
# spheroids in the 3 experimental treatments and the control group.
# For this we will use a one-way anova test

# Compute the analysis of variance
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don't know which pairs of groups are different.

anova_large_length <- aov(Length ~ Treatment, data = length_large_filtered)

# Summary of the analysis
summary(anova_large_length)
# Looks like no significance here so we will not do jitter plot.

# It's possible to perform multiple pairwise-comparison, to determine if the mean difference between 
# specific pairs of group are statistically significant.
# When the ANOVA test is significant, we can compute Tukey HSD (Tukey Honest Significant Differences
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(anova_large_length)
# Again no significances here


# Creating the barplot facet grids for each spehroid size and each treatment for Length
ggplot(spheroid_master_grouped, aes(fill=Treatment, y=Length_mean, x=Treatment)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Length_mean-Length_sd, ymax=Length_mean+Length_sd), width =.2,
                position=position_dodge(.9), color = "red") + 
  scale_fill_manual(breaks=x_axis_labels_bar, 
                    values=label_colors_bar) +
  # scale_fill_viridis(discrete = T, option = "D") +
  theme_bw() +
  ggtitle("Barplot of Comparison of Mean Length [µm] of each Spheroid Size and Treatment Type") +
  facet_wrap(~Spheroid.Size) +
  theme(plot.title = element_text(hjust = 0.2, size=9))+
  #  theme_ipsum() +
  theme(legend.position="none") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8))+
  ylab("Mean Length [µm] of Spheroids") +
  scale_y_continuous(limits = c(0, max(spheroid_master_grouped$Length_mean, na.rm = TRUE) * 1.2), breaks = seq(0, max(spheroid_master_grouped$Length_mean, na.rm = TRUE) * 1.2, by = 10)) + #the scale is the largest value + 25% extra
  scale_x_discrete(labels= x_axis_labels_bar)

  
# Creating the barplot facet grids for each spehroid size and each treatment for Length
ggplot(spheroid_master_grouped, aes(fill=Treatment, y=Length_mean, x=Treatment)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Length_mean-Length_sd, ymax=Length_mean+Length_sd), width =.2,
                position=position_dodge(.9), color = "red") + 
  scale_fill_manual(breaks=x_axis_labels_bar, 
                    values=label_colors_bar) +
  # scale_fill_viridis(discrete = T, option = "C") +
  theme_bw() +
  ggtitle("Barplot of Comparison of Mean Length [µm] of each Spheroid Size and Treatment Type") +
  facet_wrap(~Spheroid.Size) +
  theme(plot.title = element_text(hjust = 0.2, size=9))+
  #  theme_ipsum() +
  theme(legend.position="none") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8, color='white'))+
  ylab("Mean Length [µm] of Spheroids") +
  scale_y_continuous(limits = c(0, max(spheroid_master_grouped$Length_mean, na.rm = TRUE) * 1.2), breaks = seq(0, max(spheroid_master_grouped$Length_mean, na.rm = TRUE) * 1.2, by = 10)) + #the scale is the largest value + 25% extra
  scale_x_discrete(labels= x_axis_labels_bar) +
  # THis adds black background
  theme(panel.grid = element_blank()) + 
  theme(panel.background = element_rect(fill = "black"))+
  theme(legend.title = element_blank())+
  theme(legend.key=element_rect(fill="black"))+
  theme(legend.background = element_rect(fill= "black"))+
  theme(legend.text = element_text(colour="white"))+
  theme(plot.background = element_rect(fill = "black"))+
  theme(axis.text.y = element_text(colour="white"))+
  theme(title=element_text(colour ="white"))

