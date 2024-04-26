#
# Exp1 - MaxPro pipeline
# Claire Hughes claire.hughes1@ucdconnect.ie
# Feb 2021
# *************************************************

# Clear all objects from the workspace 
rm(list = ls()) 

# Set the working directory
setwd("~/OneDrive - University College Dublin/4th Year/Final Year Project/Final R")
# load packages

library("ggpubr")
library("devtools")
library("ggrepel")
library("ggplot2")
library("gridExtra")
library("grid")
library("tidyverse")

# Import data from text (base), renamed to Area

# read data, renamed to Area
Area <- read.csv ("~/OneDrive - University College Dublin/4th Year/Final Year Project/Spheroid Optimisation - exp 1/HepG2_exp1_Characterisation_updated_pipeline/R/Characterisation_Spheroid level data_Area.csv")
names(Area) <- c('Spheroid.Size','Area')
View (Area)
#code for graphs with white background

Size_fac5 <-c("Small Spheroid", "Medium Spheroid", "Large Spheroid")

Area2<- factor(Area$Spheroid.Size, levels = Size_fac5)
View(Area2)

# Get the maximum value for each spheroid size and add 10% for position of "n = " on graph
max_area_small = max(Area[Area$Spheroid.Size == 'Small Spheroid',]$Area, na.rm = TRUE) * 1.2
max_area_medium = max(Area[Area$Spheroid.Size == 'Medium Spheroid',]$Area, na.rm = TRUE) * 1.2
max_area_large = max(Area[Area$Spheroid.Size == 'Large Spheroid',]$Area, na.rm = TRUE) * 1.2

# Get the number of samples for each spheroid size group to go on the graph.
sample_size_small = nrow(Area[Area$Spheroid.Size == 'Small Spheroid',])
sample_size_medium = nrow(Area[Area$Spheroid.Size == 'Medium Spheroid',])
sample_size_large = nrow(Area[Area$Spheroid.Size == 'Large Spheroid',])


ggplot(Area, aes(x=Area2, y=Area, colour=Spheroid.Size))+geom_boxplot(lwd=1,width = 0.5)+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Small Spheroid", "Medium Spheroid", "Large Spheroid"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "Spheorid Area [µm²] of Small, Medium and Large Spheroids  ", y= "Area [µm²] ")+
  theme(plot.title = element_text(hjust = 0.22, size=14))+
  theme(axis.title.y = element_text(size=14))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(size= 12))+
  theme(axis.text.y = element_text(size= 12))+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(0, max(Area$Area, na.rm = TRUE) * 1.2), breaks = seq(0, max(Area$Area, na.rm = TRUE) * 1.2, by = 2000))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  # This is the code needed to write text onto the graph
  #annotate("text", x=1, y=max_volume_small, label= paste("n =", sample_size_small, sep=" ")) +
  #annotate("text", x=2, y=max_volume_medium, label= paste("n =", sample_size_medium, sep=" ")) +
  #annotate("text", x=3, y=max_volume_large, label= paste("n =", sample_size_large, sep=" ")) +
  # This code adds the labels to the x axis
  scale_x_discrete(labels=c("Small", "Medium", "Large"))

# BLACK BACKGROUND
ggplot(Area, aes(x=Area2, y=Area, colour=Spheroid.Size))+geom_boxplot(lwd=1,width = 0.5, fill="black")+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Small Spheroid", "Medium Spheroid", "Large Spheroid"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "Spheorid Area [µm²] of Small, Medium and Large Spheroids  ", y= "Area [µm²] ")+
  theme(plot.title = element_text(hjust = 0.22, size=14))+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_text(size=14))+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(0, max(Area$Area, na.rm = TRUE) * 1.2), breaks = seq(0, max(Area$Area, na.rm = TRUE) * 1.2, by = 2000))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  # This is the code needed to write text onto the graph
  #annotate("text", x=1, y=max_volume_small, label= paste("n =", sample_size_small, sep=" ")) +
  #annotate("text", x=2, y=max_volume_medium, label= paste("n =", sample_size_medium, sep=" ")) +
  #annotate("text", x=3, y=max_volume_large, label= paste("n =", sample_size_large, sep=" ")) +
  # This code adds the labels to the x axis
  scale_x_discrete(labels=c("Small", "Medium", "Large")) +
  # This adds the black background
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

# Import data from text (base), renamed to Roundness

# read data, renamed to Roundness
Roundness <- read.csv("~/OneDrive - University College Dublin/4th Year/Final Year Project/Spheroid Optimisation - exp 1/HepG2_exp1_Characterisation_updated_pipeline/R/Characterisation_Spheroid level data_Roundness.csv")
names(Roundness) <- c('Spheroid.Size','Roundness')
View(Roundness)

#code for graphs with white background

Size_fac5 <-c("Small Spheroid", "Medium Spheroid", "Large Spheroid")

Roundness2<- factor(Roundness$Spheroid.Size, levels = Size_fac5)
View(Roundness2)

# Get the maximum value for each spheroid size and add 20% for position of "n = " on graph
max_round_small = max(Roundness[Roundness$Spheroid.Size == 'Small Spheroid',]$Roundness, na.rm = TRUE) * 1.1
max_round_medium = max(Roundness[Roundness$Spheroid.Size == 'Medium Spheroid',]$Roundness, na.rm = TRUE) * 1.1
max_round_large = max(Roundness[Roundness$Spheroid.Size == 'Large Spheroid',]$Roundness, na.rm = TRUE) * 1.1

# Get the number of samples for each spheroid size group to go on the graph.
sample_size_small = nrow(Roundness[Roundness$Spheroid.Size == 'Small Spheroid',])
sample_size_medium = nrow(Roundness[Roundness$Spheroid.Size == 'Medium Spheroid',])
sample_size_large = nrow(Roundness[Roundness$Spheroid.Size == 'Large Spheroid',])

ggplot(Roundness, aes(x=Roundness2, y=Roundness, colour=Spheroid.Size))+geom_boxplot(lwd=1,width = 0.5)+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Small Spheroid", "Medium Spheroid", "Large Spheroid"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "Spheorid Roundness of Small, Medium and Large Spheroids  ", y= "Roundness ")+
  theme(plot.title = element_text(hjust = 0.18, size=14))+
  theme(axis.title.y = element_text(size=14))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(size= 12))+
  theme(axis.text.y = element_text(size= 12))+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(0, max(Roundness$Roundness, na.rm = TRUE) * 1.2), breaks = seq(0, max(Roundness$Roundness, na.rm = TRUE) * 1.2, by = 0.2))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  # This is the code needed to write text onto the graph
  #annotate("text", x=1, y=max_volume_small, label= paste("n =", sample_size_small, sep=" ")) +
  #annotate("text", x=2, y=max_volume_medium, label= paste("n =", sample_size_medium, sep=" ")) +
  #annotate("text", x=3, y=max_volume_large, label= paste("n =", sample_size_large, sep=" ")) +
  # This code adds the labels to the x axis
  scale_x_discrete(labels=c("Small", "Medium", "Large"))

# BLACK BACKGROUND
ggplot(Roundness, aes(x=Roundness2, y=Roundness, colour=Spheroid.Size))+geom_boxplot(lwd=1,width = 0.5, fill="black")+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Small Spheroid", "Medium Spheroid", "Large Spheroid"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "Spheorid Roundness of Small, Medium and Large Spheroids  ", y= "Roundness")+
  theme(plot.title = element_text(hjust = 0.18, size=14))+
  theme(axis.title.y = element_text(size=14))+
  theme(axis.title.x = element_blank())+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(0, max(Roundness$Roundness, na.rm = TRUE) * 1.2), breaks = seq(0, max(Roundness$Roundness, na.rm = TRUE) * 1.2, by = 0.2))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  # This is the code needed to write text onto the graph
  #annotate("text", x=1, y=max_volume_small, label= paste("n =", sample_size_small, sep=" ")) +
  #annotate("text", x=2, y=max_volume_medium, label= paste("n =", sample_size_medium, sep=" ")) +
  #annotate("text", x=3, y=max_volume_large, label= paste("n =", sample_size_large, sep=" ")) +
  # This code adds the labels to the x axis
  scale_x_discrete(labels=c("Small", "Medium", "Large")) +
  # This adds the black background
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


# Import data from text (base), renamed to Width 

# read data, renamed to Width
Width <- read.csv("~/OneDrive - University College Dublin/4th Year/Final Year Project/Spheroid Optimisation - exp 1/HepG2_exp1_Characterisation_updated_pipeline/R/Characterisation_Spheroid level data_Width.csv")
names(Width) <- c('Spheroid.Size','Width')
View (Width)

#code for graphs with white background

Size_fac5 <-c("Small Spheroid", "Medium Spheroid", "Large Spheroid")

Width2<- factor(Width$Spheroid.Size, levels = Size_fac5)
View(Width2)

# Get the maximum value for each spheroid size and add 20% for position of "n = " on graph
max_width_small = max(Width[Width$Spheroid.Size == 'Small Spheroid',]$Width, na.rm = TRUE) * 1.1
max_width_medium = max(Width[Width$Spheroid.Size == 'Medium Spheroid',]$Width, na.rm = TRUE) * 1.1
max_width_large = max(Width[Width$Spheroid.Size == 'Large Spheroid',]$Width, na.rm = TRUE) * 1.1

# Get the number of samples for each spheroid size group to go on the graph.
sample_size_small = nrow(Width[Width$Spheroid.Size == 'Small Spheroid',])
sample_size_medium = nrow(Width[Width$Spheroid.Size == 'Medium Spheroid',])
sample_size_large = nrow(Width[Width$Spheroid.Size == 'Large Spheroid',])

ggplot(Width, aes(x=Width2, y=Width, colour=Spheroid.Size))+geom_boxplot(lwd=1,width = 0.5)+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Small Spheroid", "Medium Spheroid", "Large Spheroid"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "Spheorid Width [µm] of Small, Medium and Large Spheroids  ", y= "Width [µm] ")+
  theme(plot.title = element_text(hjust = 0.18, size=14))+
  theme(axis.title.y = element_text(size=14))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(size= 12))+
  theme(axis.text.y = element_text(size= 12))+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(0, max(Width$Width, na.rm = TRUE) * 1.2), breaks = seq(0, max(Width$Width, na.rm = TRUE) * 1.2, by = 20))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  # This is the code needed to write text onto the graph
  #annotate("text", x=1, y=max_volume_small, label= paste("n =", sample_size_small, sep=" ")) +
  #annotate("text", x=2, y=max_volume_medium, label= paste("n =", sample_size_medium, sep=" ")) +
  #annotate("text", x=3, y=max_volume_large, label= paste("n =", sample_size_large, sep=" ")) +
  # This code adds the labels to the x axis
  scale_x_discrete(labels=c("Small", "Medium", "Large"))

# BLACK BACKGROUND
ggplot(Width, aes(x=Width2, y=Width, colour=Spheroid.Size))+geom_boxplot(lwd=1,width = 0.5, fill="black")+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Small Spheroid", "Medium Spheroid", "Large Spheroid"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "Spheorid Width [µm] of Small, Medium and Large Spheroids  ", y= "Width [µm]")+
  theme(plot.title = element_text(hjust = 0.18, size=14))+
  theme(axis.title.y = element_text(size=14))+
  theme(axis.title.x = element_blank())+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(0, max(Width$Width, na.rm = TRUE) * 1.2), breaks = seq(0, max(Width$Width, na.rm = TRUE) * 1.2, by = 20))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA)) +
  # This is the code needed to write text onto the graph
  #annotate("text", x=1, y=max_volume_small, label= paste("n =", sample_size_small, sep=" ")) +
  #annotate("text", x=2, y=max_volume_medium, label= paste("n =", sample_size_medium, sep=" ")) +
  #annotate("text", x=3, y=max_volume_large, label= paste("n =", sample_size_large, sep=" ")) +
  # This code adds the labels to the x axis
  scale_x_discrete(labels=c("Small", "Medium", "Large")) +
  # This adds the black background
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


# Import data from text (base), renamed to Length

# read data, renamed to Length
Length <- read.csv("~/OneDrive - University College Dublin/4th Year/Final Year Project/Spheroid Optimisation - exp 1/HepG2_exp1_Characterisation_updated_pipeline/R/Characterisation_Spheroid level data_Length.csv")
names(Length) <- c('Spheroid.Size','Length')
View (Length)

#code for graphs with white background

Size_fac5 <-c("Small Spheroid", "Medium Spheroid", "Large Spheroid")

Length2<- factor(Length$Spheroid.Size, levels = Size_fac5)
View(Length2)

# Get the maximum value for each spheroid size and add 20% for position of "n = " on graph
max_length_small = max(Length[Length$Spheroid.Size == 'Small Spheroid',]$Length, na.rm = TRUE) * 1.1
max_length_medium = max(Length[Length$Spheroid.Size == 'Medium Spheroid',]$Length, na.rm = TRUE) * 1.1
max_length_large = max(Length[Length$Spheroid.Size == 'Large Spheroid',]$Length, na.rm = TRUE) * 1.1

# Get the number of samples for each spheroid size group to go on the graph.
sample_size_small = nrow(Length[Length$Spheroid.Size == 'Small Spheroid',])
sample_size_medium = nrow(Length[Length$Spheroid.Size == 'Medium Spheroid',])
sample_size_large = nrow(Length[Length$Spheroid.Size == 'Large Spheroid',])

ggplot(Length, aes(x=Length2, y=Length, colour=Spheroid.Size))+geom_boxplot(lwd=1,width = 0.5)+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Small Spheroid", "Medium Spheroid", "Large Spheroid"))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  labs(title= "Spheorid Length [µm] of Small, Medium and Large Spheroids", y= "Length [µm]")+
  theme(plot.title = element_text(hjust = -1, size=10))+
  theme(axis.title.x = element_blank())+
  #theme(axis.text.x =element_blank())+
  theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(0, max(Length$Length, na.rm = TRUE) * 1.2), breaks = seq(0, max(Length$Length, na.rm = TRUE) * 1.2, by = 25))+ #the scale is the largest value + 25% extra
  theme(legend.key=element_rect(fill=NA))+
  # This is the code needed to write text onto the graph
  annotate("text", x=1, y=max_length_small, label= paste("n =", sample_size_small, sep=" ")) +
  annotate("text", x=2, y=max_length_medium, label= paste("n =", sample_size_medium, sep=" ")) +
  annotate("text", x=3, y=max_length_large, label= paste("n =", sample_size_large, sep=" ")) +
  # This code adds the labels to the x axis
  scale_x_discrete(labels=c("Small", "Medium", "Large"))

#code for black background
ggplot(Length, aes(x=Length2, y=Length,colour=Spheroid.Size))+geom_boxplot(lwd=1,width = 0.5, fill=NA)+
  geom_jitter(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=c("Small Spheroid", "Medium Spheroid", "Large Spheroid"))+
  theme(axis.text.x =element_blank())+
  labs(title= "Spheorid Length (µm) of Small, Medium and Large Spheroids", y= "Length  (µm)")+
  theme(axis.text = element_text(colour = "white"))+
  theme(axis.title.x = element_blank())+
  theme(plot.title = element_text(hjust = 0.1))+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "black"))+
  theme(legend.title = element_blank())+
  theme(legend.key=element_rect(fill="black"))+
  theme(legend.background = element_rect(fill= "black"))+
  theme(legend.text = element_text(colour="white"))+
  scale_y_continuous(limits = c(0, 200))+
  theme(plot.background = element_rect(fill = "black"))+
  theme(axis.text.y = element_text(colour="white"))+
  theme(title=element_text(colour ="white"))
