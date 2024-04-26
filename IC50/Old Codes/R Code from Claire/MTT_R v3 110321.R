#
# MTT assay
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
library("viridis")

# Import data from text (base), renamed to Cytoxicity

# read data, renamed to Cytotoxicty
Cytotoxicity <- read.csv("~/OneDrive - University College Dublin/4th Year/Final Year Project/MTT assay/MTT_27_11_20_2.csv")
names(Cytotoxicity)<- c('Treatment','Cytotoxicity')
View(Cytotoxicity)

#code for graphs with white background

Size_fac5 <-c("NP 0 µg/mL", "NP 12.5 µg/mL","NP 25 µg/mL", "NP 50 µg/mL", "NP 100 µg/mL", "STS 10 µM")

Cytotoxicity2<- factor(Cytotoxicity$Treatment, levels = Size_fac5)
View(Cytotoxicity2)

# Get the maximum value for each spheroid size and add 10% for position of "n = " on graph
max_cytotoxicity_0 = max(Cytotoxicity[Cytotoxicity$Treatment == 'NP 0 µg/mL',]$Cytotoxicity, na.rm = TRUE) * 1.2
max_cytotoxicity_12.5 = max(Cytotoxicity[Cytotoxicity$Treatment== 'NP 12.5 µg/mL',]$Cytotoxicity, na.rm = TRUE) * 1.2
max_cytotoxicity_25 = max(Cytotoxicity[Cytotoxicity$Treatment == 'NP 25 µg/mL',]$Cytotoxicity, na.rm = TRUE) * 1.2
max_cytotoxicity_50 = max(Cytotoxicity[Cytotoxicity$Treatment == 'NP 50 µg/mL',]$Cytotoxicity, na.rm = TRUE) * 1.2
max_cytotoxicity_100 = max(Cytotoxicity[Cytotoxicity$Treatment== 'NP 100 µg/mL',]$Cytotoxicity, na.rm = TRUE) * 1.2
max_cytotoxicity_STS = max(Cytotoxicity[Cytotoxicity$Treatment == 'STS 10 µM',]$Cytotoxicity, na.rm = TRUE) * 1.2

# Get the number of samples for each spheroid size group to go on the graph.
sample_size_0 = nrow(Cytotoxicity[Cytotoxicity$Treatment == 'NP 0 µg/mL',])
sample_size_12.5 = nrow(Cytotoxicity[Cytotoxicity$Treatment == 'NP 12.5 µg/mL',])
sample_size_25 = nrow(Cytotoxicity[Cytotoxicity$Treatment == 'NP 25 µg/mL',])
sample_size_50 = nrow(Cytotoxicity[Cytotoxicity$Treatment == 'NP 50 µg/mL',])
sample_size_100 = nrow(Cytotoxicity[Cytotoxicity$Treatment == 'NP 100 µg/mL',])
sample_size_STS = nrow(Cytotoxicity[Cytotoxicity$Treatment == 'STS 10 µM',])

label_breaks = c("NP 0 µg/mL", "NP 12.5 µg/mL", "NP 25 µg/mL", "NP 50 µg/mL", "NP 100 µg/mL", "STS 10 µM")
label_colors = c("NP 0 µg/mL"="skyblue1", "NP 12.5 µg/mL"="yellow3","NP 25 µg/mL"="salmon2","NP 50 µg/mL"="red2","NP 100 µg/mL"="coral4", "STS 10 µM"="purple1")

ggplot(Cytotoxicity, aes(x=Cytotoxicity2, y=Cytotoxicity, fill=Treatment))+
  geom_bar(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=label_breaks)+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  scale_fill_manual(breaks=label_breaks, 
                    values=label_colors) +
  # scale_fill_viridis(discrete = T, option = "D") +
  labs(title= "% Cytoxicity of HepG2 cells treated with Amine-Modified Nanoparticles", y= "Treatment")+
  theme(plot.title = element_text(hjust = 0.2, size=12))+
  theme(axis.title.x = element_blank())+
  #theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(-10, max(Cytotoxicity$Cytotoxicity, na.rm = TRUE) * 1.2), breaks = seq(-10, max(Cytotoxicity$Cytotoxicity, na.rm = TRUE) * 1.2, by = 10))+ #the scale is the largest value + 25% extra
 
   #theme(legend.key=element_rect(fill=NA)) +
  # This is the code needed to write text onto the graph
#  annotate("text", x=1, y=max_cytotoxicity_0, label= paste("n =", sample_size_0, sep=" ")) +
  #annotate("text", x=2, y=max_cytotoxicity_12.5, label= paste("n =", sample_size_12.5, sep=" ")) +
  #annotate("text", x=3, y=max_cytotoxicity_25, label= paste("n =", sample_size_25, sep=" ")) +
  #annotate("text", x=4, y=max_cytotoxicity_50, label= paste("n =", sample_size_50, sep=" ")) +
  #annotate("text", x=5, y=max_cytotoxicity_100, label= paste("n =", sample_size_100, sep=" ")) +
  #annotate("text", x=6, y=max_cytotoxicity_STS, label= paste("n =", sample_size_STS, sep=" ")) +
  # This code adds the labels to the x axis
  scale_x_discrete(labels=c("Control", "NP 12.5", "NP 25", "NP 50", "NP 100", "STS"))

#geom_errorbar(aes(ymin=Cytotoxicity-sd,
                  #ymax=Cytotoxicity+sd),
              #width=0.4, colour="dimgrey", alpha=0.9, size=1.0)
# black
ggplot(Cytotoxicity, aes(x=Cytotoxicity2, y=Cytotoxicity, fill=Treatment))+
  geom_bar(stat="identity", alpha= 0.5)+ 
  scale_color_discrete(breaks=label_breaks)+
  theme(panel.grid = element_blank())+ theme(panel.background = element_rect(fill = "white"))+ 
  scale_fill_manual(breaks=label_breaks, 
                    values=label_colors) +
  # scale_fill_viridis(discrete = T, option = "D") +
  labs(title= "% Cytoxicity of HepG2 cells treated with Amine-Modified Nanoparticles", y= "Treatment")+
  theme(plot.title = element_text(hjust = 0.2, size=12))+
  theme(axis.title.x = element_blank())+
  #theme(legend.title = element_blank())+
  scale_y_continuous(limits = c(-10, max(Cytotoxicity$Cytotoxicity, na.rm = TRUE) * 1.2), breaks = seq(-10, max(Cytotoxicity$Cytotoxicity, na.rm = TRUE) * 1.2, by = 10))+ #the scale is the largest value + 25% extra
  
  #theme(legend.key=element_rect(fill=NA)) +
  # This is the code needed to write text onto the graph
  #  annotate("text", x=1, y=max_cytotoxicity_0, label= paste("n =", sample_size_0, sep=" ")) +
  #annotate("text", x=2, y=max_cytotoxicity_12.5, label= paste("n =", sample_size_12.5, sep=" ")) +
  #annotate("text", x=3, y=max_cytotoxicity_25, label= paste("n =", sample_size_25, sep=" ")) +
  #annotate("text", x=4, y=max_cytotoxicity_50, label= paste("n =", sample_size_50, sep=" ")) +
  #annotate("text", x=5, y=max_cytotoxicity_100, label= paste("n =", sample_size_100, sep=" ")) +
  #annotate("text", x=6, y=max_cytotoxicity_STS, label= paste("n =", sample_size_STS, sep=" ")) +
  # This code adds the labels to the x axis
  scale_x_discrete(labels=c("Control", "NP 12.5", "NP 25", "NP 50", "NP 100", "STS"))+
  # black
  theme(panel.grid = element_blank()) + 
  theme(panel.background = element_rect(fill = "black"))+
  theme(legend.title = element_text(colour = "white"))+
  theme(legend.key=element_rect(fill="black"))+
  theme(legend.background = element_rect(fill= "black"))+
  theme(legend.text = element_text(colour="white"))+
  theme(plot.background = element_rect(fill = "black"))+
  theme(axis.text.y = element_text(colour="white"))+
  theme(axis.text.x = element_text(colour="white"))+
  theme(title=element_text(colour ="white"))
