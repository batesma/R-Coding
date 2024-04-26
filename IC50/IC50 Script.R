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
setwd("C:/Users/markb/Desktop")

#Set library path
.libPaths("C:/Users/markb/Documents/R/win-library/4.1") 

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
if (!require("r2symbols")) install.packages("r2symbols")
if (!require("matrixStats")) install.packages("matrixStats")
if (!require("drc")) install.packages("drc")
if (!require("lubridate")) install.packages("lubridate")

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
library(r2symbols)
library(matrixStats)
library(drc)
library(lubridate)

####Load Data###############################################
MyData <- read_xlsx(file.choose())
#Get average and STD of Reps and Get rid of redundant values
MyData2<-MyData
MyData2$Av_Rep<- rowMeans(MyData[ , c(2:4)], na.rm=TRUE)
MyData2$STD<- rowSds(as.matrix(MyData[ ,c(2:4)]))
MyData2$REP1=NULL
MyData2$REP2=NULL
MyData2$REP3=NULL
names(MyData2)[1] <- "Concentration ?M"
names(MyData2)[2] <- "% Cell Viability"
MyData2 <- slice(MyData2,-c(1, 19))
MyData2$`Concentration ?M` <- as.numeric (MyData2$`Concentration ?M`,options("scipen"=100))
MyData2$`Concentration ?M` <- round(MyData2$`Concentration ?M`, digits=2)
MyData2$`Log Concentration ?M` <- log10(MyData2$`Concentration ?M`) 
MyData2$label <- rep("IC[50]",length("MyData2$`Concentration ?M"))

############Calcute IC50 Value#################################################################
#Create Functions Slope and Intercept#
curved_fit <- drm(
  formula = `% Cell Viability` ~ `Concentration ?M`,
  data = MyData2,
  fct = LL.4(names = c("hill", "min_value", "max_value", "ec_50"))
)
summary(curved_fit)

coefs <- setNames(
  curved_fit$coefficients,
  c("hill", "min_value", "max_value", "ec_50")
)
 
IC50_Value <- with(
  as.list(coefs),
  exp(
    log(ec_50) + (1 / hill) * log(max_value / (max_value - 2 * min_value))
  )
)
EC50_Value <- with(
  as.list(coefs),(ec_50))

IC50_Value <- round(IC50_Value, digits=0)
IC50_Value
#ggplot the example
strip_chart <-
  ggplot(MyData2, aes(`Concentration ?M`,`% Cell Viability`))+ 
  geom_point(size=2)+
  labs(x="Concentration ?M",
       y="% Cell Viability")+
  geom_errorbar(aes(ymin = `% Cell Viability`-STD, ymax = `% Cell Viability`+STD), 
                size =0.5, width=0.1)+
  scale_x_continuous(trans="log10",breaks=c(0,0.1,1,10,100,1000,10000))+
  scale_y_continuous(limits = c(0,150),breaks=seq(0,150,50))+
  geom_smooth(method = drm, method.args = list(fct = L.4()), se = FALSE, colour = "black")+
  theme_classic()+
  theme(axis.text.x = element_text(size=10,colour ="black",face="bold"), #sample names
        axis.text.y = element_text(size=10,colour ="black", #y axis tick labels
                                   face="bold"),
        axis.title.x = element_text(size=12,colour ="black", 
                                    face="bold"),
        axis.title.y = element_text(size=12,colour ="black", 
                                    face="bold",vjust=+2)) #y-axis title
strip_chart
#Add IC50 Values and Dotted Line to Chart
strip_chart + 
  geom_line(data=tibble(x=c(100,100), y=c(0,140)), 
            aes(x=x, y=y), inherit.aes = FALSE,size = 0.5, lty = "dashed")+
  geom_text(data=MyData2, mapping=aes(x=200, y=125,label=label), inherit.aes = FALSE, size =3, fontface = "bold", parse = TRUE)+
  geom_text(data=MyData2, mapping=aes(x=350, y=125,label="="), inherit.aes = FALSE, size =3, fontface = "bold", parse = F)+
  geom_text(data=MyData2, mapping=aes(x=600, y=125,label=IC50_Value), inherit.aes = FALSE, size =3, parse = F)+
  geom_text(data=MyData2, mapping=aes(x=1100, y=125,label="?M"), inherit.aes = FALSE, size =3, parse = F)

############Save Plot###########################
par(mar=c(5.1,4.1,5,2.1))
ggsave("My Graph.tiff", width=5, height=4)


