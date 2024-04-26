#load packages
library(ggplot2)
library(data.table)
library(dr4pl)
library(car)

#example data
curve = c("C1","C1","C1","C1","C1","C1","C1","C1","C1","C2","C2","C2","C2","C2","C2","C2","C2","C2","C3","C3","C3","C3","C3","C3","C3","C3","C3")
POC =c(1.07129314, 0.91126280, 0.97914297, 0.95904437,0.88509670, 0.84338263, 0.75843762, 0.61319681, 0.52635571, 0.84563087,1.24435113, 1.11757648, 0.82383523, 0.82763447, 0.72585483, 0.31953609, 0.15056989,  0.10057988, 0.57384256, 0.65984339, 0.81439758, 0.84572057, 0.62797088,  0.30800934, 0.08957274,
       0.06360764, 0.04451161)
dose = c(0.078125,0.156250, 0.312500,0.625000,1.250000,2.500000, 5.000000,10.000000,20.000000,0.078125,0.156250,0.312500,0.625000,  
         1.250000,2.500000,5.000000,10.000000,20.000000,0.078125,
         0.156250,0.312500,0.625000,1.250000,2.500000,5.000000,10.000000,
         20.000000)
example2<-data.frame(POC, dose, curve)

#this code will write  model that can be incorporated into 
predict.dr4pl <- function (object, newdata=NULL, se.fit=FALSE, level, interval) {
  xseq <- if (is.null(newdata)) object$data$Dose else newdata$x
  pred <- MeanResponse(xseq, object$parameters)
  if (!se.fit) {
    return(pred)
  }
  qq <- qnorm((1+level)/2)
  se <- sapply(xseq,
               function(x) car::deltaMethod(object, 
                                            "UpperLimit + (LowerLimit - UpperLimit)/(1 + (x/IC50)^Slope)")[["Estimate"]])
  return(list(fit=data.frame(fit=pred,lwr=pred-qq*se,
                             upr=pred+qq*se), se.fit=se))
}

#ggplot the example
ggplot(example2, aes(dose,POC, col=curve))+ 
  geom_point(size=4, shape=1) +
  geom_smooth(method="dr4pl",se=F)+ 
  coord_trans(x="log10")+
  theme_bw()+
  scale_x_continuous(breaks = c(0.01, 0.1, 1, 10, 100))+
  theme(plot.title = element_text(lineheight = 0.9, face="bold", size=20, hjust=0.5))+
  ggtitle("Dose Response")+
  theme(axis.title = element_text(face="bold", size = 14))+
  theme(axis.text = element_text(face="bold", size = 12, colour="black"))

