setwd("/Users/tanyaleedilan/Desktop")  #set your desktop as your working directory
rawData <- read.csv("BBS8sensitivity.csv", header=TRUE) #load data and name it rawData
install.packages("wesanderson") #install color package 
install.packages("sjmisc")  #install data transformation package
#install ggplot2 and tidyr

library(tidyr)
library(ggplot2)
library(wesanderson)
library(sjmisc)

df<-to_long(rawData, keys = "genos", values = c("mean", "SEM"),  #use to_long() from the sjmisc-package, which gathers multiple columns at once
            c("WT.mean", "KO.mean"),
            c("WT.SEM", "KO.SEM"))

p<- ggplot(df, aes(y = mean, x = light.intensity, fill= genos)) +  #plot graph y axis is mean, x axis is light intensity and the fill is the two genotypes
  geom_point(size = .5) + geom_point(aes(color = genos)) +   #change the point size
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE, linetype = "dashed", color= 'black', size=.5) +  #add linear curve fit with method lm and a modified forumla for flexible curve
  scale_color_manual(values = wes_palette(n=2, name="GrandBudapest1")) +  #change colors
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) #change X-axis to log10 scale without log ticks
p #show plot
t<- p+ geom_errorbar(aes(ymin=mean-SEM, ymax=mean+SEM), width=.2,  #add SEM error bars that do not overlap and rename plot 't'
                     position=position_dodge(0.05)) + labs(x = "Flash Strength (cd * s/m^2", y= "Amplitude", title="Stimulus Intensity Curve")

t#show plot
