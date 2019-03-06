library(ggplot2) #load ggplot

#create a variable that holds my data

Basal.Bodies <- read.csv("/Users/tanyaleedilan/Desktop/PUBH211/BasalBodies.csv") #uploa your data, make sure you know the filepath and you save your data as a CSV file on excel

#this line of code lets me sort bars by factor ordering (if you dont use this line the graph will show KO bar first) 

Basal.Bodies$genotype <- factor(Basal.Bodies$genotype,levels = c("WT","KO")) 

#create a stacked bargraph

graph <- ggplot(Basal.Bodies, aes(fill=condition,x=genotype, y=value )) + 
  geom_bar( stat="identity", position="fill") 

#view graph

graph 

graph2<- graph(theme(legend.title = element_blank())  #remove title of legend and save as new graph2 variable
               
               FinalGraph<- graph2 + labs(y= "Percent",   #Change x,y axis as well as create title , there must be a way to make this shorter
                                          x= "Genotype",
                                          title = "Displaced Basal Bodies in ARL13B-null Photoreceptors")
               FinalGraph  #view graph
               
               #To test whether the control(WT) and treatment(KO) conditions result in different frequencies, use a 2D contingency table.
               statsBasal.Bodies<- ct <- chisq.test(xtabs(value ~ condition + genotype, data=Basal.Bodies)) #Pearson's Chi-squared test with Yates' continuity correction, This test is more conservative for small sample sizes.
               statsBasal.Bodies 

