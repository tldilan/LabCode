#setworking directory
#install.packages('ggplot2')
#install.packages("ggbeeswarm")
library(ggplot2)
library(ggbeeswarm)
colorPallette<-c("#000000","#FF0000") #gray and red color pallette
ciliaLength<-read.table(file='Google Drive/ARL2BP/Cilia Length Graphs.txt',header=TRUE)
colnames(ciliaLength)<-c('Length','Genotype')
ciliaLength$Genotype<-factor(ciliaLength$Genotype, levels=c('WT', 'KO')) #reorder the axis
colorPallette<-c("#4F4F4F","#FF0000")
lengthPlot<-ggplot(ciliaLength, aes(x=Genotype,y=Length, fill=Genotype))
lengthPlot + 
  geom_violin(trim=FALSE, scale='width', alpha=0.6, colour='#000000') +
  scale_fill_manual(values=colorPallette) + #use custom color for fill
  geom_quasirandom(varwidth=T, method='tukeyDense') +
  labs(x='Genotype', y='Axoneme Length') +
  theme(legend.position="none")


geom_violin(mapping=aes(Genotype,'Axoneme Length'),data=ciliaLength)