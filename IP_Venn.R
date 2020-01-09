
#install.packages('VennDiagram')
#install.packages('dplyr')
#install.packages('tydir')
library(VennDiagram)
library(dplyr)
library(tidyr)

# set you working folder
setwd('~/Documents/projects/Abi/')

#read the data
testisRaw<-read.csv(file='MSB-7167 WVU Ramamurthy 092818 Con Sheet Testes.csv', stringsAsFactors = F)
retinaRaw<-read.csv(file='MSB-6730 WVU Munezero 042718 (Autosaved) Con Sheet Retina.csv', stringsAsFactors = F)
head(testisRaw)

# rename the columns to something meaningfull and easy to type
colnames(testisRaw)<-colnames(retinaRaw)<-c('Description','idList','MW','WT','KO')

#label the tissues
testisRaw$tissue<-'testis'
retinaRaw$tissue<-'retina'

#combine into one table
rawCounts<-rbind(testisRaw,retinaRaw)
head(rawCounts) 

# split the accession and pull the swissprot IDs
rawCounts$spID<-apply(rawCounts,1, function(x) unlist(strsplit(x['idList'],'\\|'))[[2]])

# convert MW to number
rawCounts$MW<-apply(rawCounts,1, function(x) as.numeric(unlist(strsplit(x['MW'],' '))[[1]]))


longView<-rawCounts %>% gather(key=Experiment, value=SpC, -c(Description,idList,MW,tissue,spID))

# create a column called sample, which combines the tissue and genotype
# we will need it later when we spread the data
longView$sample<-paste(longView$tissue, longView$Experiment,sep='')


# spread the presence data into a table with samples as columns and protein ID as rows 
wideView<-longView %>% select(sample, Description, spID, MW, SpC) %>% spread(sample, SpC, fill=0)
 
# Give human readable names to our categories
category<-c('Retina WT','Retina KO','Testis WT','Testis KO')


# Draw the Venn diagram
draw.quad.venn(area1=sum(wideView$retinaWT),
               area2=sum(wideView$retinaKO),
               area3=sum(wideView$testisWT),
               area4=sum(wideView$testisKO),
               n12=sum(wideView$retinaWT & wideView$retinaKO),
               n13=sum(wideView$retinaWT & wideView$testisWT),
               n14=sum(wideView$retinaWT & wideView$testisKO),
               n23=sum(wideView$retinaKO & wideView$testisWT),
               n24=sum(wideView$retinaKO & wideView$testisKO),
               n34=sum(wideView$testisWT & wideView$testisKO),
               n123=sum(wideView$retinaWT & wideView$retinaKO & wideView$testisWT),
               n124=sum(wideView$retinaWT & wideView$retinaKO & wideView$testisKO),
               n134=sum(wideView$retinaWT & wideView$testisWT & wideView$testisKO),
               n234=sum(wideView$retinaKO & wideView$testisWT & wideView$testisKO),
               n1234=sum(wideView$retinaWT & wideView$retinaKO & wideView$testisWT & wideView$testisKO),
               category = category,
               fill = c("orange", "red", "green", "blue"))

# write combined spectral counts table
write.csv(file='combinedSpC.csv',wideView)

# write the hits with spectral counts
hitsSpC<-filter(wideView, spID %in% wideView[(wideView$retinaWT & wideView$testisWT) & !(wideView$retinaKO | wideView$testisKO),'spID'])
write.csv(file='hitsSpC.csv', hitsSpC)


# calculate the normalized spectral abundance
# normalize by MW
wideNSAF<-sweep(wideView[,c('retinaWT','retinaKO','testisWT','testisKO')],1,wideView$MW,'/')
# normalize by total counts
wideNSAF<-sweep(wideNSAF[,c('retinaWT','retinaKO','testisWT','testisKO')],2,colSums(wideView[,c('retinaWT','retinaKO','testisWT','testisKO')]),'/')

# write NSAF table
write.csv(file='combinedNSAF.csv',wideNSAF)

# write hits in NSAF
write.csv(file='hitsNSAF.csv',wideNSAF[wideNSAF$spID %in% hitsSpC$spID,])
