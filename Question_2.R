library(ggplot2)
library(gridExtra)





project_data<- dir(path="/home/ashna/Documents/UHMC/2nd Sem/Statistics/Project/",pattern="subject[0-26]",recursive = FALSE)
project_data
project_data_vector <- c(project_data)
sessions <- c("session1","session2","session3","session4","session5")

#question 2

tai_file <- read.table("/home/ashna/Documents/UHMC/2nd Sem/Statistics/Project/tai_scores.txt",header = FALSE)
g<-ggplot(tai_file,aes(tai_file$V2))
g+geom_bar()+xlab("TAI Scores")+ggtitle("TAI scores of each subject")