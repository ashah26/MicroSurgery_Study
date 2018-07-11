library(ggplot2)
library(gridExtra)
library(gridBase)
library(dplyr)
library(grid)

#project_data<- dir(path="/home/ashna/Documents/UHMC/2nd Sem/Statistics/Project/",pattern="subject[0-26]",recursive = FALSE)
project_data<- dir(path="C:/Users/Ashna/Desktop/OneDrive - University Of Houston/2nd Sem/Statistics/Project/",pattern="subject[0-26]",recursive = FALSE)
project_data
project_data_vector <- c(project_data)
sessions <- c("session1","session2","session3","session4","session5")

#question 2

#tai_file <- read.table("/home/ashna/Documents/UHMC/2nd Sem/Statistics/Project/tai_scores.txt",header = FALSE)
#g<-ggplot(tai_file,aes(tai_file$V2))
#g+geom_bar()+xlab("TAI Scores")+ggtitle("TAI scores of each subject")



#question3

sessions_cutting= list()
sessions_suturing = list()
add_path = c()
cutting_plots = list()
combined_mean_MD<-c()
combined_mean_df_MD<-data.frame()
library(ggplot2)
plots<-c()
for(i in 1:21){
  path_1 <-paste("C:/Users/Ashna/Desktop/OneDrive - University Of Houston/2nd Sem/Statistics/Project/",project_data_vector[i],sep="")
  path_2 <- paste(path_1,project_data_vector[i],sep = "/")
  #add_path[i]=path_2
  print(path_2)
  cutting_data <- list.files(path_2,pattern="Cutting[0-9]_NASA.csv",recursive = TRUE,full.names = TRUE)
  suturing_data <- list.files(path_2,pattern="Suturing[0-9]_NASA.csv",recursive = TRUE,full.names = TRUE)
  print(cutting_data)
  sessions_cutting[[i]]=c(cutting_data)
  sessions_suturing[[i]]=c(suturing_data)
  print(sessions_cutting[[i]])
  sessions_modified<- c()
  cutting_MD <- c()
  cutting_PD <- c()
  cutting_TD <- c()
  cutting_effort<-c()
  cutting_performance<-c()
  cutting_frustration<-c()
  
  suturing_MD <- c()
  suturing_PD <- c()
  suturing_TD <- c()
  suturing_effort<-c()
  suturing_performance<-c()
  suturing_frustration<-c()
  count<-as.numeric(1)
  data_cutting <-data.frame()
  data_suturing<-data.frame()
  for(j in 1:5){
    if(!is.na(sessions_cutting[[i]][j])){
      data_cutting<- read.csv(sessions_cutting[[i]][j],header=TRUE)
      sessions_modified<-c(sessions_modified,sessions[count])
      count=count+1
    }else{
      count=count+1
      break
    }
    
    Mental_Demand <- data_cutting[1,2]
    Physical_Demand <- data_cutting[2,2]
    Temporal_Demand<-data_cutting[3,2]
    Performance<-data_cutting[4,2]
    Effort<-data_cutting[5,2]
    Frustration<-data_cutting[6,2]
    cutting_MD<-c(cutting_MD,Mental_Demand)
    cutting_PD<-c(cutting_PD,Physical_Demand)
    cutting_TD<-c(cutting_TD,Temporal_Demand)
    cutting_performance<-c(cutting_performance,Performance)
    cutting_effort<-c(cutting_effort,Effort)
    cutting_frustration<-c(cutting_frustration,Frustration)
  }
  
  for(j in 1:5){
    if(!is.na(sessions_suturing[[i]][j])){
      data_suturing<- read.csv(sessions_suturing[[i]][j],header=TRUE)
    }else{
      break
    }
    
    Mental_Demand <- data_suturing[1,2]
    Physical_Demand <- data_suturing[2,2]
    Temporal_Demand<-data_suturing[3,2]
    Performance<-data_suturing[4,2]
    Effort<-data_suturing[5,2]
    Frustration<-data_suturing[6,2]
    suturing_MD<-c(suturing_MD,Mental_Demand)
    suturing_PD<-c(suturing_PD,Physical_Demand)
    suturing_TD<-c(suturing_TD,Temporal_Demand)
    suturing_performance<-c(suturing_performance,Performance)
    suturing_effort<-c(suturing_effort,Effort)
    suturing_frustration<-c(suturing_frustration,Frustration)
    
  }
  names<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,19,20,21,22,23,24,25,26)
  pdf(sprintf("subject_%s.pdf",names[i]))
  
  mean_cutting_MD<-mean(cutting_MD)
  mean_cutting_PD<-mean(cutting_PD)
  mean_cutting_TD<-mean(cutting_TD)
  mean_cutting_performance<-mean(cutting_performance)
  mean_cutting_effort<-mean(cutting_effort)
  mean_cutting_frustration<-mean(cutting_frustration)
  mean_suturing_MD<-mean(suturing_MD)
  mean_suturing_PD<-mean(suturing_PD)
  mean_suturing_TD<-mean(suturing_TD)
  mean_suturing_performance<-mean(suturing_performance)
  mean_suturing_effort<-mean(suturing_effort)
  mean_suturing_frustration<-mean(suturing_frustration)
  
  
  combined_mean_MD<-c(combined_mean_MD,mean_cutting_MD)
  combined_mean_MD<-c(combined_mean_MD,mean_suturing_MD)
  
  tasks_mean<-c("cutting","suturing")
  tasks_mean<-rep(tasks_mean,length.out=42)
  combined_mean_df_MD<-data.frame(combined_mean_MD,tasks_mean)
  
  combined_MD<-c(cutting_MD,suturing_MD)
  combined_PD<-c(cutting_PD,suturing_PD)
  combined_TD<-c(cutting_TD,suturing_TD)
  combined_performance<-c(cutting_performance,suturing_performance)
  combined_effort<-c(cutting_effort,suturing_effort)
  combined_frustration<-c(cutting_frustration,suturing_frustration)
  
  tasks<-c("cutting","suturing")
  tasks<-rep(tasks,each=length(cutting_MD),length.out = length(combined_MD))
  session_combined<-rep(sessions_modified,length.out=length(combined_MD))
  
  combined_df_MD<-data.frame(combined_MD,tasks,session_combined)
  combined_df_PD<-data.frame(combined_PD,tasks,session_combined)
  combined_df_TD<-data.frame(combined_TD,tasks,session_combined)
  combined_df_performance<-data.frame(combined_performance,tasks,session_combined)
  combined_df_effort<-data.frame(combined_effort,tasks,session_combined)
  combined_df_frustration<-data.frame(combined_frustration,tasks,session_combined)
  
  
  #plot1<-ggplot(data=NULL,aes(x=sessions_modified))+geom_bar(stat = "identity",aes(y=cutting_MD),position = "dodge")+geom_bar(stat = "identity",aes(y=suturing_MD),position = "dodge")+theme(legend.title= element_text(colour="black", size=12))+scale_color_discrete(name="Tasks",labels=c("Cutting","Suturing"),labs(y="Score"))+ylim(0,20)+xlim(sessions_modified)
  
  plot1<-ggplot(combined_df_MD,aes(x=session_combined,y=combined_MD,fill=tasks))+geom_bar(stat="identity",position = "dodge")+xlim(sessions_modified)+scale_color_discrete(name="Tasks",labels=c("Cutting","Suturing"))+ylim(0,20)+ylab("Mental Demand")+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_fill_manual(breaks=c("cutting","suturing"),values = c("green","red"))
  plot2<-ggplot(combined_df_PD,aes(x=session_combined,y=combined_PD,fill=tasks))+geom_bar(stat="identity",position = "dodge")+xlim(sessions_modified)+scale_color_discrete(name="Tasks",labels=c("Cutting","Suturing"))+ylim(0,20)+ylab("Physical Demand")+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_fill_manual(breaks=c("cutting","suturing"),values = c("green","red"))
  plot3<-ggplot(combined_df_TD,aes(x=session_combined,y=combined_TD,fill=tasks))+geom_bar(stat="identity",position = "dodge")+xlim(sessions_modified)+scale_color_discrete(name="Tasks",labels=c("Cutting","Suturing"))+ylim(0,20)+ylab("Temporal Demand")+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_fill_manual(breaks=c("cutting","suturing"),values = c("green","red"))
  plot4<-ggplot(combined_df_performance,aes(x=session_combined,y=combined_performance,fill=tasks))+geom_bar(stat="identity",position = "dodge")+xlim(sessions_modified)+scale_color_discrete(name="Tasks",labels=c("Cutting","Suturing"))+ylim(0,20)+ylab("Performance")+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_fill_manual(breaks=c("cutting","suturing"),values = c("green","red"))
  plot5<-ggplot(combined_df_effort,aes(x=session_combined,y=combined_effort,fill=tasks))+geom_bar(stat="identity",position = "dodge")+scale_color_discrete(name="Tasks",labels=c("Cutting","Suturing"))+ylim(0,20)+xlab("Sessions")+ylab("Effort")+scale_x_discrete(breaks=c("session1","session2","session3","session4","session5"),labels=c("1","2","3","4","5"))+scale_fill_manual(breaks=c("cutting","suturing"),values = c("green","red"))
  plot6<-ggplot(combined_df_frustration,aes(x=session_combined,y=combined_frustration,fill=tasks))+geom_bar(stat="identity",position = "dodge")+scale_color_discrete(name="Tasks",labels=c("Cutting","Suturing"))+ylim(0,20)+xlab("Sessions")+ylab("Frustration")+scale_x_discrete(breaks=c("session1","session2","session3","session4","session5"),labels=c("1","2","3","4","5"))+scale_fill_manual(breaks=c("cutting","suturing"),values = c("green","red"))
  
  #plot1<-ggplot(NULL,aes(x=sessions_modified,y=cutting_MD))+ylab("Mental Demand")+geom_bar(stat="identity",fill="red")+ylim(0,20)+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
  #plot2<-ggplot(NULL,aes(x=sessions_modified,y=cutting_PD))+ylab("Physical Demand")+geom_bar(stat="identity",fill="red")+ylim(0,20)+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
  #plot3<-ggplot(NULL,aes(x=sessions_modified,y=cutting_TD))+ylab("Temporal Demand")+geom_bar(stat="identity",fill="red")+ylim(0,20)+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
  #plot4<-ggplot(NULL,aes(x=sessions_modified,y=cutting_performance))+ylab("Performance")+geom_bar(stat="identity",fill="red")+ylim(0,20)+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
  #plot5<-ggplot(NULL,aes(x=sessions_modified,y=cutting_effort))+xlab("Sessions")+ylab("Effort")+geom_bar(stat="identity",fill="red")+ylim(0,20)
  #plot6<-ggplot(NULL,aes(x=sessions_modified,y=cutting_frustration))+xlab("Sessions")+ylab("Frustration")+geom_bar(stat="identity",fill="red")+ylim(0,20)
  
  #plot7<-ggplot(NULL,aes(x=sessions_modified,y=suturing_MD))+ylab("Mental Demand")+geom_bar(stat="identity",fill="blue")+ylim(0,20)+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
  #plot8<-ggplot(NULL,aes(x=sessions_modified,y=suturing_PD))+ylab("Physical Demand")+geom_bar(stat="identity",fill="blue")+ylim(0,20)+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
  #plot9<-ggplot(NULL,aes(x=sessions_modified,y=suturing_TD))+ylab("Temporal Demand")+geom_bar(stat="identity",fill="blue")+ylim(0,20)+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
  #plot10<-ggplot(NULL,aes(x=sessions_modified,y=suturing_performance))+ylab("Performance")+geom_bar(stat="identity",fill="blue")+ylim(0,20)+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
  #plot11<-ggplot(NULL,aes(x=sessions_modified,y=suturing_effort))+xlab("Sessions")+ylab("Effort")+geom_bar(stat="identity",fill="blue")+ylim(0,20)
  #lot12<-ggplot(NULL,aes(x=sessions_modified,y=suturing_frustration))+xlab("Sessions")+ylab("Frustration")+geom_bar(stat="identity",fill="blue")+ylim(0,20)
  #grid.draw(plot1)
  #grid.newpage()
  #grid.draw(rbind(ggplotGrob(plot1),ggplotGrob(plot7),size="last"))
  grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,nrow=4,ncol=2,top=paste("Cutting and Suturing scores for Subject",names[i]))
  #grid.arrange(plot7,plot8,plot9,plot10,plot11,plot12,nrow=4,ncol=2,top=paste("Suturing scores for Subject",names[i]))
  
  dev.off()
}
names<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,19,20,21,22,23,24,25,26)
plot1<- plot1<-ggplot(combined_mean_df_MD,aes(x=tasks_mean,y=combined_mean_MD,fill=tasks_mean))+geom_bar(stat="identity",position = "dodge")+xlim(sessions_modified)+scale_color_discrete(name="Tasks",labels=c("Cutting","Suturing"))+ylim(0,20)+ylab("Mental Demand")+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())+scale_fill_manual(breaks=c("cutting","suturing"),values = c("green","red"))


