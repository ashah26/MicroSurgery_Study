#question3

sessions_cutting= list()
sessions_suturing = list()
add_path = c()
cutting_plots = list()

library(ggplot2)

for(i in 1:21){
  path_1 <-paste("/home/ashna/Documents/UHMC/2nd Sem/Statistics/Project/",project_data_vector[i],sep="")
  path_2 <- paste(path_1,project_data_vector[i],sep = "/")
  add_path[i]=path_2
  cutting_data <- list.files(add_path[i],pattern="Cutting[0-9]_NASA.csv",recursive = TRUE,full.names = TRUE)
  suturing_data <- list.files(add_path[i],pattern="Suturing[0-9]_NASA.csv",recursive = TRUE,full.names = TRUE)
  
  sessions_cutting[[i]]=c(cutting_data)
  sessions_suturing[[i]]=c(suturing_data)
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
  
  data_cutting <-data.frame()
  data_suturing<-data.frame()
  for(j in 1:5){
    if(!is.na(sessions_cutting[[i]][j])){
      data_cutting<- read.csv(sessions_cutting[[i]][j],header=TRUE)
      sessions_modified<-c(sessions_modified,sessions[j])
    }else{
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
  
  #mental_demand_combine <-c(cutting_MD,suturing_MD)
  #print(mental_demand_combine)
  #tasks<-c("cutting","suturing")
  #tasks<-rep(tasks,each=2,length.out=leng)
  #sessions_modified_combine<-rep(sessions_modified,length.out=length(sessions_modified*2))
  # print(sessions_modified_combine)
  #new_data_frame<-data.frame(mental_demand_combine,sessions_modified_combine,tasks)
  plot1<-ggplot(NULL,aes(x=sessions_modified,y=cutting_MD))+ylab("Mental Demand")+geom_bar(stat="identity",fill="green",position = "dodge")+ylim(0,20)+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
  plot2<-ggplot(NULL,aes(x=sessions_modified,y=cutting_PD))+ylab("Physical Demand")+geom_bar(stat="identity",fill="green")+ylim(0,20)+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
  plot3<-ggplot(NULL,aes(x=sessions_modified,y=cutting_TD))+ylab("Temporal Demand")+geom_bar(stat="identity",fill="green")+ylim(0,20)+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
  plot4<-ggplot(NULL,aes(x=sessions_modified,y=cutting_performance))+ylab("Performance")+geom_bar(stat="identity",fill="green")+ylim(0,20)+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
  plot5<-ggplot(NULL,aes(x=sessions_modified,y=cutting_effort))+xlab("Sessions")+ylab("Effort")+geom_bar(stat="identity",fill="green")+ylim(0,20)
  plot6<-ggplot(NULL,aes(x=sessions_modified,y=cutting_frustration))+xlab("Sessions")+ylab("Frustration")+geom_bar(stat="identity",fill="green")+ylim(0,20)
  
  plot7<-ggplot(NULL,aes(x=sessions_modified,y=suturing_MD))+ylab("Mental Demand")+geom_bar(stat="identity",fill="blue")+ylim(0,20)+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
  plot8<-ggplot(NULL,aes(x=sessions_modified,y=suturing_PD))+ylab("Physical Demand")+geom_bar(stat="identity",fill="blue")+ylim(0,20)+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
  plot9<-ggplot(NULL,aes(x=sessions_modified,y=suturing_TD))+ylab("Temporal Demand")+geom_bar(stat="identity",fill="blue")+ylim(0,20)+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
  plot10<-ggplot(NULL,aes(x=sessions_modified,y=suturing_performance))+ylab("Performance")+geom_bar(stat="identity",fill="blue")+ylim(0,20)+theme(axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank())
  plot11<-ggplot(NULL,aes(x=sessions_modified,y=suturing_effort))+xlab("Sessions")+ylab("Effort")+geom_bar(stat="identity",fill="blue")+ylim(0,20)
  plot12<-ggplot(NULL,aes(x=sessions_modified,y=suturing_frustration))+xlab("Sessions")+ylab("Frustration")+geom_bar(stat="identity",fill="blue")+ylim(0,20)
  
  grid.draw()
  
  grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,nrow=4,ncol=2,top=paste("Cutting scores for Subject",names[i]))
  grid.arrange(plot7,plot8,plot9,plot10,plot11,plot12,nrow=4,ncol=2,top=paste("Suturing scores for Subject",names[i]))
  
  dev.off()
}