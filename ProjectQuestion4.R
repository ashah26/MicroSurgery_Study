# Variables
subjectPath = list()
sessions = list()
subjectNumbers = c("subject01","subject02","subject03","subject04","subject05","subject06","subject07",
                   "subject08","subject09","subject10","subject11","subject12", "subject13",
                   "subject19", "subject20","subject21", "subject22","subject23","subject24","subject25","subject26")
subjectList = vector("list",21)

# Ignore NASA files
files = list.files("C:/Users/gustavo.arismendi/Documents/Gustavo/Statistics S18/ProjectData/", pattern = "[^NASA].csv", recursive = TRUE)

# Get list of subjects
subjects = dir(path="C:/Users/gustavo.arismendi/Documents/Gustavo/Statistics S18/ProjectData/", pattern = "subject[0-26]", recursive = FALSE)
v = c(subjects)

# Get path to each subjects' session
for(i in 1:21){
  path2 = paste("C:/Users/gustavo.arismendi/Documents/Gustavo/Statistics S18/ProjectData/", subjectNumbers[i], sep = "")
  path3 = paste(path2, subjectNumbers[i], sep = "/")
  subjectPath[[i]] = path3
  sess = dir(path=subjectPath[[i]], pattern = "session[0-5]", recursive = FALSE)
  sessions[[i]] = c(sess)
}
View(meansDF)

# Get baseline, cutting, sutting files per subject per session
for(j in 1:21){
  num = length(sessions[[j]])
  counter = 1;
  sessionsList = vector("list", num)
  
  while(num != 0){
    path4 = paste(subjectPath[j],"session", sep="/")  # Add /session to the path
    path5 = paste(path4,counter, sep = "") # Add session number to the path. Ex: /session1

    baselineFile = list.files(path = path5, pattern = "_Baseline", full.names = FALSE, recursive = FALSE ) # Get baseline file
    print(baselineFile)
    baselineFilePath = paste(path5, baselineFile, sep = "/")
    
    cuttingFile = list.files(path = path5, pattern = "Cutting\\d\\W", full.names = FALSE, recursive = TRUE )
    print(cuttingFile)
    cuttingFilePath = paste(path5, cuttingFile, sep = "/")
    
    suturingFile = list.files(path = path5, pattern = "Suturing\\d\\W", full.names = FALSE, recursive = TRUE )
    print(suturingFile)
    suturingFilePath = paste(path5, suturingFile, sep = "/")
    
    sessionsList[[counter]] = c(baselineFilePath, cuttingFilePath, suturingFilePath)

    num = num - 1;
    counter = counter + 1
  }
  subjectList[[j]] = c(sessionsList)
}
View(subjectList)

# Create data frame for means
# SUBJECT = as.numeric(0), SESSION = as.numeric(0), CUTTING = as.numeric(0), SUTURING = as.numeric(0), BASELINE = as.numeric(0),CUT_MINUS_BASE = as.numeric(0), SUT_MINUS_BASE = as.numeric(0)
x <- c("SUBJECT", "SESSION", "CUTTING", "SUTURING", "BASELINE", "CUT_MINUS_BASE","SUT_MINUS_BASE")
colnames(meansDF) <- x

View(meansDF)

library(ggplot2)
library(ggpubr)
library(magrittr)
library(lemon)
library(gridExtra)
library(cowplot)

sessionNums = c(" ","Session 1", "Session 2","Session 3", "Session 4", "Session 5")

####################### LOOP OVER SUBJECTS STARTS HERE ########################################
cols <- c("Baseline" = "BLACK", "Cutting"= "GREEN", "Suturing" = "RED")

j = 20   ##### Change patient here manually

  fileName = subjectNumbers[j]
  subjectNumber = substr(fileName, 8,9)
  
  if(j > 13){                
    c = j + 5
    subjectTitle = paste("Subject", c, sep = " ")
    subj = paste("Subject", c, sep = " ")
  } else {
    subjectTitle = paste("Subject", j, sep = " ")
    subj = paste("Subject", j, sep = " ")
  }
  
  plotList = list()
  pdf(paste(fileName,".pdf", sep = ""))
  par(mfrow = c(3,2))

  plot1 <- ggplot() + labs(title=paste("\n\n\n\n\n", subj))
  plotList[[1]] = plot1
  if(length(subjectList[[j]])){      # Removed this check to print subject with less than five sessions == 5
  for(k in 1:length(subjectList[[j]])){
    meansDF[nrow(meansDF) + 1,] <- c(0, 0, 0, 0, 0, 0, 0)
    meansDF$SUBJECT[nrow(meansDF)] = j
    meansDF$SESSION[nrow(meansDF)] = k
    for(m in 1:length(subjectList[[j]][[k]])){
      
################## PRE-PROCESS DATA #############################################      
      print(subjectList[[j]][[k]][[m]]) 
      # try catch here 
      if(file.exists(subjectList[[j]][[k]][[1]])){
        dataBase = read.csv(subjectList[[j]][[k]][[1]], header = TRUE)
        #sampleData = read_excel("C:/Users/gustavo.arismendi/Documents/SmallSample.xlsx")
        #df = data.frame(sampleData) # Turn data to data frame
        dataBase$Time = floor(as.numeric(dataBase$Time)) # Turn time to integer using math floor
        
        dataBase = aggregate(. ~ dataBase$Time, data = dataBase, mean) # Add all frames with same time value
        dataBase$Frame. = NULL   # Delete frame column
        colnames(dataBase)[colnames(dataBase)=="dataBase$Time"] <- "Frame."  # Rename time column to frame
        
        # Add mean of baseline to meansDF
        totalSessionMeanBase = mean(dataBase$Perspiration)
        meansDF$BASELINE[NROW(meansDF)] = totalSessionMeanBase
        
        #attach(dataBase)
      } else {
        dataBase = NULL
      }
      if(file.exists(subjectList[[j]][[k]][[2]])){
        dataCut = read.csv(subjectList[[j]][[k]][[2]], header = TRUE)
        #attach(dataCut)
        dataCut$Time = floor(as.numeric(dataCut$Time)) # Turn time to integer using math floor
        
        dataCut = aggregate(. ~ dataCut$Time, data = dataCut, mean) # Add all frames with same time value
        dataCut$Frame. = NULL   # Delete frame column
        colnames(dataCut)[colnames(dataCut)=="dataCut$Time"] <- "Frame."  # Rename time column to frame
        
        # Add mean of cutting to meansDF
        totalSessionMeanCutting = mean(dataCut$Perspiration)
        meansDF$CUTTING[NROW(meansDF)] = totalSessionMeanCutting
      } else {
        dataCut = NULL
      }
      if(file.exists(subjectList[[j]][[k]][[3]])){
        dataSut = read.csv(subjectList[[j]][[k]][[3]], header = TRUE)
        #attach(dataSut)
        dataSut$Time = floor(as.numeric(dataSut$Time)) # Turn time to integer using math floor
        
        dataSut = aggregate(. ~ dataSut$Time, data = dataSut, mean) # Add all frames with same time value
        dataSut$Frame. = NULL   # Delete frame column
        colnames(dataSut)[colnames(dataSut)=="dataSut$Time"] <- "Frame."  # Rename time column to frame
        
        totalSessionMeanSut = mean(dataSut$Perspiration)
        meansDF$SUTURING[NROW(meansDF)] = totalSessionMeanSut
        
      } else {
        dataSut = NULL
      }
      print("Inside triple for")
    }
    mainTitle = paste(subjectTitle ,"Session ", k)
    print(mainTitle)
    
##################### CREATE PLOTS ################################
    
    # No baseline
    if(is.null(dataBase)){
      # Plot cutting, line sutting
      if(!is.null(dataCut) && !is.null(dataSut)){ 
        print("Inside NO BASELINE PLOT CUTTING AND SUT")
        # plot(x=dataCut$Time, y=dataCut$Perspiration,type="l", col = "green", ylim=c(0,0.02), xlim=c(0,1250), xlab = "Time (secs)", ylab = expression(paste("Perspiration °C"^"2")), main= mainTitle)
        # lines(dataSut$Time, dataSut$Perspiration, col="red", lty=1)
        plot1 <- ggplot() + 
          geom_line(data = dataCut, aes(x = Time, y = Perspiration, colour = "Cutting")) +
          geom_line(data = dataSut, aes(x = Time, y = Perspiration, colour = "Suturing")) +
          xlab("Time (secs)") +
          ylab(expression({"Perspiration °C"}^{2})) +
          ylim(0,0.02) +
          xlim(0,1250) + 
          scale_colour_manual(name="Tasks", values=cols)
        plotList[[k]] = plot1
        }
      # Plot sutting only
      if(is.null(dataCut) && !is.null(dataSut)){ 
        print("Inside NO BASELINE PLOT SUT ONLY")
        # plot(x=dataSut$Time, y=dataSut$Perspiration,type="l", col = "red", ylim=c(0,0.02),xlim=c(0,1250), xlab = "Time (secs)", ylab = expression(paste("Perspiration °C"^"2")), main= mainTitle)
        plot2 <- ggplot() + 
          geom_line(data = dataSut, aes(x = Time, y = Perspiration, colour = "Suturing")) +
          xlab("Time (secs)") +
          ylab(expression({"Perspiration °C"}^{2})) +
          ylim(0,0.02) +
          xlim(0,1250) +
          scale_colour_manual(name="Tasks", values=cols)
        plotList[[k + 1]] = plot2
        }
      # Plot cutting
      if(!is.null(dataCut) && is.null(dataSut)){ 
        print("Inside NO BASELINE PLOT CUT ONLY")
        # plot(x=dataCut$Time, y=dataCut$Perspiration,type="l", col = "green", ylim=c(0,0.02), xlim=c(0,1250), xlab = "Time (secs)", ylab = expression(paste("Perspiration °C"^"2")), main= mainTitle)
        plot3 <- ggplot() + 
          geom_line(data = dataCut, aes(x = Time, y = Perspiration, colour = "Cutting")) +
          xlab("Time (secs)") +
          ylab(expression({"Perspiration °C"}^{2})) +
          ylim(0,0.02) +
          xlim(0,1250) +
          scale_colour_manual(name="Tasks", values=cols)
        plotList[[k + 1]] = plot3
        }
    }
    # Baseline available
    if(!is.null(dataBase)){
      # Plot baseline, line sutting 
      if(is.null(dataCut) && !is.null(dataSut)){
        print("Inside BASELINE PLOT SUT ONLY")
        # plot(x=dataBase$Time, y=dataBase$Perspiration,type="l", ylim=c(0,0.02), xlim=c(0,1250), xlab = "Time (secs)", ylab = expression(paste("Perspiration °C"^"2")), main= mainTitle)
        # lines(dataSut$Time, dataSut$Perspiration, col="red", lty=1)
        plot4 <- ggplot() + 
          geom_line(data = dataBase, aes(x = Time, y = Perspiration, colour = "Baseline" )) +
          geom_line(data = dataSut, aes(x = Time, y = Perspiration, colour = "Suturing")) +
          xlab("Time (secs)") +
          ylab(expression({"Perspiration °C"}^{2})) +
          ylim(0,0.02) +
          xlim(0,1250) +
          scale_colour_manual(name="Tasks", values=cols)
        plotList[[k + 1]] = plot4
      }
      # Plot baseline, line cutting
      if(is.null(dataSut) && !is.null(dataCut)){
        print("Inside BASELINE CUT ONLY")
        # plot(x=dataBase$Time, y=dataBase$Perspiration,type="l", ylim=c(0,0.02), xlim=c(0,1250), xlab = "Time (secs)", ylab = expression(paste("Perspiration °C"^"2")), main= mainTitle)
        # lines(dataCut$Time, dataCut$Perspiration, col="green", lty=1)
        plot5 <- ggplot() + 
          geom_line(data = dataBase, aes(x = Time, y = Perspiration, colour = "Baseline" )) +
          geom_line(data = dataCut, aes(x = Time, y = Perspiration, colour = "Cutting")) +
          xlab("Time (secs)") +
          ylab(expression({"Perspiration °C"}^{2})) +
          ylim(0,0.02) +
          xlim(0,1250) +
          scale_colour_manual(name="Tasks", values=cols)
        plotList[[k + 1]] = plot5
      }
      # Plot all
      if(!is.null(dataSut) && !is.null(dataCut)){
        print("Inside BASELINE PLOT ALL")
        # plot(x=dataBase$Time, y=dataBase$Perspiration,type="l", ylim=c(0,0.02), xlim=c(0,1250), xlab = "Time (secs)", ylab = expression(paste("Perspiration °C"^"2")), main= mainTitle)
        # lines(dataCut$Time, dataCut$Perspiration, col="green", lty=1)
        # lines(dataSut$Time, dataSut$Perspiration, col="red", lty=1)
        
        plot6 <- ggplot() + 
          geom_line(data = dataBase, aes(x = Time, y = Perspiration, colour = "Baseline" )) +
          geom_line(data = dataCut, aes(x = Time, y = Perspiration, colour = "Cutting")) +
          geom_line(data = dataSut, aes(x = Time, y = Perspiration, colour = "Suturing")) +
          xlab("Time (secs)") +
          ylab(expression({"Perspiration °C"}^{2})) +
          ylim(0,0.02) +
          xlim(0,1250) +
          scale_colour_manual(name="Tasks", values=cols)
        plotList[[k + 1]] = plot6
        legendP6 <- get_legend(plot6)
      }
    }
  }
  }
  #grid.arrange(grobs=plotList, nrow = 3, ncol = 2, top= "TEST TITLE GUS") 
######################## PRINT PLOTS IN GRID ################################
  ggarrange(plotlist = plotList, nrow = 3, ncol = 2, common.legend = TRUE, label.x=0.5, legend = "right", labels = sessionNums) + labs(title=paste("\n\n\n\n\n\n\n\n", subj))
  dev.off()

####################### LOOP OVER SUBJECTS ENDS HERE ##########################################
###############################################################################################  
  

#### Clean data before exporting
# Calculate difference of mean from baseline
options(scipen=999) # Format data from scientific to normal notation
# Cutting minus base 
meansDF$CUT_MINUS_BASE = meansDF$CUTTING - meansDF$BASELINE
# Suturing minus base 
meansDF$SUT_MINUS_BASE = meansDF$SUTURING - meansDF$BASELINE
View(meansDF)

# If baseline is zero make cut or sut NA
indices <- (meansDF$BASELINE == 0) 
meansDF$CUT_MINUS_BASE[indices] = NA
meansDF$SUT_MINUS_BASE[indices] = NA

# Number patients correctly
meansDF$SUBJECT[57:61] = 19 # Turn subject 14 into 19
meansDF$SUBJECT[62] = 20 # Turn subject 15 into 20
meansDF$SUBJECT[63:67] = 21 # Turn subject 16 into 21
meansDF$SUBJECT[68:72] = 22 # Turn subject 17 into 22
meansDF$SUBJECT[73:76] = 23 # Turn subject 18 into 23
meansDF$SUBJECT[77:81] = 24 # Turn subject 19 into 24
meansDF$SUBJECT[82:83] = 25 # Turn subject 20 into 25
meansDF$SUBJECT[84:88] = 26 # Turn subject 21 into 26

# Remove patients that don't have performance score
meansDF <- meansDF[-21:-24,] # Remove patients 5 and 6
meansDF <- meansDF[-31:-32,] # Remove patient 9
meansDF <- meansDF[-56,] # Remove patient 20
meansDF <- meansDF[-66:-69,] # Remove patient 23
meansDF <- meansDF[-71:-72,] # Remove patient 23
View(meansDF)

# meansDFBackUP <- meansDF
# View(meansDFBackUP)

# Get mins to do linear transformations
min(meansDF[,6], na.rm = T)  # min of cutting minus base is -0.01737066
min(meansDF[,7], na.rm = T)  # min of suturing minus base is -0.02947824

# Transform cutting to positive values + error (0.2)  
meansDF$CUT_MINUS_BASE = meansDF$CUT_MINUS_BASE + (.01740 + 0.2)

# Transform suturing to positive values + error (0.2)  
meansDF$SUT_MINUS_BASE = meansDF$SUT_MINUS_BASE + (.02948 + 0.2)

# Plot average perspiration per session for cutting and suturing
for(m in 1:21){
  plot(x=df$SESSION[1:5], y=df$CUT_MINUS_BASE[1:5],type="l", ylim=c(-.025,.025), xlim=c(1,5), xlab = "Session", ylab = "Average perspiration", main= "Average perspiration per session")
}
plot(x=df$SESSION[1:5], y=df$CUT_MINUS_BASE[1:5],type="l", ylim=c(-.025,.025), xlim=c(1,5), xlab = "Session", ylab = "Average perspiration", main= "Average perspiration per session")


# Create vector of cutting - base and sut - base
vec = c()
for(i in 1:length(meansDF$SUBJECT)){
  vec <- c(vec, meansDF$CUT_MINUS_BASE[i])  # Changed this from cut_minus_base
  vec <- c(vec, meansDF$SUT_MINUS_BASE[i])  # Changed this from sut_minus_base
}
View(vec)

vec <- rep(vec, each=2, length.out= 300)
View(vec)

# Export data to csv
write.csv(vec, file = "FinalVector15SubjectsPositiveValuePlusError.csv")

write.csv(meansDF, file = "TotalMeansAllInfo.csv")

############################## FOR PRINTING CUTTING AND SUTURING AVERAGES GRAPHS #######################
# Modify empty rows to have line continuity. Ask if this is ok ?????????????????????????????????????????
indices <- (meansDF$BASELINE == 0) 
meansDF$CUT_MINUS_BASE[indices] = 0
meansDF$SUT_MINUS_BASE[indices] = 0

# Graph cutting patients
pdf("AverageAllPatientsCutting.pdf")
par(mfrow = c(3,2))
for(n in 1:21) {
  if( n < 14){
    Subject = n
  } else {
    Subject = n +5
  }
  
  subset = meansDF[meansDF$SUBJECT == n, ]
  plot(x=subset$SESSION, y=subset$CUT_MINUS_BASE,type="l", ylim=c(min(meansDF$CUT_MINUS_BASE),max(meansDF$CUT_MINUS_BASE)), xlim=c(1,5), xlab = "Session number", ylab = expression(paste("Average perspiration °C"^"2")), main= paste("Cutting Session Average for Subject", Subject))
}
dev.off()

# Graph suturing patients
pdf("AverageAllPatientsSuturing.pdf")
par(mfrow = c(3,2))
for(n in 1:21) {
  if( n < 14){
    Subject = n
  } else {
    Subject = n +5
  }
  
  subset = meansDF[meansDF$SUBJECT == n, ]
  plot(x=subset$SESSION, y=subset$SUT_MINUS_BASE,type="l", ylim=c(min(meansDF$SUT_MINUS_BASE),max(meansDF$SUT_MINUS_BASE)), xlim=c(1,5), xlab = "Session number", ylab = expression(paste("Average perspiration °C"^"2")), main= paste("Suturing Session Average for Subject", Subject))
}
dev.off()

