# Author: Methun Kamruzzaman
# Date: December 10th, 2018

# Clear all data
rm(list=ls())

# Load library
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}else{
  library(dplyr)
}

if(!require(ISLR)){
  install.packages("ISLR")
  library(ISLR)
}else{
  library(ISLR)
}

if(!require(pheatmap)){
  install.packages("pheatmap")
  library(pheatmap)
}else{
  library(pheatmap)
}

if(!require(lme4)){
  install.packages("lme4")
  library(lme4)
}else{
  library(lme4)
}

if(!require(drc)){
  install.packages("drc")
  library(drc)
}else{
  library(drc)
}

if(!require(scales)){
  install.packages("scales")
  library(scales)
}else{
  library(scales)
}

if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}else{
  library(RColorBrewer)
}

if(!require(gplots)){
  install.packages("gplots")
  library(gplots)
}else{
  library(gplots)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}else{
  library(ggplot2)
}

hData = read.csv(file.choose())

# Compute CDOT
hData$DOT_on_day = ifelse(hData$AgentId>0, yes=1, no=0)

#Compute cummulative DOT based on HospitalID, patientID, AdmissonID
hData = as.data.frame(hData%>%group_by(HospitalId, PatientId, AdmissionId)%>%mutate(cDOT=cumsum(DOT_on_day)))

# Compute hospital patient chart
hpData = as.data.frame(hData[,c(2,3)]%>%distinct())
hid = as.character(hpData$HospitalId)
ggplot(data.frame(hid), aes(x=hid)) +
  geom_bar()+
  ggtitle("Number of patients in hospitals") + xlab("Hospital ID") + ylab("Number of patients")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90))
rm(hpData, hid)


# Compute hospital, patient, admission
hpData = as.data.frame(hData[,c(2,3,4)]%>%distinct())

# Compute hospital, patient, agentID
hpData = hData%>%filter(AgentId>0)%>%group_by(AgentId)%>%summarise(cnt=n())
ggplot(data.frame(hpData), aes(x=as.character(hpData$AgentId), y=log10(hpData$cnt))) +
  geom_bar(stat="identity")+
  ggtitle("Number of antibiotics in hospitals") + xlab("Antibiotics ID") + ylab("Number of uses in log scale")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90))
rm(hpData)

# Count the number of agent applied in a hospital
ac = as.data.frame(hData%>%filter(AgentId>0)%>%group_by(HospitalId, AgentId)%>%summarise(count=n()))

# Create heatmap
uh = sort(unique(ac$HospitalId))
ua = sort(unique(ac$AgentId))
n = length(uh)*length(ua)
hMat = matrix(rep(c(0), n), nrow = length(ua), dimnames = list(as.character(ua), as.character(uh)))
for(i in 1:length(ua)){
  for(j in 1:length(uh)){
    tmp = as.data.frame(ac%>%filter(HospitalId==as.integer(uh[j]) & AgentId==as.integer(ua[i])))
    if(nrow(tmp)>0){
      hMat[i,j]=tmp$count
    }
    rm(tmp)
  }
}
rm(ac, uh, ua, n)

# Normalize column wise based on max value
hMat = apply(hMat,2, norm<-function(x){return (x/max(x))})
 
# Create heatmap
pheatmap(hMat)

# Patient age
# Remove \\N age from dataset
hData = as.data.frame(hData[-which(hData$Age=="\\N"),])
hData$Age = as.integer(hData$Age)

# Create age category
# Infant: 0-1
# Kids: 2-11
# Teen: 12-17
# Adult: 18-54
# Old: 55-65
# Older: 65+
hData$AgeCategory=""
hData$AgeCategory[which(hData$Age<2)]="Infant(<2)"
hData$AgeCategory[which(hData$Age>=2 & hData$Age<12)]="Kids(<12)"
hData$AgeCategory[which(hData$Age>=12 & hData$Age<18)]="Teen(<18)"
hData$AgeCategory[which(hData$Age>=18 & hData$Age<55)]="Adult(<55)"
hData$AgeCategory[which(hData$Age>=55 & hData$Age<66)]="Old(<66)"
hData$AgeCategory[which(hData$Age>=66)]="Older(>65)"

hData$AgeCategoryOrder=0
hData$AgeCategoryOrder[which(hData$Age<2)]=1
hData$AgeCategoryOrder[which(hData$Age>=2 & hData$Age<12)]=2
hData$AgeCategoryOrder[which(hData$Age>=12 & hData$Age<18)]=3
hData$AgeCategoryOrder[which(hData$Age>=18 & hData$Age<55)]=4
hData$AgeCategoryOrder[which(hData$Age>=55 & hData$Age<66)]=5
hData$AgeCategoryOrder[which(hData$Age>=66)]= 6

hData$AgeCategoryColor="white"
hData$AgeCategoryColor[which(hData$Age<2)]="blue"
hData$AgeCategoryColor[which(hData$Age>=2 & hData$Age<12)]="cyan4"
hData$AgeCategoryColor[which(hData$Age>=12 & hData$Age<18)]="coral"
hData$AgeCategoryColor[which(hData$Age>=18 & hData$Age<55)]="green"
hData$AgeCategoryColor[which(hData$Age>=55 & hData$Age<66)]="darkmagenta"
hData$AgeCategoryColor[which(hData$Age>=66)]= "red"

# Violine plot
data_summary <- function(x) {
  mu <- mean(x)
  sigma1 <- mu-sd(x)
  sigma2 <- mu+sd(x)
  return(c(y=mu,ymin=sigma1,ymax=sigma2))
}
ggplot(data=as.data.frame(hData%>%filter(AgentId>0)), aes(x=as.character(hData$AgentId[which(hData$AgentId>0)]), y=hData$Age[which(hData$AgentId>0)])) + 
  geom_violin() + stat_summary(fun.data=data_summary)

# Multiple bar plot
haaData = as.data.frame(hData%>%filter(AgentId>0)%>%group_by(AgentId, AgeCategory, AgeCategoryOrder)%>%summarise(cnt=n()))%>%order_by(AgeCategoryOrder)

ggplot(data=haaData, aes(x=as.character(AgentId), y=log10(cnt), fill=AgeCategory)) +
  geom_bar(stat="identity")+
  ggtitle("Antibiotics used in different age groups") + xlab("Antibiotics ID") + ylab("Number of uses in log scale")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 90))

# Create hospital category
hData$HospitalCategory = ifelse(hData$HospitalId==2000, yes="Duke", no="No-Duke")

# Save data with age category information
write.csv(hData[,-c(1)], "/Users/methun/self/Research/Dataset/Hospital/Hospital.csv")

# Plot 
tmp=as.data.frame(hData[,c(17,19)]%>%distinct())
plot(hData$DaySinceAdmission, hData$cDOT, col=hData$AgeCategoryColor, xlab = "Days Since Admission", ylab = "cDOT")
legend("topleft", legend=tmp$AgeCategory, col=tmp$AgeCategoryColor, lty=1:2, cex=0.8)


phData = as.data.frame(read.csv(file.choose()))
phData = as.data.frame(phData[,c(2:4,6:12)]%>%distinct())
phData$Location = ifelse(phData$Loc=="K", yes = "Kansas", no="Nebraska")


path_1 = data.frame("dap"=c("D1",59.00,60.50,55.50,56.50,51.00), "hum"=c("H1",59.16,58.02,64.08,60.15,76.48))
path_2 = data.frame("dap"=c("D2",90.50,89.50,91.19,92.42,92.19,98.00,95.75,98.50), "hum"=c("H2",34.54,26.26,37.15,65.60,49.08,66.82,66.50,72.44))
path_3 = data.frame("dap"=c("D3",65.50,69.00,65.50,72.00,75.00), "hum"=c("H3",28.85,31.03,28.85,35.25,40.22))
path_4 = data.frame("dap"=c("D4",77.50,79.49,72.00,75.50), "hum"=c("H4",24.35,23.29,35.25,32.71))
path_5 = data.frame("dap"=c("D5",48.50,49.33,51.00,45.79,41.66), "hum"=c("H5",72.31,75.83,76.48,64.94,60.11))
path_6 = data.frame("dap"=c("D6",41.99,44.75,39.00,41.66,35.50), "hum"=c("H6",55.59,54.33,66.18,60.11,66.96))
path_7 = data.frame("dap"=c("D7",5.50,7.00,7.50,9.55,11.00,12.12,15.20,19.33,22.60), "hum"=c("H7",19.17,20.10,38.22,51.67,60.23,60.62,60.93,62.69,71.92))
path_8 = data.frame("dap"=c("D8",52.00,54.75,56.50,58.83,60.00), "hum"=c("H8",18.74,21.22,30.97,36.60,49.60))
path_9 = data.frame("dap"=c("D9",41.99,45.40,49.50,52.00,39.59), "hum"=c("H9",22.96,22.64,22.58,18.74,24.61))
path_10 = data.frame("dap"=c("D10",25.50,29.00,32.50,22.60), "hum"=c("H10",70.97,65.74,61.89,71.92))
path_11 = data.frame("dap"=c("D11",21.40,19.66,19.33,22.49), "hum"=c("H11",46.86,52.84,62.69,35.07))
path_12 = data.frame("dap"=c("D12",29.60,32.00,25.00,34.50,35.50,22.49), "hum"=c("H12",27.96,27.36,29.67,24.48,21.74,35.07))

path = as.data.frame(path_1)
path = as.data.frame(rbind(path, path_2))
path = as.data.frame(rbind(path, path_3))
path = as.data.frame(rbind(path, path_4))
path = as.data.frame(rbind(path, path_5))
path = as.data.frame(rbind(path, path_6))
path = as.data.frame(rbind(path, path_7))
path = as.data.frame(rbind(path, path_8))
path = as.data.frame(rbind(path, path_9))
path = as.data.frame(rbind(path, path_10))
path = as.data.frame(rbind(path, path_11))
path = as.data.frame(rbind(path, path_12))


write.csv(phData[,c(1,8,3)]%>%distinct(), "/Users/methun/self/Research/Publications/TCBB/2019/tcbb_2019/Figures/data.csv")


p1 = ggplot(phData, aes(x = DAP, y = Humidity,
                           colour = Location,
                           shape = Location)) + 
  geom_point(size = 3) + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 14, face="bold"), 
        plot.title = element_text(size=rel(3.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  labs(color='Location') +
  labs(shape='Location') +
  ggtitle("") +
  labs(x = "Days After Planting (DAP)", y = "Humidity (%)")

p1 = p1 + ggplot(path_1) + geom_point(aes(x = dap, y = hum), size = 3) + geom_line(data = path_1, aes(x = dap, y = hum))
p1

ggplot(path_1) + 
  geom_point(aes(x = dap, y = hum), size = 3) +
  geom_line(data = path_1, aes(x = dap, y = hum))



ggplot(hData, aes(x = DaySinceAdmission, y = cDOT)) + 
  geom_point(size = 2) + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 14, face="bold"), 
        plot.title = element_text(size=rel(3.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  #labs(color='Location') +
  #labs(shape='Location') +
  ggtitle("") +
  labs(x = "Days Since Admission (DSA)", y = "Cumulative Days On Therapy (cDOT)")


