source("header.R")

grData_Dry_rep1=read.csv("Dry_Field/dry_data_genotype_rep1.csv")
grData_Dry_rep2=read.csv("Dry_Field/dry_data_genotype_rep2.csv")
grData_Dry=read.csv("Dry_Field/dry_data_genotype.csv")

grData_Wet_rep1=read.csv("Irrigated_Field/irri_data_genotype_rep1.csv")
grData_Wet_rep2=read.csv("Irrigated_Field/irri_data_genotype_rep2.csv")
grData_Wet=read.csv("Irrigated_Field/irri_data_genotype.csv")


###### Find genotype which belongs in both locations and have beth replica
grData_Dry$Location='Dry'
grData_Wet$Location='Wet'

grData = as.data.frame(rbind(grData_Dry, grData_Wet))
grData = as.data.frame(grData[,-c(1)])
grData = as.data.frame(grData[,c(1:3,12,4,5:11)]%>%arrange(Genotype,Location,Replica,DAP))

# Unique Genotype, location, replica
df = as.data.frame(grData[,c(3:5)]%>%distinct())
dfg = as.data.frame(df%>%group_by(Genotype)%>%summarise(obs=n()))
dfg = as.data.frame(dfg%>%filter(obs>2))

## Select genotypes in grdata
grData = as.data.frame(grData%>%filter(Genotype%in%dfg$Genotype)%>%arrange(Genotype,Location,Replica,DAP))

## Add color bases on location, replica combination
grData$LRColor = ""
grData$LRColor[grData$Location=='Dry' & grData$Replica==1]="red"
grData$LRColor[grData$Location=='Dry' & grData$Replica==2]="purple"
grData$LRColor[grData$Location=='Wet' & grData$Replica==1]="darkgreen"
grData$LRColor[grData$Location=='Wet' & grData$Replica==2]="blue"

write.csv(grData, "GrowthRate.csv")

## Plot each genotype 
# For each genotype, plot the growth curve
grData = read.csv("GrowthRate.csv")
grData = as.data.frame(grData[,-c(1)])
grData$Rep=as.character(grData$Replica)

imgPath = "img/"
genList = unique(grData$Genotype)

dev.set(which = dev.next())
for(g in genList){
  df= as.data.frame(grData%>%filter(Genotype==as.character(g)))
  
  p=ggplot(df, aes(x = DAP, y = GrowthRate,
                                 colour = Location,
                                  shape=Rep)) + 
    geom_point() + 
    theme(axis.title.x = element_text(size=20), 
          axis.text.x = element_text(size=rel(1)), 
          axis.title.y = element_text(size=20),
          axis.text.y = element_text(size=rel(1)), 
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12), 
          plot.title = element_text(size=rel(3.0), hjust = 0.5), 
          strip.text = element_text(face="bold", size = rel(2)),
          strip.background = element_rect(fill="white", colour="black",size=2)) + 
    ggtitle("Growth Rate VS DAP") +
    labs(x = "Days After Planting (DAP)", y = "Growth Rate (cm./day)")
  
  while (!is.null(dev.list()))  dev.off()
  pdf(file=paste(imgPath,as.character(g),".pdf",sep=""))
  #plot(grData$DAP[t], grData$GrowthRate[t], col=grData$LRColor[t], pch=16)
  #legend("topright", legend = c("Dry-1","Dry-2","Wet-1","Wet-2"), col = c("red","purple",'darkgreen','blue'), lwd = 2)
  print(p)
  while (!is.null(dev.list()))  dev.off()
}
while (!is.null(dev.list()))  dev.off()

########### Plot dry rep 1
imgPath = "drep1/"
genList = unique(grData_Dry_rep1$Genotype)

dev.set(which = dev.next())
for(g in genList){
  df= as.data.frame(grData_Dry_rep1%>%filter(Genotype==as.character(g)))
  
  p=ggplot(df, aes(x = DAP, y = GrowthRate)) + 
    geom_point() + 
    theme(axis.title.x = element_text(size=20), 
          axis.text.x = element_text(size=rel(1)), 
          axis.title.y = element_text(size=20),
          axis.text.y = element_text(size=rel(1)), 
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12), 
          plot.title = element_text(size=rel(3.0), hjust = 0.5), 
          strip.text = element_text(face="bold", size = rel(2)),
          strip.background = element_rect(fill="white", colour="black",size=2)) + 
    ggtitle("Growth Rate VS DAP") +
    labs(x = "Days After Planting (DAP)", y = "Growth Rate (cm./day)")
  
  while (!is.null(dev.list()))  dev.off()
  pdf(file=paste(imgPath,as.character(g),".pdf",sep=""))
  #plot(grData$DAP[t], grData$GrowthRate[t], col=grData$LRColor[t], pch=16)
  #legend("topright", legend = c("Dry-1","Dry-2","Wet-1","Wet-2"), col = c("red","purple",'darkgreen','blue'), lwd = 2)
  print(p)
  while (!is.null(dev.list()))  dev.off()
}
while (!is.null(dev.list()))  dev.off()

ggplot(grData_Dry_rep1, aes(x = cGDD, y = GrowthRate)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=20), 
        axis.text.x = element_text(size=rel(1)), 
        axis.title.y = element_text(size=20),
        axis.text.y = element_text(size=rel(1)), 
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(3.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  ggtitle("Dry field with replica 1") +
  labs(x = "Cum. Growing Degree Days (cGDD)", y = "Growth Rate (cm./day)")


ggplot(grData_Dry_rep2, aes(x = cGDD, y = GrowthRate)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=20), 
        axis.text.x = element_text(size=rel(1)), 
        axis.title.y = element_text(size=20),
        axis.text.y = element_text(size=rel(1)), 
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(3.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  ggtitle("Dry field with replica 2") +
  labs(x = "Cum. Growing Degree Days (cGDD)", y = "Growth Rate (cm./day)")

ggplot(grData_Wet_rep1, aes(x = cGDD, y = GrowthRate)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=20), 
        axis.text.x = element_text(size=rel(1)), 
        axis.title.y = element_text(size=20),
        axis.text.y = element_text(size=rel(1)), 
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(3.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  ggtitle("Wet field with replica 1") +
  labs(x = "Cum. Growing Degree Days (cGDD)", y = "Growth Rate (cm./day)")


ggplot(grData_Wet_rep2, aes(x = cGDD, y = GrowthRate)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=20), 
        axis.text.x = element_text(size=rel(1)), 
        axis.title.y = element_text(size=20),
        axis.text.y = element_text(size=rel(1)), 
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(3.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  ggtitle("Wet field with replica 2") +
  labs(x = "Cum. Growing Degree Days (cGDD)", y = "Growth Rate (cm./day)")


########## Dry rep1-rep2 for each genotype
df = grData_Dry%>%group_by(Genotype,Replica)%>%summarise(minDAP=min(DAP))
df = df%>%group_by(Genotype)%>%summarise(DAP=max(minDAP))

grData_Dry_repDiff=NULL
genList = unique(df$Genotype)

for(g in genList){
  t = which(df$Genotype==as.character(g))
  t1 = which(grData_Dry_rep1$Genotype==as.character(g) & grData_Dry_rep1$DAP>=df$DAP[t])
  t2 = which(grData_Dry_rep2$Genotype==as.character(g) & grData_Dry_rep2$DAP>=df$DAP[t])
  
  tmp = grData_Dry_rep1[t1,]
  tmp$pHeightDiff = grData_Dry_rep1$pHeight[t1]-grData_Dry_rep2$pHeight[t2]
  tmp$GrowthRateDiff = grData_Dry_rep1$GrowthRate[t1]-grData_Dry_rep2$GrowthRate[t2]
  
  tmp = as.data.frame(tmp[,c(2:4,13,14,8:12)])
  
  if(is.null(grData_Dry_repDiff)){
    grData_Dry_repDiff = as.data.frame(tmp)
  }else{
    grData_Dry_repDiff = as.data.frame(rbind(grData_Dry_repDiff, tmp))
  }
  
  rm(tmp, t, t1, t2)
}
rm(df)

write.csv(grData_Dry_repDiff,"Dry_Field/dry_data_repDiff.csv")

########## Wet rep1-rep2 for each genotype
df = grData_Wet%>%group_by(Genotype,Replica)%>%summarise(minDAP=min(DAP))
df = df%>%group_by(Genotype)%>%summarise(DAP=max(minDAP))

grData_Wet_repDiff=NULL
genList = unique(df$Genotype)

for(g in genList){
  t = which(df$Genotype==as.character(g))
  t1 = which(grData_Wet_rep1$Genotype==as.character(g) & grData_Wet_rep1$DAP>=df$DAP[t])
  t2 = which(grData_Wet_rep2$Genotype==as.character(g) & grData_Wet_rep2$DAP>=df$DAP[t])
  
  tmp = grData_Wet_rep1[t1,]
  tmp$pHeightDiff = grData_Wet_rep1$pHeight[t1]-grData_Wet_rep2$pHeight[t2]
  tmp$GrowthRateDiff = grData_Wet_rep1$GrowthRate[t1]-grData_Wet_rep2$GrowthRate[t2]
  
  tmp = as.data.frame(tmp[,c(2:4,13,14,8:12)])
  
  if(is.null(grData_Dry_repDiff)){
    grData_Wet_repDiff = as.data.frame(tmp)
  }else{
    grData_Wet_repDiff = as.data.frame(rbind(grData_Wet_repDiff, tmp))
  }
  
  rm(tmp, t, t1, t2)
}
rm(df)

write.csv(grData_Wet_repDiff,"Irrigated_Field/irri_data_repDiff.csv")
