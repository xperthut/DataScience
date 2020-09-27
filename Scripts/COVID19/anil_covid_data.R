source("header.R")

covid_data = read.csv(file.choose())

# Filter data from Georgia state
covid_data_GA = as.data.frame(covid_data%>%filter(State=="Georgia"))

# Parameter estimation for DBSCAN
nw = seq(5,50,1)
ac=c()
alc=c()
ad=c()
ald=c()
ar=c()
alr=c()

for(j in nw){
  w = ceiling(112/j)
  rt = seq(1, 112+w, w)
  
  # cases
  l=c()
  for(i in 1:length(rt)){
    y = covid_data_GA$cases[which(covid_data_GA$DaysSinceFirstCase>= rt[i-1] & covid_data_GA$DaysSinceFirstCase<rt[i])]
    if(length(l)==0) l = sd(y)
    else l = append(l, sd(y))
  }
  l = na.exclude(l)
  if(length(ac)==0) ac=mean(l)
  else ac = append(ac, mean(l))
  
  # log cases
  l=c()
  for(i in 1:length(rt)){
    y = covid_data_GA$logCases[which(covid_data_GA$DaysSinceFirstCase>= rt[i-1] & covid_data_GA$DaysSinceFirstCase<rt[i])]
    if(length(l)==0) l = sd(y)
    else l = append(l, sd(y))
  }
  l = na.exclude(l)
  if(length(alc)==0) alc=mean(l)
  else alc = append(alc, mean(l))
  
  # deaths
  l=c()
  for(i in 1:length(rt)){
    y = covid_data_GA$deaths[which(covid_data_GA$DaysSinceFirstCase>= rt[i-1] & covid_data_GA$DaysSinceFirstCase<rt[i])]
    if(length(l)==0) l = sd(y)
    else l = append(l, sd(y))
  }
  l = na.exclude(l)
  if(length(ad)==0) ad=mean(l)
  else ad = append(ad, mean(l))
  
  # log deaths
  l=c()
  for(i in 1:length(rt)){
    y = covid_data_GA$logDeaths[which(covid_data_GA$DaysSinceFirstCase>= rt[i-1] & covid_data_GA$DaysSinceFirstCase<rt[i])]
    if(length(l)==0) l = sd(y)
    else l = append(l, sd(y))
  }
  l = na.exclude(l)
  if(length(ald)==0) ald=mean(l)
  else ald = append(ald, mean(l))
  
  # recovery
  l=c()
  for(i in 1:length(rt)){
    y = covid_data_GA$recovery[which(covid_data_GA$DaysSinceFirstCase>= rt[i-1] & covid_data_GA$DaysSinceFirstCase<rt[i])]
    if(length(l)==0) l = sd(y)
    else l = append(l, sd(y))
  }
  l = na.exclude(l)
  if(length(ar)==0) ar=mean(l)
  else ar = append(ar, mean(l))
  
  # log recovery
  l=c()
  for(i in 1:length(rt)){
    y = covid_data_GA$logRecovery[which(covid_data_GA$DaysSinceFirstCase>= rt[i-1] & covid_data_GA$DaysSinceFirstCase<rt[i])]
    if(length(l)==0) l = sd(y)
    else l = append(l, sd(y))
  }
  l = na.exclude(l)
  if(length(alr)==0) alr=mean(l)
  else alr = append(alr, mean(l))
}

par(mfrow=c(3,2))
plot(nw, ac, type='o')
plot(nw, alc, type='o')
plot(nw, ad, type='o')
plot(nw, ald, type='o')
plot(nw, ar, type='o')
plot(nw, alr, type='o')
par(mfrow=c(1,1))

covid_data_GA$log2Cases = log2(covid_data_GA$cases+1)
covid_data_GA$log2Deaths = log2(covid_data_GA$deaths+1)
covid_data_GA$log2Recovery = log2(covid_data_GA$recovery+1)

covid_data_GA$logeCases = log(covid_data_GA$cases+1)
covid_data_GA$logeDeaths = log(covid_data_GA$deaths+1)
covid_data_GA$logeRecovery = log(covid_data_GA$recovery+1)

# Save georgia data
write.csv(covid_data_GA[,2:ncol(covid_data_GA)], "/Users/methun/self/Research/Dataset/COVID-19/FromAnil/Covid_data_GA.csv")



###############################################################################################
###################################### Preprocessing step #####################################
###############################################################################################
covid_confirmed = read.csv(file.choose())
covid_death = read.csv(file.choose())
covid_recovered = read.csv(file.choose())

# Filter out unknown county
covid_confirmed = as.data.frame(covid_confirmed%>%filter(tolower(County)!="unknown"))
covid_death = as.data.frame(covid_death%>%filter(tolower(County)!="unknown"))
covid_recovered = as.data.frame(covid_recovered%>%filter(tolower(County)!="unknown"))

covid_confirmed$cases = covid_confirmed$Value
covid_death$deaths = covid_death$Value
covid_recovered$recovery = covid_recovered$Value

# Reformate the table
covid_confirmed = as.data.frame(covid_confirmed[,c(1:3,6)])
covid_death = as.data.frame(covid_death[,c(1:3,6)])
covid_recovered = as.data.frame(covid_recovered[,c(1:3,6)])

# Inner join to form final table
covid_cd = as.data.frame(inner_join(covid_confirmed, covid_death))
covid_data = as.data.frame(inner_join(covid_cd, covid_recovered))

# Remove other data files
rm(covid_cd, covid_confirmed, covid_death, covid_recovered)

# Change column name
names(covid_data)[3]<-"DaysSinceFirstCase"

# Log base 10 transform
covid_data$logDSFC = log10(covid_data$DaysSinceFirstCase)
covid_data$logCases = log10(covid_data$cases+1)
covid_data$logDeaths = log10(covid_data$deaths+1)
covid_data$logRecovery = log10(covid_data$recovery+1)

# Normalization and Standardization
covid_data = as.data.frame(covid_data%>%group_by(State, County)%>%mutate(normCases = getNormalizedData(cases),
                                                                                             normDeaths = getNormalizedData(deaths),
                                                                                             normRecovery = getNormalizedData(recovery),
                                                                                             stdCases = getStandardizeddData(cases),
                                                                                             stdDeaths = getStandardizeddData(deaths),
                                                                                             stdRecovery = getStandardizeddData(recovery)))


# Save to file
write.csv(covid_data, "/Users/methun/self/Research/Dataset/COVID-19/FromAnil/Covid_data.csv")

###############################################################################################
###################################### Preprocessing step Ends here ###########################
###############################################################################################

# Read state adjacency list matrix
adjSt = read.csv(file.choose(), header = F)
adjM = as.matrix(adjSt)
el <- as.data.frame(cbind(adjM[, 1], c(adjM[, -1])))
write.csv(el, "/Users/methun/self/Research/Dataset/COVID-19/FromAnil/Incident/state_adj_edge_list.csv")

## Read adjacency list
el = read.csv(file.choose())
g = graph.adjlist(el)
adjM = get.adjacency(graph.edgelist(as.matrix(el)))
g = graph.adjacency(adjM, mode="undirected")
prg = page_rank(g, directed = F)


################# 
df = read.csv(file.choose())
df = as.data.frame(df%>%group_by(State)%>%mutate(normval=getNormalizedData(Value), stdval=getStandardizeddData(Value)))
write.csv(df[,-1], "/Users/methun/self/Research/Dataset/COVID-19/FromAnil/Incident/covid_usa_state.csv")

## Parameter estimation
# Parameter estimation for DBSCAN
nw = seq(5,45,1)
ac=c()
alc=c()
ad=c()
ald=c()
ar=c()
alr=c()
m = max(df$Day)

for(j in nw){
  w = ceiling(m/j)
  rt = seq(1, m+w, w)
  
  # cases
  l=c()
  for(i in 1:length(rt)){
    y = df$Value[which(df$Day>= rt[i-1] & df$Day<rt[i])]
    if(length(l)==0) l = sd(y)
    else l = append(l, sd(y))
  }
  l = na.exclude(l)
  if(length(ac)==0) ac=mean(l)
  else ac = append(ac, mean(l))
  
  # log2 cases
  l=c()
  for(i in 1:length(rt)){
    y = df$LogTwo[which(df$Day>= rt[i-1] & df$Day<rt[i])]
    if(length(l)==0) l = sd(y)
    else l = append(l, sd(y))
  }
  l = na.exclude(l)
  if(length(alc)==0) alc=mean(l)
  else alc = append(alc, mean(l))
  
  # log10 cases
  l=c()
  for(i in 1:length(rt)){
    y = df$LogValue[which(df$Day>= rt[i-1] & df$Day<rt[i])]
    if(length(l)==0) l = sd(y)
    else l = append(l, sd(y))
  }
  l = na.exclude(l)
  if(length(ad)==0) ad=mean(l)
  else ad = append(ad, mean(l))
  
  # normalize
  l=c()
  for(i in 1:length(rt)){
    y = df$normval[which(df$Day>= rt[i-1] & df$Day<rt[i])]
    if(length(l)==0) l = sd(y)
    else l = append(l, sd(y))
  }
  l = na.exclude(l)
  if(length(ald)==0) ald=mean(l)
  else ald = append(ald, mean(l))
  
  # standardize
  l=c()
  for(i in 1:length(rt)){
    y = df$stdval[which(df$Day>= rt[i-1] & df$Day<rt[i])]
    if(length(l)==0) l = sd(y)
    else l = append(l, sd(y))
  }
  l = na.exclude(l)
  if(length(ar)==0) ar=mean(l)
  else ar = append(ar, mean(l))
}

par(mfrow=c(3,2))
plot(nw, ac, type='o', xlab = "Number of windows", ylab = "Standard deviation", main = "Cum. Cases")
plot(nw, alc, type='o', xlab = "Number of windows", ylab = "Standard deviation", main = "Log2(Cases)")
plot(nw, ad, type='o', xlab = "Number of windows", ylab = "Standard deviation", main = "Log10(Cases)")
plot(nw, ald, type='o', xlab = "Number of windows", ylab = "Standard deviation", main = "Normalized Cases")
plot(nw, ar, type='o', xlab = "Number of windows", ylab = "Standard deviation", main = "Standardized Cases")
#plot(nw, alr, type='o')
par(mfrow=c(1,1))

## Coloring
df$color = ""
cl = getColors(length(unique(df$State)), cpal = c("red","orange","blue"))
sc = unique(df$State)
j=1
for(i in sc){
  t = which(df$State==as.character(i))
  df$color[t] = cl[j]
  j=j+1
}

# point size
df_sub = as.data.frame(df[which(df$Day==94 & df$Value>20000),])

# Normalize by max
df$maxvalue = df$Value/max(df$Value)
df$maxvalue[which(df$maxvalue<0.0001)] = 0.0


###### 2d plottings
## Days vs. values
tdf = as.data.frame(df[which(df$Day>=50),])
gg<-ggplot(df, aes(x = Day, y = maxvalue, colour = State)) + 
  geom_line() +
  scale_color_manual(values = unique(df$color)) + 
  #geom_point() + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(3.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  #labs(color='USA States') +
  ggtitle("") +
  labs(x = "Days Since First Case (DSFC)", y = "Cumulative cases")

ggl2<-ggplot(df, aes(x = Day, y = LogTwo, colour = State)) + 
  geom_line() +
  scale_color_manual(values = unique(df$color)) + 
  #geom_point() + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(3.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  #labs(color='USA States') +
  ggtitle("") +
  labs(x = "Days Since First Case (DSFC)", y = "Log2(cases)")
#gg+geom_label_repel(aes(label=State), size=2, data=df_sub) + theme(legend.position = "Right")
ggtl2<-ggl2+geom_label_repel(aes(label=State), size=2, data=df_sub) + theme(legend.position = "none")







