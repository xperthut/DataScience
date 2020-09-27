#Script for Ploting Plant height vs Date, and fit models.

#load required packages
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(magrittr)
library(ggplot2)
library(lme4)
library(graphics)
library(drc)
library(scales)
library(stats)


#load the data and filter for CameraID 296
PlantHeights296 <- read_csv("/Users/methun/self/Research/Github/BCBLAB/Project/2017_05_01/Data/SchnableGrowthRateData.csv") %>%
                  filter(CameraID == "296") %>% rename(DAP = DaysAfterPlanting)

#compute daily average plant height per genotype (mean height across all plant IDs)
c296 <- PlantHeights296 %>% group_by(DAP) %>% summarise(MeanH = mean(HeightCM, na.rm = TRUE))

#compare the fit of various non-linear models
par(mfrow=c(2,3))
result296_1<-drm(MeanH~DAP, data = c296, fct = EXD.3())
plot(result296_1,main="Exponential Model")
result296_2<-drm(MeanH~DAP, data = c296, fct = G.4())
plot(result296_2,main="Gompertz Model")
result296_3<-drm(MeanH~DAP, data = c296, fct = L.5())
plot(result296_3,main="Logistic Model")
result296_4<-drm(MeanH~DAP, data = c296, fct = LL.5())
plot(result296_4,main="Log-logistic")
result296_5<-drm(MeanH~DAP, data = c296, fct = W1.4())
plot(result296_5,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
AIC(result296_1, result296_2, result296_3, result296_4, result296_5)

#obtain the coefficients from the best fitting model
summary(result296_3)

#calculate the first derivative equation
#use the computed coefficients from the summary of the best fitting model ("summary(result296_3)") using the D function 
#the equation of the 5 parameter logistic model is in the form of:
# f(x) = c + (d-c)/((1+exp(b*(x-e)))^f)

#obtain the derivative equation using the D function
NE296 <- D(expression(3.643191 + (125.68746-3.643191)/((1+exp(-0.530322*(x-91.448689)))^0.146262)),'x')
NE296

#copy the output of the "NE296" command and create a function with it named "D296"
D296 <- function(x) { 
  (125.68746 - 3.643191) * (0.146262 * (exp(-0.530322 * (x - 91.448689)) * 0.530322 * (1 + exp(-0.530322 * (x - 91.448689)))^-0.853738))/((1 + exp(-0.530322 * (x - 91.448689)))^0.146262)^2
}

#compute the growth rate (values obtained from the first derivative equation) from DAP = 1 to DAP = 100
x <- seq(1,100, length.out = 100)
D296(x)

#create a data frame with the stored growth rate values
DAP <- seq(1, 100,length.out = 100)
c296slope <- as.data.frame(cbind(DAP, D296(x)))
colnames(c296slope) <- c("DAP", "GrowthRate")
c296slope$Location <- rep("NE", nrow(c296slope))
c296slope$Genotype <- rep("F42 x DJ7", nrow(c296slope))

#compute the Growth Rate for the other genotype/location combinations
#load the data and filter for CameraID 296
PlantHeights242 <- read_csv("~/Desktop/Anuth_Methun_WSU_Files/SchnableGrowthRateData.csv") %>%
  filter(CameraID == "242") %>% rename(DAP = DaysAfterPlanting)
PlantHeights216 <- read_csv("~/Desktop/Anuth_Methun_WSU_Files/SchnableGrowthRateData.csv") %>%
  filter(CameraID == "216") %>% rename(DAP = DaysAfterPlanting)
PlantHeights395 <- read_csv("~/Desktop/Anuth_Methun_WSU_Files/SchnableGrowthRateData.csv") %>%
  filter(CameraID == "395") %>% rename(DAP = DaysAfterPlanting)

c242 <- PlantHeights242 %>% group_by(DAP) %>% summarise(MeanH = mean(HeightCM, na.rm = TRUE))
c216 <- PlantHeights216 %>% group_by(DAP) %>% summarise(MeanH = mean(HeightCM, na.rm = TRUE))
c395 <- PlantHeights395 %>% group_by(DAP) %>% summarise(MeanH = mean(HeightCM, na.rm = TRUE))


result242<-drm(MeanH~DAP, data = c242, fct = L.5())
result216<-drm(MeanH~DAP, data = c216, fct = L.5())
result395<-drm(MeanH~DAP, data = c395, fct = L.5())


summary(result242)
summary(result216)
summary(result395)


NE242 <- D(expression(1.202554 + (119.163528-1.202554)/((1+exp(-0.680006 *(x-68.928018)))^0.122025)),'x')
NE242
D242 <- function(x) { 
  (119.163528 - 1.202554) * (0.122025 * (exp(-0.680006 * (x - 68.928018)) * 0.680006 * (1 + exp(-0.680006 * (x - 68.928018)))^-0.877975))/((1 + exp(-0.680006 * (x - 68.928018)))^0.122025)^2
}

NE216<- D(expression(-3.617951 + (183.168325+3.617951 )/((1+exp(-0.807722*(x-67.465717)))^0.088121)),'x')
NE216
D216 <- function(x) { 
  (183.168325 + 3.617951) * (0.088121 * (exp(-0.807722 * (x - 67.465717)) * 0.807722 * (1 + exp(-0.807722 * (x - 67.465717)))^-0.911879))/((1 + exp(-0.807722 * (x - 67.465717)))^0.088121)^2
  }

NE395 <- D(expression(1.964208 + (159.642608-1.964208)/((1+exp(-0.335191 *(x-84.713497)))^0.243528)),'x')
NE395
D395 <- function(x) { 
  (159.642608 - 1.964208) * (0.243528 * (exp(-0.335191 * (x - 84.713497)) * 0.335191 * (1 + exp(-0.335191 * (x - 84.713497)))^-0.756472))/((1 + exp(-0.335191 * (x - 84.713497)))^0.243528)^2
  }




c242slope <- as.data.frame(cbind(DAP, D242(x)))
colnames(c242slope) <- c("DAP", "GrowthRate")
c242slope$Location <- rep("KS", nrow(c242slope))
c242slope$Genotype <- rep("F42 x DJ7", nrow(c242slope))



c216slope <- as.data.frame(cbind(DAP, D216(x)))
colnames(c216slope) <- c("DAP", "GrowthRate")
c216slope$Location <- rep("KS", nrow(c216slope))
c216slope$Genotype <- rep("SD46 x Mo17", nrow(c216slope))



c395slope <- as.data.frame(cbind(DAP, D395(x)))
colnames(c395slope) <- c("DAP", "GrowthRate")
c395slope$Location <- rep("NE", nrow(c395slope))
c395slope$Genotype <- rep("SD46 x Mo17", nrow(c395slope))


#join the four data frames
GxE_GrowthRateData <- rbind(c216slope, c242slope, c296slope, c395slope)

#Plot the four genotypes
ggplot(GxE_GrowthRateData, aes(x = DAP, y = GrowthRate,
                        colour = Genotype,
                        shape = Location)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=25), 
        axis.text.x = element_text(size=rel(1)), 
        axis.title.y = element_text(size=25),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(3.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  ggtitle("Growth Rate VS DAP") +
  labs(x = "Days After Planting", y = "Growth Rate (cm./day)")



write_csv(GxE_GrowthRateData, "~/Desktop/SchnableLabGrowthRateGxE.csv")
