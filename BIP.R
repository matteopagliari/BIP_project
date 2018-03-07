library(dplyr)
library(plotly)
library(ineq) #compute gini index
library(base)
library(stringr)
library(ggthemes) # visualization
library(lubridate)
library(forecast)

data=read.csv("./Desktop/Data and description for the course project/dataset_polimi.csv")
fdata <- data

fdata$Weekday <- wday(fdata$Data) #1=sunday
fdata$Month   <- month(fdata$Data)
fdata$Year    <- year(fdata$Data)

#------------DATA EXPLORATION---------------

#ogni zona,area,sottoarea ha lo stesso numero di entry
gerarchia <- fdata[1:250560, ] %>%
  group_by(Zona, Area, Sottoarea ) %>%
  summarise(num = n())

#prodotti presenti in modo uguale
fdata %>% count(Categoria_prodotto)-> products
fdata %>% count(Vendite)-> vendite

fdata %>% count(Data)-> days
fdata %>% count(Area)-> areas
fdata %>% count(Sottoarea)-> subareas


sot5 <- fdata[1:250560, ] %>%
  filter(Sottoarea=="Sottoarea_5")

p1 <- sot5[1:1740, ] %>%
  filter(Year==2015) %>%
  filter(Categoria_prodotto=="Prodotto_1")

plot(Data, Vendite, type="l", main="Scatterplot Vendite",xlab="Data", ylab="Vendite",xlim=c(366,730))
lines(Data, Vendite, type="l", main="Scatterplot Vendite",xlab="Data", ylab="Vendite")


venditearea <- fdata[1:250560, ] %>%
  group_by(Area, Prodotto) %>%
  summarise(num = n())

gruppo <-data.frame(Area=integer(148), Prodotto=integer(148), Anno1=integer(148), Anno2=integer(148), Anno3=integer(148))

fdata$Z <- gsub("Zona_", "", fdata$Zona)
fdata$A <-  gsub("Area_", "", fdata$Area)
fdata$S <-  gsub("Sottoarea_", "", fdata$Sottoarea)
fdata$P <-  gsub("Prodotto_", "", fdata$Categoria_prodotto)
fdata$V <-  fdata$Vendite

#New dataset well defined
sdata <- subset(fdata, select=c("Z", "A", "S", "Weekday", "Month", "Year","Data", "P", "V"))

#Prodotti venduti per ogni area per ogni anno
for(i in 1:74){
  gruppo$Area[i] <- i
  gruppo$Prodotto[i] <- 1
  gruppo$Anno1[i] <- sum(subset(sdata, A==i & Year=="2014" & P==1)$V)
  gruppo$Anno2[i] <- sum(subset(sdata, A==i & Year=="2015" & P==1)$V)
  gruppo$Anno3[i] <- sum(subset(sdata, A==i & Year=="2016" & P==1)$V)
}

for(i in 1:74){
  gruppo$Area[i+74] <- i
  gruppo$Prodotto[i+74] <- 2
  gruppo$Anno1[i+74] <- sum(subset(sdata, A==i & Year=="2014" & P==2)$V)
  gruppo$Anno2[i+74] <- sum(subset(sdata, A==i & Year=="2015" & P==2)$V)
  gruppo$Anno3[i+74] <- sum(subset(sdata, A==i & Year=="2016" & P==2)$V)
}

#Somma prodotti 1 e 2 venduti per ogni anno per area
totgruppo <-data.frame(Area=integer(74), Anno1=integer(74), Anno2=integer(74), Anno3=integer(74))
for(i in 1:74){
  totgruppo$Area[i] <- i
  totgruppo$Anno1[i] <- sum(subset(sdata, A==i & Year=="2014")$V)
  totgruppo$Anno2[i] <- sum(subset(sdata, A==i & Year=="2015")$V)
  totgruppo$Anno3[i] <- sum(subset(sdata, A==i & Year=="2016")$V)
}

tri<-data.frame(Area=integer(148), Prodotto=integer(148), Primo2014=integer(148), Secondo2014=integer(148), Terzo2014=integer(148), Quarto2014=integer(148), Primo2015=integer(148), Secondo2015=integer(148), Terzo2015=integer(148), Quarto2015=integer(148), Primo2016=integer(148), Secondo2016=integer(148))
#Trimestri
for(i in 1:74){
  tri$Area[i] <- i
  tri$Prodotto <- 1
  tri$Primo2014[i] <- sum(subset(sdata, A==i & Year=="2014" & Month<=3 & P==1)$V)
  tri$Secondo2014[i] <- sum(subset(sdata, A==i & Year=="2014" & Month>3 & Month<=6 & P==1)$V)
  tri$Terzo2014[i] <- sum(subset(sdata, A==i & Year=="2014" & Month>6 & Month<=9 & P==1)$V)
  tri$Quarto2014[i] <- sum(subset(sdata, A==i & Year=="2014" & Month>9& Month<=12 & P==1)$V)
  tri$Primo2015[i] <- sum(subset(sdata, A==i & Year=="2015" & Month<=3 & P==1)$V)
  tri$Secondo2015[i] <- sum(subset(sdata, A==i & Year=="2015" & Month>3 & Month<=6 & P==1)$V)
  tri$Terzo2015[i] <- sum(subset(sdata, A==i & Year=="2015" & Month>6 & Month<=9 & P==1)$V)
  tri$Quarto2015[i] <- sum(subset(sdata, A==i & Year=="2015" & Month>9& Month<=12 & P==1)$V)
  tri$Primo2016[i] <- sum(subset(sdata, A==i & Year=="2016" & Month<=3 & P==1)$V)
  tri$Secondo2016[i] <- sum(subset(sdata, A==i & Year=="2016" & Month>3 & Month<=6 & P==1)$V)
}

for(i in 1:74){
  tri$Area[i+74] <- i
  tri$Prodotto[i+74] <- 2
  tri$Primo2014[i+74] <- sum(subset(sdata, A==i & Year=="2014" & Month<=3 & P==2)$V)
  tri$Secondo2014[i+74] <- sum(subset(sdata, A==i & Year=="2014" & Month>3 & Month<=6 & P==2)$V)
  tri$Terzo2014[i+74] <- sum(subset(sdata, A==i & Year=="2014" & Month>6 & Month<=9 & P==2)$V)
  tri$Quarto2014[i+74] <- sum(subset(sdata, A==i & Year=="2014" & Month>9& Month<=12 & P==2)$V)
  tri$Primo2015[i+74] <- sum(subset(sdata, A==i & Year=="2015" & Month<=3 & P==2)$V)
  tri$Secondo2015[i+74] <- sum(subset(sdata, A==i & Year=="2015" & Month>3 & Month<=6 & P==2)$V)
  tri$Terzo2015[i+74] <- sum(subset(sdata, A==i & Year=="2015" & Month>6 & Month<=9 & P==2)$V)
  tri$Quarto2015[i+74] <- sum(subset(sdata, A==i & Year=="2015" & Month>9& Month<=12 & P==2)$V)
  tri$Primo2016[i+74] <- sum(subset(sdata, A==i & Year=="2016" & Month<=3 & P==2)$V)
  tri$Secondo2016[i+74] <- sum(subset(sdata, A==i & Year=="2016" & Month>3 & Month<=6 & P==2)$V)
}


ddata<- data
ddata["Numday"] <-NA
ddata$Numday <- yday(ddata$Data)

sdata["Numday"] <- 0
sdata$Numday <- yday(sdata$Data)

for(i in 1:250560){
  if(sdata$Year[i]=="2015"){
    sdata$Numday[i] <- sdata$Numday[i] + 365
  }
  if(sdata$Year=="2016"){
    sdata$Numday[i] <- sdata$Numday[i] + 730
  }
}



kdata <- sdata
kdata$V <- kdata$V/100

write.csv(ndata, file = "./Desktop/Data and description for the course project/ndata.csv")

zeroven<- sdata[1:250560, ] %>%
  group_by(V, Weekday) %>%
  summarise(num_animals = n())




ndata <- data.frame(Area=integer(148), Prodotto=integer(148), Anno=integer(148), Trimestre=integer(148), Vendite=integer(148))

for(i in 1:1){
  for(j in 9:10){
    if(tri$Area[i]==1 & tri$Prodotto[i]==1){
      ndata$Area[j]<-1
      ndata$Prodotto[j]<-1
      ndata$Anno[j] <- 2016
      ndata$Trimestre[j] <- j-8
    }
  }
}


ndata$Vendite[3] <- 625
ndata$Vendite[4] <- 770 
ndata$Vendite[5] <- 669
ndata$Vendite[6] <- 626
ndata$Vendite[7] <- 678
ndata$Vendite[8] <- 710
ndata$Vendite[9] <- 753
ndata$Vendite[10] <- 378

for(i in 1:10){
  ndata$Trimestre[i] <- i
}


sot1 <- sdata[1:250560, ] %>%
  filter(sdata$S==1) %>%
  filter(sdata$Y==2014) %>%
  filter(sdata$Month==5)

sot1 <- sot1[1:1740, ] %>%
  filter(sot1$Month==5)

fit <- lm(V ~ Data, data=sot1)
summary(fit) # show results
plot(fit)

sequence <- subset(sot1, select=c("Data", "V"))
myts <- ts(sequence, start=c(2016, 1), end=c(2016, 140), frequency=365) 
plot(myts)
fit <-HoltWinters(sequence, gamma=FALSE)
plot(fit)

forecast(fit, 3)
plot(forecast(fit, 3))