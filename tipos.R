library(MASS)
library(tidyverse)
library(readxl)
library(DescTools)
#library(xlsx)
library(gridExtra)
#library(alr3)
#library(fpp2)
library(lubridate)
library(scales)
library(ggthemes)
library(reshape2)
#library(zoo)
library(matrixStats)
library(nortest)
#library(googlesheets)



#install.packages(googlesheets)

Solar <- read_delim("Dados.csv", delim = ",")
rad<- Solar$radGlobalCPM11.radGlobal
rad_hora<- c()
for (i in 1:8784) {
  rad_hora[i]<-mean(rad[(6*i-5):(6*i)])
  
}

matriz <-matrix( rad_hora, nrow = 366, ncol = 24 , byrow = T)


media<-c()
skewness<- c()
curtose<- c()

for (i in 1:24) {
  media[i]<-mean(matriz[,i])
  skewness[i]<- Skew(matriz[,i])
  curtose[i]<- Kurt(matriz[,i])
  
}

horas <- (c(0:23)
)
tabela <- tibble(Hour = horas, Mean = round(media,digits = 2),
                 Skewness = round(skewness, digits = 2),
                 Kurtosis = round(curtose, digits = 2)
)


tabela$Skewness



a<-subset(tabela,
          tabela$Skewness<0.4,
          select= c(Hour,Skewness, Kurtosis))

b<-subset(a,
          a$Skewness>-0.4,
          select= c(Hour,Skewness, Kurtosis))


typeI<- subset(b,
               b$Kurtosis<0.8,
               select= c(Hour,Skewness, Kurtosis))
typeVI<-subset(a,
               a$Kurtosis< -0.8,
               select= c(Hour,Skewness, Kurtosis))

remove(a,b)

a<-subset(tabela,
          tabela$Skewness>0.4,
          select= c(Hour,Skewness, Kurtosis))
typeII<- subset(a,
                a$Kurtosis<0.8,
                select= c(Hour,Skewness, Kurtosis))

b <-subset(a,
           a$Kurtosis>0.8,
           select= c(Hour,Skewness, Kurtosis))
c<-subset(a,
          a$Kurtosis< -0.8,
          select= c(Hour,Skewness, Kurtosis))

typeIII <- rbind(b,c)

remove(a,b,c)

a<-subset(tabela,
          tabela$Skewness< -0.4,
          select= c(Hour,Skewness, Kurtosis))

b <-subset(a,
           a$Kurtosis<0.8,
           select= c(Hour,Skewness, Kurtosis))
typeIV<-subset(b,
               b$Kurtosis> -0.8,
               select= c(Hour,Skewness, Kurtosis))

typeV <-subset(a,
               a$Kurtosis>0.8,
               select= c(Hour,Skewness, Kurtosis))
remove(a,b)
#subset.data.frame(Solar,
#                    Solar$X1<52272,
#                   select = c(X1,tempoData.Data, tempoHora.Hora, radGlobalCPM11.radGlobal)) #tirando os ultimos 3 dias para teste



dados<- as_tibble(matriz)
dados<- mutate(dados, dia = c(1:366))




aaa<- c(0:23)
horass<-c(formatC(aaa, width = 2, flag = "0"), "dia")
colnames(dados)<- horass


random
#typeI
typeI$Hour

normal<-rnorm(365, mean=mean(dados$`07`)) 
hist(normal)

sete<-ggplot(dados) +
  geom_histogram(mapping = aes(x = dados$`07`, y = ..density..), bins = 15, fill= "gray", colour = "black") 
  +theme_bw() 
  
oito<-ggplot(dados) +
  geom_histogram(mapping = aes(x = dados$`08`, y = ..density..), bins = 15, fill= "gray", colour = "black") +theme_bw()
dezesseis <-ggplot(dados) +
  geom_histogram(mapping = aes(x = dados$`16`, y = ..density..), bins = 15, fill= "gray", colour = "black") +theme_bw()



g_typeI<-grid.arrange(sete, oito, dezesseis, ncol=3)

g_typeII<- ggplot(dados)+
  geom_histogram(mapping = aes(x = dados$`06`, y = ..density..), bins = 15, fill= "gray", colour = "black") +theme_bw() 

#typeIII
#não vou plotar pois corresponde a horários com baixa ou nenhuma irradiação, correspondente ao intervalo de 17h até 5h.

#typeiv
typeIV$Hour

nove<-ggplot(dados) +
  geom_histogram(mapping = aes(x = dados$`09`, y = ..density..), bins = 15, fill= "gray", colour = "black") +theme_bw() 

dez<-ggplot(dados) +
  geom_histogram(mapping = aes(x = dados$`10`, y = ..density..), bins = 15, fill= "gray", colour = "black") +theme_bw() 

onze<-ggplot(dados) +
  geom_histogram(mapping = aes(x = dados$`11`, y = ..density..), bins = 15, fill= "gray", colour = "black") +theme_bw() 

doze<-ggplot(dados) +
  geom_histogram(mapping = aes(x = dados$`12`, y = ..density..), bins = 15, fill= "gray", colour = "black") +theme_bw() 

treze<-ggplot(dados) +
  geom_histogram(mapping = aes(x = dados$`13`, y = ..density..), bins = 15, fill= "gray", colour = "black") +theme_bw() 

catorze<-ggplot(dados) +
  geom_histogram(mapping = aes(x = dados$`14`, y = ..density..), bins = 15, fill= "gray", colour = "black") +theme_bw() 

quinze<-ggplot(dados) +
  geom_histogram(mapping = aes(x = dados$`15`, y = ..density..), bins = 15, fill= "gray", colour = "black") +theme_bw() 

grid.arrange(nove, dez, onze, doze, treze, catorze, quinze)
