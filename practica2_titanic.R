setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#1
data <- read.csv("titanic.csv")
head(data)
summary(data)


#1
hist(data$Age[data$Sex=='male'], col='blue')
hist(data$Age[data$Sex=='female'], col='red', add=T)

#2
data['Name']

#3
data[(grepl("(*Master.*)", data$Name, ignore.case=TRUE)),'title'] <- 'Master'
data[(grepl("(*Mlle.*)|(*Miss.*)", data$Name, ignore.case=TRUE)),'title'] <- 'Miss'
data[(grepl("(*Mr.*)", data$Name, ignore.case=TRUE)),'title'] <- 'Mr'
data[(grepl("(*Mrs.*)|(*Ms.*)|(*Mme.*)", data$Name, ignore.case=TRUE)),'title'] <- 'Mrs'

options(max.print=1000000) 
data[,c('title')]
data[,c('title','Name')]


#4
boxplot(data$Age~data$title)



#5
library("ggplot2")
data[(grepl("(*Don.*)|(*Major.*)|(*Dr.*)|(*Rev.*)|(*Capt.*)|(*Col.*)|(*Jonkheer.*)|(*the Countess.*)", data$Name, ignore.case=TRUE)),'title'] <- 'Otros'
ggplot(data, aes(x=title, y=Survived, colour=title)) + 
  geom_bar(stat = "identity", aes(fill = title)) +
  geom_smooth() +
  xlab("Title") + ylab("Survived") +
  ggtitle("Sobrevivientes por Title") +
  theme_bw()


#6
#99% del grupo  Otros fueron mujeres
data[data['title']=='Otros' & data['Survived']==1 ,'title'] <- "O.V."
data[data['title']=='Otros' & data['Survived']==0 ,'title'] <- "O.M."
ggplot(data, aes(x=title, y=Survived, colour=title)) + geom_bar(stat = "identity", aes(fill = title)) +
  geom_smooth() +
  xlab("Title") + ylab("Survived") +
  ggtitle("Sobrevivientes por Title") +
  theme_bw()
#data[data['title']=='O.V.' | data['title']=='O.M.', c('Sex','Pclass','Survived')]

#7
library("ggplot2")
ggplot(data,aes(x = Age, y = title, colour = title)) + 
  geom_point() + 
  geom_smooth() + 
  ggtitle("Edades según Clase") +
  theme_bw() +
  facet_grid(~Pclass)

#8
data[is.na(data$Age),c('Age')] <- mean(data[!is.na(data$Age),c('Age')])
ggplot(data,aes(x = Age, y = title, colour = title)) + 
  geom_point() + 
  geom_smooth() + 
  ggtitle("Edades según Clase - sin NA") +
  theme_bw() +
  facet_grid(~Pclass)



##=====================================
##=====================================
##=====================================
##============SEGUNDA PARTE============
##=====================================
##=====================================
##=====================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


#1
data <- read.csv("diabetes.data",sep='\t')
head(data)
summary(data)
data <- data[data$BMI!=-9999,]
data <- data[data$BP!=-9999,]
data <- data[data$S2!=-9999,]
data <- data[data$S5!=-9999,]


#2
summary(data)

#3
dev.off(dev.list()["RStudioGD"])
head(data)
boxplot(data)

#4
sapply(data,function(x){tapply(x, data$SEX, mean, na.rm = T)})


#5
cor(data$Y,data[,c('BMI','BP','S1','S2','S3','S4','S5','S6')])


#6
###MAYOR CORRELACIÒN
dev.off(dev.list()["RStudioGD"])
par(mfrow = c(2, 2))
plot(Y ~ BMI , data = data, xlab = "BMI", ylab = "Y", main="MAYOR CORRELACIÓN")
MLatin <- lm(Y ~ BMI, data = data)
abline(MLatin, col = "red")

###MENOR CORRELACIÒN
plot(Y ~ S2 , data = data, xlab = "S2", ylab = "Y", main="MENOR CORRELACIÓN")
MLatin <- lm(Y ~ S2, data = data)
abline(MLatin, col = "red")

###CORRELACIÒN 1
job_title <- c('DEVS','DEVOPS','IT SECURITY')
category <- c(1,2,3)
salary <- c(1000.1, 1000.5, 1000.95)
data2 <- data.frame(job_title,category, salary)
data2
cor(data2$category,data2[,c('salary')])

plot(category ~ salary, data = data2, xlab = "salary", ylab = "category", main="CORRELACIÓN 1")
MLatin <- lm(category ~ salary, data = data2)
abline(MLatin, col = "red")

###USANDO GGPLOT2
#library(ggplot2)
###MAYOR CORRELACION
#ggplot(data, aes(x=BMI, y=Y)) + geom_point() + ggtitle("T") + xlab("x") + ylab("y") + geom_smooth(method=lm)
###MENOR CORRELACION
#ggplot(data, aes(x=S2, y=Y)) + geom_point() + ggtitle("T") + xlab("x") + ylab("y") + geom_smooth(method=lm)


#7
data$SEX2[data$SEX=="M"] <- 1
data$SEX2[data$SEX=="F"] <- 2
data$SEX <- data$SEX2
data["SEX"]
data$SEX2 <- NULL

#8
###FUNCION PARA REPRESENTAR LOS OUTLIERS EN UN GRAFICO
outlier <- function (x){
    med <- median(x)
    MAD <- 3*(mad(x))  #median(abs(med-x))/0.6745
    dtf <<- data.frame(ID=seq.int(length(x)), obs=x, outlier=abs(x-med)>(MAD))
    midp <<- med
    lower <<- med-((MAD))
    upper <<- med+((MAD))
    outliern <<- length(which(dtf=="TRUE"))
    #print(med)
    #print(MAD)
    #print(dtf)
  #if (addthres==TRUE) {
    p <- ggplot(dtf, aes(x=ID, y=obs, label=ID)) + 
      geom_point(aes(colour=outlier)) + 
      geom_text_repel(data = subset(dtf, outlier=="TRUE"), aes(label = ID), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) +
      labs(x=paste("observation ID number\n number of outliers detected=", outliern, "\n( outlier detection method=", "MAD", ")"), y="observation value") +
      geom_hline(yintercept = midp, colour="black", linetype = "longdash") +
      geom_hline(yintercept = lower, colour="black", linetype = "longdash") +
      geom_hline(yintercept = upper, colour="black", linetype = "longdash")
  #} else {
  #  p <- ggplot(dtf, aes(x=ID, y=obs, label=ID)) + geom_point(aes(colour=outlier)) + geom_text_repel(data = subset(dtf, outlier=="TRUE"), aes(label = ID), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x=paste("observation ID number\n( outlier detection method=", method, ")"), y="observation value") #requires 'ggrepel'
  #}
  return(list(dtf,p))
}


require('ggrepel')
dev.off(dev.list()["RStudioGD"])
##SE LISTA UNA DATAFRAME Y EL GRAFICO ANTES QUITAR A LOS OUTLIERS

lista <- outlier(data$AGE)
df <- data.frame(lista[1])
lista[2]
if (length(df[df$outlier==TRUE,'ID']) > 0){ data <- data[-c(df[df$outlier==TRUE,'ID']),] }

lista <- outlier(data$BMI)
df <- data.frame(lista[1])
lista[2]
if (length(df[df$outlier==TRUE,'ID']) > 0){ data <- data[-c(df[df$outlier==TRUE,'ID']),] }

lista <- outlier(data$BP)
df <- data.frame(lista[1])
lista[2]
if (length(df[df$outlier==TRUE,'ID']) > 0){ data <- data[-c(df[df$outlier==TRUE,'ID']),] }

df <- outlier(data$S1)
df <- data.frame(lista[1])
lista[2]
if (length(df[df$outlier==TRUE,'ID']) > 0 ){ data <- data[-c(df[df$outlier==TRUE,'ID']),] }

df <- outlier(data$S2)
df <- data.frame(lista[1])
lista[2]
if (length(df[df$outlier==TRUE,'ID']) > 0 ){ data <- data[-c(df[df$outlier==TRUE,'ID']),] }

lista <- outlier(data$S3)
df <- data.frame(lista[1])
lista[2]
if (length(df[df$outlier==TRUE,'ID']) > 0){ data <- data[-c(df[df$outlier==TRUE,'ID']),] }

lista <- outlier(data$S4)
df <- data.frame(lista[1])
lista[2]
if (length(df[df$outlier==TRUE,'ID']) > 0){ data <- data[-c(df[df$outlier==TRUE,'ID']),] }

lista <- outlier(data$S5)
df <- data.frame(lista[1])
lista[2]
if (length(df[df$outlier==TRUE,'ID']) > 0){ data <- data[-c(df[df$outlier==TRUE,'ID']),] }

lista <- outlier(data$S6, method="median", addthres=T)
df <- data.frame(lista[1])
lista[2]
if (length(df[df$outlier==TRUE,'ID']) > 0){ data <- data[-c(df[df$outlier==TRUE,'ID']),] }

lista <- outlier(data$Y, method="median", addthres=T)
df <- data.frame(lista[1])
lista[2]
if (length(df[df$outlier==TRUE,'ID']) > 0){ data <- data[-c(df[df$outlier==TRUE,'ID']),] }

data[,]



#9
size <- floor(0.70 * nrow(data))
set.seed(123)
index <- sample(seq_len(nrow(data)), size = size)
index
data_70 <- data[index, ]
data_30 <- data[-index, ]
data_70[,]
data_30[,]


#10
data_70["AGE"] <- apply(data_70["AGE"],1,function(x)((x-sd(data_70$AGE))/median(data_70$AGE)))
data_70["BMI"] <- apply(data_70["BMI"],1,function(x)((x-sd(data_70$BMI))/median(data_70$BMI)))
data_70["BP"] <- apply(data_70["BP"],1,function(x)((x-sd(data_70$BP))/median(data_70$BP)))
data_70["S1"] <- apply(data_70["S1"],1,function(x)((x-sd(data_70$S1))/median(data_70$S1)))
data_70["S2"] <- apply(data_70["S2"],1,function(x)((x-sd(data_70$S2))/median(data_70$S2)))
data_70["S3"] <- apply(data_70["S3"],1,function(x)((x-sd(data_70$S3))/median(data_70$S3)))
data_70["S4"] <- apply(data_70["S4"],1,function(x)((x-sd(data_70$S4))/median(data_70$S4)))
data_70["S5"] <- apply(data_70["S5"],1,function(x)((x-sd(data_70$S5))/median(data_70$S5)))
data_70["S6"] <- apply(data_70["S6"],1,function(x)((x-sd(data_70$S6))/median(data_70$S6)))
data_70


data_30["AGE"] <- apply(data_30["AGE"],1,function(x)((x-sd(data_70$AGE))/median(data_70$AGE)))
data_30["BMI"] <- apply(data_30["BMI"],1,function(x)((x-sd(data_70$BMI))/median(data_70$BMI)))
data_30["BP"] <- apply(data_30["BP"],1,function(x)((x-sd(data_70$BP))/median(data_70$BP)))
data_30["S1"] <- apply(data_30["S1"],1,function(x)((x-sd(data_70$S1))/median(data_70$S1)))
data_30["S2"] <- apply(data_30["S2"],1,function(x)((x-sd(data_70$S2))/median(data_70$S2)))
data_30["S3"] <- apply(data_30["S3"],1,function(x)((x-sd(data_70$S3))/median(data_70$S3)))
data_30["S4"] <- apply(data_30["S4"],1,function(x)((x-sd(data_70$S4))/median(data_70$S4)))
data_30["S5"] <- apply(data_30["S5"],1,function(x)((x-sd(data_70$S5))/median(data_70$S5)))
data_30["S6"] <- apply(data_30["S6"],1,function(x)((x-sd(data_70$S6))/median(data_70$S6)))
data_30
