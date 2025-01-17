#Memanggil library yang diperlukan
library(tidyverse) 
library(factoextra)
library(IntroCompFinR)
library(ggplot2)
library(plotly)

#Import data return saham
data = read.delim("clipboard")
dim(data)
head(data)

#Mencari mean dan variansi return saham
mean1 = c()
var1 = c()
for(i in 1:(ncol(data)-1)){
  mean1[i] = mean(data[,i+1])
  var1[i] = var(data[,i+1])
}
df = data.frame(mean1[-44],var1[-44]) 
head(df)
view(df)

#Mengganti nama baris dan nama kolom
namecol = read.delim("clipboard",header=T) #Data nama emiten
rownames(df) = c(namecol$V1) 
colnames(df) = c("ExpectedReturn","ReturnVariance")
head(df)
df1 = filter(df,ExpectedReturn > 0)
view(df1)
dim(df1)

#Menstandarisasi data untuk K-means klastering
data1 = scale(df1)
head(data1,5)

#K-Means Clustering
set.seed(100)
km.res = kmeans(data1,6,nstart=50)
print(km.res)

fviz_cluster(km.res, data = data1,
             palette = c("#2E9FDF", "#FC4E07", "#B62FDF", "#00AFBB", "#E7B800", "#2E3FDF"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             labelsize = 4,
             ggtheme = theme_minimal()
)

#Pemilihan emiten dari masing-masing klaster
cluster = km.res$cluster
klaster = data.frame(df1,cluster)
view(klaster)
head(klaster)
tail(klaster)
str(klaster)

#Pemilihan emiten dengan expected return tertinggi tiap cluster
klaster$cluster = as.factor(klaster$cluster)
kelompok = klaster %>% group_by(cluster) %>% 
  summarise(meanMax = max(ExpectedReturn))
kelompok

#Metode Multi Objective
ReturnData = read.delim("clipboard",header = T) #Data return emiten terpilih
View(ReturnData)
CovarRt = cov(ReturnData)
CovarRt

ReturnMat = matrix(c(mean(ReturnData$MDKA),mean(ReturnData$HRUM),mean(ReturnData$EMTK),mean(ReturnData$UNTR),mean(ReturnData$ERAA),mean(ReturnData$ARTO)),nrow = 1,byrow = T)

Imat = matrix(c(1,1,1,1,1,1),nrow = 6,byrow = T)

multi_objective_portfolio = function(k){
  lambda=((t(Imat)%*%solve(CovarRt)%*%t(ReturnMat)-(2*k))/
            (t(Imat)%*%solve(CovarRt)%*%Imat))
  Weight=(1/(2*k))*solve(CovarRt)%*%(t(ReturnMat) - lambda[1,1]*Imat)
  return(Weight)
}

#Perhitungan Bobot
multi_objective_portfolio(0.01)
multi_objective_portfolio(0.1)
multi_objective_portfolio(1)
multi_objective_portfolio(2)
multi_objective_portfolio(5)
multi_objective_portfolio(10)
multi_objective_portfolio(20)
multi_objective_portfolio(50)
multi_objective_portfolio(100)
multi_objective_portfolio(1000)

#Perhitungan expected return
t(multi_objective_portfolio(0.01))%*%t(ReturnMat)
t(multi_objective_portfolio(0.1))%*%t(ReturnMat)
t(multi_objective_portfolio(1))%*%t(ReturnMat)
t(multi_objective_portfolio(2))%*%t(ReturnMat)
t(multi_objective_portfolio(5))%*%t(ReturnMat)
t(multi_objective_portfolio(10))%*%t(ReturnMat)
t(multi_objective_portfolio(20))%*%t(ReturnMat)
t(multi_objective_portfolio(50))%*%t(ReturnMat)
t(multi_objective_portfolio(100))%*%t(ReturnMat)
t(multi_objective_portfolio(1000))%*%t(ReturnMat)

#Perhitungan Risk
t(multi_objective_portfolio(0.01))%*%CovarRt%*%multi_objective_portfolio(0.01)
t(multi_objective_portfolio(0.1))%*%CovarRt%*%multi_objective_portfolio(0.1)
t(multi_objective_portfolio(1))%*%CovarRt%*%multi_objective_portfolio(1)
t(multi_objective_portfolio(2))%*%CovarRt%*%multi_objective_portfolio(2)
t(multi_objective_portfolio(5))%*%CovarRt%*%multi_objective_portfolio(5)
t(multi_objective_portfolio(10))%*%CovarRt%*%multi_objective_portfolio(10)
t(multi_objective_portfolio(20))%*%CovarRt%*%multi_objective_portfolio(20)
t(multi_objective_portfolio(50))%*%CovarRt%*%multi_objective_portfolio(50)
t(multi_objective_portfolio(100))%*%CovarRt%*%multi_objective_portfolio(100)
t(multi_objective_portfolio(1000))%*%CovarRt%*%multi_objective_portfolio(1000)

#Perhitungan sharpe ratio masing-masing portofolio
#dengan asumsi riskfree rate (Rf) = 0%
k=c(10,20,50,100,1000)
meanP=c()
varP=c()
sharpeP=c()
for (i in 1:length(k)) {
  meanP[i] = t(multi_objective_portfolio(k[i]))%*%t(ReturnMat)
  varP[i] = t(multi_objective_portfolio(k[i]))%*%CovarRt%*%multi_objective_portfolio(k[i])
  sharpeP[i] = meanP[i]/(sqrt(varP[i]))
}
shPorto = data.frame(k,sharpeP)
shPorto %>% summarise(max(sharpeP))

#Plot return vs risk
RtRisk = data.frame(meanP,varP)
ggplot(RtRisk, aes(varP,meanP)) + #Draw default ggplot2 plot
  geom_line(col="deepskyblue3") + 
  geom_text(aes(label=k),size=2,vjust=-1)

#Sharpe ratio awal dan akhir pandemi
#dengan asumsi riskfree rate (Rf) = 0%
df.aw = read.delim("clipboard") #Data return portofolio di awal pandemi
head(df.aw)
df.ak = read.delim("clipboard") #Data return portofolio di akhir pandemi
head(df.ak)

mean.aw = c()
var.aw = c()
std.aw = c()
shp.aw = c()
for(i in 1:(ncol(df.aw))){
  mean.aw[i] = mean(df.aw[,i])
  var.aw[i] = var(df.aw[,i])
  std.aw[i] = sqrt(var.aw[i])
  shp.aw[i] = mean.aw[i]/std.aw[i]
}

mean.ak = c()
var.ak = c()
std.ak = c()
shp.ak = c()
for(i in 1:(ncol(df.ak))){
  mean.ak[i] = mean(df.ak[,i])
  var.ak[i] = var(df.ak[,i])
  std.ak[i] = sqrt(var.ak[i])
  shp.ak[i] = mean.ak[i]/std.ak[i]
}
Asset = c("MDKA", "HRUM", "EMTK", "UNTR", "ERAA", "ARTO")
SharpeEmiten = data.frame(Asset,shp.aw,shp.ak)
SharpeEmiten
shapeRatioPdm = data.frame(SharpeAwalP=mean(shp.aw),SharpeAkhirP=mean(shp.ak)) #dataframe dari mean dan return portofolio
shapeRatioPdm

#Diagram batang bobot
wts = data.frame(Asset,Bobot=multi_objective_portfolio(10))
p <- wts %>%
  ggplot(aes(x = Asset, y = Bobot, fill = Asset,label = scales::percent(Bobot))) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  geom_col(position = 'dodge') +
  labs(x = 'Assets', y = 'Bobot saham', title = "Bobot Saham Pada k=10")+
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 5)+
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)

#Minimum Variance Method
er = read.delim("clipboard",header=T) #Data expected return 6 emiten terpilih
er
r.free = 0 #dengan asumsi risk free rate 0%
gmin.port = globalMin.portfolio(er[,1], CovarRt, shorts=FALSE)
print(gmin.port)
summary(gmin.port, risk.free=r.free)
