#SEMINAR22------------------------------------------------------------------

#Putnici-----------------------------------------------------------------------
BDPptnc=c(6480,6630,9120,11500,12530,12700,14050,15510,15890,17760,18460,18670,20720,25200,27230,32910,33320,35980,37150,38110,44180,49270,60130,62800,69490,85030)
Ptnc=c(3.944,3.902122,3.471637,13.315441,8.400090,5.025856,2.127451,3.905317,22.229540,6.737367,12.554741,54.438896,6.087695,28.877297,36.672327,58.840605,46.040461,74.608878,10.851380,64.566742,17.025029,78.466008,20.209931,156.743527,19.234928,39.834395)

#Ptnc_izbaceniOutlieri=c(3.944,3.902122,3.471637,13.315441,8.400090,5.025856,2.127451,3.905317,22.229540,6.737367,12.554741,54.438896,6.087695,28.877297,36.672327,58.840605,46.040461,74.608878,10.851380,64.566742,17.025029,78.466008,20.209931,19.234928)
#BDPptnc_izbaceniOutlieri=c(6480,6630,9120,11500,12530,12700,14050,15510,15890,17760,18460,18670,20720,25200,27230,32910,33320,35980,37150,38110,44180,49270,60130,69490)

hist(Ptnc, prob=T,col="steelblue",xlab="Relativni putnici",ylim=c(0,0.03),ylab="Relativna frekvencija",main="Usporedba funkcije gustoæe exp(1/x_) i histograma za putnike")
curve(dexp(x,1/mean(Ptnc)), add=T, col="red", lwd=3.0)
ks.test(Ptnc,"pexp",1/mean(Ptnc))


plot(BDPptnc,Ptnc, ylab='Relativan broj putnika', xlab='BDP (per capita)', main="Scatter plot: BDP/Putnici", col="blue", lwd=3.0)
model=lm(Ptnc~BDPptnc)
abline(model, col="Red", lwd=5.0)
summary(model) 


m1 <-  summary(model)

mtext(paste0("R squared: ",round(m1$r.squared,2)),adj = 0)

mtext(paste0("P-value: ", format.pval(pf(m1$fstatistic[1], # F-statistic
                                         m1$fstatistic[2], # df
                                         m1$fstatistic[3], # df
                                         lower.tail = FALSE))))

mtext(paste0("y =  ",round(m1$coefficients[1],3)," + ", 
             round(m1$coefficients[2],6),"x"),
      adj = 1)

qqnorm(model$residuals)
qqline(model$residuals, col="steelblue", lwd=5.0)
library(nortest)
lillie.test(model$residuals)




#Transport robe----------------------------------------------------------------
BDPrba=c(6480,6630,9120,11500,12530,12700,13020,13270,14050,15510,15890,18460,18670,20720,25200,27230,32910,33320,35980,37150,38110,41980,44180,49270,60130,62800,69490,85030)
Rba=c(4.524,2.734723,2.928837,2.6907841,18.71447,3.662610,6.263740,4.607315,23.533248,9.953824,12.789473,6.4083538,2.904776,10.056014,1.1946516,3.8487755,2.2760266,3.3459593,9.2465527,4.4976613,21.032396,13.962029,4.3893964,3.23281428,0.1396851,16.931689,8.2686602,7.014331)

#BDPrba_izbaceniOutlieri=c(6480,6630,9120,11500,12530,12700,13020,13270,15510,15890,18460,18670,20720,25200,27230,32910,33320,35980,37150,41980,44180,49270,60130,62800,69490)
#Rba_izbaceniOutlieri=c(4.524,2.734723,2.928837,2.6907841,18.71447,3.662610,6.263740,4.607315,9.953824,12.789473,6.4083538,2.904776,10.056014,1.1946516,3.8487755,2.2760266,3.3459593,9.2465527,4.4976613,13.962029,4.3893964,3.23281428,0.1396851,16.931689,8.2686602)

hist(Rba, prob=T,col="steelblue",xlab="Relativni transport robe",ylim=c(0,0.13),ylab="Relativna frekvencija",main="Usporedba funkcije gustoæe exp(1/x_) i histograma za robu")
curve(dexp(x,1/mean(Rba)), add=T, col="red", lwd=3.0)
ks.test(Rba,"pexp",1/mean(Rba))

plot(BDPrba,Rba, ylab='Relativan broj putnika', xlab='BDP (per capita)', main="Scatter plot: BDP/Putnici", col="blue", lwd=3.0)
model=lm(Rba~BDPrba)
abline(model, col="Red", lwd=5.0)
summary(model)
m1 <-  summary(model)

mtext(paste0("R squared: ",round(m1$r.squared,2)),adj = 0)

mtext(paste0("P-value: ", format.pval(pf(m1$fstatistic[1], # F-statistic
                                         m1$fstatistic[2], # df
                                         m1$fstatistic[3], # df
                                         lower.tail = FALSE))))

mtext(paste0("y =  ",round(m1$coefficients[1],3)," + ", 
             round(m1$coefficients[2],6),"x"),
      adj = 1)
qqnorm(model$residuals)
qqline(model$residuals, col="steelblue", lwd=5.0)
library(nortest)
lillie.test(model$residuals)
model$residuals

