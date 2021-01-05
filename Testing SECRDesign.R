library(secr)
library(secrdesign)
library(tidyverse)

traps11<-make.grid(nx=3, ny=3, spacing=100, detector='proximity')
traps12<-make.grid(nx=3, ny=3, spacing=150, detector='proximity')
traps13<-make.grid(nx=3, ny=3, spacing=200, detector='proximity')
traps14<-make.grid(nx=3, ny=3, spacing=250, detector='proximity')

traps21<-make.circle(n=9, spacing=100, detector = 'proximity')
traps22<-make.circle(n=9, spacing=150, detector = 'proximity')
traps23<-make.circle(n=9, spacing=200, detector = 'proximity')
traps24<-make.circle(n=9, spacing=250, detector = 'proximity')

traps31<-read.traps(data=data.frame(x=rep(0,9), y=100*c(0:8)), detector = 'proximity')
traps31<-read.traps(data=data.frame(x=rep(0,9), y=150*c(0:8)), detector = 'proximity')
traps33<-read.traps(data=data.frame(x=rep(0,9), y=200*c(0:8)), detector = 'proximity')
traps34<-read.traps(data=data.frame(x=rep(0,9), y=250*c(0:8)), detector = 'proximity')

traps41<-read.traps(data=data.frame(x=c(rep(0,5), 100*1:4), y=c(100*1:4, rep(0,5))), detector = 'proximity')
traps42<-read.traps(data=data.frame(x=c(rep(0,5), 150*1:4), y=c(150*1:4, rep(0,5))), detector = 'proximity')
traps43<-read.traps(data=data.frame(x=c(rep(0,5), 200*1:4), y=c(200*1:4, rep(0,5))), detector = 'proximity')
traps44<-read.traps(data=data.frame(x=c(rep(0,5), 250*1:4), y=c(250*1:4, rep(0,5))), detector = 'proximity')
trapset<-list(traps11, traps12, traps13, traps14,
              traps21, traps22, traps23, traps24,
              traps31, traps32, traps33, traps34,
              traps41, traps42, traps43, traps44)

scenario1<-make.scenarios(trapsindex=c(1:16),noccasions = 1, D=20, g0=0.5, sigma=100, detectfn = 'HN')

results<-run.scenarios(scenarios = scenario1, nrepl = 100, trapset=trapset, ncores=11, byscenario = FALSE, fit=TRUE)
a<-select.stats(results, parameter = 'D', statistics=c('ERR'))
b<-t(as.data.frame(summary(a, fields=c('rms'))$OUTPUT))
colnames(b)<-'RMSE'
rownames(b)<-c('11','12','13', '14','21','22','23', '24','31','32','33', '34','41','42','43', '44')
b

ggplot()+
  geom_point(aes(y=b[1:4], x=c(1, 1.5, 2, 2.5)))+
  geom_point(aes(y=b[5:8], x=c(1, 1.5, 2, 2.5)), col='red')+
  geom_point(aes(y=b[9:12], x=c(1, 1.5, 2, 2.5)), col='blue')+
  geom_point(aes(y=b[13:16], x=c(1, 1.5, 2, 2.5)), col='green')+
  ylim(1,6)

