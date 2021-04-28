library(secr)
library(secrdesign)
library(tidyverse)
library(e1071)

traps11<-make.grid(nx=3, ny=3, spacing=100, detector='proximity')
traps12<-make.grid(nx=3, ny=3, spacing=150, detector='proximity')
traps13<-make.grid(nx=3, ny=3, spacing=200, detector='proximity')
traps14<-make.grid(nx=3, ny=3, spacing=250, detector='proximity')

traps21<-make.circle(n=9, spacing=100, detector = 'proximity')
traps22<-make.circle(n=9, spacing=150, detector = 'proximity')
traps23<-make.circle(n=9, spacing=200, detector = 'proximity')
traps24<-make.circle(n=9, spacing=250, detector = 'proximity')

traps31<-read.traps(data=data.frame(x=rep(0,9), y=100*c(0:8)), detector = 'proximity')
traps32<-read.traps(data=data.frame(x=rep(0,9), y=150*c(0:8)), detector = 'proximity')
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

results<-run.scenarios(scenarios = scenario1, nrepl = 1000, trapset=trapset, ncores=11, byscenario = FALSE, fit=TRUE, seed=123)
save(results, file='resultsFinal.rda')

a<-select.stats(results, parameter = 'D', statistics=c('ERR'))
b<-t(as.data.frame(summary(a, fields=c('rms'))$OUTPUT))
colnames(b)<-'RMSE'
c<-select.stats(results, parameter = 'D', statistics = c('RB','RSE','COV'))
d<-t(as.data.frame(summary(c, fields='mean')$OUTPUT))
colnames(d)<-c('mRB', 'mRSE', 'COV')
e<-cbind(d,b)
rownames(e)<-NULL
Spacing<-c(1,1.5,2,2.5)
Layout<-c(rep('Grid', 4), rep('Circle', 4), rep('Line', 4), rep('Axis', 4))
Layout<-as.data.frame(Layout)
f<-cbind(Layout,Spacing, e)

ggplot(data=f)+
  geom_point(aes(y=RMSE, x=Spacing, col=Layout, shape=Layout), size=3)+
  geom_line(aes(y=RMSE, x=Spacing, col=Layout))

ggplot(data=f)+
  geom_point(aes(y=COV, x=Spacing, col=Layout, shape=Layout), size=3)+
  geom_line(aes(y=COV, x=Spacing, col=Layout))

ggplot(data=f)+
  geom_point(aes(y=mRB, x=Spacing, col=Layout, shape=Layout), size=3)+
  geom_line(aes(y=mRB, x=Spacing, col=Layout))

ggplot(data=f)+
  geom_point(aes(y=mRSE, x=Spacing, col=Layout, shape=Layout), size=3)+
  geom_line(aes(y=mRSE, x=Spacing, col=Layout))

ggplot(size)+
  geom_point(data=traps11, aes(x=x, y=y), shape='plus')+
  geom_point(data=traps21, aes(x=x+450, y=y+150), col='red', shape='plus')+
  geom_point(data=traps31, aes(x=x+700, y=y), shape='plus')+
  geom_point(data=traps41, aes(x=x+800, y=y), col='red', shape='plus')+
  coord_fixed(ratio=1)+
  xlab('')+
  ylab('')+
  theme(axis.text.x = element_blank(), axis.text.y= element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y= element_blank())

ggplot()+
  geom_point(data=traps11, aes(x=x, y=y), shape='plus')+
  geom_point(data=traps12, aes(x=x+250, y=y), col='red', shape='plus')+
  geom_point(data=traps13, aes(x=x+600, y=y), shape='plus')+
  geom_point(data=traps14, aes(x=x+1100, y=y), col='red', shape='plus')+
  coord_fixed(ratio=1)+
  xlab('')+
  ylab('')+
  theme(axis.text.x = element_blank(), axis.text.y= element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y= element_blank())

ggplot()+
  geom_point(data=traps21, aes(x=x, y=y), shape='plus')+
  geom_point(data=traps22, aes(x=x+450, y=y), col='red', shape='plus')+
  geom_point(data=traps23, aes(x=x+1050, y=y), shape='plus')+
  geom_point(data=traps24, aes(x=x+1800, y=y), col='red', shape='plus')+
  coord_fixed(ratio=1)+
  xlab('')+
  ylab('')+
  theme(axis.text.x = element_blank(), axis.text.y= element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y= element_blank())

ggplot()+
  geom_point(data=traps31, aes(x=x, y=y), shape='plus')+
  geom_point(data=traps32, aes(x=x+500, y=y), col='red', shape='plus')+
  geom_point(data=traps33, aes(x=x+1000, y=y), shape='plus')+
  geom_point(data=traps34, aes(x=x+1500, y=y), col='red', shape='plus')+
  coord_fixed(ratio=1)+
  xlab('')+
  ylab('')+
  theme(axis.text.x = element_blank(), axis.text.y= element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y= element_blank())

ggplot()+
  geom_point(data=traps41, aes(x=x, y=y), shape='plus')+
  geom_point(data=traps42, aes(x=x+500, y=y), col='red', shape='plus')+
  geom_point(data=traps43, aes(x=x+1200, y=y), shape='plus')+
  geom_point(data=traps44, aes(x=x+2000, y=y), col='red', shape='plus')+
  coord_fixed(ratio=1)+
  xlab('')+
  ylab('')+
  theme(axis.text.x = element_blank(), axis.text.y= element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y= element_blank())

stats1<-select.stats(results, parameter = 'D', statistics = c('estimate', 'lcl','ucl'))
par(mfrow=c(4,4))
plot(stats1, type='CI')
stats2<-select.stats(results, parameter = 'D', statistics = c('estimate'))
plot(stats2, type='hist', statistic='estimate')
DensityEstimates<-stats2$output

Skewnessdf<-data.frame(Layout=c(rep('Grid', 4), rep('Circle', 4), rep('Line', 4), rep('Axis', 4)), 
                       Spacing=c(1, 1.5, 2, 2.5),
                       Skewness=rep(NA, 16))

Skewnessdf$Skewness[1]<-skewness(as.vector(DensityEstimates$'1'))
Skewnessdf$Skewness[2]<-skewness(as.vector(DensityEstimates$'2'))
Skewnessdf$Skewness[3]<-skewness(as.vector(DensityEstimates$'3'))
Skewnessdf$Skewness[4]<-skewness(as.vector(DensityEstimates$'4'))
Skewnessdf$Skewness[5]<-skewness(as.vector(DensityEstimates$'5'))
Skewnessdf$Skewness[6]<-skewness(as.vector(DensityEstimates$'6'))
Skewnessdf$Skewness[7]<-skewness(as.vector(DensityEstimates$'7'))
Skewnessdf$Skewness[8]<-skewness(as.vector(DensityEstimates$'8'))
Skewnessdf$Skewness[9]<-skewness(as.vector(DensityEstimates$'9'))
Skewnessdf$Skewness[10]<-skewness(as.vector(DensityEstimates$'10'))
Skewnessdf$Skewness[11]<-skewness(as.vector(DensityEstimates$'11'))
Skewnessdf$Skewness[12]<-skewness(as.vector(DensityEstimates$'12'))
Skewnessdf$Skewness[13]<-skewness(as.vector(DensityEstimates$'13'))
Skewnessdf$Skewness[14]<-skewness(as.vector(DensityEstimates$'14'))
Skewnessdf$Skewness[15]<-skewness(as.vector(DensityEstimates$'15'))
Skewnessdf$Skewness[16]<-skewness(as.vector(DensityEstimates$'16'))

write.csv(Skewnessdf, file='Skewnessdf.csv')
