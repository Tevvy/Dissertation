library(secr)
library(secrdesign)

traps11<-make.grid(nx=3, ny=3, spacex=100, detector='proximity')
traps12<-make.grid(nx=3, ny=3, spacex=200, detector='proximity')
traps13<-make.grid(nx=3, ny=3, spacex=300, detector='proximity')

traps21<-make.circle(n=9, spacing=100, detector = 'proximity')
traps22<-make.circle(n=9, spacing=200, detector = 'proximity')
traps23<-make.circle(n=9, spacing=300, detector = 'proximity')

traps31<-read.traps(data=data.frame(x=rep(0,9), y=100*c(0:8)), detector = 'proximity')
traps32<-read.traps(data=data.frame(x=rep(0,9), y=200*c(0:8)), detector = 'proximity')
traps33<-read.traps(data=data.frame(x=rep(0,9), y=300*c(0:8)), detector = 'proximity')

traps41<-read.traps(data=data.frame(x=c(rep(0,5), 100*1:4), y=c(100*1:4, rep(0,5))), detector = 'proximity')
traps42<-read.traps(data=data.frame(x=c(rep(0,5), 200*1:4), y=c(200*1:4, rep(0,5))), detector = 'proximity')
traps43<-read.traps(data=data.frame(x=c(rep(0,5), 300*1:4), y=c(300*1:4, rep(0,5))), detector = 'proximity')
trapset<-list(traps11, traps12, traps13, 
              traps21, traps22, traps23, 
              traps31, traps32, traps33, 
              traps41, traps42, traps43)

scenario1<-make.scenarios(trapsindex=c(1:12),noccasions = 1, D=10, g0=0.5, sigma=100, detectfn = 'HN')

results<-run.scenarios(scenarios = scenario1, nrepl = 10, trapset=trapset, ncores=11, byscenario = FALSE, fit=TRUE)
a<-select.stats(results, parameter = 'D', statistics='ERR')
summary(a, fields='rms')$OUTPUT



