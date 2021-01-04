library(secr)
library(secrdesign)
traps1<-read.traps(data=trap_locations, detector = 'proximity')
traps2<-read.traps(data=Traps2, detector = 'proximity')
scenario1<-make.scenarios(trapsindex=c(1,2),noccasions = 1, D=30, g0=0.6, sigma=100, detectfn = 'HN')
results<-run.scenarios(scenarios = scenario1, nrepl = 10, trapset=list(traps, traps2), ncores=10, byscenario = FALSE, fit=TRUE)
a<-select.stats(results, parameter = 'D', statistics='ERR')
summary(a, fields='rms')$OUTPUT
