#Setup####
#load packages
#secr and secrdesign for layout simulation and estimation
library(secr)
library(secrdesign)
#tidyverse for nice data manipulation
library(tidyverse)
#e1071 for skewness calculation
library(e1071)

#Trap Layouts####
#Define Sigma
sigma<-100

#Create layouts with spacing dependent on sigma
#Grid
traps11<-make.grid(nx=3, ny=3, spacing=1*sigma, detector='proximity')
traps12<-make.grid(nx=3, ny=3, spacing=1.5*sigma, detector='proximity')
traps13<-make.grid(nx=3, ny=3, spacing=2*sigma, detector='proximity')
traps14<-make.grid(nx=3, ny=3, spacing=2.5*sigma, detector='proximity')

#Circle
traps21<-make.circle(n=9, spacing=1*sigma, detector = 'proximity')
traps22<-make.circle(n=9, spacing=1.5*sigma, detector = 'proximity')
traps23<-make.circle(n=9, spacing=2*sigma, detector = 'proximity')
traps24<-make.circle(n=9, spacing=2.5*sigma, detector = 'proximity')

#Line
traps31<-read.traps(data=data.frame(x=rep(0,9), y=1*sigma*c(0:8)), detector = 'proximity')
traps32<-read.traps(data=data.frame(x=rep(0,9), y=1.5*sigma*c(0:8)), detector = 'proximity')
traps33<-read.traps(data=data.frame(x=rep(0,9), y=2*sigma*c(0:8)), detector = 'proximity')
traps34<-read.traps(data=data.frame(x=rep(0,9), y=2.5*sigma*c(0:8)), detector = 'proximity')

#Axis
traps41<-read.traps(data=data.frame(x=c(rep(0,5), 1*sigma*1:4), y=c(1*sigma*1:4, rep(0,5))), detector = 'proximity')
traps42<-read.traps(data=data.frame(x=c(rep(0,5), 1.5*sigma*1:4), y=c(1.5*sigma*1:4, rep(0,5))), detector = 'proximity')
traps43<-read.traps(data=data.frame(x=c(rep(0,5), 2*sigma*1:4), y=c(2*sigma*1:4, rep(0,5))), detector = 'proximity')
traps44<-read.traps(data=data.frame(x=c(rep(0,5), 2.5*sigma*1:4), y=c(2.5*sigma*1:4, rep(0,5))), detector = 'proximity')

#Make trapset
trapset<-list(traps11, traps12, traps13, traps14,
              traps21, traps22, traps23, traps24,
              traps31, traps32, traps33, traps34,
              traps41, traps42, traps43, traps44)

#Scenario Simulation####
#Create Scenario with relevant parameters
scenario1<-make.scenarios(trapsindex=c(1:16), noccasions = 1, D=20, g0=0.5, sigma=sigma, detectfn = 'HN')

#Run Scenario with 1000 replicates and parallel processing
#(Change cores for your relevant machine, this originally ran on a 12 core processor)
results<-run.scenarios(scenarios = scenario1, nrepl = 1000, trapset=trapset, ncores=11, byscenario = FALSE, fit=TRUE, seed=123)

#Summary Statistics####
#Start by generating the ERR statistic
ERR<-select.stats(results, parameter = 'D', statistics=c('ERR'))
#calculate RMSE and make into table, changing column name
RMSEtable<-t(as.data.frame(summary(ERR, fields=c('rms'))$OUTPUT))
colnames(RMSEtable)<-'RMSE'
#repeat process for RB, RSE and COV
OtherStats<-select.stats(results, parameter = 'D', statistics = c('RB','RSE','COV'))
OtherStatsTable<-t(as.data.frame(summary(OtherStats, fields='mean')$OUTPUT))
colnames(OtherStatsTable)<-c('mRB', 'mRSE', 'COV')
#Bind all stats together and remove row names
AllStatsTable<-cbind(OtherStatsTable,RMSEtable)
rownames(AllStatsTable)<-NULL
#Create Spacing and Layout lists
Spacing<-c(1,1.5,2,2.5)
Layout<-c(rep('Grid', 4), rep('Circle', 4), rep('Line', 4), rep('Axis', 4))
Layout<-as.data.frame(Layout)
#Bind them all together into final table
SummaryStats<-cbind(Layout,Spacing, AllStatsTable)


#Plots####
#Create plots for each summary stat
#Set data
ggplot(data=SummaryStats)+
  #Set y, x and col parameters, add lines for ease of reading
  geom_point(aes(y=RMSE, x=Spacing, col=Layout, shape=Layout), size=3)+
  geom_line(aes(y=RMSE, x=Spacing, col=Layout))

#repeat for all stats
ggplot(data=SummaryStats)+
  geom_point(aes(y=COV, x=Spacing, col=Layout, shape=Layout), size=3)+
  geom_line(aes(y=COV, x=Spacing, col=Layout))

ggplot(data=SummaryStats)+
  geom_point(aes(y=mRB, x=Spacing, col=Layout, shape=Layout), size=3)+
  geom_line(aes(y=mRB, x=Spacing, col=Layout))

ggplot(data=SummaryStats)+
  geom_point(aes(y=mRSE, x=Spacing, col=Layout, shape=Layout), size=3)+
  geom_line(aes(y=mRSE, x=Spacing, col=Layout))

#Generate plots of the traps
#These were made specifically for this specific set of traps
#You will need to adjust x and y parameters for different trapsets
#Alternate colour of trapsets for ease
#plot to show all layouts
ggplot()+
  geom_point(data=traps11, aes(x=x, y=y), shape='plus')+
  geom_point(data=traps21, aes(x=x+450, y=y+150), col='red', shape='plus')+
  geom_point(data=traps31, aes(x=x+700, y=y), shape='plus')+
  geom_point(data=traps41, aes(x=x+800, y=y), col='red', shape='plus')+
  coord_fixed(ratio=1)+
  xlab('')+
  ylab('')+
  theme(axis.text.x = element_blank(), axis.text.y= element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y= element_blank())

#plots for individual layouts and their spacings

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

#Estimate Plots and skewness####
#Generate stats for estimate as well as lcl and ucl
statsCI<-select.stats(results, parameter = 'D', statistics = c('estimate', 'lcl','ucl'))
#set mfrow to be nice
par(mfrow=c(4,4))
#plot the confidence intervals
plot(statsCI, type='CI')
#Generate Density estimates
statsD<-select.stats(results, parameter = 'D', statistics = c('estimate'))
#Plot histograms of estimates
plot(statsD, type='hist', statistic='estimate')


#Create object with just density estimates
DensityEstimates<-statsD$output

#empty dataframe to contain skewness
Skewnessdf<-data.frame(Layout=c(rep('Grid', 4), rep('Circle', 4), rep('Line', 4), rep('Axis', 4)), 
                       Spacing=c(1, 1.5, 2, 2.5),
                       Skewness=rep(NA, 16))

#Calculate skewness for each scenario
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

