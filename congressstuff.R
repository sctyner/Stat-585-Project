library(reshape2)
library(plyr)
library(XLConnect)
wb <- loadWorkbook("/Users/samanthatyner/Desktop/585/Project/VitalStatisticsFullData/Vital Statistics Chapter 1- Demographics of Members of Congress.xlsx")
#sheet 1: apportionment of congressional seats 1910-2010 (435 total seats)
appor.seats = readWorksheet(wb, sheet = "1-1",startRow=4,endRow=69)
names(appor.seats)<-c("Region.and.State",1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010)
appor.seats<-appor.seats[,-3]
appor.seats$Region.and.State<-as.factor(appor.seats$Region.and.State)
appor.seats<-appor.seats[-which(is.na(appor.seats$Region.and.State)),]
appor.seats[,2]<-as.numeric(appor.seats[,2])
appor.seats[,1:2]
appor.seats[56,2]<-1
summary(appor.seats)
appor.seats.state<-appor.seats[-which(appor.seats$Region.and.State %in% c("South","Border","New England","Mid-Atlantic","Midwest","Plains","Rocky Mountains","Pacific Coast")),]
appor.seats.state<-melt(appor.seats.state, id='Region.and.State')
appor.seats.state$value<-as.numeric(sub('[a-z]','',appor.seats.state$value))
names(appor.seats.state)<-c('State','Census.Year','No.reps')

# number of seats in congress by region - appor.seats.reg
appor.seats.reg<-appor.seats[which(appor.seats$Region.and.State %in% c("South","Border","New England","Mid-Atlantic","Midwest","Plains","Rocky Mountains","Pacific Coast")),]
names(appor.seats.reg)<-c('Region',names(appor.seats.reg)[-1])
appor.seats.reg<-melt(appor.seats.reg,id="Region")
names(appor.seats.reg)<-c("Region","Census.Year","No.reps")
# number of seats in congress by state - appor.seats.ste
appor.seats.ste<-appor.seats[-which(appor.seats$Region.and.State %in% c("South","Border","New England","Mid-Atlantic","Midwest","Plains","Rocky Mountains","Pacific Coast")),]
names(appor.seats.ste)<-c('State',names(appor.seats.ste)[-1])
appor.seats.ste<-melt(appor.seats.ste,id="State")
names(appor.seats.ste)<-c("State","Census.Year","No.reps")
appor.seats.ste$State<-sub('   ','',appor.seats.ste$State)
appor.seats.ste$State<-as.factor(appor.seats.ste$State)
#regions and states dataframe for reference later
states<-read.csv("/Users/samanthatyner/Desktop/585/R files and Data/Lab Week 2/states.csv")
states$Region<-NA
states$Region[states$State %in% c("Alabama","Arkansas","Florida","Georgia","Louisiana","Mississippi","North Carolina","South Carolina","Tennessee","Texas","Virginia")]<-"South"
states$Region[states$State %in% c("Kentucky","Maryland","Missouri","Oklahoma","West Virginia")]<-"Border"
states$Region[states$State %in% c("Connecticut","Maine","Massachusetts","New Hampshire","Rhode Island","Vermont")]<-"New England"
states$Region[states$State %in% c("Delaware","New Jersey","New York","Pennsylvania")]<-"Mid-Atlantic"
states$Region[states$State %in% c("Illinois","Indiana","Michigan","Ohio","Wisconsin")] <- "Midwest"
states$Region[states$State %in% c("Iowa", "Kansas","Minnesota","Nebraska","North Dakota","South Dakota")] <- "Plains"
states$Region[states$State %in% c("Arizona","Colorado","Idaho","Montana","Nevada","New Mexico","Utah","Wyoming")] <- "Rocky Mountains"
states$Region[states$State %in% c("Alaska","California","Hawaii","Oregon","Washington")] <- "Pacific Coast"
states$Region<-as.factor(states$Region)
states<-states[-9,]

seats.allotted<-merge(appor.seats.state,states[,c(1,3)])
seats.allotted<-seats.allotted[,c(1,4,2,3)]
seats.allotted<-sort_df(seats.allotted,vars=c('State','Census.Year'))
seats.allotted$No.reps<-as.numeric(sub('[a-z]','',seats.allotted$No.reps))
seats.allotted[101,4]<-NA

#sheet 2: Democratic Party Strength in the House, by Region, 69th-113th Congresses, 1925-2014
dem.strg = readWorksheet(wb, sheet = "1-2",startRow=4,endRow=35)
names(dem.strg)<- c('Region',69,75,81,87,93,96,97,98,100:113)
dem.strg<- dem.strg[-which(is.na(dem.strg$Region)),]
dem.strg$Region<-as.factor(dem.strg$Region)
dem.strg.perc<-subset(dem.strg,Region=="Percent")
dem.strg.perc$Region<-unique(appor.seats.reg$Region)
dem.strg.perc<-melt(dem.strg.perc,id="Region")
names(dem.strg.perc)<-c("Region","Congress","Perc.dems")
dem.strg.perc$Perc.dems<-as.numeric(dem.strg.perc$Perc.dems)

# dem.strg.cnt<-subset(dem.strg,Region=="Seats")
# dem.strg.cnt$Region<-unique(appor.seats.reg$Region)
# dem.strg.cnt<-melt(dem.strg.cnt,id="Region")
# names(dem.strg.cnt)<-c("Region","Congress","No.dems")
# dem.strg.cnt$No.dems<-sub('[a-z]|[A-Z]','',dem.strg.cnt$No.dems)
# dem.strg.cnt$No.dems<-as.numeric(dem.strg.cnt$No.dems)


#sheet 3: Democratic and Republican Seats in the House, by Region, 69th-113th Congresses, 1925-2014
parties = readWorksheet(wb, sheet = "1-3",startRow=5,endRow=36)
parties<-parties[-which(is.na(parties$Region)),]

dems<-parties[,c(1,grep('D',names(parties)))]
names(dems)<-c("Region",69,75,81,87,93,96:98,101:113)
dems$Region<-as.factor(dems$Region)
dems.perc<-subset(dems,Region=="Percent")
dems.perc$Region<-unique(appor.seats.reg$Region)
dems.perc<-melt(dems.perc,id="Region")
names(dems.perc)<-c("Region","Congress","Perc.alldems")

dems.cnt<-subset(dems,Region=="Seats")
dems.cnt$Region<-unique(appor.seats.reg$Region)
dems.cnt<-melt(dems.cnt,id="Region")
names(dems.cnt)<-c("Region","Congress","No.dems")

dems.clean<-merge(dems.cnt,dems.perc)

reps<-parties[,grep('R',names(parties))]
names(reps)<-c("Region",69,75,81,87,93,96:98,101:113)
reps$Region<-as.factor(reps$Region)
reps.perc<-subset(reps,Region=="Percent")
reps.perc$Region<-unique(appor.seats.reg$Region)
reps.perc<-melt(reps.perc,id="Region")
names(reps.perc)<-c("Region","Congress","Perc.allreps")

reps.cnt<-subset(reps,Region=="Seats")
reps.cnt$Region<-unique(appor.seats.reg$Region)
reps.cnt<-melt(reps.cnt,id="Region")
names(reps.cnt)<-c("Region","Congress","No.reps")

reps.clean<-merge(reps.cnt,reps.perc)

parties.clean<-merge(dems.clean,reps.clean)
parties.clean$No.Total<-parties.clean$No.dems+parties.clean$No.reps
parties.clean<-sort_df(parties.clean,var='Congress') #in parties.clean, No.reps is # of republican representatives
parties.clean$Office<-'House'

#sheet 4: Democratic Party Strength in the Senate, by Region, 69th-113th Congresses, 1925-2014
dem.strg.sen = readWorksheet(wb, sheet = "1-4",startRow=4,endRow=35)
names(dem.strg.sen)<- c('Region',69,75,81,87,93,96,97,98,100:113)
dem.strg.sen<- dem.strg.sen[-which(is.na(dem.strg.sen$Region)),]
dem.strg.sen$Region<-as.factor(dem.strg.sen$Region)
dem.strg.sen.perc<-subset(dem.strg.sen,Region=="Percent")
dem.strg.sen.perc$Region<-unique(appor.seats.reg$Region)
dem.strg.sen.perc<-melt(dem.strg.sen.perc,id="Region")
names(dem.strg.sen.perc)<-c("Region","Congress","Perc.dems")
dem.strg.sen.perc$Perc.dems<-as.numeric(sub('[a-z]','',dem.strg.sen.perc$Perc.dems))

#sheet 5:Democratic and Republican Seats in the Senate, by Region, 69th-113th Congresses, 1925-2014
parties.sen = readWorksheet(wb, sheet = "1-5",startRow=5,endRow=36)
parties.sen<-parties.sen[-which(is.na(parties.sen$Region)),]

dems.sen<-parties.sen[,c(1,grep('D',names(parties.sen)))]
names(dems.sen)<-c("Region",69,75,81,87,93,97,101:113)
dems.sen$Region<-as.factor(dems.sen$Region)
dems.sen.perc<-subset(dems.sen,Region=="Percent")
dems.sen.perc$Region<-unique(appor.seats.reg$Region)
dems.sen.perc<-melt(dems.sen.perc,id="Region")
names(dems.sen.perc)<-c("Region","Congress","Perc.alldems.sen")

dems.sen.cnt<-subset(dems.sen,Region=="Seats")
dems.sen.cnt$Region<-unique(appor.seats.reg$Region)
dems.sen.cnt<-melt(dems.sen.cnt,id="Region")
names(dems.sen.cnt)<-c("Region","Congress","No.dems.sen")

dems.sen.clean<-merge(dems.sen.cnt,dems.sen.perc)

reps.sen<-parties.sen[,grep('R',names(parties.sen))]
names(reps.sen)<-c("Region",69,75,81,87,93,97,101:113)
reps.sen$Region<-as.factor(reps.sen$Region)
reps.sen.perc<-subset(reps.sen,Region=="Percent")
reps.sen.perc$Region<-unique(appor.seats.reg$Region)
reps.sen.perc<-melt(reps.sen.perc,id="Region")
names(reps.sen.perc)<-c("Region","Congress","Perc.allreps.sen")
reps.sen.perc$Perc.allreps.sen<-sub('[a-z]|[A-Z]','',reps.sen.perc$Perc.allreps.sen)

reps.sen.cnt<-subset(reps.sen,Region=="Seats")
reps.sen.cnt$Region<-unique(appor.seats.reg$Region)
reps.sen.cnt<-melt(reps.sen.cnt,id="Region")
names(reps.sen.cnt)<-c("Region","Congress","No.reps.sen")
reps.sen.cnt$No.reps.sen<-sub('[a-z]|[A-Z]','',reps.sen.cnt$No.reps.sen)
reps.sen.clean<-merge(reps.sen.cnt,reps.sen.perc)

parties.sen.clean<-merge(dems.sen.clean,reps.sen.clean)
parties.sen.clean$No.reps.sen<-as.numeric(parties.sen.clean$No.reps.sen)
parties.sen.clean$Perc.allreps.sen<-as.numeric(parties.sen.clean$Perc.allreps.sen)
parties.sen.clean$No.Total.sen<-parties.sen.clean$No.dems.sen+parties.sen.clean$No.reps.sen
parties.sen.clean<-sort_df(parties.sen.clean,var='Congress')
parties.sen.clean$Office<-'Senate'
names(parties.sen.clean)<-names(parties.clean)
#combining sheets 4 and 5
parties.all<-rbind(parties.clean,parties.sen.clean)
parties.all$Office<-as.factor(parties.all$Office)
parties.all<-melt(parties.all,id=c('Region','Congress','No.Total','Office'))
levels(parties.all$variable)<-c('Democrat','Perc.alldems','Republican','Perc.allreps')
parties.all.count<-subset(parties.all, variable %in% c('Democrat','Republican'))
parties.all.perc<-subset(parties.all, variable %in% c('Perc.alldems','Perc.allreps'))

qplot(Congress,value,color=Region,geom='point',shape=variable,data=parties.all.count)+facet_wrap(variable~Office)
##Data set as years of congresses for reference
congresses<-read.csv("/Users/samanthatyner/Desktop/585/Project/congress_years.csv")
congresses<-congresses[,-3]
congresses$Congress<-as.numeric(sub('[a-z][a-z]','',congresses$Congress))
congresses<-cbind(congresses,ldply(strsplit(as.character(congresses$Years),'-')))
congresses<-congresses[-2]
names(congresses)<-c("Congress","Year.Start","Year.End")
which(nchar(congresses$Year.End)==4)
pref<-c(rep(17,5),'',rep(18,49),'',rep(19,49),rep('',8))
congresses$pref<-pref
congresses$Year.End<-paste(congresses$pref,congresses$Year.End,sep='')
congresses<-congresses[-4]

#sheet 6: Seniority of Representatives, 1953 - 2014
seniority = readWorksheet(wb, sheet = "1-6",startRow=4,endRow=127)
seniority <- seniority[,-c(5,10,11)]
seniority <- seniority[-which(is.na(seniority$Congress)),]
seniority$Congress <- as.factor(seniority$Congress)
seniority$Congress <- sub("   ",'',seniority$Congress)
unique(seniority$Congress)

senior.perc<-subset(seniority,Congress=="Percent")
senior.perc<-senior.perc[,-8]
senior.perc$Congress<-unique(seniority$Congress)[-c(2,3)]
senior.perc$Congress<-ldply(strsplit(senior.perc$Congress,'[a-z][a-z]'))$V1
names(senior.perc)<-c("Congress","1.term.perc","2.terms.perc","3.terms.perc","4-6.terms.perc", "7-9.terms.perc", "10+.terms.perc")
senior.perc<-melt(senior.perc,id='Congress')

senior.cnt<-subset(seniority,Congress=="Seats")
senior.cnt$Congress<-unique(seniority$Congress)[-c(2,3)]
senior.cnt$Congress<-ldply(strsplit(senior.cnt$Congress,'[a-z][a-z]'))$V1
names(senior.cnt)<-c("Congress","1.term","2.terms","3.terms","4-6.terms", "7-9.terms", "10+.terms","Total")
senior.cnt$Total<-sub('[a-z]','',senior.cnt$Total)
senior.cnt<-melt(senior.cnt,id=c('Congress','Total'))
senior.clean<-cbind(senior.cnt,senior.perc[,3])
names(senior.clean)<-c('Congress','Total','Terms','Count','Percent')
levels(senior.clean$Terms)<-c('1','2','3','4-6','7-9','10+')
senior.clean$Congress<-as.numeric(senior.clean$Congress)
senior.clean$Total<-as.numeric(senior.clean$Total)

#seniority.clean<-merge(senior.cnt,senior.perc)
#seniority.clean$Congress<-as.numeric(seniority.clean$Congress)
#seniority.clean$Total<-as.numeric(seniority.clean$Total)

seniority.clean<-seniority.clean[order(seniority.clean$Congress),]

#sheet 7: Seniority of Senators, 1953 - 2014
senior.senate = readWorksheet(wb, sheet = "1-7",startRow=4,endRow=35)
senior.senate$Col2<-sub('\\)','',sub('\\(','',senior.senate$Col2))
senior.senate<-senior.senate[,-c(8,9)]
names(senior.senate)<-c("Congress","Year.Start","Less6years","7-12years","13-18years","More19years",'Total')
senior.senate$No.freshman<-ldply(strsplit(senior.senate$Less6years," "))$V2
senior.senate$Less6years<-ldply(strsplit(senior.senate$Less6years," "))$V1
senior.senate<-senior.senate[,c(1,2,8,3,4,5,6,7)]
senior.senate$No.freshman<-as.numeric(sub('[a-z]','',sub('\\)','',sub('\\(','',senior.senate$No.freshman))))
senior.senate$Congress<-as.numeric(sub('[a-z][a-z]','',senior.senate$Congress))
senior.senate$Year.Start<-as.numeric(senior.senate$Year.Start)
senior.senate$Less6years<-as.numeric(senior.senate$Less6years)
senior.senate<-melt(senior.senate,id=c('Congress','Year.Start','Total'))
names(senior.senate)<-c('Congress','Year.Start','Total','Terms','Count')
levels(senior.senate$Terms)<-c('FirstYear','1','2','3','4+')

#senior.senate.clean<-senior.senate[,-2]

#sheet 8: Prior Occupations of Representatives, 83rd - 113th Congresses, 1953 - 2014
priors.house<-readWorksheet(wb, sheet = "1-8",startRow=3,endRow=29)
priors.house<-priors.house[-19,]
names(priors.house)<-sub('X','',sub('[a-z][a-z].[0-9][0-9][0-9][0-9]','',names(priors.house)))
priors.house<-melt(priors.house)
names(priors.house)<-c('Occupation','Congress','No.reps')
priors.house$No.reps[is.na(priors.house$No.reps)]<-0
priors.house.clean<-priors.house[,c(2,1,3)]

#sheet 9: Prior Occupations of Democratic Representatives, 83rd - 113th Congresses, 1953 - 2014
priors.house.dem<-readWorksheet(wb, sheet = "1-9",startRow=3,endRow=29)
priors.house.dem<-priors.house.dem[-19,]
names(priors.house.dem)<-sub('X','',sub('[a-z][a-z].[0-9][0-9][0-9][0-9]','',names(priors.house.dem)))
priors.house.dem<-melt(priors.house.dem)
names(priors.house.dem)<-c('Occupation','Congress','No.reps')
priors.house.dem$No.reps[is.na(priors.house.dem$No.reps)]<-0
priors.house.dem$Party<-'Democrat'
priors.house.dem.clean<-priors.house.dem[,c(2,4,1,3)]

#sheet 10: Prior Occupations of Republican Representatives, 83rd - 110th Congresses, 1953 - 2012
priors.house.rep<-readWorksheet(wb, sheet = "1-10",startRow=3,endRow=30)
priors.house.rep<-priors.house.rep[-19,]
names(priors.house.rep)<-sub('X','',sub('[a-z][a-z].[0-9][0-9][0-9][0-9]','',names(priors.house.rep)))
priors.house.rep<-melt(priors.house.rep)
names(priors.house.rep)<-c('Occupation','Congress','No.reps')
priors.house.rep$No.reps[is.na(priors.house.rep$No.reps)]<-0
priors.house.rep$Party<-'Republican'
priors.house.rep.clean<-priors.house.rep[,c(2,4,1,3)]

priors.by.party.house<-rbind(priors.house.dem.clean,priors.house.rep.clean)
priors.by.party.house<-priors.by.party.house[order(priors.by.party.house$Congress),]
priors.by.party.house$Office<-'House'
#sheet 11: Prior Occupations of Senators, 83rd - 113th Congresses, 1953 - 2014
priors.sen<-readWorksheet(wb, sheet = "1-11",startRow=3,endRow=30)
priors.sen<-priors.sen[-19,]
names(priors.sen)<-sub('X','',sub('[a-z][a-z].[0-9][0-9][0-9][0-9]','',names(priors.sen)))
priors.sen<-melt(priors.sen)
names(priors.sen)<-c('Occupation','Congress','No.sens')
priors.sen$No.sens[is.na(priors.sen$No.sens)]<-0
priors.sen.clean<-priors.sen[,c(2,1,3)]

prior.occ.congress<-merge(priors.house.clean,priors.sen.clean)
prior.occ.congress$Congress<-as.numeric(as.character(prior.occ.congress$Congress))
prior.occ.congress<-prior.occ.congress[order(prior.occ.congress$Congress),]
prior.occ.congress<-melt(prior.occ.congress,id=c('Congress','Occupation'))
names(prior.occ.congress)<-c('Congress','Occupation','Office','Count')
levels(prior.occ.congress$Office)<-c('House','Senate')
prior.occ.congress$Occupation<-as.factor(prior.occ.congress$Occupation)

#look at changes of the occupation trends over time

#sheet 12: Prior Occupations of Democratic Senators, 83rd - 113th Congresses, 1953 - 2014
priors.sen.dem<-readWorksheet(wb, sheet = "1-12",startRow=3,endRow=30)
priors.sen.dem<-priors.sen.dem[-19,]
names(priors.sen.dem)<-sub('X','',sub('[a-z][a-z].[0-9][0-9][0-9][0-9]','',names(priors.sen.dem)))
priors.sen.dem<-melt(priors.sen.dem)
names(priors.sen.dem)<-c('Occupation','Congress','No.sens')
priors.sen.dem$No.sens[is.na(priors.sen.dem$No.sens)]<-0
priors.sen.dem$Party<-'Democrat'
priors.sen.dem.clean<-priors.sen.dem[,c(2,4,1,3)]

#sheet 13: Prior Occupations of Republican Senators, 83rd - 110th Congresses, 1953 - 2012
priors.sen.rep<-readWorksheet(wb, sheet = "1-13",startRow=3,endRow=30)
priors.sen.rep<-priors.sen.rep[-19,]
names(priors.sen.rep)<-sub('X','',sub('[a-z][a-z].[0-9][0-9][0-9][0-9]','',names(priors.sen.rep)))
priors.sen.rep<-melt(priors.sen.rep)
names(priors.sen.rep)<-c('Occupation','Congress','No.sens')
priors.sen.rep$No.sens[is.na(priors.sen.rep$No.sens)]<-0
priors.sen.rep$Party<-'Republican'
priors.sen.rep.clean<-priors.sen.rep[,c(2,4,1,3)]

priors.by.party.sen<-rbind(priors.sen.dem.clean,priors.sen.rep.clean)
priors.by.party.sen<-priors.by.party.sen[order(priors.by.party.sen$Congress),]
priors.by.party.sen$Office<-'Senate'
names(priors.by.party.sen)[4]<-'Count'
names(priors.by.party.house)[4]<-'Count'
prior.occ.congress.party<-rbind(priors.by.party.house,priors.by.party.sen)
prior.occ.congress.party$Congress<-as.numeric(as.character(prior.occ.congress.party$Congress))
prior.occ.congress.party<-prior.occ.congress.party[order(prior.occ.congress.party$Congress),]
prior.occ.congress.party<-sort_df(prior.occ.congress.party, vars=c('Congress','Occupation'))
prior.occ.congress.party<-prior.occ.congress.party[,c(1,5,2,3,4)]
prior.occ.congress.party$Party<-as.factor(prior.occ.congress.party$Party)
prior.occ.congress.party$Office<-as.factor(prior.occ.congress.party$Office)
prior.occ.congress.party$Occupation<-as.factor(prior.occ.congress.party$Occupation)
prior.occ.congress.party<-subset(prior.occ.congress.party,Count!=0)

qplot(Congress,Count,data=prior.occ.congress.party,geom='point',color=Party)+facet_wrap(~Occupation)
#sheet 14: Religious Affiliations of Representatives, 89th - 113th Congresses, 1965 - 2014
religion.house<-readWorksheet(wb, sheet = "1-14",startRow=4,endRow=18)
religion.house <- religion.house[-which(is.na(religion.house$Col1)),]
religion.house<-melt(religion.house,id='Col1')
religion.house<-religion.house[-which(as.factor(religion.house$Col1)=='Protestant'),]
religion.house$value<-sub('[a-z]','',religion.house$value)
religion.house$value<-as.numeric(religion.house$value)
religion.house$value[is.na(religion.house$value)]<-0
names(religion.house)<-c('Religion','Party','Count')

religion.house.dem<-religion.house[grep('D',religion.house$Party),]
religion.house.dem$Congress<-sort(rep(c(89,90,92:97,99,101:113),10))
religion.house.dem$Party<-'Democrat'

religion.house.rep<-religion.house[grep('R',religion.house$Party),]
religion.house.rep$Congress<-sort(rep(c(89,90,92:97,99,101:113),10))
religion.house.rep$Party<-'Republican'

religion.house<-rbind(religion.house.rep,religion.house.dem)
religion.house<-sort_df(religion.house, vars=c('Congress','Religion'))
religion.house<-religion.house[,c(4,1,2,3)]
religion.house$Office<-'House'
religion.house<-religion.house[,c(1,5,3,2,4)]
#sheet 15: Religious Affiliations of Senators, 89th - 113th Congresses, 1965 - 2014
religion.sen<-readWorksheet(wb, sheet = "1-15",startRow=4,endRow=18)
religion.sen <- religion.sen[-which(is.na(religion.sen$Col1)),]
religion.sen<-melt(religion.sen,id='Col1')
religion.sen<-religion.sen[-which(as.factor(religion.sen$Col1)=='Protestant'),]
religion.sen$value<-sub('[a-z]','',religion.sen$value)
religion.sen$value<-as.numeric(religion.sen$value)
religion.sen$value[is.na(religion.sen$value)]<-0
names(religion.sen)<-c('Religion','Party','Count')

religion.sen.dem<-religion.sen[grep('D',religion.sen$Party),]
religion.sen.dem$Congress<-sort(rep(c(89,90,92:97,99,101:113),10))
religion.sen.dem$Party<-'Democrat'

religion.sen.rep<-religion.sen[grep('R',religion.sen$Party),]
religion.sen.rep$Congress<-sort(rep(c(89,90,92:97,99,101:113),10))
religion.sen.rep$Party<-'Republican'

religion.sen<-rbind(religion.sen.rep,religion.sen.dem)
religion.sen<-sort_df(religion.sen, vars=c('Congress','Religion'))
religion.sen<-religion.sen[,c(4,1,2,3)]
religion.sen$Office<-'Senate'

religions.congress<-rbind(religion.house,religion.sen)
religions.congress<-religions.congress[order(religions.congress$Congress),]
religions.congress$Office<-as.factor(religions.congress$Office)
religions.congress$Party<-as.factor(religions.congress$Party)
religions.congress$Religion<-as.factor(religions.congress$Religion)
#plot
qplot(Congress,No.reps,data=religions.congress,group=Religion,geom='point',color=Religion,shape=Party,size=2)

#sheet 16: African Americans in Congress, 41st - 113th Congresses, 1869 - 2014
afr.am<-readWorksheet(wb, sheet = "1-16",startRow=4,endRow=63,endCol=7)
afr.am[is.na(afr.am)]<-0
afr.am<-afr.am[,-c(2,5)]
afr.am$Congress<-sub('[a-z]','',sub('[a-z][a-z]','',afr.am$Congress))

afr.am.sen<-afr.am[,c(1,4,5)]
afr.am.sen<-melt(afr.am.sen)
names(afr.am.sen)<-c('Congress','Party','No.sens')
levels(afr.am.sen$Party)<-c('D','R')

afr.am.house<-afr.am[,c(1,2,3)]
afr.am.house<-melt(afr.am.house)
names(afr.am.house)<-c('Congress','Party','No.reps')
levels(afr.am.house$Party)<-c('D','R')

afr.am<-merge(afr.am.house,afr.am.sen)
zeros<-data.frame(cbind(sort(rep(57:70,2)),rep(c('D','R'),14),rep(0,28),rep(0,28)))
names(zeros)<-names(afr.am)
afr.am<-rbind(afr.am,zeros)
afr.am<-afr.am[order(as.numeric(afr.am$Congress)),]
afr.am<-melt(afr.am,id=c('Congress','Party'))
names(afr.am)<-c("Congress","Party","Office","No.Afr.Am")
levels(afr.am$Office)<-c("House","Senate")

#sheet 17: Hispanic Americans in Congress, 41st - 113th Congresses, 1869 - 2014
hisp.am<-readWorksheet(wb, sheet = "1-17",startRow=5,endRow=59,endCol=7)
hisp.am[is.na(hisp.am)]<-0
hisp.am<-hisp.am[,-c(2,5)]
hisp.am$Congress<-sub('[a-z]','',sub('[a-z][a-z]','',hisp.am$Congress))

hisp.am.sen<-hisp.am[,c(1,4,5)]
hisp.am.sen<-melt(hisp.am.sen)
names(hisp.am.sen)<-c('Congress','Party','No.sens')
levels(hisp.am.sen$Party)<-c('D','R')

hisp.am.house<-hisp.am[,c(1,2,3)]
hisp.am.house<-melt(hisp.am.house)
names(hisp.am.house)<-c('Congress','Party','No.reps')
levels(hisp.am.house$Party)<-c('D','R')

hisp.am<-merge(hisp.am.house,hisp.am.sen)
zeros2<-data.frame(cbind(sort(rep(44:62,2)),rep(c('D','R'),19),rep(0,38),rep(0,38)))
names(zeros2)<-names(hisp.am)
hisp.am<-rbind(hisp.am,zeros)
hisp.am<-hisp.am[order(as.numeric(hisp.am$Congress)),]
hisp.am<-melt(hisp.am,id=c('Congress','Party'))
names(hisp.am)<-c("Congress","Party","Office","No.Hisp.Am")
levels(hisp.am$Office)<-c("House","Senate")

#sheet 18: Women in Congress, 65th - 113th Congresses, 1917 - 2014
women<-readWorksheet(wb, sheet = "1-18",startRow=4,endRow=53,endCol=7)
women[is.na(women)]<-0
women<-women[,-c(2,5)]
women$Congress<-sub('[a-z]','',sub('[a-z][a-z]','',women$Congress))

women.sen<-women[,c(1,4,5)]
women.sen<-melt(women.sen)
names(women.sen)<-c('Congress','Party','No.sens')
levels(women.sen$Party)<-c('D','R')

women.house<-women[,c(1,2,3)]
women.house<-melt(women.house)
names(women.house)<-c('Congress','Party','No.reps')
levels(women.house$Party)<-c('D','R')

women<-merge(women.house,women.sen)
women<-women[order(as.numeric(women$Congress)),]
women<-melt(women,id=c('Congress','Party'))
names(women)<-c("Congress","Party","Office","No.Women")
levels(women$Office)<-c("House","Senate")

diversity<-merge(merge(afr.am,hisp.am,all=T),women,all=T)
diversity$Congress<-as.numeric(diversity$Congress)
diversity$No.Afr.Am<-as.numeric(as.character(diversity$No.Afr.Am))
diversity$No.Hisp.Am<-as.numeric(as.character(diversity$No.Hisp.Am))
diversity<-melt(diversity,id=c('Congress','Party','Office'))
names(diversity)[4:5]<-c('Group','Count')
levels(diversity$Group)<-c('AfricanAmericans','HispanicAmericans','Women')
 qplot(data=diversity,x=Congress,y=Count,geom='point',color=Party)+facet_wrap(Office~Group)

write.csv(diversity,'congressional_diversity.csv')
#sheet 19: Political Parties of Senators and Representatives, 34th - 113th Congresses, 1855 - 2014
summary.party<-readWorksheet(wb, sheet = "1-19",startRow=4,endRow=84)
summary.party<-summary.party[,-c(2,3,8,9)]
summary.party[is.na(summary.party)]<-0
summary.party$Congress<-sub('[a-z][a-z]','',summary.party$Congress)

summary.party.senate<-summary.party[,c(1:5)]
summary.party.house<-summary.party[,c(1,6:9)]
names(summary.party.house)<-names(summary.party.senate)
summary.party.senate<-melt(summary.party.senate)
names(summary.party.senate)<-c('Congress','Party','Count')
summary.party.senate<-summary.party.senate[order(as.numeric(summary.party.senate$Congress)),]
summary.party.senate$Office<-'Senate'
summary.party.house$Democrats<-as.numeric(sub('[a-z]','',summary.party.house$Democrats))
summary.party.house<-melt(summary.party.house,id='Congress')
names(summary.party.house)<-c('Congress','Party','Count')
summary.party.house<-summary.party.house[order(as.numeric(summary.party.house$Congress)),]
summary.party.house$Office<-'House'

summary.party<-rbind(summary.party.house,summary.party.senate)
summary.party<-summary.party[order(as.numeric(summary.party$Congress)),]
summary.party$Office<-as.factor(summary.party$Office)
summary.party$Congress<-as.numeric(summary.party$Congress)


#####################################################################################
wb2 <- loadWorkbook("/Users/samanthatyner/Desktop/585/Project/VitalStatisticsFullData/Vital Statistics Chapter 2 - Congressional Elections.xlsx")

#Turnout in Presidential and House Elections, 1930 - 2012 (percentage of voting age population)
voter.turnout<-readWorksheet(wb2,sheet = "2-1", startRow=3, endRow=45, endCol=3)
voter.turnout<-melt(voter.turnout,id='Year')
names(voter.turnout)<-c("Year","Election.Type","Percent")
voter.turnout<-na.omit(voter.turnout)
voter.turnout$Election.Type<-as.character(voter.turnout$Election.Type)
voter.turnout[which(voter.turnout$Election.Type=='House.elections' & voter.turnout$Year %in% voter.turnout$Year[which(voter.turnout$Election.Type=='Presidential.elections')]),]$Election.Type<-"On Year"
voter.turnout$Election.Type<-as.factor(voter.turnout$Election.Type)
levels(voter.turnout$Election.Type)<-c("Midterm","House","Presidential")

qplot(data=voter.turnout,x=Year,y=Percent,geom='path',group=Election.Type,color=Election.Type)+geom_point()+ylim(c(10,80))+geom_vline(xintercept=2012)

#Losses by the President's Party in Midterm Elections, 1862 - 2010
midterm.losses<-readWorksheet(wb2,sheet = "2-4", startRow=3, endRow=41, endCol=4)
names(midterm.losses)<-c("Year","Pres.Party","House","Senate")
midterm.losses<-melt(midterm.losses,id=c('Year','Pres.Party'))
names(midterm.losses)<-c('Year','Pres.Party','Office','Net.Loss')
midterm.losses$Net.Loss<-as.numeric(sub('[a-z]','',midterm.losses$Net.Loss))
midterm.losses$Pres.Party<-as.factor(midterm.losses$Pres.Party)

qplot(Year,Net.Loss,data=midterm.losses,geom='point',group=Office,color=Pres.Party)+geom_path()

#House Seats That Changed Party, 1954 - 2012
house.changes<-readWorksheet(wb2,sheet = "2-5", startRow=4, endRow=34, endCol=7)
house.changes<-house.changes[,-5]
names(house.changes)<-c('Year','Total.changes','Incum.DtoR','Incum.RtoD','Open.DtoR','Open.RtoD')
house.changes<-melt(house.changes,id=c('Year','Total.changes'))
house.changes<-cbind(house.changes,ldply(strsplit(as.character(house.changes$variable),'[.]')))
house.changes<-house.changes[,c(1,2,5,6,4)]
names(house.changes)<-c('Year','Total.changes','Type','Party.change','No.reps')
house.changes$Type<-as.factor(house.changes$Type)
house.changes$Party.change<-as.factor(house.changes$Party.change)
levels(house.changes$Type)<-c('IncumbentDefeated','OpenSeat')
house.changes<-house.changes[order(house.changes$Year),]

qplot(Year,No.reps,data=house.changes,geom='point',color=Party.change,shape=Type)+
  geom_path()+scale_colour_manual(values = wes.palette(2, "Moonrise3")) 

#Senate Seats That Changed Party, 1954 - 2012
senate.changes<-readWorksheet(wb2,sheet = "2-6", startRow=4, endRow=34, endCol=7)
senate.changes<-senate.changes[,-5]
names(senate.changes)<-c('Year','Total.changes','Incum.DtoR','Incum.RtoD','Open.DtoR','Open.RtoD')
senate.changes<-melt(senate.changes,id=c('Year','Total.changes'))
senate.changes<-cbind(senate.changes,ldply(strsplit(as.character(senate.changes$variable),'[.]')))
senate.changes<-senate.changes[,c(1,2,5,6,4)]
names(senate.changes)<-c('Year','Total.changes','Type','Party.change','No.sens')
senate.changes$Type<-as.factor(senate.changes$Type)
senate.changes$Party.change<-as.factor(senate.changes$Party.change)
levels(senate.changes$Type)<-c('IncumbentDefeated','OpenSeat')
senate.changes<-senate.changes[order(senate.changes$Year),]
senate.changes$No.sens<-as.numeric(sub('[a-z]','',as.character(senate.changes$No.sens)))

qplot(Year,No.sens,data=senate.changes,geom='point',color=Party.change,shape=Type)+
  geom_jitter()+scale_colour_manual(values = wes.palette(2, "Moonrise3")) 
#Defeated House Incumbents, 1946 - 2012
defeat.incumb<-readWorksheet(wb2,sheet = "2-10", startRow=4, endRow=96, endCol=11)
defeat.incumb<-defeat.incumb[-which(is.na(as.factor(defeat.incumb$Party))),]
defeat.incumb$Party<-sub('Democrat[a-z]','Democrat',defeat.incumb$Party)
defeat.incumb$Party<-sub('Republican[a-z]','Republican',defeat.incumb$Party)
defeat.incumb$Party<-as.factor(defeat.incumb$Party)
defeat.incumb<-defeat.incumb[,-8]
names(defeat.incumb)<-c('Year','Party','Incumbents.lost','Avg.terms','1term','2terms','3terms','4-6terms','7-9terms','10+terms')
years<-as.numeric(as.character(levels(as.factor(defeat.incumb$Year[-which(is.na(defeat.incumb$Year))]))))
years<-c(years,2012)
defeat.incumb$Year<-sort(rep(years,3))
defeat.incumb<-defeat.incumb[-which(defeat.incumb$Party=='Total'),]
defeat.incumb<-melt(defeat.incumb,id=c('Year','Party','Incumbents.lost','Avg.terms'))
defeat.incumb<-sort_df(defeat.incumb,vars=c('Year','variable'))
defeat.incumb<-defeat.incumb[,c(1,2,5,6,3,4)]
names(defeat.incumb)<-c('Year','Party','Terms.served','Incumbents.lost','Tot.party.loss','Avg.terms')
levels(defeat.incumb$Terms.served)<-c('1','2','3','4-6','7-9','10+')
defeat.incumb$Office<-'House'
qplot(Year,Incumbents.lost,data=defeat.incumb,group=Terms.served,geom='point',color=Party,size=Terms.served)+
  geom_path(aes(x=Year,y=Tot.party.loss,group=Party,linetype=Party),size=1)

#Defeated Senate Incumbents, 1946 - 2012
defeat.incumb.sen<-readWorksheet(wb2,sheet = "2-11", startRow=4, endRow=86, endCol=11)
defeat.incumb.sen<-defeat.incumb.sen[-which(is.na(as.factor(defeat.incumb.sen$Party))),]
defeat.incumb.sen$Party<-sub('Democrat[a-z]','Democrat',defeat.incumb.sen$Party)
defeat.incumb.sen$Party<-sub('Republican[a-z]','Republican',defeat.incumb.sen$Party)
defeat.incumb.sen$Party<-as.factor(defeat.incumb.sen$Party)
names(defeat.incumb.sen)<-c('Year','Party','Incumbents.lost','Avg.terms','1term','2terms','3terms','4terms','5terms','6terms')
years<-as.numeric(as.character(levels(as.factor(defeat.incumb.sen$Year[-which(is.na(defeat.incumb.sen$Year))]))))
years<-sort(rep(years,3))
years<-years[-c(9,12,30,36,42,45,60,63,69)]
defeat.incumb.sen$Year<-years
defeat.incumb.sen<-defeat.incumb.sen[-which(defeat.incumb.sen$Party=='Total'),]
defeat.incumb.sen<-melt(defeat.incumb.sen,id=c('Year','Party','Incumbents.lost','Avg.terms'))
defeat.incumb.sen<-sort_df(defeat.incumb.sen,vars=c('Year','variable'))
defeat.incumb.sen<-defeat.incumb.sen[,c(1,2,5,6,3,4)]
names(defeat.incumb.sen)<-c('Year','Party','Terms.served','Incumbents.lost','Tot.party.loss','Avg.terms')
levels(defeat.incumb.sen$Terms.served)<-c(1:6)
defeat.incumb.sen$Office<-'Senate'
levels(defeat.incumb.sen$Party)<-c("Democrat",'Republican','Republican','Total')

qplot(Year,Incumbents.lost,data=defeat.incumb.sen,group=Terms.served,geom='point',color=Party,size=Terms.served)+
  geom_path(aes(x=Year,y=Tot.party.loss,group=Party,linetype=Party),size=1)

#Party-Line Voting in Presidential and Congressional Elections, 1956 - 2010 (as a percentage of all voters)
party.lines<-readWorksheet(wb2,sheet = "2-19", startRow=4, endRow=32, endCol=12)
party.lines<-party.lines[-c(5,9)]
names(party.lines)<-c('Year','Pres.partyline','Pres.defect','Pres.indep','Sen.partyline',
                      'Sen.defect','Sen.indep','House.partyline','House.defect','House.indep')
party.lines<-melt(party.lines,id='Year')
party.lines<-party.lines[-which(is.na(party.lines$value)),]
party.lines<-cbind(party.lines,ldply(strsplit(as.character(party.lines$variable),'[.]')))
party.lines<-party.lines[,c(1,4,5,3)]
names(party.lines)<-c('Year','Type.elec','Type.voter','Perc.voters')
party.lines$Type.elec<-as.factor(party.lines$Type.elec)
party.lines$Type.voter<-as.factor(party.lines$Type.voter)
party.lines$Perc.voters<-as.numeric(party.lines$Perc.voters)
party.lines2<-merge(party.lines,voter.turnout)
#party.lines2$Type.elec[which(party.lines2$Type.elec=='Sen' & party.lines2$Election.Type=='Midterm')]<-'Senate.midterm'
#party.lines2$Type.elec[which(party.lines2$Type.elec=='House' & party.lines2$Election.Type=='Midterm')]<-'House.midterm'
party.lines2$Type.elec<-as.factor(party.lines2$Type.elec)
party.lines2$Type.voter<-as.factor(party.lines2$Type.voter)
party.lines2$Perc.voters<-as.numeric(party.lines2$Perc.voters)

qplot(Year,Perc.voters,geom='point',color=Type.elec,data=party.lines,shape=Type.voter,size=I(3))+scale_colour_manual(values = wes.palette(3, "Zissou")) 

################################################################################
wb8<-loadWorkbook("/Users/samanthatyner/Desktop/585/Project/VitalStatisticsFullData/Vital Statistics Chapter 8 - Political Polarization in Congress and Changing Voting Alignments.xlsx")

#presidents:  DOUBLE 1974 for Nixon and Ford
#Notes: Percentages indicate the number of congressional votes supporting the president 
#divided by the total number of votes on which the president had taken a clear position.
#The percentages are normalized to eliminate the effects of absences as follows: 
#support = (support)/(support + opposition).

press<-c(rep('Truman',8),rep('Eisenhower',8),rep('Kennedy',3),rep('Johnson',5),rep('Nixon',6),rep('Ford',3),rep('Carter',4),rep('Reagan',8),rep('Bush Sr',4),rep('Clinton',8),rep('Bush Jr',8),rep('Obama',4))
yrs<-1945:2012
yrs<-sort(c(1974,yrs))
pres.yrs<-data.frame(cbind(press,yrs))
pres.yrs$yrs<-as.numeric(as.character(pres.yrs$yrs))
names(pres.yrs)<-c('President','Year')
#Presidential Victories on Votes in Congress, 1953-2012
#house.votes, senate.votes = number of votes that occurred
pres.victor<-readWorksheet(wb8,sheet = "8-1", startRow=3, endRow=94, endCol=6)
names(pres.victor)<-c('Year','Total.Perc','House.Perc','House.votes','Senate.perc','Senate.votes')
pres.victor$Year<-as.numeric(pres.victor$Year)
pres.victor<-pres.victor[-which(is.na(pres.victor$Year)),]
pres.victor$President<-press
pres.victor<-pres.victor[,c(7,1:6)]
pres.victor$House.Perc<-as.numeric(pres.victor$House.Perc)
pres.victor$House.votes<-as.numeric(pres.victor$House.votes)
pres.victor$Senate.perc<-as.numeric(pres.victor$Senate.perc)
pres.victor$Senate.votes<-as.numeric(pres.victor$Senate.votes)
pres.victor$President<-as.factor(pres.victor$President)

pres.party<-data.frame(cbind(unique(press),c('D','R','D','D','R','R','D','R','R','D','R','D')))
names(pres.party)<-c('President','Party')
pres.yr.party<-sort_df(merge(pres.party,pres.yrs),vars='Year')

pres.victor<-merge(pres.party,pres.victor)
pres.victor<-sort_df(pres.victor,vars='Year')

qplot(Year,House.votes,data=pres.victor,color=Party,geom='point')+geom_path()+geom_vline(xintercept=midterm.years[6:21])
qplot(Year,House.Perc,data=pres.victor)+geom_path()+geom_vline(xintercept=midterm.years[6:21],color=I('red'))

midterm.years<-subset(voter.turnout,Election.Type=='Midterm')$Year

qplot(Year,Senate.votes,data=pres.victor,color=Party,geom='point')+geom_path()+geom_vline(xintercept=midterm.years[6:21])
qplot(Year,Senate.perc,data=pres.victor)+geom_path()+geom_vline(xintercept=midterm.years[6:21],color=I('red'))

# Congressional Voting in Support of the President's Position, 1954-2012 (percent)
pres.support.party<-readWorksheet(wb8,sheet = "8-2", startRow=4, endRow=85, endCol=8)
pres.support.party<-pres.support.party[-which(is.na(as.numeric(pres.support.party$President.and.year))),]
pres.support.party<-pres.support.party[,-5]
names(pres.support.party)<-c('Year','House.Dems','House.South.Dems','House.Reps','Senate.Dems','Senate.South.Dems','Senate.Reps')
pres.support.party$President<-press[-1]
pres.support.party<-pres.support.party[,c(1,8,2:7)]
pres.support.party<-merge(pres.support.party,pres.party)
pres.support.party<-pres.support.party[,c(2,1,9,3:8)]
pres.support.party<-sort_df(pres.support.party,vars=c('Year','President'))
pres.support.party$House.South.Dems<-as.numeric(pres.support.party$House.South.Dems)
pres.support.party$House.Reps<-as.numeric(pres.support.party$House.Reps)
pres.support.party$Senate.South.Dems<-as.numeric(pres.support.party$Senate.South.Dems)
pres.support.party$Year<-as.numeric(pres.support.party$Year)
pres.support.party$President<-as.factor(pres.support.party$President)
str(pres.support.party)
pres.support.party<-melt.data.frame(pres.support.party,id.vars=c('Year','President','Party'))
levels(pres.support.party$variable)<-c("House.Dems","House.SouthDems","House.Reps",'Senate.Dems',"Senate.SouthDems","Senate.Reps")
pres.support.party<-cbind(pres.support.party,ldply(strsplit(as.character(pres.support.party$variable),'[.]')))
pres.support.party<-pres.support.party[,c(1:3,6,7,5)]
names(pres.support.party)<-c('Year','President','Pres.Party','Office','Party','Perc.support')
pres.support.party$Office<-as.factor(pres.support.party$Office)
pres.support.party$Party<-as.factor(pres.support.party$Party)
ggplot(data=pres.support.party,aes(x=factor(pres.support.party$President, levels = unique(pres.support.party$President)),y=Perc.support))+geom_boxplot(aes(fill=Party),notch=T)+labs(x='President',y='% in Support of President',title="Congressional Voting in Support of the President's Position")

levels(pres.support.party$Pres.Party)<-c('Democrat','Republican','SouthernDemocrat')
pres.support.party$Pres.Party[which(pres.support.party$President %in%c('Carter','Johnson','Clinton'))]<-'SouthernDemocrat'
levels(pres.support.party$Party)<-c('Democrat','Republican','SouthernDemocrat')

pres.support.party$Same.Party<-pres.support.party$Pres.Party==pres.support.party$Party
pres.support.party$Same.Party<-as.factor(pres.support.party$Same.Party)
levels(pres.support.party$Same.Party)<-c('Different','Same')

pres.support.party2<-pres.support.party
levels(pres.support.party2$Pres.Party)<-c('Democrat','Republican','Democrat')
levels(pres.support.party2$Party)<-c('Democrat','Republican','Democrat')

pres.support.party2$Same.Party<-pres.support.party2$Pres.Party==pres.support.party2$Party
pres.support.party2$Same.Party<-as.factor(pres.support.party2$Same.Party)
levels(pres.support.party2$Same.Party)<-c('Different','Same')

ggplot(data=pres.support.party2,aes(x=factor(pres.support.party2$President, levels = unique(pres.support.party2$President)),y=Perc.support))+
  geom_boxplot(aes(fill=Same.Party))+labs(x='President',y='% in Support of President',title="Congressional Voting in Support of the President's Position")

ggplot(data=pres.support.party2,aes(x=factor(pres.support.party2$President, levels = unique(pres.support.party2$President)),y=Perc.support))+
  geom_boxplot(aes(fill=Same.Party))+labs(x='President',y='% in Support of President',title="Congressional Voting in Support of the President's Position")+
  facet_wrap(~Office)


ggplot(data=pres.support.party2,aes(x=Year,y=Perc.support))+
  geom_point(aes(color=Same.Party))+labs(x='Year',y='% in Support of President',title="Congressional Voting in Support of the President's Position")+facet_wrap(~Office)


#scale_colour_manual(values = wes.palette(3, "Moonrise3"))

#Party Unity Votes in Congress, 1953-2012 (percentage of all votes)
#Note: Data indicate the percentage of all roll call votes on which a 
#majority of voting Democrats opposed a majority of voting Republicans.
party.unity<-readWorksheet(wb8,sheet = "8-3", startRow=3, endRow=63, endCol=3)
party.unity<-melt(party.unity,id='Year')
names(party.unity)<-c('Year','Office','Perc.unity.votes')
party.unity$Year<-as.numeric(party.unity$Year)

qplot(Year,Perc.unity.votes,data=party.unity,geom='point',group=Office)+geom_path(aes(color=Office))+geom_vline(xintercept=midterm.years[6:21])

#Party Unity Scores in Congressional Voting, 1954-2012 (percent)
#Note: Data show the percentage of members voting with a majority of their
#party on party unity votes. Party unity votes are those roll calls on 
#which a majority of a party votes on one side of the issue and a majority
#of the other party votes on the other side. The percentages are normalized
#to eliminate the effects of absences as follows: party unity = (unity)/(unity+opposition).

unity.by.party<-readWorksheet(wb8,sheet = "8-4", startRow=4, endRow=63, endCol=8)
unity.by.party<-unity.by.party[,-5]
names(unity.by.party)<-c('Year','House.Dems','House.SouthDems','House.Reps','Senate.Dems','Senate.SouthDems','Senate.Reps')
unity.by.party$House.Dems<-as.numeric(unity.by.party$House.Dems)
unity.by.party$House.SouthDems<-as.numeric(unity.by.party$House.SouthDems)
unity.by.party$House.Reps<-as.numeric(unity.by.party$House.Reps)
unity.by.party$Senate.Dems<-as.numeric(unity.by.party$Senate.Dems)
unity.by.party$Senate.SouthDems<-as.numeric(unity.by.party$Senate.SouthDems)
unity.by.party$Senate.Reps<-as.numeric(unity.by.party$Senate.Reps)
unity.by.party<-melt(unity.by.party,id='Year')
unity.by.party<-cbind(unity.by.party,ldply(strsplit(as.character(unity.by.party$variable),'[.]')))
unity.by.party<-unity.by.party[,c(1,4,5,3)]
names(unity.by.party)<-c('Year','Office','Party','Perc.unity')

qplot(Year,Perc.unity,data=subset(unity.by.party,Party!='SouthDems'),geom='point',color=Party)+geom_path()+facet_wrap(~Office)
#Average Ideological Positions of House Party Coalitions, 80th-112th Congresses, 1947-2012  
#scores based on members' voting records. A positive score denotes a conservative ideology, 
#while a negative score denotes a liberal one. Scores closest to zero reflect the most centrist 
#ideologies, while more extreme scores reflect stronger conservative or liberal ideologies.  						
house.ideology<-readWorksheet(wb8,sheet = "8-9", startRow=3, endRow=36, endCol=6)
house.ideology$Congress<-gsub('[(,)]','',house.ideology$Congress)
house.ideology$Congress<-gsub('[a-z][a-z]','',house.ideology$Congress)
house.ideology<-cbind(house.ideology,ldply(strsplit(house.ideology$Congress,'\\s')))
house.ideology<-house.ideology[,c(7,8,2:4)]
names(house.ideology)<-c('Congress','Year.Start',"Entire.chamber","Democrats","Republicans")
house.ideology<-melt(house.ideology,id=c('Congress','Year.Start'))
house.ideology$Congress<-as.numeric(house.ideology$Congress)
house.ideology$Year.Start<-as.numeric(house.ideology$Year.Start)

qplot(Year.Start,value,data=house.ideology,color=variable)
qplot(Year.Start,Democrats,data=house.ideology)+geom_hline(aes(yintercept=mean(Democrats)),color='blue')
qplot(Year.Start,Republicans,data=house.ideology)+geom_hline(aes(yintercept=mean(Republicans)),color='red')

#Average Ideological Positions of Senate Party Coalitions, 80th-112th Congresses, 1947-2012  
#scores based on members' voting records. A positive score denotes a conservative ideology, 
#while a negative score denotes a liberal one. Scores closest to zero reflect the most centrist 
#ideologies, while more extreme scores reflect stronger conservative or liberal ideologies.    					
senate.ideology<-readWorksheet(wb8,sheet = "8-10", startRow=3, endRow=36, endCol=6)
senate.ideology$Congress<-gsub('[(,)]','',senate.ideology$Congress)
senate.ideology$Congress<-gsub('[a-z][a-z]','',senate.ideology$Congress)
senate.ideology<-cbind(senate.ideology,ldply(strsplit(senate.ideology$Congress,'\\s')))
senate.ideology<-senate.ideology[,c(7,8,2:4)]
names(senate.ideology)<-c('Congress','Year.Start',"Entire.chamber","Democrats","Republicans")
senate.ideology<-melt(senate.ideology,id=c('Congress','Year.Start'))
senate.ideology$Congress<-as.numeric(senate.ideology$Congress)
senate.ideology$Year.Start<-as.numeric(senate.ideology$Year.Start)

qplot(Year.Start,value,data=senate.ideology,color=variable)
qplot(Year.Start,Democrats,data=senate.ideology)+geom_hline(aes(yintercept=mean(Democrats)),color='blue')
qplot(Year.Start,Republicans,data=senate.ideology)+geom_hline(aes(yintercept=mean(Republicans)),color='red')
##################################################################################################
##################################################################################################
wb6 <- loadWorkbook("~/Desktop/585/Project/VitalStatisticsFullData/Vital Statistics Chapter 6 - Legislative Productivity in Congress and Workload.xlsx")

#House Workload, 80th-112th Congresses, 1947-2012
house.workload<-readWorksheet(wb6,sheet = "6-1", startRow=3, endRow=36)
house.workload$Congress<-80:112
house.workload<-merge(congresses,house.workload,by='Congress')
house.workload$Year.Start<-as.numeric(house.workload$Year.Start)
house.workload$Year.End<-as.numeric(house.workload$Year.End)
qplot(Year.Start,Time.in.session..Days,data=house.workload,geom='point')+geom_hline(yintercept=254)
qplot(Year.Start,Bills.introduceda,data=house.workload,geom='point')+geom_hline(yintercept=7611)+geom_vline(xintercept=voter.turnout$Year[4:21])
qplot(Year.Start,Bills.passed,data=house.workload,geom='point')+geom_hline(yintercept=1739)+geom_vline(xintercept=midterm.years[6:21])
qplot(Year.Start,Ratio.of.bills.passed.to.bills.introduced,data=house.workload,geom='point')+geom_hline(yintercept=0.22848509)+ylim(0,.3)

#Senate Workload, 80th-112th Congresses, 1947-2012
senate.workload<-readWorksheet(wb6,sheet = "6-2", startRow=3, endRow=36)
senate.workload$Congress<-80:112
senate.workload<-merge(congresses,senate.workload,by='Congress')
senate.workload$Year.Start<-as.numeric(senate.workload$Year.Start)
senate.workload$Year.End<-as.numeric(senate.workload$Year.End)
qplot(Year.Start,Time.in.session..Days,data=senate.workload,geom='point')+geom_hline(yintercept=257)
qplot(Year.Start,Bills.introduceda,data=senate.workload,geom='point')+geom_hline(yintercept=3186)+geom_vline(xintercept=voter.turnout$Year[4:21])
qplot(Year.Start,Bills.passed,data=senate.workload,geom='point')+geom_hline(yintercept=1670)+geom_vline(xintercept=midterm.years[6:21])
qplot(Year.Start,Ratio.of.bills.passed.to.bills.introduced,data=senate.workload,geom='point')+geom_hline(yintercept=0.5241682)+ylim(0,.6)

#Recorded Votes in the House and the Senate, 80th-112th Congresses, 1947-2012
congress.votes<-readWorksheet(wb6,sheet = "6-3", startRow=3, endRow=69,startCol=10,endCol=12)
congress.votes<-melt(congress.votes,id='Year')
names(congress.votes)<-c('Year','Office','No.votes')
congress.votes$No.votes<-as.numeric(sub('[a-z]','',congress.votes$No.votes))
qplot(Year,No.votes,data=congress.votes,geom='point',color=Office)

#Congressional Workload, 80th-112th Congresses, 1947-2012  					
bills.passed<-readWorksheet(wb6,sheet = "6-4", startRow=4, endRow=37, endCol=8)
bills.passed<-bills.passed[,-5]
names(bills.passed)<-c('Congress','Public.BillsEnacted','Public.TotalPages','Public.AvgPPS','Private.BillsEnacted','Private.TotalPages','Private.AvgPPS')
bills.passed$Congress<-80:112
bills.passed<-melt(bills.passed,id='Congress')
bills.passed<-cbind(bills.passed,ldply(strsplit(as.character(bills.passed$variable),'[.]')))
bills.passed<-bills.passed[,-2]
names(bills.passed)<-c('Congress','Count','Bill.Type','Variable')
bills.passed<-bills.passed[,c(1,3,4,2)]

#Vetoes and Overrides, 80th-112th Congresses, 1947-2012
vetoes<-readWorksheet(wb6,sheet = "6-6", startRow=4, endRow=37, endCol=)
vetoes$Congress<-80:112
names(vetoes)<-c('Congress','Total.PresVetoes','Vetoes','PocketVetoes','Overridden','Perc.Vetoes','House.OverrideAttempts','Senate.OverrideAttempts')
vetoes<-merge(congresses,vetoes)
vetoes<-vetoes[,-3]
names(vetoes)<-c('Congress','Year','Total.PresVetoes','Vetoes','PocketVetoes','Overridden','Perc.Vetoes','House.OverrideAttempts','Senate.OverrideAttempts')
vetoes<-merge(pres.yr.party,vetoes)
qplot(Year,Total.PresVetoes,data=vetoes,color=Party)
