library(reshape)
library(plyr)
library(XLConnect)
wb <- loadWorkbook("/Users/samanthatyner/Desktop/585/VitalStatisticsFullData/Vital Statistics Chapter 1- Demographics of Members of Congress.xlsx")
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

seats.allotted<-merge(appor.seats.ste,states[,c(1,3)])
seats.allotted<-seats.allotted[,c(1,4,2,3)]
seats.allotted<-sort_df(seats.allotted,vars=c('State','Census.Year'))
seats.allotted$No.reps<-as.numeric(sub('[a-z]','',seats.allotted$No.reps))
seats.allotted[101,4]<-NA

#sheet 2: Democratic Party Strength in the House, by Region, 69th-113th Congresses, 1925-2014
dem.strg = readWorksheet(wb, sheet = "1-2",startRow=4,endRow=35)
names(dem.strg)<- c('Region',69,75,81,87,93,96,97,98,100:113)
which(is.na(dem.strg$Congress))
dem.strg<- dem.strg[-which(is.na(dem.strg$Region)),]
dem.strg$Region<-as.factor(dem.strg$Region)
dem.strg.perc<-subset(dem.strg,Region=="   Percent")
dem.strg.perc$Region<-unique(appor.seats.reg$Region)
dem.strg.perc<-melt(dem.strg.perc,id="Region")
names(dem.strg.perc)<-c("Region","Congress","Perc.dems")
dem.strg.perc$Perc.dems<-as.numeric(dem.strg.perc$Perc.dems)

dem.strg.cnt<-subset(dem.strg,Region=="   Seats")
dem.strg.cnt$Region<-unique(appor.seats.reg$Region)
dem.strg.cnt<-melt(dem.strg.cnt,id="Region")
names(dem.strg.cnt)<-c("Region","Congress","No.dems")
dem.strg.cnt$No.dems<-sub('[a-z]|[A-Z]','',dem.strg.cnt$No.dems)
dem.strg.cnt$No.dems<-as.numeric(dem.strg.cnt$No.dems)

ddply(dem.strg.cnt,.(Region,Congress),summarise,sum(No.dems))

#sheet 3: Democratic and Republican Seats in the House, by Region, 69th-113th Congresses, 1925-2014
parties = readWorksheet(wb, sheet = "1-3",startRow=5,endRow=36)
parties<-parties[-which(is.na(parties$Region)),]

dems<-parties[,c(1,grep('D',names(parties)))]
names(dems)<-c("Region",69,75,81,87,93,96:98,101:113)
dems$Region<-as.factor(dems$Region)
dems.perc<-subset(dems,Region=="   Percent")
dems.perc$Region<-unique(appor.seats.reg$Region)
dems.perc<-melt(dems.perc,id="Region")
names(dems.perc)<-c("Region","Congress","Perc.dems")

dems.cnt<-subset(dems,Region=="   Seats")
dems.cnt$Region<-unique(appor.seats.reg$Region)
dems.cnt<-melt(dems.cnt,id="Region")
names(dems.cnt)<-c("Region","Congress","No.dems")

dems.clean<-merge(dems.cnt,dems.perc)

reps<-parties[,grep('R',names(parties))]
names(reps)<-c("Region",69,75,81,87,93,96:98,101:113)
reps$Region<-as.factor(reps$Region)
reps.perc<-subset(reps,Region=="   Percent")
reps.perc$Region<-unique(appor.seats.reg$Region)
reps.perc<-melt(reps.perc,id="Region")
names(reps.perc)<-c("Region","Congress","Perc.reps")

reps.cnt<-subset(reps,Region=="   Seats")
reps.cnt$Region<-unique(appor.seats.reg$Region)
reps.cnt<-melt(reps.cnt,id="Region")
names(reps.cnt)<-c("Region","Congress","No.reps")

reps.clean<-merge(reps.cnt,reps.perc)

parties.clean<-merge(dems.clean,reps.clean)
parties.clean$No.Total<-parties.clean$No.dems+parties.clean$No.reps
parties.clean<-sort_df(parties.clean,var='Congress') #in parties.clean, No.reps is # of republican representatives

#sheet 5:Democratic and Republican Seats in the Senate, by Region, 69th-113th Congresses, 1925-2014
parties.sen = readWorksheet(wb, sheet = "1-5",startRow=5,endRow=36)
parties.sen<-parties.sen[-which(is.na(parties.sen$Region)),]

dems.sen<-parties.sen[,c(1,grep('D',names(parties.sen)))]
names(dems.sen)<-c("Region",69,75,81,87,93,97,101:113)
dems.sen$Region<-as.factor(dems.sen$Region)
dems.sen.perc<-subset(dems.sen,Region=="   Percent")
dems.sen.perc$Region<-unique(appor.seats.reg$Region)
dems.sen.perc<-melt(dems.sen.perc,id="Region")
names(dems.sen.perc)<-c("Region","Congress","Perc.dems.sen")

dems.sen.cnt<-subset(dems.sen,Region=="   Seats")
dems.sen.cnt$Region<-unique(appor.seats.reg$Region)
dems.sen.cnt<-melt(dems.sen.cnt,id="Region")
names(dems.sen.cnt)<-c("Region","Congress","No.dems.sen")

dems.sen.clean<-merge(dems.sen.cnt,dems.sen.perc)

reps.sen<-parties.sen[,grep('R',names(parties.sen))]
names(reps.sen)<-c("Region",69,75,81,87,93,97,101:113)
reps.sen$Region<-as.factor(reps.sen$Region)
reps.sen.perc<-subset(reps.sen,Region=="   Percent")
reps.sen.perc$Region<-unique(appor.seats.reg$Region)
reps.sen.perc<-melt(reps.sen.perc,id="Region")
names(reps.sen.perc)<-c("Region","Congress","Perc.reps.sen")
reps.sen.perc$Perc.reps.sen<-sub('[a-z]|[A-Z]','',reps.sen.perc$Perc.reps.sen)

reps.sen.cnt<-subset(reps.sen,Region=="   Seats")
reps.sen.cnt$Region<-unique(appor.seats.reg$Region)
reps.sen.cnt<-melt(reps.sen.cnt,id="Region")
names(reps.sen.cnt)<-c("Region","Congress","No.reps.sen")
reps.sen.cnt$No.reps.sen<-sub('[a-z]|[A-Z]','',reps.sen.cnt$No.reps.sen)
reps.sen.clean<-merge(reps.sen.cnt,reps.sen.perc)

parties.sen.clean<-merge(dems.sen.clean,reps.sen.clean)
parties.sen.clean$No.reps.sen<-as.numeric(parties.sen.clean$No.reps.sen)
parties.sen.clean$Perc.reps.sen<-as.numeric(parties.sen.clean$Perc.reps.sen)
parties.sen.clean$No.Total.sen<-parties.sen.clean$No.dems.sen+parties.sen.clean$No.reps.sen
parties.sen.clean<-sort_df(parties.sen.clean,var='Congress')

#combining sheets 4 and 5
parties.all<-merge(parties.clean,parties.sen.clean)
names(parties.all)<-c("Region","Congress","House.Dems","House.Dems.Perc","House.Reps","House.Reps.Perc","House.Total","Sen.Dems","Sen.Dems.Perc","Sen.Reps","Sen.Reps.Perc","Sen.Total")


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

senior.cnt<-subset(seniority,Congress=="Seats")
senior.cnt$Congress<-unique(seniority$Congress)[-c(2,3)]
senior.cnt$Congress<-ldply(strsplit(senior.cnt$Congress,'[a-z][a-z]'))$V1
names(senior.cnt)<-c("Congress","1.term","2.terms","3.terms","4-6.terms", "7-9.terms", "10+.terms","Total")
senior.cnt$Total<-sub('[a-z]','',senior.cnt$Total)

seniority.clean<-merge(senior.cnt,senior.perc)
seniority.clean$Congress<-as.numeric(seniority.clean$Congress)
seniority.clean$Total<-as.numeric(seniority.clean$Total)

seniority.clean<-seniority.clean[order(seniority.clean$Congress),]

#sheet 7: Seniority of Senators, 1953 - 2014
senior.senate = readWorksheet(wb, sheet = "1-7",startRow=4,endRow=35)
senior.senate$Col2<-sub('\\)','',sub('\\(','',senior.senate$Col2))
senior.senate<-senior.senate[,-c(8,9)]
names(senior.senate)<-c("Congress","Year.Start","Less6years","7-12years","13-18years","More19years",'Total')
senior.senate$No.freshman<-ldply(strsplit(senior.senate$Less6years," "))$V2
senior.senate$Less6years<-ldply(strsplit(senior.senate$Less6years," "))$V1
senior.senate<-senior.senate[,c(1,2,8,3,4,5,6,7)]
senior.senate$No.freshman<-sub('[a-z]','',sub('\\)','',sub('\\(','',senior.senate$No.freshman)))
senior.senate$Congress<-sub('[a-z][a-z]','',senior.senate$Congress)

senior.senate.clean<-senior.senate[,-2]

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

prior.occ.congress.party<-merge(priors.by.party.house,priors.by.party.sen)
prior.occ.congress.party$Congress<-as.numeric(as.character(prior.occ.congress.party$Congress))
prior.occ.congress.party<-prior.occ.congress.party[order(prior.occ.congress.party$Congress),]

prior.occ.congress.party<-sort_df(prior.occ.congress.party, vars=c('Congress','Occupation'))
prior.occ.congress.party<-prior.occ.congress.party[,c(1,3,2,4,5)]

#sheet 14: Religious Affiliations of Representatives, 89th - 113th Congresses, 1965 - 2014
religion.house<-readWorksheet(wb, sheet = "1-14",startRow=4,endRow=18)
religion.house <- religion.house[-which(is.na(religion.house$Col1)),]
religion.house<-melt(religion.house,id='Col1')
religion.house<-religion.house[-which(as.factor(religion.house$Col1)=='Protestant'),]
religion.house$value<-sub('[a-z]','',religion.house$value)
religion.house$value<-as.numeric(religion.house$value)
religion.house$value[is.na(religion.house$value)]<-0
names(religion.house)<-c('Religion','Party','No.reps')

religion.house.dem<-religion.house[grep('D',religion.house$Party),]
religion.house.dem$Congress<-sort(rep(c(89,90,92:97,99,101:113),10))
religion.house.dem$Party<-'Democrat'

religion.house.rep<-religion.house[grep('R',religion.house$Party),]
religion.house.rep$Congress<-sort(rep(c(89,90,92:97,99,101:113),10))
religion.house.rep$Party<-'Republican'

religion.house<-rbind(religion.house.rep,religion.house.dem)
religion.house<-sort_df(religion.house, vars=c('Congress','Religion'))
religion.house<-religion.house[,c(4,1,2,3)]

#sheet 15: Religious Affiliations of Senators, 89th - 113th Congresses, 1965 - 2014
religion.sen<-readWorksheet(wb, sheet = "1-15",startRow=4,endRow=18)
religion.sen <- religion.sen[-which(is.na(religion.sen$Col1)),]
religion.sen<-melt(religion.sen,id='Col1')
religion.sen<-religion.sen[-which(as.factor(religion.sen$Col1)=='Protestant'),]
religion.sen$value<-sub('[a-z]','',religion.sen$value)
religion.sen$value<-as.numeric(religion.sen$value)
religion.sen$value[is.na(religion.sen$value)]<-0
names(religion.sen)<-c('Religion','Party','No.sens')

religion.sen.dem<-religion.sen[grep('D',religion.sen$Party),]
religion.sen.dem$Congress<-sort(rep(c(89,90,92:97,99,101:113),10))
religion.sen.dem$Party<-'Democrat'

religion.sen.rep<-religion.sen[grep('R',religion.sen$Party),]
religion.sen.rep$Congress<-sort(rep(c(89,90,92:97,99,101:113),10))
religion.sen.rep$Party<-'Republican'

religion.sen<-rbind(religion.sen.rep,religion.sen.dem)
religion.sen<-sort_df(religion.sen, vars=c('Congress','Religion'))
religion.sen<-religion.sen[,c(4,1,2,3)]

religions.congress<-merge(religion.house,religion.sen)
religions.congress<-religions.congress[order(religions.congress$Congress),]
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

diversity<-merge(merge(afr.am,hisp.am),women)
diversity$Congress<-as.numeric(diversity$Congress)
diversity$No.Afr.Am<-as.numeric(as.character(diversity$No.Afr.Am))
diversity$No.Hisp.Am<-as.numeric(as.character(diversity$No.Hisp.Am))

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
names(summary.party.senate)<-c('Congress','Party','No.sens')
summary.party.senate<-summary.party.senate[order(as.numeric(summary.party.senate$Congress)),]

summary.party.house$Democrats<-as.numeric(sub('[a-z]','',summary.party.house$Democrats))
summary.party.house<-melt(summary.party.house,id='Congress')
names(summary.party.house)<-c('Congress','Party','No.reps')
summary.party.house<-summary.party.house[order(as.numeric(summary.party.house$Congress)),]

summary.party<-merge(summary.party.house,summary.party.senate)
summary.party<-summary.party[order(as.numeric(summary.party$Congress)),]

#####################################################################################
wb2 <- loadWorkbook("/Users/samanthatyner/Desktop/585/VitalStatisticsFullData/Vital Statistics Chapter 2 - Congressional Elections.xlsx")


