#######2014 candidate summary
candidates14<-read.csv('~/Desktop/585/Project/CandidateSummaryAction.csv')
names(candidates14)
candidates14<-candidates14[,-c(1,2)]

library(dplyr)

candidate_info14<-candidates14[,1:6]
names(candidate_info14)<-c("Name","Office","State","District","Party","Seat_Status")
candidate_counts<-group_by(candidate_info14,State,District)%.%arrange(State,District,Office)%.%summarise(n=length(Name))

candidate_info14<-group_by(candidate_info14,State,District)%.%arrange(State,District,Office)


#########presidents of the USA
library(XML)
url <- "http://www.enchantedlearning.com/history/us/pres/list.shtml"
tables <- readHTMLTable(url)
head(tables[[1]])
presidents<-tables[[8]]
vps<-presidents[,3:4]
pres<-presidents[,1:3]
pres<-pres[,c(1,3,2)]
redo.pres<-ldply(strsplit(as.character(pres$President),'[0-9][.]'))
redo.pres<-redo.pres[,-1]
ldply(strsplit(as.character(redo.pres),' \\('))
sub('-',':',pres[,2])
