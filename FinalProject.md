So, just how useless is the U.S. Congress? 
========================================================
Samantha Tyner
-------------------------------------------------------
### 25 Apr 2014
Also see the slide version of this on my website, [http://sctyner.github.io/projectwriteup.html#/](http://sctyner.github.io/projectwriteup.html#/)

Motivation
========================================================

I chose this project because I, not unlike most Americans, am extremely frustrated with the U.S. Congress, and when I happened upon [this blog post](http://wonkviz.tumblr.com/post/74277939647/connecting-the-dots-between-partisanship-and-gridlock) that cited the [Vital Statistics on Congress](http://www.brookings.edu/research/reports/2013/07/vital-statistics-congress-mann-ornstein) from the Brookings Institute, I decided to investigate the history of congress as it relates to partisanship and congressional productivity. I also want to look at the field of candidates for this year's election, available from the [FEC](http://www.fec.gov/data/CandidateSummary.do) to see if there's any hope for improved productivity in 2015-2016. 

See also: [Why Does Congress Suck?](http://youtu.be/V0CvmK0dVcI)

Disclaimer
========================================================
I'm a registered independent! Here's my proof! 
![Alt text](IMAG0675.jpg)

Libraries
========================================================

```r
library(plyr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, desc, failwith, id, mutate, summarise
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(reshape)
```

```
## 
## Attaching package: 'reshape'
## 
## The following objects are masked from 'package:plyr':
## 
##     rename, round_any
```

```r
library(XLConnect)
```

```
## XLConnect 0.2-7 by Mirai Solutions GmbH
## http://www.mirai-solutions.com ,
## http://miraisolutions.wordpress.com
```

```r
library(maps)
library(ggplot2)
```


Data
========================================================
[Brookings Data](http://www.brookings.edu/~/media/Research/Files/Reports/2013/07/vital%20statistics%20congress%20mann%20ornstein/Vital%20Statistics%20Full%20Data%20Set.pdf): 
--------------------------------------------------------
Either PDF (yikes!) or Excel format.
  - Excel: better, but 10+ sheets per file
  - Use the XLConnect package to read in the data



```r
wb <- loadWorkbook("~/Desktop/585/Project/VitalStatisticsFullData/Vital Statistics Chapter 1- Demographics of Members of Congress.xlsx")
seniority = readWorksheet(wb, sheet = "1-6", startRow = 4, endRow = 127)
head(seniority)
```

```
##      Congress X1.term X2.terms X3.terms X1...3.terms X4...6.terms
## 1 83rd (1953)      NA       NA       NA           NA           NA
## 2     Percent   18.71    16.86    14.78        50.35        27.02
## 3       Seats   81.00    73.00    64.00       218.00       117.00
## 4        <NA>      NA       NA       NA           NA           NA
## 5 84th (1955)      NA       NA       NA           NA           NA
## 6     Percent   13.10    16.78    14.48        44.37        27.36
##   X7...9.terms X10...terms Total Mean.term Median.term
## 1           NA          NA  <NA>        NA          NA
## 2        13.39       9.238  <NA>       4.5           3
## 3        58.00      40.000  433a        NA          NA
## 4           NA          NA  <NA>        NA          NA
## 5           NA          NA  <NA>        NA          NA
## 6        16.78      11.494  <NA>       5.0           4
```


The Brookings data have almost all of the most common problems with messy data that are listed in Wickham's "Tidy Data" paper, and some even have two or more in one data table! As a reminder, these problems are:
- column headers are values, not variable names
- multiple variables are stored in one column
- variables are stored in both rows and columns
- multiple types of observational units are stored in the same table 
- a single observational unit is stored in multiple tables

Because of the inherent messiness of the data, the bulk of the work I did on this project involved cleaning the data and formatting it for visualization. I didn't end up using as much of it as I cleaned because it didn't all fit together as well as I had hoped. I also didn't get to the Shiny app because the data cleaning took so long.  I decided to do my project in markdown because I wanted to do something I'd never done before since the Shiny app didn't happen. 


[FEC](http://www.fec.gov/data/CandidateSummary.do)
------------------------------------------------------------------------------
- I tried using XML to read in this data table, but doesn't work because the website is mostly Javascript, and it queries a database every time it is loaded, so there is no HTML table to be read!
- I downloaded a CSV file instead, which I feel is kind of a cop-out. I really wanted to use the XML package.
- The format and reading it in are fairly straightforward compared to the Brookings data.


```r
candidates14 <- read.csv("~/Desktop/585/Project/CandidateSummaryAction.csv")
candidates14 <- candidates14[, 3:8]
names(candidates14) <- c("Name", "Office", "State", "District", "Party", "Status")
head(candidates14, 5)
```

```
##                       Name Office State District Party     Status
## 1             AALDERS, TIM      H    UT        4   IAP       OPEN
## 2 ABBOTT, RICHARD ALLEN IV      H    MI        3   DEM CHALLENGER
## 3          ABELER, JAMES J      S    MN        0   REP CHALLENGER
## 4                  ABRAHAM      H    UT        3   IAP CHALLENGER
## 5          ABRAHAMS, KEVAN      H    NY        4   DEM       OPEN
```

Data Cleaning
========================================================
In addition to XLConnect, I also relied heavily on the reshape packages and the plyr packages to do my data cleaning. 


Messy Data Example
--------------------------------------------------------
Defeated House Incumbents, 1946-2012

```r
wb2 <- loadWorkbook("/Users/samanthatyner/Desktop/585/Project/VitalStatisticsFullData/Vital Statistics Chapter 2 - Congressional Elections.xlsx")
defeat.incumb2 <- readWorksheet(wb2, sheet = "2-10", startRow = 4, endRow = 96, 
    endCol = 11)
head(defeat.incumb2, 4)
```

```
##   Election      Party Incumbents.lost Average.terms X1 X2 X3 X1...3 X4...6
## 1     1946   Democrat              62           2.7 35  5  4     44     11
## 2     <NA> Republican               7           3.6  2  0  1      3      3
## 3     <NA>      Total              69           2.8 37  5  5     47     14
## 4     <NA>       <NA>              NA            NA NA NA NA     NA     NA
##   X7...9 X10.
## 1      5    2
## 2      1    0
## 3      6    2
## 4     NA   NA
```

This is an example of columns headers being values instead of variables. 

After some cleaning...

```r
defeat.incumb <- readWorksheet(wb2, sheet = "2-10", startRow = 4, endRow = 96, 
    endCol = 11)
defeat.incumb <- defeat.incumb[-which(is.na(as.factor(defeat.incumb$Party))), 
    ]
defeat.incumb$Party <- sub("Democrat[a-z]", "Democrat", defeat.incumb$Party)
defeat.incumb$Party <- sub("Republican[a-z]", "Republican", defeat.incumb$Party)
defeat.incumb$Party <- as.factor(defeat.incumb$Party)
defeat.incumb <- defeat.incumb[, -8]
names(defeat.incumb) <- c("Year", "Party", "Incumbents.lost", "Avg.terms", "1term", 
    "2terms", "3terms", "4-6terms", "7-9terms", "10+terms")
years <- as.numeric(as.character(levels(as.factor(defeat.incumb$Year[-which(is.na(defeat.incumb$Year))]))))
years <- c(years, 2012)
defeat.incumb$Year <- sort(rep(years, 3))
defeat.incumb <- defeat.incumb[-which(defeat.incumb$Party == "Total"), ]
defeat.incumb <- melt(defeat.incumb, id = c("Year", "Party", "Incumbents.lost", 
    "Avg.terms"))
defeat.incumb <- sort_df(defeat.incumb, vars = c("Year", "variable"))
defeat.incumb <- defeat.incumb[, c(1, 2, 5, 6, 3, 4)]
names(defeat.incumb) <- c("Year", "Party", "Terms", "In.lost", "Tot.party.loss", 
    "Avg.terms")
levels(defeat.incumb$Terms) <- c("1", "2", "3", "4-6", "7-9", "10+")
head(defeat.incumb, 15)
```

```
##     Year      Party Terms In.lost Tot.party.loss Avg.terms
## 1   1946   Democrat     1      35             62       2.7
## 2   1946 Republican     1       2              7       3.6
## 47  1946   Democrat     2       5             62       2.7
## 48  1946 Republican     2       0              7       3.6
## 93  1946   Democrat     3       4             62       2.7
## 94  1946 Republican     3       1              7       3.6
## 139 1946   Democrat   4-6      11             62       2.7
## 140 1946 Republican   4-6       3              7       3.6
## 185 1946   Democrat   7-9       5             62       2.7
## 186 1946 Republican   7-9       1              7       3.6
## 231 1946   Democrat   10+       2             62       2.7
## 232 1946 Republican   10+       0              7       3.6
## 3   1948   Democrat     1       4              9       2.7
## 4   1948 Republican     1      41             73       2.2
## 49  1948   Democrat     2       1              9       2.7
```


Second Messy Data Example
-----------------------------------------------------------
Democratic and Republican Seats in the Senate by Region

```r
parties.sen = readWorksheet(wb, sheet = "1-5", startRow = 5, endRow = 36)
head(parties.sen[, c(1:10)], 7)
```

```
##    Region     D     R Col4   D.1 R.1 Col7   D.2   R.2 Col10
## 1   South    NA    NA   NA    NA  NA   NA    NA    NA    NA
## 2 Percent 53.66 0.000   NA 28.95   0   NA 40.74 0.000    NA
## 3   Seats 22.00 0.000   NA 22.00   0   NA 22.00 0.000    NA
## 4    <NA>    NA    NA   NA    NA  NA   NA    NA    NA    NA
## 5  Border    NA    NA   NA    NA  NA   NA    NA    NA    NA
## 6 Percent 12.20 9.259   NA 13.16   0   NA 14.81 4.762    NA
## 7   Seats  5.00 5.000   NA 10.00   0   NA  8.00 2.000    NA
```


Column headers are values (D or R), and multiple variables are stored in one column (region, percent, and seat count in one column). This table (and many others) has the additional problem of congress and year (two more variables) stored on top of the two D and R columns, as well as a lot of empty columns and rows. Silly Excel! 

After some cleaning...

```r
parties.sen <- parties.sen[-which(is.na(parties.sen$Region)), ]

dems.sen <- parties.sen[, c(1, grep("D", names(parties.sen)))]
names(dems.sen) <- c("Region", 69, 75, 81, 87, 93, 97, 101:113)
dems.sen$Region <- as.factor(dems.sen$Region)
dems.sen.perc <- subset(dems.sen, Region == "Percent")
dems.sen.perc$Region <- c("South", "Border", "New England", "Mid-Atlantic", 
    "Midwest", "Plains", "Rocky Mountains", "Pacific Coast")
dems.sen.perc <- melt(dems.sen.perc, id = "Region")
names(dems.sen.perc) <- c("Region", "Congress", "Perc.alldems.sen")

dems.sen.cnt <- subset(dems.sen, Region == "Seats")
dems.sen.cnt$Region <- c("South", "Border", "New England", "Mid-Atlantic", "Midwest", 
    "Plains", "Rocky Mountains", "Pacific Coast")
dems.sen.cnt <- melt(dems.sen.cnt, id = "Region")
names(dems.sen.cnt) <- c("Region", "Congress", "No.dems.sen")

dems.sen.clean <- merge(dems.sen.cnt, dems.sen.perc)

reps.sen <- parties.sen[, grep("R", names(parties.sen))]
names(reps.sen) <- c("Region", 69, 75, 81, 87, 93, 97, 101:113)
reps.sen$Region <- as.factor(reps.sen$Region)
reps.sen.perc <- subset(reps.sen, Region == "Percent")
reps.sen.perc$Region <- c("South", "Border", "New England", "Mid-Atlantic", 
    "Midwest", "Plains", "Rocky Mountains", "Pacific Coast")
reps.sen.perc <- melt(reps.sen.perc, id = "Region")
names(reps.sen.perc) <- c("Region", "Congress", "Perc.allreps.sen")
reps.sen.perc$Perc.allreps.sen <- sub("[a-z]|[A-Z]", "", reps.sen.perc$Perc.allreps.sen)

reps.sen.cnt <- subset(reps.sen, Region == "Seats")
reps.sen.cnt$Region <- c("South", "Border", "New England", "Mid-Atlantic", "Midwest", 
    "Plains", "Rocky Mountains", "Pacific Coast")
reps.sen.cnt <- melt(reps.sen.cnt, id = "Region")
names(reps.sen.cnt) <- c("Region", "Congress", "No.reps.sen")
reps.sen.cnt$No.reps.sen <- sub("[a-z]|[A-Z]", "", reps.sen.cnt$No.reps.sen)
reps.sen.clean <- merge(reps.sen.cnt, reps.sen.perc)

parties.sen.clean <- merge(dems.sen.clean, reps.sen.clean)
parties.sen.clean$No.reps.sen <- as.numeric(parties.sen.clean$No.reps.sen)
parties.sen.clean$Perc.allreps.sen <- as.numeric(parties.sen.clean$Perc.allreps.sen)
parties.sen.clean$No.Total.sen <- parties.sen.clean$No.dems.sen + parties.sen.clean$No.reps.sen
parties.sen.clean <- sort_df(parties.sen.clean, var = "Congress")
parties.sen.clean$Office <- "Senate"
names(parties.sen.clean) <- names(parties.clean)
```

```
## Error: object 'parties.clean' not found
```

```r
head(parties.sen.clean)
```

```
##            Region Congress No.dems.sen Perc.alldems.sen No.reps.sen
## 14         Border       69           5           12.195           5
## 33   Mid-Atlantic       69           3            7.317           5
## 52        Midwest       69           1            2.439           9
## 71    New England       69           1            2.439          11
## 90  Pacific Coast       69           1            2.439           5
## 109        Plains       69           0            0.000          11
##     Perc.allreps.sen No.Total.sen Office
## 14             9.259           10 Senate
## 33             9.259            8 Senate
## 52            16.667           10 Senate
## 71            20.370           12 Senate
## 90             9.259            6 Senate
## 109           20.370           11 Senate
```


Congressional Demographics
==========================================================
First, I look at how congress has changed over time.
- House seats per state after census
- Strength of the democratic party by state
- Diversity of congresspeople

House Representatives per State (Note: This section looks better in the slides!)
---------------------------------------------------------

```r
appor.seats = readWorksheet(wb, sheet = "1-1", startRow = 4, endRow = 69)
names(appor.seats) <- c("Region.and.State", 1910, 1920, 1930, 1940, 1950, 1960, 
    1970, 1980, 1990, 2000, 2010)
appor.seats <- appor.seats[, -3]
appor.seats$Region.and.State <- as.factor(appor.seats$Region.and.State)
appor.seats <- appor.seats[-which(is.na(appor.seats$Region.and.State)), ]
appor.seats[, 2] <- as.numeric(appor.seats[, 2])
appor.seats[56, 2] <- 1
appor.seats.state <- appor.seats[-which(appor.seats$Region.and.State %in% c("South", 
    "Border", "New England", "Mid-Atlantic", "Midwest", "Plains", "Rocky Mountains", 
    "Pacific Coast")), ]
appor.seats.state <- melt(appor.seats.state, id = "Region.and.State")
appor.seats.state$value <- as.numeric(sub("[a-z]", "", appor.seats.state$value))
names(appor.seats.state) <- c("State", "Census.Year", "No.reps")
data(state)
states.map <- map_data("state")
appor.for.map <- appor.seats.state
appor.for.map$State <- tolower(appor.for.map$State)
names(appor.for.map)[1] <- "region"
states.seats <- merge(states.map, appor.for.map, by = "region", sort = F)
states.seats <- sort_df(states.seats, vars = c("group", "order"))
ggplot() + labs(title = "House Members per State after 1910 Census") + geom_polygon(data = subset(states.seats, 
    Census.Year == 1910), aes(x = long, y = lat, group = group, fill = No.reps)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(1, 53), 
    low = "grey", high = "#B8078C")
```

![plot of chunk reps_per_state](figure/reps_per_state1.png) 

```r

ggplot() + labs(title = "House Members per State after 1930 Census") + geom_polygon(data = subset(states.seats, 
    Census.Year == 1930), aes(x = long, y = lat, group = group, fill = No.reps)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(1, 53), 
    low = "grey", high = "#B8078C")
```

![plot of chunk reps_per_state](figure/reps_per_state2.png) 

```r

ggplot() + labs(title = "House Members per State after 1940 Census") + geom_polygon(data = subset(states.seats, 
    Census.Year == 1940), aes(x = long, y = lat, group = group, fill = No.reps)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(1, 53), 
    low = "grey", high = "#B8078C")
```

![plot of chunk reps_per_state](figure/reps_per_state3.png) 

```r

ggplot() + labs(title = "House Members per State after 1950 Census") + geom_polygon(data = subset(states.seats, 
    Census.Year == 1950), aes(x = long, y = lat, group = group, fill = No.reps)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(1, 53), 
    low = "grey", high = "#B8078C")
```

![plot of chunk reps_per_state](figure/reps_per_state4.png) 

```r

ggplot() + labs(title = "House Members per State after 1960 Census") + geom_polygon(data = subset(states.seats, 
    Census.Year == 1960), aes(x = long, y = lat, group = group, fill = No.reps)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(1, 53), 
    low = "grey", high = "#B8078C")
```

![plot of chunk reps_per_state](figure/reps_per_state5.png) 

```r

ggplot() + labs(title = "House Members per State after 1970 Census") + geom_polygon(data = subset(states.seats, 
    Census.Year == 1970), aes(x = long, y = lat, group = group, fill = No.reps)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(1, 53), 
    low = "grey", high = "#B8078C")
```

![plot of chunk reps_per_state](figure/reps_per_state6.png) 

```r

ggplot() + labs(title = "House Members per State after 1980 Census") + geom_polygon(data = subset(states.seats, 
    Census.Year == 1980), aes(x = long, y = lat, group = group, fill = No.reps)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(1, 53), 
    low = "grey", high = "#B8078C")
```

![plot of chunk reps_per_state](figure/reps_per_state7.png) 

```r

ggplot() + labs(title = "House Members per State after 1990 Census") + geom_polygon(data = subset(states.seats, 
    Census.Year == 1990), aes(x = long, y = lat, group = group, fill = No.reps)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(1, 53), 
    low = "grey", high = "#B8078C")
```

![plot of chunk reps_per_state](figure/reps_per_state8.png) 

```r

ggplot() + labs(title = "House Members per State after 2000 Census") + geom_polygon(data = subset(states.seats, 
    Census.Year == 2000), aes(x = long, y = lat, group = group, fill = No.reps)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(1, 53), 
    low = "grey", high = "#B8078C")
```

![plot of chunk reps_per_state](figure/reps_per_state9.png) 

```r

ggplot() + labs(title = "House Members per State after 2010 Census") + geom_polygon(data = subset(states.seats, 
    Census.Year == 2010), aes(x = long, y = lat, group = group, fill = No.reps)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(1, 53), 
    low = "grey", high = "#B8078C")
```

![plot of chunk reps_per_state](figure/reps_per_state10.png) 


Summary: House Representatives per State
----------------------------------------------------------------------
- The number of representatives spread west as time passed.   
- There is a large number concentrated into California, Texas, Flordia, and away from New York and New England.

Regional Democratic Strength (Note: This section looks better in the slides!)
====================================================================================

```r
states <- read.csv("/Users/samanthatyner/Desktop/585/R files and Data/Lab Week 2/states.csv")
states$Region <- NA
states$Region[states$State %in% c("Alabama", "Arkansas", "Florida", "Georgia", 
    "Louisiana", "Mississippi", "North Carolina", "South Carolina", "Tennessee", 
    "Texas", "Virginia")] <- "South"
states$Region[states$State %in% c("Kentucky", "Maryland", "Missouri", "Oklahoma", 
    "West Virginia")] <- "Border"
states$Region[states$State %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", 
    "Rhode Island", "Vermont")] <- "New England"
states$Region[states$State %in% c("Delaware", "New Jersey", "New York", "Pennsylvania")] <- "Mid-Atlantic"
states$Region[states$State %in% c("Illinois", "Indiana", "Michigan", "Ohio", 
    "Wisconsin")] <- "Midwest"
states$Region[states$State %in% c("Iowa", "Kansas", "Minnesota", "Nebraska", 
    "North Dakota", "South Dakota")] <- "Plains"
states$Region[states$State %in% c("Arizona", "Colorado", "Idaho", "Montana", 
    "Nevada", "New Mexico", "Utah", "Wyoming")] <- "Rocky Mountains"
states$Region[states$State %in% c("Alaska", "California", "Hawaii", "Oregon", 
    "Washington")] <- "Pacific Coast"
states$Region <- as.factor(states$Region)
states <- states[-9, ]
states$region <- tolower(states$State)

dem.strg = readWorksheet(wb, sheet = "1-2", startRow = 4, endRow = 35)
names(dem.strg) <- c("Region", 69, 75, 81, 87, 93, 96, 97, 98, 100:113)
dem.strg <- dem.strg[-which(is.na(dem.strg$Region)), ]
dem.strg$Region <- as.factor(dem.strg$Region)
dem.strg.perc <- subset(dem.strg, Region == "Percent")
dem.strg.perc$Region <- c("South", "Border", "New England", "Mid-Atlantic", 
    "Midwest", "Plains", "Rocky Mountains", "Pacific Coast")
dem.strg.perc <- melt(dem.strg.perc, id = "Region")
names(dem.strg.perc) <- c("Region", "Congress", "Perc.dems")
dem.strg.perc$Perc.dems <- as.numeric(dem.strg.perc$Perc.dems)

dem.reg.strg <- merge(merge(states.map, states[, 2:4], by = "region"), dem.strg.perc, 
    by = "Region")
dem.reg.strg <- sort_df(dem.reg.strg, vars = c("group", "order"))

ggplot() + labs(title = "Regional Democratic Strength, 69th Congress") + geom_polygon(data = subset(dem.reg.strg, 
    Congress == 69), aes(x = long, y = lat, group = group, fill = Perc.dems)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(0, 100), 
    low = "grey", high = "#2B07B8")
```

![plot of chunk reg_dem_strg](figure/reg_dem_strg1.png) 

```r

ggplot() + labs(title = "Regional Democratic Strength, 75th Congress") + geom_polygon(data = subset(dem.reg.strg, 
    Congress == 75), aes(x = long, y = lat, group = group, fill = Perc.dems)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(0, 100), 
    low = "grey", high = "#2B07B8")
```

![plot of chunk reg_dem_strg](figure/reg_dem_strg2.png) 

```r

ggplot() + labs(title = "Regional Democratic Strength, 81st Congress") + geom_polygon(data = subset(dem.reg.strg, 
    Congress == 81), aes(x = long, y = lat, group = group, fill = Perc.dems)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(0, 100), 
    low = "grey", high = "#2B07B8")
```

![plot of chunk reg_dem_strg](figure/reg_dem_strg3.png) 

```r

ggplot() + labs(title = "Regional Democratic Strength, 87th Congress") + geom_polygon(data = subset(dem.reg.strg, 
    Congress == 87), aes(x = long, y = lat, group = group, fill = Perc.dems)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(0, 100), 
    low = "grey", high = "#2B07B8")
```

![plot of chunk reg_dem_strg](figure/reg_dem_strg4.png) 

```r

ggplot() + labs(title = "Regional Democratic Strength, 93rd Congress") + geom_polygon(data = subset(dem.reg.strg, 
    Congress == 93), aes(x = long, y = lat, group = group, fill = Perc.dems)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(0, 100), 
    low = "grey", high = "#2B07B8")
```

![plot of chunk reg_dem_strg](figure/reg_dem_strg5.png) 

```r

ggplot() + labs(title = "Regional Democratic Strength, 96th Congress") + geom_polygon(data = subset(dem.reg.strg, 
    Congress == 96), aes(x = long, y = lat, group = group, fill = Perc.dems)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(0, 100), 
    low = "grey", high = "#2B07B8")
```

![plot of chunk reg_dem_strg](figure/reg_dem_strg6.png) 

```r

ggplot() + labs(title = "Regional Democratic Strength, 97th Congress") + geom_polygon(data = subset(dem.reg.strg, 
    Congress == 97), aes(x = long, y = lat, group = group, fill = Perc.dems)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(0, 100), 
    low = "grey", high = "#2B07B8")
```

![plot of chunk reg_dem_strg](figure/reg_dem_strg7.png) 

```r

ggplot() + labs(title = "Regional Democratic Strength, 98th Congress") + geom_polygon(data = subset(dem.reg.strg, 
    Congress == 98), aes(x = long, y = lat, group = group, fill = Perc.dems)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(0, 100), 
    low = "grey", high = "#2B07B8")
```

![plot of chunk reg_dem_strg](figure/reg_dem_strg8.png) 

```r

ggplot() + labs(title = "Regional Democratic Strength, 100th Congress") + geom_polygon(data = subset(dem.reg.strg, 
    Congress == 100), aes(x = long, y = lat, group = group, fill = Perc.dems)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(0, 100), 
    low = "grey", high = "#2B07B8")
```

![plot of chunk reg_dem_strg](figure/reg_dem_strg9.png) 

```r

ggplot() + labs(title = "Regional Democratic Strength, 105th Congress") + geom_polygon(data = subset(dem.reg.strg, 
    Congress == 105), aes(x = long, y = lat, group = group, fill = Perc.dems)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(0, 100), 
    low = "grey", high = "#2B07B8")
```

![plot of chunk reg_dem_strg](figure/reg_dem_strg10.png) 

```r

ggplot() + labs(title = "Regional Democratic Strength, 110th Congress") + geom_polygon(data = subset(dem.reg.strg, 
    Congress == 110), aes(x = long, y = lat, group = group, fill = Perc.dems)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(0, 100), 
    low = "grey", high = "#2B07B8")
```

![plot of chunk reg_dem_strg](figure/reg_dem_strg11.png) 

```r

ggplot() + labs(title = "Regional Democratic Strength, 112th Congress") + geom_polygon(data = subset(dem.reg.strg, 
    Congress == 112), aes(x = long, y = lat, group = group, fill = Perc.dems)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(0, 100), 
    low = "grey", high = "#2B07B8")
```

![plot of chunk reg_dem_strg](figure/reg_dem_strg12.png) 

```r

ggplot() + labs(title = "Regional Democratic Strength, 113th Congress") + geom_polygon(data = subset(dem.reg.strg, 
    Congress == 113), aes(x = long, y = lat, group = group, fill = Perc.dems)) + 
    theme(rect = element_blank(), line = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank()) + scale_fill_continuous(limits = c(0, 100), 
    low = "grey", high = "#2B07B8")
```

![plot of chunk reg_dem_strg](figure/reg_dem_strg13.png) 


Summary: Regional Democratic Strength
---------------------------------------------------------------------------------
- The concentration of Democratic representatives moves from the Southern states, to the Midwest and Mountain States, and ends up with concentrations on the coasts, and very little strength in the middle. 

Congressional Diversity
---------------------------------------------------------------------------------

```r
# African Americans in Congress
afr.am <- readWorksheet(wb, sheet = "1-16", startRow = 4, endRow = 63, endCol = 7)
afr.am[is.na(afr.am)] <- 0
afr.am <- afr.am[, -c(2, 5)]
afr.am$Congress <- sub("[a-z]", "", sub("[a-z][a-z]", "", afr.am$Congress))

afr.am.sen <- afr.am[, c(1, 4, 5)]
afr.am.sen <- melt(afr.am.sen)
names(afr.am.sen) <- c("Congress", "Party", "No.sens")
levels(afr.am.sen$Party) <- c("D", "R")

afr.am.house <- afr.am[, c(1, 2, 3)]
afr.am.house <- melt(afr.am.house)
names(afr.am.house) <- c("Congress", "Party", "No.reps")
levels(afr.am.house$Party) <- c("D", "R")

afr.am <- merge(afr.am.house, afr.am.sen)
zeros <- data.frame(cbind(sort(rep(57:70, 2)), rep(c("D", "R"), 14), rep(0, 
    28), rep(0, 28)))
names(zeros) <- names(afr.am)
afr.am <- rbind(afr.am, zeros)
afr.am <- afr.am[order(as.numeric(afr.am$Congress)), ]
afr.am <- melt(afr.am, id = c("Congress", "Party"))
names(afr.am) <- c("Congress", "Party", "Office", "No.Afr.Am")
levels(afr.am$Office) <- c("House", "Senate")

# Hispanic Americans in Congress
hisp.am <- readWorksheet(wb, sheet = "1-17", startRow = 5, endRow = 59, endCol = 7)
hisp.am[is.na(hisp.am)] <- 0
hisp.am <- hisp.am[, -c(2, 5)]
hisp.am$Congress <- sub("[a-z]", "", sub("[a-z][a-z]", "", hisp.am$Congress))

hisp.am.sen <- hisp.am[, c(1, 4, 5)]
hisp.am.sen <- melt(hisp.am.sen)
names(hisp.am.sen) <- c("Congress", "Party", "No.sens")
levels(hisp.am.sen$Party) <- c("D", "R")

hisp.am.house <- hisp.am[, c(1, 2, 3)]
hisp.am.house <- melt(hisp.am.house)
names(hisp.am.house) <- c("Congress", "Party", "No.reps")
levels(hisp.am.house$Party) <- c("D", "R")

hisp.am <- merge(hisp.am.house, hisp.am.sen)
zeros2 <- data.frame(cbind(sort(rep(44:62, 2)), rep(c("D", "R"), 19), rep(0, 
    38), rep(0, 38)))
names(zeros2) <- names(hisp.am)
hisp.am <- rbind(hisp.am, zeros)
hisp.am <- hisp.am[order(as.numeric(hisp.am$Congress)), ]
hisp.am <- melt(hisp.am, id = c("Congress", "Party"))
names(hisp.am) <- c("Congress", "Party", "Office", "No.Hisp.Am")
levels(hisp.am$Office) <- c("House", "Senate")

# Women in Congress
women <- readWorksheet(wb, sheet = "1-18", startRow = 4, endRow = 53, endCol = 7)
women[is.na(women)] <- 0
women <- women[, -c(2, 5)]
women$Congress <- sub("[a-z]", "", sub("[a-z][a-z]", "", women$Congress))

women.sen <- women[, c(1, 4, 5)]
women.sen <- melt(women.sen)
names(women.sen) <- c("Congress", "Party", "No.sens")
levels(women.sen$Party) <- c("D", "R")

women.house <- women[, c(1, 2, 3)]
women.house <- melt(women.house)
names(women.house) <- c("Congress", "Party", "No.reps")
levels(women.house$Party) <- c("D", "R")

women <- merge(women.house, women.sen)
women <- women[order(as.numeric(women$Congress)), ]
women <- melt(women, id = c("Congress", "Party"))
names(women) <- c("Congress", "Party", "Office", "No.Women")
levels(women$Office) <- c("House", "Senate")

diversity <- merge(merge(afr.am, hisp.am, all = T), women, all = T)
diversity$Congress <- as.numeric(diversity$Congress)
diversity$No.Afr.Am <- as.numeric(as.character(diversity$No.Afr.Am))
diversity$No.Hisp.Am <- as.numeric(as.character(diversity$No.Hisp.Am))
diversity <- melt(diversity, id = c("Congress", "Party", "Office"))
names(diversity)[4:5] <- c("Group", "Count")
levels(diversity$Group) <- c("AfricanAmericans", "HispanicAmericans", "Women")

qplot(data = diversity, x = Congress, y = Count, geom = "point", color = Party, 
    size = I(1.5), main = "Congressional Diversity") + scale_color_manual(values = c("#2B07B8", 
    "#E30926")) + facet_wrap(Office ~ Group)
```

![plot of chunk diversity](figure/diversity.png) 


Summary: Congressional Diversity
---------------------------------------------------------------------------------
- Democrats tend to be more diverse than Republicans.
- The House tends to be more diverse than senate.
- The number of women in the the house is about the same for both parties, then the number of Democrats shoots up in the 103rd congress (1993-1994).  

Politics of Congress
====================================================================================
Next, I look at political alignments and ideologies in Congress.
- Conservative/liberal ideology scores
- Party unity voting and polarization: A party unity vote is a vote in which a majority of Democrats opposed a majority of Republicans
- Presidential party solidarity and polarization

Political Ideology of Congress
====================================================================================
Quantified on a scale of (-1,1). 
A positive score denotes a conservative ideology, while a negative score denotes a liberal one. Further from zero is more extreme. See [Poole-Rosenthal Scores](http://www.voteview.com/about.asp) for more info.
![http://blog.timesunion.com/opinion/files/2012/01/0131_WVparty.jpg](http://blog.timesunion.com/opinion/files/2012/01/0131_WVparty.jpg)


```r
wb6 <- loadWorkbook("~/Desktop/585/Project/VitalStatisticsFullData/Vital Statistics Chapter 6 - Legislative Productivity in Congress and Workload.xlsx")
wb8 <- loadWorkbook("/Users/samanthatyner/Desktop/585/Project/VitalStatisticsFullData/Vital Statistics Chapter 8 - Political Polarization in Congress and Changing Voting Alignments.xlsx")

house.ideology <- readWorksheet(wb8, sheet = "8-9", startRow = 3, endRow = 36, 
    endCol = 6)
house.ideology$Congress <- gsub("[(,)]", "", house.ideology$Congress)
house.ideology$Congress <- gsub("[a-z][a-z]", "", house.ideology$Congress)
house.ideology <- cbind(house.ideology, ldply(strsplit(house.ideology$Congress, 
    "\\s")))
house.ideology <- house.ideology[, c(7, 8, 2:4)]
names(house.ideology) <- c("Congress", "Year.Start", "Entire.chamber", "Democrats", 
    "Republicans")
house.ideology <- melt(house.ideology, id = c("Congress", "Year.Start"))
house.ideology$Congress <- as.numeric(house.ideology$Congress)
house.ideology$Year.Start <- as.numeric(house.ideology$Year.Start)

senate.ideology <- readWorksheet(wb8, sheet = "8-10", startRow = 3, endRow = 36, 
    endCol = 6)
senate.ideology$Congress <- gsub("[(,)]", "", senate.ideology$Congress)
senate.ideology$Congress <- gsub("[a-z][a-z]", "", senate.ideology$Congress)
senate.ideology <- cbind(senate.ideology, ldply(strsplit(senate.ideology$Congress, 
    "\\s")))
senate.ideology <- senate.ideology[, c(7, 8, 2:4)]
names(senate.ideology) <- c("Congress", "Year.Start", "Entire.chamber", "Democrats", 
    "Republicans")
senate.ideology <- melt(senate.ideology, id = c("Congress", "Year.Start"))
senate.ideology$Congress <- as.numeric(senate.ideology$Congress)
senate.ideology$Year.Start <- as.numeric(senate.ideology$Year.Start)

house.ideology$Office <- "House"
senate.ideology$Office <- "Senate"

ideology <- data.frame(rbind(house.ideology, senate.ideology))
names(ideology) <- c("Congress", "Year.Start", "Party", "Score", "Office")

bills.passed <- readWorksheet(wb6, sheet = "6-4", startRow = 4, endRow = 37, 
    endCol = 8)
bills.passed <- bills.passed[, -5]
names(bills.passed) <- c("Congress", "Public.BillsEnacted", "Public.TotalPages", 
    "Public.AvgPPS", "Private.BillsEnacted", "Private.TotalPages", "Private.AvgPPS")
bills.passed$Congress <- 80:112
bills.passed <- melt(bills.passed, id = "Congress")
bills.passed <- cbind(bills.passed, ldply(strsplit(as.character(bills.passed$variable), 
    "[.]")))
bills.passed <- bills.passed[, -2]
names(bills.passed) <- c("Congress", "Count", "Bill.Type", "Variable")
bills.passed <- bills.passed[, c(1, 3, 4, 2)]
bills.passed$Bill.Type <- as.factor(bills.passed$Bill.Type)
bills.passed$Variable <- as.factor(bills.passed$Variable)
ideology.bills <- merge(ideology, bills.passed, by = "Congress")
avg.ideol <- filter(ideology.bills, Variable == "BillsEnacted", Party == "Entire.chamber")
```


Quiz Time! (Note: This part looks better on the slides!)
====================================================================================
In which year was the House of Representatives the most ideologically **liberal** with a score of -0.126?
- **1975**, at the start of the 94th Congress 
- Right after 1974 midterm elections, Nixon (Republican) president

In which year was the House of Representatives the most ideologically **conservative** with a score of 0.193?
- **2011**, at the start of the 112th Congress 
- Right after the 2010 midterm elections, Obama (Democrat) president

Political Ideology of the Congress Overall

```r
qplot(Year.Start, Score, geom = "point", color = Score, shape = Office, data = avg.ideol, 
    size = I(2), ) + scale_color_gradient(limits = c(-0.2, 0.2), low = "#2B07B8", 
    high = "#E30926") + geom_vline(aes(xintercept = 1975), linetype = "dotdash", 
    color = "#2B07B8") + geom_vline(aes(xintercept = 2011), linetype = "dotdash", 
    color = "#E30926") + labs(title = "Overall Ideology of Congress, 1974-2011")
```

![plot of chunk ideol_all](figure/ideol_all.png) 

Political Ideology of the Parties in Congress

```r
qplot(Year.Start, Score, geom = "point", color = Party, data = subset(ideology.bills, 
    Party != "Entire.chamber"), main = "Political Ideology in the House and Senate by Party") + 
    facet_wrap(~Office) + scale_color_manual(values = c("#2B07B8", "#E30926"))
```

![plot of chunk ideol_party](figure/ideol_party.png) 


I was really intrigued by the pattern in House Republican scores, so I fit a change point model to see exactly where the increasing conservative trend started. 

```r
library(segmented)
rep.score.house <- filter(ideology.bills, Party == "Republicans", Office == 
    "House")
model <- segmented(lm(Score ~ Year.Start, data = rep.score.house), seg.Z = ~Year.Start, 
    psi = 1975)
pred.score <- predict.segmented(model, newdata = rep.score.house$Year.Start)
model$coef
```

```
##     (Intercept)      Year.Start   U1.Year.Start psi1.Year.Start 
##        3.943322       -0.001885        0.016195        0.000000
```

```r
model$psi
```

```
##                 Initial Est. St.Err
## psi1.Year.Start    1975 1978 0.2017
```

Fitted Model: 
 - Score = 3.94 - 0.0019 * Year + 0.016 * (Year-1978)


```r
pval <- davies.test(lm(Score ~ Year.Start, data = rep.score.house), ~Year.Start, 
    k = nrow(rep.score.house), alternative = "two.sided")$p.value
pval
```

```
## [1] 0
```

The p-value for the change point is less than 2.2e-16. So, something significant happened in 1978 that started a trend that has changed political ideology of the Republican party in the U.S. drastically. I postulate that it was the midterm election of Carter's presidency that set off this change, which contributed to Reagan getting elected, which I believe escalated the change greatly.  


```r
rep.score.house$fitted <- pred.score
library(ggplot2)
qplot(Year.Start, Score, data = rep.score.house, geom = "point", main = "Change Point Model for Change in Political Ideology of Republicans in the House 1947-2011") + 
    geom_path(aes(y = fitted)) + geom_vline(aes(xintercept = 1978)) + annotate("text", 
    label = "Midterm Election (Carter)", x = 1965, y = 0.64, size = 5, colour = "red")
```

![plot of chunk fits_seg](figure/fits_seg.png) 


Polarization of Congress
====================================================================================
Party Unity
-----------------------------------------------------------------------------------
Party Unity is defined in the Brookings data as the percentage of votes during a session of congress in which a majority of Republicans opposed a majority of Democrats. 

```r
party.unity <- readWorksheet(wb8, sheet = "8-3", startRow = 3, endRow = 63, 
    endCol = 3)
party.unity <- melt(party.unity, id = "Year")
names(party.unity) <- c("Year", "Office", "Perc.unity.votes")
party.unity$Year <- as.numeric(party.unity$Year)

qplot(Year, Perc.unity.votes, data = party.unity, geom = "point") + geom_path() + 
    facet_wrap(~Office) + geom_vline(xintercept = 1978, color = "red") + labs(title = "Percentage of Party Unity Votes")
```

![plot of chunk party_unity](figure/party_unity.png) 


Percent Unity
-----------------------------------------------------------------------------------
The percent unity variable refers to the percent of members voting with their party in the party unity votes described above. 


```r
unity.by.party <- readWorksheet(wb8, sheet = "8-4", startRow = 4, endRow = 63, 
    endCol = 8)
unity.by.party <- unity.by.party[, -5]
names(unity.by.party) <- c("Year", "House.Dems", "House.SouthDems", "House.Reps", 
    "Senate.Dems", "Senate.SouthDems", "Senate.Reps")
unity.by.party$House.Dems <- as.numeric(unity.by.party$House.Dems)
```

```
## Warning: NAs introduced by coercion
```

```r
unity.by.party$House.SouthDems <- as.numeric(unity.by.party$House.SouthDems)
```

```
## Warning: NAs introduced by coercion
```

```r
unity.by.party$House.Reps <- as.numeric(unity.by.party$House.Reps)
```

```
## Warning: NAs introduced by coercion
```

```r
unity.by.party$Senate.Dems <- as.numeric(unity.by.party$Senate.Dems)
```

```
## Warning: NAs introduced by coercion
```

```r
unity.by.party$Senate.SouthDems <- as.numeric(unity.by.party$Senate.SouthDems)
```

```
## Warning: NAs introduced by coercion
```

```r
unity.by.party$Senate.Reps <- as.numeric(unity.by.party$Senate.Reps)
```

```
## Warning: NAs introduced by coercion
```

```r
unity.by.party <- melt(unity.by.party, id = "Year")
unity.by.party <- cbind(unity.by.party, ldply(strsplit(as.character(unity.by.party$variable), 
    "[.]")))
unity.by.party <- unity.by.party[, c(1, 4, 5, 3)]
names(unity.by.party) <- c("Year", "Office", "Party", "Perc.unity")

qplot(Year, Perc.unity, data = subset(unity.by.party, Party != "SouthDems"), 
    geom = "point", color = Party) + geom_path() + facet_wrap(~Office) + scale_color_manual(values = c("#2B07B8", 
    "#E30926")) + labs(title = "Percent of Members Voting with their Own Party") + 
    geom_vline(xintercept = 1978)
```

```
## Warning: Removed 2 rows containing missing values (geom_point).
## Warning: Removed 2 rows containing missing values (geom_point).
```

![plot of chunk perc_unity](figure/perc_unity.png) 


Presidential Unity
----------------------------------------------------------------------
There is a data table called Congressional Voting in Support of the President's Position that I used to investigate the relationship between the president's party and the percent of congressmen of that party who support his position. 



```r
press <- c(rep("Truman", 8), rep("Eisenhower", 8), rep("Kennedy", 3), rep("Johnson", 
    5), rep("Nixon", 6), rep("Ford", 3), rep("Carter", 4), rep("Reagan", 8), 
    rep("Bush Sr", 4), rep("Clinton", 8), rep("Bush Jr", 8), rep("Obama", 4))

yrs <- 1945:2012
yrs <- sort(c(1974, yrs))
pres.yrs <- data.frame(cbind(press, yrs))
pres.yrs$yrs <- as.numeric(as.character(pres.yrs$yrs))
names(pres.yrs) <- c("President", "Year")

pres.party <- data.frame(cbind(unique(press), c("D", "R", "D", "D", "R", "R", 
    "D", "R", "R", "D", "R", "D")))
names(pres.party) <- c("President", "Party")
pres.yr.party <- sort_df(merge(pres.party, pres.yrs), vars = "Year")
wb8 <- loadWorkbook("/Users/samanthatyner/Desktop/585/Project/VitalStatisticsFullData/Vital Statistics Chapter 8 - Political Polarization in Congress and Changing Voting Alignments.xlsx")
pres.support.party <- readWorksheet(wb8, sheet = "8-2", startRow = 4, endRow = 85, 
    endCol = 8)
pres.support.party <- pres.support.party[-which(is.na(as.numeric(pres.support.party$President.and.year))), 
    ]
```

```
## Warning: NAs introduced by coercion
```

```r
pres.support.party <- pres.support.party[, -5]
names(pres.support.party) <- c("Year", "House.Dems", "House.South.Dems", "House.Reps", 
    "Senate.Dems", "Senate.South.Dems", "Senate.Reps")
pres.support.party$President <- press[-c(1:9)]
pres.support.party <- pres.support.party[, c(1, 8, 2:7)]
pres.support.party <- merge(pres.support.party, pres.party)
pres.support.party <- pres.support.party[, c(2, 1, 9, 3:8)]
pres.support.party <- sort_df(pres.support.party, vars = c("Year", "President"))
pres.support.party$House.South.Dems <- as.numeric(pres.support.party$House.South.Dems)
```

```
## Warning: NAs introduced by coercion
```

```r
pres.support.party$House.Reps <- as.numeric(pres.support.party$House.Reps)
```

```
## Warning: NAs introduced by coercion
```

```r
pres.support.party$Senate.South.Dems <- as.numeric(pres.support.party$Senate.South.Dems)
```

```
## Warning: NAs introduced by coercion
```

```r
pres.support.party$Year <- as.numeric(pres.support.party$Year)
pres.support.party$President <- as.factor(pres.support.party$President)
pres.support.party <- melt.data.frame(pres.support.party, id.vars = c("Year", 
    "President", "Party"))
levels(pres.support.party$variable) <- c("House.Dems", "House.SouthDems", "House.Reps", 
    "Senate.Dems", "Senate.SouthDems", "Senate.Reps")
pres.support.party <- cbind(pres.support.party, ldply(strsplit(as.character(pres.support.party$variable), 
    "[.]")))
pres.support.party <- pres.support.party[, c(1:3, 6, 7, 5)]
names(pres.support.party) <- c("Year", "President", "Pres.Party", "Office", 
    "Party", "Perc.support")
pres.support.party$Office <- as.factor(pres.support.party$Office)
pres.support.party$Party <- as.factor(pres.support.party$Party)
levels(pres.support.party$Pres.Party) <- c("Democrat", "Republican", "SouthernDemocrat")
pres.support.party$Pres.Party[which(pres.support.party$President %in% c("Carter", 
    "Johnson", "Clinton"))] <- "SouthernDemocrat"
levels(pres.support.party$Party) <- c("Democrat", "Republican", "SouthernDemocrat")

pres.support.party$Same.Party <- pres.support.party$Pres.Party == pres.support.party$Party
pres.support.party$Same.Party <- as.factor(pres.support.party$Same.Party)
levels(pres.support.party$Same.Party) <- c("Different", "Same")

pres.support.party2 <- pres.support.party
levels(pres.support.party2$Pres.Party) <- c("Democrat", "Republican", "Democrat")
levels(pres.support.party2$Party) <- c("Democrat", "Republican", "Democrat")

pres.support.party2$Same.Party <- pres.support.party2$Pres.Party == pres.support.party2$Party
pres.support.party2$Same.Party <- as.factor(pres.support.party2$Same.Party)
levels(pres.support.party2$Same.Party) <- c("Different", "Same")
```

House:

```r
ggplot(data = subset(pres.support.party2, Office == "House"), aes(x = factor(subset(pres.support.party2, 
    Office == "House")$President, levels = unique(subset(pres.support.party2, 
    Office == "House")$President)), y = Perc.support)) + geom_boxplot(aes(fill = Same.Party)) + 
    labs(x = "President", y = "% in Support of President", title = "House Votes in Support of the President's Position") + 
    scale_fill_manual(values = c("#2B07B8", "#E30926")) + coord_fixed(ratio = 0.12)
```

```
## Warning: Removed 15 rows containing non-finite values (stat_boxplot).
```

![plot of chunk house](figure/house.png) 


Senate:

```r
ggplot(data = subset(pres.support.party2, Office == "Senate"), aes(x = factor(subset(pres.support.party2, 
    Office == "Senate")$President, levels = unique(subset(pres.support.party2, 
    Office == "Senate")$President)), y = Perc.support)) + geom_boxplot(aes(fill = Same.Party)) + 
    labs(x = "President", y = "% in Support of President", title = "Senate Votes in Support of the President's Position") + 
    scale_fill_manual(values = c("#2B07B8", "#E30926")) + coord_fixed(ratio = 0.15)
```

```
## Warning: Removed 14 rows containing non-finite values (stat_boxplot).
```

![plot of chunk senate](figure/senate.png) 

The senate seems to be getting just as polarized as the house, but the overlap of Democratic support in the senate for Bush Jr's policies surprised me at first. The percent is actually skewed up by the show of support after 9/11. 

Summary: Polarization of Congress
-------------------------------------------------------------------------------------
- Congress has become extremely polarized
- The start of this increasing polarization seems to be the midterm election of 1978
- This trend has continued for the last 30+ years, and shows no signs of stopping.

Congressional Productivity
====================================================================================
When I refer to congressional productivity, I will mainly be referring to number of bills passed in a given year and/or the ratio of bills passed to bills enacted in a given year. I also compare all congresses to the so-called "Do-Nothing" congress of Truman's presidency.

```r
congresses <- read.csv("/Users/samanthatyner/Desktop/585/Project/congress_years.csv")
congresses <- congresses[, -3]
congresses$Congress <- as.numeric(sub("[a-z][a-z]", "", congresses$Congress))
congresses <- cbind(congresses, ldply(strsplit(as.character(congresses$Years), 
    "-")))
congresses <- congresses[-2]
names(congresses) <- c("Congress", "Year.Start", "Year.End")
pref <- c(rep(17, 5), "", rep(18, 49), "", rep(19, 49), rep("", 8))
congresses$pref <- pref
congresses$Year.End <- paste(congresses$pref, congresses$Year.End, sep = "")
congresses <- congresses[-4]

# House Workload, 80th-112th Congresses, 1947-2012
house.workload <- readWorksheet(wb6, sheet = "6-1", startRow = 3, endRow = 36)
house.workload$Congress <- 80:112
house.workload <- merge(congresses, house.workload, by = "Congress")
house.workload$Year.Start <- as.numeric(house.workload$Year.Start)
house.workload$Year.End <- as.numeric(house.workload$Year.End)
# Senate Workload, 80th-112th Congresses, 1947-2012
senate.workload <- readWorksheet(wb6, sheet = "6-2", startRow = 3, endRow = 36)
senate.workload$Congress <- 80:112
senate.workload <- merge(congresses, senate.workload, by = "Congress")
senate.workload$Year.Start <- as.numeric(senate.workload$Year.Start)
senate.workload$Year.End <- as.numeric(senate.workload$Year.End)

senate.workload$Office <- "Senate"
house.workload$Office <- "House"
names(house.workload) <- names(senate.workload)
workload <- data.frame(rbind(senate.workload, house.workload))
names(workload) <- c("Congress", "Year", "Year.End", "No.Introduced", "Intro/Member", 
    "Bills.Passed", "PassedtoIntroduced", "No.votes", "DaysinSession", "HoursinSession", 
    "Hrs/Day.inSession", "Committee", "Office")

names(congresses)[2] <- "Year"
work.yrs <- merge(workload, congresses)
work.yrs.pres <- merge(work.yrs, pres.yr.party, by = "Year")
names(work.yrs.pres)[15] <- "Pres.Party"

ggplot(data = work.yrs.pres) + geom_point(aes(Year, Bills.Passed, color = Pres.Party), 
    size = I(2)) + facet_wrap(~Office) + geom_vline(xintercept = 1978, color = "red") + 
    geom_vline(xintercept = 1947, color = "green") + scale_color_manual(values = c("#2B07B8", 
    "#E30926")) + annotate("text", label = "\"Do-Nothing\" Congress", x = 1960, 
    y = 500, size = 3, color = "green") + annotate("text", label = "Carter Midterm Election", 
    x = 1990, y = 2000, size = 3, color = "red") + labs(title = "Number of Bills Passed by the House and Senate, 1947-2011")
```

![plot of chunk congr_prod](figure/congr_prod.png) 



```r
ggplot(data = work.yrs.pres) + geom_point(aes(Year, PassedtoIntroduced, color = Pres.Party), 
    size = I(2)) + facet_wrap(~Office) + geom_vline(xintercept = 1978, color = "red") + 
    geom_vline(xintercept = 1947, color = "green") + scale_color_manual(values = c("#2B07B8", 
    "#E30926")) + annotate("text", label = "\"Do-Nothing\" Congress", x = 1960, 
    y = 0.2, size = 3, color = "green") + annotate("text", label = "Carter Midterm Election", 
    x = 1990, y = 0.4, size = 3, color = "red") + labs(title = "Ratio of Bills Passed to Bills Introduced in Congress, 1947-2011")
```

![plot of chunk congr_prod2](figure/congr_prod2.png) 


Summary: Congressional Productivity
--------------------------------------------------------------------
- Congressional productivity has almost continually decreased since 1949.
- The infamous 'Do-Nothing' congress (the 80th, from 1947-48) is shockingly productive compared to the most recent congress in the data (the 112th, from 2011-12)
- Why has this happened??

Does Ideology Affect Productivity?
======================================================================
First I look at the relationship between overall ideology in Congress and congressional productivity.

```r
ideols <- filter(ideology, Party == "Entire.chamber")
names(ideols)[2] <- "Year"
ideol.work <- merge(ideols, workload)

ggplot(data = ideol.work, aes(x = Score, y = Bills.Passed)) + geom_point(aes(color = Score), 
    size = I(3)) + facet_wrap(~Office) + geom_text(aes(label = Year, y = Bills.Passed + 
    40)) + scale_color_gradient(limits = c(-0.2, 0.2), low = "#2B07B8", high = "#E30926") + 
    labs(title = "Overall Ideology of Congress and Bills Passed")
```

![plot of chunk ideo_prod](figure/ideo_prod.png) 



```r
ggplot(data = ideol.work, aes(x = Score, y = ideol.work[, 10])) + geom_point(aes(color = Score), 
    size = I(3)) + facet_wrap(~Office) + geom_text(aes(label = Year, y = ideol.work[, 
    10] + 0.01)) + scale_color_gradient(limits = c(-0.2, 0.2), low = "#2B07B8", 
    high = "#E30926") + labs(title = "Overall Ideology of Congress & Ratio of Bills Passed to Bills Introduced", 
    y = "Passed/Introduced")
```

![plot of chunk ideo_prod2](figure/ideo_prod2.png) 


From these plots, it appears that the overall ideology of the Congress does not seem to have a strong or obvious effect on either number of bills passed or on ratio of bill passed to bills introduced. 

But, what about the ideological differences between the parties?

```r
house.ideology <- readWorksheet(wb8, sheet = "8-9", startRow = 3, endRow = 36, 
    endCol = 6)
house.ideology$Congress <- gsub("[(,)]", "", house.ideology$Congress)
house.ideology$Congress <- gsub("[a-z][a-z]", "", house.ideology$Congress)
house.ideology <- cbind(house.ideology, ldply(strsplit(house.ideology$Congress, 
    "\\s")))
house.ideology <- house.ideology[, c(7, 8, 2:4)]
names(house.ideology) <- c("Congress", "Year", "Entire.chamber", "Democrats", 
    "Republicans")
house.ideology$Office <- "House"
senate.ideology <- readWorksheet(wb8, sheet = "8-10", startRow = 3, endRow = 36, 
    endCol = 6)
senate.ideology$Congress <- gsub("[(,)]", "", senate.ideology$Congress)
senate.ideology$Congress <- gsub("[a-z][a-z]", "", senate.ideology$Congress)
senate.ideology <- cbind(senate.ideology, ldply(strsplit(senate.ideology$Congress, 
    "\\s")))
senate.ideology <- senate.ideology[, c(7, 8, 2:4)]
names(senate.ideology) <- c("Congress", "Year", "Entire.chamber", "Democrats", 
    "Republicans")
senate.ideology$Office <- "Senate"
ideols2 <- data.frame(rbind(house.ideology, senate.ideology))

ideols2.work <- merge(ideols2, workload[, c(1, 2, 3, 5, 6, 13)])
ideols2.work$diff <- abs(ideols2.work$Democrats - ideols2.work$Republicans)

ggplot(data = ideols2.work, aes(x = diff, y = Bills.Passed)) + geom_point(aes(color = diff), 
    size = I(3)) + facet_wrap(~Office) + geom_text(aes(label = Year, y = Bills.Passed + 
    40)) + scale_color_gradient(limits = c(0, 1.2), low = "white", high = "#E30926") + 
    labs(title = "Ideology Differences in Congress and Bills Passed")
```

![plot of chunk partyideol1](figure/partyideol1.png) 


Looking at the above plot, there is definitely evidence to suggest that as the two parties in congress develop more extreme ideologies, congressional productivity decreases dramatically.  There is also evidence to suggest that the increased polarization is in large part due to the drastic increase of conservative ideology amongst congressional Republicans since 1978. The congressional Democrats has also become slightly more liberal since that time, but their change is nowhere near as radical as the republicans'.  (See previous plot titled 'Political Ideology in the House and Senate by Party')

2014 Midterm Elections
====================================================================================
So, what does this mean for the elections coming up in November of this year?

- We cannot afford to elect more and more extreme candidates to congress. The legislative branch could become completely stalled!
- We cannot let ourselves get sucked into the polarization of American political parties like we have been for the last 30+ years. (See next plot.)

Polarization of Voters
-----------------------------------------------------------------------------------

```r
wb2 <- loadWorkbook("/Users/samanthatyner/Desktop/585/Project/VitalStatisticsFullData/Vital Statistics Chapter 2 - Congressional Elections.xlsx")
party.lines <- readWorksheet(wb2, sheet = "2-19", startRow = 4, endRow = 32, 
    endCol = 12)
party.lines <- party.lines[-c(5, 9)]
names(party.lines) <- c("Year", "Pres.partyline", "Pres.defect", "Pres.indep", 
    "Sen.partyline", "Sen.defect", "Sen.indep", "House.partyline", "House.defect", 
    "House.indep")
party.lines <- melt(party.lines, id = "Year")
party.lines <- party.lines[-which(is.na(party.lines$value)), ]
party.lines <- cbind(party.lines, ldply(strsplit(as.character(party.lines$variable), 
    "[.]")))
party.lines <- party.lines[, c(1, 4, 5, 3)]
names(party.lines) <- c("Year", "Type.elec", "Type.voter", "Perc.voters")
party.lines$Type.elec <- as.factor(party.lines$Type.elec)
party.lines$Type.voter <- as.factor(party.lines$Type.voter)
party.lines$Perc.voters <- as.numeric(party.lines$Perc.voters)
```

```
## Warning: NAs introduced by coercion
```

```r

voter.turnout <- readWorksheet(wb2, sheet = "2-1", startRow = 3, endRow = 45, 
    endCol = 3)
voter.turnout <- melt(voter.turnout, id = "Year")
names(voter.turnout) <- c("Year", "Election.Type", "Percent")
voter.turnout <- na.omit(voter.turnout)
voter.turnout$Election.Type <- as.character(voter.turnout$Election.Type)
voter.turnout[which(voter.turnout$Election.Type == "House.elections" & voter.turnout$Year %in% 
    voter.turnout$Year[which(voter.turnout$Election.Type == "Presidential.elections")]), 
    ]$Election.Type <- "On Year"
voter.turnout$Election.Type <- as.factor(voter.turnout$Election.Type)
levels(voter.turnout$Election.Type) <- c("Midterm", "House", "Presidential")
party.lines2 <- merge(party.lines, voter.turnout)
party.lines2$Type.elec <- as.factor(party.lines2$Type.elec)
party.lines2$Type.voter <- as.factor(party.lines2$Type.voter)
party.lines2$Perc.voters <- as.numeric(party.lines2$Perc.voters)
party.lines2$Year <- as.numeric(party.lines2$Year)
party.lines3 <- subset(party.lines2, Type.elec != "Pres")

qplot(Year, Perc.voters, geom = "point", color = Type.voter, data = subset(party.lines3, 
    Type.voter != "indep"), size = I(2), main = "Percent of Voters Voting along Party Lines") + 
    geom_vline(xintercept = 1978) + facet_wrap(~Type.elec) + coord_fixed(ratio = 1.3)
```

```
## Warning: Removed 4 rows containing missing values (geom_point).
```

![plot of chunk voters_pol](figure/voters_pol.png) 


Will the polarization continue in the 2014 election? 
------------------------------------------------------------------
- Look at parties of current candidates
- Compare to parties of incumbents
- See [Party Abbreviations](http://www.fec.gov/finance/disclosure/metadata/DataDictionaryPartyCodeDescriptions.shtml) for help decoding the party labels in the following graphs.


```r
candidates14 <- read.csv("~/Desktop/585/Project/CandidateSummaryAction.csv")
candidates14 <- candidates14[, 3:8]
names(candidates14) <- c("Name", "Office", "State", "District", "Party", "Status")
candidate_info14 <- group_by(candidates14, State, District) %.% arrange(State, 
    District, Office)
candidate.open <- filter(candidate_info14, Status == "OPEN") %.% group_by(Office, 
    Party)
tot.open <- dplyr::summarise(candidate.open, n = n())

qplot(State, n, data = tot.open, geom = "bar", fill = Party, main = "Number of Candidates for Open Seats") + 
    facet_wrap(~Office) + coord_flip()
```

![plot of chunk cand1](figure/cand1.png) 



```r
summ.cand <- filter(candidate_info14, Status %in% c("CHALLENGER", "INCUMBENT"), 
    Office %in% c("H", "S")) %.% group_by(State, Office, Status, Party) %.% 
    dplyr::summarise(n = n())
names(summ.cand)[1] <- "Abbreviation"
summ.cand.reg <- merge(summ.cand, states, by = "Abbreviation")

qplot(data = summ.cand.reg, Region, n, fill = Party, geom = "bar", main = "Numbers of Challengers and Incumbents by State") + 
    facet_grid(Status ~ Office)
```

![plot of chunk cand2](figure/cand2.png) 


- Based on the amount of pink (Republican candidates) in the previous two plots, it looks like the Democrats will have a hard time holding onto their majority in the senate.  
- If the Republicans have the majority in both the House and the Senate in 2015 (they currently have the majority in the House), congressional productivity will likely increase due to party unity.
- But with a Democratic president in his last 2 years, there will also probably be a lot of vetoes, so fewer bills are likely to become law.  Support of this claim follows: 

Vetoes
---------------------------------------------------------------------------------

```r
midterm.years <- subset(voter.turnout, Election.Type == "Midterm")$Year
vetoes <- readWorksheet(wb6, sheet = "6-6", startRow = 4, endRow = 37)
vetoes$Congress <- 80:112
names(vetoes) <- c("Congress", "Total.PresVetoes", "Vetoes", "PocketVetoes", 
    "Overridden", "Perc.Vetoes", "House.OverrideAttempts", "Senate.OverrideAttempts")
vetoes <- merge(congresses, vetoes)
vetoes <- vetoes[, -3]
names(vetoes) <- c("Congress", "Year", "Total.PresVetoes", "Vetoes", "PocketVetoes", 
    "Overridden", "Perc.Vetoes", "House.OverrideAttempts", "Senate.OverrideAttempts")
vetoes <- merge(pres.yr.party, vetoes)
qplot(Year, Total.PresVetoes, data = vetoes, color = Party, main = "Presidential Vetoes by Congress, 1947-2011 (lines are midterm elections)", 
    size = I(2)) + scale_color_manual(values = c("#2B07B8", "#E30926")) + geom_vline(xintercept = midterm.years[6:21], 
    linetype = "dashed")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 


Notice that when the president changes party, the previous president increases his vetoes in his last 2 years. This pattern starts after 1978, much like many of the other patterns we've seen. 

Conclusion
===========================================================================
I might be letting my personal bias affect my conclusions a little bit, but it really seems that the increased polarization in two-party system is destroying the effectiveness of the United States Congress.  To solve this problem, Americans have to have some sort of mass crisis of conscience and come together to fix it (not likely), **or** we need to get rid of the two-party system (also not likely),  **or** we dropkick the entire U.S. Congress (just kidding).  

Basically, the system is broken, and we have to figure out how to fix it.  The best thing we can do about this is learn about the candidates in our district and state and **VOTE** in the midterm election in November.  Maybe if we as citizens stop picking the most vitriolic candidates, we will actually have a government that governs someday. 

A Final Note: 
===========================================================================
So, just how useless is the U.S. Congress?

As it turns out, pretty darn useless. 

![text](http://www.washingtonpost.com/rf/image_296w/2010-2019/WashingtonPost/2011/04/08/Editorial-Opinion/Graphics/c_04102011.jpg) 
![text](http://d12yn0b633iq0y.cloudfront.net/media/catalog/product/cache/1/thumbnail/9df78eab33525d08d6e5fb8d27136e95/u/s/useless_as_congress_t_1.jpg)
