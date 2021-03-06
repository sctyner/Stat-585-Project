585 Project Plan
========================================================
Samantha Tyner
--------------------------------------------------------
### March 26, 2014
For my project, I will examine the Vital Statistics on Congress put together by the Brookings Institution and the American Enterprise Institute in conjuction with the Federal Election Commission's data on all candidates for the 2014 midterm election.   

Motivation
--------------------------------------------------------
I chose this project because I, not unlike most Americans, am extremely frustrated with the U.S. Congress, and when I happened upon [this blog post](http://wonkviz.tumblr.com/post/74277939647/connecting-the-dots-between-partisanship-and-gridlock) that cited the Vital Stats from Brookings, I decided to investigate the history of congress as it relates to partisanship and congressional productivity. I also want to look at the field of candidates for this year's election and investigate how the history of congress might affect campaign spending, number of candidates, proportion of candidates running unopposed, or any other factors in this year's election. 

Data
--------------------------------------------------------
The data I will be using comes from two different sources. First, I will use the [full data set](http://www.brookings.edu/research/reports/2013/07/vital-statistics-congress-political-polarization-voting-alignments-mann-ornstein) (in Excel format) from the Brookings Institution's website. There are 8 sections total: 

1. Members of Congress 
2. Elections
3. Campaign Finance
4. Committees
5. Congressional Staff and Operating Expenses
6. Workload 
7. Budgeting
8. Voting Alignments 

I will focus on 1, 2, 3, and 8, though I may rely on some parts of the other sections if the need arises. Since the data is in Excel format, it has all sorts of fancy headers that make it very difficult to read into R all at once. To help with reading the many sheets into R, I will use the XLConnect package. (If there is a better one than this, please let me know!) This data is also very messy: the sheets I've seen so far have almost all of the most common problems with messy data that are listed in Wickham's "Tidy Data" paper, sometimes two or more on one sheet. So, a lot of work on this project will go into tidying the data from Excel. 

The second data source I will use is the [2014 Candidate Summary](http://www.fec.gov/data/CandidateSummary.do) data available from the FEC's website. It contains every candidate's name, district, office, incumbent/challenger status, as well as some campaign finance information where available.  This data is very tidy, so all I will have to do to use it is read it into R using the XML package.   

Questions of Interest
--------------------------------------------------------
I have many ideas for this project, and I've listed them all here. I probably won't be able to do all of these things or answer all of these questions, but I'm certainly going to try!

* What characterizes a seat as vulnerable or as a stronghold? How can a party make its own vulnerable seats stronger? How can a party make opposing strongholds weaker?
* Is there a connection between campaign spending and congressional productivity? I suspect there might be, and that they might both be affected by ideological differences between parties.
* What characterizes a productive congress? What characterizes a "do-nothing" congress? How should the country vote to make sure the 114th and future congresses are not do-nothing?
* Bonus: What does the history of women in congress look like?
* SUPER Bonus: Can I make a Shiny app that aggregates all (or part of) of the Vital Statistics on Congress so that future interested parties can easily visualize these statistics for themselves and their district? 

My Project So Far: Wed, April 9th
--------------------------------------------------------
So far, I have cleaned all of the data in the first section of the Brookings institute data (the demographics of congress), and I have started a shiny app to easily visualize the Brookings data.  I've also started looking at the FEC data. A sample of the cleaned data is in congressional_diversity.csv, which lists the number of senators and representatives from the historically underrepresented groups given in the Brookings data: African Americans, Hispanic Americans, and Women.  The code I used to clean the data is in congressstuff.R. 

I've also started examining the FEC data, which was very easy to read into R and group by state and district using dplyr. The data cleaning for the Brookings data, on the other hand, is really turning out to be quite cumbersome, but I definitely have a good system in place that I can apply to cleaning the other 3 areas I'm interested in. I've started looking at the second item, which contains a lot of information about change of party power and net gains and losses of seats, and I'm excited to start putting that information together with the demographic information.  

For the future, I will be focusing on the Shiny app to visualize the Brookings Institute data, and I will also put together a report on party makeup of congress and how that effects congressional productivity. I will also combine that with the FEC data and aim to design the most productive 114th congress.   
