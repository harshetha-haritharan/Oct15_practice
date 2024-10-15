#1a. How many of these bonds were approved by voters?
length(Bonds[Bonds$Result=="Carried",10])
#7210 bonds were approved by voters. 

#1b. How many were defeated?
length(Bonds[Bonds$Result=="Defeated",11])
#1638 bonds were defeated by voters. 

#1c. Are there any differences in the rates of approved bonds across the four different government types? 
City <- (Bonds[Bonds$Type=="CITY",])
prop.table(table(City$Result))
#City approved 87.55% of the bonds.
ISD <- (Bonds[Bonds$Type=="ISD",])
prop.table(table(ISD$Result))
#ISD approved 72.43% of the bonds.
WD <- (Bonds[Bonds$Type=="WD",])
prop.table(table(WD$Result))
#WD approved 94.52% of the bonds.
County <- (Bonds[Bonds$Type=="COUNTY",])
prop.table(table(County$Result))
#County approved 82.8% of the bonds.
#WD has the highest rate of approved bonds and ISD  has the lowest.

#2a.Calculate a new variable in the dataframe called “Votes_Total” that is the sum of the votes “for” and “against” the bond measure.  
Votes_Total <- (Bonds$VotesFor+Bonds$VotesAgainst)

#2b.When and where did the bond measure with the highest voter turnout occur? What was it for?
Bonds$VoterTurnout<-Votes_Total
Bonds[Bonds$VoterTurnout==max(Bonds$VoterTurnout),]
#Harris County on 11/8/22 had the highest voter turnout. It was for road utilities. 

#3a.Create a subset of this dataset that contains the bond measures that were approved and had at least 100 total votes.
Approved_Bonds<-Bonds[Bonds$Result=="Carried"&Bonds$VoterTurnout>=100,]

#3b.Create a new variable within the subset dataframe that gives the percentage of total votes that were for the bond measure.
Approved_Bonds$Percentage<-(Approved_Bonds$VotesFor/Approved_Bonds$VoterTurnout)

#3c. Make a graph of the distribution of this new variable.
hist(Approved_Bonds$Percentage,main='Distribution of Percentage of Total Votes', xlab='Percentage of Total Voters',col='pink',ylim=c(0,700),xlim=c(0,1.0))

#3d. Describe its distribution with the appropriate statistics.
fivenum(Approved_Bonds$Percentage)
#The graph is skewed to the right.
#It has a median of 65.08%.

#4a. Is the margin a bond was approved by related to its cost? Answer this question, citing the appropriate descriptive statistic. 
round(cor(Approved_Bonds$Amount,Approved_Bonds$Percentage), 3)
#The margin a bond was approved by is not related to its cost because the correlation Coefficient is 0.000559102 which indicates there is almost no linear correlation between the margin a bond was approved and its cost. 

#4b. Use your subset from #3 to create a graph to display this relationship.
plot(Approved_Bonds$Amount,Approved_Bonds$Percentage,main='Cost and Percentage of Approved Bonds',xlab='Cost ($)',ylab='Percentage of Approved Bonds',pch=20,ylim=c(0,1),xlim=c(0,1000000000) )
