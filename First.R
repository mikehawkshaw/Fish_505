#First R Program for Fish 505
#Mike Hawkshaw
#Jan 7 2014
#
#Very Simple Bull Trout Population Model
#########################################################################################################
#Model Steps:  Please See presentation figures for Bull Trout Life Hstory Diagram
#########################################################################################################
#
#	1) 	Eggs->Juveille 							 	(3 year lag density dependant recruitment)
#	2)	Juvenile->Adult								(three years of juvenile growth)
#	3)	Adult->Post Harvest Adults					(Recreational fishing mortality)
#	4)	Adult->Post Poaching Adults					(Poaching mortlity)
#	5)	Adult->Annual Survival						(Adult overwinter suvival)
#
#########################################################################################################
#Pseudo Code: Conceptual framework we build from 
#########################################################################################################
#	Set variables
#	Generate Recruitment deviations (optional)
#	Initialize populations
#	Simulate population dynamics
#
#		Loop over y years
#
#		1) 	 	rt[y-3]=st[y-6]*exp(ricker_a-ricker_b*st[y-6]+wt[y-6]) 		(3 year lag density dependant recruitment)
#		2)	 	st[y]=st[y-1]*Sa+Rt[y-3]*(Sj^3)								(three years of juvenile growth)
#		3)	 	st[y]=st[y]*(1-u_rec)										(Recreational fishing mortality)
#		4)	 	st[y]=st[y]*(1-u_poach)										(Poaching mortlity)
#	
#########################################################################################################
#Header
#########################################################################################################

rm(list=ls())
set.seed(999)

#########################################################################################################
#Sub Functions
#########################################################################################################
#Dynamics - Subs
bull_trout<-function(sd=0.6,sa=0.8,sj=0.6,ricker_a=1.2,ricker_b=0.001,u_rec=0.05,u_poach=0.25,years=25)
{
wt<-rnorm(years,0,sd)

#	Initialize populations

st<-rep((sj^3*ricker_a/ricker_b),length=years)
rt<-st*exp(ricker_a-ricker_b*st)

#	Simulate population dynamics

for(y in 7:years)
{
	rt[y-3]<-st[y-6]*exp(ricker_a-ricker_b*st[y-6]+wt[y-6])
	st[y]<-sa*st[y-1]+(sj^3)*rt[y-3]
	st[y]<-st[y]*(1-u_rec)
	st[y]<-st[y]*(1-u_poach)
}

#Get things organized by Brood Year

s_by<-st[7:years]
r_by<-rt[10:years]
lnrs<-log(r_by/s_by[1:length(r_by)])

recovered_ricker<-lm(lnrs~s_by[1:length(r_by)])

output<-list(Esc=st,Rec=rt,lnrs=lnrs,a=recovered_ricker$coeff[1],b=(-recovered_ricker$coeff[2]))
return(output)
}


#########################################################################################################
#Graphics - Subs


#########################################################################################################
#Main
#########################################################################################################

#	Set parameters

sa<-0.8
sj<-0.6
ricker_a<-1.2
ricker_b<-0.001
u_rec<-0.05
u_poach<-0.25
years<-25
sims<-1000


#	Generate Recruitment deviations (optional)

wt<-rnorm(years,0,0.6)

#	Initialize populations

st<-rep((sj^3*ricker_a/ricker_b),length=years)
rt<-st*exp(ricker_a-ricker_b*st)

#	Simulate population dynamics

for(y in 7:years)
{
	rt[y-3]<-st[y-6]*exp(ricker_a-ricker_b*st[y-6]+wt[y-6])
	st[y]<-sa*st[y-1]+(sj^3)*rt[y-3]
	st[y]<-st[y]*(1-u_rec)
	st[y]<-st[y]*(1-u_poach)
}

#Get things organized by Brood Year

s_by<-st[7:years]
r_by<-rt[10:years]
lnrs<-log(r_by/s_by[1:length(r_by)])

recovered_ricker<-lm(lnrs~s_by[1:length(r_by)])


asim<-rep(NA,sims)
bsim<-rep(NA,sims)

for(s in 1:sims)
{
	bt<-bull_trout()
	asim[s]<-bt$a
	bsim[s]<-bt$b
}

par(mfcol=c(1,2))
plot(density(asim))
abline(v=ricker_a)
plot(density(bsim))
abline(v=ricker_b)
















