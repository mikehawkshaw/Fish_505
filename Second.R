#Second R Program for Fish 505
#Mike Hawkshaw
#Jan 15 2014
#
#COHO modeling
#########################################################################################################
#Model Steps:  
#########################################################################################################
#
#	1) 	Ricker Model Spawners->Recruits
#	2)	Adult->Harvest->Spawners						
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
#		1)R[t]<-S[t-k]*exp(ricker_a-ricker_b*S[t-k]+wt[t-k])   #Ricker Recruit Model
#		2)S[t]<-R[t]*(1-hr[t])									#Recruits that survive the harvest become spawners
#		3)C[t]<-hr[t]*R[t]										#catches are the hr*returning fish
#	
#########################################################################################################
#Header
#########################################################################################################

rm(list=ls())												#clear the crud out of local memory
set.seed(999)												#all use same random numbers
setwd("/Users/mikehawkshaw/Desktop/Fish_505_Example/Fish_505")			#it makes reading and writing files easier if you're in the right directory										#set a working directory


#########################################################################################################
#Sub Functions
#########################################################################################################
#Dynamics - Subs

#RICKER REC SUB
ricker_simple<-function(x,a,b,proc_error)				#a function to calculate the return associated with any spawning stock size 
{														#given ricker _a ricker_b and Process errors
	y<-x*exp(a-b*x+proc_error)

return(y)
}

ricker_predator<-function(x,a,b,q,p,proc_error)				#a function to calculate the return associated with any spawning stock size 
{															#given ricker _a ricker_b and Process errors
	y<-x*exp(a-b*x-q*p+proc_error)							#incorperating predator effects

return(y)
}


#Plotting Sub

second_axis<-function(x,y1,y1_1,y1_2,y2,ax_1,ax_2)
{
	par(mar=c(5, 12, 4, 4) + 0.1)
	plot(x,y1,type="b",col="black",lty=1,lwd=1.2,axes=T,xlab="", ylab="", main="", ylim=c(0,max(y1,y1_1,y1_2,na.rm=T)))
	lines(x,y1_1,type="b",col="dark red")
	lines(x,y1_2,type="b",col="dark green")
	axis(2, ylim=c(0,max(y1,na.rm=T)),col="black",lwd=2)
	mtext(2,text=ax_1,line=2)

	#plot Harvest Rate vs Populations Extinct n second axis
	par(new=T)
	plot(x,y2,type="l",col="red",lty=1,lwd=1.2,axes=F,xlab="", ylab="", main="")
	axis(2, ylim=c(0,max(y2,na.rm=T)),lwd=2,line=3.5, col="red")
	mtext(2,text=ax_2,line=5.5, col="red")
}

#########################################################################################################
#Graphics - Subs


#########################################################################################################
#Main
#########################################################################################################

#read in marine mortality data 
fn<-"COHO_data.csv"								#assign filename to a variable 
coho_time_series<-read.csv(fn,header=T)			#read coho timeseries into model
#Set parameters

years<-coho_time_series$Year 					#Year of observations
s_obs<-coho_time_series$COHO_Spawners			#Spwaning Stock (DFO Data)
hr_obs<-coho_time_series$est_ER					#estimated Harvest Rates (DFO Data)
pred_obs<-coho_time_series$Piniped				#estimated predator populations size (DFO Data - Ben Nelson Model)

r_derived<-s_obs/(1-hr_obs)						#reivation of recruitments (by Brood Year given pawners and HR from DFO DATA)
LNRS<-log(r_derived/s_obs)

l_pred<-lm(LNRS~s_obs+pred_obs)

ricker_a_p<-l_pred$coeff[1]
ricker_b_p<- -l_pred$coeff[2]
ricker_q_p<- -l_pred$coeff[3]

k<-3
n_years<-length(years)

#Generate Recruitment deviations (optional)
wt<-rnorm(n_years,0,0.6)						#normaly distributed proicess error

#Initialize populations

s<-rep(s_obs,length=n_years)					#initialize our spawning stock 
r<-rep(NA,length=n_years)						#vector to hold our recruits
yt<-rep(NA,length=n_years)						#vector to hold our yeild

hr<-rep(0.6,length=n_years)

for(y in (k+1):n_years)								#generate a population given HR and other parameters
{
	r[y]<-ricker_predator(s[y-k],ricker_a_p,ricker_b_p,ricker_q_p,pred[y],wt[y-k])		#generate a recruitments
	s[y]<-r[y]*(1-hr[y])										#spawners are the ones who survive our fishery
	yt[y]<-r[y]*hr[y]											#catch are the ones who dont
}

#Simulate population dynamics




#Get things organized by Brood Year
