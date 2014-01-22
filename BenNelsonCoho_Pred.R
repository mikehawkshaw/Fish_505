#Third R Program for Fish 505
#Mike Hawkshaw
#Jan 15 2014
#
#COHO modeling - with predator interactions based on work with Ben Nelson
#########################################################################################################
#Model Steps:  
#########################################################################################################
#	predator interaction
#	1) 	Ricker Model Spawners ->Recruits w/ predator effect
#	2)	Adult->Harvest->Spawners						
#
#	no predator interaction
#	1) 	Ricker Model Spawners->Recruits
#	2)	Adult->Harvest->Spawners						
#
#########################################################################################################
#Pseudo Code: Conceptual framework we build from 
#########################################################################################################
#
#Read in Coho data from file
#set up data structures
#build linear model
#	
#########################################################################################################
#Header
#########################################################################################################

rm(list=ls())															#clear the crud out of local memory
set.seed(999)															#all use same random numbers
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

#RICKER REC SUB
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
fn<-"COHO_data_Fraser.csv"						#assign filename to a variable 
coho_time_series<-read.csv(fn,header=T)			#read coho timeseries into model
#Set parameters

years<-coho_time_series$Year 					#Year of observations
s_obs<-coho_time_series$COHO_Spawners			#Spwaning Stock (DFO Data)
hr_obs<-coho_time_series$est_ER					#estimated Harvest Rates (DFO Data)
pred_obs<-coho_time_series$Piniped				#estimated predator populations size (DFO Data - Ben Nelson Model)
n_years<-length(years)							#how many years of data is there
k<-3											#interior fraser coho life history is very specific

#fit models to data

#Simulate population dynamics
