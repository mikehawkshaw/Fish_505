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
#fit linear models to that data
#do a very quick and dirty model selection excersize
#Brute force equilibrium analysis
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
ricker_simple<-function(x,a,b,wt)				#a function to calculate the return associated with any spawning stock size 
{
	y=x*exp(a-b*x+wt)							#Simple ricker function chosen for utility of parameters														#given ricker _a ricker_b and Process errors

return(y)
}

#RICKER REC SUB
ricker_predator<-function(x,a,b,q,p,wt)			#a function to calculate the return associated with any spawning stock size 
{												#given ricker _a ricker_b and Process errors
	y=x*exp(a-b*x-q*p+wt)						#more complicated predator impact ricker model see walters et al 1986

return(y)
}

#Plotting Sub

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

r_derived<-s_obs/(1-hr_obs)						#R=S/(1-hr) relationship between returns and harvest rate
#Index by Brood Year for sanity
s_by<-s_obs[1:(n_years-k)]						#spawner b in brood year
r_by<-r_derived[(k+1):n_years]					#associated recruitment
pred_by<-pred_obs[3:(n_years-1)]


lnrs<-log(r_by/s_by)							#ln(R/S) for model fitting

#fit models to data
#?lm()

l_simple<-lm(lnrs~s_by)
l_pred<-lm(lnrs~s_by+pred_obs[k+1])

#Simulate population dynamics








