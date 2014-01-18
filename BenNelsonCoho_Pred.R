#Second R Program for Fish 505
#Mike Hawkshaw
#Jan 15 2014
#]
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
#
#Read in Coho data from file
#set up data structures
#build linear model
#
#Build BAyesian Mo
#
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
n_years<-length(years)
k<-3

r_derived<-s_obs/(1-hr_obs)						#reivation of recruitments (by Brood Year given pawners and HR from DFO DATA)
r_by<-r_derived[(k+1):n_years]
s_by<-s_obs[1:(n_years-k)]
pred_by<-pred_obs[1:(n_years-k)]

LNRS<-log(r_by/s_by)
l_normal<-lm(LNRS~s_by)							#total BS fit
l_pred<-lm(LNRS~s_by+pred_by)					#quite good fit

ricker_a_p<-l_pred$coeff[1]
ricker_b_p<- -l_pred$coeff[2]
ricker_q_p<- -l_pred$coeff[3]

ricker_a<-l_normal$coeff[1]
ricker_b<- -l_normal$coeff[2]

s_vec<-seq(0,1.25*max(s_obs,na.rm=T),length=200)
r_vec_p_low<-ricker_predator(s_vec,ricker_a_p,ricker_b_p,ricker_q_p,pred_obs[10],0)	
r_vec_p_high<-ricker_predator(s_vec,ricker_a_p,ricker_b_p,ricker_q_p,pred_obs[35],0)	
r_vec<-ricker_simple(s_vec,ricker_a,ricker_b,0)

r_pred_l<-ricker_simple(s_by,ricker_a,ricker_b,0)
r_pred_p<-ricker_predator(s_by,ricker_a_p,ricker_b_p,ricker_q_p,pred_by,0)	

par(mfcol=c(1,2))
plot(s_by,r_by, ylim=c(0,1.25*max(r_by)),xlim=c(0,1.25*max(s_obs,na.rm=T)),ylab="Recruits",xlab="Spawners")
points(s_by,r_pred_p,pch=3,col="red")
#points(s_by,r_pred_l,pch=2,col="green")
lines(s_vec,r_vec_p_low,col="red",lty=2)
lines(s_vec,r_vec_p_high,col="red",lty=3)
#lines(s_vec,r_vec,col="green",lty=2)

plot(s_by,LNRS, ylim=c(0,1.25*max(LNRS)),xlim=c(0,1.25*max(s_obs,na.rm=T)),ylab="ln(Recruits/Spawners)",xlab="Spawners")
points(s_by,log(r_pred_p/s_by),pch=3,col="red")
lines(s_vec,log(r_vec_p_low/s_vec),col="red",lty=2)
lines(s_vec,log(r_vec_p_high/s_vec),col="red",lty=3)


n_eq<-1000
n_policy<-1000
#Generate Recruitment deviations (optional)
wt<-rnorm(n_years,0,0.6)						#normaly distributed proicess error

#Initialize populations
hr<-seq(0,1,length=n_policy)
s_eq<-rep(0,length=n_policy)
r_eq<-rep(0,length=n_policy)
yt_eq<-rep(0,length=n_policy)

for(p in 1:n_policy)
{

s<-rep(s_obs,length=n_eq)					#initialize our spawning stock 
r<-rep(NA,length=n_eq)						#vector to hold our recruits
yt<-rep(NA,length=n_eq)						#vector to hold our yeild

for(y in (k+1):n_eq)								#generate a population given HR and other parameters
{
	r[y]<-ricker_predator(s[y-k],ricker_a_p,ricker_b_p,ricker_q_p,pred_obs[35],0)		#generate a recruitments
	s[y]<-r[y]*(1-hr[p])										#spawners are the ones who survive our fishery
	yt[y]<-r[y]*hr[p]											#catch are the ones who dont

}
s_eq[p]<-s[n_eq]
r_eq[p]<-r[n_eq]
yt_eq[p]<-yt[n_eq]

}

plot(hr,s_eq, ylab="Numbers of Coho",xlab="Harvest Rate", type="l", col="blue")
lines(hr,yt_eq,col="blue",lty=2)


for(p in 1:n_policy)
{

s<-rep(s_obs,length=n_eq)					#initialize our spawning stock 
r<-rep(NA,length=n_eq)						#vector to hold our recruits
yt<-rep(NA,length=n_eq)						#vector to hold our yeild

for(y in (k+1):n_eq)								#generate a population given HR and other parameters
{
	r[y]<-ricker_predator(s[y-k],ricker_a_p,ricker_b_p,ricker_q_p,pred_obs[10],0)		#generate a recruitments
	s[y]<-r[y]*(1-hr[p])										#spawners are the ones who survive our fishery
	yt[y]<-r[y]*hr[p]											#catch are the ones who dont

}
s_eq[p]<-s[n_eq]
r_eq[p]<-r[n_eq]
yt_eq[p]<-yt[n_eq]

}

plot(hr,s_eq, ylab="Numbers of Coho",xlab="Harvest Rate", type="l", col="red")
lines(hr,yt_eq,col="red",lty=2)


#Simulate population dynamics




#Get things organized by Brood Year
