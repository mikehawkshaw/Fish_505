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


#########################################################################################################
#Sub Functions
#########################################################################################################
#Dynamics - Subs


#########################################################################################################
#Graphics - Subs


#########################################################################################################
#Main
#########################################################################################################

