source("ER.r")
# Define PCFA group which are whales seen from 1 June to 30 November except in Puget Sound, Hood Canal, etc
PCFA=ER[ER$Month>=6&!is.na(ER$Month)&ER$Month<=11&!ER$Region%in%c("NPS","SCA","CCA","NAK","MX"),]
oldwhales=unique(PCFA$ID[PCFA$Year<1996])
# Add a field old to indicate the whale had been seen previous to 1996 in the PCFA
PCFA$old=0
PCFA$old[PCFA$ID%in%oldwhales]=1
# reset levels of Region and collapse MUA
PCFA$Region=PCFA$region
PCFA$Region[PCFA$Region%in%c("NWA","SJF")]="MUA"
PCFA$Region=factor(PCFA$Region,levels=c("NCA","SOR","OR","GH+","MUA","SVI","WVI","NBC","SEAK","KAK"))
PCFA$Sreg=factor(PCFA$Sreg)

