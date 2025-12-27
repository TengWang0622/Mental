##########################################################################################################
#
#                                    P   R   O   J   E   C   T
#                                                of
#          Climate hazards and mental health hospitalizations in China: socioeconomic disparities,
#                               demographic drivers, and adaptation
#
###########################################################################################################

# Developed by Teng Wang & Hanxu Shi

# Contact: wang.teng19@alumni.imperial.ac.uk
#          shx@bjmu.edu.cn

# Version - 20240325

# Description: Main Script


# ----------- N O T E ------------------------------------------------------------------------------
# The age≤18 denotes the age ≤19 subgroup. For convenience, we did not update the variable name


############################################
#             Preparation
############################################

library(readxl)
library(tidyverse)
#library(dlnm)
#library(splines)
#library(survival)
#library(mvmeta)
library(dplyr)
library(magrittr)
library(Matrix)

library(foreach)
library(doParallel)
library(progress)

library(lme4)
#library(lmerTest)

library(ape)

library(MatchIt)
library(WeightIt)
library(MASS)
library(cli)
library(readxl)
library(openxlsx)

library(networkD3)
library(dplyr)


# ========================= Loading files ========================= 

# Loading the GDP, Pop, Age & Gender populations
City_GDP_Pop=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/Burden/City_GDP_Pop.rds")

# Loading Disasters
TypeDisaster_province_Formal=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/Disasters_Classified/TypeDisaster_province_Formal Mental.xlsx")
TypeDisaster_city_Formal=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/Disasters_Classified/TypeDisaster_city_Formal Mental.xlsx")
TypeDisaster_county_Formal=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/Disasters_Classified/TypeDisaster_county_Formal Mental.xlsx")
HeatwaveEvent=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/Disasters_Classified/HW_Sum_Loc90.xlsx")

# Loading incidence rates
# (1) Org Respiratory 40
#IncidenceRate=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/IncidenceRate.xlsx")
# (2) Updated Respiratory 0.2
IncidenceRate=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/Burden/IncidenceRate_Updated.xlsx")

# Loading DALYs
DALYs=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/Burden/DALYs.xlsx")

# Loading Medical Expenditure and Reimbursement
ReimbursementAll=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/Burden/ReimbursementAll.xlsx")

# Loading population weighted Incidence rate matrix
# (1) Org Respiratory 40
#IncidenceALL=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/IncidenceSummary_Org.xlsx")
# (2) Updated Respiratory 0.2
IncidenceALL=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/Burden/IncidenceSummary_Updated.xlsx")


# Loading DALYs matrix
DALYsALL=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/Burden/DALYsSummary.xlsx")

# Loading Risk matrix (Note: Risk*0.01)
RiskALL=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/Burden/RiskSummary.xlsx")

# Province county group
GDP=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/GDP.rds")

# Loading Population density
Pop_density=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/2016_2024_CityLevel_Pop.xlsx")


########################################################################################
#                      Build the China_Event dataframe
########################################################################################

# ------------------ Directly load the resilience level from preliminary results ----------------------

CityT0=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/ResilienceLevel/CityT0.rds")
CityT1=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/ResilienceLevel/CityT1.rds")
CityT2=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/ResilienceLevel/CityT2.rds")
CityT3=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/ResilienceLevel/CityT3.rds")

CountyT0=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/ResilienceLevel/CountyT0.rds")
CountyT1=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/ResilienceLevel/CountyT1.rds")
CountyT2=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/ResilienceLevel/CountyT2.rds")
CountyT3=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/ResilienceLevel/CountyT3.rds")

# ========================= Event summary table preparation ========================= 

Pop_exposure_Org=data.frame() # Save population exposure data
Pop_exposure_Age=data.frame()
Pop_exposure_Grow=data.frame()

for(SubYear in 2016:2023){       # ---------------------------------------------------------- Input the years
  
  # Input the burden scenario
  Control_Type='Org'           # Org, Age, Grow --------------------->>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Input
  
  
  Pop_density=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Src/2016_2024_CityLevel_Pop.xlsx")
  
  
  China_Event=City_GDP_Pop
  
  df_Province=TypeDisaster_province_Formal
  df_City=TypeDisaster_city_Formal
  df_County=TypeDisaster_county_Formal
  
  eval(parse(text = paste('df_Province <- df_Province[format(df_Province$Begin, "%Y") == "',SubYear,'", ]',sep="")))
  eval(parse(text = paste('df_City <- df_City[format(df_City$Begin, "%Y") == "',SubYear,'", ]',sep="")))
  eval(parse(text = paste('df_County <- df_County[format(df_County$Begin, "%Y") == "',SubYear,'", ]',sep="")))
  
  
  eval(parse(text = paste('Pop_density$Age18=Pop_density$`<20 years_',SubYear,'`',sep="")))
  eval(parse(text = paste('Pop_density$Age19_44=Pop_density$`20 to 44 years_',SubYear,'`',sep="")))
  eval(parse(text = paste('Pop_density$Age45_64=Pop_density$`45 to 64 years_',SubYear,'`',sep="")))
  eval(parse(text = paste('Pop_density$Age65=Pop_density$`>64 years_',SubYear,'`',sep="")))
  
  # ============== Demographic transition effect =========================
  
  Pop_density$AgeALL=Pop_density$Age18+Pop_density$Age19_44+Pop_density$Age45_64+Pop_density$Age65
  Pop_density$AgeALLRef=Pop_density$`<20 years_2016`+Pop_density$`20 to 44 years_2016`+Pop_density$`45 to 64 years_2016`+Pop_density$`>64 years_2016`
  Pop_density$Scaling=Pop_density$AgeALL/Pop_density$AgeALLRef
  
  sum(Pop_density$AgeALL)
  sum(Pop_density$AgeALLRef)
  sum(Pop_density$AgeALL)/sum(Pop_density$AgeALLRef)
  
  Pop_density$Age18_Ratio=Pop_density$`<20 years_2016`/Pop_density$AgeALLRef
  Pop_density$Age19_44_Ratio=Pop_density$`20 to 44 years_2016`/Pop_density$AgeALLRef
  Pop_density$Age45_64_Ratio=Pop_density$`45 to 64 years_2016`/Pop_density$AgeALLRef
  Pop_density$Age65_Ratio=Pop_density$`>64 years_2016`/Pop_density$AgeALLRef
  
  # Statistics
  Pop_density_before=sum(Pop_density$Age18+Pop_density$Age19_44+Pop_density$Age45_64+Pop_density$Age65)
  Pop_density_Ref_before=sum(Pop_density$AgeALLRef)
  
  
  
  if(Control_Type=='Age'){
    
    Pop_density$Age18=Pop_density$AgeALL*Pop_density$Age18_Ratio
    Pop_density$Age19_44=Pop_density$AgeALL*Pop_density$Age19_44_Ratio
    Pop_density$Age45_64=Pop_density$AgeALL*Pop_density$Age45_64_Ratio
    Pop_density$Age65=Pop_density$AgeALL*Pop_density$Age65_Ratio
    
  }
  
  if(Control_Type=='Grow'){
    
    Pop_density$Age18=Pop_density$Age18/Pop_density$Scaling
    Pop_density$Age19_44=Pop_density$Age19_44/Pop_density$Scaling
    Pop_density$Age45_64=Pop_density$Age45_64/Pop_density$Scaling
    Pop_density$Age65=Pop_density$Age65/Pop_density$Scaling
    
  }
  
  # 找出China_Event中在Pop_density里存在的城市
  cities_to_update <- China_Event$City %in% Pop_density$City
  
  # 对这些城市进行更新
  match_index <- match(China_Event$City[cities_to_update], Pop_density$City)
  China_Event$Age18[cities_to_update] <- Pop_density$Age18[match_index]
  China_Event$Age19_44[cities_to_update] <- Pop_density$Age19_44[match_index]
  China_Event$Age45_64[cities_to_update] <- Pop_density$Age45_64[match_index]
  China_Event$Age65[cities_to_update] <- Pop_density$Age65[match_index]
  
  China_Event$Pop=China_Event$Age18+China_Event$Age19_44+China_Event$Age45_64+China_Event$Age65
  
  sum(China_Event$Pop)
  sum(Pop_density$AgeALL)
  
  sum(Pop_density$AgeALLRef)
  
  
  # Statistics
  Pop_density_after=sum(Pop_density$Age18+Pop_density$Age19_44+Pop_density$Age45_64+Pop_density$Age65)
  Pop_density_after/Pop_density_before
  
  
  # =============== Fill resilience level ================
  
  China_Event$City_Tier=3
  
  is_Tier1=China_Event$City %in% CityT1
  is_Tier2=China_Event$City %in% CityT2
  is_Tier3=China_Event$City %in% CityT3
  
  China_Event$City_Tier=ifelse(is_Tier1, 1, China_Event$City_Tier) # 1 for High resilience
  China_Event$City_Tier=ifelse(is_Tier2, 2, China_Event$City_Tier) # 2 for Moderate resilience
  #China_Event$City_Tier=ifelse(is_Tier3, 3, China_Event$City_Tier) # 3 for Low resilience
  
  #China_Event$City_Tier[which(China_Event$City=='重庆郊县')]=1
  #China_Event$City_Tier[which(China_Event$City=='重庆城区')]=1
  #China_Event$City_Tier[which(China_Event$City_Tier==4)]=3
  
  China_Event$No_Storm=0
  China_Event$No_Flood=0
  China_Event$No_Cyclone=0
  China_Event$No_Drought=0
  #China_Event$No_Heatwave=0
  China_Event$No_WinterStorm=0
  China_Event$No_Sand=0
  
  China_Event$Days_Storm=0
  China_Event$Days_Flood=0
  China_Event$Days_Cyclone=0
  China_Event$Days_Drought=0
  China_Event$Days_Heatwave=0
  China_Event$Days_WinterStorm=0
  China_Event$Days_Sand=0
  
  # ============ Input the number of events by disaster type ============= --------------------------------------------------------------------
  
  Event_ALL=c('Storm', 'Flood', 'Cyclone', 'Drought', 'WinterStorm', 'Sand')
  Event_level="ALL"    # all level - ALL; Billion - B; Million - M
  
  for(k in 1:length(Event_ALL)){
    Event_type=Event_ALL[k]
    
    # Event filter
    df_EProvince=df_Province[df_Province$Disaster==Event_type,]
    df_ECity=df_City[df_City$Disaster==Event_type,]
    df_ECounty=df_County[df_County$Disaster==Event_type,]
    
    # ========= Province =========
    
    No_Event=length(unique(df_EProvince$Events))
    
    if(No_Event>0){
      Judgement_Province=1
      ID=unique(df_EProvince$Events)
      
      Event_info=data.frame()
      for(i in 1:No_Event){
        
        Loc_row=which(df_EProvince$Events==ID[i])
        Group=df_EProvince[Loc_row,]
        Loc=df_EProvince$Province
        StartT=df_EProvince$Begin[Loc_row]
        EndT=df_EProvince$End[Loc_row]
        No_days=difftime(EndT, StartT, units = "days")+1
        Damage=df_EProvince$`Total Damage, Adjusted ('000 US$)`[Loc_row]
        
        
        No_subLoc=nrow(Group)
        
        LocLIST=data.frame()
        for(j in 1:No_subLoc){
          subLoc=Group$Province[j]
          
          AllLoc_row=which(GDP$Province==subLoc)
          subGroup=GDP[AllLoc_row,]
          
          LocLIST_temp=data.frame(EventCity=unique(subGroup$City))
          
          #AllLoc=GDP$County[AllLoc_row]
          #AllGDP=GDP$GDP_t[AllLoc_row]
          
          
          #LocLIST_temp=data.frame(AllLoc,AllGDP)
          LocLIST=rbind(LocLIST,LocLIST_temp)
        }
        
        #LocLIST=na.omit(LocLIST)
        
        #LocLIST$EventID=i
        #LocLIST$Start=StartT[1]
        #LocLIST$End=EndT[1]
        LocLIST$No_days=No_days[1]
        #LocLIST$DamageTotal=Damage[1]
        #LocLIST$DamageCounty=LocLIST$AllGDP/sum(LocLIST$AllGDP)*Damage[1]
        
        Event_info=rbind(Event_info,LocLIST) # Produce the Event information
      }
      
      Event_Province=Event_info
    } else{
      Judgement_Province=0
    }
    
    
    # ========= City =========
    
    No_Event=length(unique(df_ECity$Events))
    
    if(No_Event>0){
      Judgement_City=1
      ID=unique(df_ECity$Events)
      
      Event_info=data.frame()
      for(i in 1:No_Event){
        
        Loc_row=which(df_ECity$Events==ID[i])
        Group=df_ECity[Loc_row,]
        Loc=df_ECity$City
        StartT=df_ECity$Begin[Loc_row]
        EndT=df_ECity$End[Loc_row]
        No_days=difftime(EndT, StartT, units = "days")+1
        Damage=df_ECity$`Total Damage, Adjusted ('000 US$)`[Loc_row]
        
        No_subLoc=nrow(Group)
        
        LocLIST=data.frame()
        for(j in 1:No_subLoc){
          subLoc=Group$City[j]
          
          AllLoc_row=which(GDP$City==subLoc)
          subGroup=GDP[AllLoc_row,]
          AllLoc=GDP$County[AllLoc_row]
          AllGDP=GDP$GDP_t[AllLoc_row]
          
          LocLIST_temp=data.frame(EventCity=unique(subGroup$City))
          
          #LocLIST_temp=data.frame(AllLoc,AllGDP)
          LocLIST=rbind(LocLIST,LocLIST_temp)
        }
        
        #LocLIST=na.omit(LocLIST)
        
        #LocLIST$EventID=i
        #LocLIST$Start=StartT[1]
        #LocLIST$End=EndT[1]
        LocLIST$No_days=No_days[1]
        #LocLIST$DamageTotal=Damage[1]
        #LocLIST$DamageCounty=LocLIST$AllGDP/sum(LocLIST$AllGDP)*Damage[1]
        
        Event_info=rbind(Event_info,LocLIST) # Produce the Event information
      } 
      
      Event_City=Event_info
    } else{
      Judgement_City=0
    }
    
    
    # Combine all the counties
    
    Event_info=data.frame()
    if(Judgement_Province==1){
      Event_info=rbind(Event_info,Event_Province)
    }
    if(Judgement_City==1){
      Event_info=rbind(Event_info,Event_City)
    }  
    #if(Judgement_County==1){
    #  Event_info=rbind(Event_info,Event_County)
    #}  
    
    #quantile(Event_info$DamageCounty,seq(0,1,by=0.1)) # ------------------CHECK
    
    #Event_info_ALL=Event_info
    
    #if(Event_level=="ALL"){
    #  Event_info=Event_info_ALL
    #}
    #if(Event_level=="M"){
    #  Event_info_M=Event_info[Event_info$DamageCounty<=1000,]
    #  Event_info=Event_info[Event_info$DamageCounty<=1000,]
    #  if(nrow(Event_info_M)==0){
    #    print("No M-level event is found!")
    #  }
    #  if(nrow(Event_info_M)>0){
    #    RatioEvent=round(nrow(Event_info_M)/nrow(Event_info_ALL),2)
    #    print(c("The proportion of M-level events",Event_type,RatioEvent))
    #  }
    #}
    #if(Event_level=="B"){
    #  Event_info_B=Event_info[Event_info$DamageCounty>1000,]
    #  Event_info=Event_info[Event_info$DamageCounty>1000,]
    #  if(nrow(Event_info_B)==0){
    #    print("No B-level event is found!")
    #  }
    #  if(nrow(Event_info_B)>0){
    #    RatioEvent=round(nrow(Event_info_B)/nrow(Event_info_ALL),2)
    #    print(c("The proportion of B-level events",Event_type,RatioEvent))
    #  }
    #}
    
    # Remove the NA rows
    
    Event_info=na.omit((Event_info))
    
    for(i in 1:nrow(Event_info)){
      NoRow=which(China_Event$City==Event_info$EventCity[i])
      eval(parse(text = paste('China_Event$No_',Event_type,'[NoRow]=China_Event$No_',Event_type,'[NoRow]+1',sep="")))
      eval(parse(text = paste('China_Event$Days_',Event_type,'[NoRow]=China_Event$No_',Event_type,'[NoRow]+Event_info$No_days',sep="")))
    }
    
  }
  
  # Add the heatwave info into the China_Event data frame
  
  #China_Event=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/China_Event.rds") # This file has already been computed by the upper scripts
  #HeatwaveEvent=read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Disasters_Classified/HW_Sum_Loc90.xlsx") # This file is obtained through the supplementary script - National_Scale_HW_Identification_Loc.R - see README for help
  
  #China_Event$No_Heatwave=0
  
  #HeatwaveEvent=na.omit(HeatwaveEvent)
  #for(i in 1:nrow(HeatwaveEvent)){
  #  China_Event$No_Heatwave[which(China_Event$City==HeatwaveEvent$County[i])]=HeatwaveEvent$NoEvent[i]
  #  China_Event$Days_Heatwave[which(China_Event$City==HeatwaveEvent$County[i])]=HeatwaveEvent$Duration[i]
  #}
  
  #China_Event$No_Sum=China_Event$No_Storm+China_Event$No_Flood+China_Event$No_Cyclone+China_Event$No_Drought+China_Event$No_Heatwave+China_Event$No_WinterStorm+China_Event$No_Sand
  #China_Event$No_Frequency=China_Event$No_Sum/8
  
  #China_Event$Days_Sum=China_Event$Days_Storm+China_Event$Days_Flood+China_Event$Days_Cyclone+China_Event$Days_Drought+China_Event$Days_Heatwave+China_Event$Days_WinterStorm+China_Event$Days_Sand
  #China_Event$Days_AVE=China_Event$Days_Sum/8
  
  

  
  ############################################
  #               PAF
  ############################################
  
  # Time setting
  
  eval(parse(text = paste('Days_Total=as.numeric(difftime(as.Date("',SubYear,'-12-31"), as.Date("',SubYear,'-01-01"), units = "days"))',sep="")))
  
  Window_Length=14
  
  # Group setting
  
  AgeALL=c("Age18","Age19_44","Age45_64","Age65")
  EventALL=c("Storm","Flood","Cyclone")
  
  # Risk setting
  Risk_Storm_Age18=(6.1+2.71+1.04+0.41)*0.01
  Risk_Storm_Age19_44=(3.33+1.71+0.61+0.23)*0.01
  Risk_Storm_Age45_64=(4.41+2.35+0.91+0.2)*0.01
  Risk_Storm_Age65=(7.94+3.2+1.41+0.42)*0.01
  
  
  Risk_Flood_Age18=(6.31+2.03+1.13+0.36)*0.01
  Risk_Flood_Age19_44=(3.43+1.56+0.54+0.16)*0.01
  Risk_Flood_Age45_64=(5.51+1.36+0.77+0.19)*0.01
  Risk_Flood_Age65=(6.95+2.35+1.61+0.47)*0.01
  
  Risk_Cyclone_Age18=(4.39+2.11+0.3+0.04)*0.01
  Risk_Cyclone_Age19_44=(3.54+1.72+0.15+0.16)*0.01
  Risk_Cyclone_Age45_64=(4.78+2.15+0.18+0.07)*0.01
  Risk_Cyclone_Age65=(5.17+3.87+0.41+0.26)*0.01
  
  # ------------------------------------
  # List of variables - China_Event
  # PAF_Storm_Age18
  # Hosp_Storm_Age18
  # DALYs_Storm_Age18
  # China_Event_2016
  # Burden_Overview_2016
  # -------------------------------------
  
  for(i in 1:length(EventALL)){
    Event=EventALL[i]
    
    for(j in 1:length(AgeALL)){
      Age=AgeALL[j]
      
      # PAF
      eval(parse(text = paste('China_Event$PAF_',Event,'_',Age,'=(China_Event$No_',Event,'*Risk_',Event,'_',Age,'*Window_Length)/(China_Event$No_',Event,'*Risk_',Event,'_',Age,'*Window_Length+Days_Total)',sep="")))
      #eval(parse(text = paste('China_Event$PAF_',Event,'_',Age,'min=(China_Event$No_',Event,'*Risk_',Event,'_',Age,'min*Window_Length)/(China_Event$No_',Event,'*Risk_',Event,'_',Age,'min*Window_Length+Days_Total)',sep="")))
      #eval(parse(text = paste('China_Event$PAF_',Event,'_',Age,'min=(China_Event$No_',Event,'*Risk_',Event,'_',Age,'max*Window_Length)/(China_Event$No_',Event,'*Risk_',Event,'_',Age,'max*Window_Length+Days_Total)',sep="")))
      
      # Hospitalizations
      eval(parse(text = paste('China_Event$Hosp_',Event,'_',Age,'=China_Event$',Age,'*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="Mental"),c("',Age,'")])*0.01*China_Event$PAF_',Event,'_',Age,sep="")))
      eval(parse(text = paste('China_Event$Hosp_',Event,'_',Age,'min=China_Event$',Age,'*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="Mental"),c("',Age,'min")])*0.01*China_Event$PAF_',Event,'_',Age,sep="")))
      eval(parse(text = paste('China_Event$Hosp_',Event,'_',Age,'max=China_Event$',Age,'*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="Mental"),c("',Age,'max")])*0.01*China_Event$PAF_',Event,'_',Age,sep="")))
      
      # DALYs
      eval(parse(text = paste('China_Event$DALYs_',Event,'_',Age,'=China_Event$',Age,'*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="Mental"),c("',Age,'")])*0.01*China_Event$PAF_',Event,'_',Age,'*as.numeric(DALYsALL[which(DALYsALL$Disease=="Mental"),c("',Age,'")])',sep="")))
      eval(parse(text = paste('China_Event$DALYs_',Event,'_',Age,'min=China_Event$',Age,'*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="Mental"),c("',Age,'min")])*0.01*China_Event$PAF_',Event,'_',Age,'*as.numeric(DALYsALL[which(DALYsALL$Disease=="Mental"),c("',Age,'min")])',sep="")))
      eval(parse(text = paste('China_Event$DALYs_',Event,'_',Age,'max=China_Event$',Age,'*as.numeric(IncidenceALL[which(IncidenceALL$Disease=="Mental"),c("',Age,'max")])*0.01*China_Event$PAF_',Event,'_',Age,'*as.numeric(DALYsALL[which(DALYsALL$Disease=="Mental"),c("',Age,'max")])',sep="")))
      
      
      # Sum
      eval(parse(text = paste('Hosp_',Event,'_',Age,'=sum(China_Event$Hosp_',Event,'_',Age,')',sep="")))
      eval(parse(text = paste('Hosp_',Event,'_',Age,'min=sum(China_Event$Hosp_',Event,'_',Age,'min)',sep="")))
      eval(parse(text = paste('Hosp_',Event,'_',Age,'max=sum(China_Event$Hosp_',Event,'_',Age,'max)',sep="")))
      
      eval(parse(text = paste('DALYs_',Event,'_',Age,'=sum(China_Event$DALYs_',Event,'_',Age,')',sep="")))
      eval(parse(text = paste('DALYs_',Event,'_',Age,'min=sum(China_Event$DALYs_',Event,'_',Age,'min)',sep="")))
      eval(parse(text = paste('DALYs_',Event,'_',Age,'max=sum(China_Event$DALYs_',Event,'_',Age,'max)',sep="")))

    }
    

  }
  
  
  # Total Hospitalizations
  Hosp_Storm_AgeALL=sum(China_Event$Hosp_Storm_Age18)+sum(China_Event$Hosp_Storm_Age19_44)+sum(China_Event$Hosp_Storm_Age45_64)+sum(China_Event$Hosp_Storm_Age65)
  Hosp_Storm_AgeALLmin=sum(China_Event$Hosp_Storm_Age18min)+sum(China_Event$Hosp_Storm_Age19_44min)+sum(China_Event$Hosp_Storm_Age45_64min)+sum(China_Event$Hosp_Storm_Age65min)
  Hosp_Storm_AgeALLmax=sum(China_Event$Hosp_Storm_Age18max)+sum(China_Event$Hosp_Storm_Age19_44max)+sum(China_Event$Hosp_Storm_Age45_64max)+sum(China_Event$Hosp_Storm_Age65max)
  
  # Total DALYs
  DALYs_Storm_AgeALL=sum(China_Event$DALYs_Storm_Age18)+sum(China_Event$DALYs_Storm_Age19_44)+sum(China_Event$DALYs_Storm_Age45_64)+sum(China_Event$DALYs_Storm_Age65)
  DALYs_Storm_AgeALLmin=sum(China_Event$DALYs_Storm_Age18min)+sum(China_Event$DALYs_Storm_Age19_44min)+sum(China_Event$DALYs_Storm_Age45_64min)+sum(China_Event$DALYs_Storm_Age65min)
  DALYs_Storm_AgeALLmax=sum(China_Event$DALYs_Storm_Age18max)+sum(China_Event$DALYs_Storm_Age19_44max)+sum(China_Event$DALYs_Storm_Age45_64max)+sum(China_Event$DALYs_Storm_Age65max)
  
  # Overview
  
  Burden_Storm=as.data.frame(cbind(Event=c("Storm","Storm","Storm","Storm"),
                                   AgeALL,
                                   Hosp=round(c(Hosp_Storm_Age18,Hosp_Storm_Age19_44,Hosp_Storm_Age45_64,Hosp_Storm_Age65)),
                                   Hosp_min=round(c(Hosp_Storm_Age18min,Hosp_Storm_Age19_44min,Hosp_Storm_Age45_64min,Hosp_Storm_Age65min)),
                                   Hosp_max=round(c(Hosp_Storm_Age18max,Hosp_Storm_Age19_44max,Hosp_Storm_Age45_64max,Hosp_Storm_Age65max)),
                                   DALYs=round(c(DALYs_Storm_Age18,DALYs_Storm_Age19_44,DALYs_Storm_Age45_64,DALYs_Storm_Age65),2),
                                   DALYs_min=round(c(DALYs_Storm_Age18min,DALYs_Storm_Age19_44min,DALYs_Storm_Age45_64min,DALYs_Storm_Age65min),2),
                                   DALYs_max=round(c(DALYs_Storm_Age18max,DALYs_Storm_Age19_44max,DALYs_Storm_Age45_64max,DALYs_Storm_Age65max),2)))
  
  print(Burden_Storm)
  
  Burden_Flood=as.data.frame(cbind(Event=c("Flood","Flood","Flood","Flood"),
                                   AgeALL,
                                   Hosp=round(c(Hosp_Flood_Age18,Hosp_Flood_Age19_44,Hosp_Flood_Age45_64,Hosp_Flood_Age65)),
                                   Hosp_min=round(c(Hosp_Flood_Age18min,Hosp_Flood_Age19_44min,Hosp_Flood_Age45_64min,Hosp_Flood_Age65min)),
                                   Hosp_max=round(c(Hosp_Flood_Age18max,Hosp_Flood_Age19_44max,Hosp_Flood_Age45_64max,Hosp_Flood_Age65max)),
                                   DALYs=round(c(DALYs_Flood_Age18,DALYs_Flood_Age19_44,DALYs_Flood_Age45_64,DALYs_Flood_Age65),2),
                                   DALYs_min=round(c(DALYs_Flood_Age18min,DALYs_Flood_Age19_44min,DALYs_Flood_Age45_64min,DALYs_Flood_Age65min),2),
                                   DALYs_max=round(c(DALYs_Flood_Age18max,DALYs_Flood_Age19_44max,DALYs_Flood_Age45_64max,DALYs_Flood_Age65max),2)))
  
  print(Burden_Flood)
  
  Burden_Cyclone=as.data.frame(cbind(Event=c("Cyclone","Cyclone","Cyclone","Cyclone"),
                                     AgeALL,
                                     Hosp=round(c(Hosp_Cyclone_Age18,Hosp_Cyclone_Age19_44,Hosp_Cyclone_Age45_64,Hosp_Cyclone_Age65)),
                                     Hosp_min=round(c(Hosp_Cyclone_Age18min,Hosp_Cyclone_Age19_44min,Hosp_Cyclone_Age45_64min,Hosp_Cyclone_Age65min)),
                                     Hosp_max=round(c(Hosp_Cyclone_Age18max,Hosp_Cyclone_Age19_44max,Hosp_Cyclone_Age45_64max,Hosp_Cyclone_Age65max)),
                                     DALYs=round(c(DALYs_Cyclone_Age18,DALYs_Cyclone_Age19_44,DALYs_Cyclone_Age45_64,DALYs_Cyclone_Age65),2),
                                     DALYs_min=round(c(DALYs_Cyclone_Age18min,DALYs_Cyclone_Age19_44min,DALYs_Cyclone_Age45_64min,DALYs_Cyclone_Age65min),2),
                                     DALYs_max=round(c(DALYs_Cyclone_Age18max,DALYs_Cyclone_Age19_44max,DALYs_Cyclone_Age45_64max,DALYs_Cyclone_Age65max),2)))
  
  print(Burden_Cyclone)
  
  # ==========================================
  #                Output
  # ==========================================
  Burden_Overview=rbind(Burden_Storm,Burden_Flood,Burden_Cyclone)
  
  # Format transfer
  Burden_Overview$Hosp=as.numeric(Burden_Overview$Hosp)
  Burden_Overview$Hosp_min=as.numeric(Burden_Overview$Hosp_min)
  Burden_Overview$Hosp_max=as.numeric(Burden_Overview$Hosp_max)
  Burden_Overview$DALYs=as.numeric(Burden_Overview$DALYs)
  Burden_Overview$DALYs_min=as.numeric(Burden_Overview$DALYs_min)
  Burden_Overview$DALYs_max=as.numeric(Burden_Overview$DALYs_max)
  
  # ============== Exposure Population Aggregation ====================
  
  for(ii in 1:length(EventALL)){
    Event=EventALL[ii]
    
    eval(parse(text = paste('Event_Row=which(China_Event$No_',Event,'>0)',sep="")))
    
    eval(parse(text = paste('Temp_Pop_exposure=data.frame(Event="',Event,'",Year=',SubYear,',
                               Age18=sum(China_Event$Age18[Event_Row]),
                               Age19_44=sum(China_Event$Age19_44[Event_Row]),
                               Age45_64=sum(China_Event$Age45_64[Event_Row]),
                               Age65=sum(China_Event$Age65[Event_Row]))',sep="")))
    
    Temp_Pop_exposure$PopALL=Temp_Pop_exposure$Age18+Temp_Pop_exposure$Age19_44+Temp_Pop_exposure$Age45_64+Temp_Pop_exposure$Age65
    
    if(Control_Type=='Org'){
      Pop_exposure_Org=rbind(Pop_exposure_Org,Temp_Pop_exposure)
    }
    if(Control_Type=='Age'){
      Pop_exposure_Age=rbind(Pop_exposure_Age,Temp_Pop_exposure)
    }
    if(Control_Type=='Grow'){
      Pop_exposure_Grow=rbind(Pop_exposure_Grow,Temp_Pop_exposure)
    }
    
  }
  
  ############ SAVE ####################
  
  # SubYear dataset
  eval(parse(text = paste('China_Event_',SubYear,'=China_Event',sep="")))
  eval(parse(text = paste('Burden_Overview_',SubYear,'=Burden_Overview',sep="")))
  
  #write.xlsx(Burden_Overview, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Result/Burden/Burden_Overview.xlsx', rowNames = FALSE)
  
  if(Control_Type=='Org'){
    eval(parse(text = paste('write.xlsx(China_Event, "/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Result/Burden/Burden_China_Event_',SubYear,'.xlsx", rowNames = FALSE)',sep="")))
    eval(parse(text = paste('write.xlsx(Burden_Overview, "/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Result/Burden/Burden_Overview_',SubYear,'.xlsx", rowNames = FALSE)',sep="")))
  }
  
  if(Control_Type=='Age'){
    eval(parse(text = paste('write.xlsx(China_Event, "/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Result/Burden/Age/Burden_China_Event_',SubYear,'_Age.xlsx", rowNames = FALSE)',sep="")))
    eval(parse(text = paste('write.xlsx(Burden_Overview, "/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Result/Burden/Age/Burden_Overview_',SubYear,'_Age.xlsx", rowNames = FALSE)',sep="")))
  }
  
  if(Control_Type=='Grow'){
    eval(parse(text = paste('write.xlsx(China_Event, "/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Result/Burden/Grow/Burden_China_Event_',SubYear,'_Grow.xlsx", rowNames = FALSE)',sep="")))
    eval(parse(text = paste('write.xlsx(Burden_Overview, "/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Result/Burden/Grow/Burden_Overview_',SubYear,'_Grow.xlsx", rowNames = FALSE)',sep="")))
  }
  
  
}

Burden_Overview_2016
Burden_Overview_2017
Burden_Overview_2018
Burden_Overview_2019
Burden_Overview_2020
Burden_Overview_2021
Burden_Overview_2022
Burden_Overview_2023

# Overall Burden 2016-2023
Burden_Overview_ALL=data.frame(Event=Burden_Overview_2016$Event,
                               AgeALL=Burden_Overview_2016$AgeALL,
                               Hosp=Burden_Overview_2016$Hosp+Burden_Overview_2017$Hosp+Burden_Overview_2018$Hosp+Burden_Overview_2019$Hosp+Burden_Overview_2020$Hosp+Burden_Overview_2021$Hosp+Burden_Overview_2022$Hosp+Burden_Overview_2023$Hosp,
                               Hosp_min=Burden_Overview_2016$Hosp_min+Burden_Overview_2017$Hosp_min+Burden_Overview_2018$Hosp_min+Burden_Overview_2019$Hosp_min+Burden_Overview_2020$Hosp_min+Burden_Overview_2021$Hosp_min+Burden_Overview_2022$Hosp_min+Burden_Overview_2023$Hosp_min,
                               Hosp_max=Burden_Overview_2016$Hosp_max+Burden_Overview_2017$Hosp_max+Burden_Overview_2018$Hosp_max+Burden_Overview_2019$Hosp_max+Burden_Overview_2020$Hosp_max+Burden_Overview_2021$Hosp_max+Burden_Overview_2022$Hosp_max+Burden_Overview_2023$Hosp_max,
                               DALYs=Burden_Overview_2016$DALYs+Burden_Overview_2017$DALYs+Burden_Overview_2018$DALYs+Burden_Overview_2019$DALYs+Burden_Overview_2020$DALYs+Burden_Overview_2021$DALYs+Burden_Overview_2022$DALYs+Burden_Overview_2023$DALYs,
                               DALYs_min=Burden_Overview_2016$DALYs_min+Burden_Overview_2017$DALYs_min+Burden_Overview_2018$DALYs_min+Burden_Overview_2019$DALYs_min+Burden_Overview_2020$DALYs_min+Burden_Overview_2021$DALYs_min+Burden_Overview_2022$DALYs_min+Burden_Overview_2023$DALYs_min,
                               DALYs_max=Burden_Overview_2016$DALYs_max+Burden_Overview_2017$DALYs_max+Burden_Overview_2018$DALYs_max+Burden_Overview_2019$DALYs_max+Burden_Overview_2020$DALYs_max+Burden_Overview_2021$DALYs_max+Burden_Overview_2022$DALYs_max+Burden_Overview_2023$DALYs_max)


# Annual AVE burden
Burden_Overview_AVE=Burden_Overview_ALL
Burden_Overview_AVE$Hosp=round(Burden_Overview_ALL$Hosp/8)
Burden_Overview_AVE$Hosp_min=round(Burden_Overview_ALL$Hosp_min/8)
Burden_Overview_AVE$Hosp_max=round(Burden_Overview_ALL$Hosp_max/8)
Burden_Overview_AVE$DALYs=round(Burden_Overview_ALL$DALYs/8,2)
Burden_Overview_AVE$DALYs_min=round(Burden_Overview_ALL$DALYs_min/8,2)
Burden_Overview_AVE$DALYs_max=round(Burden_Overview_ALL$DALYs_max/8,2)


if(Control_Type=='Org'){
  eval(parse(text = paste('write.xlsx(Burden_Overview_ALL, "/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Result/Burden/Burden_Overview_2016-2023.xlsx", rowNames = FALSE)',sep="")))
  eval(parse(text = paste('write.xlsx(Burden_Overview_AVE, "/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Result/Burden/Burden_Overview_AVE.xlsx", rowNames = FALSE)',sep="")))
}

if(Control_Type=='Age'){
  eval(parse(text = paste('write.xlsx(Burden_Overview_ALL, "/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Result/Burden/Age/Burden_Overview_2016-2023_Age.xlsx", rowNames = FALSE)',sep="")))
  eval(parse(text = paste('write.xlsx(Burden_Overview_AVE, "/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Result/Burden/Age/Burden_Overview_AVE_Age.xlsx", rowNames = FALSE)',sep="")))
}

if(Control_Type=='Grow'){
  eval(parse(text = paste('write.xlsx(Burden_Overview_ALL, "/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Result/Burden/Grow/Burden_Overview_2016-2023_Grow.xlsx", rowNames = FALSE)',sep="")))
  eval(parse(text = paste('write.xlsx(Burden_Overview_AVE, "/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Mental/Result/Burden/Grow/Burden_Overview_AVE_Grow.xlsx", rowNames = FALSE)',sep="")))
}



######################### Summary #######################################  

# Overall burden 2016-2023
print(Burden_Overview_ALL)
# annual AVE Burden 
print(Burden_Overview_AVE)



# Org 
Burden_Overview_ALL_Org=Burden_Overview_ALL
Burden_Overview_AVE_Org=Burden_Overview_AVE
print(Burden_Overview_AVE_Org)
sum(Burden_Overview_AVE_Org$Hosp)

# Age
Burden_Overview_ALL_Age=Burden_Overview_ALL
Burden_Overview_AVE_Age=Burden_Overview_AVE
print(Burden_Overview_AVE_Age)
sum(Burden_Overview_AVE_Age$Hosp)

# Grow
Burden_Overview_ALL_Grow=Burden_Overview_ALL
Burden_Overview_AVE_Grow=Burden_Overview_AVE
print(Burden_Overview_AVE_Grow)
sum(Burden_Overview_AVE_Grow$Hosp)

# ================================================================================================ !!!!!!!!!!!!!!!!!!!!!

# ---- build the summary function -------------------

Burden_Summary=function(DataSrc){
  
  Storm_Hosp=sum(DataSrc$Hosp[which(DataSrc$Event=='Storm')])
  Storm_Hosp_min=sum(DataSrc$Hosp_min[which(DataSrc$Event=='Storm')])
  Storm_Hosp_max=sum(DataSrc$Hosp_max[which(DataSrc$Event=='Storm')])
  
  Flood_Hosp=sum(DataSrc$Hosp[which(DataSrc$Event=='Flood')])
  Flood_Hosp_min=sum(DataSrc$Hosp_min[which(DataSrc$Event=='Flood')])
  Flood_Hosp_max=sum(DataSrc$Hosp_max[which(DataSrc$Event=='Flood')])
  
  Cyclone_Hosp=sum(DataSrc$Hosp[which(DataSrc$Event=='Cyclone')])
  Cyclone_Hosp_min=sum(DataSrc$Hosp_min[which(DataSrc$Event=='Cyclone')])
  Cyclone_Hosp_max=sum(DataSrc$Hosp_max[which(DataSrc$Event=='Cyclone')])
  
  Storm_DALYs=sum(DataSrc$DALYs[which(DataSrc$Event=='Storm')])
  Storm_DALYs_min=sum(DataSrc$DALYs_min[which(DataSrc$Event=='Storm')])
  Storm_DALYs_max=sum(DataSrc$DALYs_max[which(DataSrc$Event=='Storm')])
  
  Flood_DALYs=sum(DataSrc$DALYs[which(DataSrc$Event=='Flood')])
  Flood_DALYs_min=sum(DataSrc$DALYs_min[which(DataSrc$Event=='Flood')])
  Flood_DALYs_max=sum(DataSrc$DALYs_max[which(DataSrc$Event=='Flood')])
  
  Cyclone_DALYs=sum(DataSrc$DALYs[which(DataSrc$Event=='Cyclone')])
  Cyclone_DALYs_min=sum(DataSrc$DALYs_min[which(DataSrc$Event=='Cyclone')])
  Cyclone_DALYs_max=sum(DataSrc$DALYs_max[which(DataSrc$Event=='Cyclone')])
  
  Output=data.frame(Event=c('Storm','Flood','Cyclone'),
                    Hosp=c(Storm_Hosp,Flood_Hosp,Cyclone_Hosp),
                    Hosp_min=c(Storm_Hosp_min,Flood_Hosp_min,Cyclone_Hosp_min),
                    Hosp_max=c(Storm_Hosp_max,Flood_Hosp_max,Cyclone_Hosp_max),
                    DALYs=c(Storm_DALYs,Flood_DALYs,Cyclone_DALYs),
                    DALYs_min=c(Storm_DALYs_min,Flood_DALYs_min,Cyclone_DALYs_min),
                    DALYs_max=c(Storm_DALYs_max,Flood_DALYs_max,Cyclone_DALYs_max))
  
  Temp_sum=data.frame(Event=c('Sum'),
                      Hosp=sum(Output$Hosp),
                      Hosp_min=sum(Output$Hosp_min),
                      Hosp_max=sum(Output$Hosp_max),
                      DALYs=sum(Output$DALYs),
                      DALYs_min=sum(Output$DALYs_min),
                      DALYs_max=sum(Output$DALYs_max))
  
  Output_final=rbind(Output,Temp_sum)
  
  return(Output_final)
  
}

Summary_Burden_Org=Burden_Summary(Burden_Overview_AVE_Org)
Summary_Burden_Age=Burden_Summary(Burden_Overview_AVE_Age)
Summary_Burden_Grow=Burden_Summary(Burden_Overview_AVE_Grow)

Summary_Overview=data.frame(Event=Summary_Burden_Org$Event,
                            Hosp_Org=Summary_Burden_Org$Hosp,
                            Hosp_Age=Summary_Burden_Age$Hosp,
                            Hosp_Grow=Summary_Burden_Grow$Hosp,
                            Age_Hosp=round((Summary_Burden_Org$Hosp-Summary_Burden_Age$Hosp)/Summary_Burden_Org$Hosp*100,2),
                            Grow_Hosp=round((Summary_Burden_Org$Hosp-Summary_Burden_Grow$Hosp)/Summary_Burden_Org$Hosp*100,2),
                            DALYs_Org=Summary_Burden_Org$DALYs,
                            DALYs_Age=Summary_Burden_Age$DALYs,
                            DALYs_Grow=Summary_Burden_Grow$DALYs,
                            Age_DALYs=round((Summary_Burden_Org$DALYs-Summary_Burden_Age$DALYs)/Summary_Burden_Org$DALYs*100,2),
                            Grow_DALYs=round((Summary_Burden_Org$DALYs-Summary_Burden_Grow$DALYs)/Summary_Burden_Org$DALYs*100,2))

print(Summary_Overview)


# ------- build the population exposure function --------------------

Pop_exposure_Statistics=function(DataSrc){
  
  No_Year=8   # study years for average
  
  Storm_Pop_exp_Age18=sum(DataSrc$Age18[which(DataSrc$Event=="Storm")])/No_Year
  Storm_Pop_exp_Age19_44=sum(DataSrc$Age19_44[which(DataSrc$Event=="Storm")])/No_Year
  Storm_Pop_exp_Age45_64=sum(DataSrc$Age45_64[which(DataSrc$Event=="Storm")])/No_Year
  Storm_Pop_exp_Age65=sum(DataSrc$Age65[which(DataSrc$Event=="Storm")])/No_Year
  
  Flood_Pop_exp_Age18=sum(DataSrc$Age18[which(DataSrc$Event=="Flood")])/No_Year
  Flood_Pop_exp_Age19_44=sum(DataSrc$Age19_44[which(DataSrc$Event=="Flood")])/No_Year
  Flood_Pop_exp_Age45_64=sum(DataSrc$Age45_64[which(DataSrc$Event=="Flood")])/No_Year
  Flood_Pop_exp_Age65=sum(DataSrc$Age65[which(DataSrc$Event=="Flood")])/No_Year
  
  Cyclone_Pop_exp_Age18=sum(DataSrc$Age18[which(DataSrc$Event=="Cyclone")])/No_Year
  Cyclone_Pop_exp_Age19_44=sum(DataSrc$Age19_44[which(DataSrc$Event=="Cyclone")])/No_Year
  Cyclone_Pop_exp_Age45_64=sum(DataSrc$Age45_64[which(DataSrc$Event=="Cyclone")])/No_Year
  Cyclone_Pop_exp_Age65=sum(DataSrc$Age65[which(DataSrc$Event=="Cyclone")])/No_Year
  
  Output=data.frame(Event=c("Storm","Flood","Cyclone"),
                    Age18=c(Storm_Pop_exp_Age18,Flood_Pop_exp_Age18,Cyclone_Pop_exp_Age18),
                    Age19_44=c(Storm_Pop_exp_Age19_44,Flood_Pop_exp_Age19_44,Cyclone_Pop_exp_Age19_44),
                    Age45_64=c(Storm_Pop_exp_Age45_64,Flood_Pop_exp_Age45_64,Cyclone_Pop_exp_Age45_64),
                    Age65=c(Storm_Pop_exp_Age65,Flood_Pop_exp_Age65,Cyclone_Pop_exp_Age65))
  
  Output$ALL=Output$Age18+Output$Age19_44+Output$Age45_64+Output$Age65
  
  return(Output)
}

Summary_Pop_exposure_Org=Pop_exposure_Statistics(Pop_exposure_Org)
Summary_Pop_exposure_Age=Pop_exposure_Statistics(Pop_exposure_Age)
Summary_Pop_exposure_Grow=Pop_exposure_Statistics(Pop_exposure_Grow)

# ------- build the PopAVE burden per 100k populations function --------------------

Burden_PopAVE=Burden_Overview_AVE_Org

for(ii in 1:length(EventALL)){
  
  Event=EventALL[ii]
  
  for(jj in 1:length(AgeALL)){
    
    Age=AgeALL[jj]
    
    eval(parse(text = paste('Pop_temp=Summary_Pop_exposure_Org$',Age,'[which(Summary_Pop_exposure_Org$Event=="',Event,'")]',sep="")))
    
    # Hosp
    eval(parse(text = paste('Burden_PopAVE$Hosp[which(Burden_PopAVE$Event=="',Event,'" & Burden_PopAVE$AgeALL=="',Age,'")]=round(Burden_PopAVE$Hosp[which(Burden_PopAVE$Event=="',Event,'" & Burden_PopAVE$AgeALL=="',Age,'")]/Pop_temp*100000,2)',sep="")))
    eval(parse(text = paste('Burden_PopAVE$Hosp_min[which(Burden_PopAVE$Event=="',Event,'" & Burden_PopAVE$AgeALL=="',Age,'")]=round(Burden_PopAVE$Hosp_min[which(Burden_PopAVE$Event=="',Event,'" & Burden_PopAVE$AgeALL=="',Age,'")]/Pop_temp*100000,2)',sep="")))
    eval(parse(text = paste('Burden_PopAVE$Hosp_max[which(Burden_PopAVE$Event=="',Event,'" & Burden_PopAVE$AgeALL=="',Age,'")]=round(Burden_PopAVE$Hosp_max[which(Burden_PopAVE$Event=="',Event,'" & Burden_PopAVE$AgeALL=="',Age,'")]/Pop_temp*100000,2)',sep="")))
    # DALYs
    eval(parse(text = paste('Burden_PopAVE$DALYs[which(Burden_PopAVE$Event=="',Event,'" & Burden_PopAVE$AgeALL=="',Age,'")]=round(Burden_PopAVE$DALYs[which(Burden_PopAVE$Event=="',Event,'" & Burden_PopAVE$AgeALL=="',Age,'")]/Pop_temp*100000,4)',sep="")))
    eval(parse(text = paste('Burden_PopAVE$DALYs_min[which(Burden_PopAVE$Event=="',Event,'" & Burden_PopAVE$AgeALL=="',Age,'")]=round(Burden_PopAVE$DALYs_min[which(Burden_PopAVE$Event=="',Event,'" & Burden_PopAVE$AgeALL=="',Age,'")]/Pop_temp*100000,4)',sep="")))
    eval(parse(text = paste('Burden_PopAVE$DALYs_max[which(Burden_PopAVE$Event=="',Event,'" & Burden_PopAVE$AgeALL=="',Age,'")]=round(Burden_PopAVE$DALYs_max[which(Burden_PopAVE$Event=="',Event,'" & Burden_PopAVE$AgeALL=="',Age,'")]/Pop_temp*100000,4)',sep="")))
  }
}

print(Burden_PopAVE) # Burdens per 100k pop


################## Print output ##############################


print('Detailed burdens --------------------- Org')
print(Burden_Overview_AVE_Org)
print(Burden_Overview_AVE_Age)
print(Burden_Overview_AVE_Grow)

print('Annual averaged age- and hazard-specific Burdens --------------------- Org')
print(Summary_Burden_Org)

print('Counterfactual simulations considering population Aging --------------------- Age')
print(Summary_Burden_Age)

print('Counterfactual simulations considering population Grow and migration --------------------- Grow')
print(Summary_Burden_Grow)

print('Age and Grow contribution in % value --------------------- Decomposition analysis (%)')
print(Summary_Overview)

print('Detailed Population exposure statistics')
print(Pop_exposure_Org)
print(Pop_exposure_Age)
print(Pop_exposure_Grow)

print('Summary Population exposure')
print(Summary_Pop_exposure_Org)
print(Summary_Pop_exposure_Age)
print(Summary_Pop_exposure_Grow)

sum(Summary_Pop_exposure_Org$Age18+Summary_Pop_exposure_Org$Age65)/sum(Summary_Pop_exposure_Org$ALL)*100

print('Burden per 100000 populations --------------------- Decomposition analysis (%)')
print(Burden_PopAVE)





# Compute the number of hazards

Storm_union <- length(Reduce(union, list(
  unique(TypeDisaster_province_Formal$Events[TypeDisaster_province_Formal$Disaster=="Storm"]),
  unique(TypeDisaster_city_Formal$Events[TypeDisaster_city_Formal$Disaster=="Storm"]),
  unique(TypeDisaster_county_Formal$Events[TypeDisaster_county_Formal$Disaster=="Storm"])
)))

Flood_union <- length(Reduce(union, list(
  unique(TypeDisaster_province_Formal$Events[TypeDisaster_province_Formal$Disaster=="Flood"]),
  unique(TypeDisaster_city_Formal$Events[TypeDisaster_city_Formal$Disaster=="Flood"]),
  unique(TypeDisaster_county_Formal$Events[TypeDisaster_county_Formal$Disaster=="Flood"])
)))

Cyclone_union <- length(Reduce(union, list(
  unique(TypeDisaster_province_Formal$Events[TypeDisaster_province_Formal$Disaster=="Cyclone"]),
  unique(TypeDisaster_city_Formal$Events[TypeDisaster_city_Formal$Disaster=="Cyclone"]),
  unique(TypeDisaster_county_Formal$Events[TypeDisaster_county_Formal$Disaster=="Cyclone"])
)))

