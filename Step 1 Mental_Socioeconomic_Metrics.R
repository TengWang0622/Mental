##########################################################################################################
#
#                                    P   R   O   J   E   C   T
#                                                of
#   Mental and behavioural disorder hospitalizations caused by multiple climate hazards in China
#
###########################################################################################################

# Developed by Teng Wang, Hanxu Shi

# Contact: wang.teng19@alumni.imperial.ac.uk
#          shx@bjmu.edu.cn

# Version - 20240325

# Description: Main Script

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

library(openxlsx)

library(ggridges)
library(viridis)

# ============================ Loading files ============================
# GDP
GDP=readRDS("/Users/teng/Project/Mental/Src/Metrics/Socioeconomic/GDP.rds")


# County profile
CountyProfile=read_excel("/Users/teng/Project/Mental/Src/Metrics/Socioeconomic/CountyProfile.xlsx")

# Additional
Add_Profile=read_excel("/Users/teng/Project/Mental/Src/Metrics/Socioeconomic/City_var_info.xlsx")

########################################################################
#                   Regional Climate Resilience
########################################################################

# Input

df_Profile=CountyProfile

# Manipulation

# ! The five dimensions descriptions:
# Economy - GDP, GDP3Ratio, Budget_payment
# Society - Population, GenderRatio, AgeRatio, EDURatio
# Governance - Employed_PublicSociety, Infrasture_No
# Environment - LandArea, CityRatio, GreenRatio, Infrasture_Road, Infrastructure_Pipe, Enviroment_treatment
# Health workforce - Employed_health, Hospital, MedicalCare

df_City=df_Profile[,c("Province","City","Chinese",
                      "GDP","GDP3Ratio","Budget_payment",                                           # Economy
                      "Population","GenderRatio","AgeRatio","EDURatio",                             # Society
                      "Employed_PublicSociety","Infrastructure_No",                                 # Governance
                      "LandArea","CityRatio","GreenRatio","Road","Pipe","Environment_treatment",    # Environment
                      "Employed_Health","Hospital","MedicalCare")]                                  # Health


Supple_Profile=Add_Profile[,c("Chinese",
                              "GRP_Growth_Rate","Primary_Industry_over_GRP","Secondary_Industry_over_GRP","Budget_Income","Science_Tech_Expenditure","Edu_Expenditure","Deposit_Banking_System","Household_Saving","Loans_Banking_System","Import_Goods","Export_Goods","Ave_Wage_Employed","Domestic_Funded_Enterprise","Foreign_Funded_Enterprise","HK_MC_TW_Funded_Enterprise","Employee_Rate",               # Economy
                              "Natural_Growth_Rate","Pop_Sum","RD_Personnel_Ratio","Pop_Proportion_1_Industry","Pop_Proportion_2_Industry","Pop_Proportion_3_Industry","No_Higher_Edu_Institution","Secondary_School","Primary_School","Scientific_Res_Tech_Service","Management_Water_Conservancy_Env","Households_Other_Service","Traffic_Transport_Service","Hotel_Catering_Service","Info_Trans_Computer_Service","Telephone_Service","Internet_Service","Culture_Sport","Patent",                                   # Society
                              "Production_Distribution_Electricity_Gas_Water","Gov_Concern","Gov_Service","Gov_Integrity_Pos","Gov_Transparency","Grassroots_Organization",
                              "WaterResource_Total","Gas_Supply_Total","Liquified_Petroleum_Gas_Supply_Total","Electricity_Consumption_Total","Bus","Civil_Aviation_Passenger_Traffic","Treatment_SolidWaste","Treatment_Wastewater","Treatment_HouseholdWaste","Emission_Dust","Emission_Wastewater","Emission_SO2","Emission_ND","PM_Condition","Subway","InterSection_Bridge","Lamp","Drainage_Pipe","Env_Protect_Investment","Afforestation","Forest_Management","Wetland","Gas_Investment","Central_Heating","Sewerage_Project","Gardening_Investment","Sanitation_Investment",
                              "Dead_Pop","ED_Visit_Rate","Gov_Financial_Healthcare_Pop","Infectious_Inc_Rate","Infectious_Mortality_Rate","Longevity","Maternal_Healthcare_Management_Rate","Maternal_Antenatal_Exm_Rate","Maternal_Antenatal_Postpartum_Visit_Rate","No_Pension_Insurance","Foodborne_Disease","Price_Index_Medicine","Price_Index_Medical_Service","Consultation_Health","Physician_Workload","Mental_Hosp_Rate","Mental_Outpatient_Rate","GeneralPractice_Outpatient_Rate","Fluoride_Prevention_Coverage","Iodine_Prevention_Rate","Medical_Training_Coverage","Health_Education","Neonatal_Visit_Rate","Children_Visit")]                                  # Health

# Combine the data frames
df_City=merge(df_City, Supple_Profile, by = "Chinese", all = FALSE)

df_City[, 4:ncol(df_City)]=lapply(df_City[, 4:ncol(df_City)], as.numeric)

# Computation ----------------------------------------------------------

df_City$GRP_Growth_Rate=round(df_City$GRP_Growth_Rate,2)
df_City$Natural_Growth_Rate=round(df_City$Natural_Growth_Rate,2)
df_City$WaterResource_Pop=round(df_City$WaterResource_Total/df_City$Population,2)
df_City$Mortality_Rate=round(df_City$Dead_Pop/df_City$Population,2) #per 10000 pop per year
df_City$Gov_Financial_Healthcare_Region=round(df_City$Gov_Financial_Healthcare_Pop*df_City$Population,2)

df_City$Gas_Supply_Pop=round(df_City$Gas_Supply_Total*10000/df_City$Population,2)
df_City$Liquified_Petroleum_Gas_Supply_Pop=round(df_City$Liquified_Petroleum_Gas_Supply_Total*10000/df_City$Population,2)
df_City$Electricity_Consumption_Pop=round(df_City$Electricity_Consumption_Total*1000/df_City$Population,2)

df_City$No_Higher_Edu_Institution_Pop=df_City$No_Higher_Edu_Institution/df_City$Population
df_City$Secondary_School_Pop=round(df_City$Secondary_School/df_City$Population,2)
df_City$Primary_School_Pop=round(df_City$Primary_School/df_City$Population,2)

df_City$No_Higher_Edu_Institution_Area=df_City$No_Higher_Edu_Institution/df_City$LandArea
df_City$Secondary_School_Area=round(df_City$Secondary_School/df_City$LandArea,2)
df_City$Primary_School_Area=round(df_City$Primary_School/df_City$LandArea,2)

df_City$Scientific_Res_Tech_Service_Pop=df_City$Scientific_Res_Tech_Service/df_City$Population
df_City$Management_Water_Conservancy_Env_Pop=df_City$Management_Water_Conservancy_Env/df_City$Population
df_City$Households_Other_Service_Pop=df_City$Households_Other_Service/df_City$Population

df_City$Traffic_Transport_Service_Pop=df_City$Traffic_Transport_Service/df_City$Population
df_City$Hotel_Catering_Service_Pop=df_City$Hotel_Catering_Service/df_City$Population
df_City$Info_Trans_Computer_Service_Pop=df_City$Info_Trans_Computer_Service/df_City$Population

df_City$Emission_Dust_Area=df_City$Emission_Dust/df_City$LandArea
df_City$Emission_Wastewater_Area=df_City$Emission_Wastewater/df_City$LandArea
df_City$Emission_SO2_Area=df_City$Emission_SO2/df_City$LandArea
df_City$Emission_ND_Area=df_City$Emission_ND/df_City$LandArea

df_City$Foodborne_Disease_Pop=df_City$Foodborne_Disease/df_City$Population

df_City$Grassroots_Organization_Area=df_City$Grassroots_Organization/df_City$LandArea

df_City$Consultation_Health_Pop=df_City$Consultation_Health/df_City$Population
df_City$Consultation_Health_Area=df_City$Consultation_Health/df_City$LandArea
df_City$Bus_Area=df_City$Bus/df_City$LandArea

df_City$Medical_Training_Coverage_Area=df_City$Medical_Training_Coverage/df_City$LandArea
df_City$Health_Education_Area=df_City$Health_Education/df_City$LandArea
df_City$Iodine_Prevention_Rate_Area=df_City$Iodine_Prevention_Rate/df_City$LandArea

df_City$Subway_Area=df_City$Subway/df_City$LandArea

df_City$InterSection_Bridge_Area=df_City$InterSection_Bridge/df_City$LandArea
df_City$Lamp_Area=df_City$Lamp/df_City$LandArea
df_City$Drainage_Pipe_Area=df_City$Drainage_Pipe/df_City$LandArea

df_City$Telephone_Service_Pop=df_City$Telephone_Service/df_City$Population
df_City$Internet_Service_Pop=df_City$Internet_Service/df_City$Population

df_City$Env_Protect_Investment_Area=df_City$Env_Protect_Investment/df_City$LandArea
df_City$Afforestation_Area=df_City$Afforestation/df_City$LandArea
df_City$Forest_Management_Area=df_City$Forest_Management/df_City$LandArea
df_City$Wetland_Area=df_City$Wetland/df_City$LandArea

df_City$Gas_Investment_Area=df_City$Gas_Investment/df_City$LandArea
df_City$Central_Heating_Area=df_City$Central_Heating/df_City$LandArea
df_City$Sewerage_Project_Area=df_City$Sewerage_Project/df_City$LandArea
df_City$Gardening_Investment_Area=df_City$Gardening_Investment/df_City$LandArea
df_City$Sanitation_Investment_Area=df_City$Sanitation_Investment/df_City$LandArea

df_City$Culture_Sport_Pop=df_City$Culture_Sport/df_City$Population
df_City$Patent_Pop=df_City$Patent/df_City$Population

# Sequence -----------------------------------------------------------------------

# Regional Climate Resilience
df_City_Org=df_City[,c("GDP","GRP_Growth_Rate",                                                                      # Gross regional product (GRP) ---------------
                       "Primary_Industry_over_GRP","Secondary_Industry_over_GRP","GDP3Ratio",                        # Industry composition
                       "Budget_Income","Budget_payment","Science_Tech_Expenditure","Edu_Expenditure",                # Budget revenue and investment
                       "Deposit_Banking_System","Household_Saving","Loans_Banking_System",                           # Deposits and loans
                       "Import_Goods","Export_Goods",                                                                # Trade relations
                       "Domestic_Funded_Enterprise","HK_MC_TW_Funded_Enterprise","Foreign_Funded_Enterprise",        # Industrial enterprises
                       "Employee_Rate","Ave_Wage_Employed",                                                          # Employment and wage conditions -------------
                       "Population","Natural_Growth_Rate","GenderRatio","AgeRatio",                                  # Population and demography ------------------
                       "Primary_School_Area","Secondary_School_Area","No_Higher_Edu_Institution_Area","EDURatio",    # Education
                       "Pop_Proportion_1_Industry","Pop_Proportion_2_Industry","Pop_Proportion_3_Industry",          # Labour force engagement
                       "Management_Water_Conservancy_Env_Pop","Traffic_Transport_Service_Pop","Hotel_Catering_Service_Pop","Info_Trans_Computer_Service_Pop","Culture_Sport_Pop","Households_Other_Service_Pop",  # Public and household service
                       "RD_Personnel_Ratio","Scientific_Res_Tech_Service_Pop","Patent_Pop",                          # Innovation and research
                       "Telephone_Service_Pop","Internet_Service_Pop",                                               # Social network -----------------------------
                       "Employed_PublicSociety",                                                                     # Management ---------------------------------
                       "Production_Distribution_Electricity_Gas_Water",                                              # In-service staff
                       "Infrastructure_No",                                                                          # Institutions
                       "Gov_Concern",                                                                                # Government concern
                       "Gov_Service",                                                                                # Public service
                       "Gov_Integrity_Pos",                                                                          # Government integrity
                       "Gov_Transparency",                                                                           # Government transparency
                       "Grassroots_Organization_Area",                                                               # Organizations -------------------------------
                       "LandArea","GreenRatio","CityRatio",                                                                                                # Forest, land and urbanization ---------------
                       "Road","Bus_Area","Subway_Area","Civil_Aviation_Passenger_Traffic",                                                                 # Transportation
                       "Pipe","Drainage_Pipe_Area","InterSection_Bridge_Area","Lamp_Area",                                                                 # Municipal facilities
                       "Gas_Investment_Area","Central_Heating_Area","Sewerage_Project_Area","Gardening_Investment_Area","Sanitation_Investment_Area",      # Living condition 
                       "WaterResource_Pop","Electricity_Consumption_Pop","Gas_Supply_Pop","Liquified_Petroleum_Gas_Supply_Pop",                            # Resource and energy
                       "Emission_Dust_Area","Emission_Wastewater_Area","Emission_SO2_Area","Emission_ND_Area","PM_Condition",                              # Emissions
                       "Environment_treatment","Treatment_SolidWaste","Treatment_Wastewater","Treatment_HouseholdWaste",                                   # Pollutant treatment
                       "Env_Protect_Investment_Area","Afforestation_Area","Forest_Management_Area","Wetland_Area",                                         # Preservation efforts ------------------------
                       "Hospital","Employed_Health","MedicalCare","No_Pension_Insurance","Gov_Financial_Healthcare_Region",                                                   # Care systems ----------------------------------
                       "Consultation_Health_Area","Medical_Training_Coverage_Area","Health_Education_Area","Fluoride_Prevention_Coverage","Iodine_Prevention_Rate_Area",      # Preventative interventions
                       "Longevity","Mortality_Rate",                                                                                                                          # Longevity
                       "Price_Index_Medicine","Price_Index_Medical_Service",                                                                                                  # Medical price
                       "ED_Visit_Rate","Physician_Workload","Foodborne_Disease_Pop","GeneralPractice_Outpatient_Rate",                                                        # Physical health
                       "Mental_Hosp_Rate","Mental_Outpatient_Rate",                                                                                                           # Mental health                       
                       "Maternal_Healthcare_Management_Rate","Maternal_Antenatal_Exm_Rate","Maternal_Antenatal_Postpartum_Visit_Rate","Neonatal_Visit_Rate","Children_Visit", # Maternal and child health
                       "Infectious_Inc_Rate","Infectious_Mortality_Rate",                                                                                                     # Infectious disease ----------------------------
                       "Province","City","Chinese")]                                                      


# Socioeconomic development index
df_City_Org=df_City[,c("GDP","GRP_Growth_Rate",                                                                      # Gross regional product (GRP) ---------------
                       "Primary_Industry_over_GRP","Secondary_Industry_over_GRP","GDP3Ratio",                        # Industry composition
                       "Budget_Income","Budget_payment","Science_Tech_Expenditure","Edu_Expenditure",                # Budget revenue and investment
                       "Deposit_Banking_System","Household_Saving","Loans_Banking_System",                           # Deposits and loans
                       "Import_Goods","Export_Goods",                                                                # Trade relations
                       "Domestic_Funded_Enterprise","HK_MC_TW_Funded_Enterprise","Foreign_Funded_Enterprise",        # Industrial enterprises
                       "Employee_Rate","Ave_Wage_Employed",                                                          # Employment and wage conditions -------------
                       "Population","Natural_Growth_Rate","GenderRatio","AgeRatio",                                  # Population and demography ------------------
                       "Primary_School_Area","Secondary_School_Area","No_Higher_Edu_Institution_Area","EDURatio",    # Education
                       "Pop_Proportion_1_Industry","Pop_Proportion_2_Industry","Pop_Proportion_3_Industry",          # Labour force engagement
                       "Management_Water_Conservancy_Env_Pop","Traffic_Transport_Service_Pop","Hotel_Catering_Service_Pop","Info_Trans_Computer_Service_Pop","Culture_Sport_Pop","Households_Other_Service_Pop",  # Public and household service
                       "RD_Personnel_Ratio","Scientific_Res_Tech_Service_Pop","Patent_Pop",                          # Innovation and research
                       "Telephone_Service_Pop","Internet_Service_Pop",                                               # Social network -----------------------------
                       "Province","City","Chinese")]    


###########################################################################
#                      Build the Resilience Metrics
###########################################################################

# ===================== Normalization ===================== 

normalize_data=function(x) {
  lower_bound=quantile(x, 0.1, na.rm = TRUE)
  #lower_bound=0
  upper_bound=quantile(x, 0.9, na.rm = TRUE)
  
  # Set values outside bounds to 0 or 1
  x[x < lower_bound]=lower_bound
  x[x > upper_bound]=upper_bound
  
  # Apply normalization
  x_normalized=(x - lower_bound) / (upper_bound - lower_bound)
  
  return(x_normalized)
}

df_City_Nor=df_City_Org
df_City_Nor[, 1:(ncol(df_City_Nor)-3)]=lapply(df_City_Nor[, 1:(ncol(df_City_Nor)-3)], normalize_data)


# ===================== Weight matrix ===================== 

# Specify the data frame to process

#df_City=df_City_Org
df_City=df_City_Nor

# PCA Computation

df_PCA=prcomp(df_City[,1:(ncol(df_City)-3)],center = TRUE,scale. = TRUE)

summary(df_PCA)

view(df_PCA$rotation)
PCAWeight=as.data.frame(df_PCA$rotation)

#saveRDS(PCAWeight,'/Users/teng/Project/Mental/Result/PCA/PCAWeight.rds')


min(PCAWeight$PC1)
max(PCAWeight$PC1)

sorted_abs_loadings = sort(abs(PCAWeight$PC1))
median = sorted_abs_loadings[21]  # 第21个值
quartile1 = sorted_abs_loadings[11]  # 第11个值

Column_PC1=which(abs(PCAWeight$PC1)>quartile1) # The major indicators
DimALL=length(Column_PC1)

No_PC=1

Resilience = rowSums((as.matrix(df_City[,1:(ncol(df_City)-3)]) %*% as.matrix(PCAWeight[,1:No_PC])))

df_City_SE=df_City
df_City_SE$SE_Score=rowSums((as.matrix(df_City[,Column_PC1]) %*% as.matrix(PCAWeight[Column_PC1,1:No_PC])))

df_City_SE$Ranking_PCA <- rank(-df_City_SE$SE_Score, ties.method = "min")




############## Plot the distribution ################

df_City_SE <- df_City_SE %>%
  mutate(
    # Min-Max 标准化到 [0, 1]
    SE_Score_normalized = (SE_Score - min(SE_Score, na.rm = TRUE)) / 
      (max(SE_Score, na.rm = TRUE) - min(SE_Score, na.rm = TRUE)),
    
    Ranking_Quartile = ntile(Ranking_PCA, 4),
    Ranking_Label = case_when(
      Ranking_Quartile == 1 ~ "Fourth Quartile",          # Reverse the quartile as ranking
      Ranking_Quartile == 2 ~ "Third Quartile",
      Ranking_Quartile == 3 ~ "Second Quartile",
      Ranking_Quartile == 4 ~ "First Quartile"
    ),
    Ranking_Label = factor(Ranking_Label, levels = c(
      "First Quartile",
      "Second Quartile",
      "Third Quartile",
      "Fourth Quartile"
    ))
  )

# 密度图（总体分布）
p1 <- ggplot(df_City_SE, aes(x = SE_Score_normalized)) +
  geom_density(fill = "purple", alpha = 0.7, color = "white", linewidth = 0.5) +
  labs(x = NULL, y = "Density") +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(5, 5, 0, 5)
  )

# 箱线图
p2 <- ggplot(df_City_SE, aes(x = SE_Score_normalized, y = Ranking_Label, fill = Ranking_Label)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 16, outlier.size = 2) +
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Normalized socioeconomic development index",
       y = "") +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(0, 5, 5, 5)
  )

# 组合
combined_plot <- p1 / p2 + 
  plot_layout(heights = c(1, 3)) #+
  #plot_annotation(
  #  title = "City Score Distribution by Ranking Quartiles",
  #  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
  #)

print(combined_plot)







df_City_SE <- df_City_SE %>%
  mutate(
    # Min-Max 标准化到 [0, 1]
    SE_Score_normalized = (SE_Score - min(SE_Score, na.rm = TRUE)) / 
      (max(SE_Score, na.rm = TRUE) - min(SE_Score, na.rm = TRUE)),
    
    Ranking_Quartile = ntile(Ranking_PCA, 4),
    Ranking_Label = case_when(
      Ranking_Quartile == 1 ~ "Fourth Quartile",          # Reverse the quartile as ranking
      Ranking_Quartile == 2 ~ "Third Quartile",
      Ranking_Quartile == 3 ~ "Second Quartile",
      Ranking_Quartile == 4 ~ "First Quartile"
    ),
    Ranking_Label = factor(Ranking_Label, levels = c(
      "First Quartile",
      "Second Quartile",
      "Third Quartile",
      "Fourth Quartile"
    ))
  )

# 密度图（总体分布）
p1 <- ggplot(df_City_SE, aes(x = SE_Score_normalized)) +
  geom_density(fill = "purple", alpha = 0.5, color = "white", linewidth = 0.5) +
  labs(x = NULL, y = "") +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(5, 5, 0, 5)
  )

# 箱线图 - 按顺序给4个颜色（从底部到顶部）
p2 <- ggplot(df_City_SE, aes(x = SE_Score_normalized, y = Ranking_Label, fill = Ranking_Label)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 16, outlier.size = 2) +
  scale_fill_manual(values = c("grey", "grey", "grey", "grey")) +  # 4个颜色，按因子顺序
  labs(x = "Normalized socioeconomic development index",
       y = "") +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.margin = margin(0, 5, 5, 5)
  )

# 组合
combined_plot <- p1 / p2 + 
  plot_layout(heights = c(1, 3))

print(combined_plot)


ggsave("/Users/teng/Project/Mental/Fig/3 SI/ridge_plot.png", plot = combined_plot, width = 10000, height = 10000, units = "px", dpi = 600)


ggsave(
  filename = "/Users/teng/Project/Mental/Fig/3 SI/ridge_plot.png",  # 指定路径和文件名
  plot = combined_plot,
  width = 8,      # 宽度（英寸）
  height = 6,     # 高度（英寸）
  dpi = 600       # 分辨率（用于发表文章建议300-600）
)










df_City_SE <- df_City_SE %>%
  mutate(
    # Min-Max 标准化到 [0, 1]
    SE_Score_normalized = (SE_Score - min(SE_Score)) / (max(SE_Score) - min(SE_Score)),
    
    Ranking_Quartile = ntile(Ranking_PCA, 4),
    Ranking_Label = case_when(
      Ranking_Quartile == 1 ~ "First Quartile (Top 25%)",
      Ranking_Quartile == 2 ~ "Second Quartile (25-50%)",
      Ranking_Quartile == 3 ~ "Third Quartile (50-75%)",
      Ranking_Quartile == 4 ~ "Fourth Quartile (Bottom 25%)"
    ),
    Ranking_Label = factor(Ranking_Label, levels = c(
      "Fourth Quartile (Bottom 25%)",
      "Third Quartile (50-75%)",
      "Second Quartile (25-50%)",
      "First Quartile (Top 25%)"
    ))
  )

ggplot(df_City_SE, aes(x = SE_Score_normalized, y = Ranking_Label, fill = Ranking_Label)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 16, outlier.size = 2) +
  scale_fill_viridis_d(option = "plasma") +
  labs(title = "City Score Distribution by Ranking Quartiles",
       x = "Normalized City Score (0-1 Scale)",
       y = "Ranking Quartile") +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )



# =================================================================================
#           Randomised weighting approach - Monte Carlo simulations
# =================================================================================

#df_City=read_excel("Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Resilience/City_Resilience_Domain_Element_Indicator.xlsx")

CandidateWeight=c(seq(0.0963, 0.2168, length.out = DimALL))  # 0.5, 1, 1.5, 2

df_MC=df_City_SE  # Target df

No_MC=1000

for(i in 1:No_MC){
  
  MCWeight=matrix(sample(CandidateWeight, DimALL, replace = TRUE), ncol = 1)
  
  df_MC$MC_Score=100*rowSums((as.matrix(df_MC[,Column_PC1]) %*% as.matrix(MCWeight[1:DimALL,1:No_PC])))/sum(abs(as.matrix(MCWeight[1:DimALL,1:No_PC])))
  
  eval(parse(text = paste('df_MC$Ranking_',i,'=rank(-df_MC$MC_Score,ties.method = "min")',sep="")))
}

df_MC_sorted=df_MC[order(df_MC$Ranking_PCA),]

df_MC_sorted$lower5=0
df_MC_sorted$upper95=0

for(i in 1:nrow(df_MC_sorted)){
  df_MC_sorted$lower5[i]=quantile(as.numeric(df_MC_sorted[i,((ncol(df_MC_sorted)-999):ncol(df_MC_sorted))]),0.05)
  df_MC_sorted$upper95[i]=quantile(as.numeric(df_MC_sorted[i,((ncol(df_MC_sorted)-999):ncol(df_MC_sorted))]),0.95)
}

write.xlsx(df_MC_sorted, '/Users/teng/Project/Mental/Result/PCA/df_MC_sorted_uniform.xlsx', rowNames = FALSE)




############################ Backup ##############################

# Print out the healthcare system Developed and Underdeveloped regions

Tier=quantile(df_MC_sorted$SE_Score,seq(0,1,by=0.25),na.rm=TRUE)

Tier0=df_MC_sorted[which(df_MC_sorted$SE_Score<=Tier["100%"]),]

Tier1=df_MC_sorted[which(df_MC_sorted$SE_Score>Tier["75%"]),]
Tier2=df_MC_sorted[which(df_MC_sorted$SE_Score<=Tier["75%"]),]

CityT0=Tier0$Chinese
CityT1=Tier1$Chinese
CityT2=Tier2$Chinese

df_MC_sorted$SE_Level=2
df_MC_sorted$SE_Level[which(df_MC_sorted$Chinese %in% CityT1)]=1


# ============ Fill the China_Event form ============ 

China_Event=readRDS("/Users/teng/Project/Mental/Src/Burden/China_Event.rds") # This file has already been computed by the upper scripts

China_Event$City_Tier=2
China_Event$City_Tier[which(China_Event$City %in% CityT1)]=1
China_Event$City_Tier[which(China_Event$City %in% CityT2)]=2

China_Event$City_Tier[which(China_Event$City=="重庆城区")]=1
China_Event$City_Tier[which(China_Event$City=="重庆郊县")]=1

write.xlsx(China_Event, '/Users/teng/Project/Mental/Src/County Ranking/China_Event_SE.xlsx', rowNames = FALSE)

# Print out the results

print(CityT0)
print(CityT1)
print(CityT2)

CountyT0=GDP$County[which(GDP$City %in% Tier0$Chinese)]
CountyT1=GDP$County[which(GDP$City %in% Tier1$Chinese)]
CountyT2=GDP$County[which(GDP$City %in% Tier2$Chinese)]

GDP$SE_Level=2
GDP$SE_Level[which(GDP$County %in% CountyT1)]=1
GDP$SE_Level[which(GDP$County %in% CountyT2)]=2

write.xlsx(GDP, '/Users/teng/Project/Mental/Src/County Ranking/CountySE.xlsx', rowNames = FALSE)

saveRDS(CountyT0,'/Users/teng/Project/Mental/Src/County Ranking/CountyT0.rds')
saveRDS(CountyT1,'/Users/teng/Project/Mental/Src/County Ranking/CountyT1.rds')
saveRDS(CountyT2,'/Users/teng/Project/Mental/Src/County Ranking/CountyT2.rds')

saveRDS(CityT0,'/Users/teng/Project/Mental/Src/County Ranking/CityT0.rds')
saveRDS(CityT1,'/Users/teng/Project/Mental/Src/County Ranking/CityT1.rds')
saveRDS(CityT2,'/Users/teng/Project/Mental/Src/County Ranking/CityT2.rds')


County_SE_Summary=GDP[,c("County","SE_Level")]
County_SE_Summary=County_SE_Summary[!duplicated(County_SE_Summary$County), ]

write.xlsx(County_SE_Summary, '/Users/teng/Project/Mental/Src/County Ranking/County_SE_Summary.xlsx', rowNames = FALSE)




















Dim_Eco_A=1:2
Dim_Eco_B=3:5
Dim_Eco_C=6:9
Dim_Eco_D=10:12
Dim_Eco_E=13:14
Dim_Eco_F=15:17
Dim_Eco_G=18:19

Dim_Soc_A=20:23
Dim_Soc_B=24:27
Dim_Soc_C=28:30
Dim_Soc_D=31:36
Dim_Soc_E=37:39
Dim_Soc_F=40:41

Dim_Gov_A=42:42
Dim_Gov_B=43:43
Dim_Gov_C=44:30
Dim_Gov_D=45:45
Dim_Gov_E=46:46
Dim_Gov_F=47:47
Dim_Gov_G=48:48
Dim_Gov_H=49:49

Dim_Env_A=50:52
Dim_Env_B=53:56
Dim_Env_C=57:60
Dim_Env_D=61:65
Dim_Env_E=66:69
Dim_Env_F=70:74
Dim_Env_G=75:78
Dim_Env_H=79:82

Dim_Hea_A=83:87
Dim_Hea_B=88:92
Dim_Hea_C=93:94
Dim_Hea_D=95:96
Dim_Hea_E=97:100
Dim_Hea_F=101:102
Dim_Hea_G=103:107
Dim_Hea_H=108:109

# ============== PCA weight ==============
df_City$Dim_Eco_A=100*rowSums((as.matrix(df_City[,Dim_Eco_A]) %*% as.matrix(PCAWeight[Dim_Eco_A,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Eco_A,1:No_PC])))
df_City$Dim_Eco_B=100*rowSums((as.matrix(df_City[,Dim_Eco_B]) %*% as.matrix(PCAWeight[Dim_Eco_B,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Eco_B,1:No_PC])))
df_City$Dim_Eco_C=100*rowSums((as.matrix(df_City[,Dim_Eco_C]) %*% as.matrix(PCAWeight[Dim_Eco_C,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Eco_C,1:No_PC])))
df_City$Dim_Eco_D=100*rowSums((as.matrix(df_City[,Dim_Eco_D]) %*% as.matrix(PCAWeight[Dim_Eco_D,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Eco_D,1:No_PC])))
df_City$Dim_Eco_E=100*rowSums((as.matrix(df_City[,Dim_Eco_E]) %*% as.matrix(PCAWeight[Dim_Eco_E,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Eco_E,1:No_PC])))
df_City$Dim_Eco_F=100*rowSums((as.matrix(df_City[,Dim_Eco_F]) %*% as.matrix(PCAWeight[Dim_Eco_F,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Eco_F,1:No_PC])))
df_City$Dim_Eco_G=100*rowSums((as.matrix(df_City[,Dim_Eco_G]) %*% as.matrix(PCAWeight[Dim_Eco_G,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Eco_G,1:No_PC])))

df_City$Dim_Soc_A=100*rowSums((as.matrix(df_City[,Dim_Soc_A]) %*% as.matrix(PCAWeight[Dim_Soc_A,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Soc_A,1:No_PC])))
df_City$Dim_Soc_B=100*rowSums((as.matrix(df_City[,Dim_Soc_B]) %*% as.matrix(PCAWeight[Dim_Soc_B,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Soc_B,1:No_PC])))
df_City$Dim_Soc_C=100*rowSums((as.matrix(df_City[,Dim_Soc_C]) %*% as.matrix(PCAWeight[Dim_Soc_C,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Soc_C,1:No_PC])))
df_City$Dim_Soc_D=100*rowSums((as.matrix(df_City[,Dim_Soc_D]) %*% as.matrix(PCAWeight[Dim_Soc_D,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Soc_D,1:No_PC])))
df_City$Dim_Soc_E=100*rowSums((as.matrix(df_City[,Dim_Soc_E]) %*% as.matrix(PCAWeight[Dim_Soc_E,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Soc_E,1:No_PC])))
df_City$Dim_Soc_F=100*rowSums((as.matrix(df_City[,Dim_Soc_F]) %*% as.matrix(PCAWeight[Dim_Soc_F,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Soc_F,1:No_PC])))

df_City$Dim_Gov_A=100*rowSums((as.matrix(df_City[,Dim_Gov_A]) %*% as.matrix(PCAWeight[Dim_Gov_A,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Gov_A,1:No_PC])))
df_City$Dim_Gov_B=100*rowSums((as.matrix(df_City[,Dim_Gov_B]) %*% as.matrix(PCAWeight[Dim_Gov_B,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Gov_B,1:No_PC])))
df_City$Dim_Gov_C=100*rowSums((as.matrix(df_City[,Dim_Gov_C]) %*% as.matrix(PCAWeight[Dim_Gov_C,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Gov_C,1:No_PC])))
df_City$Dim_Gov_D=100*rowSums((as.matrix(df_City[,Dim_Gov_D]) %*% as.matrix(PCAWeight[Dim_Gov_D,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Gov_D,1:No_PC])))
df_City$Dim_Gov_E=100*rowSums((as.matrix(df_City[,Dim_Gov_E]) %*% as.matrix(PCAWeight[Dim_Gov_E,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Gov_E,1:No_PC])))
df_City$Dim_Gov_F=100*rowSums((as.matrix(df_City[,Dim_Gov_F]) %*% as.matrix(PCAWeight[Dim_Gov_F,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Gov_F,1:No_PC])))
df_City$Dim_Gov_G=100*rowSums((as.matrix(df_City[,Dim_Gov_G]) %*% as.matrix(PCAWeight[Dim_Gov_G,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Gov_G,1:No_PC])))
df_City$Dim_Gov_H=100*rowSums((as.matrix(df_City[,Dim_Gov_H]) %*% as.matrix(PCAWeight[Dim_Gov_H,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Gov_H,1:No_PC])))

df_City$Dim_Env_A=100*rowSums((as.matrix(df_City[,Dim_Env_A]) %*% as.matrix(PCAWeight[Dim_Env_A,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Env_A,1:No_PC])))
df_City$Dim_Env_B=100*rowSums((as.matrix(df_City[,Dim_Env_B]) %*% as.matrix(PCAWeight[Dim_Env_B,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Env_B,1:No_PC])))
df_City$Dim_Env_C=100*rowSums((as.matrix(df_City[,Dim_Env_C]) %*% as.matrix(PCAWeight[Dim_Env_C,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Env_C,1:No_PC])))
df_City$Dim_Env_D=100*rowSums((as.matrix(df_City[,Dim_Env_D]) %*% as.matrix(PCAWeight[Dim_Env_D,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Env_D,1:No_PC])))
df_City$Dim_Env_E=100*rowSums((as.matrix(df_City[,Dim_Env_E]) %*% as.matrix(PCAWeight[Dim_Env_E,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Env_E,1:No_PC])))
df_City$Dim_Env_F=100*rowSums((as.matrix(df_City[,Dim_Env_F]) %*% as.matrix(PCAWeight[Dim_Env_F,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Env_F,1:No_PC])))
df_City$Dim_Env_G=100*rowSums((as.matrix(df_City[,Dim_Env_G]) %*% as.matrix(PCAWeight[Dim_Env_G,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Env_G,1:No_PC])))
df_City$Dim_Env_H=100*rowSums((as.matrix(df_City[,Dim_Env_H]) %*% as.matrix(PCAWeight[Dim_Env_H,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Env_H,1:No_PC])))

df_City$Dim_Hea_A=100*rowSums((as.matrix(df_City[,Dim_Hea_A]) %*% as.matrix(PCAWeight[Dim_Hea_A,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Hea_A,1:No_PC])))
df_City$Dim_Hea_B=100*rowSums((as.matrix(df_City[,Dim_Hea_B]) %*% as.matrix(PCAWeight[Dim_Hea_B,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Hea_B,1:No_PC])))
df_City$Dim_Hea_C=100*rowSums((as.matrix(df_City[,Dim_Hea_C]) %*% as.matrix(PCAWeight[Dim_Hea_C,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Hea_C,1:No_PC])))
df_City$Dim_Hea_D=100*rowSums((as.matrix(df_City[,Dim_Hea_D]) %*% as.matrix(PCAWeight[Dim_Hea_D,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Hea_D,1:No_PC])))
df_City$Dim_Hea_E=100*rowSums((as.matrix(df_City[,Dim_Hea_E]) %*% as.matrix(PCAWeight[Dim_Hea_E,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Hea_E,1:No_PC])))
df_City$Dim_Hea_F=100*rowSums((as.matrix(df_City[,Dim_Hea_F]) %*% as.matrix(PCAWeight[Dim_Hea_F,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Hea_F,1:No_PC])))
df_City$Dim_Hea_G=100*rowSums((as.matrix(df_City[,Dim_Hea_G]) %*% as.matrix(PCAWeight[Dim_Hea_G,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Hea_G,1:No_PC])))
df_City$Dim_Hea_H=100*rowSums((as.matrix(df_City[,Dim_Hea_H]) %*% as.matrix(PCAWeight[Dim_Hea_H,1:No_PC])))/sum(abs(as.matrix(PCAWeight[Dim_Hea_H,1:No_PC])))


# ============== Equal weight =================
df_City$Dim_Eco_A=100*rowSums((as.matrix(df_City[,Dim_Eco_A]) %*% (as.matrix(PCAWeight[Dim_Eco_A,1:No_PC])/as.matrix(PCAWeight[Dim_Eco_A,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Eco_A,1:No_PC])/as.matrix(PCAWeight[Dim_Eco_A,1:No_PC])))
df_City$Dim_Eco_B=100*rowSums((as.matrix(df_City[,Dim_Eco_B]) %*% (as.matrix(PCAWeight[Dim_Eco_B,1:No_PC])/as.matrix(PCAWeight[Dim_Eco_B,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Eco_B,1:No_PC])/as.matrix(PCAWeight[Dim_Eco_B,1:No_PC])))
df_City$Dim_Eco_C=100*rowSums((as.matrix(df_City[,Dim_Eco_C]) %*% (as.matrix(PCAWeight[Dim_Eco_C,1:No_PC])/as.matrix(PCAWeight[Dim_Eco_C,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Eco_C,1:No_PC])/as.matrix(PCAWeight[Dim_Eco_C,1:No_PC])))
df_City$Dim_Eco_D=100*rowSums((as.matrix(df_City[,Dim_Eco_D]) %*% (as.matrix(PCAWeight[Dim_Eco_D,1:No_PC])/as.matrix(PCAWeight[Dim_Eco_D,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Eco_D,1:No_PC])/as.matrix(PCAWeight[Dim_Eco_D,1:No_PC])))
df_City$Dim_Eco_E=100*rowSums((as.matrix(df_City[,Dim_Eco_E]) %*% (as.matrix(PCAWeight[Dim_Eco_E,1:No_PC])/as.matrix(PCAWeight[Dim_Eco_E,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Eco_E,1:No_PC])/as.matrix(PCAWeight[Dim_Eco_E,1:No_PC])))
df_City$Dim_Eco_F=100*rowSums((as.matrix(df_City[,Dim_Eco_F]) %*% (as.matrix(PCAWeight[Dim_Eco_F,1:No_PC])/as.matrix(PCAWeight[Dim_Eco_F,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Eco_F,1:No_PC])/as.matrix(PCAWeight[Dim_Eco_F,1:No_PC])))
df_City$Dim_Eco_G=100*rowSums((as.matrix(df_City[,Dim_Eco_G]) %*% (as.matrix(PCAWeight[Dim_Eco_G,1:No_PC])/as.matrix(PCAWeight[Dim_Eco_G,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Eco_G,1:No_PC])/as.matrix(PCAWeight[Dim_Eco_G,1:No_PC])))

df_City$Dim_Soc_A=100*rowSums((as.matrix(df_City[,Dim_Soc_A]) %*% (as.matrix(PCAWeight[Dim_Soc_A,1:No_PC])/as.matrix(PCAWeight[Dim_Soc_A,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Soc_A,1:No_PC])/as.matrix(PCAWeight[Dim_Soc_A,1:No_PC])))
df_City$Dim_Soc_B=100*rowSums((as.matrix(df_City[,Dim_Soc_B]) %*% (as.matrix(PCAWeight[Dim_Soc_B,1:No_PC])/as.matrix(PCAWeight[Dim_Soc_B,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Soc_B,1:No_PC])/as.matrix(PCAWeight[Dim_Soc_B,1:No_PC])))
df_City$Dim_Soc_C=100*rowSums((as.matrix(df_City[,Dim_Soc_C]) %*% (as.matrix(PCAWeight[Dim_Soc_C,1:No_PC])/as.matrix(PCAWeight[Dim_Soc_C,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Soc_C,1:No_PC])/as.matrix(PCAWeight[Dim_Soc_C,1:No_PC])))
df_City$Dim_Soc_D=100*rowSums((as.matrix(df_City[,Dim_Soc_D]) %*% (as.matrix(PCAWeight[Dim_Soc_D,1:No_PC])/as.matrix(PCAWeight[Dim_Soc_D,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Soc_D,1:No_PC])/as.matrix(PCAWeight[Dim_Soc_D,1:No_PC])))
df_City$Dim_Soc_E=100*rowSums((as.matrix(df_City[,Dim_Soc_E]) %*% (as.matrix(PCAWeight[Dim_Soc_E,1:No_PC])/as.matrix(PCAWeight[Dim_Soc_E,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Soc_E,1:No_PC])/as.matrix(PCAWeight[Dim_Soc_E,1:No_PC])))
df_City$Dim_Soc_F=100*rowSums((as.matrix(df_City[,Dim_Soc_F]) %*% (as.matrix(PCAWeight[Dim_Soc_F,1:No_PC])/as.matrix(PCAWeight[Dim_Soc_F,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Soc_F,1:No_PC])/as.matrix(PCAWeight[Dim_Soc_F,1:No_PC])))

df_City$Dim_Gov_A=100*rowSums((as.matrix(df_City[,Dim_Gov_A]) %*% (as.matrix(PCAWeight[Dim_Gov_A,1:No_PC])/as.matrix(PCAWeight[Dim_Gov_A,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Gov_A,1:No_PC])/as.matrix(PCAWeight[Dim_Gov_A,1:No_PC])))
df_City$Dim_Gov_B=100*rowSums((as.matrix(df_City[,Dim_Gov_B]) %*% (as.matrix(PCAWeight[Dim_Gov_B,1:No_PC])/as.matrix(PCAWeight[Dim_Gov_B,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Gov_B,1:No_PC])/as.matrix(PCAWeight[Dim_Gov_B,1:No_PC])))
df_City$Dim_Gov_C=100*rowSums((as.matrix(df_City[,Dim_Gov_C]) %*% (as.matrix(PCAWeight[Dim_Gov_C,1:No_PC])/as.matrix(PCAWeight[Dim_Gov_C,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Gov_C,1:No_PC])/as.matrix(PCAWeight[Dim_Gov_C,1:No_PC])))
df_City$Dim_Gov_D=100*rowSums((as.matrix(df_City[,Dim_Gov_D]) %*% (as.matrix(PCAWeight[Dim_Gov_D,1:No_PC])/as.matrix(PCAWeight[Dim_Gov_D,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Gov_D,1:No_PC])/as.matrix(PCAWeight[Dim_Gov_D,1:No_PC])))
df_City$Dim_Gov_E=100*rowSums((as.matrix(df_City[,Dim_Gov_E]) %*% (as.matrix(PCAWeight[Dim_Gov_E,1:No_PC])/as.matrix(PCAWeight[Dim_Gov_E,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Gov_E,1:No_PC])/as.matrix(PCAWeight[Dim_Gov_E,1:No_PC])))
df_City$Dim_Gov_F=100*rowSums((as.matrix(df_City[,Dim_Gov_F]) %*% (as.matrix(PCAWeight[Dim_Gov_F,1:No_PC])/as.matrix(PCAWeight[Dim_Gov_F,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Gov_F,1:No_PC])/as.matrix(PCAWeight[Dim_Gov_F,1:No_PC])))
df_City$Dim_Gov_G=100*rowSums((as.matrix(df_City[,Dim_Gov_G]) %*% (as.matrix(PCAWeight[Dim_Gov_G,1:No_PC])/as.matrix(PCAWeight[Dim_Gov_G,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Gov_G,1:No_PC])/as.matrix(PCAWeight[Dim_Gov_G,1:No_PC])))
df_City$Dim_Gov_H=100*rowSums((as.matrix(df_City[,Dim_Gov_H]) %*% (as.matrix(PCAWeight[Dim_Gov_H,1:No_PC])/as.matrix(PCAWeight[Dim_Gov_H,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Gov_H,1:No_PC])/as.matrix(PCAWeight[Dim_Gov_H,1:No_PC])))

df_City$Dim_Env_A=100*rowSums((as.matrix(df_City[,Dim_Env_A]) %*% (as.matrix(PCAWeight[Dim_Env_A,1:No_PC])/as.matrix(PCAWeight[Dim_Env_A,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Env_A,1:No_PC])/as.matrix(PCAWeight[Dim_Env_A,1:No_PC])))
df_City$Dim_Env_B=100*rowSums((as.matrix(df_City[,Dim_Env_B]) %*% (as.matrix(PCAWeight[Dim_Env_B,1:No_PC])/as.matrix(PCAWeight[Dim_Env_B,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Env_B,1:No_PC])/as.matrix(PCAWeight[Dim_Env_B,1:No_PC])))
df_City$Dim_Env_C=100*rowSums((as.matrix(df_City[,Dim_Env_C]) %*% (as.matrix(PCAWeight[Dim_Env_C,1:No_PC])/as.matrix(PCAWeight[Dim_Env_C,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Env_C,1:No_PC])/as.matrix(PCAWeight[Dim_Env_C,1:No_PC])))
df_City$Dim_Env_D=100*rowSums((as.matrix(df_City[,Dim_Env_D]) %*% (as.matrix(PCAWeight[Dim_Env_D,1:No_PC])/as.matrix(PCAWeight[Dim_Env_D,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Env_D,1:No_PC])/as.matrix(PCAWeight[Dim_Env_D,1:No_PC])))
df_City$Dim_Env_E=100*rowSums((as.matrix(df_City[,Dim_Env_E]) %*% (as.matrix(PCAWeight[Dim_Env_E,1:No_PC])/as.matrix(PCAWeight[Dim_Env_E,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Env_E,1:No_PC])/as.matrix(PCAWeight[Dim_Env_E,1:No_PC])))
df_City$Dim_Env_F=100*rowSums((as.matrix(df_City[,Dim_Env_F]) %*% (as.matrix(PCAWeight[Dim_Env_F,1:No_PC])/as.matrix(PCAWeight[Dim_Env_F,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Env_F,1:No_PC])/as.matrix(PCAWeight[Dim_Env_F,1:No_PC])))
df_City$Dim_Env_G=100*rowSums((as.matrix(df_City[,Dim_Env_G]) %*% (as.matrix(PCAWeight[Dim_Env_G,1:No_PC])/as.matrix(PCAWeight[Dim_Env_G,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Env_G,1:No_PC])/as.matrix(PCAWeight[Dim_Env_G,1:No_PC])))
df_City$Dim_Env_H=100*rowSums((as.matrix(df_City[,Dim_Env_H]) %*% (as.matrix(PCAWeight[Dim_Env_H,1:No_PC])/as.matrix(PCAWeight[Dim_Env_H,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Env_H,1:No_PC])/as.matrix(PCAWeight[Dim_Env_H,1:No_PC])))

df_City$Dim_Hea_A=100*rowSums((as.matrix(df_City[,Dim_Hea_A]) %*% (as.matrix(PCAWeight[Dim_Hea_A,1:No_PC])/as.matrix(PCAWeight[Dim_Hea_A,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Hea_A,1:No_PC])/as.matrix(PCAWeight[Dim_Hea_A,1:No_PC])))
df_City$Dim_Hea_B=100*rowSums((as.matrix(df_City[,Dim_Hea_B]) %*% (as.matrix(PCAWeight[Dim_Hea_B,1:No_PC])/as.matrix(PCAWeight[Dim_Hea_B,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Hea_B,1:No_PC])/as.matrix(PCAWeight[Dim_Hea_B,1:No_PC])))
df_City$Dim_Hea_C=100*rowSums((as.matrix(df_City[,Dim_Hea_C]) %*% (as.matrix(PCAWeight[Dim_Hea_C,1:No_PC])/as.matrix(PCAWeight[Dim_Hea_C,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Hea_C,1:No_PC])/as.matrix(PCAWeight[Dim_Hea_C,1:No_PC])))
df_City$Dim_Hea_D=100*rowSums((as.matrix(df_City[,Dim_Hea_D]) %*% (as.matrix(PCAWeight[Dim_Hea_D,1:No_PC])/as.matrix(PCAWeight[Dim_Hea_D,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Hea_D,1:No_PC])/as.matrix(PCAWeight[Dim_Hea_D,1:No_PC])))
df_City$Dim_Hea_E=100*rowSums((as.matrix(df_City[,Dim_Hea_E]) %*% (as.matrix(PCAWeight[Dim_Hea_E,1:No_PC])/as.matrix(PCAWeight[Dim_Hea_E,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Hea_E,1:No_PC])/as.matrix(PCAWeight[Dim_Hea_E,1:No_PC])))
df_City$Dim_Hea_F=100*rowSums((as.matrix(df_City[,Dim_Hea_F]) %*% (as.matrix(PCAWeight[Dim_Hea_F,1:No_PC])/as.matrix(PCAWeight[Dim_Hea_F,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Hea_F,1:No_PC])/as.matrix(PCAWeight[Dim_Hea_F,1:No_PC])))
df_City$Dim_Hea_G=100*rowSums((as.matrix(df_City[,Dim_Hea_G]) %*% (as.matrix(PCAWeight[Dim_Hea_G,1:No_PC])/as.matrix(PCAWeight[Dim_Hea_G,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Hea_G,1:No_PC])/as.matrix(PCAWeight[Dim_Hea_G,1:No_PC])))
df_City$Dim_Hea_H=100*rowSums((as.matrix(df_City[,Dim_Hea_H]) %*% (as.matrix(PCAWeight[Dim_Hea_H,1:No_PC])/as.matrix(PCAWeight[Dim_Hea_H,1:No_PC]))))/sum(abs(as.matrix(PCAWeight[Dim_Hea_H,1:No_PC])/as.matrix(PCAWeight[Dim_Hea_H,1:No_PC])))


# Combine the elements to compute the domain score
df_City$Res_Economy=(df_City$Dim_Eco_A+df_City$Dim_Eco_B+df_City$Dim_Eco_C+df_City$Dim_Eco_D+df_City$Dim_Eco_E+df_City$Dim_Eco_F+df_City$Dim_Eco_G)/7
df_City$Res_Society=(df_City$Dim_Soc_A+df_City$Dim_Soc_B+df_City$Dim_Soc_C+df_City$Dim_Soc_D+df_City$Dim_Soc_E+df_City$Dim_Soc_F)/6
df_City$Res_Governance=(df_City$Dim_Gov_A+df_City$Dim_Gov_B+df_City$Dim_Gov_C+df_City$Dim_Gov_D+df_City$Dim_Gov_E+df_City$Dim_Gov_F+df_City$Dim_Gov_G+df_City$Dim_Gov_H)/8
df_City$Res_Environment=(df_City$Dim_Env_A+df_City$Dim_Env_B+df_City$Dim_Env_C+df_City$Dim_Env_D+df_City$Dim_Env_E+df_City$Dim_Env_F+df_City$Dim_Env_G+df_City$Dim_Env_H)/8
df_City$Res_Health=(df_City$Dim_Hea_A+df_City$Dim_Hea_B+df_City$Dim_Hea_C+df_City$Dim_Hea_D+df_City$Dim_Hea_E+df_City$Dim_Hea_F+df_City$Dim_Hea_G+df_City$Dim_Hea_H)/8

df_City$Resilience=(df_City$Res_Economy+df_City$Res_Society+df_City$Res_Governance+df_City$Res_Environment+df_City$Res_Health)/5

# Check
Dim_Economy=19
Dim_Society=22
Dim_Governance=8
Dim_Environment=33
Dim_Health=27

DimALL=Dim_Economy+Dim_Society+Dim_Governance+Dim_Environment+Dim_Health
print(DimALL)

# Quantile
Q_Economy=quantile(df_City$Res_Economy,seq(0,1,by=0.1))
Q_Society=quantile(df_City$Res_Society,seq(0,1,by=0.1))
Q_Governance=quantile(df_City$Res_Governance,seq(0,1,by=0.1))
Q_Environment=quantile(df_City$Res_Environment,seq(0,1,by=0.1))
Q_Health=quantile(df_City$Res_Health,seq(0,1,by=0.1))

Q_Resilience=quantile(df_City$Resilience,seq(0,1,by=0.1))

ScoreName=c("Economy","Society","Governance","Environment","Health","Resilience")

Score_Res_City=as.data.frame(rbind(df_City$Res_Economy,df_City$Res_Society,df_City$Res_Governance,df_City$Res_Environment,df_City$Res_Health,df_City$Resilience))
Score_Res_City=cbind(ScoreName,Score_Res_City)

Percentile_Res=rbind(Q_Economy,Q_Society,Q_Governance,Q_Environment,Q_Health,Q_Resilience)
print(Percentile_Res)

########################################################################################################
#                       Visualization: Plot the score distributions
########################################################################################################

# Create data frame
df_Plot=data.frame(
  Economy = df_City$Res_Economy,
  Society = df_City$Res_Society,
  Governance = df_City$Res_Governance,
  Environment = df_City$Res_Environment,
  Health = df_City$Res_Health,
  Resilience = df_City$Resilience
)

# Reshape and prepare data
df_Plot=df_Plot %>%
  gather(key = "text", value = "value") %>%
  mutate(text = gsub("\\.", " ", text)) %>%
  mutate(value = round(as.numeric(value), 4)) %>%
  filter(text %in% c("Economy", "Society", "Governance", "Environment", "Health", "Resilience"))

# Define custom colors
custom_colors=c(
  "Economy" = "red",
  "Society" = "orange",
  "Governance" = "gold",
  "Environment" = "#3CB371",
  "Health" = "blue",
  "Resilience" = "#7F00FF"
)

# Plot with customizations
RidgePlot=df_Plot %>%
  mutate(text = factor(text, levels = c("Health","Environment","Governance","Society","Economy","Resilience"))) %>%
  ggplot(aes(y = text, x = value, fill = text)) +
  geom_density_ridges(alpha = 0.6, color="white",scale=1.75,quantile_lines=TRUE, quantiles=2,bandwidth = 4) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +  # Change background to white
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 6,family="Times New Roman"),
    panel.background = element_rect(color = "black", size = 1),  # Add outer border
    axis.line = element_line(color = "black"),  # Add axis line
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.text.x = element_text(size = 12, family = "Times New Roman"),
    axis.text.y = element_text(size = 12, family = "Times New Roman")
  ) +
  xlab("") +
  ylab("Assigned Probability (%)") +
  xlim(0, 100)  # Set x-axis range from 0 to 100

#ggsave("/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Resilience/ridge_plot.png", plot = RidgePlot, width = 1000, height = 1000, units = "px", dpi = 600)

# =================================== Add the scatters ========================================
# Plot with customizations
RidgePlot=df_Plot %>%
  mutate(text = factor(text, levels = c("Health","Environment","Governance","Society","Economy","Resilience"))) %>%
  ggplot(aes(y = text, x = value, fill = text)) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0, height = 0.1), size = 1.5,aes(color=text)) +
  scale_color_manual(values = custom_colors) +
  geom_density_ridges(alpha = 0.6, color="white",scale=1.75,quantile_lines=TRUE, quantiles=2,bandwidth = 4) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +  # Change background to white
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 6,family="Times New Roman"),
    panel.background = element_rect(color = "black", size = 1),  # Add outer border
    axis.line = element_line(color = "black"),  # Add axis line
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.text.x = element_text(size = 12, family = "Times New Roman"),
    axis.text.y = element_text(size = 12, family = "Times New Roman")
  ) +
  xlab("") +
  ylab("Assigned Probability (%)") +
  xlim(0, 100)  # Set x-axis range from 0 to 100

# =========================================================================
#                       Regional Climate resilience
# =========================================================================

# Print out the healthcare system Developed and Underdeveloped regions
Tier=quantile(df_City$Res_Health,seq(0,1,by=0.25))

Tier0=df_City[which(df_City$Res_Health<=Tier["100%"]),]

Tier1=df_City[which(df_City$Res_Health>Tier["50%"]),]
Tier2=df_City[which(df_City$Res_Health<=Tier["50%"]),]

CityT0=Tier0$Chinese
CityT1=Tier1$Chinese
CityT2=Tier2$Chinese

df_City$Resilience_Level=2
df_City$Resilience_Level[which(df_City$Chinese %in% CityT1)]=1

write.xlsx(df_City, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result_Metrics/City_Resilience_Domain_Element_Indicator.xlsx', rowNames = FALSE)
write.xlsx(PCAWeight, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result_Metrics/PCAWeight.xlsx', rowNames = TRUE)

df_City$Ranking=0
df_City$Ranking=rank(-df_City$Res_Health,ties.method = "min")


# =================================================================================
#           Randomised weighting approach - Monte Carlo simulations
# =================================================================================

#df_City=read_excel("Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Resilience/City_Resilience_Domain_Element_Indicator.xlsx")

CandidateWeight=c(0.5, 1, 1.5)  # 0.5, 1, 1.5, 2

df_MC=df_City

No_MC=1000

for(i in 1:No_MC){
  
  MCWeight=matrix(sample(CandidateWeight, DimALL, replace = TRUE), ncol = 1)
  
  df_MC$Dim_Eco_A=100*rowSums((as.matrix(df_MC[,Dim_Eco_A]) %*% as.matrix(MCWeight[Dim_Eco_A,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Eco_A,1:No_PC])))
  df_MC$Dim_Eco_B=100*rowSums((as.matrix(df_MC[,Dim_Eco_B]) %*% as.matrix(MCWeight[Dim_Eco_B,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Eco_B,1:No_PC])))
  df_MC$Dim_Eco_C=100*rowSums((as.matrix(df_MC[,Dim_Eco_C]) %*% as.matrix(MCWeight[Dim_Eco_C,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Eco_C,1:No_PC])))
  df_MC$Dim_Eco_D=100*rowSums((as.matrix(df_MC[,Dim_Eco_D]) %*% as.matrix(MCWeight[Dim_Eco_D,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Eco_D,1:No_PC])))
  df_MC$Dim_Eco_E=100*rowSums((as.matrix(df_MC[,Dim_Eco_E]) %*% as.matrix(MCWeight[Dim_Eco_E,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Eco_E,1:No_PC])))
  df_MC$Dim_Eco_F=100*rowSums((as.matrix(df_MC[,Dim_Eco_F]) %*% as.matrix(MCWeight[Dim_Eco_F,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Eco_F,1:No_PC])))
  df_MC$Dim_Eco_G=100*rowSums((as.matrix(df_MC[,Dim_Eco_G]) %*% as.matrix(MCWeight[Dim_Eco_G,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Eco_G,1:No_PC])))
  
  df_MC$Dim_Soc_A=100*rowSums((as.matrix(df_MC[,Dim_Soc_A]) %*% as.matrix(MCWeight[Dim_Soc_A,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Soc_A,1:No_PC])))
  df_MC$Dim_Soc_B=100*rowSums((as.matrix(df_MC[,Dim_Soc_B]) %*% as.matrix(MCWeight[Dim_Soc_B,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Soc_B,1:No_PC])))
  df_MC$Dim_Soc_C=100*rowSums((as.matrix(df_MC[,Dim_Soc_C]) %*% as.matrix(MCWeight[Dim_Soc_C,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Soc_C,1:No_PC])))
  df_MC$Dim_Soc_D=100*rowSums((as.matrix(df_MC[,Dim_Soc_D]) %*% as.matrix(MCWeight[Dim_Soc_D,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Soc_D,1:No_PC])))
  df_MC$Dim_Soc_E=100*rowSums((as.matrix(df_MC[,Dim_Soc_E]) %*% as.matrix(MCWeight[Dim_Soc_E,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Soc_E,1:No_PC])))
  df_MC$Dim_Soc_F=100*rowSums((as.matrix(df_MC[,Dim_Soc_F]) %*% as.matrix(MCWeight[Dim_Soc_F,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Soc_F,1:No_PC])))
  
  df_MC$Dim_Gov_A=100*rowSums((as.matrix(df_MC[,Dim_Gov_A]) %*% as.matrix(MCWeight[Dim_Gov_A,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Gov_A,1:No_PC])))
  df_MC$Dim_Gov_B=100*rowSums((as.matrix(df_MC[,Dim_Gov_B]) %*% as.matrix(MCWeight[Dim_Gov_B,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Gov_B,1:No_PC])))
  df_MC$Dim_Gov_C=100*rowSums((as.matrix(df_MC[,Dim_Gov_C]) %*% as.matrix(MCWeight[Dim_Gov_C,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Gov_C,1:No_PC])))
  df_MC$Dim_Gov_D=100*rowSums((as.matrix(df_MC[,Dim_Gov_D]) %*% as.matrix(MCWeight[Dim_Gov_D,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Gov_D,1:No_PC])))
  df_MC$Dim_Gov_E=100*rowSums((as.matrix(df_MC[,Dim_Gov_E]) %*% as.matrix(MCWeight[Dim_Gov_E,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Gov_E,1:No_PC])))
  df_MC$Dim_Gov_F=100*rowSums((as.matrix(df_MC[,Dim_Gov_F]) %*% as.matrix(MCWeight[Dim_Gov_F,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Gov_F,1:No_PC])))
  df_MC$Dim_Gov_G=100*rowSums((as.matrix(df_MC[,Dim_Gov_G]) %*% as.matrix(MCWeight[Dim_Gov_G,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Gov_G,1:No_PC])))
  df_MC$Dim_Gov_H=100*rowSums((as.matrix(df_MC[,Dim_Gov_H]) %*% as.matrix(MCWeight[Dim_Gov_H,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Gov_H,1:No_PC])))
  
  df_MC$Dim_Env_A=100*rowSums((as.matrix(df_MC[,Dim_Env_A]) %*% as.matrix(MCWeight[Dim_Env_A,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Env_A,1:No_PC])))
  df_MC$Dim_Env_B=100*rowSums((as.matrix(df_MC[,Dim_Env_B]) %*% as.matrix(MCWeight[Dim_Env_B,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Env_B,1:No_PC])))
  df_MC$Dim_Env_C=100*rowSums((as.matrix(df_MC[,Dim_Env_C]) %*% as.matrix(MCWeight[Dim_Env_C,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Env_C,1:No_PC])))
  df_MC$Dim_Env_D=100*rowSums((as.matrix(df_MC[,Dim_Env_D]) %*% as.matrix(MCWeight[Dim_Env_D,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Env_D,1:No_PC])))
  df_MC$Dim_Env_E=100*rowSums((as.matrix(df_MC[,Dim_Env_E]) %*% as.matrix(MCWeight[Dim_Env_E,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Env_E,1:No_PC])))
  df_MC$Dim_Env_F=100*rowSums((as.matrix(df_MC[,Dim_Env_F]) %*% as.matrix(MCWeight[Dim_Env_F,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Env_F,1:No_PC])))
  df_MC$Dim_Env_G=100*rowSums((as.matrix(df_MC[,Dim_Env_G]) %*% as.matrix(MCWeight[Dim_Env_G,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Env_G,1:No_PC])))
  df_MC$Dim_Env_H=100*rowSums((as.matrix(df_MC[,Dim_Env_H]) %*% as.matrix(MCWeight[Dim_Env_H,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Env_H,1:No_PC])))
  
  df_MC$Dim_Hea_A=100*rowSums((as.matrix(df_MC[,Dim_Hea_A]) %*% as.matrix(MCWeight[Dim_Hea_A,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Hea_A,1:No_PC])))
  df_MC$Dim_Hea_B=100*rowSums((as.matrix(df_MC[,Dim_Hea_B]) %*% as.matrix(MCWeight[Dim_Hea_B,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Hea_B,1:No_PC])))
  df_MC$Dim_Hea_C=100*rowSums((as.matrix(df_MC[,Dim_Hea_C]) %*% as.matrix(MCWeight[Dim_Hea_C,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Hea_C,1:No_PC])))
  df_MC$Dim_Hea_D=100*rowSums((as.matrix(df_MC[,Dim_Hea_D]) %*% as.matrix(MCWeight[Dim_Hea_D,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Hea_D,1:No_PC])))
  df_MC$Dim_Hea_E=100*rowSums((as.matrix(df_MC[,Dim_Hea_E]) %*% as.matrix(MCWeight[Dim_Hea_E,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Hea_E,1:No_PC])))
  df_MC$Dim_Hea_F=100*rowSums((as.matrix(df_MC[,Dim_Hea_F]) %*% as.matrix(MCWeight[Dim_Hea_F,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Hea_F,1:No_PC])))
  df_MC$Dim_Hea_G=100*rowSums((as.matrix(df_MC[,Dim_Hea_G]) %*% as.matrix(MCWeight[Dim_Hea_G,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Hea_G,1:No_PC])))
  df_MC$Dim_Hea_H=100*rowSums((as.matrix(df_MC[,Dim_Hea_H]) %*% as.matrix(MCWeight[Dim_Hea_H,1:No_PC])))/sum(abs(as.matrix(MCWeight[Dim_Hea_H,1:No_PC])))
  
  # Combine the elements to compute the domain score
  df_MC$Res_Economy=(df_MC$Dim_Eco_A+df_MC$Dim_Eco_B+df_MC$Dim_Eco_C+df_MC$Dim_Eco_D+df_MC$Dim_Eco_E+df_MC$Dim_Eco_F+df_MC$Dim_Eco_G)/7
  df_MC$Res_Society=(df_MC$Dim_Soc_A+df_MC$Dim_Soc_B+df_MC$Dim_Soc_C+df_MC$Dim_Soc_D+df_MC$Dim_Soc_E+df_MC$Dim_Soc_F)/6
  df_MC$Res_Governance=(df_MC$Dim_Gov_A+df_MC$Dim_Gov_B+df_MC$Dim_Gov_C+df_MC$Dim_Gov_D+df_MC$Dim_Gov_E+df_MC$Dim_Gov_F+df_MC$Dim_Gov_G+df_MC$Dim_Gov_H)/8
  df_MC$Res_Environment=(df_MC$Dim_Env_A+df_MC$Dim_Env_B+df_MC$Dim_Env_C+df_MC$Dim_Env_D+df_MC$Dim_Env_E+df_MC$Dim_Env_F+df_MC$Dim_Env_G+df_MC$Dim_Env_H)/8
  df_MC$Res_Health=(df_MC$Dim_Hea_A+df_MC$Dim_Hea_B+df_MC$Dim_Hea_C+df_MC$Dim_Hea_D+df_MC$Dim_Hea_E+df_MC$Dim_Hea_F+df_MC$Dim_Hea_G+df_MC$Dim_Hea_H)/8
  
  df_MC$Resilience=(df_MC$Res_Economy+df_MC$Res_Society+df_MC$Res_Governance+df_MC$Res_Environment+df_MC$Res_Health)/5
  eval(parse(text = paste('df_MC$Ranking_',i,'=rank(-df_MC$Res_Health,ties.method = "min")',sep="")))
}

df_MC_sorted=df_MC[order(df_MC$Ranking),]

df_MC_sorted$lower5=0
df_MC_sorted$upper95=0

for(i in 1:nrow(df_MC_sorted)){
  df_MC_sorted$lower5[i]=quantile(as.numeric(df_MC_sorted[i,((ncol(df_MC_sorted)-999):ncol(df_MC_sorted))]),0.05)
  df_MC_sorted$upper95[i]=quantile(as.numeric(df_MC_sorted[i,((ncol(df_MC_sorted)-999):ncol(df_MC_sorted))]),0.95)
}

write.xlsx(df_MC_sorted, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Result_Metrics/df_MC_sorted.xlsx', rowNames = FALSE)


# ============ Fill the China_Event form ============ 

China_Event=readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Src/Burden/China_Event.rds") # This file has already been computed by the upper scripts

China_Event$City_Tier=2
China_Event$City_Tier[which(China_Event$City %in% CityT1)]=1
China_Event$City_Tier[which(China_Event$City %in% CityT2)]=2

China_Event$City_Tier[which(China_Event$City=="重庆城区")]=1
China_Event$City_Tier[which(China_Event$City=="重庆郊县")]=1

#write.xlsx(China_Event, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Src/Burden/China_Event.xlsx', rowNames = FALSE)

# Print out the results

print(CityT0)
print(CityT1)
print(CityT2)

CountyT0=GDP$County[which(GDP$City %in% Tier0$Chinese)]
CountyT1=GDP$County[which(GDP$City %in% Tier1$Chinese)]
CountyT2=GDP$County[which(GDP$City %in% Tier2$Chinese)]

GDP$Health_Level=2
GDP$Health_Level[which(GDP$County %in% CountyT1)]=1
GDP$Health_Level[which(GDP$County %in% CountyT2)]=2

write.xlsx(GDP, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Src/HealthLevel/CountyHealth.xlsx', rowNames = FALSE)

saveRDS(CountyT0,'/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Src/HealthLevel/CountyT0.rds')
saveRDS(CountyT1,'/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Src/HealthLevel/CountyT1.rds')
saveRDS(CountyT2,'/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Src/HealthLevel/CountyT2.rds')

saveRDS(CityT0,'/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Src/HealthLevel/CityT0.rds')
saveRDS(CityT1,'/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Src/HealthLevel/CityT1.rds')
saveRDS(CityT2,'/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Src/HealthLevel/CityT2.rds')


County_Health_Summary=GDP[,c("County","Health_Level")]
County_Health_Summary=County_Health_Summary[!duplicated(County_Health_Summary$County), ]

write.xlsx(County_Health_Summary, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/Spillover/Src/County_Health_Summary.xlsx', rowNames = FALSE)












# backup
# Average scores
Score_EcoT1=mean(Tier1$Res_Economy)
Score_EcoT2=mean(Tier2$Res_Economy)
Score_EcoT3=mean(Tier3$Res_Economy)

Score_SocT1=mean(Tier1$Res_Society)
Score_SocT2=mean(Tier2$Res_Society)
Score_SocT3=mean(Tier3$Res_Society)

Score_GovT1=mean(Tier1$Res_Governance)
Score_GovT2=mean(Tier2$Res_Governance)
Score_GovT3=mean(Tier3$Res_Governance)

Score_EnvT1=mean(Tier1$Res_Environment)
Score_EnvT2=mean(Tier2$Res_Environment)
Score_EnvT3=mean(Tier3$Res_Environment)

Score_HelthT1=mean(Tier1$Res_Health)
Score_HelthT2=mean(Tier2$Res_Health)
Score_HelthT3=mean(Tier3$Res_Health)

df_Score=data.frame(Region=c("High","Moderate","Low"),Economy=c(Score_EcoT1,Score_EcoT2,Score_EcoT3),Society=c(Score_SocT1,Score_SocT2,Score_SocT3),Governance=c(Score_GovT1,Score_GovT2,Score_GovT3),Environment=c(Score_EnvT1,Score_EnvT2,Score_EnvT3),Health=c(Score_HelthT1,Score_HelthT2,Score_HelthT3))
print(df_Score)

write.xlsx(df_Score, '/Users/wangteng/Library/Mobile Documents/com~apple~CloudDocs/Project/City Resilience/Result_Resilience/df_Score.xlsx', rowNames = FALSE)




