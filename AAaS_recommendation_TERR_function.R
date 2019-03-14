#################################################################
# Author:   D. Bart Masters (d.bart.masters@perkinelmer.com)
# Purpose:  Build a function to apply recommendations in Spotfire in a functional way so I only have to change it in one place
# Date: 3/7/19
#################################################################

# library(sqldf)
library(dplyr)
# library(h2o)
# library(jsonlite)

#import binary from Spotfire
##see instructions 
###https://docs.tibco.com/pub/enterprise-runtime-for-R/4.4.0/doc/html/TIB_terr_Documentation/GUID-6E84BB7E-5562-4625-9946-FA95F764A447.html

#######################################################
#import data for debugging
#######################################################
model_results <- SpotfireData::importDataFromSBDF("C:\\Users\\MasteD17189\\WORK\\AAaS development\\ML_model_results.sbdf")
#shape
# dim(model_results)
#first ten rows
# head(tbl_df(model_results), n=10)
# head(tbl_df(model_results[,'Device Type']), n=10)

#names of columns
#names(model_results)

#another option to try for mode function
# Mode <- function(x, na.rm = FALSE) {
#   if(na.rm){
#     x = x[!is.na(x)]
#   }
#   
#   ux <- unique(x)
#   return(ux[which.max(tabulate(match(x, ux)))])
# }
################################################################################
#put this portion into the spotfire expression
benchmark_reco<-function(pms,global_pms_p_yr,prescribed_pms,equipment_id)
{
  # Create mode function since it's not built in
  getmode <- function(v) {
    uniqv <- unique(na.omit(v))
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  #sum of PMs  
  sum_pm<-sum(pms, na.rm =TRUE)
  #mode of global PMs per year
  mode_global_pm_p_year<-getmode(global_pms_p_yr)
  #avg prescribed PMs
  avg_px_pms<-mean(prescribed_pms, na.rm=TRUE)
  #unique equipment ids
  u_eq_id<-NROW(unique(equipment_id))

  reco<-
    case_when(
    ((sum_pm/u_eq_id)< mode_global_pm_p_year)&(avg_px_pms<=mode_global_pm_p_year)&(avg_px_pms<=(sum_pm /u_eq_id))~"1.Investigate Further: benchmark and model disagree",
    ((sum_pm/u_eq_id)< mode_global_pm_p_year)&(avg_px_pms> mode_global_pm_p_year)&(avg_px_pms> (sum_pm /u_eq_id))~"2.Increase PMs",
    ((sum_pm/u_eq_id)< mode_global_pm_p_year)&(avg_px_pms<=mode_global_pm_p_year)&(avg_px_pms> (sum_pm /u_eq_id))~"3.Increase PMs",
    #
    ((sum_pm/u_eq_id)==mode_global_pm_p_year)&(avg_px_pms> mode_global_pm_p_year)~"4.Investigate Further: Current PMs at benchmark. Model prescribes MORE PMs.",
    ((sum_pm/u_eq_id)==mode_global_pm_p_year)&(avg_px_pms<=mode_global_pm_p_year)~"5.Investigate Further: Current PMs at benchmark. Model prescribes LESS PMs.",
    #
    ((sum_pm/u_eq_id)> mode_global_pm_p_year)&(avg_px_pms> mode_global_pm_p_year)&(avg_px_pms>=(sum_pm/u_eq_id))~"6.Investigate Further: benchmark and model disagree",
    ((sum_pm/u_eq_id)> mode_global_pm_p_year)&(avg_px_pms> mode_global_pm_p_year)&(avg_px_pms< (sum_pm/u_eq_id))~"7.Decrease PMs",
    ((sum_pm/u_eq_id)> mode_global_pm_p_year)&(avg_px_pms<=mode_global_pm_p_year)&(avg_px_pms< (sum_pm/u_eq_id))~"8.Decrease PMs",    
    TRUE~'test_out'#NA_character_  
    )
  print(paste('Actual PMs',toString((sum_pm/u_eq_id)),sep=":"))
  print(paste('Global Mode PMs',toString(mode_global_pm_p_year),sep=":"))
  print(paste('Avg. prescribed PMs',toString(avg_px_pms),sep=":"))
  return(reco)
}
# output<-benchmark_reco(pms=input1,global_pms_p_yr=input2,prescribed_pms=input3,equipment_id=input4)
output<-benchmark_reco(model_results$PM,model_results$'Global PMs per year',model_results$prescribed_pms,model_results$'EQUIPMENT ID')
output
################################################################################
#rm(list=ls(all=TRUE))
