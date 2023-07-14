
# load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(popbio)

# load data ---------------------------------------------------------------


# Pull Survey Data from the `HeliconiaSurveys` repository 

urlfile_plants<-("https://raw.githubusercontent.com/BrunaLab/HeliconiaSurveys/master/data/survey_archive/HDP_survey.csv")
ha_plants<-read_csv(url(urlfile_plants))
# write_csv(ha_plants, here("manuscript_files", 
# "MetadataS1", "data_downloaded","ha_plants.csv"))

urlfile_plots<-"https://raw.githubusercontent.com/BrunaLab/HeliconiaSurveys/master/data/survey_archive/HDP_plots.csv"
ha_plots<-read_csv(url(urlfile_plots))
# write_csv(ha_plots,here("manuscript_files", 
# "MetadataS1", "data_downloaded","ha_plots.csv"))

# This is to save the info on the version of the data sets used in the paper 
urlfile_v_plots<-"https://raw.githubusercontent.com/BrunaLab/HeliconiaSurveys/master/data/survey_archive/HDP_plots_version_info.txt"
version_plots<-read_lines(url(urlfile_v_plots))

urlfile_v_survey<-"https://raw.githubusercontent.com/BrunaLab/HeliconiaSurveys/master/data/survey_archive/HDP_survey_version_info.txt"
version_survey<-read_lines(url(urlfile_v_survey))

version_survey$file<-"survey"
names(version_survey)<-c("version", "date", "file")
# version_plots$file<-"plots"
# names(version_plots)<-c("version", "date", "file")
# versions_used<-bind_rows(as_tibble(version_survey),
#                            as_tibble(version_plots)) %>% 
#   relocate(file,.before=1)
# write_csv(versions_used,here("manuscript_files", 
# "MetadataS1", "data_downloaded","versions_for_ms.csv"))

# Pull Data on Flowering from the `Hacuminata_leaf_shoot_height_data` repository 
# these data were for a pilot study on self-fertilization and are useful records of number fof flowers, fruits, etc.
# urlfile_reprodata2<-paste0("https://raw.githubusercontent.com/BrunaLab/Hacuminata_leaf_shoot_height_data/master/data_clean/ha_size_data_1998_cor.csv")
# ha_repro2<-read_csv(url(urlfile_reprodata2))


# create transition matrices ----------------------------------------------

# ha_plants_wide<-ha_plants %>% select(-treefall_status) %>% 
#   pivot_wider(names_from = year, values_from = c(shts:tag_number))

# Put the data in a format from which you can create transition matrices

matrix_data_compiler <- function(ha_plants) {
  
  matrix_data<-tibble(plot_id=ha_plants$plot_id,
         plant_id=ha_plants$plant_id,
         year=ha_plants$year,
         sdlg=ha_plants$recorded_sdlg,
         stage=ha_plants$shts, 
         infl=ha_plants$infl, 
         yr2 = lead(ha_plants$year),
         fate = lead(ha_plants$shts),
         status=lead(ha_plants$census_status)) %>% 
    arrange(desc(sdlg),plot_id,year,stage,fate,status) 
  
  matrix_data
  
  # reduce to 7 post-seedling size classes
  
  matrix_data<-matrix_data %>% 
    mutate(fate = case_when(
      fate > 6 ~ 7,
      # is.na(stage) ~ "NA",
      .default = as.numeric(fate))) %>% 
    mutate(stage = case_when(
      stage > 6 ~ 7,
      # is.na(stage) ~ "NA",
      .default = as.numeric(stage))) %>%
    mutate(stage = as.character(stage),
           sdlg = as.character(sdlg)) %>% 
    mutate(stage = case_when(
      sdlg == "TRUE" ~ "sdlg",
      # is.na(stage) ~ "NA",
      .default = as.character(stage))) %>% 
    mutate(fate = case_when(
      status == "dead" ~ "dead",
      # is.na(stage) ~ "NA",
      .default = as.character(fate))) %>% 
    mutate(infl=replace_na(infl,0))
    
  return(matrix_data)

}


matrix_data<-matrix_data_compiler(ha_plants)
# calculate the Fertility matrix ------------------------------------------

# OVERALL: all plots and years combined ----------------------------------------
# How many infloresences in each plot in each year?
infl<-matrix_data %>% tally(infl)%>% rename(tot_infl=n)
infl$plot_id<-"all combined"
# how many seedlings appear in each plot in each year?
sdlg<-matrix_data %>% tally(sdlg==TRUE) %>% rename(tot_sdlgs=n)
sdlg$plot_id<-"all combined"
# The number of seedlings is based on the no. of infl. in the PREVIOUS year
repro<-left_join(infl,sdlg) %>% 
  mutate(tot_sdlgs=lead(tot_sdlgs)) %>% 
  mutate(sds_per_infl=tot_sdlgs/tot_infl) %>% 
  mutate(sds_per_infl=round(sds_per_infl,2)) %>% 
  ungroup()
# %>% 
#   select(year,plot_id,sds_per_infl)
str(repro)

# There are some plots x years with no seedlings or no flowering plants, 
# resulting in NaN or InF in seedlings per infl. Replace those values with zero
repro<-repro %>% replace_na(list(sds_per_infl=0)) %>% 
  mutate(sds_per_infl=case_when(
    is.nan(sds_per_infl)~0,
    .default = as.numeric(sds_per_infl))) %>% 
  mutate(sds_per_infl=case_when(
    is.infinite(sds_per_infl)~0,
    .default = as.numeric(sds_per_infl)))

# Add the reproductive data to the matrix data and muliply number of infl
# by the number of seedlings per infl. to get value for how many 
# seedlings in subsequent year
matrix_data$plot_id<-"all combined"
matrix_data<-left_join(matrix_data,repro) %>% 
  relocate(sds_per_infl,.after=infl) %>% 
  mutate(seedling=infl*sds_per_infl) %>% 
  relocate(seedling,.after=sds_per_infl)

# Create a transition matrix for each plot 
plots<-sort(unique(matrix_data$plot_id))
years<-sort(unique(matrix_data$year))


  trans01<-matrix_data %>% 
    filter(stage!=0) %>% 
    filter(fate!=0) %>% 
    select(plot_id,
           plant_id,
           year,
           stage,
           infl,
           fate,
           seedling) %>% 
    mutate(stage=as.factor(stage)) %>%
    mutate(stage=fct_relevel(stage,"sdlg","1","2","3","4","5","6","7")) %>%
    mutate(fate=as.factor(fate)) %>% 
    mutate(fate=fct_relevel(fate,"1","2","3","4","5","6","7","dead")) %>% 
    mutate(seedling=replace_na(seedling,0))
  
  levels(trans01$stage)
  levels(trans01$fate)
  
  tf <- table(trans01$fate, trans01$stage)
  T.mat_all <- prop.table(tf, 2)
  T.mat<-T.mat_all[1:7,]
  F.mat <- T.mat * 0
  F.mat[1, ] <- tapply(trans01$seedling, trans01$stage, mean)
  
  
  
  T.mat<-rbind(F.mat[1, ],T.mat)
  rownames(T.mat)<-c("sdlg","1","2","3","4","5","6","7")
  
  
  # stages <- unique(matrix_data$stage)
  # fates <- unique(matrix_data$fate)
  
  n <- c(5,24,23,21,13,5,2,3)
  p <- pop.projection(T.mat, n, 50)
  p$lambda
  
  eigT.mat <- eigen.analysis(T.mat)
  eigT.mat$lambda1
  
  fundamental.matrix(T.mat)
  eta<-fundamental.matrix(T.mat)
  eta$N
  
  

eta_overall <- eta$meaneta
names(eta_overall)<-c("sdlg","shts_1","shts_2","shts_3","shts_4","shts_5","shts_6","shts_7")
eta_overall<-as.data.frame(eta_overall)
eta_overall$stage <- rownames(eta_overall)

eta_var_overall <- eta$vareta
names(eta_var_overall)<-c("sdlg","shts_1","shts_2","shts_3","shts_4","shts_5","shts_6","shts_7")
eta_var_overall<-as.data.frame(eta_var_overall)
eta_var_overall$stage <- rownames(eta_var_overall)


write_csv(eta_overall, "./eta_overall.csv")
write_csv(eta_var_overall, "./eta_var_overall.csv")




# eta
# var_eta
# eta_long<-pivot_longer(eta,sdlg:shts_7,names_to = "category") %>% rename(mean_years=value)
# eta_var_long<-pivot_longer(var_eta,sdlg:shts_7,names_to = "category") %>% rename(mean_years=value)




# plot-level T, all years combined ----------------------------------------


matrix_data<-matrix_data_compiler(ha_plants)
# How many infloresences in each plot in each year?
infl<-matrix_data %>% group_by(plot_id) %>% tally(infl)%>% rename(tot_infl=n)

# how many seedlings appear in each plot in each year?
sdlg<-matrix_data %>% group_by(plot_id) %>% tally(sdlg==TRUE) %>% rename(tot_sdlgs=n)

# The number of seedlings is based on the no. of infl. in the PREVIOUS year
repro<-left_join(infl,sdlg) %>% 
  mutate(tot_sdlgs=lead(tot_sdlgs)) %>% 
  mutate(sds_per_infl=tot_sdlgs/tot_infl) %>% 
  mutate(sds_per_infl=round(sds_per_infl,2)) %>% 
  ungroup()
# %>% 
#   select(year,plot_id,sds_per_infl)
str(repro)

# There are some plots x years with no seedlings or no flowering plants, 
# resulting in NaN or InF in seedlings per infl. Replace those values with zero
repro<-repro %>% replace_na(list(sds_per_infl=0)) %>% 
  mutate(sds_per_infl=case_when(
    is.nan(sds_per_infl)~0,
    .default = as.numeric(sds_per_infl))) %>% 
  mutate(sds_per_infl=case_when(
    is.infinite(sds_per_infl)~0,
  .default = as.numeric(sds_per_infl)))

# Add the reproductive data to the matrix data and muliply number of infl
# by the number of seedlings per infl. to get value for how many 
# seedlings in subsequent year
matrix_data<-left_join(matrix_data,repro) %>% 
  relocate(sds_per_infl,.after=infl) %>% 
  mutate(seedling=infl*sds_per_infl) %>% 
  relocate(seedling,.after=sds_per_infl)

# Create a transition matrix for each plot 
plots<-sort(unique(matrix_data$plot_id))
years<-sort(unique(matrix_data$year))



data1 <- vector("list", nrow(as.data.frame(plots)))
data2 <- vector("list", nrow(as.data.frame(plots)))


for (i in seq_along(plots)) {
  trans01<-matrix_data %>% 
    filter(plot_id==plots[i]) %>% 
    filter(stage!=0) %>% 
    filter(fate!=0) %>% 
    select(plot_id,
           plant_id,year,
           stage,
           infl,
           fate,
           seedling) %>% 
    mutate(stage=as.factor(stage)) %>%
    mutate(stage=fct_relevel(stage,"sdlg","1","2","3","4","5","6","7")) %>%
    mutate(fate=as.factor(fate)) %>% 
    mutate(fate=fct_relevel(fate,"1","2","3","4","5","6","7","dead")) %>% 
    mutate(seedling=replace_na(seedling,0))
  
  levels(trans01$stage)
  levels(trans01$fate)
  
  tf <- table(trans01$fate, trans01$stage)
  T.mat_all <- prop.table(tf, 2)
  T.mat<-T.mat_all[1:7,]
  F.mat <- T.mat * 0
  F.mat[1, ] <- tapply(trans01$seedling, trans01$stage, mean)
  
  
  
  T.mat<-rbind(F.mat[1, ],T.mat)
  rownames(T.mat)<-c("sdlg","1","2","3","4","5","6","7")
  
  
  # stages <- unique(matrix_data$stage)
  # fates <- unique(matrix_data$fate)
  
  n <- c(5,24,23,21,13,5,2,3)
  p <- pop.projection(T.mat, n, 1000)
  p$lambda
  
  eigT.mat <- eigen.analysis(T.mat)
  eigT.mat$lambda1
  
  fundamental.matrix(T.mat)
  eta<-fundamental.matrix(T.mat)
  eta$N
  eta$meaneta
  data1[i]<-as_tibble(eta$meaneta)
  data2[i]<-as_tibble(eta$vareta)
  
}


plot_eta <- data.frame(t(sapply(data1,c)))
names(plot_eta)<-c("sdlg","shts_1","shts_2","shts_3","shts_4","shts_5","shts_6","shts_7")
plots<-as_tibble(plots) %>% rename(plot=value)
plot_eta<-bind_cols(plots,plot_eta)

plot_eta_var <- data.frame(t(sapply(data2,c)))
names(plot_eta_var)<-c("sdlg","shts_1","shts_2","shts_3","shts_4","shts_5","shts_6","shts_7")
plot_eta_var<-bind_cols(plots,plot_eta_var)

write_csv(plot_eta, "./plot_eta.csv")
write_csv(plot_eta_var, "./plot_eta_var.csv")




# eta
# var_eta
# eta_long<-pivot_longer(eta,sdlg:shts_7,names_to = "category") %>% rename(mean_years=value)
# eta_var_long<-pivot_longer(var_eta,sdlg:shts_7,names_to = "category") %>% rename(mean_years=value)


# # per-year per plot T ----------------------------------------
# matrix_data<-matrix_data_compiler(ha_plants)
# # How many infloresences in each year?
# infl<-matrix_data %>% group_by(year, plot_id) %>% tally(infl)%>% rename(tot_infl=n)
# 
# # how many seedlings appear in each year?
# sdlg<-matrix_data %>% group_by(year, plot_id) %>% tally(sdlg==TRUE) %>% rename(tot_sdlgs=n)
# 
# # The number of seedlings is based on the no. of infl. in the PREVIOUS year
# repro<-left_join(infl,sdlg) %>% 
#   mutate(tot_sdlgs=lead(tot_sdlgs)) %>% 
#   mutate(sds_per_infl=tot_sdlgs/tot_infl) %>% 
#   mutate(sds_per_infl=round(sds_per_infl,2)) %>% 
#   ungroup()
# # %>% 
# #   select(year,plot_id,sds_per_infl)
# str(repro)
# 
# # There are some plots x years with no seedlings or no flowering plants, 
# # resulting in NaN or InF in seedlings per infl. Replace those values with zero
# repro<-repro %>% replace_na(list(sds_per_infl=0)) %>% 
#   mutate(sds_per_infl=case_when(
#     is.nan(sds_per_infl)~0,
#     .default = as.numeric(sds_per_infl))) %>% 
#   mutate(sds_per_infl=case_when(
#     is.infinite(sds_per_infl)~0,
#   .default = as.numeric(sds_per_infl)))
# 
# # Add the reproductive data to the matrix data and muliply number of infl
# # by the number of seedlings per infl. to get value for how many 
# # seedlings in subsequent year
# matrix_data<-left_join(matrix_data,repro) %>% 
#   relocate(sds_per_infl,.after=infl) %>% 
#   mutate(seedling=infl*sds_per_infl) %>% 
#   relocate(seedling,.after=sds_per_infl)
# 
# # Create a transition matrix for each year 
# plots<-sort(unique(matrix_data$plot_id))
# years<-sort(unique(matrix_data$year))
# years<-years[2]:years[(length(years)-1)]
# # 
# # data1 <- list(NA,NA,NA,NA)
# # data1 <- tibble("plot"=NA,"year"=NA,"eta"=as.list(NA),"eta_var"=as.list(NA))
# # data1 <- list("plot"=NA,"year"=NA,"eta"= NA,"eta_var"=NA)
# # data2 <- list("plot"=NA,"year"=NA,"eta"= NA,"eta_var"=NA)
# # 
# # data3<-paste(data1, data1)
# 
# # 
# # data1 <- vector("list", nrow(as.data.frame(plots))*nrow(as.data.frame(years)))
# # data1 <- vector("list", nrow(as.data.frame(plots)))
# # data2 <- vector("list", nrow(as.data.frame(plots)))
# # data3 <- vector("list", nrow(as.data.frame(plots)))
# # data4 <- vector("list", nrow(as.data.frame(years)))
# 
# # i=1
# # j=4
# str(matrix_data)
# for (i in seq_along(years)) {
#   for (j in seq_along(plots)) {
#   trans01<-matrix_data %>% 
#     filter(year==years[i]) %>% 
#     filter(plot_id==plots[j]) %>% 
#     filter(stage!=0) %>% 
#     filter(fate!=0) %>% 
#     select(plot_id,
#            plant_id,
#            year,
#            stage,
#            infl,
#            fate,
#            seedling) %>% 
#     mutate(stage=as.factor(stage)) %>%
#     mutate(stage=fct_relevel(stage,"sdlg","1","2","3","4","5","6","7")) %>%
#     mutate(fate=as.factor(fate)) %>% 
#     mutate(fate=fct_relevel(fate,"1","2","3","4","5","6","7","dead")) %>% 
#     mutate(seedling=replace_na(seedling,0))
#   
#   levels(trans01$stage)
#   levels(trans01$fate)
#   
#   tf <- table(trans01$fate, trans01$stage)
#   T.mat_all <- prop.table(tf, 2)
#   T.mat<-T.mat_all[1:7,]
#   F.mat <- T.mat * 0
#   F.mat[1, ] <- tapply(trans01$seedling, trans01$stage, mean)
#   
#   
#   
#   T.mat<-rbind(F.mat[1, ],T.mat)
#   rownames(T.mat)<-c("sdlg","1","2","3","4","5","6","7")
#   
#   
#   stages <- unique(matrix_data$stage)
#   fates <- unique(matrix_data$fate)
#   
#   n <- c(5,24,23,21,13,5,2,3)
#   p <- pop.projection(T.mat, n, 1000)
#   p$lambda
#   
#   eigT.mat <- eigen.analysis(T.mat)
#   eigT.mat$lambda1
#   
#   fundamental.matrix(T.mat)
#   eta<-fundamental.matrix(T.mat)
#   # eta$N
#   # eta$meaneta
#   # 
#   
#   data$eta<-eta$meaneta
#   data$eta_var<-eta$vareta
#   data<-as.data.frame(data)
#   data$stage<-row.names(data)
#   data$plot<-plots[j]
#   data$year<-years[i]
#   
#   data2<-bind_rows(data2,data)
#   }
# }
# 
# 
# 
# plot_yr_eta <- data.frame(t(sapply(data1,c)))
# names(plot_yr_eta)<-c("sdlg","shts_1","shts_2","shts_3","shts_4","shts_5","shts_6","shts_7")
# plots <- unlist(data3)
# plots<-as_tibble(plots) %>% rename(plot=value)
# plot_yr_eta<-bind_cols(plots,plot_yr_eta)
# 
# plot_yr_eta_var <- data.frame(t(sapply(data2,c)))
# names(plot_yr_eta_var)<-c("sdlg","shts_1","shts_2","shts_3","shts_4","shts_5","shts_6","shts_7")
# plot_yr_eta_var<-bind_cols(plots,plot_yr_eta_var)
# 
# write_csv(plot_yr_eta, "./plot_yr_eta.csv")
# write_csv(plot_yr_eta_var, "./plot_yr_eta_var.csv")
#  
# 
# 
# # How many infloresences in each plot in each year?
# infl<-matrix_data %>% group_by(year, plot_id) %>% tally(infl)%>% rename(tot_infl=n)
# 
# # how many seedlings appear in each plot in each year?
# sdlg<-matrix_data %>% group_by(year, plot_id) %>% tally(sdlg==TRUE) %>% rename(tot_sdlgs=n)
# 
# # The number of seedlings is based on the no. of infl. in the PREVIOUS year
# repro<-left_join(infl,sdlg) %>% 
#   mutate(tot_sdlgs=lead(tot_sdlgs)) %>% 
#   mutate(sds_per_infl=tot_sdlgs/tot_infl) %>% 
#   mutate(sds_per_infl=round(sds_per_infl,2)) %>% 
#   ungroup()
# # %>% 
# #   select(year,plot_id,sds_per_infl)
# str(repro)
