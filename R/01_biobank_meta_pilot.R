
setwd("~/Projects/32_brava_recessive_summary_statistics/brava_recessive_summary_statistics/")

#devtools::install_github("frhl/bravastring")
#library(bravastring)
devtools::load_all("/Users/flassen/Projects/28_bravastring/bravastring")
source("scripts/00_utils.R")
library(data.table)
library(dplyr)


# get paths to files in biobank
ukb <- get_biobank_files_dt("ukb")
bbj <- get_biobank_files_dt("bbj")
gnh <- get_biobank_files_dt("gnh")

# fix various biobank traits manually
gnh$trait[is.na(gnh$trait) & grepl("Uroloth",gnh$full_path)] <- "Urolith"
#gnh$trait[is.na(gnh$trait) & grepl("CK",gnh$full_path)] <- "??"

# combined biobank files
files_dt <- rbind(ukb, bbj, gnh)
files_dt$description <- get_brava_description(files_dt$trait)

# random
files_dt <- files_dt[files_dt$trait %in% "HTN",]
files_dt <- files_dt[files_dt$encoding %in% "recessive",]
files_dt <- files_dt[files_dt$sex %in% "ALL",]

dt <- do.call(rbind, lapply(1:nrow(files_dt), function(i){
  
  biobank <- files_dt$biobank[i]
  read_fun <- ifelse(biobank == "BBJ", fread_regenie, fread_saige)
  d <- read_fun(files_dt$full_path[i])
  d$BIOBANK <- biobank
  d$ANCESTRY <- files_dt$ancestry[i]
  d$TRAIT <- files_dt$trait[i]
  d$SEX <- files_dt$sex[i]
  d$ENCODING <- files_dt$encoding[i]
  d$ANNOTATION <- files_dt$annotation[i]
  return(d)
}))


dt %>% group_by(TRAIT, SEX, ANNOTATION) %>%
  summarise()




files_dt %>% group_by(MarkerID, Annotation)










