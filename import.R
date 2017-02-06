require(readr)
require(dplyr)
require(tidyr)
require(purrr)
require(lubridate)
source("config.R")


"
The first version of this project will focus only on working with data from the Android app FitNotes by Jason Gay: 
https://play.google.com/store/apps/details?id=com.github.jamesgay.fitnotes
"


merge_fn_logs <- function(f) {
  df <- read_csv(f, col_types = cols(.default = "c"))
  df["File"] <- basename(f)
  # Remove columns with cardio activity (= no reps) and drop related columns
  drop_cols <- c("Distance", "Distance Unit", "Time", "Comment")
  df <- df %>% filter(!is.na(Reps))  
  df <- df %>% select(-one_of(drop_cols)) %>% group_by(Date, Exercise) %>% mutate(Set_Num = row_number()) 
  df <- rename(df, Weight = `Weight (kgs)`)
  return(df)
}

import_from_fitnotes <- function() {
  csv_files <- dir(path=FN_DIR, pattern ="*.csv", full.name=TRUE)
  logs <- csv_files %>% map_df(merge_fn_logs)
  "Deduplicate, if there are multiple rows for the combination of  Date/Exercise/Set_Num then the same workout was logged in multiple CSV files. Source file
  information is then no longer needed."
  logs <- logs %>% group_by(Date, Exercise, Set_Num) %>% filter(row_number() == 1)
  logs <- logs %>% select(-File)
  names(logs) <- toupper(names(logs))
  logs$DATE <- ymd(logs$DATE)
  logs$SOURCE <- "FitNotes"
  utils::write.csv(logs, FN_PATH)
  return(logs)
}


merge_fitocracy_logs <- function(f) {
  df <- read_csv(f, col_types = cols(.default = "c"), col_names=F, skip=1)
  df <- df %>% select(1,2,4,6)
  colnames(df) <- c("EXERCISE", "DATE", "WEIGHT", "REPS")  
  df$REPS <-gsub(" reps", "", df$REPS)
  #df <- df %>% select(`Date (YYYYMMDD)`, Activity, Combined)
  return(df)
}

import_from_fitocracy <- function() {
  csv_files <- dir(path=FC_DIR, pattern ="*.csv", full.name=TRUE)
  logs <- csv_files %>% map_df(merge_fitocracy_logs)
  logs$SOURCE <- "Fitocracy"
  logs$WEIGHT <- as.numeric(logs$WEIGHT)
  logs$REPS <- as.numeric(logs$REPS)
  logs$DATE <- ymd(logs$DATE)
  logs$CATEGORY <- NA
  logs$SET_NUM <- NA
  utils::write.csv(logs, FC_PATH)

  return(logs)
}

fn_logs <- import_from_fitnotes()
fc_logs <- import_from_fitocracy()

logs <- rbind.data.frame(fn_logs, fc_logs)
utils::write.csv(logs, LOG_PATH)