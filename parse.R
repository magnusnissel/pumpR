require(plyr)
require(readr)
require(dplyr)
require(tidyr)
require(purrr)
require(lubridate)
require(stringr)
source("config.R")

log_df = read.csv(LOG_PATH)



parse_imp <- function(ex) {
  ex <- tolower(ex)
  if (str_detect(ex, "(barbell|bb )")){
      imp <- "Barbell"}
  else if (str_detect(ex, "(dumbbell|db )")){
      imp <- "Dumbbell"}
  else if (str_detect(ex, "(kettlebell|kb )")){
    imp <- "Dumbbell"}
  else if (str_detect(ex, "(ez-bar |ezbar |ez bar )")){
    imp <- "EZ-Bar"}
  else if (str_detect(ex, "(trap-bar |trapbar |trap bar )")){
    imp <- "Trap-Bar"}
  else if (str_detect(ex, "(football-bar |footballbar |football bar|swiss bar|swiss-bar|swissbar)")){
    imp <- "Football-Bar"}
  else if (str_detect(ex, "(machine|cybex |hammer strength |hammerstrength |hammer-strength )")){
    imp <- "Machine"}
  else if (str_detect(ex, "cable")){
    imp <- "Cable"}
  else if (str_detect(ex, "band ")){
    imp <- "Band"}
  else if (str_detect(ex, "pipe ")){
    imp <- "Pipe"}
  else if (str_detect(ex, "sled ")){
    imp <- "Sled"}
  else if (str_detect(ex, "(suspension trainer|trx) ")){
    imp <- "Suspension Trainer"}
  else if (str_detect(ex, "(body weight |bodyweight |bw )")) {
    imp <- "Bodyweight"
    }
  else {imp <- NA}
  imp
}

parse_implement <- function(dfr) {
  ex <- tolower(dfr$EXERCISE)
  ex <- gsub("-", " ", ex)

  i <- str_detect(ex, "(barbell|snatch grip)")
  dfr[i, "IMPLEMENT"] <- "Barbell"
  i <- str_detect(ex, "dumbbell")
  dfr[i, "IMPLEMENT"] <- "Dumbbell"
  i <- str_detect(ex, "kettlebell")
  dfr[i, "IMPLEMENT"] <- "Kettlebell"
  i <- str_detect(ex, "(ez|curl) bar")
  dfr[i, "IMPLEMENT"] <- "EZ Bar"
  i <- str_detect(ex, "(trap|hex) bar")
  dfr[i, "IMPLEMENT"] <- "Trap Bar"
  i <- str_detect(ex, "(football|swiss) bar")
  dfr[i, "IMPLEMENT"] <- "Football Bar"
  i <- str_detect(ex, "(machine|cybex|hammer strength)")
  dfr[i, "IMPLEMENT"] <- "Machine"
  i <- str_detect(ex, "cable")
  dfr[i, "IMPLEMENT"] <- "Cable"
  i <- str_detect(ex, "sled ")
  dfr[i, "IMPLEMENT"] <- "Sled"
  i <- str_detect(ex, "medicine ball")
  dfr[i, "IMPLEMENT"] <- "Medicine Ball"
  i <- str_detect(ex, "band ")
  dfr[i, "IMPLEMENT"] <- "Band"
  i <- str_detect(ex, "(trx|suspension trainer)")
  dfr[i, "IMPLEMENT"] <- "Suspension Trainer"
  
  
  
  return(dfr)
}

parse_big_four <- function(dfr) {
  dfr[, "BIG_FOUR"] <- "Other"
  ex <- tolower(dfr$EXERCISE)
  ex <- gsub("-", " ", ex)
  # Deadlift variation
  dl <- str_detect(ex, "deadlift")
  dfr[dl, "BIG_FOUR"] <- "Deadlift*"
  # Squat variation
  dl <- str_detect(ex, "squat")
  dfr[dl, "BIG_FOUR"] <- "Squat*"
  # Bench Press variation
  dl <- str_detect(ex, "bench press")
  dfr[dl, "BIG_FOUR"] <- "Bench Press*"
  # Overhead Press variation
  dl <- str_detect(ex, "(overhead press|shoulder press)")
  dfr[dl, "BIG_FOUR"] <- "Overhead Press*"
  
  #Deadlift
  dl <- str_detect(ex, "^(deadlift|barbell deadlift|sumo deadlift|barbell sumo deadlift|sumo deadlift)$")
  dfr[dl, "BIG_FOUR"] <- "Deadlift"
  # Squat
  dl <- str_detect(ex, "^(barbell squat|barbell high bar back squat|barbell low bar back squat|back squat|barbell back squat|barbell high bar squat|barbell low bar squat)$")
  dfr[dl, "BIG_FOUR"] <- "Squat" 
  # Bench Press
  dl <- str_detect(ex, "^(bench press|barbell bench press|flat barbell bench press)$")
  dfr[dl, "BIG_FOUR"] <- "Bench Press" 
  # Overhead Press
  dl <- str_detect(ex, "^(overhead press|barbell overhead press|standing barbell press|standing shoulder press)$")
  dfr[dl, "BIG_FOUR"] <- "Overhead Press" 
  
  return(dfr)
}

parse_assisted <- function(dfr) {
  ex <- tolower(dfr$EXERCISE)
  ex <- gsub("-", " ", ex)
  dfr[, "ASSISTED"] <- FALSE
  i <- str_detect(ex, "assisted")
  dfr[i, "ASSISTED"] <- TRUE  
  return(dfr)
}


parse_movement <- function(dfr) {
  ex <- tolower(dfr$EXERCISE)
  ex <- gsub("-", " ", ex)
  i <- str_detect(ex, "(press|push|dip|jerk)")
  dfr[i, "MOVEMENT"] <- "Push"
  i <- str_detect(ex, "(pull|row|chin up|snatch|clean|raise)")
  dfr[i, "MOVEMENT"] <- "Pull"
  i <- str_detect(ex, "(deadlift|kettlebell swing)")
  dfr[i, "MOVEMENT"] <- "Hinge"
  i <- str_detect(ex, "(squat)")
  dfr[i, "MOVEMENT"] <- "Squat"
  i <- str_detect(ex, "(carry|walk)")
  dfr[i, "MOVEMENT"] <- "Carry"
  return(dfr)
}

rename_exercises <- function(dfr) {
  # rename.csv is still a work-in-progress
  ren_df = read_csv(RENAME_PATH)
  dfr <- dfr %>% left_join(ren_df, by = "EXERCISE")
  # Rename is RENAMED value found
  dfr <- dfr %>% mutate(EXERCISE = ifelse(!is.na(RENAMED), RENAMED, EXERCISE)) %>% select(-RENAMED)
  return(dfr)
}


plog_df <- rename_exercises(log_df)
plog_df <- parse_big_four(plog_df)
plog_df[, "VOLUME"] <- plog_df[, "WEIGHT"] * plog_df[, "REPS"]
plog_df <- parse_implement(plog_df)
plog_df <- parse_assisted(plog_df)
plog_df <- parse_movement(plog_df)



write_csv(plog_df, PLOG_PATH)

