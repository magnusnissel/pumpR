require(plyr)
require(readr)
require(dplyr)
require(tidyr)
require(purrr)
require(lubridate)
require(stringr)
source("config.R")

log_df = read.csv(LOG_PATH)



parse_implement <- function(ex) {
  ex <- tolower(ex)
  if (str_detect(ex, "barbell|bb ")){
      imp <- "Barbell"}
  else if (str_detect(ex, "dumbbell|db ")){
      imp <- "Dumbbell"}
  else if (str_detect(ex, "kettlebell|kb ")){
    imp <- "Dumbbell"}
  else if (str_detect(ex, "ez-bar |ezbar |ez bar ")){
    imp <- "EZ-Bar"}
  else if (str_detect(ex, "trap-bar |trapbar |trap bar ")){
    imp <- "Trap-Bar"}
  else if (str_detect(ex, "football-bar |footballbar |football bar")){
    imp <- "Football-Bar"}
  else if (str_detect(ex, "machine|cybex |hammer strength |hammerstrength |hammer-strength ")){
    imp <- "Machine"}
  else if (str_detect(ex, "cable")){
    imp <- "Cable"}
  else if (str_detect(ex, "band ")){
    imp <- "Band"}
  else if (str_detect(ex, "pipe ")){
    imp <- "Pipe"}
  else if (str_detect(ex, "sled ")){
    imp <- "Sled"}
  else if (str_detect(ex, "body weight |bodyweight |bw ")) {
    imp <- "Bodyweight"
    }
  else {imp <- "NA"}
  imp
}

parse_big_four <- function(ex) {
  ex <- tolower(ex)
  if (str_detect(ex, "(barbell deadlift|sumo deadlift)")){
    core <- "Deadlift"}
  else if (str_detect(ex, "deadlift")){
    core <- "Deadlift*"}
  else if (ex == "deadlift") {core <- "Deadlift"}

  else  if (str_detect(ex, "(barbell squat|high bar squat|high-bar squat|low-bar squat|low bar squat)")){
    core <- "Squat"}
  else if (str_detect(ex, "squat")){
    core <- "Squat*"}
  else if (ex == "squat") {core <- "Squat"}
  
  else if (str_detect(ex, "flat barbell bench press")){
    core <- "Bench Press"}
  else if (ex == "barbell bench press") {core <- "Bench Press"}
  else if (str_detect(ex, "bench press")){
    core <- "Bench Press*"}
  
  else if (str_detect(ex, "barbell press|barbell overhead press")){
    core <- "Overhead Press"}
  else if (ex == "overhead press") {core <- "Overhead Press"}
  else if (str_detect(ex, "overhead press|shoulder press")){
    core <- "Overhead Press*"}
  
  else {core <- ""}
  core
}

plog_df <- log_df %>% group_by(EXERCISE) %>% rowwise()  %>% mutate(IMPLEMENT = parse_implement(EXERCISE))
plog_df <- log_df %>% group_by(EXERCISE) %>% rowwise()  %>% mutate(BIG_FOUR = parse_big_four(EXERCISE))
plog_df

write_csv(plog_df, PLOG_PATH)