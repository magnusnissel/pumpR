require(readr)
require(dplyr)
require(tidyr)
require(purrr)
require(lubridate)
source("config.R")

log_df = read.csv(LOG_PATH)
