BASE_DIR <- getwd()

# Import
IMP_DIR <-file.path(BASE_DIR, "import")
FN_DIR <- file.path(IMP_DIR, "fitnotes")
FC_DIR <- file.path(IMP_DIR, "fitocracy")

# Export
DATA_DIR <-file.path(BASE_DIR, "data")
FN_PATH <- file.path(DATA_DIR, "from_fitnotes.csv")
FC_PATH <- file.path(DATA_DIR, "from_fitocracy.csv")
LOG_PATH <- file.path(DATA_DIR, "workout_log.csv")
PLOG_PATH <- file.path(DATA_DIR, "parsed_workout_log.csv")

# Ensure folders exist
suppressWarnings(dir.create(DATA_DIR))
