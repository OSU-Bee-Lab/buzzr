## ----tldr---------------------------------------------------------------------
df <- buzzr::bin_directory(
  dir_results=system.file('extdata/five_flowers', package='buzzr'),
  thresholds = c(ins_buzz=-1.2),
  posix_formats = '%y%m%d_%H%M',
  dir_nesting = c('flower','recorder'),
  tz = 'America/New_York',
  binwidth = 20
)

head(df)

## ----setup--------------------------------------------------------------------
library(buzzr)
dir_data <- system.file('extdata/five_flowers', package='buzzr')

paths_results <- list.files(
  dir_data,
  recursive=T
)

head(paths_results)

## ----onefile_base-------------------------------------------------------------
path_results <- file.path(dir_data, paths_results[1])

df_base <- read.csv(path_results)
head(df_base)

## ----onefile_buzzr_notime-----------------------------------------------------
df_buzzr <- buzzr::read_results(path_results)
head(df_buzzr)

## ----onefile_buzzr_time-------------------------------------------------------
df_datetime <- buzzr::read_results(
  path_results,
  posix_formats = '%y%m%d_%H%M',  # YYMMDD_HHMM
  first_match = FALSE,  # doesn't matter in this case, since we have only one format
  drop_filetime = FALSE,  # keep the start_filetime column (by default, it's dropped)
  tz='America/New_York'  # the time zone these data were collected in
)

head(df_datetime)

