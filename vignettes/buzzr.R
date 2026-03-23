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

## ----onefile_nesting----------------------------------------------------------
df_nesting <- buzzr::read_results(
  path_results,
  dir_nesting = c('flower', 'recorder')
)

head(df_nesting)

## ----onefile_call-------------------------------------------------------------
df_called <- buzzr::call_detections(
  df_datetime,
  thresholds = c(
    ins_buzz = -1.2,
    mech_plane = -2
  )
)

head(df_called)

## ----onefile_sum--------------------------------------------------------------
total_detections <- sum(df_called$detections_ins_buzz)
print(total_detections)

## ----onefile_bin--------------------------------------------------------------
df_bin <- buzzr::bin(
  df_called,
  binwidth=15,  # 15-minute bins
  calculate_rate = TRUE  # calculate our detection rates for us
)

head(df_bin)

## ----onefile_plot-------------------------------------------------------------
plot(
  df_bin$bin_datetime,
  df_bin$detections_ins_buzz
)

## ----grouping-----------------------------------------------------------------
df_called$newcol <- rep(c('A', 'B'), nrow(df_called)/2)

df_bin_customgroups <- buzzr::bin(
  df_called,
  binwidth=15,  # 15-minute bins
  calculate_rate = TRUE  # calculate our detection rates for us
)

## -----------------------------------------------------------------------------
head(df_bin_customgroups)

## ----bin_dir------------------------------------------------------------------
df_fullbin <- buzzr::bin_directory(
  dir_results=dir_data,
  thresholds = c(ins_buzz=-1.2),
  posix_formats = '%y%m%d_%H%M',
  first_match = FALSE,
  drop_filetime = TRUE,
  dir_nesting = c('flower','recorder'),
  return_ident = TRUE,
  tz = 'America/New_York',
  binwidth = 20,
  calculate_rate = TRUE
)

head(df_fullbin)
print(unique(df_fullbin$flower))

## ----rebin_dir----------------------------------------------------------------
head(bin(df_fullbin, 120))

## ----read_dir-----------------------------------------------------------------
df_fullread <- buzzr::read_directory(
  dir_results=dir_data,
  posix_formats = '%y%m%d_%H%M',
  first_match = FALSE,
  drop_filetime = TRUE,
  dir_nesting = c('flower','recorder'),
  return_ident = TRUE,
  tz = 'America/New_York'
)

nrow(df_fullread)

