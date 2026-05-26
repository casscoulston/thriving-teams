# scripts/00_import_raw_data.R
# Import the raw SPSS dataset and save an R working file.

source(here::here("R", "utils.R"))
write_session_log("00_import_raw_data")

raw_spss <- read_sav(here::here("data_raw", "02_02_26_WIDE_Mergedfiles_LATEST_dataset.sav"))

glimpse(raw_spss)
dim(raw_spss)
names(raw_spss)[1:30]

saveRDS(raw_spss, here::here("data_raw", "wide_df_raw.rds"))
