library(haven)
library(tidyverse)

raw_spss <- read_sav("data_raw/02_02_26_WIDE_Mergedfiles_LATEST_dataset.sav")

glimpse(raw_spss)
dim(raw_spss)
names(raw_spss)[1:30]

saveRDS(raw_spss, "data_raw/wide_df_raw.rds")
