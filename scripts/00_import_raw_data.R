# Load required libraries with checks
required_packages <- c("haven", "tidyverse")
lapply(required_packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
})

raw_spss <- read_sav("data_raw/02_02_26_WIDE_Mergedfiles_LATEST_dataset.sav")

glimpse(raw_spss)
dim(raw_spss)
names(raw_spss)[1:30]

saveRDS(raw_spss, "data_raw/wide_df_raw.rds")
