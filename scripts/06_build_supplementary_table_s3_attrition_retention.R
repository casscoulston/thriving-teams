install.packages(c("officer", "flextable"))
library(officer)
library(flextable)

library(tidyverse)
library(officer)
library(flextable)

sample_by_wave <- tibble(
  Characteristic = c(
    "Full sample respondents, n",
    "Full sample teams, n",
    "Analytic sample respondents, n",
    "Analytic sample teams, n",
    "Excluded respondents due to teams with < 3 respondents, n",
    "Excluded teams due to teams with < 3 respondents, n"
  ),
  T1 = c("329", "72", "308", "58", "21", "14"),
  T2 = c("240", "66", "210", "44", "30", "22"),
  T3 = c("155", "56", "121", "31", "34", "25")
)

retention_table <- tibble(
  Characteristic = c(
    "Participant retention from T1, full sample, n (%)",
    "Participant retention from T1, analytic sample, n (%)",
    "Team retention from T1, full sample, n (%)",
    "Team retention from T1, analytic sample, n (%)"
  ),
  T1 = c("329 (100.0)", "308 (100.0)", "72 (100.0)", "58 (100.0)"),
  T2 = c("208 (63.2)", "182 (59.1)", "65 (90.3)", "43 (74.1)"),
  T3 = c("144 (43.8)", "114 (37.0)", "56 (77.8)", "31 (53.4)")
)

s3_table <- bind_rows(
  tibble(Characteristic = "Sample by wave", T1 = "", T2 = "", T3 = ""),
  sample_by_wave,
  tibble(Characteristic = "", T1 = "", T2 = "", T3 = ""),
  tibble(Characteristic = "Retention from T1", T1 = "", T2 = "", T3 = ""),
  retention_table
)

print(s3_table, n = 20)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
write_csv(s3_table, "output/tables/supplementary_table_s3_attrition_retention.csv")

ft <- flextable(s3_table) |>
  theme_booktabs() |>
  autofit()

section_rows <- which(s3_table$Characteristic %in% c("Sample by wave", "Retention from T1"))
if (length(section_rows) > 0) {
  ft <- bold(ft, i = section_rows, bold = TRUE, part = "body")
}

ft <- align(ft, j = 2:4, align = "center", part = "all")
ft <- valign(ft, valign = "center", part = "all")
ft <- fontsize(ft, size = 10, part = "all")
ft <- set_header_labels(ft, Characteristic = "Characteristic", T1 = "T1", T2 = "T2", T3 = "T3")
ft <- add_footer_lines(
  ft,
  values = c(
    "Note. The analytic sample was defined as teams with at least three responding members at a given wave.",
    "Retention percentages are calculated relative to the T1 baseline within each sample."
  )
)

doc <- read_docx() |>
  body_add_par("Supplementary Table S3", style = "Normal") |>
  body_add_par("Sample comparison and attrition summary", style = "Normal") |>
  body_add_flextable(ft)

print(doc, target = "output/tables/Supplementary_Table_S3_attrition_retention.docx")
