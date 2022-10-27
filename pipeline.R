# pipeline.R --------------------------------------------------------------
# This script is used to run the RAP for the PCHC publication


# 1. install required packages --------------------------------------------
req_pkgs <-
  c(
    "dplyr",
    "stringr",
    "data.table",
    "yaml",
    "openxlsx",
    "rmarkdown",
    "highcharter",
    "lubridate",
    "dbplyr",
    "tidyr",
    "readxl",
    "DT"
  )

#utils::install.packages(req_pkgs, dependencies = TRUE)
#
# devtools::install_github("nhsbsa-data-analytics/pchcR",
#                          auth_token = Sys.getenv("GITHUB_PAT"))
#
#devtools::install_github("nhsbsa-data-analytics/nhsbsaR")

invisible(lapply(c(req_pkgs,  "nhsbsaR", "pchcR"), library, character.only = TRUE))

# 2. set options ----------------------------------------------------------
pchc_options()

# 3. build fact table if new data available -------------------
con <- con_nhsbsa(
  dsn = "FBS_8192k",
  driver = "Oracle in OraClient19Home1",
  "DWCP",
  username = rstudioapi::showPrompt(title = "Username", message = "Username"),
  password = rstudioapi::askForPassword()
)

#only run if need to build new fact table
#pchcR::create_fact(con, to = max_month)

# 4. extract data tables from fact table -----------------------------------------

#dwh tables
table_1_dwh <- table_1_dwh(con)
table_2_dwh <- table_2_dwh(con)
table_3_dwh <- table_3_dwh(con)
table_4_dwh <- table_4_dwh(con)
table_5_dwh <- table_5_dwh(con)
table_6_dwh <- table_6_dwh(con)
table_7_dwh <- table_7_dwh(con)

# disconnect from DWH
DBI::dbDisconnect(con)

#scmd tables
scmd_icb_bnf_data <- scmd_icb_bnf_data("Y:\\Official Stats\\PCHC\\data\\ICB_BNF_2022.xlsx")

scmd_national_monthly <- read_excel("Y:\\Official Stats\\PCHC\\data\\NATIONAL_MONTHLY_2022.xlsx")

table_1_scmd <- scmd_icb_bnf_data |>
  group_by(
    FINANCIAL_YEAR
  ) |>
  summarise(
    COST = sum(COST, na.rm = T),
    .groups = "drop"
  )

table_3_scmd <- scmd_national_monthly %>%
  rename(
    YEAR_MONTH = 1,
    COST = 2
  ) %>%
  mutate(
    YEAR_MONTH = paste0(
      substr(
        as.character(
          YEAR_MONTH
          ),
        1,
        4
        ),
      substr(
        as.character(
          YEAR_MONTH
        ),
        6,
        7
      )
    )
  )

# 5. data manipulation ----------------------------------------------------

#table 1
table_1a <- table_1_dwh |>
  left_join(
    table_1_scmd,
    by = c("Financial Year" = "FINANCIAL_YEAR")
  ) |>
  select(
    1, 5, 3, 4, 2
  ) |>
  rename(
    "Hospital prescribing issued within hospitals (GBP)" = 2
  ) |>
  rowwise() |>
  mutate(
    `Total (GBP)` = sum(
      across(
        contains("GBP")
      ),
      na.rm = T
    )
  )

table_3 <- table_3_dwh |>
  mutate(
    `Year Month` = as.character(`Year Month`)
  ) |>
  left_join(
    table_3_scmd,
    by = c("Year Month" = "YEAR_MONTH")
  ) |>
  select(
    1, 5, 3, 4, 2
  ) |>
  rename(
    "Hospital prescribing issued within hospitals (GBP)" = 2
  ) |>
  rowwise() |>
  mutate(
    `Total (GBP)` = sum(
      across(
        contains("GBP")
      ),
      na.rm = T
    )
  )


# get stp population
stp_pop <- ons_stp_pop()


# 6. write data to .xlsx --------------------------------------------------

# FY Excel
# create wb object
# create list of sheetnames needed (overview and metadata created automatically)
sheetNames <- c(

)

wb <- create_wb(sheetNames)

#create metadata tab (will need to open file and auto row heights once ran)
meta_fields <- c(

)

meta_descs <-
  c(
    
  )

create_metadata(wb,
                meta_fields,
                meta_descs)

#### Patient identification
# write data to sheet
write_sheet(
  wb,
  "Patient_Identification",
  paste0(
    "Hormone replacement therapy - England - 2015/2016 to ",
    ltst_year_ytd,
    " - Proportion of items for which an NHS number was recorded (%)"
  ),
  c(
    "1. Field definitions can be found on the 'Metadata' tab.",
    "2. The figures in this table relate to prescribing of HRT medications in England that are subsequently dispensed in the community in England, Scotland, Wales, Isle of Man or the Channel Islands by a pharmacy, appliance contractor, dispensing doctor, or have been personally administered by a GP practice. They do not include data on medicines used in secondary care, prisons, or issued by a private prescriber.",
    "3. The below proportions reflect the percentage of prescription items where a NHS number was recorded."
  ),
  pi_data_annual,
  30
)

#left align columns A
format_data(wb,
            "Patient_Identification",
            c("A"),
            "left",
            "")

#right align columns and round to 2 DP
format_data(wb,
            "Patient_Identification",
            c("B"),
            "right",
            "0.00")

# 7. automate narratives --------------------------------------------------

# 8. render markdowns ------------------------------------------------------

rmarkdown::render("hrt-narrative.Rmd",
                  output_format = "html_document",
                  output_file = paste0("outputs/hrt_",
                                       gsub(" ", "_", ltst_month_tidy),
                                       "_v001.html"))


rmarkdown::render("hrt-narrative.Rmd",
                  output_format = "word_document",
                  output_file = paste0("outputs/hrt_",
                                       gsub(" ", "_", ltst_month_tidy),
                                       "_v001.docx"))

rmarkdown::render("hrt-background.Rmd",
                  output_format = "html_document",
                  output_file = "outputs/hrt-background-info-methodology-v001.html")

rmarkdown::render("hrt-background.Rmd",
                  output_format = "word_document",
                  output_file = "outputs/hrt-background-info-methodology-v001.docx")

