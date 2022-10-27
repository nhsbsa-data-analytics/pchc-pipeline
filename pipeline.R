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
    "DT",
    "kableExtra"
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

# disconnect from DWH
DBI::dbDisconnect(con)

# 5. data manipulation ----------------------------------------------------

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

