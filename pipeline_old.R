user <- Sys.getenv("HOME")

setwd(
  paste0(
    user, 
    "/NHS Business Services Authority/Official Statistics - Documents/Publications/Prescribing Dispensing/Prescribing Costs in Hospitals and the Community/Production"
  )
)

options(scipen = 100)

library(tidyverse)
library(highcharter)
library(data.table)
library(ggthemes)
library(readxl)
library(stringr)
library(fy)
library(openxlsx)
library(janitor)
library(knitr)
library(kableExtra)

# set highcharts options for thousand separator
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
hcoptslang$numericSymbols <- c("k", "M", "B", "T")
options(highcharter.lang = hcoptslang)

##build uk financial year function (fy package uses AUS)
date2fy_uk <- function(date) {
  if (!inherits(date, "Date")) {
    date <- as.Date(date)
  }
  out <- yr2fy(year(date) + {month(date) >= 4L})
  class(out) <- "fy"
  out
}

#build not in function
'%!in%' <- function(x,y)!('%in%'(x,y))

#build function to convert numbers to 3 sfs
sigfig <- function(vec, digits = 3){
  return(gsub("\\.$", "", formatC(signif(vec,digits=digits), digits=digits, format="fg", flag="#")))
}

##read in stp lookup
stp_lookup <- read.csv(
  "lookup_data/april_2020_stp_lookup.csv"
)  %>%
  select(
    STP20CD,
    STP20CDH,
    RX_INFO_STP
  ) %>%
  unique()


###read in population data and join to stp lookup to create data to join to scmd/pcmd
stp_data_raw <- read_excel(
  "lookup_data/stp_populations.xlsx",
  sheet = "Mid-2020 Persons"
) 

##renaming column
names(stp_data_raw) <- unlist(stp_data_raw[6,])

#mutate into correct format
stp_data <- stp_data_raw %>%
  slice(-1:-7) %>%
  data.frame() %>%
  mutate(
    All.Ages = as.numeric(All.Ages)
  ) %>%
  group_by(
    STP21.Name,
    STP21.Code
  ) %>%
  summarise(
    Pop = sum(All.Ages, na.rm = T)
  ) %>%
  ungroup() %>%
  left_join(
    stp_lookup,
    by = c("STP21.Code" = "STP20CD")
  ) %>%
  mutate(RX_INFO_STP = substr(RX_INFO_STP,1,nchar(RX_INFO_STP)-4))

##read in pcmd data set
pcmd_stp <- readRDS(
  "raw_data/pcmd_data_stp.rds"
  ) %>%
  left_join(
    stp_data,
    by = c("STP_CODE" = "STP20CDH")
  ) %>%
  mutate(
    LVL_5_LTST_TYPE = case_when(
      LVL_5_LTST_TYPE == "GP PRACTICE / COST CENTRE" ~ "Primary care prescribing dispensed in the community",
      LVL_5_LTST_TYPE == "COMMUNITY NURSE PRESCRIBING CONTRACT" ~ "Primary care prescribing dispensed in the community",
      LVL_5_LTST_TYPE == "HOSPITAL" ~ "Hospital prescribing dispensed in the community",
      LVL_5_LTST_TYPE == "DENTIST PRACTICE" ~ "Dental prescribing dispensed in the community"
    ),
    YEAR_MONTH = as.Date(paste0(YEAR_MONTH, "01"),format = "%Y%m%d"),
    STP_CODE = case_when(
      is.na(Pop) ~ "Undefined",
      TRUE ~ STP_CODE
    ),
    STP21.Name = case_when(
      is.na(Pop) ~ "Undefined",
      TRUE ~ STP21.Name
    )
  )

#build bnf lookup table from pcmd_stp
bnf_lookup <- pcmd_stp %>%
  select(BNF_CHAPTER,
         CHAPTER_DESCR,
         BNF_SECTION,
         SECTION_DESCR) %>%
  unique()

bnf_chapter_lookup <- bnf_lookup %>%
  select(-BNF_SECTION,-SECTION_DESCR) %>%
  unique()

######### SCMD #########

# overall monthly costs #
scmd_monthly <- read_excel(
  "raw_data/STP_MONTH_2021.xlsx"
) %>%
  rename(
    "Month" = 1
  ) %>%
  mutate(
    Month = as.Date(as.POSIXct(Month)),
    FY = date2fy_uk(Month),
    FY = gsub("-", "/20", FY),
    FY = as.character(FY)
  )

# stp and bnf data #
# get all sheet names in file
sheets <- excel_sheets("raw_data/STP_BNF_2021.xlsx")

stp_bnf_data_raw <- data.frame()

for(i in 1:length(sheets)) {

data <- read_excel(
  "raw_data/STP_BNF_2021.xlsx",
  sheet = sheets[i]
) %>%
  mutate(`FY    ` = sheets[i]) %>%
  relocate(`FY    `)

names(data) <-  substr(names(data),1,nchar(names(data))-4)

stp_bnf_data_raw <- stp_bnf_data_raw %>%
  bind_rows(data)
}

stp_bnf_data_raw <- stp_bnf_data_raw %>%
  rowwise() %>%
  mutate(Undefined= sum(across(contains("....")), na.rm = T))


stp_bnf_data_clean <- stp_bnf_data_raw %>%
  mutate(
    FY = gsub(" ", "/", FY),
    Cost = case_when(
      Cost == "22 - Incontinence Appliances" ~ "22.1 - Incontinence Appliances",
      TRUE ~ Cost
    )
  ) %>%
  rename(
    BNF = 2
  ) %>%
  gather(
    STP, Cost, -c(FY, BNF)
  ) %>%
  filter(!grepl('\\....', STP)) %>%
  mutate(
    BNF_CHAPTER = str_extract(BNF, "[^\\.]+"),
    BNF_SECTION = as.character(str_extract_all(BNF,"(?<=\\.).+(?= -)")),
    BNF_CHAPTER = case_when(
      str_length(BNF_CHAPTER) == 1 ~ paste0("0",BNF_CHAPTER),
      TRUE ~ BNF_CHAPTER
    ),
    BNF_SECTION = case_when(
      str_length(BNF_SECTION) == 1 ~ paste0("0",BNF_SECTION),
      TRUE ~ BNF_SECTION
    ),
    BNF_SECTION = paste0(BNF_CHAPTER, BNF_SECTION)
    ) %>%
  replace(is.na(.), 0) %>%
  left_join(bnf_lookup) %>%
  left_join(stp_data,
            by = c("STP" = "RX_INFO_STP"))
  

#build bnf lookup for those missing under primary care data
scmd_bnf_lookup <- stp_bnf_data_clean %>%
  select(BNF_CHAPTER, BNF_SECTION, BNF) %>%
  unique() %>%
  filter(BNF_SECTION %!in% bnf_lookup$BNF_SECTION) %>%
  mutate(BNF = gsub(".*- ", "", BNF)) %>%
  left_join(bnf_chapter_lookup) %>%
  rename("SECTION_DESCR" = "BNF")

bnf_lookup <- bnf_lookup %>%
  bind_rows(scmd_bnf_lookup)

#re-run data with new lookup
stp_bnf_data_clean <- stp_bnf_data_clean %>%
  select(-CHAPTER_DESCR, -SECTION_DESCR)%>%
  left_join(bnf_lookup)

## top substance data
sheets2 <- excel_sheets("raw_data/TOP_30_SPENDS.xlsx")

top_substance <- data.frame()

for(j in 1:length(sheets)) {
  
  data <- read_excel(
    "raw_data/TOP_30_SPENDS.xlsx",
    sheet = sheets[j]
  ) %>%
    mutate(`FY` = sheets[j],
           FY = gsub(" ", "/", FY)) %>%
    relocate(`FY`)
  
  names(data) <-  c("Financial Year", "WHO ATC", "Actual cost (GBP)")
  
  top_substance <- top_substance %>%
    bind_rows(data)
}
top_substance2 <- top_substance %>%
  group_by(`Financial Year`) %>%
  top_n(10) %>%
  mutate(`WHO ATC` = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",    # Uppercase with Base R
                                   `WHO ATC`,
                                   perl = TRUE)) %>%
  ungroup()


#table 1 build - overall costs per setting by financial year
pcmd_tab_1 <- pcmd_stp%>%
  group_by(FINANCIAL_YEAR, LVL_5_LTST_TYPE) %>%
  summarise(Cost = sum(ITEM_PAY_DR_NIC)) %>%
  spread(LVL_5_LTST_TYPE, Cost) %>%
  ungroup() %>%
  rename("Financial Year" = 1)

scmd_tab_1 <- scmd_monthly %>%
  mutate(LVL_5_LTST_TYPE = "Hospital prescribing issued within hospitals") %>%
  group_by(FY, LVL_5_LTST_TYPE) %>%
  summarise(Cost = sum(Total)) %>%
  spread(LVL_5_LTST_TYPE, Cost) %>%
  ungroup() %>%
  rename("Financial Year" = 1) 


table_1 <- scmd_tab_1 %>%
  left_join(pcmd_tab_1) %>%
  rowwise() %>%
  mutate(Total = sum(across(!starts_with("Financial")), na.rm = T))

names(table_1)[2:ncol(table_1)] <- c(paste0(names(table_1)[2:ncol(table_1)], " (GBP)"))

#table 2 build - % change by setting each year
pcmd_tab_2 <- pcmd_stp %>%
  group_by(FINANCIAL_YEAR, LVL_5_LTST_TYPE) %>%
  summarise(Cost = sum(ITEM_PAY_DR_NIC)) %>%
  arrange(LVL_5_LTST_TYPE) %>%
  ungroup() %>%
  mutate(`Annual change (%)` = (Cost - lag(Cost,1))/lag(Cost,1) * 100) %>%
  select(-Cost) %>%
  spread(LVL_5_LTST_TYPE, `Annual change (%)`) %>%
  slice(-1) %>%
  rename("Financial Year" = 1) 

scmd_tab_2 <- scmd_monthly %>%
  mutate(LVL_5_LTST_TYPE = "Hospital prescribing issued within hospitals") %>%
  group_by(FY, LVL_5_LTST_TYPE) %>%
  summarise(Cost = sum(Total)) %>%
  ungroup() %>%
  mutate(`Annual change (%)` = (Cost - lag(Cost,1))/lag(Cost,1) * 100) %>%
  select(-Cost) %>%
  spread(LVL_5_LTST_TYPE, `Annual change (%)`) %>%
  slice(-1) %>%
  rename("Financial Year" = 1) 
 
table_2 <- scmd_tab_2 %>%
  left_join(pcmd_tab_2)

names(table_2)[2:ncol(table_2)] <- c(paste0(names(table_2)[2:ncol(table_2)], " (%)"))

#table 3 build - overall costs per setting by month
pcmd_tab_3 <- pcmd_stp %>%
  group_by(YEAR_MONTH, LVL_5_LTST_TYPE) %>%
  summarise(Cost = sum(ITEM_PAY_DR_NIC)) %>%
  rename("Month" = 1) %>%
  spread(LVL_5_LTST_TYPE, Cost) %>%
  ungroup()

scmd_tab_3 <- scmd_monthly %>%
  mutate(LVL_5_LTST_TYPE = "Hospital prescribing issued within hospitals") %>%
  group_by(Month, LVL_5_LTST_TYPE) %>%
  summarise(Cost = sum(Total))  %>%
  spread(LVL_5_LTST_TYPE, Cost) %>%
  ungroup() 

table_3 <- scmd_tab_3 %>%
  left_join(pcmd_tab_3)%>%
  rowwise() %>%
  mutate(Total= sum(across(!starts_with("Month")), na.rm = T))

names(table_3)[2:ncol(table_3)] <- c(paste0(names(table_3)[2:ncol(table_3)], " (GBP)"))

# #table 4 build - top who atc
# table_4 <- top_substance2

#table 4 build - costs by setting and bnf chapter
scmd_tab_4 <- stp_bnf_data_clean %>%
  mutate(LVL_5_LTST_TYPE = "Hospital prescribing issued within hospitals") %>%
  group_by(
    FY, LVL_5_LTST_TYPE, BNF_CHAPTER, CHAPTER_DESCR
  ) %>%
  summarise(Cost = sum(Cost)) %>%
  mutate(
    BNF_CHAPTER = case_when(
      is.na(CHAPTER_DESCR) ~ as.character("Undefined"),
      TRUE ~ BNF_CHAPTER
    ),
    CHAPTER_DESCR = case_when(
      is.na(CHAPTER_DESCR) ~ as.character("Undefined"),
      TRUE ~ CHAPTER_DESCR
    )
  ) %>%
  spread(LVL_5_LTST_TYPE, Cost) 

names(scmd_tab_4)[1:3] <- c("Financial Year", "BNF Chapter", "BNF Chapter Description")

pcmd_tab_4 <- pcmd_stp %>%
  group_by(FINANCIAL_YEAR, LVL_5_LTST_TYPE, BNF_CHAPTER, CHAPTER_DESCR) %>%
  summarise(Cost = sum(ITEM_PAY_DR_NIC)) %>%
  spread(LVL_5_LTST_TYPE, Cost) 

names(pcmd_tab_4)[1:3] <- c("Financial Year", "BNF Chapter", "BNF Chapter Description")

table_4 <- scmd_tab_4 %>%
  left_join(pcmd_tab_4) %>%
  replace(is.na(.), 0) %>%
  mutate(Total= sum(across(contains("prescribing")), na.rm = T))

names(table_4)[4:ncol(table_4)] <- c(paste0(names(table_4)[4:ncol(table_4)], " (GBP)"))

#table 5 build - costs by setting and bnf chapter and section
scmd_tab_5 <- stp_bnf_data_clean %>%
  mutate(LVL_5_LTST_TYPE = "Hospital prescribing issued within hospitals") %>%
  group_by(
    FY, LVL_5_LTST_TYPE, BNF_CHAPTER, CHAPTER_DESCR, BNF_SECTION, SECTION_DESCR
  ) %>%
  summarise(Cost = sum(Cost)) %>%
  mutate(
    BNF_CHAPTER = case_when(
      is.na(CHAPTER_DESCR) ~ as.character("Undefined"),
      TRUE ~ BNF_CHAPTER
    ),
    BNF_SECTION = case_when(
      is.na(CHAPTER_DESCR) ~ as.character("Undefined"),
      TRUE ~ BNF_SECTION
    ),
    SECTION_DESCR = case_when(
      is.na(CHAPTER_DESCR) ~ as.character("Undefined"),
      TRUE ~ SECTION_DESCR
    ),
    CHAPTER_DESCR = case_when(
      is.na(CHAPTER_DESCR) ~ as.character("Undefined"),
      TRUE ~ CHAPTER_DESCR
    )
  )

pcmd_tab_5<- pcmd_stp %>%
  group_by(FINANCIAL_YEAR, LVL_5_LTST_TYPE, BNF_CHAPTER, CHAPTER_DESCR, BNF_SECTION, SECTION_DESCR) %>%
  summarise(Cost = sum(ITEM_PAY_DR_NIC))

names(pcmd_tab_5) <- names(scmd_tab_5)

table_5 <- scmd_tab_5 %>%
  bind_rows(pcmd_tab_5)%>%
  spread(LVL_5_LTST_TYPE, Cost)%>%
  replace(is.na(.), 0) %>%
  arrange(BNF_SECTION) %>%
  arrange(FY) %>%
  ungroup() %>%
  complete(
    FY, 
    nesting(BNF_CHAPTER, CHAPTER_DESCR, BNF_SECTION, SECTION_DESCR),
    fill = list(`Dental prescribing dispensed in the community` = 0,
                `Hospital prescribing dispensed in the community` = 0,
                `Primary care prescribing dispensed in the community` = 0,
                `Hospital prescribing issued within hospitals` = 0)) %>%
  rowwise() %>%
  mutate(Total= sum(across(contains("prescribing")), na.rm = T))
  

names(table_5)[6:ncol(table_5)] <- c(paste0(names(table_5)[6:ncol(table_5)], " (GBP)"))
names(table_5)[1:5] <- c("Financial Year", "BNF Chapter", "BNF Chapter Description", "BNF Section", "BNF Section Description")

table_5 <- table_5 %>%
  select(
    `Financial Year`,
    `BNF Chapter`,
    `BNF Chapter Description`,
    `BNF Section`,
    `BNF Section Description`,
    `Hospital prescribing issued within hospitals (GBP)`,
    `Dental prescribing dispensed in the community (GBP)`,
    `Hospital prescribing dispensed in the community (GBP)`,
    `Primary care prescribing dispensed in the community (GBP)`,
    `Total (GBP)`
  )


#table 6 build - costs by stp
scmd_tab_6 <- stp_bnf_data_clean %>%
  mutate(LVL_5_LTST_TYPE = "Hospital prescribing issued within hospitals") %>%
  group_by(
    FY, LVL_5_LTST_TYPE, STP20CDH, STP21.Name 
  ) %>%
  summarise(Cost = sum(Cost)) %>%
  spread(LVL_5_LTST_TYPE, Cost) %>%
  replace(is.na(.), "Undefined")

names(scmd_tab_6)[1:3] <- c("Financial Year", "STP Code", "STP")

pcmd_tab_6 <- pcmd_stp %>%
  group_by(FINANCIAL_YEAR, LVL_5_LTST_TYPE, STP_CODE, STP21.Name) %>%
  summarise(Cost = sum(ITEM_PAY_DR_NIC)) %>%
  spread(LVL_5_LTST_TYPE, Cost) %>%
  replace(is.na(.), 0)

names(pcmd_tab_6)[1:3] <- c("Financial Year", "STP Code", "STP")

table_6 <- scmd_tab_6 %>%
  left_join(pcmd_tab_6) %>%
  replace(is.na(.), 0) %>%
  mutate(Total= sum(across(contains("prescribing")), na.rm = T)) %>%
  arrange(`STP`)%>%
  arrange(`Financial Year`)

stp_pop <- stp_data %>%
  select(STP20CDH, Pop)

table_6 <- table_6 %>%
  left_join(stp_pop, by = c("STP Code" = "STP20CDH")) %>%
  mutate(`Total Cost Per Capita` = Total/Pop)

names(table_6)[4:ncol(table_6)] <- c(paste0(names(table_6)[4:ncol(table_6)], " (GBP)"))

names(table_6)[9] <- "Population"

## table 7 build - bnf chapter, section, stp and population
scmd_tab_7 <- stp_bnf_data_clean %>%
  mutate(LVL_5_LTST_TYPE = "Hospital prescribing issued within hospitals") %>%
  group_by(
    FY, LVL_5_LTST_TYPE, BNF_CHAPTER, CHAPTER_DESCR, BNF_SECTION, SECTION_DESCR, STP20CDH, STP21.Name
  ) %>%
  summarise(Cost = sum(Cost)) %>%
  mutate(
    BNF_CHAPTER = case_when(
      is.na(CHAPTER_DESCR) ~ as.character("Undefined"),
      TRUE ~ BNF_CHAPTER
    ),
    BNF_SECTION = case_when(
      is.na(CHAPTER_DESCR) ~ as.character("Undefined"),
      TRUE ~ BNF_SECTION
    ),
    SECTION_DESCR = case_when(
      is.na(CHAPTER_DESCR) ~ as.character("Undefined"),
      TRUE ~ SECTION_DESCR
    ),
    CHAPTER_DESCR = case_when(
      is.na(CHAPTER_DESCR) ~ as.character("Undefined"),
      TRUE ~ CHAPTER_DESCR
    ),
    STP20CDH = case_when(
      is.na(STP20CDH) ~ as.character("Undefined"),
      TRUE ~ STP20CDH
    ),
    STP21.Name = case_when(
      is.na(STP21.Name) ~ as.character("Undefined"),
      TRUE ~ STP21.Name
    )
  ) %>%
  ungroup()

pcmd_tab_7 <- pcmd_stp %>%
  group_by(FINANCIAL_YEAR, LVL_5_LTST_TYPE, BNF_CHAPTER, CHAPTER_DESCR, BNF_SECTION, SECTION_DESCR, STP_CODE, STP21.Name) %>%
  summarise(Cost = sum(ITEM_PAY_DR_NIC))

names(pcmd_tab_7) <- names(scmd_tab_7)

table_7 <- scmd_tab_7 %>%
  bind_rows(pcmd_tab_7)%>%
  spread(LVL_5_LTST_TYPE, Cost)%>%
  replace(is.na(.), 0) %>%
  arrange(BNF_SECTION) %>%
  arrange(FY) %>%
  ungroup() %>%
  complete(
    FY, 
    nesting(BNF_CHAPTER, CHAPTER_DESCR, BNF_SECTION, SECTION_DESCR, STP20CDH, STP21.Name),
    fill = list(`Dental prescribing dispensed in the community` = 0,
                `Hospital prescribing dispensed in the community` = 0,
                `Primary care prescribing dispensed in the community` = 0,
                `Hospital prescribing issued within hospitals` = 0)) %>%
  rowwise() %>%
  mutate(Total= sum(across(contains("prescribing")), na.rm = T))


names(table_7)[8:ncol(table_7)] <- c(paste0(names(table_7)[8:ncol(table_7)], " (GBP)"))
names(table_7)[1:7] <- c("Financial Year", "BNF Chapter", "BNF Chapter Description", "BNF Section", "BNF Section Description", "STP Code", "STP Name")

table_7 <- table_7 %>%
  select(
    `Financial Year`,
    `BNF Chapter`,
    `BNF Chapter Description`,
    `BNF Section`,
    `BNF Section Description`,
    `STP Code`,
    `STP Name`,
    `Hospital prescribing issued within hospitals (GBP)`,
    `Dental prescribing dispensed in the community (GBP)`,
    `Hospital prescribing dispensed in the community (GBP)`,
    `Primary care prescribing dispensed in the community (GBP)`,
    `Total (GBP)`
  ) 

#call table build functions -------------------------------------------

# create outputs\styles object to hold tables and styles
outputs <- list()
styles <- list()
table_titles <- list()
table_names <- list()

# assign outputs
outputs$table_one <- table_1
table_titles$table_one <- "Prescribing Costs in Hospitals and the Community - Total costs in each setting by financial year, 2016/17 to 2020/21"
table_names$table_one <- "Cost_by_Setting_Annual"

outputs$table_two <- table_2
table_titles$table_two <- "Prescribing Costs in Hospitals and the Community - % change in costs in each setting by financial year, 2016/17 to 2020/21"
table_names$table_two <- "Percentage_Change_in_Cost_by_Setting"

outputs$table_three <- table_3
table_titles$table_three <- "Prescribing Costs in Hospitals and the Community - Total costs in each setting by month, April 2016 to March 2021"
table_names$table_three <- "Cost_by_Setting_Monthly"

outputs$table_four <- table_4
table_titles$table_four <- "Prescribing Costs in Hospitals and the Community - Total costs in each setting by BNF Chapter and financial year, 2016/17 to 2020/21"
table_names$table_four <- "Cost_by_BNF_Chapter"

outputs$table_five <- table_5
table_titles$table_five <- "Prescribing Costs in Hospitals and the Community - Total costs in each setting by BNF Section and financial year, 2016/17 to 2020/21"
table_names$table_five <- "Cost_by_BNF_Section"

outputs$table_six <- table_6
table_titles$table_six <- "Prescribing Costs in Hospitals and the Community - Total costs in each setting by STP and financial year, 2016/17 to 2020/21"
table_names$table_six <- "Cost_by_STP"

outputs$table_seven <- table_7
table_titles$table_seven <- "Prescribing Costs in Hospitals and the Community - Total costs in each setting by BNF Section, STP and financial year, 2016/17 to 2020/21"
table_names$table_seven <- "Cost_by_BNF_Section_and_STP"

#build excel tables using openxlsx ------------------------------------

# create workbook object
wb <- openxlsx::createWorkbook()

for(j in seq_along(outputs)){

  # create worksheet in workbook object
  openxlsx::addWorksheet(
    wb,
    sheetName = stringr::str_to_title(gsub("_"," ",names(outputs)[j]))
  )
  
  #   # add excel data to each sheet
  openxlsx::writeDataTable(
    wb,
    sheet = stringr::str_to_title(gsub("_"," ",names(outputs)[j])),
    startRow = 3,
    x = outputs[[j]],
    tableStyle = "none",
    withFilter = FALSE,
    tableName = table_names[[j]]
  )
  
  openxlsx::writeData(
    wb,
    sheet = stringr::str_to_title(gsub("_"," ",names(outputs)[j])),
    x = table_titles[[j]],
    xy = c(1,1)
  )
  
  
 #set font to Arial
  modifyBaseFont(wb, fontName = "Arial", fontSize = 10)
  
 #remove gridlines
  showGridLines(wb, stringr::str_to_title(gsub("_"," ",names(outputs)[j])), showGridLines = FALSE)
  
 # bold rows 1 and 3 (title and headers)
  addStyle(
    wb,
    stringr::str_to_title(gsub("_"," ",names(outputs)[j])),
    style = createStyle(textDecoration = "bold"),
    cols = 1:ncol(outputs[[j]]),
    rows = 1
  )
  addStyle(
    wb,
    stringr::str_to_title(gsub("_"," ",names(outputs)[j])),
    style = createStyle(textDecoration = "bold"),
    cols = 1:ncol(outputs[[j]]),
    rows = 3
  )
}

# save workbook
openxlsx::saveWorkbook(
  wb,
  file = paste0("Outputs/pchc-2021-summary-tables-v001.xlsx"),
  overwrite = TRUE
)

###build chart functions

##figure 1 - overall costs per setting by financial year

scmd_annual_spend <- scmd_monthly %>%
  group_by(FY) %>%
  summarise(Cost = sum(Total)) %>%
  mutate(sf = prettyNum(as.numeric(sigfig(Cost)), big.mark = ","))

pcmd_annual_spend <- pcmd_stp %>%
  group_by(FINANCIAL_YEAR, LVL_5_LTST_TYPE) %>%
  summarise(Cost = sum(ITEM_PAY_DR_NIC)) %>%
  mutate(sf = prettyNum(as.numeric(sigfig(Cost)), big.mark = ","))

figure_1 <- highchart() %>%
  hc_add_series(
    name = "Primary care in the community",
    type = "column",
    data = pcmd_annual_spend %>% filter(LVL_5_LTST_TYPE == "Primary care prescribing dispensed in the community"),
    color = "#009639",
    hcaes(
      x = FINANCIAL_YEAR,
      y = Cost,
      sf = sf
    )
  ) %>%
  hc_add_series(
    name = "Issued in hospitals",
    type = "column",
    data = scmd_annual_spend,
    color = "#005EB8",
    hcaes(
      x = FY,
      y = Cost,
      sf = sf
    )
  ) %>%
  hc_add_series(
    name = "Hospital in the community",
    type = "column",
    data = pcmd_annual_spend %>% filter(LVL_5_LTST_TYPE == "Hospital prescribing dispensed in the community"),
    color = "#7C2855",
    hcaes(
      x = FINANCIAL_YEAR,
      y = Cost,
      sf = sf
    )
  ) %>%
  hc_add_series(
    name = "Dental in the community",
    type = "column",
    data = pcmd_annual_spend %>% filter(LVL_5_LTST_TYPE == "Dental prescribing dispensed in the community"),
    color = "#768692",
    hcaes(
      x = FINANCIAL_YEAR,
      y = Cost,
      sf = sf
    )
  ) %>%
  hc_xAxis(
    type = "category",
    title =list(
      text = "Financial year"
    )
  ) %>%
  hc_yAxis(
    title =list(
      text = "Costs (GBP)"
    )
  ) %>%
  hc_plotOptions(
    column = list(
      stacking = "normal"
    )
  ) %>%
  hc_tooltip(
    shared = T,
    pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>£{point.sf}</b><br/>'  ) %>%
  hc_title(
    text = "Figure 1: Total costs by setting between 2016/2017 and 2020/2021",
    style = list(fontSize = "16px",
                 fontWeight = "bold")
  ) %>%
  highcharter::hc_credits(enabled = TRUE)

#figure_1

#figure 2 - % change by setting each year
scmd_annual_change <- scmd_monthly %>%
  group_by(FY) %>%
  summarise(Cost = sum(Total)) %>%
  ungroup() %>%
  mutate(`Annual change (%)` = (Cost - lag(Cost,1))/lag(Cost,1) * 100) %>%
  select(-Cost) %>%
  slice(-1) %>%
  mutate(sf = prettyNum(as.numeric(sigfig(`Annual change (%)`)), big.mark = ","))

pcmd_annual_change <- pcmd_stp %>%
  group_by(FINANCIAL_YEAR, LVL_5_LTST_TYPE) %>%
  summarise(Cost = sum(ITEM_PAY_DR_NIC)) %>%
  ungroup() %>%
  arrange(LVL_5_LTST_TYPE) %>%
  mutate(`Annual change (%)` = (Cost - lag(Cost,1))/lag(Cost,1) * 100) %>%
  select(-Cost) %>%
  group_by(LVL_5_LTST_TYPE) %>%
  slice(-1) %>%
  ungroup() %>%
  mutate(sf = prettyNum(as.numeric(sigfig(`Annual change (%)`)), big.mark = ","))

figure_2 <- highchart() %>%
  hc_add_series(
    name = "Primary care in the community",
    type = "line",
    data = pcmd_annual_change %>% filter(LVL_5_LTST_TYPE == "Primary care prescribing dispensed in the community"),
    color = "#009639",
    hcaes(
      x = FINANCIAL_YEAR,
      y = `Annual change (%)`,
      sf = sf
    )
  ) %>%
  hc_add_series(
    name = "Issued in hospitals",
    type = "line",
    data = scmd_annual_change,
    color = "#005EB8",
    hcaes(
      x = FY,
      y = `Annual change (%)`,
      sf = sf
    )
  )%>%
  hc_add_series(
    name = "Hospital in the community",
    type = "line",
    data = pcmd_annual_change %>% filter(LVL_5_LTST_TYPE == "Hospital prescribing dispensed in the community"),
    color = "#7C2855",
    hcaes(
      x = FINANCIAL_YEAR,
      y = `Annual change (%)`,
      sf = sf
    )
  ) %>%
  hc_add_series(
    name = "Dental in the community",
    type = "line",
    data = pcmd_annual_change %>% filter(LVL_5_LTST_TYPE == "Dental prescribing dispensed in the community"),
    color = "#768692",
    hcaes(
      x = FINANCIAL_YEAR,
      y = `Annual change (%)`,
      sf = sf
    )
  ) %>%
  hc_xAxis(
    type = "category",
    title =list(
      text = "Financial year"
    )
  ) %>%
  hc_yAxis(
    title =list(
      text = "% Change in costs (%)"
    )
  ) %>%
  hc_plotOptions(
    column = list(
      stacking = "normal"
    )
  ) %>%
  hc_tooltip(
    shared = T,
    pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.sf}%</b><br/>'
  ) %>%
  hc_title(
    text = "Figure 2: Percentage change from previous financial year in costs by setting between 2017/2018 and 2020/2021",
    style = list(fontSize = "16px",
                 fontWeight = "bold")
  ) %>%
  highcharter::hc_credits(enabled = TRUE)

#figure_2

##figure 3 - overall costs per setting by month 

scmd_monthly_spend <- scmd_monthly %>%
  group_by(Month) %>%
  summarise(Cost = sum(Total)) %>%
  mutate(sf = prettyNum(as.numeric(sigfig(Cost)), big.mark = ","))

pcmd_monthly_spend <- pcmd_stp %>%
  group_by(YEAR_MONTH, LVL_5_LTST_TYPE) %>%
  summarise(Cost = sum(ITEM_PAY_DR_NIC)) %>%
  mutate(sf = prettyNum(as.numeric(sigfig(Cost)), big.mark = ",")) %>% 
  ungroup() 

figure_3 <- highchart() %>%
  hc_add_series(
    name = "Primary care in the community",
    type = "line",
    data = pcmd_monthly_spend %>% filter(LVL_5_LTST_TYPE == "Primary care prescribing dispensed in the community"),
    color = "#009639",
    hcaes(
      x = YEAR_MONTH,
      y = Cost,
      sf = sf
    )
  ) %>%
  hc_add_series(
    name = "Issued in hospitals",
    type = "line",
    data = scmd_monthly_spend,
    color = "#005EB8",
    hcaes(
      x = Month,
      y = Cost,
      sf = sf
    )
  ) %>%
  hc_add_series(
    name = "Hospital in the community",
    type = "line",
    data = pcmd_monthly_spend %>% filter(LVL_5_LTST_TYPE == "Hospital prescribing dispensed in the community"),
    color = "#7C2855",
    hcaes(
      x = YEAR_MONTH,
      y = Cost,
      sf = sf
    )
  ) %>%
  hc_add_series(
    name = "Dental in the community",
    type = "line",
    data = pcmd_monthly_spend %>% filter(LVL_5_LTST_TYPE == "Dental prescribing dispensed in the community"),
    color = "#768692",
    hcaes(
      x = YEAR_MONTH,
      y = Cost,
      sf = sf
    )
  ) %>%
  hc_xAxis(
    type = "datetime",
    title =list(
      text = "Month"
    )
  ) %>%
  hc_yAxis(
    title =list(
      text = "Costs (GBP)"
    )
  ) %>%
  hc_plotOptions(
    line = list(
      marker = list(
        enabled = FALSE
      )
    )
  ) %>%
  hc_tooltip(
    shared = T,
    pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>£{point.sf}</b><br/>'
  ) %>%
  hc_title(
    text = "Figure 3: Total costs by setting between April 2016 and March 2021",
    style = list(fontSize = "16px",
                 fontWeight = "bold")
  ) %>%
  highcharter::hc_credits(enabled = TRUE)

#figure_3

##table 1 latest fy top 10 WHO chemical
kable_1 <- top_substance2 %>%
  filter(`Financial Year` == "2020/2021") %>%
  select(-`Financial Year`) %>%
  mutate(`Actual cost (GBP)` = prettyNum(as.numeric(sigfig(`Actual cost (GBP)`)), big.mark = ","))

 
##figure 4 lastest fy costs by bnf chapter

scmd_bnf_spend <- stp_bnf_data_clean %>%
  filter(FY == "2020/2021") %>%
  group_by(BNF_CHAPTER, CHAPTER_DESCR) %>%
  mutate(
    BNF_CHAPTER = case_when(
      is.na(CHAPTER_DESCR) ~ as.character("Undefined"),
      TRUE ~ BNF_CHAPTER
    ),
    CHAPTER_DESCR = case_when(
      is.na(CHAPTER_DESCR) ~ as.character("Undefined"),
      TRUE ~ CHAPTER_DESCR
    )
  ) %>%
  summarise(Cost = sum(Cost)) %>%
  mutate(sf = prettyNum(as.numeric(sigfig(Cost)), big.mark = ","))

pcmd_bnf_spend <- pcmd_stp %>%
  filter(FINANCIAL_YEAR == "2020/2021") %>%
  group_by(LVL_5_LTST_TYPE, BNF_CHAPTER, CHAPTER_DESCR) %>%
  summarise(Cost = sum(ITEM_PAY_DR_NIC)) %>%
  mutate(sf = prettyNum(as.numeric(sigfig(Cost)), big.mark = ","))

figure_4 <- highchart() %>%
  hc_add_series(
    name = "Primary care in the community",
    type = "column",
    data = pcmd_bnf_spend %>% filter(LVL_5_LTST_TYPE == "Primary care prescribing dispensed in the community"),
    color = "#009639",
    hcaes(
      x = BNF_CHAPTER,
      y = Cost,
      sf = sf,
      desc = CHAPTER_DESCR
    )
  ) %>%
  hc_add_series(
    name = "Issued in hospitals",
    type = "column",
    data = scmd_bnf_spend,
    color = "#005EB8",
    hcaes(
      x = BNF_CHAPTER,
      y = Cost,
      sf = sf,
      desc = CHAPTER_DESCR
    )
  ) %>%
  hc_add_series(
    name = "Hospital in the community",
    type = "column",
    data = pcmd_bnf_spend %>% filter(LVL_5_LTST_TYPE == "Hospital prescribing dispensed in the community"),
    color = "#7C2855",
    hcaes(
      x = BNF_CHAPTER,
      y = Cost,
      sf = sf,
      desc = CHAPTER_DESCR
    )
  ) %>%
  hc_add_series(
    name = "Dental in the community",
    type = "column",
    data = pcmd_bnf_spend %>% filter(LVL_5_LTST_TYPE == "Dental prescribing dispensed in the community"),
    color = "#768692",
    hcaes(
      x = BNF_CHAPTER,
      y = Cost,
      sf = sf,
      desc = CHAPTER_DESCR
    )
  ) %>%
  hc_xAxis(
    type = "category",
    title =list(
      text = "BNF Chapter"
    )
  ) %>%
  hc_yAxis(
    title =list(
      text = "Costs (GBP)"
    )
  ) %>%
  hc_plotOptions(
    column = list(
      stacking = "normal"
    )
  ) %>%
  hc_tooltip(
    shared = T,
    headerFormat = '{point.key}: {point.point.desc}<br>',
    pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>£{point.sf}</b><br/>'
  ) %>%
  hc_title(
    text = "Figure 4: Total costs by BNF Chapter in the 2020/2021 financial year",
    style = list(fontSize = "16px",
                 fontWeight = "bold")
  ) %>%
  highcharter::hc_credits(enabled = TRUE)

#figure_4

##figure 5 costs in 2020/21 by top 10 bnf sections
scmd_section_spend <- stp_bnf_data_clean %>%
  mutate(LVL_5_LTST_TYPE = "Issued in hospitals") %>%
  filter(FY == "2020/2021") %>%
  group_by(LVL_5_LTST_TYPE, BNF_SECTION, SECTION_DESCR) %>%
  mutate(
    BNF_SECTION = case_when(
      is.na(CHAPTER_DESCR) ~ as.character("Undefined"),
      TRUE ~ BNF_SECTION
    ),
    SECTION_DESCR = case_when(
      is.na(CHAPTER_DESCR) ~ as.character("Undefined"),
      TRUE ~ SECTION_DESCR
    )
  ) %>%
  summarise(Cost = sum(Cost))%>%
  mutate(sf = prettyNum(as.numeric(sigfig(Cost)), big.mark = ","))

pcmd_section_spend <- pcmd_stp %>%
  filter(FINANCIAL_YEAR == "2020/2021") %>%
  group_by(LVL_5_LTST_TYPE, BNF_SECTION, SECTION_DESCR) %>%
  summarise(Cost = sum(ITEM_PAY_DR_NIC))%>%
  mutate(sf = prettyNum(as.numeric(sigfig(Cost)), big.mark = ","))

fig_5_order <- scmd_section_spend %>%
  bind_rows(pcmd_section_spend) %>%
  group_by(BNF_SECTION) %>%
  summarise(Cost= sum(Cost)) %>%
  ungroup() %>%
  arrange(desc(Cost)) %>%
  head(10)

scmd_section_spend <- scmd_section_spend %>%
  filter(BNF_SECTION %in% fig_5_order$BNF_SECTION) %>%
  arrange(match(BNF_SECTION, c(fig_5_order$BNF_SECTION)))

pcmd_section_spend <- pcmd_section_spend %>%
  filter(BNF_SECTION %in% fig_5_order$BNF_SECTION) %>%
  arrange(match(BNF_SECTION, c(fig_5_order$BNF_SECTION)))


figure_5 <- highchart() %>%
  hc_add_series(
    name = "Primary care in the community",
    type = "column",
    data = pcmd_section_spend %>% filter(LVL_5_LTST_TYPE == "Primary care prescribing dispensed in the community"),
    color = "#009639",
    hcaes(
      x = BNF_SECTION,
      y = Cost,
      sf = sf,
      desc = SECTION_DESCR
    )
  ) %>%
  hc_add_series(
    name = "Issued in hospitals",
    type = "column",
    data = scmd_section_spend,
    color = "#005EB8",
    hcaes(
      x = BNF_SECTION,
      y = Cost,
      sf = sf,
      desc = SECTION_DESCR
    )
  ) %>%
  hc_add_series(
    name = "Hospital in the community",
    type = "column",
    data = pcmd_section_spend %>% filter(LVL_5_LTST_TYPE == "Hospital prescribing dispensed in the community"),
    color = "#7C2855",
    hcaes(
      x = BNF_SECTION,
      y = Cost,
      sf = sf,
      desc = SECTION_DESCR
    )
  ) %>%
  hc_add_series(
    name = "Dental in the community",
    type = "column",
    data = pcmd_section_spend %>% filter(LVL_5_LTST_TYPE == "Dental prescribing dispensed in the community"),
    color = "#768692",
    hcaes(
      x = BNF_SECTION,
      y = Cost,
      sf = sf,
      desc = SECTION_DESCR
    )
  ) %>%
  hc_xAxis(
    type = "category",
    title =list(
      text = "BNF Chapter"
    )
  ) %>%
  hc_yAxis(
    title =list(
      text = "Costs (GBP)"
    )
  ) %>%
  hc_plotOptions(
    column = list(
      stacking = "normal"
    )
  ) %>%
  hc_tooltip(
    shared = T,
    headerFormat = '{point.key}: {point.point.desc}<br>',
    pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>£{point.sf}</b><br/>'
  ) %>%
  hc_title(
    text = "Figure 5: Top 10 BNF Sections by spend across all settings in 2020/21",
    style = list(fontSize = "16px",
                 fontWeight = "bold")
  ) %>%
  highcharter::hc_credits(enabled = TRUE)

#figure_5

##figure 6 lastest fy costs by stp chapter

scmd_stp_spend <- stp_bnf_data_clean %>%
  filter(FY == "2020/2021") %>%
  group_by(STP21.Name, STP20CDH) %>%
  summarise(Cost = sum(Cost)) %>%
  mutate(sf = prettyNum(as.numeric(sigfig(Cost)), big.mark = ",")) %>%
  ungroup() %>%
  mutate(STP20CDH = case_when(
    is.na(STP21.Name) ~ "Undefined",
    TRUE ~ STP20CDH
  ),
  STP21.Name = case_when(
    is.na(STP21.Name) ~ "Undefined",
    TRUE ~ STP21.Name
  ))

pcmd_stp_spend <- pcmd_stp %>%
  filter(FINANCIAL_YEAR == "2020/2021") %>%
  group_by(LVL_5_LTST_TYPE, STP21.Name, STP_CODE) %>%
  summarise(Cost = sum(ITEM_PAY_DR_NIC)) %>%
  ungroup() %>%
  mutate(sf = prettyNum(as.numeric(sigfig(Cost)), big.mark = ",")) %>%
  filter(LVL_5_LTST_TYPE != "Dental prescribing dispensed in the community")

scmd_for_group <- scmd_stp_spend %>%
  select(STP20CDH, Cost) %>%
  rename(
    "STP" = 1
  )

pcmd_for_group <- pcmd_stp_spend %>%
  select(STP_CODE, Cost) %>%
  rename(
    "STP" = 1
  )

fig_6_order <- scmd_for_group %>%
  bind_rows(pcmd_for_group) %>%
  group_by(STP) %>%
  summarise(totalcost = sum(Cost)) 

scmd_stp_spend <- scmd_stp_spend %>%
  left_join(fig_6_order, by = c("STP20CDH" = "STP")) %>%
  arrange(desc(totalcost))

pcmd_stp_spend <- pcmd_stp_spend %>%
  left_join(fig_6_order, by = c("STP_CODE" = "STP")) %>%
  arrange(desc(totalcost))

figure_6 <- highchart() %>%
  hc_add_series(
    name = "Primary care in the community",
    type = "column",
    data = pcmd_stp_spend %>% filter(LVL_5_LTST_TYPE == "Primary care prescribing dispensed in the community"),
    color = "#009639",
    hcaes(
      x = STP_CODE,
      y = Cost,
      sf = sf,
      stp = STP21.Name
    )
  ) %>%
  hc_add_series(
    name = "Issued in hospitals",
    type = "column",
    data = scmd_stp_spend,
    color = "#005EB8",
    hcaes(
      x = STP20CDH,
      y = Cost,
      sf = sf,
      stp = STP21.Name
    )
  ) %>%
  hc_add_series(
    name = "Hospital in the community",
    type = "column",
    data = pcmd_stp_spend %>% filter(LVL_5_LTST_TYPE == "Hospital prescribing dispensed in the community"),
    color = "#7C2855",
    hcaes(
      x = STP_CODE,
      y = Cost,
      sf = sf,
      stp = STP21.Name
    )
  ) %>%
  # hc_add_series(
  #   name = "Dental in the community",
  #   type = "column",
  #   data = pcmd_stp_spend %>% filter(LVL_5_LTST_TYPE == "Dental prescribing dispensed in the community"),
  #   color = "#768692",
  #   hcaes(
  #     x = STP21.Name,
  #     y = Cost,
  #     sf = sf
  #   )
  # ) %>%
  hc_xAxis(
    type = "category",
    title =list(
      text = "STP"
    )
  ) %>%
  hc_yAxis(
    title =list(
      text = "Costs (GBP)"
    )
  ) %>%
  hc_plotOptions(
    column = list(
      stacking = "normal"
    )
  ) %>%
  hc_tooltip(
    shared = T,
    headerFormat = '{point.key}: {point.point.stp}<br>',
    pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>£{point.sf}</b><br/>'
  ) %>%
  hc_title(
    text = "Figure 6: Total costs by STP in the 2020/2021 financial year",
    style = list(fontSize = "16px",
                 fontWeight = "bold")
  ) %>%
  highcharter::hc_credits(enabled = TRUE)

#figure_6

##figure 7 build
scmd_per_capita <- stp_bnf_data_clean %>%
  mutate(LVL_5_LTST_TYPE = "Hospital prescribing issued within hospitals") %>%
  group_by(
    FY, LVL_5_LTST_TYPE, STP20CDH, STP21.Name 
  ) %>%
  summarise(Cost = sum(Cost)) %>%
  spread(LVL_5_LTST_TYPE, Cost) %>%
  replace(is.na(.), "Undefined")

names(scmd_per_capita)[1:3] <- c("Financial Year", "STP Code", "STP")

pcmd_per_capita <- pcmd_stp %>%
  group_by(FINANCIAL_YEAR, LVL_5_LTST_TYPE, STP_CODE, STP21.Name) %>%
  summarise(Cost = sum(ITEM_PAY_DR_NIC)) %>%
  spread(LVL_5_LTST_TYPE, Cost) %>%
  replace(is.na(.), 0)

names(pcmd_per_capita)[1:3] <- c("Financial Year", "STP Code", "STP")

figure_7_table <- scmd_per_capita %>%
  left_join(pcmd_per_capita) %>%
  replace(is.na(.), 0) %>%
  mutate(Total= sum(across(contains("prescribing")), na.rm = T)) %>%
  arrange(`STP`)%>%
  arrange(`Financial Year`) %>%
  left_join(stp_pop, by = c("STP Code" = "STP20CDH")) %>%
  mutate(`Total Cost Per Capita` = Total/Pop) %>%
  filter(`Financial Year` == "2020/2021")

ave_per_capita <- sum(figure_7_table$Total, na.rm = T)/sum(figure_7_table$Pop, na.rm = T)

figure_7_table <- figure_7_table %>%
  filter(`STP Code` != "Undefined") %>%
  select(`STP Code`, STP, `Total Cost Per Capita`) %>%
  arrange(desc(`Total Cost Per Capita`)) %>%
  mutate(sf = prettyNum(as.numeric(sigfig(`Total Cost Per Capita`)), big.mark = ","),
         ave = ave_per_capita,
         sf_ave = prettyNum(as.numeric(sigfig(ave)), big.mark = ","))

figure_7<- highchart() %>%
  hc_add_series(
    name = "STP",
    type = "column",
    data = figure_7_table,
    color = "#005EB8",
    hcaes(
      x = `STP Code`,
      y = `Total Cost Per Capita`,
      sf = sf,
      stp = STP
    )
  ) %>%
  hc_add_series(
    name = "National average",
    type = "line",
    data = figure_7_table,
    color = "black",
    hcaes(
      x = `STP Code`,
      y = ave,
      sf = sf_ave
    )
  ) %>%
hc_xAxis(
  type = "category",
  title =list(
    text = "STP"
  )
) %>%
  hc_yAxis(
    title =list(
      text = "Costs (GBP)"
    )
  ) %>%
  hc_plotOptions(
    column = list(
      stacking = "normal"
    ),
    line = list(
      marker = list(
        enabled = FALSE
      )
    )
  ) %>%
  hc_tooltip(
    shared = T,
    headerFormat = '{point.key}: {point.point.stp}<br>',
    pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>£{point.sf}</b><br/>'
  ) %>%
  hc_title(
    text = "Figure 7: Average costs per capita by STP in the 2020/2021 financial year",
    style = list(fontSize = "16px",
                 fontWeight = "bold")
  ) %>%
  highcharter::hc_credits(enabled = TRUE)

figure_7

## figure 8 build - comparison of hospital costs in new data and iqvia data

##read in old data

scmd_annual_spend_comp <- scmd_monthly %>%
  group_by(FY) %>%
  summarise(Cost = sum(Total)) %>%
  mutate(sf = prettyNum(as.numeric(sigfig(Cost)), big.mark = ",")) %>%
  filter(FY != "2020/2021")

iqvia_data <- read_excel("iqvia_data/201920-tables.xlsx", sheet = "Table1")

iqvia_data_table <- data.frame(
  "FINANCIAL_YEAR" = c("2016/2017", "2017/2018", "2018/2019", "2019/2020"),
  "Cost" = c(as.numeric(iqvia_data[11,6]) * 1000000,
             as.numeric(iqvia_data[11,7]) * 1000000,
             as.numeric(iqvia_data[11,8]) * 1000000,
             as.numeric(iqvia_data[11,9]) * 1000000)
)%>%
  mutate(sf = prettyNum(as.numeric(sigfig(Cost)), big.mark = ","))

figure_8 <- highchart() %>%
  hc_add_series(
    name = "Issued in hospitals (at list price)",
    type = "column",
    data = iqvia_data_table,
    color = "#41B6E6",
    hcaes(
      x = FINANCIAL_YEAR,
      y = Cost,
      sf = sf
    )
  ) %>%
  hc_add_series(
    name = "Issued in hospitals (actual costs)",
    type = "column",
    data = scmd_annual_spend_comp,
    color = "#005EB8",
    hcaes(
      x = FY,
      y = Cost,
      sf = sf
    )
  ) %>%
  hc_xAxis(
    type = "category",
    title =list(
      text = "Financial year"
    )
  ) %>%
  hc_yAxis(
    title =list(
      text = "Costs (GBP)"
    )
  ) %>%
  hc_tooltip(
    shared = T,
    pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>£{point.sf}</b><br/>'  ) %>%
  hc_title(
    text = "Figure 8: Comparison of costs in hospitals reported from actual costs to reporting costs at list price",
    style = list(fontSize = "16px",
                 fontWeight = "bold")
  ) %>%
  highcharter::hc_credits(enabled = TRUE)

#figure_8


#render the markdown
rmarkdown::render(
  "Outputs/pres-costs-hosp-comm-v1.Rmd",
  output_dir = "./Outputs",
  output_file = paste0("pchc-2020-2021-narrative-v001.html")
)

rmarkdown::render(
  "Outputs/pres-costs-hosp-comm-v1.Rmd",
  output_format = "word_document",
  output_dir = "./Outputs",
  output_file = paste0("pchc-2020-2021-narrative-v001.docx")
)

