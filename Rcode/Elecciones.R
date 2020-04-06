# Script to check elections data.

# Elections data was taken from INAFED, CIDAC and other sources.
# Lajous clean all this databases and put them together to create a large dataset.
# The script they used to clean databases is calles "cleaning" and is in "C:\Users\pc Luis\Documents\World Bank\Elecciones\poder_municipal-master"
# The cleaned data (after running the previos script) is "allmunicipalities.Rdata".
# There are 2410 municipalities.

require(dplyr)
require(tidyr)

load("C:/Users/pc Luis/Documents/World Bank/Elecciones/poder_municipal-master/allmunicipalities.RData")

## set system local so that R can read accents and other special characters
Sys.setlocale('LC_ALL', locale = 'es_ES')  

# we only need info from 1993 to 2014.
# There are two observatiosn that have mun_name but do not have neither CVE_ENT or CVE_MUN. Those names are not in INEGI's catalog.
# There are missings in the winning parties (INAFED, CIDAC)

##generate a dataframe for every year in power between 1993 and 2016, and clean party names. 

expan <- all_pres_munis 

expan$year_start <- ifelse(nchar(expan$year_start) == 11, 
                           as.numeric(substr(expan$year_start, 8, nchar(expan$year_start))), as.numeric(expan$year_start))

expan$year_finish <- ifelse(nchar(expan$year_finish) == 11, 
                            as.numeric(substr(expan$year_finish, 8, nchar(expan$year_finish))), as.numeric(expan$year_finish))


#expan$party_INAFED <- ifelse(expan$year_start == 2014 & expan$CVE_ENT == "1" & expan$party_INAFED == "UPT", "PAN-PRD", expan$party_INAFED)

expan <- expan %>% 
  filter(year_start >= 1992)


expan$party_CIDAC <- ifelse(is.na(expan$party_CIDAC), expan$party_INAFED, expan$party_CIDAC)

noparty <- c("COALICIÓN", "AXB", "COAL.", "C.C.", 
             "AXC", "CXBCH", "CXBDT", "CXBCHS", "CXBDC", "DESCONOCIDOS",
             "ALIANZA_", "CXBDCH", "CXBNT", "CXBNDT", "AXM", "CXBP",  "CQRA",
             "CPSL", "CSA", "ALIANZA", "CPBT", "CUXT", 
             "ASD", "CAFV", "AVE", "CXBNCH", "APM", "ATT", "UPT", "CUPG")

expan$party_mixed <- ifelse(expan$party_INAFED  %in% noparty, expan$party_CIDAC, expan$party_INAFED)

year <- data.frame(muncode = rep((unique(expan$muncode)), 25))

year <- arrange(year, muncode)

year$year_start <- rep(1992:2016, 2461)

expan1 <- left_join(year, expan, by = c("muncode", "year_start"))

expan1 <- expan1 %>% group_by(muncode) %>% fill(everything())

expan1 <- tbl_df(expan1)

# I do not know what "CONV" means. It appears in chimalhuacan but it should be PRI

expan1$party_mixed <- ifelse(grepl("PRI", expan1$party_mixed), "PRI", expan1$party_mixed)
expan1$party_mixed <- ifelse(grepl("PAN-PRD|PRD-PAN", expan1$party_mixed), "PRANRD", expan1$party_mixed)
#expan1$party_mixed <- ifelse(grepl("PANAL", expan1$party_mixed, fixed = TRUE), "PNA", expan1$party_mixed)
expan1$party_mixed <- ifelse(grepl("PAN", expan1$party_mixed), "PAN", expan1$party_mixed)
expan1$party_mixed <- ifelse(grepl("PRD", expan1$party_mixed), "PRD", expan1$party_mixed)
expan1$party_mixed <- gsub("PRANRD", "PAN-PRD", expan1$party_mixed)

expan1$party_mixed <- ifelse(grepl("PVEM-PNA|PNA-PVEM", expan1$party_mixed), "PVANA", expan1$party_mixed)
expan1$party_mixed <- ifelse(grepl("PVEM", expan1$party_mixed), "PVEM", expan1$party_mixed)
expan1$party_mixed <- ifelse(grepl("PNA", expan1$party_mixed), "PNA", expan1$party_mixed)
expan1$party_mixed <- gsub("PVANA", "PVEM-PNA", expan1$party_mixed)

expan1$party_mixed <- gsub("TRANSIN", "PRI", expan1$party_mixed)
expan1$party_mixed <- gsub("CPU", "PAN-PRD", expan1$party_mixed)
expan1$party_mixed <- gsub("CXBDT", "PRD", expan1$party_mixed)
expan1$party_mixed <- ifelse(grepl("PAN-PRD", expan1$party_mixed), "PAN-PRD", expan1$party_mixed)
expan1$party_mixed <- gsub("COAL.", "PRI", expan1$party_mixed , fixed = TRUE)
expan1$party_mixed <- ifelse(expan1$party_mixed == "COALICIÓN" & expan1$muncode == " 5025", "PRI",  expan1$party_mixed)
expan1$party_mixed <- ifelse(expan1$party_mixed == "CANDIDATURA COMÚN", "PRI",  expan1$party_mixed)

expan1$party_mixed <- ifelse(expan1$party_mixed == "COALICIÓN" & expan1$CVE_ENT == "14", "PRD",  expan1$party_mixed)
expan1$party_mixed <- ifelse(expan1$party_mixed == "COALICIÓN" & expan1$CVE_ENT == "30", "AVE",  expan1$party_mixed)

#big_p <- c("PRI", "PAN", "PRD", "PAN-PRD", "UYC")

#expan1$party_mixed <- ifelse(!expan1$party_mixed %in% big_p, "OTHER", expan1$party_mixed)

expan92_16 <- expan1 %>%
  #filter(year_start == "1993" | year_start == "1998" | year_start == "2003" | year_start == "2013") %>%
  mutate(muncode = as.numeric(muncode)) %>%
  select(year_elec,year_start,year_finish, muncode,CVE_ENT,CVE_MUN, party_mixed) %>% filter(!is.na(muncode))


data.table::fwrite(expan92_16, "C:/Users/pc Luis/Documents/World Bank/Elecciones/winner_parties_mun_1992_2016.csv")
