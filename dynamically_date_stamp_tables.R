# Dynamically Date-Stamp Tables Written to SQL Server --------------------------


# load libraries
library(dplyr)
library(lubridate) # work with dates
library(RODBC)     # connect to sql server
library(stringr)   # work with strings (in sql queries)

path <- "P:/Data Science Unit"

# create dynamic date values that change each month/year
effective_month <- gsub("[-]","",substr(Sys.Date()+months(1),1,7))
dop_first       <- paste0(gsub("[-]","",substr(Sys.Date()-months(1),1,7)),"01")
dop_last        <- gsub("[-]", "",
                        as.Date(paste0(format(Sys.Date(), "%Y-%m"), "-01")) 
                          + months(1) - days(1)
                        )
prior_eff_month <- gsub("[-]","",format(Sys.Date(),"%Y-%m"))
fy_current      <- paste0("FY",
                          as.numeric(substr(format(Sys.Date(),"%Y"),3,4))-1,
                          substr(format(Sys.Date(),"%Y"),3,4))

# create a folder for the month if it does not exist. 
ifelse(!dir.exists(paste0(effective_month," xxx Census")), 
       dir.create(paste0(effective_month," xxx Census")), 
       FALSE)

# turn off scientific notation
options(scipen = 999)

# make a "not in" operator
`%nin%` <- Negate(`%in%`)

# connect to sql server
connection <- odbcConnect("fill_in")

# make the new census table ----------------------------------------------------
# get ids from last month's xxx census table
census <- 
  sqlQuery(mda_knowli, 
           paste0("SELECT id FROM xxx..xxx_census_", 
                  prior_eff_month),
           stringsAsFactors = FALSE,
           as.is = TRUE)

# get the most recent two months of xxx claims
claims <- 
  sqlQuery(mda_knowli,
           gsub("[\n]","",
                paste0("SELECT id, 
                        FROM   ", fy_current,"..v_claim
                        WHERE  ndc in (SELECT ndc 
                                       FROM xxx..xxx_ndc_list)
                        AND dtl_status = 'p'
                        AND dop BETWEEN '", dop_first,"' AND '", 
                                            dop_last,"'")),
           stringsAsFactors = FALSE,
           as.is = TRUE) 


# make a vector of all xxx IDs
options(useFancyQuotes = FALSE) # turn off directional quotes first
all_ids <- paste(sQuote(unique(c(claims$id, census$id))),
                  collapse=",")

# Get current program and latest eligibility month
program <- 
  sqlQuery(connection,
           gsub("[\n]", "",
                paste0(
                  "SELECT TOP 1 WITH TIES 
                          id,
                          program,
                          month
                   FROM  ", fy_current, "..xxx 
                   WHERE id IN (", all_ids ,")   
                   ORDER BY ROW_NUMBER() OVER(PARTITION BY id ORDER BY month DESC)"
                )),
           stringsAsFactors = FALSE,
           as.is = TRUE)


# write tables -----------------------------------------------------------------
## program info
sqlSave(channel   = connection, 
        dat       = program,
        tablename = paste0("xxx..xxx_program", effective_month), 
        rownames  = FALSE,
        safer     = TRUE)
# check the number of rows matches
sqlQuery(connection, 
         paste0("SELECT COUNT(*) 
                 FROM xxx..xxx_program",
                effective_month))

Close(connection)




