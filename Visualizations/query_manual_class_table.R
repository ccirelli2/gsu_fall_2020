# Clearn Namespace & Plots ------------------------------
rm(list=ls())
dev.off(dev.list()["RStudioGD"])

# IMPORT LIBRARIES
library(ggplot2)
library(DBI)


# Instantiate Connection to DB -------------------------
con <- dbConnect(odbc::odbc(), 
                 Driver = "SQL Server", 
                 Server = "yde2xj08jm.database.windows.net", 
                 Database = "SwyfftAnalyticsCentral", 
                 UID = "AnalyticsReadOnly ", 
                 PWD = rstudioapi::askForPassword("Database password"), 
                 port = 1433)


# Load Data ---------------------------------------------
rt.data <- dbGetQuery(con, '
       SELECT 
	   CAST([CLAIM_NBR/RES_NBR] AS nvarchar(100))
      ,CAST([CLAIM_TYPE] AS varchar(100))
      ,CAST([Claim_Type_Coverage_Group] AS varchar(100))
      ,CAST([COVERAGE] AS varchar(100))
      ,CAST([CAUSE_OF_LOSS] AS varchar(100))
      ,CAST([PERIL] AS varchar(100))
      ,CAST([SUB_PERIL] AS varchar(100))
      ,CAST([ACC_DESC]AS varchar(250))
      ,[Covid_Flag]
  FROM [dbo].[chris_core_claims_reclassification_manual_with_claim_type]')
rt.data
