options(scipen = 99)

box::use(
  glue[glue]
)

#' google sheet urls & creds
google_sheet_url  <- Sys.getenv("GOOGLE_SHEET_URL")
google_service_acc_token <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_TOKEN")
google_service_acc_path <- glue(".secrets/{google_service_acc_token}")

#' gsheet names & table to df colnames
data_cols <- config::get(file = "data_cols.yml")

sheet_name_spr <- "spr"
sheet_name_pencairan <- "pencairan"
sheet_name_kontraktor <- "kontraktor"
sheet_name_konstruksi <- "konstruksi"
sheet_name_kontr_progress <- "kontraktor_progress"

cols_spr <- data_cols[[sheet_name_spr]]
cols_pencairan  <- data_cols[[sheet_name_pencairan]]
cols_konstruksi  <- data_cols[[sheet_name_konstruksi]]
cols_kontraktor  <- data_cols[[sheet_name_kontraktor]]
cols_kontr_progress  <- data_cols[[sheet_name_kontr_progress]]
