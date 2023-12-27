box::use(
  glue[glue]
)

#' columns
data_cols <- config::get(file = "data_cols.yml")

google_sheet_url  <- Sys.getenv("GOOGLE_SHEET_URL")
google_service_acc_token <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_TOKEN")
google_service_acc_path <- glue(".secrets/{google_service_acc_token}")