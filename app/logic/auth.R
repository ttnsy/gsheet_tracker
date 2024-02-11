box::use(
    dplyr[`%>%`],
    base64enc[base64decode],
    googlesheets4[gs4_auth],
    googledrive[drive_auth],
)

token_path <- "google_service_acc_token.json"

auth_google <- function() {
    tryCatch(
        expr = {
            Sys.getenv("GOOGLE_SERVICE_ACCOUNT_TOKEN_ENCODED") %>%
                base64decode() %>%
                rawToChar() %>%
                writeLines(token_path)

            gs4_auth(path = token_path)
            drive_auth(path = token_path)
        },
        error = function (e) {
            warning(e)
        },
        finally = {
            unlink(token_path)
        }
    )
}
