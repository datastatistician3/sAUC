library(googlesheets)
## prepare the OAuth token and set up the target sheet:
##  - do this interactively
##  - do this EXACTLY ONCE

# shiny_token <- gs_auth() # authenticate w/ your desired Google identity here
# saveRDS(shiny_token, "shiny_app_token.rds")
# ss$sheet_key


## if you version control your app, don't forget to ignore the token file!
## e.g., put it into .gitignore

googlesheets::gs_auth(token = "./inst/shiny-examples/sAUC/shiny_app_token.rds", cache = FALSE)
sAUC_sheet <- gs_title("sAUC Response")
sheet_key <- sAUC_sheet$sheet_key
ss <- googlesheets::gs_key(sheet_key)
