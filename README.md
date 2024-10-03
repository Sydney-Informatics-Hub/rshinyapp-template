# Shiny App Deployment Template

This is a template for deploying R Shiny apps.

## Set up local folder environment

```
rshinyapp-template/
│
├── app.R
├── renv.lock
├── .gitignore
├── README.md
├── input_file.qs
├── 001_data_cleaning_script.qmd
│
├── 100_data_raw/ (add to .gitignore)
├── 200_data_clean/ (add to .gitignore)
├── renv/
├── shapefiles/
│   └── *.json
├── www/
│   └── *.png
└── rsconnect/
```
## If using renv

```
renv::init()
renv::snapshot()
```

## Deploying to shinyapps.io

Register and/or log in with your account at https://www.shinyapps.io.

```
library(rsconnect)

rsconnect::setAccountInfo(name='NAME_OF_YOUR_ACCOUNT',
                          token='YOUR_ACCOUNT_TOKEN',
                          secret='YOUR_ACCOUNT_SECRET')

rsconnect::deployApp(appDir = "./",
                     appName = "PROVIDE_NAME",
                     account = "NAME_OF_YOUR_ACCOUNT",
                     forceUpdate=TRUE,
                     appFiles =c("app.R", "renv.lock"))
```
