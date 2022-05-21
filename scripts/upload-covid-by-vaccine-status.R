library(knitr)
library(markdown)
library(rmarkdown)

## knitr loop

render('~/R/CA-Vaccines/covid-19-cases-by-vaccination-status.Rmd', output_file="covid-19-cases-by-vaccination-status.html")
render('~/R/CA-Vaccines/covid-19-hosp-by-vaccination-status.Rmd', output_file="covid-19-hosp-by-vaccination-status.html")
render('~/R/CA-Vaccines/covid-19-deaths-by-vaccination-status.Rmd', output_file="covid-19-deaths-by-vaccination-status.html")

