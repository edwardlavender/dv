#########################
#########################
#### dev.R

#### Aims
# 1) Record package development

#### Prerequisites
# 1) NA


#########################
#########################
#### Set up

#### Wipe workspace
rm(list = ls())
try(pacman::p_unload("all"), silent = TRUE)
cat("\014")

#### Essential packages
library(usethis)


#########################
#########################
#### Package development

#### Set up tidy package
use_tidy_package()

#### Use Git/GitHub
use_git()
use_github(private = TRUE)

#### Add standard files
usethis::use_code_of_conduct("el72@st-andrews.ac.uk")
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md(open = FALSE)


#### End of code.
#########################
#########################
