## Start ####

# devtools::install_github("Andy00000000000/nonmem2mrgsolve")
# setwd("...")

library("nonmem2mrgsolve")

nonmem2mrgsolve(
  "mod1.ctl", # filename
  "./vignette/models/evolocumab-tmdd-qss-pkpd-101/", # dir
  out.filename = "mrgsolve_code_evolocumab-tmdd"
)

source("./vignette/models/evolocumab-tmdd-qss-pkpd-101/mrgsolve_code_evolocumab-tmdd.R")
mrgsolve::mcode("Test_Unmodified_Translation",code)

source("./vignette/models/evolocumab-tmdd-qss-pkpd-101/mrgsolve_code_evolocumab-tmdd_user-modified.R")
mrgsolve::mcode("Test_Modified_Translation",code)

# Shortcomings of the current translation which requires the user's attention:
#
# Line 69: This line needs to be manually changed by the user
#   1) Rename "TIME" to "SOLVERTIME" (since nonmem2mrgsolve does not currently translate reserved time-related variables)

## End ####
