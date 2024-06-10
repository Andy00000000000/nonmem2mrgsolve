## Start ####

# devtools::install_github("Andy00000000000/nonmem2mrgsolve")
# setwd("...")

library("nonmem2mrgsolve")

nonmem2mrgsolve(
  "mod1.ctl", # filename
  "./vignette/models/mavoglurant-pbpk-101/", # dir
  out.filename = "mrgsolve_code_mavoglurant-pbpk",
  sigdig = 2
)

source("./vignette/models/mavoglurant-pbpk-101/mrgsolve_code_mavoglurant-pbpk.R")
mrgsolve::mcode("Test_Unmodified_Translation",code)

source("./vignette/models/mavoglurant-pbpk-101/mrgsolve_code_mavoglurant-pbpk_user-modified.R")
mrgsolve::mcode("Test_Modified_Translation",code)

# Shortcomings of the current translation which requires the user's attention:
#
# Line 42: This line needs to be manually changed by the user
#   1) Rename "RATE" to a non-reserved term such as "INRATE"
#   2) Add "INRATE" to $PARAM
#   3) Pass "INRATE" into the mrgsolve simulation using either param(INRATE = ...) or data_set()

## End ####
