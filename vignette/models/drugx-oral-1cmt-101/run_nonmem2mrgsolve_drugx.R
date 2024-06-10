## Start ####

# devtools::install_github("Andy00000000000/nonmem2mrgsolve")
# setwd("...")

library("nonmem2mrgsolve")

nonmem2mrgsolve(
  "mod1.ctl", # filename
  "./vignette/models/drugx-oral-1cmt-101/", # dir
  out.filename = "mrgsolve_code_drugx-oral",
  sigdig = 3
)

source("./vignette/models/drugx-oral-1cmt-101/mrgsolve_code_drugx-oral.R")
mrgsolve::mcode("Test_Unmodified_Translation",code)

# Shortcomings of the current translation which requires the user's attention:
#
# None, this model translates exactly as needed

## End ####
