## FUNCTION WRITE MRGSOLVE ####

#' Write the mrgsolve code to an R file
#'
#' Writes the mrgsolve code, translated from the input NONMEM run, to an R file.
#'
#' @param mrg_code Dataframe of the NONMEM model translated into mrgsolve code
#' @param filename String of the name for the mrgsolve output file without the .R extension
#' @param dir String of the directory path to the NONMEM run files
#'
#' @return R file of the mrgsolve code
#'
#' @examples
#' # writemrgsolve()
#'
#' @export
writemrgsolve <- function(mrg_code = NULL, filename = "mrgsolve_code0", dir = NULL){

  tdir <- ifelse(is.na(dir) | is.null(dir), "", paste0(dir,"/"))
  nme_pth <- paste0(tdir,filename,".R")

  file <- file(nme_pth)

  cat("code <- ' ",file=nme_pth,sep="\n\n")

  for(i in 1:nrow(mrg_code)){
    cat(mrg_code[i,"V1"],file=nme_pth,sep = "\n",append=TRUE)
  }

  cat("'",file=nme_pth,append = T)

  close(file)

  print("Mrgsolve Code was Saved to an R File")
}

## END ####
