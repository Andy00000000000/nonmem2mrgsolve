## FUNCTION MASTER ####

#' Automated NONMEM to mrgsolve Translation
#'
#' Translates a NONMEM model into mrgsolve syntax using the NONMEM ctl and ext (or cnv) files.
#'
#' @param filename String of the NONMEM model name with or without the .ctl extension
#' @param dir String of the directory path to the NONMEM files (if not already given in the filename input; or if the working directory was not already set)
#' @param sigdig Numeric of the number of significant digits to round non-fixed thetas and etas to; default NULL for no rounding
#' @param write Logical for whether to write the mrgsolve code to an R file (\code{T} or \code{F})
#' @param return.orig Logical for whether to output the original NONMEM ctl and ext files (\code{T} or \code{F})
#' @param out.filename String of the name for the mrgsolve output file without the .R extension
#' @param use.cnv Logical for whether to use the NONMEM cnv file for final parameter estimates instead of the ext file (\code{T} or \code{F})
#'
#' @return R dataframe of the mrgsolve code
#'
#' @examples
#' setwd("path/to/directory")
#' nonmem2mrgsolve::nonmem2mrgsolve(filename = "nonmem-model.ctl")
#'
#' @export
nonmem2mrgsolve <- function(filename = NULL, dir = NULL, sigdig = NULL, write = T, return.orig = F, out.filename = NULL, use.cnv = F){

  keep_block <- c("PROB", "PROBL", "PROBLEM", "INPUT", "MODEL", "PK", "DES", "TABLE")

  in.filename <- gsub(".ctl","",filename)

  tsigdig <- ifelse(!is.null(sigdig), sigdig, -1)
  tdir <- ifelse(!is.null(dir), paste0(dir,"/"), "")
  tusecnv <- ifelse(use.cnv == T, T, F)

  btemp <- load_ext(in.filename, tdir, sigdig = tsigdig, use.cnv = tusecnv)

  ext0 <- btemp$ext0
  ext <- btemp$ext

  ctl0 <- load_ctl(in.filename, tdir)

  ctl <- ctl0 %>%
    dplyr::filter(BLOCK %in% keep_block) # filter to useful blocks

  btemp <- get_block_input(ctl,ext)
  params <- btemp$params
  mrg_code <- btemp$mrg_code

  btemp <- get_block_model(ctl, mrg_code)
  mrg_code <- btemp$mrg_code
  cmts <- btemp$cmts

  btemp <- get_block_pk(ctl, mrg_code, cmts)
  mrg_code <- btemp$mrg_code
  params2 <- btemp$params2

  mrg_code <- get_block_omega(ext0 = ext, mrg_code = mrg_code)

  all_params <- sort(c(params$V1,params2))
  mrg_code <- get_block_table(ctl, mrg_code, all_params, cmts$V1)

  if(write == T){

    outfile <- paste0("mrgsolve-code-V0_",in.filename)

    if(!is.null(out.filename)){
      if(!is.na(out.filename) & out.filename != "" & out.filename != " "){
        outfile <- out.filename
      }
    }

    writemrgsolve(mrg_code, outfile, dir = dir)
  }

  if(return.orig == T){

    return(list(mrg_code=mrg_code,ctl0=ctl0,ext0=ext0,ext=ext))

  }else{

    return(mrg_code)
  }
}

## END ####
