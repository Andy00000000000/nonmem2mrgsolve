## Loading Nonmem Files ####

## FUNCTION LOAD CTL ####

#' Load NONMEM ctl (or mod) file into R
#'
#' Loads the NONMEM ctl (or mod) file into R for translation to mrgsolve format.
#'
#' @param filename String of the NONMEM model name without the .ctl (or .mod) extension
#' @param dir String of the directory path to the NONMEM run files
#'
#' @return R dataframe of the NONMEM ctl (or mod) file
#'
#' @examples
#' # load_ctl(filename = "nonmem-model", dir = "path/to/directory/")
#'
#' @export
load_ctl <- function(filename = NULL, dir = ""){

  ctl <- try(suppressWarnings(read.delim2(paste0(dir,filename,".ctl"), header = F)),silent=T)

  if(class(ctl)=="try-error"){
    ctl <- try(suppressWarnings(read.delim2(paste0(dir,filename,".mod"), header = F)),silent=T)
  }

  if(class(ctl)=="try-error"){
    err_msg <- ctl[1]
    ctl <- NULL
  }

  if(!is.null(ctl)){
    ctl <- ctl %>%
      dplyr::mutate(V1 = trimws(V1, which = "both"))%>% # remove leading and trailing white space
      dplyr::filter(substr(V1,1,1) != ";", V1 != "")%>% # remove white space or comment rows
      dplyr::mutate(V1 = gsub(";.*","",V1))%>% # remove anything after comment
      dplyr::mutate(FLG_BLOCK = ifelse(startsWith(V1, "$"), 1, 0))%>% # flag beginning of each block
      dplyr::mutate(BLOCK = suppressWarnings(ifelse(FLG_BLOCK == 1, trimws(substr(V1,2,6), which = "both"), NA)))%>% # get block name
      dplyr::mutate(BLOCK = zoo::na.locf(BLOCK, na.rm = F, fromLast = F)) # pull block name down until next block starts

    print("NONMEM Control Stream Successfully Loaded")
  }else{
    print("NONMEM Control Stream Failed to Load")
    print(err_msg)
  }

  return(ctl)
}

## FUNCTION LOAD EXT ####

#' Load NONMEM ext file into R
#'
#' Loads the NONMEM ext file into R for capture of the final estimates.
#'
#' @param filename String of the NONMEM model name without the .ext extension
#' @param dir String of the directory path to the NONMEM run files
#' @param sigdig Numeric of the number of significant digits to round non-fixed thetas and etas to; -1 for no rounding
#' @param use.cnv Logical for whether to use the NONMEM cnv file for final parameter estimates instead of the ext file (\code{T} or \code{F})
#'
#' @return R list of the NONMEM final OFV, parameter estimates, and IIV magnitudes
#'
#' @examples
#' load_ext(filename = "nonmem-model", dir = "path/to/directory/")
#'
#' @export
load_ext <- function(filename = NULL, dir = "", sigdig = -1, use.cnv = F){

  ext00 <- try(suppressWarnings(read.table(paste0(dir,filename,".ext"),sep='',header=T,fill=T,na.strings=".", skip=1)),silent=T)

  if(class(ext00) != "try-error"){
    if(class(ext00$ITERATION) != "numeric"){
      for(i in 1:ncol(ext00)){
        ext00[,i] <- as.numeric(ext00[,i])
      }
      rm(i)
    }
  }

  if(use.cnv == T){

    ext0 <- try(suppressWarnings(read.table(paste0(dir,filename,".cnv"),sep='',header=T,fill=T,na.strings =".",skip=1)),silent=T)

    if(class(ext0) != "try-error"){
      ext0 <- ext0 %>%
        dplyr::filter(ITERATION == -2000000000)
    }else{
      ext0 <- NULL
    }
  }else{

    if(class(ext00) != "try-error"){
      ext0 <- ext00 %>%
        dplyr::filter(ITERATION == -1000000000) %>% # NONMEM User Guide VIII for definition (final estimates)
        dplyr::filter(dplyr::row_number() == max(dplyr::row_number()))
    }else{
      ext0 <- NULL
    }
  }

  if(!is.null(ext0)){

    print("NONMEM Final Estimates Successfully Loaded")

    tmpfixed <- ext00 %>%
      dplyr::filter(ITERATION == -1000000006) %>% # NONMEM User Guide VIII for definition (fixed parameters)
      dplyr::filter(dplyr::row_number() == max(dplyr::row_number()))

    if(sigdig > 0){
      omnofix <- tmpfixed[1,dplyr::starts_with("OMEGA", vars = colnames(tmpfixed))]

      if(class(omnofix)=="numeric"){ # 3/17/2024 fix load when only 1 IIV
        omnofix <- data.frame("OMEGA.1.1." = omnofix)
      }

      omnofix <- colnames(omnofix[which(omnofix[1,] != 1)])

      if(length(omnofix)>0){ # 3/17/2024 fix load when all IIV fixed
        tmpomnofix <- lapply(1:length(omnofix), function(i){
          nofixtmpa <- strsplit(trimws(omnofix[[i]]),"\\.")
          return(data.frame(R = as.numeric(nofixtmpa[[1]][2]), C = as.numeric(nofixtmpa[[1]][3])))
        })
        omnofix <- dplyr::bind_rows(tmpomnofix)
      }else{
        omnofix <- NULL
      }
    }else{
      omnofix <- NULL
    }

    ext <- list(NITER = NA, OFV = NA, THETA = NA, OMEGA = NA)

    ext$NITER <- max(ext00$ITERATION)
    ext$OFV <- ext0[1,which(substr(colnames(ext0),nchar(colnames(ext0))-2,nchar(ext0)) == "OBJ")]
    ext$THETA <- ext0[1,dplyr::starts_with("THETA",vars = colnames(ext0))]

    tmpom0 <- ext0[1,dplyr::starts_with("OMEGA",vars = colnames(ext0))]
    if(class(tmpom0)=="numeric"){ # 3/17/2024 fix load when only 1 IIV
      tmpom0 <- data.frame("OMEGA.1.1." = tmpom0)
    }

    tmpn <- strsplit(trimws(colnames(tmpom0)[ncol(tmpom0)]), "\\.")
    tmpn <- as.numeric(tmpn[[1]][length(tmpn[[1]])])

    tmpom2 <- matrix(data = 0, ncol = tmpn, nrow = tmpn)
    colnames(tmpom2) <- paste0("OMEGA",1:tmpn)
    rownames(tmpom2) <- paste0("OMEGA",1:tmpn)

    tmpom <- tmpom0[1, which(tmpom0 != 0)]
    if(class(tmpom)=="numeric"){ # 3/17/2024 fix load when only 1 IIV
      tmpom_val <- tmpom
      tmpom <- data.frame()
      tmpom[1,colnames(tmpom0)[which(tmpom0!=0)]] = tmpom_val
    }

    if(length(tmpom)>0){ # 3/17/2024 fix error all IIV fixed to zero
      for(i in 1:length(tmpom)){

        tmp <- strsplit(trimws(colnames(tmpom)[i]),"\\.")[[1]]
        tmpr <- as.numeric(tmp[2])
        tmpc <- as.numeric(tmp[3])

        if(sigdig > 0 & !is.null(omnofix)){ # 3/17/2024 fix load when all iiv fixed

          omvalround <- tmpom[1,i]

          tmpa <- which(omnofix$R == tmpr)

          if(length(tmpa)>0){

            tmpa <- omnofix[tmpa,]
            tmpa <- which(tmpa$C == tmpc)

            if(length(tmpa)>0){

              omvalround <- signif(tmpom[1,i], sigdig)
            }
          }

          tmpom2[tmpr,tmpc] <- omvalround
          tmpom2[tmpc,tmpr] <- omvalround

        }else{

          tmpom2[tmpr,tmpc] <- tmpom[1,i]
          tmpom2[tmpc,tmpr] <- tmpom[1,i]
        }
      }
    }

    ext$OMEGA <- tmpom2

    if(sigdig > 0){

      thnofix <- which(tmpfixed[1,dplyr::starts_with("THETA",vars = colnames(tmpfixed))] != 1)

      if(length(thnofix)>0){
        ext[["THETA"]][thnofix] <- signif(ext[["THETA"]][thnofix],sigdig)
      }
    }
  }else{
    ext <- NULL
    print("NONMEM Final Estimates Failed to Load")
  }

  return(list(ext=ext,ext0=ext0))
}

## END ####
