## Loading Nonmem Files ####

## FUNCTION LOAD CTL ####

#' Load NONMEM ctl file into R
#'
#' Loads the NONMEM ctl model file into R for translation to mrgsolve format.
#'
#' @param filename String of the NONMEM model file name without the .ctl extension
#' @param dir String of the directory path to the NONMEM run files
#'
#' @return R dataframe of the NONMEM ctl file
#'
#' @examples
#' load_ctl()
#'
#' @export
load_ctl <- function(filename = NULL, dir = ""){

  ctl <- read.delim2(paste0(dir,filename,".ctl"), header = F) %>%
    dplyr::mutate(V1 = trimws(V1, which = "both"))%>% # remove leading and trailing white space
    dplyr::filter(substr(V1,1,1) != ";", V1 != "")%>% # remove white space or comment rows
    dplyr::mutate(V1 = gsub(";.*","",V1))%>% # remove anything after comment
    dplyr::mutate(FLG_BLOCK = ifelse(startsWith(V1, "$"), 1, 0))%>% # flag beginning of each block
    dplyr::mutate(BLOCK = suppressWarnings(ifelse(FLG_BLOCK == 1, trimws(substr(V1,2,6), which = "both"), NA)))%>% # get block name
    dplyr::mutate(BLOCK = zoo::na.locf(BLOCK, na.rm = F, fromLast = F)) # pull block name down until next block starts

  return(ctl)
}

## FUNCTION LOAD EXT ####

#' Load NONMEM ext file into R
#'
#' Loads the NONMEM ext model file into R for capture of the final estimates.
#'
#' @param filename String of the NONMEM model file name without the .ext extension
#' @param dir String of the directory path to the NONMEM run files
#' @param sigdig Numeric of the number of significant digits to round non-fixed thetas and etas to; -1 for no rounding
#' @param use.cnv Logical for whether to use NONMEM cnv file final parameter estimates instead of ext estimates (\code{T} or \code{F})
#'
#' @return R list of the NONMEM final OFV, parameter estimates, and IIV magnitudes
#'
#' @examples
#' load_ext()
#'
#' @export
load_ext <- function(filename = NULL, dir = "", sigdig = -1, use.cnv = F){

  ext00 <- read.table(paste0(dir,filename,".ext"),sep='',header=T,fill=T,na.strings=".", skip=1)

  if(class(ext00$ITERATION) != "numeric"){
    for(i in 1:ncol(ext00)){
      ext00[,i] <- as.numeric(ext00[,i])
    }
    rm(i)
  }

  if(use.cnv == T){

    ext0 <- read.table(paste0(dir,filename,".cnv"),sep='',header=T,fill=T,na.strings =".",skip=1)%>%
      dplyr::filter(ITERATION == -2000000000)

  }else{

    ext0 <- ext00 %>%
      dplyr::filter(ITERATION == -1000000000) %>% # NONMEM User Guide VIII for definition (final estimates)
      dplyr::filter(dplyr::row_number() == max(dplyr::row_number()))
  }

  tmpfixed <- ext00 %>%
    dplyr::filter(ITERATION == -1000000006) %>% # NONMEM User Guide VIII for definition (fixed parameters)
    dplyr::filter(dplyr::row_number() == max(dplyr::row_number()))

  if(sigdig > 0){
    omnofix <- tmpfixed[1,dplyr::starts_with("OMEGA", vars = colnames(tmpfixed))]
    omnofix <- colnames(omnofix[which(omnofix[1,] != 1)])

    tmpomnofix <- lapply(1:length(omnofix), function(i){
      nofixtmpa <- strsplit(trimws(omnofix[[i]]),"\\.")
      return(data.frame(R = as.numeric(nofixtmpa[[1]][2]), C = as.numeric(nofixtmpa[[1]][3])))
    })
    omnofix <- dplyr::bind_rows(tmpomnofix)
  }

  ext <- list(NITER = NA, OFV = NA, THETA = NA, OMEGA = NA)

  ext$NITER <- max(ext00$ITERATION)
  ext$OFV <- ext0[1,which(substr(colnames(ext0),nchar(colnames(ext0))-2,nchar(ext0)) == "OBJ")]
  ext$THETA <- ext0[1,dplyr::starts_with("THETA",vars = colnames(ext0))]

  tmpom <- ext0[1,dplyr::starts_with("OMEGA",vars = colnames(ext0))]
  tmpn <- strsplit(trimws(colnames(tmpom)[ncol(tmpom)]), "\\.")
  tmpn <- as.numeric(tmpn[[1]][length(tmpn[[1]])])

  tmpom2 <- matrix(data = 0, ncol = tmpn, nrow = tmpn)
  colnames(tmpom2) <- paste0("OMEGA",1:tmpn)
  rownames(tmpom2) <- paste0("OMEGA",1:tmpn)

  tmpom <- tmpom[1, which(tmpom != 0)]

  for(i in 1:length(tmpom)){

    tmp <- strsplit(trimws(colnames(tmpom)[i]),"\\.")[[1]]
    tmpr <- as.numeric(tmp[2])
    tmpc <- as.numeric(tmp[3])

    if(sigdig > 0){

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

  ext$OMEGA <- tmpom2

  if(sigdig > 0){

    thnofix <- which(tmpfixed[1,dplyr::starts_with("THETA",vars = colnames(tmpfixed))] != 1)

    if(length(thnofix)>0){
      ext[["THETA"]][thnofix] <- signif(ext[["THETA"]][thnofix],sigdig)
    }
  }

  return(list(ext=ext,ext0=ext0))
}

## END ####
