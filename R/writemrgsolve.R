## FUNCTION WRITE MRGSOLVE ####

writemrgsolve <- function(mrg_code, filename = "mrgsolve_code0", dir = NULL){
  
  tdir <- ifelse(is.na(dir) | is.null(dir), "", paste0(dir,"/"))
  nme_pth <- paste0(tdir,filename,".R")
  
  file <- file(nme_pth)
  
  cat("code <- ' ",file=nme_pth,sep="\n\n")
  
  for(i in 1:nrow(mrg_code)){
    cat(mrg_code[i,"V1"],file=nme_pth,sep = "\n",append=TRUE)
  }
  
  cat("'",file=nme_pth,append = T)
  
  close(file)
}

## END ####
