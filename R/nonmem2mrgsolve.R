## FUNCTION MASTER ####

nonmem2mrgsolve <- function(filename = NULL, dir = NULL, sigdig = NULL, write = T, return.orig = F){
  
  keep_block <- c("PROB","INPUT", "MODEL", "PK", "DES", "TABLE")
  
  tsigdig <- ifelse(!is.null(sigdig), sigdig, -1)
  tdir <- ifelse(!is.null(dir), paste0(dir,"/"), "")
  
  btemp <- load_ext(filename, tdir, sigdig = tsigdig)
  
  ext0 <- btemp$ext0
  ext <- btemp$ext
  
  ctl0 <- load_ctl(filename, tdir)
  
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
    
    writemrgsolve(mrg_code, paste0("mrgsolve-code-Ver0_",filename), dir = dir)
  }
  
  if(return.orig == T){
    
    return(list(mrg_code=mrg_code,ctl0=ctl0,ext0=ext0,ext=ext))
    
  }else{
    
    return(mrg_code)
  }
}

## END ####