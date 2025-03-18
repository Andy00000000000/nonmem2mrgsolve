## Manipulating and Creating Each Block ####

## FUNCTION $PROB $INPUT $THETA ####

get_block_input <- function(ctl0 = NULL, ext0 = NULL){

  remove_param <- c("AMT", "DUR", "RATE", "DV", "MDV", "EVID", "CMT", "TIME", "ID", "$INPUT") # common NONMEM variables that should not appear in mrgsolve PARAM

  note_param1 <- c("DOSEN", "DOSENUM", "OCC", "CYC", "CYCLE", "DAY")
  note_param2 <- "TSFD"
  note_param3 <- c("TSLD","TSLDE")
  note_param4 <- c("DOSEUG","DOSEMG","DOSE","DOSEMGKG","DOSENG")

  note1 <- " /* NOTE: Parameter identified as time-varying. A full dataset must be supplied to mrgsolve or the parameter should instead be calculated in $ODE. */"
  note2 <- " /* NOTE: Parameter identified as time-varying. Please refer to TIME, self.time(), and SOLVERTIME if TSFD is equivalent to TIME. */"
  note3 <- " /* NOTE: Parameter identified as time-varying. Please refer to self.tad() and mrg::tadose. */"
  note4 <- " /* NOTE: Parameter identified as related to AMT. Please refer to self.amt() or supply in the dataset. */"

  input <- ctl0 %>%
    dplyr::filter(BLOCK == "INPUT")%>%
    dplyr::select(V1)%>%
    dplyr::mutate(V1 = paste(V1, collapse = " "))%>% # join all rows of input to single row
    dplyr::distinct() # remove duplicates

  input <- data.frame(V1 = strsplit(input$V1, "\\s")[[1]]) %>% # single row to many by white space
    dplyr::mutate(V1 = trimws(V1, which = "both"))%>%
    dplyr::filter(V1 != "")

  input <- input %>%
    dplyr::mutate(FLG_RM = ifelse(grepl("=DROP",V1) == T, 1, 0)) %>% # remove dropped parameters
    dplyr::mutate(FLG_RM = ifelse(toupper(V1) %in% remove_param, 1, FLG_RM)) %>% # remove input parameters common to NONMEM that should not appear in mrgsolve PARAM
    dplyr::filter(FLG_RM == 0)%>%
    dplyr::select(-FLG_RM)

  compareTo <- ctl0 %>%
    dplyr::filter(BLOCK == "DES" | BLOCK == "PK")%>%
    dplyr::select(V1) %>%
    dplyr::mutate(V1 = paste(V1, collapse = " "))%>% # join all rows of input to single row
    dplyr::distinct() # remove duplicates

  for(i in 1:length(input$V1)){
    input[i, "FLG_KP"] <- length(grep(input[i,"V1"],compareTo$V1)) # keep only input parameters found in either $PK or $DES of CTL
  }

  input <- input %>%
    dplyr::filter(FLG_KP == 1)%>%
    dplyr::select(V1)

  mrg_code <- input %>%
    dplyr::mutate(NOTE = ifelse(toupper(V1) %in% note_param1, note1,
                                ifelse(toupper(V1) %in% note_param2, note2,
                                       ifelse(toupper(V1) %in% note_param3, note3,
                                              ifelse(toupper(V1) %in% note_param4, note4,
                                                     "")))))%>%
    dplyr::mutate(V1 = paste0(V1,"=NA_real_",ifelse(dplyr::row_number() == max(dplyr::row_number()), "", ""),NOTE))%>% # removed "," from second ifelse 3/17/2024
    dplyr::select(-NOTE)

  mrg_code <- ctl0 %>%
    dplyr::filter(substr(BLOCK,1,4) == "PROB")%>%
    dplyr::select(V1)%>%
    dplyr::mutate(V1 = gsub("\\$..*?\\s","$PROB ",V1))%>%
    dplyr::bind_rows(blank_df())%>%
    dplyr::bind_rows(data.frame(V1 = "$PARAM"))%>%
    dplyr::bind_rows(blank_df())%>%
    dplyr::bind_rows(mrg_code)

  names(ext0$THETA) <- NULL

  mrg_theta <- data.frame(V1 = c("$THETA","",paste(ext0$THETA, collapse = " ")))

  mrg_code <- mrg_code %>%
    dplyr::bind_rows(blank_df())%>%
    dplyr::bind_rows(mrg_theta)

  return(list(params = input, mrg_code = mrg_code))
}

## FUNCTION $MODEL ####

get_block_model <- function(ctl0 = NULL, mrg_code = NULL){

  input <- ctl0 %>%
    dplyr::filter(BLOCK == "MODEL")%>%
    dplyr::select(V1)%>%
    dplyr::mutate(V1 = paste(V1, collapse = " "))%>% # join all rows of input to single row
    dplyr::distinct() # remove duplicates

  input <- data.frame(V1 = strsplit(input$V1, "\\s")[[1]]) %>% # single row to many by white space
    dplyr::mutate(V1 = trimws(V1, which = "both"))%>% # remove white space
    dplyr::filter(V1 != "")%>% # remove white space
    dplyr::mutate(V1 = ifelse(V1 == "COMP" | V1 == "COMP=", ifelse(dplyr::lead(V1)=="=", paste0(V1,"=",dplyr::lead(V1,2)), paste0(V1,"=",dplyr::lead(V1))), V1))%>% # add = sign if not used
    dplyr::filter(substr(V1,1,4) == "COMP") %>% # filter to just compartment declares
    dplyr::mutate(V1 = gsub("COMP","",V1))%>%
    dplyr::mutate(V1 = gsub("=","",V1))%>% # get down to just names and options
    dplyr::mutate(V1 = gsub("\\(","",V1))%>%
    dplyr::mutate(V1 = gsub("\\)","",V1))%>%
    dplyr::mutate(V1 = gsub("\\,"," ",V1))%>%
    dplyr::mutate(V1 = gsub("\\s.*","",V1)) # remove anything after first white space

  mrg_code <- mrg_code %>%
    dplyr::bind_rows(blank_df())%>%
    dplyr::bind_rows(data.frame(V1 = "$CMT"))%>%
    dplyr::bind_rows(blank_df())%>%
    dplyr::bind_rows(input)%>%
    dplyr::bind_rows(blank_df())%>%
    # bind_rows(data.frame(V1 = "$PLUGIN Rccp"))%>%
    # bind_rows(blank_df())%>%
    dplyr::bind_rows(data.frame(V1 = "$MAIN"))%>%
    dplyr::bind_rows(blank_df())

  return(list(cmts = input, mrg_code = mrg_code))
}

## FUNCTION $PK $DES ####

get_block_pk <- function(ctl0 = NULL, mrg_code = NULL, cmts = NULL){

  ### Initial Syntax Changes ####

  input <- ctl0 %>%
    dplyr::filter((BLOCK == "PK" | BLOCK == "DES"), FLG_BLOCK != 1)%>%
    dplyr::select(V1,BLOCK)%>%

    dplyr::mutate(V1 = gsub("EXP\\(","exp\\(",V1))%>% # convert to mrgsolve syntax
    dplyr::mutate(V1 = gsub("LOG\\(","log\\(",V1))%>%
    dplyr::mutate(V1 = gsub("LOG10\\(","log10\\(",V1))%>%
    dplyr::mutate(V1 = gsub("SQRT\\(","sqrt\\(",V1))%>%

    dplyr::mutate(V1 = gsub("\\s*ELSE\\s*IF\\s*\\(\\s*", "}else if\\(",V1))%>% # 06aug2024 changed all \\s to \\s* in this code header section; put call w/ most whitespace replacement first
    dplyr::mutate(V1 = gsub("\\s*ELSE\\s*IF\\s*\\(", "}else if\\(",V1))%>% # 06aug2024
    dplyr::mutate(V1 = gsub("ELSE\\s*IF\\s*\\(\\s*", "}else if\\(",V1))%>% # 06aug2024
    dplyr::mutate(V1 = gsub("ELSE\\s*IF\\s*\\(", "}else if\\(",V1))%>% # 06aug2024
    dplyr::mutate(V1 = gsub("\\s*ELSE\\s*IF\\(\\s*", "}else if\\(",V1))%>% # 06aug2024
    dplyr::mutate(V1 = gsub("\\s*ELSE\\s*IF\\(", "}else if\\(",V1))%>% # 06aug2024
    dplyr::mutate(V1 = gsub("ELSE\\s*IF\\(\\s*", "}else if\\(",V1))%>% # 06aug2024
    dplyr::mutate(V1 = gsub("ELSE\\s*IF\\(", "}else if\\(",V1))%>% # 06aug2024

    dplyr::mutate(V1 = gsub("\\s*IF\\s*\\(\\s*","if\\(",V1))%>% # 06aug2024
    dplyr::mutate(V1 = gsub("\\s*IF\\s*\\(","if\\(",V1))%>% # 06aug2024
    dplyr::mutate(V1 = gsub("IF\\s*\\(\\s*","if\\(",V1))%>% # 06aug2024
    dplyr::mutate(V1 = gsub("IF\\s*\\(","if\\(",V1))%>%
    dplyr::mutate(V1 = gsub("\\s*IF\\(\\s*","if\\(",V1))%>% # 06aug2024
    dplyr::mutate(V1 = gsub("\\s*IF\\(","if\\(",V1))%>% # 06aug2024
    dplyr::mutate(V1 = gsub("IF\\(\\s*","if\\(",V1))%>% # 06aug2024
    dplyr::mutate(V1 = gsub("IF\\(","if\\(",V1))%>%

    dplyr::mutate(V1 = gsub("\\s*ELSE\\s*","}else{",V1))%>% # 06aug2024
    dplyr::mutate(V1 = gsub("\\s*ELSE","}else{",V1))%>% # 06aug2024
    dplyr::mutate(V1 = gsub("ELSE\\s*","}else{",V1))%>% # 06aug2024
    dplyr::mutate(V1 = gsub("ELSE","}else{",V1))%>%

    dplyr::mutate(V1 = gsub("\\s*THEN\\s*","\\{",V1))%>% # 06aug2024
    dplyr::mutate(V1 = gsub("\\s*THEN","\\{",V1))%>% # 06aug2024
    dplyr::mutate(V1 = gsub("THEN\\s*","\\{",V1))%>% # 06aug2024
    dplyr::mutate(V1 = gsub("THEN","\\{",V1))%>%

    dplyr::mutate(V1 = gsub("\\s*ENDIF\\s*","\\}",V1))%>% #06aug2024
    dplyr::mutate(V1 = gsub("\\s*ENDIF","\\}",V1))%>% #06aug2024
    dplyr::mutate(V1 = gsub("ENDIF\\s*","\\}",V1))%>% #06aug2024
    dplyr::mutate(V1 = gsub("ENDIF","\\}",V1))%>%

    dplyr::mutate(V1 = gsub("\\s*\\.EQ\\.\\s*","==",V1))%>%
    dplyr::mutate(V1 = gsub("\\s*\\.NE\\.\\s*","!=",V1))%>%
    dplyr::mutate(V1 = gsub("\\s*\\.GT\\.\\s*",">",V1))%>%
    dplyr::mutate(V1 = gsub("\\s*\\.LT\\.\\s*","<",V1))%>%
    dplyr::mutate(V1 = gsub("\\s*\\.GE\\.\\s*",">=",V1))%>%
    dplyr::mutate(V1 = gsub("\\s*\\.LE\\.\\s*","<=",V1))%>%
    dplyr::mutate(V1 = gsub("\\s*\\.AND\\.\\s*","\\&\\&",V1))%>% #18mar2025
    dplyr::mutate(V1 = gsub("\\s*\\.OR\\.\\s*","\\|\\|",V1))%>% #18mar2025

    dplyr::mutate(V1 = gsub("\\.EQ\\.\\s*","==",V1))%>%
    dplyr::mutate(V1 = gsub("\\.NE\\.\\s*","!=",V1))%>%
    dplyr::mutate(V1 = gsub("\\.GT\\.\\s*",">",V1))%>%
    dplyr::mutate(V1 = gsub("\\.LT\\.\\s*","<",V1))%>%
    dplyr::mutate(V1 = gsub("\\.GE\\.\\s*",">=",V1))%>%
    dplyr::mutate(V1 = gsub("\\.LE\\.\\s*","<=",V1))%>%
    dplyr::mutate(V1 = gsub("\\.AND\\.\\s*","\\&\\&",V1))%>% #18mar2025
    dplyr::mutate(V1 = gsub("\\.OR\\.\\s*","\\|\\|",V1))%>% #18mar2025

    dplyr::mutate(V1 = gsub("\\s*\\.EQ\\.","==",V1))%>%
    dplyr::mutate(V1 = gsub("\\s*\\.NE\\.","!=",V1))%>%
    dplyr::mutate(V1 = gsub("\\s*\\.GT\\.",">",V1))%>%
    dplyr::mutate(V1 = gsub("\\s*\\.LT\\.","<",V1))%>%
    dplyr::mutate(V1 = gsub("\\s*\\.GE\\.",">=",V1))%>%
    dplyr::mutate(V1 = gsub("\\s*\\.LE\\.","<=",V1))%>%
    dplyr::mutate(V1 = gsub("\\s*\\.AND\\.","\\&\\&",V1))%>% #18mar2025
    dplyr::mutate(V1 = gsub("\\s*\\.OR\\.","\\|\\|",V1))%>% #18mar2025

    dplyr::mutate(V1 = gsub("\\.EQ\\.","==",V1))%>%
    dplyr::mutate(V1 = gsub("\\.NE\\.","!=",V1))%>%
    dplyr::mutate(V1 = gsub("\\.GT\\.",">",V1))%>%
    dplyr::mutate(V1 = gsub("\\.LT\\.","<",V1))%>%
    dplyr::mutate(V1 = gsub("\\.GE\\.",">=",V1))%>%
    dplyr::mutate(V1 = gsub("\\.LE\\.","<=",V1))%>%
    dplyr::mutate(V1 = gsub("\\.AND\\.","\\&\\&",V1))%>% #18mar2025
    dplyr::mutate(V1 = gsub("\\.OR\\.","\\|\\|",V1)) #18mar2025

  ### Flag Conditional Statements ####

  input <- input %>%
    dplyr::mutate(FLG_COND = ifelse(startsWith(V1,"if"), 1,
                                    ifelse(startsWith(V1, "else"), 1,
                                           ifelse(startsWith(V1,"{"), 1,
                                                  ifelse(startsWith(V1, "}"), 1,
                                                         0))))) %>% # mark which lines are conditional statements so that double not added before
    dplyr::mutate(RM_FLG = as.numeric(grepl("ETASXI", V1))) %>% # remove lines of ETASXI since not needed in mrgsolve
    dplyr::filter(RM_FLG == 0)%>%
    dplyr::select(-RM_FLG)%>%
    dplyr::mutate(RM_FLG = ifelse( (endsWith(V1,"{") & dplyr::lead(V1) == "}") | (endsWith(dplyr::lag(V1),"{") & V1 == "}"), 1, 0))%>% # remove empty conditional statements
    dplyr::filter(RM_FLG == 0)%>%
    dplyr::select(-RM_FLG)%>%
    dplyr::mutate(FLG_COND2 = ifelse(FLG_COND == 0 | (endsWith(V1, "{") | V1 == "}" | endsWith(V1, "else")), 0, 1))%>% # identify single line conditional statements for expansion
    dplyr::mutate(ROW = dplyr::row_number())

  ### Expand Single Line Conditional Statements to Multi-line ####

  tmp <- input %>%
    dplyr::filter(FLG_COND2 == 1)%>%
    dplyr::mutate(V1 = gsub("\\s*", "", V1))%>% # remove all white space from single line conditional statements; 06aug2024 changed //s to //s*
    dplyr::mutate(V1 = gsub("\\{", "", V1))%>%  # remove { from single line conditional statements
    dplyr::mutate(V1 = gsub("\\}", "", V1))     # remove } from single line conditional statements

  if(nrow(tmp)>0){

    ind_par <- par_match(data = tmp)

    tmp <- tmp %>%
      dplyr::bind_cols(data.frame(IND = ind_par))

    tmp1 <- tmp %>% # expand out single line conditionals
      dplyr::mutate(V1 = substr(V1,1,IND))%>%
      dplyr::mutate(V1 = paste0(V1,"{"))%>%
      dplyr::select(-IND)

    tmp2 <- tmp %>%
      dplyr::mutate(V1 = substr(V1,IND+1,nchar(V1)))%>%
      dplyr::mutate(ROW = ROW+0.1)%>%
      dplyr::mutate(FLG_COND = 0)%>%
      dplyr::select(-IND)

    tmp3 <- tmp %>%
      dplyr::mutate(V1 = "}")%>%
      dplyr::mutate(ROW = ROW+0.2)%>%
      dplyr::select(-IND)

    input <- input %>%
      dplyr::filter(!ROW %in% tmp$ROW)%>%
      dplyr::bind_rows(tmp1)%>%
      dplyr::bind_rows(tmp2)%>%
      dplyr::bind_rows(tmp3)%>%
      dplyr::arrange(ROW)%>%
      dplyr::select(-FLG_COND2)

    rm(tmp,tmp1,tmp2,tmp3,ind_par)

  }else{

    input <- input %>%
      dplyr::arrange(ROW) %>%
      dplyr::select(-FLG_COND2)
  }

  ### Remove Parenthesis from THETA() ####

  input <- input %>%
    dplyr::mutate(FLG_THETA = as.numeric(grepl("THETA\\(", V1)))

  tmp <- input %>%
    dplyr::filter(FLG_THETA==1)

  ind_par <- stringr::str_locate_all(tmp$V1,"THETA\\(") %>% sapply(., function(x) x[,2]) # locate only ends

  for(i in 1:length(tmp$V1)){

    tmpl <- length(ind_par[[i]])

    tmp[i,"V2"] <- substr(tmp[i,"V1"], 1, ind_par[[i]][1]-1)

    for(j in 1:tmpl){
      tmps <- stringr::str_locate(substr(tmp[i,"V1"], ind_par[[i]][j]+1,nchar(tmp[i,"V1"])),"\\)")+ind_par[[i]][j]
      if(j != tmpl){
        tmp[i,"V2"] <- paste0(tmp[i,"V2"], substr(tmp[i,"V1"], ind_par[[i]][j]+1, tmps-1), substr(tmp[i,"V1"], tmps+1, ind_par[[i]][j+1]-1))
      }else{
        tmp[i,"V2"] <- paste0(tmp[i,"V2"], substr(tmp[i,"V1"], ind_par[[i]][j]+1, tmps-1), substr(tmp[i,"V1"], tmps+1, nchar(tmp[i,"V1"])))
      }
      rm(tmps)
    }
    rm(j,tmpl)
  }
  rm(i,ind_par)

  tmp <- tmp %>%
    dplyr::mutate(V1 = V2)%>%
    dplyr::select(-V2)

  input <- input %>%
    dplyr::filter(!ROW %in% tmp$ROW)%>%
    dplyr::bind_rows(tmp)%>%
    dplyr::arrange(ROW)

  rm(tmp)

  ### Change ** to pow() ####

  input <- input %>%
    dplyr::mutate(FLG_POW = as.numeric(grepl("\\*\\*", V1)))

  tmp <- input %>%
    dplyr::filter(FLG_POW==1)

  for(i in 1:length(tmp$V1)){

    iter<-0
    tmps <- unlist(stringr::str_split(tmp[i,"V1"], "\\*\\*", n = 2))

    while(length(tmps) > 1 & iter < 10000){

      #### first half of pow(x,x) ####

      if(substr(tmps[1],nchar(tmps[1]),nchar(tmps[1])) != ")"){

        ind1 <- stringr::str_locate_all(tmps[1], "\\+") %>% sapply(., function(x) x[,2]) # locate only ends
        ind2 <- stringr::str_locate_all(tmps[1], "\\-") %>% sapply(., function(x) x[,2]) # locate only ends
        ind3 <- stringr::str_locate_all(tmps[1], "\\/") %>% sapply(., function(x) x[,2]) # locate only ends
        ind4 <- stringr::str_locate_all(tmps[1], "\\*") %>% sapply(., function(x) x[,2]) # locate only ends
        ind5 <- stringr::str_locate_all(tmps[1], "\\=") %>% sapply(., function(x) x[,2]) # locate only ends
        ind6 <- stringr::str_locate_all(tmps[1], "\\(") %>% sapply(., function(x) x[,2]) # locate only ends

        indmax <- max(c(unlist(ind1),unlist(ind2),unlist(ind3),unlist(ind4),unlist(ind5),unlist(ind6)))

        tmps[1] <- paste0(substr(tmps[1],1,indmax),"pow(",substr(tmps[1],indmax+1,nchar(tmps[1])),",")

        rm(ind1,ind2,ind3,ind4,ind5,indmax)

      }else{

        revtmps <- stringi::stri_reverse(tmps[1])
        # revind <- str_locate(revtmps,"\\(")[,2]
        revind <- par_match(data = as.data.frame(revtmps), colnme = "revtmps", openChar = "\\)", closedChar = "\\(")
        ind <- nchar(revtmps)-revind+1

        ind2 <- -1

        if(ind >= 5){ # special case
          if(substr(tmps[1], ind-4, ind-1) == "sqrt"){
            ind2 <- ind - 4
          }
        }

        if(ind >= 4){ # special case
          if(substr(tmps[1], ind-3, ind-1) == "exp"){
            ind2 <- ind - 3
          }
        }

        if(ind2 > 0){
          ind <- ind2
        }
        rm(ind2,revtmps,revind)

        tmps[1] <- paste0(substr(tmps[1],1,ind-1),"pow(",substr(tmps[1],ind,nchar(tmps[1])),",")
      }

      #### second half of pow(x,x) ####

      if(substr(tmps[2],1,1)=="(" | substr(tmps[2],1,4)=="exp(" | substr(tmps[2],1,5)=="sqrt("){

        # ind <- str_locate(tmps[2],"\\)")[,2]
        ind <- par_match(data.frame(V1=tmps[2]))
        tmps[2] <- paste0(substr(tmps[2],1,ind),")",substr(tmps[2],ind+1,nchar(tmps[2])))

      }else{

        ind1 <- stringr::str_locate_all(tmps[2], "\\+") %>% sapply(., function(x) x[,2]) # locate only ends
        ind2 <- stringr::str_locate_all(tmps[2], "\\-") %>% sapply(., function(x) x[,2]) # locate only ends
        ind3 <- stringr::str_locate_all(tmps[2], "\\/") %>% sapply(., function(x) x[,2]) # locate only ends
        ind4 <- stringr::str_locate_all(tmps[2], "\\*") %>% sapply(., function(x) x[,2]) # locate only ends
        ind5 <- stringr::str_locate_all(tmps[2], "\\)") %>% sapply(., function(x) x[,2]) # locate only ends

        ind <- min(c(unlist(ind1),unlist(ind2),unlist(ind3),unlist(ind4),unlist(ind5),nchar(tmps[2])))

        if(ind == nchar(tmps[2])){
          tmps[2] <- paste0(trimws(tmps[2]),")") # 06aug2024 added trimws
        }else{
          tmps[2] <- paste0(substr(tmps[2],1,ind-1),")",substr(tmps[2],ind,nchar(tmps[2])))
        }
        rm(ind1,ind2,ind3,ind4)
      }
      rm(ind)

      #### combine pow(x,x) ####

      tmp[i,"V1"] <- paste0(tmps[1], tmps[2])
      tmps <- unlist(stringr::str_split(tmp[i,"V1"], "\\*\\*", n = 2))
      iter<-iter+1
    }
    if(iter==10000){
      print("Error: While loop hit max allowed iterations.")
    }
  }
  rm(i,iter,tmps)

  input <- input %>%
    dplyr::filter(!ROW %in% tmp$ROW)%>%
    dplyr::bind_rows(tmp)%>%
    dplyr::arrange(ROW)%>%
    dplyr::select(-FLG_THETA,-FLG_POW)

  rm(tmp)

  ### Detect Variable Name (before = sign) ####

  special_rm <- c("CALLFL", "MTIME")
  special_convert_rm <- c("N")
  special_convert_kp <- c("F","D","R") # moved "R" and "D" from special_convert_rm 3/17/2024
  special_convert_kp2 <- c("ALAG")
  special_convert_init <- c("A_0(")

  special_rm <- append(special_rm, paste0(special_rm[2],"(",1:1000,")"))

  special_convert_rm <- paste0(special_convert_rm, sort(rep(1:length(cmts$V1), length(special_convert_rm))))
  special_convert_kp <- paste0(special_convert_kp, sort(rep(1:length(cmts$V1), length(special_convert_kp))))
  special_convert_kp2 <- paste0(special_convert_kp2, 1:length(cmts$V1))
  special_convert_init <- paste0(special_convert_init,1:length(cmts$V1),")")

  input <- input %>%
    dplyr::mutate(V2 = stringr::str_split(V1, "=", n = 2, simplify = T)[,1])%>%
    dplyr::mutate(V3 = stringr::str_split(V1, "=", n = 2, simplify = T)[,2])%>%
    dplyr::mutate(V2 = suppressWarnings(ifelse(FLG_COND==1,NA,V2)))%>%
    dplyr::mutate(V3 = suppressWarnings(ifelse(FLG_COND==1,NA,V3)))%>%
    dplyr::mutate(V2 = trimws(V2,"both"))%>%
    dplyr::mutate(V3 = trimws(V3,"both"))%>%
    dplyr::mutate(FLG_COMMENT = ifelse(V2 %in% c(special_rm,special_convert_rm), 1, 0))%>%
    dplyr::mutate(FLG_SPEC1 = ifelse(V2 %in% c(special_convert_rm,special_convert_kp), 1, 0))%>%
    dplyr::mutate(FLG_SPEC2 = ifelse(V2 %in% special_convert_kp2, 1, 0))%>%
    dplyr::mutate(FLG_INIT = ifelse(V2 %in% special_convert_init, 1, 0))%>%
    dplyr::mutate(V1 = suppressWarnings(ifelse(FLG_SPEC1 == 1, paste0(substr(V2,1,1),"_",cmts[as.numeric(substr(V2,2,nchar(V2))),"V1"],"=",V3), V1)))%>%
    dplyr::mutate(V1 = suppressWarnings(ifelse(FLG_SPEC2 == 1, paste0(substr(V2,1,4),"_",cmts[as.numeric(substr(V2,5,nchar(V2))),"V1"],"=",V3), V1)))%>%
    dplyr::mutate(V1 = suppressWarnings(ifelse(FLG_INIT == 1, paste0(cmts[as.numeric(substr(V2,5,nchar(V2)-1)),"V1"],"_0=",V3), V1)))%>%
    dplyr::mutate(V2 = suppressWarnings(ifelse(FLG_SPEC1==1|FLG_SPEC2==1|FLG_INIT==1|FLG_COMMENT==1,NA,V2)))%>%
    dplyr::mutate(V3 = suppressWarnings(ifelse(FLG_SPEC1==1|FLG_SPEC2==1|FLG_INIT==1|FLG_COMMENT==1,NA,V3)))%>%
    dplyr::select(-FLG_SPEC1,-FLG_SPEC2,-FLG_INIT)

  ### If not conditional add ; to end of each line also comment out FLG_COMMENT rows ####

  input <- input %>%
    dplyr::mutate(V1 = trimws(V1,"both"))%>%
    dplyr::mutate(V1 = ifelse(FLG_COND == 0, paste0(V1,";"), V1))%>%
    dplyr::mutate(V3 = ifelse(FLG_COND == 0, paste0(V3,";"), V3))%>% # for ode block
    dplyr::mutate(V1 = ifelse(FLG_COMMENT == 1, paste0("/* ",V1," */"), V1))

  ### If not conditional statement and not ode and not special reserved term add double to first occurrence of each variable ####

  input <- input %>%
    dplyr::mutate(FLG_ODE = ifelse(substr(V1,1,5)=="DADT(",1,0))%>%
    dplyr::group_by(V2)%>%
    dplyr::mutate(VAR_OCC = dplyr::row_number())%>%
    dplyr::ungroup()%>%
    as.data.frame()%>%
    dplyr::mutate(V1 = ifelse(FLG_COMMENT == 0 & FLG_ODE == 0 & FLG_COND == 0 & !is.na(V2) & VAR_OCC == 1, paste0("double ",V1), V1))%>%
    dplyr::mutate(V2.1 = ifelse(FLG_COMMENT == 0 & FLG_ODE == 0 & FLG_COND == 0 & !is.na(V2) & VAR_OCC == 1, paste0("double ",V2), V2))%>% # for ode block
    dplyr::select(-FLG_COMMENT,-VAR_OCC)

  ### Replace ODE syntax ####

  tmp <- input %>%
    dplyr::filter(BLOCK == "DES")%>%
    dplyr::mutate(V2.2 = ifelse(FLG_ODE == 1, substr(V2.1,6,nchar(V2.1)-1), V2.1))%>%
    dplyr::mutate(V2.3 = ifelse(FLG_ODE == 1, paste0("dxdt_",cmts[V2.2,"V1"]), V2.2))

  tmp2 <- tmp %>%
    dplyr::filter(FLG_ODE == 0, !is.na(V2))

  params2 <- c()

  if(length(tmp2$V2) > 0){
    params2 <- c(params2,unique(tmp2$V2))
  }
  rm(tmp2)

  tmp <- tmp %>%
    dplyr::select(-V2,-V2.1,-V2.2,-FLG_ODE)

  for(j in 1:nrow(tmp)){
    for(i in 1:nrow(cmts)){
      tmp[j,"V3"] <- stringr::str_replace_all(tmp[j,"V3"], paste0("A\\(",i,"\\)"), cmts[i,"V1"])
      tmp[j,"V1"] <- stringr::str_replace_all(tmp[j,"V1"], paste0("A\\(",i,"\\)"), cmts[i,"V1"])
    }
    rm(i)
  }
  rm(j)

  tmp <- tmp %>%
    dplyr::mutate(V1 = ifelse(FLG_COND == 0, paste0(V2.3,"=",V3), V1))

  input <- input %>%
    dplyr::filter(BLOCK != "DES")

  params2 <- sort(c(params2, unique(input$V2)))
  params2 <- params2[which(!is.na(params2))]

  input <- input %>%
    dplyr::bind_rows(tmp)%>%
    dplyr::arrange(ROW)%>%
    dplyr::select(V1,BLOCK)

  ### Finalize mrg_code return ####

  mrg_code <- mrg_code %>%
    dplyr::bind_rows(data.frame(V1 = "/* NOTE: There is no guarantee to the accuracy of the NONMEM to mrgsolve translator. It remains the responsibility of the user to validate the resulting mrgsolve code. */"))%>%
    dplyr::bind_rows(blank_df())%>%
    dplyr::bind_rows(data.frame(V1 = "/* NOTE: The nonmem2mrgsolve package remains in active development, please report bugs and feature requests to improve future versions. */"))%>%
    dplyr::bind_rows(data.frame(V1 = "/* NOTE: If you find nonmem2mrgsolve helpful, please consider giving it a star at github.com/Andy00000000000/nonmem2mrgsolve. */"))%>%
    dplyr::bind_rows(blank_df())%>%
    # dplyr::bind_rows(data.frame(V1 = "/* NOTE: If a variable was declared within a conditional statement, then it may be missing double before the variable name. */"))%>% # comment out 3/17/2024
    dplyr::bind_rows(data.frame(V1 = "/* NOTE: The translator does not currently convert T or TIME to SOLVERTIME. */"))%>%
    dplyr::bind_rows(data.frame(V1 = "/* NOTE: The translator does not currently convert MTIME() to self.mtime(). */"))%>%
    # dplyr::bind_rows(data.frame(V1 = "/* NOTE: The translator does not currently handle IOV. */"))%>% # comment out 3/17/2024
    dplyr::bind_rows(blank_df())%>%
    dplyr::bind_rows(input %>% dplyr::filter(BLOCK == "PK") %>% dplyr::select(V1))%>%
    dplyr::bind_rows(blank_df())%>%
    dplyr::bind_rows(data.frame(V1 = "$ODE"))%>%
    dplyr::bind_rows(blank_df())%>%
    dplyr::bind_rows(input %>% dplyr::filter(BLOCK == "DES") %>% dplyr::select(V1))%>%
    dplyr::bind_rows(blank_df())%>%
    dplyr::bind_rows(data.frame(V1 = "$OMEGA @block"))%>%
    dplyr::bind_rows(blank_df())

  return(list(mrg_code = mrg_code, params2 = params2))

  ### End of Function ####
}

## FUNCTION $OMEGA ####

get_block_omega <- function(ext0 = NULL, mrg_code = NULL){

  tmpomeg <- ext0$OMEGA
  tmpindex <- lower.tri(tmpomeg,diag=T)
  tmpomeg[which(tmpindex==F)] <- ""

  tmpomeg <- as.data.frame(tmpomeg)
  rownames(tmpomeg) <- NULL
  tmpomeg <- tmpomeg %>%
    dplyr::mutate(V1 = "")

  for(i in 1:(ncol(tmpomeg)-1)){
    tmpomeg <- tmpomeg %>%
      dplyr::mutate(V1 = paste(V1, tmpomeg[,i]))
  }
  rm(i)

  tmpomeg <- tmpomeg %>%
    dplyr::select(V1)%>%
    dplyr::mutate(V1 = trimws(V1,"both"))

  mrg_code <- mrg_code %>%
    dplyr::bind_rows(tmpomeg) %>%
    dplyr::bind_rows(blank_df())%>%
    dplyr::bind_rows(data.frame(V1 = "$CAPTURE"))%>%
    dplyr::bind_rows(blank_df())

  return(mrg_code)
}

## FUNCTION $TABLE ####

get_block_table<- function(ctl0 = ctl, mrg_code = mrg_code, params = all_params, cmts = cmts){

  input <- ctl0 %>%
    dplyr::filter(BLOCK == "TABLE")%>%
    dplyr::select(V1)%>%
    dplyr::mutate(V1 = paste(V1, collapse = " "))%>% # join all rows of input to single row
    dplyr::distinct() # remove duplicates; this line likely isn't needed/redundant, but leaving until time to test

  input <- data.frame(V1 = strsplit(input$V1, "\\s")[[1]]) %>% # single row to many by white space
    dplyr::mutate(V1 = trimws(V1, which = "both"))%>% # remove white space
    dplyr::filter(V1 != "")

  input <- input %>%
    dplyr::filter(V1 != "$STABLE")%>%
    dplyr::filter(V1 %in% params)%>%
    dplyr::filter(!(V1 %in% cmts))%>%
    dplyr::distinct() # remove duplicates; 06aug2024

  mrg_code <- mrg_code %>%
    dplyr::bind_rows(input)%>%
    dplyr::bind_rows(blank_df())

  return(mrg_code)
}

## END ####
