add_column <- function(dat = NULL, nominator = NULL, denominator = NULL){
    if (!is.null(dat) & !is.null(nominator) & !is.null(denominator)) {
        if(all(unique(c(nominator, denominator)) %in% colnames(dat))){
            nominator = sort(unique(nominator))
            denominator = sort(unique(denominator))
            dat = dat %>% mutate(newly_add_column = rowSums(.[nominator])/rowSums(.[denominator]))
            colnames(dat)[colnames(dat) == "newly_add_column"] = paste(
              paste(nominator, collapse = ""),
              paste(denominator, collapse = ""),
              sep = "_vs_"
            )
        }
    } else if(!is.null(dat) & !is.null(nominator) & is.null(denominator)){
        if(all(unique(nominator) %in% colnames(dat)) & length(unique(nominator))>1){
            nominator = sort(unique(nominator))
            dat = dat %>% mutate(newly_add_column = rowSums(.[nominator]))
            colnames(dat)[colnames(dat) == "newly_add_column"] = paste(nominator, collapse = "")
        }
    }
    return(dat)
}

keep_column <- function(dat = NULL, column = NULL){
    if(is.null(dat) | is.null(column)){
        return(NULL)
    }
    column = c(column, "Line", "line", "Taxa", "taxa", "Population", "population")
    index = NULL
    for(i in 1:length(column)){
        if(i == 1){
            index = str_detect(colnames(dat), column[i])
        } else{
            index = index | str_detect(colnames(dat), column[i])
        }
    }
    if(!is.null(index)){
        dat = dat[, index]
    }
    return(dat)
}



func_BCAA_Fam <- function(dat = NULL, type = NULL){
    if(is.null(dat) | is.null(type)){
        return(NULL)
    }

    if(type == "FAA"){
        dat = add_column(dat, c("I", "V"))
        dat = add_column(dat, c("I", "L"))
        dat = add_column(dat, c("L", "V"))
        dat = add_column(dat, c("I", "L", "V"))
        dat = add_column(dat, "I", "Total")
        dat = add_column(dat, "V", "Total")
        dat = add_column(dat, "L", "Total")
        dat = add_column(dat, "I", "V")
        dat = add_column(dat, "I", "L")
        dat = add_column(dat, "I", c("I", "V"))
        dat = add_column(dat, "I", c("I", "L"))
        dat = add_column(dat, "I", c("V", "L"))
        dat = add_column(dat, "I", c("I", "V", "L"))
        dat = add_column(dat, "V", "I")
        dat = add_column(dat, "V", "L")
        dat = add_column(dat, "V", c("I", "V"))
        dat = add_column(dat, "V", c("I", "L"))
        dat = add_column(dat, "V", c("V", "L"))
        dat = add_column(dat, "V", c("I", "V", "L"))
        dat = add_column(dat, "L", "I")
        dat = add_column(dat, "L", "V")
        dat = add_column(dat, "L", c("I", "V"))
        dat = add_column(dat, "L", c("I", "L"))
        dat = add_column(dat, "L", c("V", "L"))
        dat = add_column(dat, "L", c("I", "V", "L"))
        dat = keep_column(dat, c("V", "I", "L", "Total"))
        return(dat)
    } else if(type == "BAA"){
        dat = add_column(dat, c("i", "v"))
        dat = add_column(dat, c("i", "l"))
        dat = add_column(dat, c("l", "v"))
        dat = add_column(dat, c("i", "l", "v"))
        dat = add_column(dat, "i", "total")
        dat = add_column(dat, "v", "total")
        dat = add_column(dat, "l", "total")
        dat = add_column(dat, "i", "v")
        dat = add_column(dat, "i", "l")
        dat = add_column(dat, "i", c("i", "v"))
        dat = add_column(dat, "i", c("i", "l"))
        dat = add_column(dat, "i", c("v", "l"))
        dat = add_column(dat, "i", c("i", "v", "l"))
        dat = add_column(dat, "v", "i")
        dat = add_column(dat, "v", "l")
        dat = add_column(dat, "v", c("i", "v"))
        dat = add_column(dat, "v", c("i", "l"))
        dat = add_column(dat, "v", c("v", "l"))
        dat = add_column(dat, "v", c("i", "v", "l"))
        dat = add_column(dat, "l", "i")
        dat = add_column(dat, "l", "v")
        dat = add_column(dat, "l", c("i", "v"))
        dat = add_column(dat, "l", c("i", "l"))
        dat = add_column(dat, "l", c("v", "l"))
        dat = add_column(dat, "l", c("i", "v", "l"))
        dat = keep_column(dat, c("i", "v", "l", "total"))
        return(dat)
    }
    return(NULL)
}
