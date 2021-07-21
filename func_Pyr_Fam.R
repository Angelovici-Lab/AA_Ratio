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



func_Pyr_Fam <- function(dat = NULL, type = NULL){
    if(is.null(dat) | is.null(type)){
        return(NULL)
    }

    if(type == "FAA"){
        dat = add_column(dat, "L", "Total")
        dat = add_column(dat, "A", "Total")
        dat = add_column(dat, "V", "Total")
        dat = add_column(dat, "L", "A")
        dat = add_column(dat, "L", "V")
        dat = add_column(dat, "L", c("A", "L"))
        dat = add_column(dat, "L", c("L", "V"))
        dat = add_column(dat, "L", c("A", "V"))
        dat = add_column(dat, "L", c("A", "L", "V"))
        dat = add_column(dat, "A", "L")
        dat = add_column(dat, "A", "V")
        dat = add_column(dat, "A", c("A", "L"))
        dat = add_column(dat, "A", c("L", "V"))
        dat = add_column(dat, "A", c("A", "V"))
        dat = add_column(dat, "A", c("A", "L", "V"))
        dat = add_column(dat, "V", "L")
        dat = add_column(dat, "V", "A")
        dat = add_column(dat, "V", c("L", "A"))
        dat = add_column(dat, "V", c("V", "L"))
        dat = add_column(dat, "V", c("V", "A"))
        dat = add_column(dat, "V", c("L", "A", "V"))
        dat = keep_column(dat, c("L", "A", "V"))
        return(dat)
    } else if(type == "BAA"){
        dat = add_column(dat, "l", "total")
        dat = add_column(dat, "a", "total")
        dat = add_column(dat, "v", "total")
        dat = add_column(dat, "l", "a")
        dat = add_column(dat, "l", "v")
        dat = add_column(dat, "l", c("l", "a"))
        dat = add_column(dat, "l", c("l", "v"))
        dat = add_column(dat, "l", c("a", "v"))
        dat = add_column(dat, "l", c("a", "l", "v"))
        dat = add_column(dat, "a", "l")
        dat = add_column(dat, "a", "v")
        dat = add_column(dat, "a", c("a", "l"))
        dat = add_column(dat, "a", c("l", "v"))
        dat = add_column(dat, "a", c("a", "v"))
        dat = add_column(dat, "a", c("a", "l", "v"))
        dat = add_column(dat, "v", "l")
        dat = add_column(dat, "v", "a")
        dat = add_column(dat, "v", c("l", "a"))
        dat = add_column(dat, "v", c("v", "l"))
        dat = add_column(dat, "v", c("v", "a"))
        dat = add_column(dat, "v", c("l", "a", "v"))
        dat = keep_column(dat, c("l", "a", "v")) %>% select(-"total") %>% as.data.frame(stringsAsFactors = FALSE)
        return(dat)
    }
    return(NULL)
}
