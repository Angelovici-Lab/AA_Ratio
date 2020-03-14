add_column <- function(dat = NULL, nominator = NULL, denominator = NULL){
    if(is.null(dat) | is.null(nominator) | is.null(denominator)){
        return(NULL)
    }
    if(all(unique(c(nominator, denominator)) %in% colnames(dat))){
        dat = dat %>% mutate(newly_add_column = rowSums(.[nominator])/rowSums(.[denominator]))
        colnames(dat)[colnames(dat) == "newly_add_column"] = paste(
            paste(nominator, collapse = ""),
            paste(denominator, collapse = ""),
            sep = "_vs_"
        )
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



func_Ser_Fam <- function(dat = NULL, type = NULL){
    if(is.null(dat) | is.null(type)){
        return(NULL)
    }

    if(type == "FAA"){
        dat = add_column(dat, "S", "Total")
        dat = add_column(dat, "G", "Total")
        dat = add_column(dat, "S", "G")
        dat = add_column(dat, "S", c("S", "G"))
        dat = add_column(dat, "G", "S")
        dat = add_column(dat, "G", c("S", "G"))
        dat = keep_column(dat, c("S", "G"))
        return(dat)
    } else if(type == "BAA"){
        dat = add_column(dat, "s", "total")
        dat = add_column(dat, "g", "total")
        dat = add_column(dat, "s", "g")
        dat = add_column(dat, "s", c("s", "g"))
        dat = add_column(dat, "g", "s")
        dat = add_column(dat, "g", c("s", "g"))
        dat = keep_column(dat, c("s", "g"))
        return(dat)
    }
    return(NULL)
}
