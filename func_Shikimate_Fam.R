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



func_Shikimate_Fam <- function(dat = NULL, type = NULL){
    if(is.null(dat) | is.null(type)){
        return(NULL)
    }

    if(type == "FAA"){
        dat = add_column(dat, "W", "Total")
        dat = add_column(dat, "F", "Total")
        dat = add_column(dat, "Y", "Total")
        dat = add_column(dat, "W", "F")
        dat = add_column(dat, "W", "Y")
        dat = add_column(dat, "W", c("W", "F"))
        dat = add_column(dat, "W", c("W", "Y"))
        dat = add_column(dat, "W", c("F", "Y"))
        dat = add_column(dat, "W", c("W", "F", "Y"))
        dat = add_column(dat, "F", "W")
        dat = add_column(dat, "F", "Y")
        dat = add_column(dat, "F", c("W", "F"))
        dat = add_column(dat, "F", c("W", "Y"))
        dat = add_column(dat, "F", c("F", "Y"))
        dat = add_column(dat, "F", c("W", "F", "Y"))
        dat = add_column(dat, "Y", "W")
        dat = add_column(dat, "Y", "F")
        dat = add_column(dat, "Y", c("W", "F"))
        dat = add_column(dat, "Y", c("W", "Y"))
        dat = add_column(dat, "Y", c("F", "Y"))
        dat = add_column(dat, "Y", c("W", "F", "Y"))
        dat = keep_column(dat, c("W", "F", "Y"))
        return(dat)
    } else if(type == "BAA"){
        dat = add_column(dat, "f", "total")
        dat = add_column(dat, "y", "total")
        dat = add_column(dat, "f", "y")
        dat = add_column(dat, "f", c("f", "y"))
        dat = add_column(dat, "y", "f")
        dat = add_column(dat, "y", c("f", "y"))
        dat = keep_column(dat, c("f", "y"))
        return(dat)
    }
    return(NULL)
}
