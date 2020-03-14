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



func_Glu_Fam <- function(dat = NULL, type = NULL){
    if(is.null(dat) | is.null(type)){
        return(NULL)
    }

    if(type == "FAA"){
        dat = add_column(dat, "E", "Total")
        dat = add_column(dat, "P", "Total")
        dat = add_column(dat, "R", "Total")
        dat = add_column(dat, "Q", "Total")
        dat = add_column(dat, "E", "P")
        dat = add_column(dat, "E", "R")
        dat = add_column(dat, "E", "Q")
        dat = add_column(dat, "E", c("E", "P"))
        dat = add_column(dat, "E", c("E", "R"))
        dat = add_column(dat, "E", c("E", "Q"))
        dat = add_column(dat, "E", c("P", "R"))
        dat = add_column(dat, "E", c("P", "Q"))
        dat = add_column(dat, "E", c("R", "Q"))
        dat = add_column(dat, "E", c("E", "P", "R"))
        dat = add_column(dat, "E", c("E", "P", "Q"))
        dat = add_column(dat, "E", c("E", "R", "Q"))
        dat = add_column(dat, "E", c("P", "R", "Q"))
        dat = add_column(dat, "E", c("E", "P", "R", "Q"))
        dat = add_column(dat, "P", "E")
        dat = add_column(dat, "P", "R")
        dat = add_column(dat, "P", "Q")
        dat = add_column(dat, "P", c("E", "P"))
        dat = add_column(dat, "P", c("E", "R"))
        dat = add_column(dat, "P", c("E", "Q"))
        dat = add_column(dat, "P", c("P", "R"))
        dat = add_column(dat, "P", c("P", "Q"))
        dat = add_column(dat, "P", c("R", "Q"))
        dat = add_column(dat, "P", c("E", "P", "R"))
        dat = add_column(dat, "P", c("E", "P", "Q"))
        dat = add_column(dat, "P", c("E", "R", "Q"))
        dat = add_column(dat, "P", c("P", "R", "Q"))
        dat = add_column(dat, "P", c("E", "P", "R", "Q"))
        dat = add_column(dat, "R", "E")
        dat = add_column(dat, "R", "P")
        dat = add_column(dat, "R", "Q")
        dat = add_column(dat, "R", c("E", "P"))
        dat = add_column(dat, "R", c("E", "R"))
        dat = add_column(dat, "R", c("E", "Q"))
        dat = add_column(dat, "R", c("P", "R"))
        dat = add_column(dat, "R", c("P", "Q"))
        dat = add_column(dat, "R", c("R", "Q"))
        dat = add_column(dat, "R", c("E", "P", "R"))
        dat = add_column(dat, "R", c("E", "P", "Q"))
        dat = add_column(dat, "R", c("E", "R", "Q"))
        dat = add_column(dat, "R", c("P", "R", "Q"))
        dat = add_column(dat, "R", c("E", "P", "R", "Q"))
        dat = add_column(dat, "Q", "E")
        dat = add_column(dat, "Q", "P")
        dat = add_column(dat, "Q", "R")
        dat = add_column(dat, "Q", c("E", "P"))
        dat = add_column(dat, "Q", c("E", "R"))
        dat = add_column(dat, "Q", c("E", "Q"))
        dat = add_column(dat, "Q", c("P", "R"))
        dat = add_column(dat, "Q", c("P", "Q"))
        dat = add_column(dat, "Q", c("R", "Q"))
        dat = add_column(dat, "Q", c("E", "P", "R"))
        dat = add_column(dat, "Q", c("E", "P", "Q"))
        dat = add_column(dat, "Q", c("E", "R", "Q"))
        dat = add_column(dat, "Q", c("P", "R", "Q"))
        dat = add_column(dat, "Q", c("E", "P", "R", "Q"))
        dat = keep_column(dat, c("E", "P", "R", "Q"))
        return(dat)
    } else if(type == "BAA"){
        dat = add_column(dat, "p", "total")
        dat = add_column(dat, "r", "total")
        dat = add_column(dat, "z", "total")
        dat = add_column(dat, "p", "r")
        dat = add_column(dat, "p", "z")
        dat = add_column(dat, "p", c("p", "r"))
        dat = add_column(dat, "p", c("p", "z"))
        dat = add_column(dat, "p", c("r", "z"))
        dat = add_column(dat, "p", c("p", "r", "z"))
        dat = add_column(dat, "r", "p")
        dat = add_column(dat, "r", "z")
        dat = add_column(dat, "r", c("p", "r"))
        dat = add_column(dat, "r", c("p", "z"))
        dat = add_column(dat, "r", c("r", "z"))
        dat = add_column(dat, "r", c("p", "r", "z"))
        dat = add_column(dat, "z", "p")
        dat = add_column(dat, "z", "r")
        dat = add_column(dat, "z", c("p", "r"))
        dat = add_column(dat, "z", c("p", "z"))
        dat = add_column(dat, "z", c("r", "z"))
        dat = add_column(dat, "z", c("p", "r", "z"))
        dat = keep_column(dat, c("p", "r", "z"))
        return(dat)
    }
    return(NULL)
}
