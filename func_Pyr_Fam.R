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



func_Pyr_Fam <- function(dat = NULL, type = NULL){
    if(is.null(dat) | is.null(type)){
        return(NULL)
    }

    if(type == "FAA"){
        if(all(c("L", "Total") %in% colnames(dat))){ dat$L_vs_Total = dat$L/dat$Total }
        if(all(c("A", "Total") %in% colnames(dat))){ dat$A_vs_Total = dat$A/dat$Total }
        if(all(c("V", "Total") %in% colnames(dat))){ dat$V_vs_Total = dat$V/dat$Total }
        if(all(c("L", "A") %in% colnames(dat))){ dat$L_vs_A = dat$L/dat$A }
        if(all(c("L", "V") %in% colnames(dat))){ dat$L_vs_V = dat$L/dat$V }
        if(all(c("L", "A") %in% colnames(dat))){ dat$L_vs_LA = dat$L/(dat$L+dat$A) }
        if(all(c("L", "V") %in% colnames(dat))){ dat$L_vs_LV = dat$L/(dat$L+dat$V) }
        if(all(c("L", "A", "V") %in% colnames(dat))){ dat$L_vs_AV = dat$L/(dat$A+dat$V) }
        if(all(c("L", "A", "V") %in% colnames(dat))){ dat$L_vs_LAV = dat$L/(dat$L+dat$A+dat$V) }
        if(all(c("A", "L") %in% colnames(dat))){ dat$A_vs_L = dat$A/dat$L }
        if(all(c("A", "V") %in% colnames(dat))){ dat$A_vs_V = dat$A/dat$V }
        if(all(c("A", "L") %in% colnames(dat))){ dat$A_vs_LA = dat$A/(dat$L+dat$A) }
        if(all(c("A", "L", "V") %in% colnames(dat))){ dat$A_vs_LV = dat$A/(dat$L+dat$V) }
        if(all(c("A", "V") %in% colnames(dat))){ dat$A_vs_AV = dat$A/(dat$A+dat$V) }
        if(all(c("A", "L", "V") %in% colnames(dat))){ dat$A_vs_LAV = dat$A/(dat$L+dat$A+dat$V) }
        if(all(c("V", "L") %in% colnames(dat))){ dat$V_vs_L = dat$V/dat$L }
        if(all(c("V", "A") %in% colnames(dat))){ dat$V_vs_A = dat$V/dat$A }
        if(all(c("V", "L", "A") %in% colnames(dat))){ dat$V_vs_LA = dat$V/(dat$L+dat$A) }
        if(all(c("V", "L") %in% colnames(dat))){ dat$V_vs_LV = dat$V/(dat$L+dat$V) }
        if(all(c("V", "A") %in% colnames(dat))){ dat$V_vs_AV = dat$V/(dat$A+dat$V) }
        if(all(c("V", "L", "A") %in% colnames(dat))){ dat$V_vs_LAV = dat$V/(dat$L+dat$A+dat$V) }
        dat = keep_column(dat, c("L", "A", "V"))
        return(dat)
    } else if(type == "BAA"){
        if(all(c("l", "total") %in% colnames(dat))){ dat$l_vs_total = dat$l/dat$total }
        if(all(c("a", "total") %in% colnames(dat))){ dat$a_vs_total = dat$a/dat$total }
        if(all(c("v", "total") %in% colnames(dat))){ dat$v_vs_total = dat$v/dat$total }
        if(all(c("l", "a") %in% colnames(dat))){ dat$l_vs_a = dat$l/dat$a }
        if(all(c("l", "v") %in% colnames(dat))){ dat$l_vs_v = dat$l/dat$v }
        if(all(c("l", "a") %in% colnames(dat))){ dat$l_vs_la = dat$l/(dat$l+dat$a) }
        if(all(c("l", "v") %in% colnames(dat))){ dat$l_vs_lv = dat$l/(dat$l+dat$v) }
        if(all(c("l", "a", "v") %in% colnames(dat))){ dat$l_vs_av = dat$l/(dat$a+dat$v) }
        if(all(c("l", "a", "v") %in% colnames(dat))){ dat$l_vs_lav = dat$l/(dat$l+dat$a+dat$v) }
        if(all(c("a", "l") %in% colnames(dat))){ dat$a_vs_l = dat$a/dat$l }
        if(all(c("a", "v") %in% colnames(dat))){ dat$a_vs_v = dat$a/dat$v }
        if(all(c("a", "l") %in% colnames(dat))){ dat$a_vs_la = dat$a/(dat$l+dat$a) }
        if(all(c("a", "l", "v") %in% colnames(dat))){ dat$a_vs_lv = dat$a/(dat$l+dat$v) }
        if(all(c("a", "v") %in% colnames(dat))){ dat$a_vs_av = dat$a/(dat$a+dat$v) }
        if(all(c("a", "l", "v") %in% colnames(dat))){ dat$a_vs_lav = dat$a/(dat$l+dat$a+dat$v) }
        if(all(c("v", "l") %in% colnames(dat))){ dat$v_vs_l = dat$v/dat$l }
        if(all(c("v", "a") %in% colnames(dat))){ dat$v_vs_a = dat$v/dat$a }
        if(all(c("v", "l", "a") %in% colnames(dat))){ dat$v_vs_la = dat$v/(dat$l+dat$a) }
        if(all(c("v", "l") %in% colnames(dat))){ dat$v_vs_lv = dat$v/(dat$l+dat$v) }
        if(all(c("v", "a") %in% colnames(dat))){ dat$v_vs_av = dat$v/(dat$a+dat$v) }
        if(all(c("v", "l", "a") %in% colnames(dat))){ dat$v_vs_lav = dat$v/(dat$l+dat$a+dat$v) }
        dat = keep_column(dat, c("l", "a", "v")) %>% select(-"total") %>% as.data.frame(stringsAsFactors = FALSE)
        return(dat)
    }
    return(NULL)
}
