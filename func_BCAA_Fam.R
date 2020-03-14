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



func_BCAA_Fam <- function(dat = NULL, type = NULL){
    if(is.null(dat) | is.null(type)){
        return(NULL)
    }

    if(type == "FAA"){
        if(all(c("I", "Total") %in% colnames(dat))){ dat$I_vs_Total = dat$I/dat$Total }
        if(all(c("V", "Total") %in% colnames(dat))){ dat$V_vs_Total = dat$V/dat$Total }
        if(all(c("L", "Total") %in% colnames(dat))){ dat$L_vs_Total = dat$L/dat$Total }
        if(all(c("I", "V") %in% colnames(dat))){ dat$I_vs_V = dat$I/dat$V }
        if(all(c("I", "L") %in% colnames(dat))){ dat$I_vs_L = dat$I/dat$L }
        if(all(c("I", "V") %in% colnames(dat))){ dat$I_vs_IV = dat$I/(dat$I+dat$V) }
        if(all(c("I", "L") %in% colnames(dat))){ dat$I_vs_IL = dat$I/(dat$I+dat$L) }
        if(all(c("I", "V", "L") %in% colnames(dat))){ dat$I_vs_VL = dat$I/(dat$V+dat$L) }
        if(all(c("I", "V", "L") %in% colnames(dat))){ dat$I_vs_IVL = dat$I/(dat$I+dat$V+dat$L) }
        if(all(c("V", "I") %in% colnames(dat))){ dat$V_vs_I = dat$V/dat$I }
        if(all(c("V", "L") %in% colnames(dat))){ dat$V_vs_L = dat$V/dat$L }
        if(all(c("V", "I") %in% colnames(dat))){ dat$V_vs_IV = dat$V/(dat$I+dat$V) }
        if(all(c("V", "I", "L") %in% colnames(dat))){ dat$V_vs_IL = dat$V/(dat$I+dat$L) }
        if(all(c("V", "L") %in% colnames(dat))){ dat$V_vs_VL = dat$V/(dat$V+dat$L) }
        if(all(c("V", "I", "L") %in% colnames(dat))){ dat$V_vs_IVL = dat$V/(dat$I+dat$V+dat$L) }
        if(all(c("L", "I") %in% colnames(dat))){ dat$L_vs_I = dat$L/dat$I }
        if(all(c("L", "V") %in% colnames(dat))){ dat$L_vs_V = dat$L/dat$V }
        if(all(c("L", "I", "V") %in% colnames(dat))){ dat$L_vs_IV = dat$L/(dat$I+dat$V) }
        if(all(c("L", "I") %in% colnames(dat))){ dat$L_vs_IL = dat$L/(dat$I+dat$L) }
        if(all(c("V", "L") %in% colnames(dat))){ dat$L_vs_VL = dat$L/(dat$V+dat$L) }
        if(all(c("V", "I", "L") %in% colnames(dat))){ dat$L_vs_IVL = dat$L/(dat$I+dat$V+dat$L) }
        dat = keep_column(dat, c("V", "I", "L", "Total"))
        return(dat)
    } else if(type == "BAA"){
        if(all(c("i", "total") %in% colnames(dat))){ dat$i_vs_total = dat$i/dat$total }
        if(all(c("v", "total") %in% colnames(dat))){ dat$v_vs_total = dat$v/dat$total }
        if(all(c("l", "total") %in% colnames(dat))){ dat$l_vs_total = dat$l/dat$total }
        if(all(c("i", "v") %in% colnames(dat))){ dat$i_vs_v = dat$i/dat$v }
        if(all(c("i", "l") %in% colnames(dat))){ dat$i_vs_l = dat$i/dat$l }
        if(all(c("i", "v") %in% colnames(dat))){ dat$i_vs_iv = dat$i/(dat$i+dat$v) }
        if(all(c("i", "l") %in% colnames(dat))){ dat$i_vs_il = dat$i/(dat$i+dat$l) }
        if(all(c("i", "v", "l") %in% colnames(dat))){ dat$i_vs_vl = dat$i/(dat$v+dat$l) }
        if(all(c("i", "v", "l") %in% colnames(dat))){ dat$i_vs_ivl = dat$i/(dat$i+dat$v+dat$l) }
        if(all(c("v", "i") %in% colnames(dat))){ dat$v_vs_i = dat$v/dat$i }
        if(all(c("v", "l") %in% colnames(dat))){ dat$v_vs_l = dat$v/dat$l }
        if(all(c("v", "i") %in% colnames(dat))){ dat$v_vs_iv = dat$v/(dat$i+dat$v) }
        if(all(c("v", "i", "l") %in% colnames(dat))){ dat$v_vs_il = dat$v/(dat$i+dat$l) }
        if(all(c("v", "l") %in% colnames(dat))){ dat$v_vs_vl = dat$v/(dat$v+dat$l) }
        if(all(c("v", "i", "l") %in% colnames(dat))){ dat$v_vs_ivl = dat$v/(dat$i+dat$v+dat$l) }
        if(all(c("l", "i") %in% colnames(dat))){ dat$l_vs_i = dat$l/dat$i }
        if(all(c("l", "v") %in% colnames(dat))){ dat$l_vs_v = dat$l/dat$v }
        if(all(c("l", "i", "v") %in% colnames(dat))){ dat$l_vs_iv = dat$l/(dat$i+dat$v) }
        if(all(c("l", "i") %in% colnames(dat))){ dat$l_vs_il = dat$l/(dat$i+dat$l) }
        if(all(c("v", "l") %in% colnames(dat))){ dat$l_vs_vl = dat$l/(dat$v+dat$l) }
        if(all(c("v", "i", "l") %in% colnames(dat))){ dat$l_vs_ivl = dat$l/(dat$i+dat$v+dat$l) }
        dat = keep_column(dat, c("i", "v", "l", "total"))
        return(dat)
    }
    return(NULL)
}
