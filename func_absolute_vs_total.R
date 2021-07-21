func_absolute_vs_total <- function(dat = NULL, type = NULL){
    if(is.null(dat) | is.null(type)){
        return(NULL)
    }

    if(type == "FAA"){
        if(("A" %in% colnames(dat)) & ("Total" %in% colnames(dat))){ dat$A_vs_Total = dat$A/dat$Total }
        if(("R" %in% colnames(dat)) & ("Total" %in% colnames(dat))){ dat$R_vs_Total = dat$R/dat$Total }
        if(("D" %in% colnames(dat)) & ("Total" %in% colnames(dat))){ dat$D_vs_Total = dat$D/dat$Total }
        if(("Q" %in% colnames(dat)) & ("Total" %in% colnames(dat))){ dat$Q_vs_Total = dat$Q/dat$Total }
        if(("E" %in% colnames(dat)) & ("Total" %in% colnames(dat))){ dat$E_vs_Total = dat$E/dat$Total }
        if(("G" %in% colnames(dat)) & ("Total" %in% colnames(dat))){ dat$G_vs_Total = dat$G/dat$Total }
        if(("H" %in% colnames(dat)) & ("Total" %in% colnames(dat))){ dat$H_vs_Total = dat$H/dat$Total }
        if(("I" %in% colnames(dat)) & ("Total" %in% colnames(dat))){ dat$I_vs_Total = dat$I/dat$Total }
        if(("L" %in% colnames(dat)) & ("Total" %in% colnames(dat))){ dat$L_vs_Total = dat$L/dat$Total }
        if(("K" %in% colnames(dat)) & ("Total" %in% colnames(dat))){ dat$K_vs_Total = dat$K/dat$Total }
        if(("M" %in% colnames(dat)) & ("Total" %in% colnames(dat))){ dat$M_vs_Total = dat$M/dat$Total }
        if(("F" %in% colnames(dat)) & ("Total" %in% colnames(dat))){ dat$F_vs_Total = dat$F/dat$Total }
        if(("P" %in% colnames(dat)) & ("Total" %in% colnames(dat))){ dat$P_vs_Total = dat$P/dat$Total }
        if(("S" %in% colnames(dat)) & ("Total" %in% colnames(dat))){ dat$S_vs_Total = dat$S/dat$Total }
        if(("T" %in% colnames(dat)) & ("Total" %in% colnames(dat))){ dat$T_vs_Total = dat$T/dat$Total }
        if(("W" %in% colnames(dat)) & ("Total" %in% colnames(dat))){ dat$W_vs_Total = dat$W/dat$Total }
        if(("Y" %in% colnames(dat)) & ("Total" %in% colnames(dat))){ dat$Y_vs_Total = dat$Y/dat$Total }
        if(("V" %in% colnames(dat)) & ("Total" %in% colnames(dat))){ dat$V_vs_Total = dat$V/dat$Total }
        return(dat)
    } else if(type == "BAA"){
        if(("a" %in% colnames(dat)) & ("total" %in% colnames(dat))){ dat$a_vs_total = dat$a/dat$total }
        if(("f" %in% colnames(dat)) & ("total" %in% colnames(dat))){ dat$f_vs_total = dat$f/dat$total }
        if(("g" %in% colnames(dat)) & ("total" %in% colnames(dat))){ dat$g_vs_total = dat$g/dat$total }
        if(("h" %in% colnames(dat)) & ("total" %in% colnames(dat))){ dat$h_vs_total = dat$h/dat$total }
        if(("i" %in% colnames(dat)) & ("total" %in% colnames(dat))){ dat$i_vs_total = dat$i/dat$total }
        if(("k" %in% colnames(dat)) & ("total" %in% colnames(dat))){ dat$k_vs_total = dat$k/dat$total }
        if(("l" %in% colnames(dat)) & ("total" %in% colnames(dat))){ dat$l_vs_total = dat$l/dat$total }
        if(("m" %in% colnames(dat)) & ("total" %in% colnames(dat))){ dat$m_vs_total = dat$m/dat$total }
        if(("p" %in% colnames(dat)) & ("total" %in% colnames(dat))){ dat$p_vs_total = dat$p/dat$total }
        if(("r" %in% colnames(dat)) & ("total" %in% colnames(dat))){ dat$r_vs_total = dat$r/dat$total }
        if(("s" %in% colnames(dat)) & ("total" %in% colnames(dat))){ dat$s_vs_total = dat$s/dat$total }
        if(("t" %in% colnames(dat)) & ("total" %in% colnames(dat))){ dat$t_vs_total = dat$t/dat$total }
        if(("v" %in% colnames(dat)) & ("total" %in% colnames(dat))){ dat$v_vs_total = dat$v/dat$total }
        if(("x" %in% colnames(dat)) & ("total" %in% colnames(dat))){ dat$x_vs_total = dat$x/dat$total }
        if(("y" %in% colnames(dat)) & ("total" %in% colnames(dat))){ dat$y_vs_total = dat$y/dat$total }
        if(("z" %in% colnames(dat)) & ("total" %in% colnames(dat))){ dat$z_vs_total = dat$z/dat$total }
        return(dat)
    }
    return(NULL)
}