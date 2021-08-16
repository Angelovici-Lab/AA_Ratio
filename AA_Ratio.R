# Clear all
rm(list = ls())

library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(purrr)
library(lubridate)
library(ggplot2)

library(foreach)
library(doParallel)

library(argparse)


source("func_absolute_vs_total.R")
source("func_BCAA_Fam.R")
source("func_Glu_Fam.R")
source("func_Pyr_Fam.R")
source("func_Asp_Fam.R")
source("func_Shikimate_Fam.R")
source("func_Ser_Fam.R")


##########################################################################################################
# Parsing inputs
##########################################################################################################
# create parser object
parser = ArgumentParser()

parser$add_argument("-c", "--cores", help = "Computing cores", type="integer", default=1)
parser$add_argument("-i", "--input-csv", help = "Input file in CSV format", required = TRUE)
parser$add_argument("-t", "--type", help = "CSV type (FAA/BAA)", required = TRUE)
parser$add_argument("-o", "--output", help = "Output folder path", required = TRUE)
parser$add_argument("-n", "--output-filename", help = "Output filename", required = TRUE)

args = parser$parse_args()


##########################################################################################################
# input assignments
##########################################################################################################
cores = args$cores
input_csv = args$input_csv
type = args$type
output = args$output
output_filename = args$output_filename


##########################################################################################################
# Create output folder
##########################################################################################################
if(!dir.exists(output)){
    dir.create(output, recursive = TRUE)
    if(dir.exists(output)){
        cat(rep("\n", 1), "Output folder ", output, "created!!!", rep("\n", 2))
    } else{
        cat(rep("\n", 1), "Output folder cannot be created!!!", rep("\n", 2))
        quit(status = 1)
    }
}


##########################################################################################################
# read csv
##########################################################################################################
if(file.exists(input_csv)){
    dat = read.csv(
    file = file.path(as.character(input_csv)),
    header = TRUE,
    check.names = TRUE,
    stringsAsFactors = FALSE
    )
} else{
    cat(rep("\n", 1))
    cat("Input file does not exists!!!")
    quit(status = 1)
}


#####################################################################################################################
# AA table and colnames transform
#####################################################################################################################
aa_dat <- data.frame(
    V1=c(
        "Ala", "Arg", "Asn", "Asp", "Cys", "Gln", "Glu", "Gly", "His", "Ile", "Leu", "Lys", "Met",
        "Phe", "Pro", "Ser", "Thr", "Trp", "Tyr", "Val", "Asx", "Glx"
    ),
    V2=c(
        "A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K", "M", "F", "P", "S",
        "T", "W", "Y", "V", "B", "Z"
    ),
    V3=c(
        "a", "r", "n", "d", "c", "q", "e", "g", "h", "i", "l", "k", "m", "f", "p", "s",
        "t", "w", "y", "v", "b", "z"
    ),
    V4=c(
        "Alanine", "Arginine", "Asparagine", "Aspartic acid", "Cysteine", "Glutamine",
        "Glutamic acid", "Glycine", "Histidine", "Isoleucine", "Leucine", "Lysine",
        "Methionine", "Phenylalanine", "Proline", "Serine",
        "Threonine", "Tryptophan", "Tyrosine", "Valine", "Aspartic acid or Asparagine",
        "Glutamic acid or Glutamine"
    ),
    stringsAsFactors = FALSE
)

faa_dat = dat
baa_dat = dat

index = match(tolower(colnames(dat)), tolower(aa_dat[,1]))
colnames(faa_dat)[!is.na(index)] = aa_dat[index,2][!is.na(index)]
colnames(baa_dat)[!is.na(index)] = aa_dat[index,3][!is.na(index)]


#####################################################################################################################
# Calculate Total
#####################################################################################################################
dat = dat %>% mutate(Total = rowSums(.[which(!is.na(index))], na.rm=TRUE)) %>% as.data.frame(stringsAsFactors = FALSE)
faa_dat = faa_dat %>% mutate(Total = rowSums(.[which(!is.na(index))], na.rm=TRUE)) %>% as.data.frame(stringsAsFactors = FALSE)
baa_dat = baa_dat %>% mutate(total = rowSums(.[which(!is.na(index))], na.rm=TRUE)) %>% as.data.frame(stringsAsFactors = FALSE)


#####################################################################################################################
# Absolute vs Total calculations
#####################################################################################################################
if(type == "FAA"){
    absolute_vs_total_dat = func_absolute_vs_total(dat = faa_dat, type = type)
    if(!is.null(absolute_vs_total_dat)){
        index = match(colnames(absolute_vs_total_dat), aa_dat[,2])
        colnames(absolute_vs_total_dat)[!is.na(index)] = aa_dat[index,1][!is.na(index)]
    } else{
        cat(rep("\n", 2), "absolute_vs_total_dat is NULL!!!")
        quit(status = 1)
    }
} else if(type == "BAA"){
    absolute_vs_total_dat = func_absolute_vs_total(dat = baa_dat, type = type)
    if(!is.null(absolute_vs_total_dat)){
        index = match(colnames(absolute_vs_total_dat), aa_dat[,3])
        colnames(absolute_vs_total_dat)[!is.na(index)] = aa_dat[index,1][!is.na(index)]
    } else{
        cat(rep("\n", 2), "absolute_vs_total_dat is NULL!!!")
        quit(status = 1)
    }
} else{
    absolute_vs_total_dat = NULL
}

# print(head(absolute_vs_total_dat))
# print(dim(absolute_vs_total_dat))


#####################################################################################################################
# BCAA Family calculations
#####################################################################################################################
if(type == "FAA"){
    bcaa_fam_dat = func_BCAA_Fam(dat = faa_dat, type = type)
    if(!is.null(bcaa_fam_dat)){
        index = match(colnames(bcaa_fam_dat), aa_dat[,2])
        colnames(bcaa_fam_dat)[!is.na(index)] = aa_dat[index,1][!is.na(index)]
    } else{
        cat(rep("\n", 2), "bcaa_fam_dat is NULL!!!")
        quit(status = 1)
    }
} else if(type == "BAA"){
    bcaa_fam_dat = func_BCAA_Fam(dat = baa_dat, type = type)
    if(!is.null(bcaa_fam_dat)){
        index = match(colnames(bcaa_fam_dat), aa_dat[,3])
        colnames(bcaa_fam_dat)[!is.na(index)] = aa_dat[index,1][!is.na(index)]
    } else{
        cat(rep("\n", 2), "bcaa_fam_dat is NULL!!!")
        quit(status = 1)
    }
} else{
    bcaa_fam_dat = NULL
}

# print(head(bcaa_fam_dat))
# print(dim(bcaa_fam_dat))


#####################################################################################################################
# Glu Family calculations
#####################################################################################################################
if(type == "FAA"){
    glu_fam_dat = func_Glu_Fam(dat = faa_dat, type = type)
    if(!is.null(glu_fam_dat)){
        index = match(colnames(glu_fam_dat), aa_dat[,2])
        colnames(glu_fam_dat)[!is.na(index)] = aa_dat[index,1][!is.na(index)]
    } else{
        cat(rep("\n", 2), "glu_fam_dat is NULL!!!")
        quit(status = 1)
    }
} else if(type == "BAA"){
    glu_fam_dat = func_Glu_Fam(dat = baa_dat, type = type)
    if(!is.null(glu_fam_dat)){
        index = match(colnames(glu_fam_dat), aa_dat[,3])
        colnames(glu_fam_dat)[!is.na(index)] = aa_dat[index,1][!is.na(index)]
    } else{
        cat(rep("\n", 2), "glu_fam_dat is NULL!!!")
        quit(status = 1)
    }
} else{
    glu_fam_dat = NULL
}

# print(head(glu_fam_dat))
# print(dim(glu_fam_dat))


#####################################################################################################################
# Pyr Family calculations
#####################################################################################################################
if(type == "FAA"){
    pyr_fam_dat = func_Pyr_Fam(dat = faa_dat, type = type)
    if(!is.null(pyr_fam_dat)){
        index = match(colnames(pyr_fam_dat), aa_dat[,2])
        colnames(pyr_fam_dat)[!is.na(index)] = aa_dat[index,1][!is.na(index)]
    } else{
        cat(rep("\n", 2), "pyr_fam_dat is NULL!!!")
        quit(status = 1)
    }
} else if(type == "BAA"){
    pyr_fam_dat = func_Pyr_Fam(dat = baa_dat, type = type)
    if(!is.null(pyr_fam_dat)){
        index = match(colnames(pyr_fam_dat), aa_dat[,3])
        colnames(pyr_fam_dat)[!is.na(index)] = aa_dat[index,1][!is.na(index)]
    } else{
        cat(rep("\n", 2), "pyr_fam_dat is NULL!!!")
        quit(status = 1)
    }
} else{
    pyr_fam_dat = NULL
}

# print(head(pyr_fam_dat))
# print(dim(pyr_fam_dat))


#####################################################################################################################
# Asp Family calculations
#####################################################################################################################
if(type == "FAA"){
    asp_fam_dat = func_Asp_Fam(dat = faa_dat, type = type)
    if(!is.null(asp_fam_dat)){
        index = match(colnames(asp_fam_dat), aa_dat[,2])
        colnames(asp_fam_dat)[!is.na(index)] = aa_dat[index,1][!is.na(index)]
    } else{
        cat(rep("\n", 2), "asp_fam_dat is NULL!!!")
        quit(status = 1)
    }
} else if(type == "BAA"){
    asp_fam_dat = func_Asp_Fam(dat = baa_dat, type = type)
    if(!is.null(asp_fam_dat)){
        index = match(colnames(asp_fam_dat), aa_dat[,3])
        colnames(asp_fam_dat)[!is.na(index)] = aa_dat[index,1][!is.na(index)]
    } else{
        cat(rep("\n", 2), "asp_fam_dat is NULL!!!")
        quit(status = 1)
    }
} else{
    asp_fam_dat = NULL
}

# print(head(asp_fam_dat))
# print(dim(asp_fam_dat))


#####################################################################################################################
# Shikimate Family calculations
#####################################################################################################################
if(type == "FAA"){
    shikimate_fam_dat = func_Shikimate_Fam(dat = faa_dat, type = type)
    if(!is.null(shikimate_fam_dat)){
        index = match(colnames(shikimate_fam_dat), aa_dat[,2])
        colnames(shikimate_fam_dat)[!is.na(index)] = aa_dat[index,1][!is.na(index)]
    } else{
        cat(rep("\n", 2), "shikimate_fam_dat is NULL!!!")
        quit(status = 1)
    }
} else if(type == "BAA"){
    shikimate_fam_dat = func_Shikimate_Fam(dat = baa_dat, type = type)
    if(!is.null(shikimate_fam_dat)){
        index = match(colnames(shikimate_fam_dat), aa_dat[,3])
        colnames(shikimate_fam_dat)[!is.na(index)] = aa_dat[index,1][!is.na(index)]
    } else{
        cat(rep("\n", 2), "shikimate_fam_dat is NULL!!!")
        quit(status = 1)
    }
} else{
    shikimate_fam_dat = NULL
}

# print(head(shikimate_fam_dat))
# print(dim(shikimate_fam_dat))


#####################################################################################################################
# Ser Family calculations
#####################################################################################################################
if(type == "FAA"){
    ser_fam_dat = func_Ser_Fam(dat = faa_dat, type = type)
    if(!is.null(ser_fam_dat)){
        index = match(colnames(ser_fam_dat), aa_dat[,2])
        colnames(ser_fam_dat)[!is.na(index)] = aa_dat[index,1][!is.na(index)]
    } else{
        cat(rep("\n", 2), "ser_fam_dat is NULL!!!")
        quit(status = 1)
    }
} else if(type == "BAA"){
    ser_fam_dat = func_Ser_Fam(dat = baa_dat, type = type)
    if(!is.null(ser_fam_dat)){
        index = match(colnames(ser_fam_dat), aa_dat[,3])
        colnames(ser_fam_dat)[!is.na(index)] = aa_dat[index,1][!is.na(index)]
    } else{
        cat(rep("\n", 2), "ser_fam_dat is NULL!!!")
        quit(status = 1)
    }
} else{
    ser_fam_dat = NULL
}

# print(head(ser_fam_dat))
# print(dim(ser_fam_dat))


#####################################################################################################################
# Remove Inf, -Inf, and NaN
#####################################################################################################################
absolute_vs_total_dat[absolute_vs_total_dat == Inf] = NA
bcaa_fam_dat[bcaa_fam_dat == Inf] = NA
glu_fam_dat[glu_fam_dat == Inf] = NA
pyr_fam_dat[pyr_fam_dat == Inf] = NA
asp_fam_dat[asp_fam_dat == Inf] = NA
shikimate_fam_dat[shikimate_fam_dat == Inf] = NA
ser_fam_dat[ser_fam_dat == Inf] = NA

absolute_vs_total_dat[absolute_vs_total_dat == -Inf] = NA
bcaa_fam_dat[bcaa_fam_dat == -Inf] = NA
glu_fam_dat[glu_fam_dat == -Inf] = NA
pyr_fam_dat[pyr_fam_dat == -Inf] = NA
asp_fam_dat[asp_fam_dat == -Inf] = NA
shikimate_fam_dat[shikimate_fam_dat == -Inf] = NA
ser_fam_dat[ser_fam_dat == -Inf] = NA

absolute_vs_total_dat[absolute_vs_total_dat == NaN] = NA
bcaa_fam_dat[bcaa_fam_dat == NaN] = NA
glu_fam_dat[glu_fam_dat == NaN] = NA
pyr_fam_dat[pyr_fam_dat == NaN] = NA
asp_fam_dat[asp_fam_dat == NaN] = NA
shikimate_fam_dat[shikimate_fam_dat == NaN] = NA
ser_fam_dat[ser_fam_dat == NaN] = NA


#####################################################################################################################
# Save Ratios
#####################################################################################################################
if(!is.null(absolute_vs_total_dat)){
    write.csv(
        x = absolute_vs_total_dat,
        file = file.path(output, paste0(output_filename, "_absolute_vs_total_dat.csv")),
        quote = FALSE,
        row.names = FALSE,
        na = ""
    )
}
if(!is.null(bcaa_fam_dat)){
    write.csv(
        x = bcaa_fam_dat,
        file = file.path(output, paste0(output_filename, "_bcaa_fam_dat.csv")),
        quote = FALSE,
        row.names = FALSE,
        na = ""
    )
}
if(!is.null(glu_fam_dat)){
    write.csv(
        x = glu_fam_dat,
        file = file.path(output, paste0(output_filename, "_glu_fam_dat.csv")),
        quote = FALSE,
        row.names = FALSE,
        na = ""
    )
}
if(!is.null(pyr_fam_dat)){
    write.csv(
        x = pyr_fam_dat,
        file = file.path(output, paste0(output_filename, "_pyr_fam_dat.csv")),
        quote = FALSE,
        row.names = FALSE,
        na = ""
    )
}
if(!is.null(asp_fam_dat)){
    write.csv(
        x = asp_fam_dat,
        file = file.path(output, paste0(output_filename, "_asp_fam_dat.csv")),
        quote = FALSE,
        row.names = FALSE,
        na = ""
    )
}
if(!is.null(shikimate_fam_dat)){
    write.csv(
        x = shikimate_fam_dat,
        file = file.path(output, paste0(output_filename, "_shikimate_fam_dat.csv")),
        quote = FALSE,
        row.names = FALSE,
        na = ""
    )
}
if(!is.null(ser_fam_dat)){
    write.csv(
        x = ser_fam_dat,
        file = file.path(output, paste0(output_filename, "_ser_fam_dat.csv")),
        quote = FALSE,
        row.names = FALSE,
        na = ""
    )
}
