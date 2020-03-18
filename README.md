# AA_Ratio
A script written in R to create amino acid ratios.

## Upgrade Packages
* update.packages(ask=FALSE)

## Install Packages (if these packages are not in your machine)
* install.packages("dplyr", dependencies = TRUE)
* install.packages("dplyr", dependencies = TRUE)
* install.packages("tidyr", dependencies = TRUE)
* install.packages("tibble", dependencies = TRUE)
* install.packages("stringr", dependencies = TRUE)
* install.packages("purrr", dependencies = TRUE)
* install.packages("lubridate", dependencies = TRUE)
* install.packages("ggplot2", dependencies = TRUE)
* install.packages("foreach", dependencies = TRUE)
* install.packages("doParallel", dependencies = TRUE)
* install.packages("argparse", dependencies = TRUE)

## Usage
1. Clone the repository: git clone https://github.com/Angelovici-Lab/AA_Ratio.git
2. cd AA_Ratio
3. Rscript AA_Ratio.R [COMMANDS]
    - COMMANDS:
        - -i CSV_input_file
        - -t FAA/BAA
        - -o Output_directory
        - -n Output_filename (No file extension)

## Example
* Rscript AA_Ratio.R -i 07_03_2019_Arabidopsis_1001_BAA.csv -t BAA -o ./Ratio_data -n 03_13_2020_Arabidopsis_1001_BAA
* Rscript AA_Ratio.R -i /home/uname/data/07_03_2019_Arabidopsis_1001_FAA.csv -t FAA -o /home/uname/data/Ratio_data -n 03_13_2020_Arabidopsis_1001_FAA

## Notes
* Please refer to 07_03_2019_Arabidopsis_1001_BAA.csv or 07_03_2019_Arabidopsis_1001_FAA.csv when you create your input data.
