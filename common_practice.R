library(openxlsx)
library(tidyverse)

areas <- read.xlsx("assessment_area_data_file.xlsx", fillMergedCells = TRUE)

forest_type <- read_csv("plot_loc.csv")
