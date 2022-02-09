### Obs_to_map.R
### This script is used to convert model annotations to a GIS-readable format with georeferencing.


### Assign species to their corresponding class ID in the model (starting from 1, so Desmophyllum with id 0 gets added as no 1, and so on.)
Species <- NULL
Species[1] <- "Desmophyllum"
Species[2] <- "Bolocera"
Species[3] <- "Geodia"
Species[4] <- "Fish"

### Set working directory if not working within an R project (parent directory of the directory of the current script)
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

### Load functions
source("scripts/functions.R")

### "openxlsx" is used to handle the xlsx files because it can both read and write to xlsx files
### install.packages("openxlsx")
library("openxlsx")


### Function to annotate cells in a grid overlay with presence or absence of species
To_GIS <- annotate.grid.cells(
             ### File name including path
                ### Sheet name
                   ### Column name
             xlsx_filename = "xlsx_spreadsheets/Example_grid_times.xlsx",
                Grid_times_sheet_name = "Grid times",
                   PageName = "PageName",
                   PageNumber = "PageNumber",
                   Unixtime_in = "FMEAS",
                   Unixtime_out = "TMEAS",
                Film_info_sheet_name = "Film info",
                   Film = "Film",
                   Unixtime_start = "Unixtime start",
                   Unixtime_end = "Unixtime end",
                   fps = "fps",
             ### Path to the CSV output files from the model
             CSV_path = "model_output",
             ### The CSV files should be named with the respective film file name + an optional suffix
             Suffix = "_(ct0.05_ot0.3_v4best).csv",
             ### Should the annotations be in the form of highest confidence value (TRUE) or just presence/absence (FALSE)?
             Annotate_with_cv = TRUE,
             ### Should the annotations be in the form of the model's precision rate for every grid cell's highest cv?
             ### (If TRUE, overrides Annotate_with_cv setting. NB! Takes really long time)
             Annotate_with_PPV = FALSE,
             ### Only return the rows that get annotated (TRUE), or all rows (FALSE)?
             Drop_NA_rows = TRUE,
             ### Set the confidence thresholds to run the script for (all observations with a lower confidence are excluded)
             ### (If Annotate_with_cv is set to TRUE, only the lowest value will be used)
             ct = c(0.05),
             ### Set the minimum total time in seconds that a species needs to be observed within any grid cell to count there
             min_time_within_cell = c(0, 0.1, 0.2)
          )

### Function to save a data frame or list of data frames to a new xlsx workbook or CSV file
### (For lists: creates one CSV file, or one xlsx worksheet, per list object)
save.to.file(
       input = To_GIS,
       output_file = paste0("script_output/Annotated_", filename, ".xlsx")
    )

