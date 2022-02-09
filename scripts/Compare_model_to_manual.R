### Compare_model_to_manual.R
### This script is used to test the performance of a trained model by comparing manual annotations to model annotations on a frame-by-frame basis.


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


### Function to calculate confusion matrix statistics for different confidence thresholds and films
compare.film.by.film(
  ### File name including path
     ### Sheet names
        ### Column name
  Manual_workbook = "xlsx_spreadsheets/Manual_annotations.xlsx",
     Sheets = c("All filmtypes"),
        Class = "Species",
        Frame_in = "Frame in",
        Frame_out = "Frame out",
        Confidence = "Confidence",
        fps = "fps",
        Filename = "Filename",
        Total_frames = "Total frames",
  ### Path to the CSV output files from the model
  CSV_path = "model_output",
  ### The CSV files should be named with the respective film file name + an optional suffix
  Suffix = "_(ct0.05_ot0.3_v4best).csv",
  ### Import the global Species variable to a local one
  Species = get("Species", envir = .GlobalEnv),
  ### Set the name of the variable to assign to. Has to be given here as a parameter to allow for appending to an already existing such variable.
  Assign_to = "Results_by_film",
  ### Should only unique rows be returned, with the same set of thresholds, film and results?
  Drop_duplicates = TRUE,
  ### Should NaN values resulting from division by zero be replaced with zeros? (Precision, Sensitivity, FPR and Perf)
  Set_undefs_to_zero = FALSE,
  ### Seconds before and after each manual annotation that are not to be evaluated (NE)
  margin_secs = c(0.3),
  ### Set the confidence thresholds to run the script for (all observations with a lower confidence are excluded)
  ct = seq(0.05, 0.95, by = 0.05),
  ### Accordingly, set the confidence thresholds for the manual annotations
  man_ct = c(0.5, 1)
)

### Function to summarize the result statistics for all films.
### If there are more than one row with identical sets of thresholds for a certain film, takes the mean values for those.
Results_summarized <- summarize.by.species(input_list = Results_by_film, Set_undefs_to_zero = FALSE)

### Function to save a data frame or list of data frames to a new xlsx workbook or CSV file
### (For lists: creates one CSV file, or one xlsx worksheet, per list object)
save.to.file(
  input = Results_summarized,
  output_file = "script_output/Example_model_test_results.xlsx"
)

