### functions.R
### This file contains the functions used by the other scripts.



### Function to calculate confusion matrix statistics for different confidence thresholds and films
                                 ### Default values
compare.film.by.film <- function(Manual_workbook = "xlsx_spreadsheets/Manual_annotations.xlsx",
                                    Sheets = c("All filmtypes"),
                                       Class = "Species",
                                       Frame_in = "Frame in",
                                       Frame_out = "Frame out",
                                       Confidence = "Confidence",
                                       fps = "fps",
                                       Filename = "Filename",
                                       Total_frames = "Total frames",
                                 CSV_path = "model_output",
                                 Suffix = ".csv",
                                 Species = get("Species", envir = .GlobalEnv),
                                 Assign_to = "Results_by_film",
                                 Drop_duplicates = TRUE,
                                 Set_undefs_to_zero = FALSE,
                                 margin_secs = c(0),
                                 ct = seq(0.05, 0.95, by = 0.05),
                                 man_ct = c(0.5, 1)
                                 ) {
  
  cat("Processing...\n")
  
  ### Create variables
  missing_CSVs <- vector()
  if (exists(Assign_to, where = parent.frame())) {
    assign("Results", get(Assign_to, envir = parent.frame()))
  } else {
    Results <- list()
  }
  
  ### Iterate all given worksheets in the workbook of manual annotations
  for (current_sheet in Sheets) {
    
    ### Read the current sheet of manual annotations and divide them by filename
    Manual_obs <- read.xlsx(Manual_workbook, sheet = current_sheet, sep.names = " ")
    Manual_obs <- split(Manual_obs, Manual_obs[Filename])
    
    ### Iterate all films given in the current worksheet manual workbook
    for (current_film in names(Manual_obs)) {
      
      ### Import the corresponding model output file
      if (file.exists(paste0(CSV_path, "/", current_film, Suffix))) {
        Model_obs <- read.csv(paste0(CSV_path, "/", current_film, Suffix))
      }
      else {
        missing_CSVs <- c(missing_CSVs, paste0(current_film, Suffix))
        next
      }
      
      ### Read the total number of frames in the film
      tot_frames <- Manual_obs[[current_film]][1, Total_frames]
      
      ### Divide the observations by species
      Manual_obs[[current_film]] <- split(Manual_obs[[current_film]], Manual_obs[[current_film]][[Class]])
      Model_obs <- split(Model_obs, Model_obs["class_id"])
      
      ### Iterate all species to be analysed (those that haven't been left out in the beginning)
      for (current_sp in Species[!is.na(Species)]) {
        
        ### Create a data frame to save the results of the current species to, or if one is already present, append to it
        if (!(current_sp %in% names(Results))) {
          Results[[current_sp]] <- data.frame(matrix(ncol = 13, nrow = (length(man_ct)*length(ct)*length(margin_secs))))
          colnames(Results[[current_sp]]) <- c("man_ct", "ct", "margin_secs", "NE", "TP", "FP", "TN", "FN",
                                               "Precision", "Sensitivity", "FPR", "Perf", "Filename")
          row <- 1L
        }
        else {
          row <- nrow(Results[[current_sp]]) + 1L
        }
        
        ### Iterate all manual confidence thresholds
        for (current_man_ct in man_ct) {
          
          ### Iterate all margin values
          for (current_margin in margin_secs) {
            
            ### Convert the chosen margin interval from seconds to frames
            margin_fps <- floor(current_margin * Manual_obs[[current_film]][[current_sp]][1, fps])
            
            ### Extract all the manual annotations of the current species in the current film with a minimum confidence of the current manual ct
            current_observations <- Manual_obs[[current_film]][[current_sp]][Manual_obs[[current_film]][[current_sp]][,Confidence] >= current_man_ct, ]
            
            pos_frames <- integer()
            margin_frames <- integer()
            if (length(current_observations) > 0) {
              ### Create an integer sequence of the all the frames within the manually annotated intervals
              for (i in 1:(nrow(current_observations))) {
                obs <- seq.int(current_observations[i, Frame_in], current_observations[i, Frame_out])
                pos_frames <- c(pos_frames, obs[!(obs %in% pos_frames)])
              }
              
              ### Create an integer sequence of all the frames within the margin intervals, which surround every annotation with up to margin_fps frames
              if (margin_fps >= 1) {
                for (j in 1:(nrow(current_observations))) {
                  obs_margin <- c(seq.int((current_observations[j, Frame_in]-margin_fps), (current_observations[j, Frame_in]-1L)), 
                                  seq.int((current_observations[j, Frame_out]+1L), (current_observations[j, Frame_out]+margin_fps)))
                  margin_frames <- c(margin_frames, obs_margin[!(obs_margin %in% margin_frames)])
                }
              }
            }
            
            ### Create an integer sequence of the all the frames outside of the manually annotated intervals
            all_frames_excl_margin <- (1:tot_frames)[!((1:tot_frames) %in% margin_frames)]
            neg_frames <- all_frames_excl_margin[!(all_frames_excl_margin %in% pos_frames)]
            
            ### Iterate all model confidence thresholds
            for (current_ct in ct) {
              
              ### Extract all the frames where the model has detected the current species in the current film, with a minimum conf value of the current ct
              current_obs_model <- Model_obs[[as.character(which(Species == current_sp)-1L)]][(
                                   Model_obs[[as.character(which(Species == current_sp)-1L)]]["conf"] >= current_ct), "frame_no"]
              
              ### Calculate the result statistics
              NE <- length(margin_frames[margin_frames %in% current_obs_model])                         ### Number of not evaluated frames
              TP <- length(pos_frames[pos_frames %in% current_obs_model])                               ### Number of true positives
              FP <- length(current_obs_model[!(current_obs_model %in% c(pos_frames, margin_frames))])   ### Number of false positives
              TN <- length(neg_frames[!(neg_frames %in% current_obs_model)])                            ### Number of true negatives
              FN <- length(pos_frames[!(pos_frames %in% current_obs_model)])                            ### Number of false negatives
              
              Precision <- TP/(TP+FP)       ### Positive predictive value (PPV): What is the probability that any given model detection is correct?
              Sensitivity <- TP/(TP+FN)     ### True positive rate (TPR): How many percent of the positive frames does the model detect?
              FPR <- FP/(TN+FP)             ### False positive rate (FPR): How many percent of the negative frames does the model falsely detect?
              
              
              ### Optionally, replace undefined values with zero
              if (Set_undefs_to_zero == TRUE) {
                if (is.nan(Precision)) {Precision <- 0L}
                if (is.nan(Sensitivity)) {Sensitivity <- 0L}
                if (is.nan(FPR)) {FPR <- 0L}
              }
              
              ### Save the results to the results data frame of the current species
              Results[[current_sp]][row, "man_ct"] <- current_man_ct
              Results[[current_sp]][row, "ct"] <- current_ct
              Results[[current_sp]][row, "margin_secs"] <- current_margin
              Results[[current_sp]][row, "NE"] <- NE
              Results[[current_sp]][row, "TP"] <- TP
              Results[[current_sp]][row, "FP"] <- FP
              Results[[current_sp]][row, "TN"] <- TN
              Results[[current_sp]][row, "FN"] <- FN
              Results[[current_sp]][row, "Precision"] <- Precision
              Results[[current_sp]][row, "Sensitivity"] <- Sensitivity
              Results[[current_sp]][row, "FPR"] <- FPR
              Results[[current_sp]][row, "Perf"] <- Precision * Sensitivity
              Results[[current_sp]][row, "Filename"] <- names(Manual_obs[current_film])
              
              row <- row+1L
            }
          }
        }
        ### Order the rows in ascending order according to one factor at a time: Filename, man_ct, ct and margin_secs
        Results[[current_sp]] <- Results[[current_sp]][order(Results[[current_sp]][,"Filename"], Results[[current_sp]][,"man_ct"],
                                                             Results[[current_sp]][,"ct"], Results[[current_sp]][,"margin_secs"]), ]
        row.names(Results[[current_sp]]) <- NULL
      }
    }
  }
  ### Optionally, remove duplicate rows
  if (Drop_duplicates == TRUE) {
    for (current_sp in names(Results)) {
      Results[[current_sp]] <- unique(Results[[current_sp]])
    }
  }
  
  ### Assign all the data frames containing the results to a global list variable, whose name is given by Assign_to 
  assign(Assign_to, Results, envir = parent.frame())
  
  ### Print warning/success messages
  cat("DONE\n")
  if (length(missing_CSVs) > 0) {
    cat("\nExcluded films - missing CSV files:", unique(missing_CSVs), paste0("in directory ", CSV_path), sep = "\n")
  } else {
    cat(paste0("\nAll films in sheet", if (length(Sheets) > 1) {paste0("s '", paste(Sheets[1:(length(Sheets)-1)], collapse = "', '"),
               "' and '", Sheets[length(Sheets)])} else {paste0(" '", Sheets)}, "' are included\n"))
  }
}



### Function to summarize the result statistics for all films.
### If there are more than one row with identical sets of thresholds for a certain film, takes the mean values for those.
summarize.by.species <- function(input_list, Set_undefs_to_zero = FALSE) {
  
  cat("Processing...\n")
  output_list <- list()
  averaged <- data.frame(matrix(nrow = 0, ncol = 6))
  colnames(averaged) <- c("Species", "Film", "man_ct", "ct", "margin_secs", "# of averaged rows")
  
  ### Iterate all species in the input list, given by the names of the constituent data frames
  for (Spec in names(input_list)) {
    
    ### Exclude all duplicate rows in the current data frame
    input_list[[Spec]] <- unique(input_list[[Spec]])
    row.names(input_list[[Spec]]) <- NULL
    
    ### Count the number of rows that are unique in their combination of factors: man_ct, ct and margin_secs
    factor_combinations <- unique(input_list[[Spec]][, c("man_ct", "ct", "margin_secs")])
    factor_combinations <- factor_combinations[order(factor_combinations[,"man_ct"], factor_combinations[,"ct"], factor_combinations[,"margin_secs"]), ]
    
    ### Create as many rows in the the data frame that is to contain the summarized results
    output_list[[Spec]] <- data.frame(matrix(nrow = nrow(factor_combinations), ncol = ncol(input_list[[Spec]])))
    colnames(output_list[[Spec]]) <- c(colnames(input_list[[Spec]])[colnames(input_list[[Spec]]) != "Filename"], "Included films")
    
    ### Iterate the rows of unique combinations of factors (man_ct-ct-margin_secs)
    for (row in 1:nrow(factor_combinations)) {
      
      ### Create a temporary data frame
      current_fac_combo <- data.frame(matrix(nrow = 0, ncol = ncol(input_list[[Spec]])))
      colnames(current_fac_combo) <- colnames(input_list[[Spec]])
      
      ### Write the summarized results of all films to the temporary data frame, one row per possible combination of factors (man_ct-ct-margin_secs)
      ### If there are more than one row for the same combination of film and factors, take the mean values of NE, TP, FP, TN and FN for these rows
      for (current_film in unique(input_list[[Spec]][,"Filename"])) {
        current_fac_film_combo <- merge(factor_combinations[row,], input_list[[Spec]][c(input_list[[Spec]][,"Filename"] == current_film),])
        if (nrow(current_fac_film_combo) == 0) {
          next
        } else if (nrow(current_fac_film_combo) >= 2) {
          current_fac_film_combo[1, "NE"] <- mean(current_fac_film_combo[,"NE"])
          current_fac_film_combo[1, "TP"] <- mean(current_fac_film_combo[,"TP"])
          current_fac_film_combo[1, "FP"] <- mean(current_fac_film_combo[,"FP"])
          current_fac_film_combo[1, "TN"] <- mean(current_fac_film_combo[,"TN"])
          current_fac_film_combo[1, "FN"] <- mean(current_fac_film_combo[,"FN"])
          
          averaged[nrow(averaged)+1,] <- c(Spec, current_film, current_fac_film_combo[1, "man_ct"], current_fac_film_combo[1, "ct"],
                                           current_fac_film_combo[1, "margin_secs"], nrow(current_fac_film_combo))
          
          current_fac_film_combo <- current_fac_film_combo[1,]
        }
        current_fac_combo[nrow(current_fac_combo)+1,] <- current_fac_film_combo
      }
      
      ### Write the results for the current combination of factors, based on all films pooled together, to the final output data frame
      output_list[[Spec]][row, "man_ct"] <- current_fac_combo[1, "man_ct"]
      output_list[[Spec]][row, "ct"] <- current_fac_combo[1, "ct"]
      output_list[[Spec]][row, "margin_secs"] <- current_fac_combo[1, "margin_secs"]
      output_list[[Spec]][row, "NE"] <- sum(current_fac_combo[,"NE"])
      output_list[[Spec]][row, "TP"] <- sum(current_fac_combo[,"TP"])
      output_list[[Spec]][row, "FP"] <- sum(current_fac_combo[,"FP"])
      output_list[[Spec]][row, "TN"] <- sum(current_fac_combo[,"TN"])
      output_list[[Spec]][row, "FN"] <- sum(current_fac_combo[,"FN"])
      output_list[[Spec]][row, "Precision"] <- sum(current_fac_combo[,"TP"]) / (sum(current_fac_combo[,"TP"]) + sum(current_fac_combo[,"FP"]))
      output_list[[Spec]][row, "Sensitivity"] <- sum(current_fac_combo[,"TP"]) / (sum(current_fac_combo[,"TP"]) + sum(current_fac_combo[,"FN"]))
      output_list[[Spec]][row, "FPR"] <- sum(current_fac_combo[,"FP"]) / (sum(current_fac_combo[,"TN"]) + sum(current_fac_combo[,"FP"]))
      
      ### Optionally, replace undefined values with zero
      if (Set_undefs_to_zero == TRUE) {
        if (is.nan(output_list[[Spec]][row, "Precision"])) {output_list[[Spec]][row, "Precision"] <- 0L}
        if (is.nan(output_list[[Spec]][row, "Sensitivity"])) {output_list[[Spec]][row, "Sensitivity"] <- 0L}
        if (is.nan(output_list[[Spec]][row, "FPR"])) {output_list[[Spec]][row, "FPR"] <- 0L}
      }
      
      output_list[[Spec]][row, "Perf"] <- output_list[[Spec]][row, "Precision"] * output_list[[Spec]][row, "Sensitivity"]
      
      ### Also, for each row (= combination of factors), annotate which films the pooled results are based on, if it's not all of the films
      if (nrow(current_fac_combo) == length(unique(input_list[[Spec]][,"Filename"]))) {
        output_list[[Spec]][row, "Included films"] <- paste0("All (", nrow(current_fac_combo), ")")
      } else {
        output_list[[Spec]][row, "Included films"] <- paste(current_fac_combo[,"Filename"], collapse = ", ")
      }
    }
  }
  
  cat("DONE\n")
  
  ### If there have been film-factors-combo duplicates, print them to show which
  if (nrow(averaged) > 0) {
    cat("\nFor the following combinations of factors, there were more than one row. NE, TP, FP, TN and FN are given as mean values of these rows:\n")
    print.data.frame(averaged, print.gap = 6, row.names = FALSE)
  }
  
  return(output_list)
}



### Function to annotate cells in a grid overlay with presence or absence of species
                                ### Default values
annotate.grid.cells <- function(xlsx_filename,
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
                                CSV_path = "model_output",
                                Suffix = ".csv",
                                Annotate_with_cv = FALSE,
                                Annotate_with_PPV = FALSE,
                                Drop_NA_rows = TRUE,
                                ct = c(0),
                                min_time_within_cell = c(0)
                                ) {
  
  cat("Processing...\n")
  start_time <- Sys.time()
  
  ### Read spreadsheet
  Grid_times <- read.xlsx(xlsx_filename, sheet = Grid_times_sheet_name, sep.names = " ")
  Film_info <- read.xlsx(xlsx_filename, sheet = Film_info_sheet_name, sep.names = " ")
  
  ### Check which of PageNumber and PageName that are present in the spreadsheet
  CellCol <- c(PageNumber, PageName)[which(c(PageNumber, PageName) %in% names(Grid_times))]
  if (length(CellCol) == 0) {
    stop("Neither a valid PageName nor PageNumber column name provided")
  }
  
  ### If annotating with confidence values, only use the lowest confidence threshold
  if (Annotate_with_cv == TRUE) {
    ct = min(ct)
  }
  
  ### Extract which films overlap with the in/out timestamps of the grid cells
  films_to_read <- Film_info[c((Film_info[,Unixtime_end] >= min(Grid_times[,Unixtime_in])) &
                             (Film_info[,Unixtime_start] <= max(Grid_times[,Unixtime_out])) &
                             (rowSums(is.na(Film_info[,c(Film, Unixtime_start, Unixtime_end, fps)])) == 0)), Film]
  Tot <- NULL
  noCSVfilms <- NULL
  ### Iterate the extracted films
  for (readfilm in films_to_read) {
    ### Skip the current film if its corresponding CSV file isn't present in the working directory
    if (!(file.exists(paste0(CSV_path, "/", readfilm, Suffix)))) {
      noCSVfilms <- c(noCSVfilms, paste0(readfilm, Suffix))
      next
    }
    else {
      ### Otherwise, read the CSV files and divide the observations by class_id
      Tot[[readfilm]] <- read.csv(paste0(CSV_path, "/", readfilm, Suffix))
      Tot[[readfilm]] <- split(Tot[[readfilm]], Tot[[readfilm]]$class_id)
    }
  }
  
  ### Print which films that were excluded due to non-overlapping timestamps, and which that were excluded due to missing CSV files
  films_not_to_read <- Film_info[which(!(Film_info[,Film] %in% films_to_read) & !is.na(Film_info[,Film])), Film]
  if (length(films_not_to_read) > 0) {
    cat("\nExcluded films, not overlapping with track timings:", films_not_to_read, sep = "\n")
  }
  if (length(noCSVfilms[which(!(noCSVfilms %in% films_not_to_read))]) > 0) {
    cat("\nExcluded films - missing CSV files:", noCSVfilms[which(!(noCSVfilms %in% films_not_to_read))], paste0("in directory ", CSV_path), sep = "\n")
  }
  
  ### Create the resulting data frame to be returned at the end, with one row per grid cell
  output_df <- unique(Grid_times[sort(CellCol)])
  
  ### Iterate all species
  for (ID in 1:length(Species)) {
    ### Skip the species that have been left out in the beginning
    if (is.na(Species[ID])) {
      next
    }
    else {
      
      ### A species variable containing only the current species, used later if Annotate_with_PPV is set to TRUE
      Species2 <- NULL
      Species2[ID] <- Species[ID]
      
      ### Iterate all confidence thresholds
      for (current_ct in ct) {
        ### Iterate all minimum time per cell thresholds
        for (current_min_time in min_time_within_cell) {
          
          ### Create an annotation column named with the current species, confidence threshold in percent, and minimum time per cell threshold
          current_col <- paste0(Species[ID], "_", current_ct*100, "%_", current_min_time, "s")
          output_df[,current_col] <- NA
          
          ### Iterate all grid cells
          for (current_cell in output_df[,CellCol[1]]) {
            obs_within_cell <- numeric()
            time_within_cell <- 0
            overlapping <- 0
            ### Iterate all time spans during which the ROV has been in the current grid cell
            for (current_row in rownames(Grid_times[c(Grid_times[,CellCol[1]] == current_cell),])) {
              
              ### Extract which films, of those that have CSV files, that overlap with the current time span
              films_within_cell <- Film_info[(Film_info[,Unixtime_end] >= Grid_times[current_row, Unixtime_in]) &
                                               (Film_info[,Unixtime_start] <= Grid_times[current_row, Unixtime_out]) &
                                               (rowSums(is.na(Film_info[,c(Film, Unixtime_start, Unixtime_end, fps)])) == 0), Film]
              films_within_cell <- films_within_cell[which(films_within_cell %in% names(Tot))]
              overlapping <- overlapping + length(films_within_cell)
              
              ### Iterate all films that overlap with the current time span
              for (current_film in films_within_cell) {
                
                ### Extract all observations with a confidence value equal to the current confidence threshold or higher
                all_obs <- Tot[[current_film]][[as.character(ID-1)]][(Tot[[current_film]][[as.character(ID-1)]]$conf >= current_ct),]
                ### Convert the frame numbers to unixtime with two decimals (hundredths of seconds)
                all_obs$time <- round((all_obs$frame_no / Film_info[which(Film_info[,Film] == current_film), fps])
                                      + Film_info[which(Film_info[,Film] == current_film), Unixtime_start], digits = 2)
                
                ### Count how many frames that are within the current time span of the current grid cell (by 0.01 sec accuracy)
                current_obs <- all_obs$conf[all_obs$time %in% seq(round(Grid_times[current_row, Unixtime_in], digits = 2),
                                                                  round(Grid_times[current_row, Unixtime_out], digits = 2), by = 0.01)]
                obs_within_cell <- c(obs_within_cell, current_obs)
                
                ### Count for how many seconds the current species has been observed within the current grid cell in total
                time_within_cell <- time_within_cell + (length(current_obs) / Film_info[which(Film_info[,Film] == current_film), fps])
              }
            }
            
            ### As long as there are films whose timestamps overlap with those of the current grid cell, annotate...
            if (overlapping > 0) {
              ### ...absence ("0") if there are less frames than the minimum threshold
              if (time_within_cell == 0 | time_within_cell < current_min_time) {
                output_df[which(output_df[,CellCol[1]] == current_cell), current_col] <- 0L
              }
              else {
                ### ...presence ("1") or the highest confidence value detected in that grid cell, otherwise
                if (Annotate_with_cv == FALSE) {
                  output_df[which(output_df[,CellCol[1]] == current_cell), current_col] <- 1L
                }
                else {
                  if (Annotate_with_PPV == FALSE) {
                    output_df[which(output_df[,CellCol[1]] == current_cell), current_col] <- max(obs_within_cell)
                  }
                  else {
                    ### ...or optionally, the precision of the model at the highest confidence value detected within the grid cell
                    invisible(capture.output(
                      compare.film.by.film(Suffix = get("Suffix", envir = environment()),
                                           Species = Species2,
                                           Assign_to = "Results_by_film",
                                           ct = max(obs_within_cell),
                                           man_ct = 0.5)
                    ))
                    Precision <- sum(Results_by_film[[Species[ID]]][,"TP"]) / (sum(Results_by_film[[Species[ID]]][,"TP"]) +
                                                                               sum(Results_by_film[[Species[ID]]][,"FP"]))
                    #if (is.nan(Precision)) {TPR <- 0}    ### (This line was not a very good idea, because it forces really high confidence
                                                          ### observations to zero if there are no equally high conf observations in the
                                                          ### CSV files for the model testing.)
                    output_df[which(output_df[,CellCol[1]] == current_cell), current_col] <- Precision
                    rm(Results_by_film, envir = environment())
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  ### Assign the xlsx_filename parameter to the global environment so it becomes accessible to the print.to.xlsx function
  xlsx_filename_without_path <- tail(unlist(strsplit(xlsx_filename, "[/]|[\\]")), n = 1L)
  xlsx_filename_without_path <- substr(xlsx_filename_without_path, 1L, nchar(xlsx_filename_without_path)-5L)
  assign("filename", xlsx_filename_without_path, envir = .GlobalEnv)
  
  ### Optionally, remove rows representing grid cells that have no overlapping films (where all annotation columns are NA)
  if (Drop_NA_rows == TRUE) {
    if (ncol(output_df[,-c(which(colnames(output_df) %in% c(PageName, PageNumber)))]) == 1L) {
      output_df <- output_df[is.na(output_df[,-c(which(colnames(output_df) %in% c(PageName, PageNumber)))]),]
    }
    else {
      output_df <- output_df[rowSums(is.na(output_df[,-c(which(colnames(output_df) %in% c(PageName, PageNumber)))])) == 0,]
    }
  }
  
  cat("\nDONE in ", format.difftime(difftime(Sys.time(), start_time)), "\n", sep = "")
  return(output_df)
}



### Function to save a data frame or list of data frames to a new xlsx workbook or CSV file
### (For lists: creates one CSV file, or one xlsx worksheet, per list object)
save.to.file <- function(input, output_file = "R_results.xlsx") {
  
  ### Extract file path
  path <- dirname(output_file)
  if (path == ".") {
    path <- getwd()   ### Default path
  }
  
  ### Extract filename and file extension
  split <- tail(unlist(strsplit(output_file, "/")), n = 1L)
  split <- unlist(strsplit(split, "\\."))
  if (length(split) > 1) {
    filename <- paste(head(split, n = length(split)-1L), collapse = ".")
    filetype <- tail(split, n = 1L)
  }
  else {
    filename <- split
    filetype <- "xlsx"    ### Default filetype
  }
  
  ### Determine the sheet names to use: either the name of the input data frame, or the names of the objects in the input list
  if (is.data.frame(input) | is.matrix(input) | is.array(input)) {
    names <- deparse(substitute(input))
  }
  else if (is.list(input)) {
    names <- names(input)
  }
  else {
    stop("Invalid input type")
  }
  
  legalize.filename <- function(filename, filetype) {
    ### Remove illegal characters from the filename
    filename <- gsub('[<>*?":/|]|\\[|\\]|\\\\|(\\s+\\.+)+$|(\\.+\\s+)+$', '', filename)
    
    ### Append a number at the end of the filename if there already exists a file with that name
    file_num <- 2L
    numbered_filename <- filename
    while (file.exists(paste0(path, "/", numbered_filename, ".", filetype))) {
      numbered_filename <- paste0(numbered_filename, "_(", file_num, ")")
      file_num <- file_num + 1L
    }
    return(numbered_filename)
  }
  
  if (filetype == "xlsx") {
    
    library("openxlsx")
    
    numbered_filename <- legalize.filename(filename, filetype)
    
    ### Create a workbook
    wb <- openxlsx::createWorkbook()
    
    ### Iterate the objects in the input variable
    for (current_obj in names) {
      
      ### Remove illegal characters from the current sheet name
      sheet_name <- unlist(strsplit(current_obj,'[$*?":/|]|\\[|\\]|\\\\'))
      sheet_name <- paste0(sheet_name[sheet_name != ""], collapse = " - ")
      
      ### Create a worksheet with the modified sheet name and write the corresponding data to it (data frame or the current list object)
      openxlsx::addWorksheet(wb, sheetName = sheet_name)
      openxlsx::writeData(wb, sheet = sheet_name, x = if (is.data.frame(input)) {input} else if (is.list(input)) {input[[sheet_name]]})
    }
    
    ### Finally, save the file, and print a success/error message
    success <- openxlsx::saveWorkbook(wb, file = paste0(path, "/", numbered_filename, ".xlsx"), overwrite = FALSE, returnValue = TRUE)
    if (success == TRUE) {
      cat("Saved '", deparse(substitute(input)), "' to workbook '", numbered_filename, ".xlsx'", sep = "")
    }
    else {
      warning("Something went wrong saving the file (openxlsx::saveWorkbook() didn't return a success message)")
    }
  }
  else if (filetype == "csv") {
    
    ### Iterate the objects in the input variable
    for (current_obj in names) {
      
      ### If the input is a list, append the name of the current list object to the filename, then apply potential modifications
      new_filename <- paste0(filename, if (!is.data.frame(input)) {paste0(" - ", current_obj)})
      numbered_filename <- legalize.filename(new_filename, filetype)
      
      data <- if (is.data.frame(input)) {input} else {input[[current_obj]]}
      
      ### Save the object to a new CSV file with the modified filename
      write.csv(
        x = data,
        file = paste0(path, "/", numbered_filename, ".csv"),
        row.names = FALSE
      )
      
      ### Also, make a CSVT file specifying the data types of the different columns in the CSV file
      ### (To allow for more convenient GIS import. More info on the CSVT format: https://giswiki.hsr.ch/GeoCSV#CSVT_file_format_specification)
      field_types <- NULL
      for (column in data) {
        current_type <- typeof(column)
        if (current_type == "character") {current_type <- "String"}
        if (current_type == "double") {current_type <- "Real"}
        if (current_type == "integer") {current_type <- "Integer"}
        field_types <- c(field_types, current_type)
      }
      field_types <- paste0('\"', paste(field_types, collapse = '","'), '\"')
      cat(field_types, file = paste0(path, "/", numbered_filename, ".csvt"))
      
      ### If no errors have occurred so far, assume that the files were saved correctly and print a success message
      cat("Saved '", if (is.data.frame(input)) {deparse(substitute(input))} else {deparse(substitute(input[[current_obj]]))},
          "' to CSV file '", numbered_filename, ".csv'\n", sep = "")
    }
  }
  ### Print error if filetype is neither xlsx nor csv
  else {
    stop(paste0("Invalid filetype. File extension must be either .xlsx or .csv, \".", filetype, "\" is not supported"))
  }
}


