#!/usr/bin/env Rscript

# Stop if R package "readxl" is not installed:
if (!require("readxl", quietly = TRUE)) {
  stop("\nmake_robot_files requires the R package \"readxl\".\n",
       "Please install it by running the following command into your R console:\n",
       "install.packages(\"readxl\")\n")
}

# Capture make_robot_files arguments from standard input:
args <- commandArgs(trailingOnly = TRUE)

# Resolve arguments - case 1: user provided only --help option ------------

# Print help:
if (length(args) == 1L && args == "--help") { 
  message("
  NAME
  make_robot_files -- Create lookup files for Phenix robot runs
  
  SYNOPSIS
  make_robot_files [--date] [--pathin] [--pathout]
  make_robot_files --help
  
  DESCRIPTION
  make_robot_files generates the lookup files necessary for a Phenix robot run, provided a \"Robot experiments.xlsx\" file and a run date.
  There are 3 lookup files (Protocols, Inventory and Process) per type of plate location (rack / incubator). 
  make_robot_files writes 3 files if only rack or incubator is used, and 6 files if both rack and incubator are used.
  If the --date option is not provided, make_robot_files assumes that the target date is the current date. 
  If the --pathin option is not provided, make_robot_files tries to read files from the \"~/Desktop/Robot protocols/\" folder.
  If the --pathout option is not provided, make_robot_files tries to write files to the \"//PH2-101/Users/Automation/Desktop/Robot protocols/\" folder.
  
  The following options are available:
  
  --date Robot run date for which the lookup files should be generated. 
         Valid values are in the form YYYY-MM-DD or YYYY.MM.DD.

  --pathin Absolute / relative path leading to the \"Robot experiments.xlsx\" file. 
           Note that a relative path starting from your HOME folder begins with the string \"~/\".
           
  --pathout Absolute / relative path indicating the folder where to write the lookup files.
            The path must lead to an existing folder. 
          
  --help Show this screen.
  
  EXAMPLES
  make_robot_files --help
  make_robot_files
  make_robot_files --date 2023.01.12
  make_robot_files --pathin ~/Desktop
  make_robot_files --date 2023-01-12 --pathin ~/Desktop --pathout ~/Downloads
  
  INPUT FILE SPECIFICATIONS
  - make_robot_files works on the sheet \"CURRENT\" in the \"Robot experiments.xlsx\" file.
    -> Please DO NOT DELETE OR RENAME this sheet.
  - The sheet \"CURRENT\" must contain the following 8 UPPERCASE columns:
    RUN DATE, HARMONY USERNAME, HARMONY EXPERIMENT NAME, EXPERIMENT DURATION (MIN), PLATE NAME, INCUBATOR, RACK, SLOT.
    -> Please DO NOT MODIFY these column names.
  - The required UPPERCASE columns must not contain missing values and must meet the following criteria:
    -> RUN DATE: type \"text\" / values are in the form YYYY-MM-DD or YYYY.MM.DD.
    -> HARMONY USERNAME: type \"text\".
    -> HARMONY EXPERIMENT NAME: type \"text\" / values do not contain comma(s).
    -> EXPERIMENT DURATION (MIN): type \"numeric\".
    -> PLATE NAME: type \"character\" / values do not contain comma(s) and have no more than 30 characters.
    -> LOCATION: type \"text\" / values are either \"RACK\" or \"INCUBATOR\" (case insensitive).
    -> RACK: type \"numeric\" / values are within a valid range: from 1 to 4 for RACK, 1 or 2 for INCUBATOR.
    -> SLOT: type \"numeric\" / values are within a valid range: from 1 to 14 for RACK, from 1 to 22 for INCUBATOR.
  - make_robot_files does not cross-check HARMONY USERNAME nor HARMONY EXPERIMENT NAME between \"Robot experiments.xlsx\" and Harmony.
    -> If the generated lookup files do not work, please make sure that this information matches exactly.\n"
  )
  quit(save = "no")
}

# Resolve arguments - case 2: user did not provide options ----------------

# Set default date, pathin and pathout values: 
if (length(args) == 0L) { 
  no_args <- T
  # Set date=Sys.date():
  date <- as.character(Sys.Date())
  # Set pathin=$HOME/Desktop/Robot protocols/:
  if (Sys.info()["sysname"] == "Windows") {
    pathin <- paste0(Sys.getenv("USERPROFILE"), "/Desktop/Robot protocols/")
  } else {
    pathin <- paste0(path.expand('~'), "/Desktop/Robot protocols/")
  }
  if (!dir.exists(pathin)) {
    stop("\n\"~/Desktop/Robot protocols/\" folder does not exist.\nIf \"Robot experiments.xlsx\" is located elsewhere, please provide a --pathin value.")
  }
  if (!file.exists(paste0(pathin, "Robot experiments.xlsx"))) {
    stop("\n\"Robot experiments.xlsx\" file not in \"~/Desktop/Robot protocols/\" folder.\nIf located elsewhere, please provide a --pathin value.\n")
  }
  # Set pathout=//PH2-101/Users/Automation/Desktop/Robot protocols/:
  pathout <- "//PH2-101/Users/Automation/Desktop/Robot protocols/"
  if (!dir.exists(pathout)) {
    stop("\nCannot write lookup files to disk: \"//PH2-101/Users/Automation/Desktop/Robot protocols/\" folder does not exist.")
  }
} else {
  no_args <- F
}

# Resolve arguments - case 3: user provided option(s) ---------------------

if (!no_args) {
  
  # Stop if only 1 option name which is not --help is provided without value:
  if ((length(args) == 1L) && args != "--help") { 
    if (args %in% c("--date", "--pathin", "--pathout")) {
      stop("\n", args, " option must be given a value and cannot be used on its own.\n") 
    } else {
      stop("\n", args, " is not a valid option.\n") 
    }
  }
  
  # Stop if --help option is misused:
  if ((length(args) > 1L) && (sum(args == "--help", na.rm = TRUE) > 0L)) {
    stop("\n--help option must be invoqued on its own.\n")
  }
  
  # Stop if the number of option name / option value pair(s) is invalid:
  if (!length(args) %in% c(0L, 1L, 2L, 4L, 6L)) { 
    stop("\nInvalid number of option name / option value pairs.\n")
  }
  
  # Check the validity of option name(s):
  arg.table <- data.frame(arg.nm = args[seq.int(from = 1L, to = (length(args)-1L), by = 2L)],
                          arg.val = args[seq.int(from = 2L, to = length(args), by = 2L)])
  arg.table$arg.nm.valid <- arg.table$arg.nm %in% c("--date", "--pathin", "--pathout")
  
  # Stop if one or more option name(s) is/are invalid:
  if (!all(arg.table$arg.nm.valid)) {
    if (length(arg.table[arg.table$arg.nm.valid == FALSE, "arg.nm"]) == 1L) {
      stop("\n", arg.table[arg.table$arg.nm.valid == FALSE, "arg.nm"], " is not a valid option.\n")
    } else {
      stop("\n",paste0(arg.table[arg.table$arg.nm.valid == FALSE, "arg.nm"], collapse = ", "), " are not valid options.\n")
    }
  }
  
  # Stop if one or more option name(s) is/are provided more than one time:
  arg.table$arg.nm.unique <- lapply(X = arg.table$arg.nm,
                                    FUN = function (var) { if (length(grep(pattern = paste0("^", var, "$"), x = arg.table$arg.nm)) == 1L) { TRUE } else { FALSE }})
  
  if (any(arg.table$arg.nm.unique == FALSE)) {
    if (length(unique(arg.table[arg.table$arg.nm.unique == FALSE, "arg.nm"])) == 1L) {
      stop("\n", unique(arg.table[arg.table$arg.nm.unique == FALSE, "arg.nm"]), " option must be invoqued only once.\n")
    } else {
      stop("\n", paste0(unique(arg.table[arg.table$arg.nm.unique == FALSE, "arg.nm"]), collapse = ", "), " options must be invoqued only once.\n")
    }
  }
  
  # Check the validity of option value(s):
  arg.table$arg.val.valid <- vector(mode = "logical", length = length(arg.table$arg.val))
  
  # => Check --date option value:
  if (any(arg.table$arg.nm == "--date")) {
    arg.table[arg.table$arg.nm == "--date", "arg.val"] <-  gsub(x = arg.table[arg.table$arg.nm == "--date", "arg.val"], 
                                                                pattern = "\\.", 
                                                                replacement = "-")
    if (grepl(x = arg.table[arg.table$arg.nm == "--date", "arg.val"], pattern = "^\\d{4}-\\d{2}-\\d{2}$")) {
      arg.table[arg.table$arg.nm == "--date", "arg.val.valid"] <- TRUE
    } else {
      arg.table[arg.table$arg.nm == "--date", "arg.val.valid"] <- FALSE
    }
  }
  
  # => Check --pathin option value:
  if (any(arg.table$arg.nm == "--pathin")) {
    if (!grepl(x = arg.table[arg.table$arg.nm == "--pathin", "arg.val"], pattern = "^.*/$")) {
      arg.table[arg.table$arg.nm == "--pathin", "arg.val"] <- paste0(arg.table[arg.table$arg.nm == "--pathin", "arg.val"], "/")
    }
    if (dir.exists(arg.table[arg.table$arg.nm == "--pathin", "arg.val"])) {
        if (file.exists(paste0(arg.table[arg.table$arg.nm == "--pathin", "arg.val"], "Robot experiments.xlsx"))) {
          arg.table[arg.table$arg.nm == "--pathin", "arg.val.valid"] <- TRUE
        } else {
          arg.table[arg.table$arg.nm == "--pathin", "arg.val.valid"] <- FALSE
        }
    } else {
      arg.table[arg.table$arg.nm == "--pathin", "arg.val.valid"] <- FALSE
    }
  }
  
  # => Check --pathout option value
  if (any(arg.table$arg.nm == "--pathout")) {
    if (!grepl(x = arg.table[arg.table$arg.nm == "--pathout", "arg.val"], pattern = "^.*/$")) {
      arg.table[arg.table$arg.nm == "--pathout", "arg.val"] <- paste0(arg.table[arg.table$arg.nm == "--pathout", "arg.val"], "/")
    }
    arg.table[arg.table$arg.nm == "--pathout", "arg.val.valid"] <- dir.exists(arg.table[arg.table$arg.nm == "--pathout", "arg.val"])
  } 
  
  # Stop if one or more option value(s) is/are invalid:
  if (length(arg.table[arg.table$arg.val.valid == FALSE, "arg.nm"]) > 0L) {
    if (length(arg.table[arg.table$arg.val.valid == FALSE, "arg.nm"]) == 1L) {
      if (arg.table[arg.table$arg.val.valid == FALSE, "arg.nm"] == "--pathin") {
        if (!dir.exists(arg.table[arg.table$arg.nm == "--pathin", "arg.val"])) {
          stop('\nFolder \"', arg.table[arg.table$arg.val.valid == FALSE, "arg.val"], '\" does not exist.\n')
        } else {
          stop("\n\"Robot experiments.xlsx\" file not in \"", arg.table[arg.table$arg.val.valid == FALSE, "arg.val"], "\" folder.\n")
        }
      } else if (arg.table[arg.table$arg.val.valid == FALSE, "arg.nm"] == "--pathout") {
        stop("\nCannot write lookup files to disk: ", arg.table[arg.table$arg.val.valid == FALSE, "arg.val"], " folder does not exist.\n")
      } else {
        stop("\n", arg.table[arg.table$arg.val.valid == FALSE, "arg.val"], " is not a valid value for ", 
             arg.table[arg.table$arg.val.valid == FALSE, "arg.nm"], " option.\n")
      }
    } else {
      stop("\n", paste0(arg.table[arg.table$arg.val.valid == FALSE, "arg.val"], collapse = ", "), " are not valid values for ", 
           paste0(arg.table[arg.table$arg.val.valid == FALSE, "arg.nm"], collapse = ", "), " options, respectively.\n")
    }
  }
  
  # Set date, pathin and pathout to user defined value(s) or to default if not provided:
  if (any(arg.table$arg.nm == "--date")) {
    date <- arg.table[arg.table$arg.nm == "--date", "arg.val"]
  } else {
    date <- as.character(Sys.Date())
  }
  
  if (any(arg.table$arg.nm == "--pathin")) {
    pathin <- arg.table[arg.table$arg.nm == "--pathin", "arg.val"]
  } else {
    if (Sys.info()["sysname"] == "Windows") {
      pathin <- paste0(Sys.getenv("USERPROFILE"), "/Desktop/Robot protocols/")
    } else {
      pathin <- paste0(path.expand('~'), "/Desktop/Robot protocols/")
    }
    if (!dir.exists(pathin)) {
      stop("\n\"~/Desktop/Robot protocols/\" folder does not exist.\nIf \"Robot experiments.xlsx\" is located elsewhere, please provide a --pathin value.")
    }
    if (!file.exists(paste0(pathin, "Robot experiments.xlsx"))) {
      stop("\n\"Robot experiments.xlsx\" file not in \"~/Desktop/Robot protocols/\" folder.\nIf located elsewhere, please provide a --pathin value.\n")
    }
  }
  
  if (any(arg.table$arg.nm == "--pathout")) {
    pathout <- arg.table[arg.table$arg.nm == "--pathout", "arg.val"]
  } else {
    pathout <- "//PH2-101/Users/Automation/Desktop/Robot protocols/"
    if (!dir.exists(pathout)) {
      stop("\nCannot write lookup files to disk: \"//PH2-101/Users/Automation/Desktop/Robot protocols/\" folder does not exist.")
    }
  } 
}

# Read and parse "Robot experiments.xlsx file" ----------------------------

# Stop if "Robot experiments.xlsx" file does not contain the "CURRENT" sheet:
current_sheet_missing <- unique(class(try(expr = readxl::read_excel(path = paste0(pathin, "/Robot experiments.xlsx"),
                                                                    sheet = "CURRENT",
                                                                    .name_repair = "unique",
                                                                    n_max = 0L), silent = T)) == "try-error")
if (current_sheet_missing) {
  stop("\nThe \"Robot experiments.xlsx\" file does not contain the \"CURRENT\" sheet!!!\n")
}

# Store the column names of "Robot experiments.xlsx" - sheet "CURRENT" in col_names_robot_exps:
col_names_robot_exps <- colnames(readxl::read_excel(path = paste0(pathin, "/Robot experiments.xlsx"),
                                                    sheet = "CURRENT",
                                                    .name_repair = "unique",
                                                    n_max = 0L))

# Stop if one or more required column(s) is/are missing in "Robot experiments.xlsx":
required_columns <- c("RUN DATE", "HARMONY USERNAME", "HARMONY EXPERIMENT NAME", "EXPERIMENT DURATION (MIN)", 
                      "PLATE NAME", "LOCATION", "RACK", "SLOT")

if (any(!required_columns %in% col_names_robot_exps)) {
  if (sum(!required_columns %in% col_names_robot_exps, na.rm = TRUE) == 1) {
    stop("\nColumn ", required_columns[!required_columns %in% col_names_robot_exps],
         " not present in \"Robot experiments.xlsx\" file!!!\n")
  } else {
    stop("\nColumns ", paste0(required_columns[!required_columns %in% col_names_robot_exps], collapse = ", "),
         " not present in \"Robot experiments.xlsx\" file!!!\n")
  }
}

# Retrieve the indices of required columns:
tar_col_indices <- seq_along(col_names_robot_exps)
names(tar_col_indices) <- col_names_robot_exps
tar_col_indices <- tar_col_indices[required_columns]

# Set the expected types for required columns and store pairs of column index / column type in tar_col_indices_types:
tar_col_types <- c(rep("text", 3), "numeric", rep("text", 2), rep("numeric", 2))
tar_col_indices_types <- mapply(FUN = list, ... = tar_col_indices, tar_col_types, SIMPLIFY = FALSE)

# Stop if column RUN DATE does not contain the target date:
run_date_col <- readxl::read_excel(path = paste0(pathin, "/Robot experiments.xlsx"),
                                   sheet = "CURRENT",
                                   .name_repair = "unique",
                                   range = readxl::cell_cols(tar_col_indices_types[["RUN DATE"]][[1]]),
                                   col_types = tar_col_indices_types[["RUN DATE"]][[2]],
                                   na = c("", " ", "NA", "NaN"))

run_date_col$`RUN DATE` <- gsub(x = run_date_col$`RUN DATE`,
                                pattern = "\\.", 
                                replacement = "-")

if (sum(run_date_col$`RUN DATE` == date, na.rm = TRUE) == 0) {
  stop("\nDate ", date, " not found in \"Robot experiments.xlsx\" file.\n")
}

# Store target data in tar_robot_exps (reading the required columns one by one): 
tar_robot_exps <- do.call(cbind, 
                          lapply(X = tar_col_indices_types,
                                 FUN = function(x) {
                                   readxl::read_excel(path = paste0(pathin, "/Robot experiments.xlsx"),
                                                      sheet = "CURRENT",
                                                      .name_repair = "unique",
                                                      range = readxl::cell_limits(ul = c(1, x[[1]]), 
                                                                                  lr = c(max(which(run_date_col$`RUN DATE` == date), na.rm=T)+1, x[[1]])),
                                                      col_types = x[[2]],
                                                      na = c("", " ", "NA", "NaN")) }))

tar_robot_exps$`RUN DATE` <- gsub(x = tar_robot_exps$`RUN DATE`,
                                  pattern = "\\.", 
                                  replacement = "-")

# Filter out non-target date(s) and cast column EXPERIMENT DURATION (MIN) as integer:
tar_robot_exps <- tar_robot_exps[tar_robot_exps$`RUN DATE` == date & !is.na(tar_robot_exps$`RUN DATE`),]
tar_robot_exps$`EXPERIMENT DURATION (MIN)` <- as.integer(tar_robot_exps$`EXPERIMENT DURATION (MIN)`)

# Stop if one or more required columns(s) contain(s) missing value(s):
columns_with_na <- lapply(X = tar_robot_exps, FUN = function(x) { any(is.na(x))})
columns_with_na <- columns_with_na[columns_with_na == T]

if (length(columns_with_na) > 0) {
  if (length(columns_with_na) == 1) {
    stop("\nColumn ", names(columns_with_na), " contains missing value(s), or value(s) of invalid type.\n")
  } else {
    stop("\nColumns ", paste0(names(columns_with_na), collapse = ", "), " contain missing value(s), or value(s) of invalid type.\n")
  }
}

# Stop if column HARMONY EXPERIMENT NAME contains values with comma(s):
if (any(grepl(x = tar_robot_exps$`HARMONY EXPERIMENT NAME`, pattern = ","))) {
  stop("\nColumn HARMONY EXPERIMENT NAME contains value(s) with comma(s).\n",
       "Problematic experiment name(s):\n",
       paste0(tar_robot_exps[grepl(x = tar_robot_exps$`HARMONY EXPERIMENT NAME`, pattern = ","), "HARMONY EXPERIMENT NAME"],
              collapse = "\n"), ".\n")
}

# Stop if column PLATE NAME contains values with comma(s):
if (any(grepl(x = tar_robot_exps$`PLATE NAME`, pattern = ","))) {
  stop("\nColumn PLATE NAME contains value(s) with comma(s).\n",
       "Problematic plate name(s):\n",
       paste0(tar_robot_exps[grepl(x = tar_robot_exps$`PLATE NAME`, pattern = ","), "PLATE NAME"],
              collapse = "\n"), ".\n")
}

# Stop if column PLATE NAME contains values with more than 30 characters:
if (any(nchar(tar_robot_exps$`PLATE NAME`) > 30)) {
  stop("\nColumn PLATE NAME contains value(s) with more than 30 characters.\n",
       "Problematic plate name(s):\n",
       paste0(tar_robot_exps[nchar(tar_robot_exps$`PLATE NAME`) > 30, "PLATE NAME"],
              collapse = "\n"), ".\n")
}

# Stop if column LOCATION contains invalid value(s):
tar_robot_exps$LOCATION <- toupper(tar_robot_exps$LOCATION)

if (!all(tar_robot_exps$LOCATION %in% c("RACK", "INCUBATOR"))) {
  stop("\nColumn LOCATION contains invalid value(s).\n",
       "Problematic value(s): ", paste0(tar_robot_exps[!tar_robot_exps$LOCATION %in% c("RACK", "INCUBATOR"), "LOCATION"], collapse = ", "), 
       ".\nExpected values are either \"RACK\" or \"INCUBATOR\" (case insensitive).\n")
}

# Determine if lookup files should be generated for RACK location, INCUBATOR location, or both:
if (sum(tar_robot_exps$LOCATION == "RACK", na.rm = TRUE) > 0) {
  make_rack_files <- T
  rack_tar_robot_exps <- tar_robot_exps[tar_robot_exps$LOCATION == "RACK",]
} else {
  make_rack_files <- F
}

if (sum(tar_robot_exps$LOCATION == "INCUBATOR", na.rm = TRUE) > 0) {
  make_incubator_files <- T
  incubator_tar_robot_exps <- tar_robot_exps[tar_robot_exps$LOCATION == "INCUBATOR",]
} else {
  make_incubator_files <- F
}

# Stop if columns RACK and SLOT contain invalid value(s) for RACK location:
if (make_rack_files) {
  valid_rack_rack_values = seq(4)
  valid_rack_slot_values = seq(14)
  if (any(!rack_tar_robot_exps$RACK %in% valid_rack_rack_values)) {
    stop("\nColumn RACK contains invalid value(s) for RACK location.\n",
         "Problematic value(s): ", paste0(rack_tar_robot_exps[!rack_tar_robot_exps$RACK %in% valid_rack_rack_values, "RACK"], collapse = ", "),
         ".\nRack values for RACK location must be between ", min(valid_rack_rack_values), " and ", max(valid_rack_rack_values), ".\n")
  }
  if (any(!rack_tar_robot_exps$SLOT %in% valid_rack_slot_values)) {
    stop("\nColumn SLOT contains invalid value(s) for RACK location.\n",
         "Problematic value(s): ", paste0(rack_tar_robot_exps[!rack_tar_robot_exps$SLOT %in% valid_rack_slot_values, "SLOT"], collapse = ", "),
         ".\nSLOT values for RACK location must be between ", min(valid_rack_slot_values), " and ", max(valid_rack_slot_values), ".\n")
  }
}

# Stop if columns RACK and SLOT contain invalid value(s) for INCUBATOR location:
if (make_incubator_files) {
  valid_incubator_rack_values = seq(2)
  valid_incubator_slot_values = seq(22)
  if (any(!incubator_tar_robot_exps$RACK %in% valid_incubator_rack_values)) {
    stop("\nColumn RACK contains invalid value(s) for INCUBATOR location.\n",
         "Problematic value(s): ", paste0(incubator_tar_robot_exps[!incubator_tar_robot_exps$RACK %in% valid_incubator_rack_values, "RACK"], collapse = ", "),
         ".\nRack values for INCUBATOR location must be between ", min(valid_incubator_rack_values), " and ", max(valid_incubator_rack_values), ".\n")
  }
  if (any(!incubator_tar_robot_exps$SLOT %in% valid_incubator_slot_values)) {
    stop("\nColumn SLOT contains invalid value(s) for INCUBATOR location.\n",
         "Problematic value(s): ", paste0(incubator_tar_robot_exps[!incubator_tar_robot_exps$SLOT %in% valid_incubator_slot_values, "SLOT"], collapse = ", "),
         ".\nSLOT values for INCUBATOR location must be between ", min(valid_incubator_slot_values), " and ", max(valid_incubator_slot_values), ".\n")
  }
}

# Stop if one or more combination(s) of LOCATION, RACK and SLOT is/are not unique:
non_unique_lrs_combi <- tar_robot_exps[, c("LOCATION", "RACK", "SLOT")]

if (length(non_unique_lrs_combi[[1]]) > length(unique(non_unique_lrs_combi)[[1]])) {
  
  non_unique_lrs_combi$lrs <- paste(non_unique_lrs_combi$LOCATION, non_unique_lrs_combi$RACK, non_unique_lrs_combi$SLOT)
  
  non_unique_lrs_combi$lrs_count <- vapply(X = seq_along(non_unique_lrs_combi$lrs),
                                           FUN = function(x) { sum(non_unique_lrs_combi$lrs == non_unique_lrs_combi$lrs[[x]], na.rm = TRUE) }, 
                                           FUN.VALUE = NA_integer_)
  
  non_unique_lrs_combi <- unique(non_unique_lrs_combi[non_unique_lrs_combi$lrs_count > 1L, "lrs"])
  
  non_unique_lrs_combi <- strsplit(x = non_unique_lrs_combi, split = " ")
  
  non_unique_lrs_combi <- lapply(X = non_unique_lrs_combi,
                                 FUN = function(x) {
                                   paste0("Location: ", x[1],
                                          ", Rack: ", x[2],
                                          ", Slot: ", x[3])})
  stop("\nThe following combination(s) of LOCATION, RACK and SLOT are not unique:\n",
       paste0(non_unique_lrs_combi, collapse = "\n"), "\n")
}

# Write lookup files ------------------------------------------------------

# Define function to write Protocols files(s):
mk_protocol_file <- function(data, file_name) {
  
  data <- data[data$`RUN DATE` == date, c("HARMONY USERNAME", "HARMONY EXPERIMENT NAME", "EXPERIMENT DURATION (MIN)")]
  
  names(data) <- c("ExperimentOwner", "ExperimentName", "Duration")
  
  data$Barcode <-  rep("unknown", dim(data)[1])
  
  data <- data[, c("Barcode", "ExperimentOwner", "ExperimentName", "Duration")]
  
  write.csv(x = data, file = paste0(pathout, file_name), row.names = F)

}

# Define function to write Inventory files(s):
mk_inventory_file <- function(data, file_name) {
  
  data <- data.frame(col1 = data$`PLATE NAME`,
                     col2 = NA,
                     Col3 = NA,
                     Col4 = 1L,
                     Col5 = NA,
                     Col6 = NA,
                     Col7 = NA,
                     Col8 = data$RACK,
                     Col9 = data$SLOT)
  
  write.table(x = data, file = paste0(pathout, file_name),
              row.names = F, col.names = F, sep = ",", na = "", quote = F)
  
}

# Define function to write Process files(s):
mk_process_file <- function(data, file_name) {
  
  data <- data.frame(col1 = data$`PLATE NAME`)
  
  write.table(x = data, file = paste0(pathout, file_name),
              row.names = F, col.names = F, sep = "\t", na = "", quote = F)
  
}

# Write lookup files for RACK location:
if (make_rack_files == T) {
  mk_protocol_file(data = rack_tar_robot_exps, file_name = "Protocols_Rack.csv")
  mk_inventory_file(data = rack_tar_robot_exps, file_name = "Inventory_HCS_Rack.csv")
  mk_process_file(data = rack_tar_robot_exps, file_name = "Process_HCS_Rack.txt")
  message(paste0("\n---> Lookup files for RACK location saved in \"", pathout, "\" folder."))
}

# Write lookup files for INCUBATOR location:
if (make_incubator_files == T) {
  mk_protocol_file(data = incubator_tar_robot_exps, file_name = "Protocols_Incubator.csv")
  mk_inventory_file(data = incubator_tar_robot_exps, file_name = "Inventory_HCS_Incubator.csv")
  mk_process_file(data = incubator_tar_robot_exps, file_name = "Process_HCS_Incubator.txt")
  message(paste0("\n---> Lookup files for INCUBATOR location saved in \"", pathout, "\" folder."))
}

# Print closing message:
message("\n--------------------", 
        "\nSummary:\n",
        "Run date: ", date, "\n",
        "Number of rack plates: ", ifelse(make_rack_files, nrow(rack_tar_robot_exps), 0L), "\n",
        "Number of incubator plates: ", ifelse(make_incubator_files, nrow(incubator_tar_robot_exps), 0L), "\n",
        "User(s): ", paste0(unique(tar_robot_exps$`HARMONY USERNAME`), collapse = ", "), "\n",
        "Estimated total run time: ", round(sum(tar_robot_exps$`EXPERIMENT DURATION (MIN)`, na.rm = TRUE) / 60, 1), " hours\n",
        "--------------------\n")
