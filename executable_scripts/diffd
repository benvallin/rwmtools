#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

# Print help --------------------------------------------------------------

if (length(args) == 1L && args == "--help") { 
  message(
  "\nNAME
  diffd -- Calculate days or dates of differentiation of iPSCs into DaNs
  
  SYNOPSIS
  diffd -rdate -rday [OPTION]...
  diffd --help
  diffd --TSR
  
  DESCRIPTION
  diffd prints the value of a target differentiation day or date to standard output, provided a reference date and a reference day. 
  If requested, a differentiation calendar is generated and written to file.
  
  The following options are available:
  
  -rdate Reference date of differentiation matching the reference day. 
         Valid values are in the form YYYY-MM-DD or YYYY.MM.DD. 
         Alternatively, the string \"today\" can be provided and will be converted into the system's idea of the current date.
  
  -rday Reference day of differentiation matching the reference date. 
        Valid values include any integer.
  
  -tdate Target date for which a differentiation day is requested. 
         Valid values are in the form YYYY-MM-DD or YYYY.MM.DD. 
         Alternatively, the string \"today\" can be provided and will be converted into the system's idea of the current date.
         
  -tday Target day for which a differentiation date is requested. 
        Valid values include any integer.
        
  --exp Logical indicating if an expansion phase should be included. 
        Valid values include TRUE, T, FALSE, and F. Default is TRUE.
        
  --expdur Numerical indicating the duration of the expansion phase in days. 
           Valid values include any positive integer. Default is 21.
           
  --cal Logical indicating if a differentiation calendar should be printed to standard output. 
        Valid values include TRUE, T, FALSE, and F. Default is FALSE.
       
  --write Absolute or relative path indicating the directory where to write the differentiation calendar. 
          The path must lead to an existing directory. It cannot be used in combination with --cal = FALSE.
          
  --help Show this screen.
  
  --TSR Try it.
  
  EXAMPLES
  diffd --help
  diffd -rdate 2022.02.14 -rday 10+17 -tday 60
  diffd -rdate 2022.02.14 -rday -2 -tday 45 --expdur 23
  diffd -rdate 2022.04.10 -rday 61 -tdate 2022.02.18 --exp F
  diffd -rdate today -rday 29 -tdate 2022.05.01 --cal T
  diffd -rdate 2022.05.10 -rday 0 -tday 70 --write ~/Desktop\n"
  )
  quit(save = "no")
}

# Launch Hugo TSR --------------------------------------------------------------

if (length(args) == 1L && args == "--TSR") { 
  message("\nHere is some real music...\n")
  browseURL("https://www.youtube.com/watch?v=WSzVWijz04Y")
  quit(save = "no")
}

# Manage invalid command line arguments -----------------------------------

# Stop if no argument is provided:
if (length(args) == 0L) { 
  stop(
  "No option was provided.
  Please invoke -rdate, -rday and -tdate or -tday to get a target differentiation day or date. 
  Provide at least -rdate and -rday and set --cal = TRUE to generate a differentiation calendar.
  Use the --help option to get help.\n"
  )
}

# Stop if only 1 option name which is neither --help nor --TSR is provided without value:
if ((length(args) == 1L) && (!args %in% c("--help", "--TSR"))) { 
  if (args %in% c("-rdate", "-rday", "-tdate", "-tday", "--exp", "--expdur", "--cal", "--write")) {
    stop(args, " option must be given a value and cannot be used on its own.\n") 
    } else {
      stop(args, " is not a valid option.\n") 
    }
}

# Stop if --help and/or --TSR option is misused:
if ((length(args) > 1L) && (sum(args %in% c("--help", "--TSR")) > 0L)) {
  if (sum(args %in% c("--help", "--TSR")) < 2L) {
    if (any(args == "--help")) {
      stop("--help option must be invoqued on its own.\n")
      } else {
        stop("--TSR option must be invoqued on its own.\n")
        }
    } else {
      stop("--help and --TSR options must be invoqued on their own.\n") 
    }
}

# Stop if the number of option name / option value pairs is invalid:
if (!length(args) %in% c(1L, 6L, 8L, 10L, 12L, 14L)) { 
  stop(
  "Invalid number of option name / option value pairs.
  Please invoke -rdate, -rday and -tdate or -tday to get a target differentiation day or date. 
  Provide at least -rdate and -rday and set --cal = TRUE to generate a differentiation calendar.
  Use the --help option to get help.\n"
  )
}

# Check the validity of option names:
arg.table <- data.frame(arg.nm = args[seq.int(from = 1L, to = (length(args)-1L), by = 2L)],
                        arg.val = args[seq.int(from = 2L, to = length(args), by = 2L)])

arg.table$arg.nm.valid <- arg.table$arg.nm %in% c("-rdate", "-rday", "-tdate", "-tday", "--exp", "--expdur", "--cal", "--write")

# Stop if one or more option name is invalid:
if (!all(arg.table$arg.nm.valid)) {
  if (length(arg.table[arg.table$arg.nm.valid == FALSE, "arg.nm"]) == 1L) {
    stop(arg.table[arg.table$arg.nm.valid == FALSE, "arg.nm"], " is not a valid option.\n")
    } else {
      stop(paste0(arg.table[arg.table$arg.nm.valid == FALSE, "arg.nm"], collapse = ", "), " are not valid options.\n")
    }
}

# Stop if one or more option name is provided more than one time:
arg.table$arg.nm.unique <- lapply(X = arg.table$arg.nm,
                                  FUN = function (var) { if (length(grep(pattern = paste0("^", var, "$"), x = arg.table$arg.nm)) == 1L) { TRUE } else { FALSE }})

if (length(unique(arg.table[arg.table$arg.nm.unique == FALSE, "arg.nm"])) > 0L) {
  if (length(unique(arg.table[arg.table$arg.nm.unique == FALSE, "arg.nm"])) == 1L) {
    stop(unique(arg.table[arg.table$arg.nm.unique == FALSE, "arg.nm"]), " option must be invoqued only once.\n")
    } else {
      stop(paste0(unique(arg.table[arg.table$arg.nm.unique == FALSE, "arg.nm"]), collapse = ", "), " options must be invoqued only once.\n")
    }
}

# Check the validity of option values:
arg.table$arg.val.valid <- vector(mode = "logical", length = length(arg.table$arg.val))

# => Check -rdate and -tdate options values:
for (i in c("-rdate", "-tdate")) {
  if (any(arg.table$arg.nm == i)) {
    arg.table[arg.table$arg.nm == i, "arg.val"] <- gsub(x = arg.table[arg.table$arg.nm == i, "arg.val"], pattern = "\\.", replacement = "-")
    if (arg.table[arg.table$arg.nm == i, "arg.val"] == "today") {
      arg.table[arg.table$arg.nm == i, "arg.val"] <- as.character(Sys.Date())
    }
    if (grepl(x = arg.table[arg.table$arg.nm == i, "arg.val"], pattern = "^\\d{4}-\\d{2}-\\d{2}$")) {
      arg.table[arg.table$arg.nm == i, "arg.val.valid"] <- TRUE
    } else {
      arg.table[arg.table$arg.nm == i, "arg.val.valid"] <- FALSE
    }
  }
}

# => Check -rday and -tday options values:
for (i in c("-rday", "-tday")) {
  if (any(arg.table$arg.nm == i)) {
    if (!grepl(x = arg.table[arg.table$arg.nm == i, "arg.val"], pattern = "\\+")) {
      if (grepl(x = arg.table[arg.table$arg.nm == i, "arg.val"], pattern = "^-?\\d+$")) {
        arg.table[arg.table$arg.nm == i, "arg.val.valid"] <- TRUE
      } else {
        arg.table[arg.table$arg.nm == i, "arg.val.valid"] <- FALSE
      }
    } else {
      if (grepl(x = arg.table[arg.table$arg.nm == i, "arg.val"], pattern = "^10\\+\\d+$")) {
        arg.table[arg.table$arg.nm == i, "arg.val.valid"] <- TRUE
      } else {
        arg.table[arg.table$arg.nm == i, "arg.val.valid"] <- FALSE
      }
    }
  }
}

# => Check --exp option value
if (any(arg.table$arg.nm == "--exp")) {
  arg.table[arg.table$arg.nm == "--exp", "arg.val.valid"] <- arg.table[arg.table$arg.nm == "--exp", "arg.val"] %in% c("TRUE", "T", "FALSE", "F") 
} 

# => Check --expdur option value
if (any(arg.table$arg.nm == "--expdur")) {
  arg.table[arg.table$arg.nm == "--expdur", "arg.val.valid"] <- grepl(x = arg.table[arg.table$arg.nm == "--expdur", "arg.val"], pattern = "^[1-9][0-9]*$")
}

# => Check --cal option value
if (any(arg.table$arg.nm == "--cal")) {
  arg.table[arg.table$arg.nm == "--cal", "arg.val.valid"] <- arg.table[arg.table$arg.nm == "--cal", "arg.val"] %in% c("TRUE", "T", "FALSE", "F") 
} 

# => Check --write option value
if (any(arg.table$arg.nm == "--write")) {
  arg.table[arg.table$arg.nm == "--write", "arg.val.valid"] <- dir.exists(arg.table[arg.table$arg.nm == "--write", "arg.val"])
} 

# Stop if one or more option value is invalid:
if (length(arg.table[arg.table$arg.val.valid == FALSE, "arg.nm"]) > 0L) {
  if (length(arg.table[arg.table$arg.val.valid == FALSE, "arg.nm"]) == 1L) {
    if (arg.table[arg.table$arg.val.valid == FALSE, "arg.nm"] == "--write") {
      stop("Cannot write calendar to disk: ", 
           arg.table[arg.table$arg.val.valid == FALSE, "arg.val"], 
           " directory does not exist.\n")
      } else {
        stop(arg.table[arg.table$arg.val.valid == FALSE, "arg.val"], 
             " is not a valid value for ",
             arg.table[arg.table$arg.val.valid == FALSE, "arg.nm"], 
             " option.\n")
      }
    } else {
      stop(paste0(arg.table[arg.table$arg.val.valid == FALSE, "arg.val"], collapse = ", "), 
           " are not valid values for ", 
           paste0(arg.table[arg.table$arg.val.valid == FALSE, "arg.nm"], collapse = ", "), 
           " options, respectively.\n")
    }
}

# Resolve the value of unprovided optional arguments and manage conflicts -----------

# Define character vector "missing.args".
# missing.args = all the valid options which are not provided by user:
missing.args <- setdiff(c("-rdate", "-rday", "-tdate", "-tday", "--exp", "--expdur", "--cal", "--write"), arg.table$arg.nm)

# Define character vector "missing.refs".
# missing.refs = the reference options (-rdate and -tdate) which are not provided by user:
missing.refs <- missing.args[missing.args %in% c("-rdate", "-rday")]

# Stop if one or more reference option (-rdate and -tdate) is not provided:
if (length(missing.refs) > 0L) {
  if (length(missing.refs) == 1L) {
    stop(missing.refs, " option was not provided.\n")
    } else {
      stop("-rdate and -rday options were not provided.\n")
    }
}

# Define logical vector "write".
# write = FALSE if --write option is not provided by user.
# write = TRUE if --write option is provided by user.
write <- ifelse(any(missing.args == "--write"), FALSE, TRUE)

# Define logical vector "cal".
# cal = FALSE if neither --cal nor --write option is provided by user.
# cal = TRUE if user does not provide --cal option but provides --write option.
# cal = --cal option value if provided by user.
# Stop if user provides --cal = FALSE and a --write option value:
if (any(missing.args == "--cal")) {
  cal <- ifelse(isFALSE(write), FALSE, TRUE)
  } else {
    cal <- as.logical(arg.table[arg.table$arg.nm == "--cal", "arg.val"])
    if (isFALSE(cal) && isTRUE(write)) { 
      stop("--cal = FALSE but a write path was provided.\nIf you want to write a calendar to disk but hide it from standard output, simply provide the --write option and omit the --cal option.\n")
    }
  }

# Define character vector "target".
# target = the target options (-rdate and -tdate) which are not provided by user:
target <- missing.args[missing.args %in% c("-tdate", "-tday")]
  
# Stop if neither -tdate nor -tday option is provided by user and cal = FALSE:
if (length(target) == 2L && isFALSE(cal)) {
  stop("-tdate and -tday options were not provided and --cal = FALSE. Please provide either -tdate or -tday, or set --cal = TRUE.\n")
}

# Stop if both -tdate and -tday options are provided by user:
if (length(target) == 0L) {
  stop("Please provide either -tdate or -tday option, not both. Alternatively, provide none but set --cal = TRUE.\n")
}

# Define logical vector "exp".
# exp = TRUE if --exp option is not provided by user.
# exp = FALSE if user explicitly set --exp = FALSE.
# Stop if user provides --exp = FALSE and a --expdur option value:
if (any(missing.args == "--exp")) {
  exp <- TRUE
  } else {
    exp <- as.logical(arg.table[arg.table$arg.nm == "--exp", "arg.val"])
    if (isFALSE(exp) && isFALSE(any(missing.args == "--expdur"))) {
      stop("--exp = FALSE but an expansion duration was provided.\n")
    }
  }

# Stop if exp = FALSE but the value of -rday option is in expansion style format:
if (isFALSE(exp) && isTRUE(grepl(x = arg.table[arg.table$arg.nm == "-rday", "arg.val"], pattern = "^10\\+\\d+$"))) {
  stop("--exp = FALSE but the value of -rday was provided in expansion style format.\n")
}

# Stop if exp = FALSE but the value of -tday option is in expansion style format:
if (length(target) == 1 && target == "-tdate") {
  if (isFALSE(exp) && isTRUE(grepl(x = arg.table[arg.table$arg.nm == "-tday", "arg.val"], pattern = "^10\\+\\d+$"))) {
    stop("--exp = FALSE but the value of -tday was provided in expansion style format.\n")
  }
}

# Define integer vector "expdur" if exp = TRUE.
# expdur = 21L if the --expdur option is not provided by user.
# expdur = --expdur option value if provided by user:
if (isTRUE(exp)) {
  expdur <- ifelse(any(missing.args == "--expdur"), 
                   21L, 
                   as.integer(arg.table[arg.table$arg.nm == "--expdur", "arg.val"]))
}

# Stop if user provides a rday and/or tday in expansion phase with a value higher than that of the expansion phase duration:
for (i in c("-rday", "-tday")) {
  if (isTRUE(grepl(x = arg.table[arg.table$arg.nm == i, "arg.val"], pattern = "^10\\+\\d+$"))) {
    if(as.integer(sub(x = arg.table[arg.table$arg.nm == i, "arg.val"], pattern = "^10\\+", replacement = "")) > expdur) {
      stop("Provided ", i, " is in expansion phase but its value is superior to that of expansion phase duration")
    }
  }
}

# Resolve and return the value of target option if provided --------------------------------------

# If a target option is provided and exp = TRUE, define logical vectors "rday.inexp" and "rday.preexp" indicating if rday is in / before expansion phase:
if (length(target) == 1 && isTRUE(exp)) {
  rday.inexp <- ifelse(grepl(x = arg.table[arg.table$arg.nm == "-rday", "arg.val"], pattern = "\\+"), 
                       TRUE, 
                       FALSE)
  
  rday.preexp <- ifelse(isTRUE(rday.inexp),
                        FALSE,
                        ifelse(as.integer(arg.table[arg.table$arg.nm == "-rday", "arg.val"]) <= 10L, TRUE, FALSE))
}

# Case when target = tday (-tdate option was provided)
if (length(target) == 1 && target == "-tday") {
  
  # If exp = TRUE:
  if (isTRUE(exp)) {
    # Define numerical vector "temp.tday".
    # temp.tday = temporary numerical value of tday with expansion days added to the total.
    # If -rday is before expansion phase:
    if (isTRUE(rday.preexp)) {
      temp.tday <- as.integer(as.integer(arg.table[arg.table$arg.nm == "-rday", "arg.val"]) + (as.Date(arg.table[arg.table$arg.nm == "-tdate", "arg.val"]) - as.Date(arg.table[arg.table$arg.nm == "-rdate", "arg.val"])))
      # If -rday is during expansion phase: 
      } else if (isTRUE(rday.inexp)) {
        temp.tday <- as.integer((10L + as.integer(sub(x = arg.table[arg.table$arg.nm == "-rday", "arg.val"], pattern = "^10\\+", replacement = ""))) + (as.Date(arg.table[arg.table$arg.nm == "-tdate", "arg.val"]) - as.Date(arg.table[arg.table$arg.nm == "-rdate", "arg.val"])))
        # If -rday is after expansion phase:
        } else {
          temp.tday <- as.integer(as.integer(arg.table[arg.table$arg.nm == "-rday", "arg.val"]) + (as.Date(arg.table[arg.table$arg.nm == "-tdate", "arg.val"]) - as.Date(arg.table[arg.table$arg.nm == "-rdate", "arg.val"])) + expdur)
        }
    
    # Define character vector "tday".
    # tday = value of requested tday (follows the rules of expansion style format).
    # If requested -tday is before expansion phase:
    if (temp.tday <= 10L) {
      tday <- as.character(temp.tday)
      # If requested -tday is during expansion phase:
      } else if (temp.tday > 10L & temp.tday <= (10L + expdur)) {
        tday <- paste0("10+", temp.tday - 10L)
        # If requested -tday is after expansion phase:
        } else {
          tday <- as.character(temp.tday - expdur)
        }
    
    # If exp = FALSE:
    } else {
      # Define character vector "tday".
      # tday = value of requested tday (follows the rules of expansion style format).
      tday <- as.character(as.integer(arg.table[arg.table$arg.nm == "-rday", "arg.val"]) + (as.Date(arg.table[arg.table$arg.nm == "-tdate", "arg.val"]) - as.Date(arg.table[arg.table$arg.nm == "-rdate", "arg.val"])))
    }
  arg.table <- rbind(arg.table[, c("arg.nm", "arg.val")], data.frame(arg.nm = "-tday", arg.val = tday))
  
  # Return target day to standard output:
  message("\nYour target day is ", tday, ".\n")
}

# Case when target = tdate (-tday option was provided):
if (length(target) == 1 && target == "-tdate") {
  
  # If exp = TRUE:
  if (isTRUE(exp)) {
    # Define logical vectors "tday.inexp" and "tday.preexp" indicating if tday is in / before expansion phase:
    tday.inexp <- ifelse(grepl(x = arg.table[arg.table$arg.nm == "-tday", "arg.val"], pattern = "\\+"), 
                         TRUE, 
                         FALSE)
    
    tday.preexp <- ifelse(isTRUE(tday.inexp),
                          FALSE,
                          ifelse(as.integer(arg.table[arg.table$arg.nm == "-tday", "arg.val"]) <= 10L, TRUE, FALSE))
    
    # Define numerical vector "temp.rday".
    # temp.rday = temporary numerical value of rday with expansion days added to the total.
    # If -rday is before expansion phase:
    if (isTRUE(rday.preexp)) {
      temp.rday <- as.integer(arg.table[arg.table$arg.nm == "-rday", "arg.val"])
      # If -rday is during expansion phase:
      } else if (isTRUE(rday.inexp)) {
        temp.rday <- as.integer((10L + as.integer(sub(x = arg.table[arg.table$arg.nm == "-rday", "arg.val"], pattern = "^10\\+", replacement = ""))))
        # If -rday is after expansion phase:
        } else {
          temp.rday <- as.integer(arg.table[arg.table$arg.nm == "-rday", "arg.val"]) + expdur
        }
    
    # Define numerical vector "temp.tday".
    # temp.tday = temporary numerical value of tday with expansion days added to the total.
    # If -tday is before expansion phase:
    if (isTRUE(tday.preexp)) {
      temp.tday <- as.integer(arg.table[arg.table$arg.nm == "-tday", "arg.val"])
      # If -tday is during expansion phase:
      } else if (isTRUE(tday.inexp)) {
        temp.tday <- as.integer((10L + as.integer(sub(x = arg.table[arg.table$arg.nm == "-tday", "arg.val"], pattern = "^10\\+", replacement = ""))))
        # If -tday is after expansion phase:
        } else {
          temp.tday <- as.integer(arg.table[arg.table$arg.nm == "-tday", "arg.val"]) + expdur
        }
    
    # Define character vector "tdate".
    # tdate = value of requested tdate.
    tdate <- as.character(as.Date(arg.table[arg.table$arg.nm == "-rdate", "arg.val"]) + (temp.tday - temp.rday))
    
    # If exp = FALSE:
    } else {
      # Define character vector "tdate".
      # tdate = value of requested tdate.
      tdate <- as.character(as.Date(arg.table[arg.table$arg.nm == "-rdate", "arg.val"]) + (as.integer(arg.table[arg.table$arg.nm == "-tday", "arg.val"]) - as.integer(arg.table[arg.table$arg.nm == "-rday", "arg.val"])))
    }
  arg.table <- rbind(arg.table[, c("arg.nm", "arg.val")], data.frame(arg.nm = "-tdate", arg.val = tdate))
  
  # Return target date to standard output:
  message("\nYour target date is ", tdate, ".\n")
}

# Stop execution here if a differentiation calendar is not requested:
if (isFALSE(cal)) { quit(save = "no") }

# Generate and write to file differentiation calendar if requested ---------------------------------------

# If a differentiation calendar is requested (--cal = TRUE and/or --write option is provided):
if(isTRUE(cal)) {
  
  # Define numerical vectors "sday" and "eday".
  # sday = start day of the calendar. It is the lower value between -2, rday and tday (if provided):
  sday <- min(c(-2L,
                as.integer(sub(x = arg.table[arg.table$arg.nm == "-rday", "arg.val"], pattern = "\\+\\d+$", replacement = "")),
                as.integer(sub(x = arg.table[arg.table$arg.nm == "-tday", "arg.val"], pattern = "\\+\\d+$", replacement = ""))))
  
  # eday = end day of the calendar. It is the higher value between 60, rday and tday (if provided):
  eday <- max(c(60L,
                as.integer(sub(x = arg.table[arg.table$arg.nm == "-rday", "arg.val"], pattern = "\\+\\d+$", replacement = "")),
                as.integer(sub(x = arg.table[arg.table$arg.nm == "-tday", "arg.val"], pattern = "\\+\\d+$", replacement = ""))))
  
  # Define table "calendar".
  # The calendar is composed of a single column "diffday" indicating the days of differentiation.
  # If exp = TRUE, the calendar includes a number of expansion days equal to the duration of the expansion phase:
  if(isTRUE(exp)) {
    calendar <- data.frame(diffday = as.character(c(seq.int(from = sday, to = 10L, by = 1L),
                                                    paste0("10+", seq.int(from = 1L, to = expdur, by = 1L)),
                                                    seq.int(from = 11L, to = eday, by = 1L))))
    } else {
      calendar <- data.frame(diffday = as.character(c(seq.int(from = sday, to = eday, by = 1L))))
    }
  
  # Add a second column "diffdate" to the calendar, to be filled with dates corresponding to the differentiation days.
  # The diffdate column is initialised with the provided rdate only: 
  calendar[calendar$diffday == arg.table[arg.table$arg.nm == "-rday", "arg.val"], "diffdate"] <- arg.table[arg.table$arg.nm == "-rdate", "arg.val"]
  
  # Define date vectors "before.rdate" and "after.rdate".
  # before.rdate = ranges from date of sday to date of rday:
  before.rdate <- seq.Date(from = as.Date(arg.table[arg.table$arg.nm == "-rdate", "arg.val"]) - (match(x = arg.table[arg.table$arg.nm == "-rdate", "arg.val"], table = calendar$diffdate) - 1L),
                           to = as.Date(arg.table[arg.table$arg.nm == "-rdate", "arg.val"]),
                           by = "day")
  
  # after.rdate = ranges from date of rday+1 to date of eday:
  after.rdate <- seq.Date(from = as.Date(arg.table[arg.table$arg.nm == "-rdate", "arg.val"]) + 1L,
                          length.out = nrow(calendar) - match(x = arg.table[arg.table$arg.nm == "-rdate", "arg.val"], table = calendar$diffdate),
                          by = "day")
  
  # Fill out the diffdate column of the calendar with the differentiation dates before and after rdate:
  calendar$diffdate <- c(before.rdate, after.rdate)
  
  # If user provides --cal = TRUE, print the differentiation calendar to standard output:
  if (isTRUE(as.logical(arg.table[arg.table$arg.nm == "--cal", "arg.val"]))) {
    if (length(target) == 1L) {
      message("See below your differentiation calendar:\n")
      } else {
        message("\nSee below your differentiation calendar:\n")
      }
    print(calendar)
  }
  
  # If the --write option is provided, write the differentiation calendar to file:
  if (isTRUE(write)) {
    
    write.csv(x = calendar,
              file = paste0(arg.table[arg.table$arg.nm == "--write", "arg.val"], 
                            "/diffd.calendar_", 
                            gsub(x = gsub(x = gsub(x = Sys.time(), pattern = ":", replacement = "."), pattern = "\\s", replacement = "_"), pattern = "-", replacement = "."), 
                            ".csv"),
              row.names = F)
    
    if (isTRUE(as.logical(arg.table[arg.table$arg.nm == "--cal", "arg.val"])) || length(target) == 2L) {
      message("\nDifferentiation calendar was written in ", arg.table[arg.table$arg.nm == "--write", "arg.val"], " directory.\n")
      } else {
        message("Differentiation calendar was written in ", arg.table[arg.table$arg.nm == "--write", "arg.val"], " directory.\n")
      }
    } else {
      message("\n")
    }
}


