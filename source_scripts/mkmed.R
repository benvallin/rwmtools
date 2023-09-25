#!/usr/bin/env Rscript

options(warn = -1)

args <- commandArgs(trailingOnly = TRUE)

# Print help --------------------------------------------------------------

if (length(args) == 1L && args == "--help") { 
  message(
  "\nNAME
  mkmed -- Make culture media for differentiation of iPSCs into DaNs
  
  SYNOPSIS
  mkmed -d -v [OPTION]...
  mkmed --help
  mkmed --ko
  mkmed --mtesr
  mkmed --nb
  mkmed --nnb
  
  DESCRIPTION
  mkmed prints the composition of your culture medium to standard output, provided the day of iPSC differentiation (Kriks protocol) and the required volume. 
  If requested, medium composition also includes ROCKi and AntiAnti. 
  Alternatively, mkmed can be used to print the recipe of basal media (mTeSR, KO DMEM KSR, NNB and NB).
  
  The following options are available:
  
  -d  Numerical indicating the day of differentiation protocol. 
      Valid values include any positive integer, including 0.
      
  -v  Numerical indicating the required volume of culture medium in milliliter. 
      Valid values include any positive double, excluding 0.
      
  --split Logical indicating if ROCKi should be included. 
          Valid values include TRUE, T, FALSE, and F. 
          Note that --split = TRUE is not applied is -d > 20.
          
  --antianti  Logical indicating if Anti-Anti should be included. 
              Valid values include TRUE, T, FALSE, and F.
              
  --mtesr Print the recipe of mTeSR.
  
  --ko  Print the recipe of KO DMEM KSR.
  
  --nnb Print the recipe of NNB.
  
  --nb  Print the recipe of NB.
  
  --help  Show this screen.
  
  EXAMPLES
  mkmed --help
  mkmed --mtesr
  mkmed -d 0 -v 25
  mkmed -d 10 -v 130 --split TRUE
  mkmed -d 22 -v 200
  mkmed -d 45 -v 200 --antianti TRUE\n"
  )
  quit(save = "no")
}

# Print the recipe of basal media -----------------------------------------

# Case of mTeSR:

if (length(args) == 1L && args == "--mtesr") {
  message(
  "\nHere is the recipe of mTeSR:
  - 400 ml of mTeSR Basal Medium (a full new bottle)
  - 100 ml of mTeSR 5X Supplement (1:5; 1X final)
  - 5 ml of 100X penicillin-streptomycin solution (1:100; 1X final)\n"
  )
  quit(save = "no")
}

# Case of KO DMEM KSR:

if (length(args) == 1L && args == "--ko") {
  message(
  "\nHere is the recipe of KO DMEM KSR:
  - 410 ml of KO DMEM Medium (remove 90 ml from a new 500 ml bottle)
  - 75 ml of 100% KnockOut Serum Replacement (15% final)
  - 5 ml of 1mM beta-mercaptoethanol (1:100; 10 uM final)
  - 5 ml of 200 mM L-glutamine (1:100; 2 mM final)
  - 5 ml of 100X MEM Non-Essential Amino Acids solution (1:100; 1X final)\n"
  )
  quit(save = "no")
}

# Case of NNB:

if (length(args) == 1L && args == "--nnb") {
  message(
  "\nHere is the recipe of NNB:
  - 487.5 ml of Neurobasal Medium (remove 12.5 ml from a new 500 ml bottle)
  - 2.5 ml of 100X N2 Supplement (1:200; 0.5X final)
  - 5 ml of 50X B-27 Supplement without vitamin A (1:100; 0.5X final)
  - 5 ml of 200 mM L-glutamine (1:100; 2 mM final)\n"
  )
  quit(save = "no")
}

# Case of NB:

if (length(args) == 1L && args == "--nb") {
  message(
  "\nHere is the recipe of NB:
  - 485 ml of Neurobasal Medium (remove 15 ml from a new 500 ml bottle)
  - 10 ml of 50X B-27 Supplement without vitamin A (1:50; 1X final)
  - 5 ml of 200 mM L-glutamine (1:100; 2 mM final)\n"
  )
  quit(save = "no")
}

# Manage invalid command line arguments -----------------------------------

# Case when no option was provided:

if (length(args) == 0L) { 
  stop(
  "No option was provided.
  Please invoke:
  - at least -d and -v with their respective values to get the composition of a particular medium.
  - one of --ko, --mtesr, --nnb, or --nb to get the recipe of a basal medium.
  - the --help option to get help.\n"
  ) 
}

# Case when only 1 option which is not one of --help, --mtesr, --ko, --nnb, or --nb was provided without value:

if ((length(args) == 1L) && (!args %in% c("--help", "--ko", "--mtesr", "--nnb", "--nb"))) { 
  if (args %in% c("-d", "-v", "--split", "--antianti")) {
    stop(args, " option must be given a value and cannot be used on its own.\n") 
    } else {
      stop(args, " is not a valid option.\n") 
    }
}

# Case when --help, --mtesr, --ko, --nnb, or --nb options were misused:

if ((length(args) > 1L) && (any(args %in% c("--help", "--ko", "--mtesr", "--nnb", "--nb")))) { stop("--help, --ko, --mtesr, --nnb and --nb options must be invoqued on their own.\n") }

# Case when more than 1 option was provided but the rule of 2, 3 or 4 option-value pairs was not respected:

if (!length(args) %in% c(1L, 4L, 6L, 8L)) { 
  stop(
  "Invalid number of options / option-value pairs.
  Please provide:
  - at least -d and -v with their respective values to get the composition of a particular medium.
  - either --ko, --mtesr, --nnb, or --nb on its own to get the recipe of a basal medium.
  - the --help option on its own to get help.\n"
  ) 
}

# Case when one or more option name is invalid:

basic.checks <- data.frame(arg.nm = args[seq.int(from = 1, to = (length(args)-1), by = 2)],
                           arg.val = args[seq.int(from = 2, to = length(args), by = 2)])

basic.checks$arg.nm.valid <- basic.checks$arg.nm %in% c("-d", "-v", "--split", "--antianti")

if (!all(basic.checks$arg.nm.valid)) {
  invalid.arg.nm <- basic.checks[basic.checks$arg.nm.valid == FALSE, "arg.nm"]
  if (length(invalid.arg.nm ) == 1L) {
    stop(invalid.arg.nm, " is not a valid option.\n")
    } else {
      stop(paste0(invalid.arg.nm, collapse = ", "), " are not valid options.\n")
    }
}

# Case when one or more option name is provided more than one time:

basic.checks$arg.nm.unique <- lapply(X = basic.checks$arg.nm,
                                     FUN = function (var) { if (length(grep(pattern = var, x = basic.checks$arg.nm)) == 1) { TRUE } else { FALSE }})

repeated.arg.nm <- unique(basic.checks[basic.checks$arg.nm.unique == FALSE, "arg.nm"])

if (length(repeated.arg.nm) > 0) {
  if (length(repeated.arg.nm) == 1) {
    stop(repeated.arg.nm, " option must be invoqued only once.\n")
    } else {
      stop(paste0(repeated.arg.nm, collapse = ", "), " options must be invoqued only once.\n")
    }
}

# Case when one or more option value is invalid:

basic.checks$arg.val.valid <- vector(mode = "logical", length = length(basic.checks$arg.val))
basic.checks[basic.checks$arg.nm == "-d", "arg.val.valid"] <- grepl(x = basic.checks[basic.checks$arg.nm == "-d", "arg.val"], pattern = "^(0|[1-9]+\\d*)$")
basic.checks[basic.checks$arg.nm == "-v", "arg.val.valid"] <- ifelse(is.na(as.numeric(basic.checks[basic.checks$arg.nm == "-v", "arg.val"])) || as.numeric(basic.checks[basic.checks$arg.nm == "-v", "arg.val"]) <= 0L, FALSE, TRUE)
basic.checks[basic.checks$arg.nm == "--split", "arg.val.valid"] <- basic.checks[basic.checks$arg.nm == "--split", "arg.val"] %in% c("TRUE", "T", "FALSE", "F")
basic.checks[basic.checks$arg.nm == "--antianti", "arg.val.valid"] <- basic.checks[basic.checks$arg.nm == "--antianti", "arg.val"] %in% c("TRUE", "T", "FALSE", "F")       

invalid.arg.val <- basic.checks[basic.checks$arg.val.valid == FALSE, c("arg.nm", "arg.val")]

if (length(invalid.arg.val[[1]]) > 0L) {
  if (length(invalid.arg.val[[1]]) == 1L) {
    stop(invalid.arg.val$arg.val, " is not a valid value for ", invalid.arg.val$arg.nm, " option.\n")
    } else {
      stop(paste0(invalid.arg.val$arg.val, collapse = ", "), " are not valid values for ", paste0(invalid.arg.val$arg.nm, collapse = ", "), " options, respectively.\n")
    }
}

# Calculate the composition of a differentiation medium -------------------

# Generate clean variables:

day <- as.numeric(basic.checks[basic.checks$arg.nm == "-d", "arg.val"])
vol <- ceiling(as.numeric(basic.checks[basic.checks$arg.nm == "-v", "arg.val"]))

if (day == 20) {
  split <- TRUE
  } else if (day > 20 || length(grep(pattern = "--split", x = args)) != 1) {
    split <- FALSE
    } else {
      split <- as.logical(basic.checks[basic.checks$arg.nm == "--split", "arg.val"])
    }


if (length(grep(pattern = "--antianti", x = args)) == 1) {
  antianti <- as.logical(basic.checks[basic.checks$arg.nm == "--antianti", "arg.val"])
  } else {
    antianti <- FALSE
  }

# Make calculations:

if (day == 21) {
  message("\nRelax, c'est cool, no need to feed after a day 20...\n")
  quit(save = "no")
}

if (day == 23) {
  message("\nRelax, tout roule, no need to feed after a day 22...\n")
  quit(save = "no")
}

if (day == 0) {
  output <- data.frame(component = c("KO DMEM KSR", "LDN", "SB"),
                       volume = round(c(vol, (vol/10), vol), 2),
                       unit = c("ml", rep("ul", 2)))
  } else if (day %in% c(1, 2)) {
    output <- data.frame(component = c("KO DMEM KSR", "LDN", "SB", "SHH", "Purmo", "FGF8a"),
                         volume = round(c(vol, (vol/10), rep(vol, 2), rep((vol/5), 2)), 2),
                         unit = c("ml", rep("ul", 5)))
    } else if (day %in% c(3, 4)) {
      output <- data.frame(component = c("KO DMEM KSR", "LDN", "SB", "SHH", "Purmo", "FGF8a", "CHIR"),
                           volume = round(c(vol, (vol/10), rep(vol, 2), rep((vol/5), 2), (vol/3.333)), 2),
                           unit = c("ml", rep("ul", 6)))
      } else if (day %in% c(5, 6)) {
        output <- data.frame(component = c("KO DMEM KSR", "NNB", "LDN", "SHH", "Purmo", "FGF8a", "CHIR"),
                             volume = round(c((vol*(3/4)), (vol*(1/4)), (vol/10), vol, rep((vol/5), 2), (vol/3.333)), 2),
                             unit = c(rep("ml", 2), rep("ul", 5)))
        } else if (day %in% c(7, 8)) {
          output <- data.frame(component = c("KO DMEM KSR", "NNB", "LDN", "CHIR"),
                               volume = round(c((vol*(1/2)), (vol*(1/2)), (vol/10), (vol/3.333)), 2),
                               unit = c(rep("ml", 2), rep("ul", 2)))
          } else if (day %in% c(9, 10)) {
            output <- data.frame(component = c("KO DMEM KSR", "NNB", "LDN", "CHIR"),
                                 volume = round(c((vol*(1/4)), (vol*(3/4)), (vol/10), (vol/3.333)), 2),
                                 unit = c(rep("ml", 2), rep("ul", 2))) 
            } else if (day %in% c(11, 12)) {
              output <- data.frame(component = c("NB", "CHIR", "BDNF", "GDNF", "TGFb3", "DAPT", "AA", "db-cAMP"),
                                   volume = round(c(vol, (vol/3.333), rep((vol/5), 2), rep((vol/10), 2), vol, (vol/0.2)), 2),
                                   unit = c("ml", rep("ul", 7))) 
              } else {
                output <- data.frame(component = c("NB", "BDNF", "GDNF", "TGFb3", "DAPT", "AA", "db-cAMP"),
                                     volume = round(c(vol, rep((vol/5), 2), rep((vol/10), 2), vol, (vol/0.2)), 2),
                                     unit = c("ml", rep("ul", 6)))
              }

if (day == 22) {
  output <- data.frame(component = c("NB", "MitoC", "---", "NB (WASH)", "---", output$component),
                       volume = c(vol, vol, "---", vol, "---", output$volume),
                       unit = c("ml", "ul", "---", "ml", "---", output$unit))
}

if (split == TRUE) {
  output <- data.frame(component = c(output$component, "ROCKi"),
                       volume = c(output$volume, vol),
                       unit = c(output$unit, "ul"))
}

if (antianti == TRUE) {
  output <- data.frame(component = c(output$component, "Anti-Anti"),
                       volume = c(output$volume, (vol/100)),
                       unit = c(output$unit, "ml"))
}

# Print medium composition to standard output:

if (day %in% c(20, 22)) {
  if (antianti == FALSE) {
    message("\nBIG DAY!\nHere is the composition of ", vol, " mL of d", day, " medium:\n")
    } else {
      message("\nBIG DAY!\nHere is the composition of ", vol, " mL of d", day, " medium (with AntiAnti):\n")
    }
  } else {
    if (split == FALSE && antianti == FALSE) {
      message("\nHere is the composition of ", vol, " mL of d", day, " medium:\n")
      } else if (split == TRUE && antianti == FALSE) {
        message("\nHere is the composition of ", vol, " mL of d", day, " medium. Enjoy your split!\n")
        } else if (split == FALSE && antianti == TRUE) {
          message("\nHere is the composition of ", vol, " mL of d", day, " medium (with AntiAnti):\n")
          } else if (split == TRUE && antianti == TRUE) {
            message("\nHere is the composition of ", vol, " mL of d", day, " medium (with AntiAnti). Enjoy your split!\n")
          }
  }

print(output)
message("\n")

if ((length(basic.checks[basic.checks$arg.nm == "--split", "arg.val"]) == 1) && (basic.checks[basic.checks$arg.nm == "--split", "arg.val"] %in% c("FALSE", "F")) && (day == 20L)) {
  message("Note that ROCKi was included although you provided --split FALSE; this is because cells are expected to be split on day 20.\n")
}

if ((length(basic.checks[basic.checks$arg.nm == "--split", "arg.val"]) == 1) && (basic.checks[basic.checks$arg.nm == "--split", "arg.val"] %in% c("TRUE", "T")) && (day > 20L)) {
  message("Note that ROCKi was not included although you provided --split TRUE; this is because cells are not expected to be split after day 20.\n")
}

if ((length(basic.checks[basic.checks$arg.nm == "--antianti", "arg.val"]) == 1) && (basic.checks[basic.checks$arg.nm == "--antianti", "arg.val"] %in% c("TRUE", "T")) && (day < 20L)) {
  message("Note that the use of AntiAnti is not recommended in the early stages of differentiation.\n")
}

if (day %in% c(20, 22)) { message("Good luck on your day ", day, "!\n") }

options(warn = 0)
