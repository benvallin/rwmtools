library(shiny)

# Define UI ----

ui <- shinyUI(navbarPage(title = "difftools",
                         
                         ################################## MKMED ##################################
                         
                         tabPanel(title = "mkmed",
                                  # Display title
                                  span(titlePanel(title = h2("mkmed")),
                                       style = "color:#355070"),
                                  
                                  # Display subtitle
                                  fluidRow(column(width = 12,
                                                  h4(em("Make culture media for differentiation of iPSCs into DaNs"))),
                                           div(style = "height:80px")),
                                  
                                  # Set layout for message(s) in case of missing day and volume
                                  fluidRow(
                                    column(width = 3,
                                           span(textOutput(outputId = "day_missing_check"),
                                                style = "font-size:12px; font-style:italic")),
                                    column(width = 3,
                                           span(textOutput("volume_missing_check"),
                                                style = "font-size:12px; font-style:italic"))),
                                  
                                  # Display boxes for day, volume, split and antiani input
                                  fluidRow(
                                    column(width = 3,
                                           span(textInput(inputId = "-d", 
                                                          label = h5("day of differentiation")),
                                                style = "color:#355070")),
                                    column(width = 3,
                                           span(textInput(inputId = "-v", 
                                                          label = h5("volume (ml)")),
                                                style = "color:#355070")),
                                    column(width = 3,
                                           span(selectInput(inputId = "--split", 
                                                            label = h5("split cells?"), 
                                                            choices = list("yes" = T, "no" = F), 
                                                            selected = F),
                                                style = "color:#355070")),
                                    column(width = 3,
                                           span(selectInput(inputId = "--antianti", 
                                                            label = h5("use Anti-Anti?"), 
                                                            choices = list("yes" = T, "no" = F), 
                                                            selected = F),
                                                style = "color:#355070"))),
                                  
                                  # Set layout for message(s) in case of invalid day / volume
                                  fluidRow(
                                    column(width = 3,
                                           span(textOutput("day_validity_check"),
                                                style = "font-size:12px; color:red")),
                                    column(width = 3,
                                           span(textOutput("volume_validity_check"),
                                                style = "font-size:12px; color:red"))),
                                  
                                  # Set layout for basal media recipe input
                                  fluidRow(
                                    column(width = 12,
                                           checkboxInput(inputId = "basal_media",
                                                         label = "display basal media recipe",
                                                         value = F)),
                                    div(style = "height:40px")),

                                  # Set layout to announce media composition
                                  fluidRow(
                                    column(width = 12,
                                           verbatimTextOutput("announce_media_composition"))),
                                  
                                  # Set layout to return media composition table
                                  fluidRow(
                                    column(width = 12,
                                           tableOutput("media_composition_table"))),
                                  
                                  # Set layout for closing message
                                  fluidRow(
                                    column(width = 12,
                                           verbatimTextOutput("warnings_and_greetings"))),
                                  
                                  # Set layout to display basal media recipe
                                  fluidRow(
                                    column(width = 12,
                                           verbatimTextOutput("basal_media")))),
                         
                         ################################## DIFFD ##################################
                         
                         tabPanel(title = "diffd",
                                  # Display title
                                  span(titlePanel(title = h2("diffd")),
                                       style = "color:#355070"),
                                  
                                  # Display subtitle
                                  fluidRow(column(width = 12,
                                                  h4(em("Calculate days and dates of differentiation of iPSCs into DaNs"))),
                                           div(style = "height:80px")),
                                  
                                  # Set layout for message(s) in case of missing rdate and rday, and invalid tdate / tday combination
                                  fluidRow(
                                    column(width = 3,
                                           span(textOutput(outputId = "rdate_missing_check"),
                                                style = "font-size:12px; font-style:italic")),
                                    column(width = 3,
                                           span(textOutput("rday_missing_check"),
                                                style = "font-size:12px; font-style:italic")),
                                    column(width = 6,
                                           span(textOutput("target_combination_check"),
                                                style = "font-size:12px; font-style:italic"))),
                                  
                                  # Display boxes for rdate, rday, tdate and tday input
                                  fluidRow(
                                    column(width = 3,
                                           span(textInput(inputId = "reference_date", 
                                                          label = h5("reference date")),
                                                style = "color:#355070")),
                                    column(width = 3,
                                           span(textInput(inputId = "reference_day", 
                                                          label = h5("reference day")),
                                                style = "color:#355070")),
                                    column(width = 3,
                                           span(textInput(inputId = "target_date", 
                                                          label = h5("target date")),
                                                style = "color:#355070")),
                                    column(width = 3,
                                           span(textInput(inputId = "target_day", 
                                                          label = h5("target day")),
                                                style = "color:#355070"))),
                                  
                                  # Set layout for message(s) in case of invalid rdate / rday / tdate / tday 
                                  fluidRow(
                                    column(width = 3,
                                           span(textOutput("rdate_validity_check"),
                                                style = "font-size:12px; color:red")),
                                    column(width = 3,
                                           span(textOutput("rday_validity_check"),
                                                style = "font-size:12px; color:red")),
                                    column(width = 3,
                                           span(textOutput("tdate_validity_check"),
                                                style = "font-size:12px; color:red")),
                                    column(width = 3,
                                           span(textOutput("tday_validity_check"),
                                                style = "font-size:12px; color:red"))),
                                  
                                  # Display boxes for exp, spec_exp, expdur and cal input
                                  fluidRow(
                                    column(width = 3,
                                           span(selectInput(inputId = "expansion", 
                                                            label = h5("expansion?"), 
                                                            choices = list("yes" = "yes", "no" = "no"), 
                                                            selected = "yes"),
                                                style = "color:#355070")),
                                    column(width = 3,
                                           span(conditionalPanel(condition = "input.expansion == 'yes'",
                                                                 selectInput(inputId = "special_expansion", 
                                                                             label = h5("special expansion?"),
                                                                             choices = list("yes" = 'yes', "no" = 'no'), 
                                                                             selected = "no")),
                                                style = "color:#355070")),
                                    column(width = 3,
                                           span(conditionalPanel(condition = "input.expansion == 'yes' & input.special_expansion == 'yes'",
                                                                 textInput(inputId = "expansion_duration",
                                                                           label = h5("expansion duration"))),
                                                style = "color:#355070")),
                                    column(width = 3,
                                           checkboxInput(inputId = "calendar",
                                                         label = "display calendar",
                                                         value = F))),
                                  
                                  # Set layout for message(s) in case of default expdur or invalid expdur 
                                  fluidRow(
                                    column(width = 6,
                                           textOutput("signal_expdur_default"),
                                           style = "font-size:12px; font-style:italic"),
                                    
                                    column(width = 3,
                                           offset = 6,
                                           textOutput("expdur_validity_check"),
                                           style = "font-size:12px; color:red"),
                                    div(style = "height:40px")),
                                  
                                  # Set layout for message in case of incompatibility between day(s) and exp or expdur
                                  fluidRow(
                                    column(width = 12,
                                           textOutput("day_exp_incomp_signal"),
                                           style = "color:red")),
                                  
                                  # Set layout to return target date or day
                                  fluidRow(
                                    column(width = 6,
                                           verbatimTextOutput("deliver_target")),
                                    div(style = "height:40px")),
                                  
                                  # Set layout to announce calendar
                                  fluidRow(
                                    column(width = 12,
                                           textOutput("calendar_signal")),
                                    div(style = "height:40px")),
                                  
                                  # Set layout to return calendar
                                  fluidRow(
                                    column(width = 6,
                                           tableOutput("deliver_calendar")))),
                         
                         ################################## DIFFD ##################################
                         
                         tabPanel(title = "help",
                                  # Display title
                                  span(titlePanel(title = h2("help")),
                                       style = "color:#355070",
                                       div(style = "height:20px")),
                                  
                                  fluidRow(column(width = 12,
                                                  h3("About difftools", style = "color:#355070"))),
                                  
                                  fluidRow(column(width = 12,
                                                  p("difftools aims to help with the differentiation of iPSCs into dopaminergic neurons (DaNs).",  br(),
                                                    "It is composed of two programs, mkmed and diffd, which compute medium composition and date / day of differentiation.", br(),
                                                    "The implementation of difftools is based on:", br(),
                                                    "- the differentiation protocol published by Kriks et al. (PMID: 19731555).", br(),
                                                    "- the reagents and stock concentrations in use in the RWM lab.", br(),
                                                    "In case you use a different protocol or different reagents / stock concentrations, difftools is not for you.", br(), br())),
                                           div(style = "height:20px")),
                                  
                                  fluidRow(column(width = 12,
                                                  h3("mkmed", style = "color:#355070"))),
                                  
                                  fluidRow(column(width = 12,
                                                  p("mkmed returns the composition of your culture medium, provided a day of differentiation and a required volume.",  br(),
                                                    "If split and/or Anti-Anti are set to \"yes\", the medium composition also includes ROCKi and Anti-Anti.", br(),
                                                    "If requested, mkmed also returns the recipe of the basal media required to prepare the medium.", br(), br(),
                                                    strong("Input:"), br(), br(),
                                                    strong("day of differentiation", style = "color:#355070"), " day of kriks differentiation protocol (required).", br(), 
                                                    "Valid values include any negative or positive integer, including 0 (e.g.: -2, 60).", br(), br(),
                                                    strong("volume (ml)", style = "color:#355070"), " volume of culture medium in milliliters (required).", br(), 
                                                    "Valid values include any strictly positive floating point number (e.g.: 18.5, 230).", br(), br(),
                                                    strong("split cells?", style = "color:#355070"), " indicates if ROCKi should be included (default is \"no\").", br(), br(),
                                                    strong("use Anti-Anti?", style = "color:#355070"), " indicates if Anti-Anti should be included (default is \"no\").", br(), br(),
                                                    strong("display basal media recipe", style = "color:#355070"), " if ticked, returns the recipe of the required basal media (optional).", br(), br()))),
                                  div(style = "height:20px"),
                                  
                                  fluidRow(column(width = 12,
                                                  h3("diffd", style = "color:#355070"))),
                                  
                                  fluidRow(column(width = 12,
                                                  p("diffd returns a target differentiation day or date, provided a reference date, a reference day and a target date or day.", br(),
                                                    "If requested, a differentiation calendar is generated.", br(), br(),
                                                    strong("Input:"), br(), br(),
                                                    strong("reference date", style = "color:#355070"), " reference date of differentiation matching the reference day (required).", br(), 
                                                    "Valid values are in the form YYYY-MM-DD or YYYY.MM.DD (e.g.: 2022.10.18, 2023-05-23).", br(),
                                                    "Alternatively, the string \"today\" can be provided and will be converted into the system's idea of the current date.", br(),  br(),
                                                    strong("reference day", style = "color:#355070"), " reference day of differentiation matching the reference date (required).", br(), 
                                                    "Valid values include any negative or positive integer (e.g.: -5, 85), or days in expansion style format (e.g.: 10+1, 10+15).", br(), br(),
                                                    strong("target date", style = "color:#355070"), " target date for which a differentiation date is requested (mutually exclusive with target day).", br(), 
                                                    "Valid values are in the form YYYY-MM-DD or YYYY.MM.DD (e.g.: 2022.09.28, 2023-03-06).", br(),
                                                    "Alternatively, the string \"today\" can be provided and will be converted into the system's idea of the current date.", br(),  br(),
                                                    strong("target day", style = "color:#355070"), " target day for which a differentiation date is requested (mutually exclusive with target date).", br(), 
                                                    "Valid values include any negative or positive integer (e.g.: 3, 22), or days in expansion style format (e.g.: 10+1, 10+15).", br(), br(),
                                                    strong("expansion?", style = "color:#355070"), " indicates if an expansion phase should be included (default is \"yes\").", br(),
                                                    "By default, the duration of the expansion phase is 21 days.", br(), br(),
                                                    strong("special expansion?", style = "color:#355070"), " if set to \"yes\", opens a new box to provide the duration of the expansion phase manually.", br(), br(),
                                                    strong("expansion duration", style = "color:#355070"), " duration of the expansion phase in days (optional).", br(),
                                                    "Valid values include any strictly positive whole number (e.g.: 20, 23)", br(), br(),
                                                    strong("display calendar", style = "color:#355070"), " if ticked, returns a complete differentiation calendar.", br(),
                                                    "Note that a differentiation calendar can be obtained by providing only a reference date and a reference day (no target).", br(),
                                                    "The first day of the calendar is the lowest value between -2, reference day and target day (if provided).", br(),
                                                    "The last day is the highest value between 60, reference day and target day (if provided)."))),
                                  div(style = "height:20px"))))

# Define server logic ----

server <- function(input, output) {
  
  ################################## MKMED ##################################
  
  ### CAPTURE USER INPUT
  
  # Update default input for --split depending on -d 
  observe({
    current_day <- input[["-d"]]
    # Set split to true if day 20
    if (current_day == "20") {
      updateSelectInput(
        inputId = "--split",
        selected = T)
    }
    # Set split to false if day 22
    if (current_day == "22") {
      updateSelectInput(
        inputId = "--split",
        selected = F)
    }
    
  })
  
  # Bind input values to variables
  day <- reactive({input[["-d"]]})
  volume <- reactive({input[["-v"]]})
  split <- reactive({input[["--split"]]})
  antianti <- reactive({input[["--antianti"]]})
  basal_media <- reactive({input[["basal_media"]]})
  
  ### PROCESS INPUT AND PERFORM QC
  
  # Check validity of day 
  valid_day <- reactive({grepl(x = day(), pattern = "^(0|-?[1-9]+\\d*)$")})
  
  # Check validity of volume
  valid_volume <- reactive({ifelse(is.na(as.numeric(volume())) | as.numeric(volume()) <= 0L, F, T)})
  
  # Format input for computation
  d <- reactive({if (valid_day()) {as.numeric(day())}})
  v <- reactive({if (valid_volume()) {ceiling(as.numeric(volume()))}})
  s <- reactive({ as.logical(split()) })
  a <- reactive({as.logical(antianti())})
  b <- reactive({ifelse(basal_media() == T, T, F)})
  
  ### DISPLAY MESSAGES FOR MISSING / INVALID INPUT

  # Display message in case of missing day 
  output$day_missing_check <- renderText({
    if (!valid_day() & day() == "") {
      c("Please provide a day")
    }
  })
  
  # Display message in case of missing volume 
  output$volume_missing_check <- renderText({
    if (!valid_volume() & volume() == "") {
      c("Please provide a volume")
    }
  })
  
  # Display message in case of invalid day 
  output$day_validity_check <- renderText({
    if (!valid_day() & day() != "") {
      paste0(day(), " is not a valid day")
    }
  })
  
  # Display message in case of invalid volume 
  output$volume_validity_check <- renderText({
    if (!valid_volume() & volume() != "") {
      paste0(volume(), " is not a valid volume")
    }
  })
  
  ### RETURN MEDIA COMPOSITION
  
  # Announce media composition
  output$announce_media_composition <- renderText({
    if (valid_day() & valid_volume()) {
      if (d() == 21) {
        c("Relax, c'est cool, no need to feed after a day 20...\n")
      } else if (d() == 23) {
        c("Relax, tout roule, no need to feed after a day 22...\n")
      } else if (d() %in% c(20, 22)) {
        if (!a()) {
          paste0("BIG day!\nHere is the composition of ", v(), "mL of d", d(), " medium:\n")
        } else {
          paste0("BIG day!\nHere is the composition of ", v(), "mL of d", d(), " medium (with Anti-Anti):\n")
        }
      } else {
        if (!s() & !a()) {
          paste0("Here is the composition of ", v(), "mL of d", d(), " medium:\n")
        } else if (s() & !a()) {
          paste0("Here is the composition of ", v(), "mL of d", d(), " medium. Enjoy your split!\n")
        } else if (!s() & a()) {
          paste0("Here is the composition of ", v(), "mL of d", d(), " medium (with Anti-Anti):\n")
        } else if (s() & a()) {
          paste0("Here is the composition of ", v(), "mL of d", d(), " medium (with Anti-Anti). Enjoy your split!\n")
        }
      }
    }
  })
  
  # Return media composition table
  output$media_composition_table <- renderTable({
    if (valid_day() & valid_volume()) {
      ul <- paste0(utf8::utf8_print("\u03BC"),"l")
      if (d() < 0) {
        output <- data.frame(component = "mTeSR",
                             volume = round(v(), 2),
                             unit = "ml")
      } else if (d() == 0) {
        output <- data.frame(component = c("KO DMEM KSR", "LDN", "SB"),
                             volume = round(c(v(), (v()/10), v()), 2),
                             unit = c("ml", rep(ul, 2)))
      } else if (d() %in% c(1, 2)) {
        output <- data.frame(component = c("KO DMEM KSR", "LDN", "SB", "SHH", "Purmo", "FGF8a"),
                             volume = round(c(v(), (v()/10), rep(v(), 2), rep((v()/5), 2)), 2),
                             unit = c("ml", rep(ul, 5)))
      } else if (d() %in% c(3, 4)) {
        output <- data.frame(component = c("KO DMEM KSR", "LDN", "SB", "SHH", "Purmo", "FGF8a", "CHIR"),
                             volume = round(c(v(), (v()/10), rep(v(), 2), rep((v()/5), 2), (v()/3.333)), 2),
                             unit = c("ml", rep(ul, 6)))
      } else if (d() %in% c(5, 6)) {
        output <- data.frame(component = c("KO DMEM KSR", "NNB", "LDN", "SHH", "Purmo", "FGF8a", "CHIR"),
                             volume = round(c((v()*(3/4)), (v()*(1/4)), (v()/10), v(), rep((v()/5), 2), (v()/3.333)), 2),
                             unit = c(rep("ml", 2), rep(ul, 5)))
      } else if (d() %in% c(7, 8)) {
        output <- data.frame(component = c("KO DMEM KSR", "NNB", "LDN", "CHIR"),
                             volume = round(c((v()*(1/2)), (v()*(1/2)), (v()/10), (v()/3.333)), 2),
                             unit = c(rep("ml", 2), rep(ul, 2)))
      } else if (d() %in% c(9, 10)) {
        output <- data.frame(component = c("KO DMEM KSR", "NNB", "LDN", "CHIR"),
                             volume = round(c((v()*(1/4)), (v()*(3/4)), (v()/10), (v()/3.333)), 2),
                             unit = c(rep("ml", 2), rep(ul, 2))) 
      } else if (d() %in% c(11, 12)) {
        output <- data.frame(component = c("NB", "CHIR", "BDNF", "GDNF", paste0("TGF", utf8::utf8_print("\u03B2"),"3"), "DAPT", "AA", "db-cAMP"),
                             volume = round(c(v(), (v()/3.333), rep((v()/5), 2), rep((v()/10), 2), v(), (v()/0.2)), 2),
                             unit = c("ml", rep(ul, 7))) 
      } else {
        output <- data.frame(component = c("NB", "BDNF", "GDNF", paste0("TGF", utf8::utf8_print("\u03B2"),"3"), "DAPT", "AA", "db-cAMP"),
                             volume = round(c(v(), rep((v()/5), 2), rep((v()/10), 2), v(), (v()/0.2)), 2),
                             unit = c("ml", rep(ul, 6)))
      }
      if (d() == 22) {
        output <- data.frame(component = c("NB", "MitoC", "---", "NB (WASH)", "---", output$component),
                             volume = c(v(), v(), "---", v(), "---", output$volume),
                             unit = c("ml", ul, "---", "ml", "---", output$unit))
      }
      if (s()) {
        output <- data.frame(component = c(output$component, "ROCKi"),
                             volume = c(output$volume, v()),
                             unit = c(output$unit, ul))
      }
      if (antianti()) {
        output <- data.frame(component = c(output$component, "Anti-Anti"),
                             volume = c(output$volume, (v()/100)),
                             unit = c(output$unit, "ml"))
      }
      if (d() %in% c(21, 23)) {
        output <- NULL
      }
      output
    }
  })
  
  # Display closing message
  output$warnings_and_greetings <- renderText({
    if (valid_day() & valid_volume()) {
      if (!(d() %in% c(21, 23))) {
        if (!s() & d() == 20) {
          c("Warning: split is set to \"no\" but cells are expected to be split on day 20.\n")
        } else if (s() & d() > 20) {
          c("Warning: split is set to \"yes\" but cells are not expected to be split after day 20.\n")
        } else if (a() & d() < 20) {
          c("Warning: Anti-Anti is set to \"yes\" but it is not recommended before day 20.\n")
        } else if ((d() == 20 & s()) | (d() == 22 & !s())) {
          paste0(c("Good luck on your day ", d(), "!\n"), collapse ="")
        } else if (d() < 0 & s()) {
          c("Note that ROCKi could be ommited if you split iPSCs by colony passaging")
        }
      }
    }
  })
  
  ### RETURN BASAL MEDIA RECIPE
  
  # Define recipe of mTeSR, KO DMEM KSR, NNB and NB
  mtesr_recipe <- c(
  "mTeSR:
  - 400 ml of mTeSR Basal Medium (a full new bottle)
  - 100 ml of mTeSR 5X Supplement (1:5; 1X final)
  - OPTIONAL: 5 ml of 100X penicillin-streptomycin solution (1:100; 1X final)\n\n"
  )
  ko_recipe <- c(
  "KO DMEM KSR:
  - 410 ml of KO DMEM Medium (remove 90 ml from a new 500 ml bottle)
  - 75 ml of 100% KnockOut Serum Replacement (15% final)
  - 5 ml of 1mM beta-mercaptoethanol (1:100; 10 uM final)
  - 5 ml of 200 mM L-glutamine (1:100; 2 mM final)
  - 5 ml of 100X MEM Non-Essential Amino Acids solution (1:100; 1X final)\n\n"
  )
  nnb_recipe <- c(
  "NNB:
  - 487.5 ml of Neurobasal Medium (remove 12.5 ml from a new 500 ml bottle)
  - 2.5 ml of 100X N2 Supplement (1:200; 0.5X final)
  - 5 ml of 50X B-27 Supplement without vitamin A (1:100; 0.5X final)
  - 5 ml of 200 mM L-glutamine (1:100; 2 mM final)\n\n"
  )
  nb_recipe <- c(
  "NB:
  - 485 ml of Neurobasal Medium (remove 15 ml from a new 500 ml bottle)
  - 10 ml of 50X B-27 Supplement without vitamin A (1:50; 1X final)
  - 5 ml of 200 mM L-glutamine (1:100; 2 mM final)\n\n"
  )

  # Resolve recipe of required basal media depending on input day
  basal_media_recipe <- reactive({
    if (d() < 0) {
      paste0("See below the composition of required basal media:\n\n", mtesr_recipe)
    } else if (d() < 5) {
      paste0("See below the composition of required basal media:\n\n", ko_recipe)
    } else if (d() < 11) {
      paste0("See below the composition of required basal media:\n\n", ko_recipe, nnb_recipe)
    } else {
      paste0("See below the composition of required basal media:\n\n", nb_recipe)
    }
  })

  # Return recipe of required basal media
  output$basal_media <- renderText({
    if (valid_day() & valid_volume() & b()) {
      basal_media_recipe()
    }
  })
  
  ################################## DIFFD ##################################
  
  ### CAPTURE USER INPUT
  
  # Update default input for special_expansion and expansion_duration depending on expansion input
  observe({
    current_exp <- input[["expansion"]]
    if (current_exp == "no") {
      updateSelectInput(
        inputId = "special_expansion",
        selected = "no")
    }
    current_spec_exp <- input[["special_expansion"]]
    if (current_spec_exp == "no") {
      updateTextInput(
        inputId = "expansion_duration",
        value = "")
    }
  })
  
  # Bind input values to variables
  reference_date <- reactive({input[["reference_date"]]})
  rday <- reactive({input[["reference_day"]]})
  target_date <- reactive({input[["target_date"]]})
  tday <- reactive({input[["target_day"]]})
  expansion <- reactive({input[["expansion"]]})
  special_expansion <- reactive({input[["special_expansion"]]})
  expansion_duration <- reactive({input[["expansion_duration"]]})
  calendar <- reactive({input[["calendar"]]})
  
  ### DEFINE FUNCTIONS TO PROCESS INPUT
  
  # Function to check if input with no default is missing
  check_missing_input <- function(x) { ifelse(x == "", T, F) }
  
  # Function to format dates
  format_date <- function(x) {
    out <- ifelse(x == "today", as.character(Sys.Date()), gsub(x = x, pattern = "\\.", replacement = "-"))
  }
  
  # Function to check validity of dates
  check_date_validity <- function(x) {
    if (grepl(x = x, pattern = "^\\d{4}-\\d{2}-\\d{2}$")) {
      ifelse(class(try(expr = as.Date(x), silent = T)) == "try-error", F, T)
    } else {
      F
    }
  }
  
  # Function to check validity of days
  check_day_validity <- function(x) {
    if (!grepl(x = x, pattern = "\\+")) {
      ifelse(grepl(x = x, pattern = "^-?\\d+$"), T, F)
    } else {
      ifelse(grepl(x = x, pattern = "^10\\+[1-9]+$"), T, F)
    }
  }
  
  # Function to check is days are compatible with no expansion
  check_day_noexp_validity <- function(x) {
    ifelse(!exp() & grepl(x = x, pattern = "^10\\+[1-9]+$"), F, T) 
  }
  
  # Function to check is days are compatible with expansion duration
  check_day_expdur_validity <- function(x) {
    if (grepl(x = x, pattern = "^10\\+[1-9]+$")) {
      ifelse(as.integer(sub(x = x, pattern = "^10\\+", replacement = "")) > as.integer(expdur()), F, T)
    } else {
      T
    }
  }
  
  ### PROCESS INPUT AND PERFORM QC
  
  # Check if input with no default are missing
  rdate_missing <- reactive({check_missing_input(reference_date())})
  rday_missing <- reactive({check_missing_input(rday())})
  tdate_missing <- reactive({check_missing_input(target_date())})
  tday_missing <- reactive({check_missing_input(tday())})
  expdur_missing <- reactive({check_missing_input(expansion_duration())})
  
  # Format input for computation
  rdate <- reactive({format_date(reference_date())})
  tdate <- reactive({format_date(target_date())})
  exp <- reactive({ifelse(expansion() == "yes", T, F)})
  spec_exp <- reactive({
    if (!exp()) {
      F
    } else {
      ifelse(special_expansion() == "yes", T, F)
    }
  })
  expdur <- reactive({
    if (!exp()) {
      "0"
    } else if (!spec_exp() | expansion_duration() == "") {
      "21"
    } else {
      expansion_duration()
    }
  })
  cal <- reactive({ifelse(calendar() == T, T, F)})
  
  # Check validity of input after formatting
  rdate_valid <- reactive({check_date_validity(rdate())})
  rday_valid <- reactive({check_day_validity(rday())})
  tdate_valid <- reactive({check_date_validity(tdate())})
  tday_valid <- reactive({check_day_validity(tday())})
  expdur_valid <- reactive({ifelse(!exp(), T, grepl(x = expdur(), pattern = "^[1-9][0-9]*$"))})
  rday_noexp_compatible <- reactive({check_day_noexp_validity(rday())})
  tday_noexp_compatible <- reactive({check_day_noexp_validity(tday())})
  days_noexp_compatible <- reactive({ifelse(rday_noexp_compatible() & tday_noexp_compatible(), T, F)})
  rday_expdur_compatible <- reactive({check_day_expdur_validity(rday())})
  tday_expdur_compatible <- reactive({check_day_expdur_validity(tday())})
  days_expdur_compatible <- reactive({ifelse(rday_expdur_compatible() & tday_expdur_compatible(), T, F)})
  
  # Check that rdate and rday are both present and valid
  two_refs <- reactive({ifelse(rdate_valid() & rday_valid(), T, F)})
  
  # Check that calendar is ticked in case both targets are missing
  zero_target_but_calendar <- reactive({ifelse(tdate_missing() & tday_missing() & cal(), T,F)})
  
  # Check that only one target is present and valid, and that the second target is missing (not simply present and invalid)
  one_target <- reactive({
    if (tdate_valid()) {
      ifelse(tday_missing(), T, F)
    } else if (tday_valid()) {
      ifelse(tdate_missing(), T, F)
    } else {
      F
    }
  })
  
  # In case one_target is true, resolve target ID
  target_id <- reactive({
    if(one_target()) { 
      ifelse(tdate_valid(), "tday", "tdate")
    } else {
      NA_character_
    }
  })
  
  # Check that everything is ok for further computation:
  # => rdate and rday are both present and valid
  # => only one target is present and valid and the second target is missing (or both targets are missing but calendar is ticked)
  # => expansion duration is valid
  # => rday and tday are not in expansion style format if exp is false (days compatible with no expansion)
  # => rday and tday are not greater than expansion duration (days compatible with expansion duration)
  all_ok <- reactive({ifelse(two_refs() & (one_target() | zero_target_but_calendar()) & expdur_valid() & days_noexp_compatible() & days_expdur_compatible(),
                             T, F)})
  
  ### RESOLVE REQUESTED TARGET VALUE
  
  # If one_target and exp are true, define logical vectors rday.inexp and rday.preexp indicating if rday is in / before expansion:
  rday.inexp <- reactive({
    if (all_ok() & exp()) {
      ifelse(grepl(x = rday(), pattern = "\\+"), T, F)
    }
  })
  
  rday.preexp <- reactive({
    if (all_ok() & exp()) {
      ifelse(rday.inexp(), FALSE, ifelse(as.integer(rday()) <= 10L, TRUE, FALSE))
    }
  })
  
  # Resolve tday value (tdate provided)
  solved_tday <- reactive({
    if (all_ok() & one_target() & target_id() == "tday") {
      # If exp is true
      if (exp()) {
        # Define numerical vector temp.tday 
        # => temp.tday = temporary numerical value of tday with expansion days added to the total
        # If rday is before expansion phase
        if (rday.preexp()) {
          temp.tday <- as.integer(as.integer(rday()) + (as.Date(tdate()) - as.Date(rdate())))
          # If rday is during expansion phase
        } else if (rday.inexp()) {
          temp.tday <- as.integer((10L + as.integer(sub(x = rday(), pattern = "^10\\+", replacement = ""))) + (as.Date(tdate()) - as.Date(rdate())))
          # If rday is after expansion phase
        } else {
          temp.tday <- as.integer(as.integer(rday()) + (as.Date(tdate()) - as.Date(rdate())) + as.integer(expdur()))
        }
        # Define character vector tday
        # => tday = value of requested tday (follows the rules of expansion style format)
        # If requested tday is before expansion phase
        if (temp.tday <= 10L) {
          tday <- as.character(temp.tday)
          # If requested tday is during expansion phase
        } else if (temp.tday > 10L & temp.tday <= (10L + as.integer(expdur()))) {
          tday <- paste0("10+", temp.tday - 10L)
          # If requested tday is after expansion phase
        } else {
          tday <- as.character(temp.tday - as.integer(expdur()))
        }
        # If exp is false
      } else {
        # Define character vector tday
        # => tday = value of requested tday (follows the rules of expansion style format)
        tday <- as.character(as.integer(rday()) + (as.Date(tdate()) - as.Date(rdate())))
      }
    }
  })
  
  # Resolve tday value (tdate provided)
  solved_tdate <- reactive({
    if (all_ok() & one_target() & target_id() == "tdate") {
      # If exp is true
      if (exp()) {
        # Define logical vectors tday.inexp and tday.preexp indicating if tday is in / before expansion phase:
        tday.inexp <- ifelse(grepl(x = tday(), pattern = "\\+"), TRUE, FALSE)
        tday.preexp <- ifelse(tday.inexp, FALSE, ifelse(as.integer(tday()) <= 10L, TRUE, FALSE))
        # Define numerical vector temp.rday
        # => temp.rday = temporary numerical value of rday with expansion days added to the total
        # If rday is before expansion phase
        if (rday.preexp()) {
          temp.rday <- as.integer(rday())
          # If rday is during expansion phase
        } else if (rday.inexp()) {
          temp.rday <- as.integer((10L + as.integer(sub(x = rday(), pattern = "^10\\+", replacement = ""))))
          # If rday is after expansion phase
        } else {
          temp.rday <- as.integer(rday()) + as.integer(expdur())
        }
        # Define numerical vector temp.tday
        # => temp.tday = temporary numerical value of tday with expansion days added to the total
        # If tday is before expansion phase
        if (tday.preexp) {
          temp.tday <- as.integer(tday())
          # If tday is during expansion phase
        } else if (tday.inexp) {
          temp.tday <- as.integer((10L + as.integer(sub(x = tday(), pattern = "^10\\+", replacement = ""))))
          # If tday is after expansion phase
        } else {
          temp.tday <- as.integer(tday()) + as.integer(expdur())
        }
        # Define character vector tdate
        # => tdate = value of requested tdate
        tdate <- as.character(as.Date(rdate()) + (temp.tday - temp.rday))
        # If exp is false
      } else {
        # Define character vector tdate
        # => tdate = value of requested tdate
        tdate <- as.character(as.Date(rdate()) + (as.integer(tday()) - as.integer(rday())))
      }
    }
  })
  
  ### PRODUCE CALENDAR
  
  # If cal is true, define vector cal_tday for calendar production
  # => cal_tday = NULL if no target was provided
  # => cal_tday = solved_tday if rdate was provided
  cal_tday <- reactive({
    if (all_ok() & cal()) {
      if (zero_target_but_calendar()) {
        NULL
      } else {
        ifelse(target_id() == "tday", solved_tday(), tday())
      }
    }
  })
  
  # Generate differentiation calendar
  cal_table <- reactive({
    if (all_ok() & cal()) {
      # Define numerical vectors sday and eday
      # => sday = start day of the calendar. It is the lowest value between -2, rday and tday (if provided)
      sday <- min(c(-2L,
                    as.integer(sub(x = rday(), pattern = "\\+\\d+$", replacement = "")),
                    as.integer(sub(x = cal_tday(), pattern = "\\+\\d+$", replacement = ""))))
      # => eday = end day of the calendar. It is the highest value between 60, rday and tday (if provided)
      eday <- max(c(60L,
                    as.integer(sub(x = rday(), pattern = "\\+\\d+$", replacement = "")),
                    as.integer(sub(x = cal_tday(), pattern = "\\+\\d+$", replacement = ""))))
      # Define data.frame calendar
      # calendar is composed of a single column diffday indicating the days of differentiation
      # If exp is true, calendar includes a number of expansion days equal to the duration of the expansion phase
      if(exp()) {
        calendar <- data.frame(diffday = as.character(c(seq.int(from = sday, to = 10L, by = 1L),
                                                        paste0("10+", seq.int(from = 1L, to = as.integer(expdur()), by = 1L)),
                                                        seq.int(from = 11L, to = eday, by = 1L))))
      } else {
        calendar <- data.frame(diffday = as.character(c(seq.int(from = sday, to = eday, by = 1L))))
      }
      # Add a second column diffdate to calendar, to be filled with dates corresponding to the differentiation days
      # The diffdate column is initialised with the provided rdate only
      calendar[calendar$diffday == rday(), "diffdate"] <- rdate()
      # Define date vectors before.rdate and after.rdate
      # => before.rdate = ranges from date of sday to date of rday
      before.rdate <- seq.Date(from = as.Date(rdate()) - (match(x = rdate(), table = calendar$diffdate) - 1L),
                               to = as.Date(rdate()),
                               by = "day")
      # => after.rdate = ranges from date of rday+1 to date of eday
      after.rdate <- seq.Date(from = as.Date(rdate()) + 1L,
                              length.out = nrow(calendar) - match(x = rdate(), table = calendar$diffdate),
                              by = "day")
      # Fill out the diffdate column of the calendar with the differentiation dates before and after rdate
      calendar$diffdate <- c(as.character(before.rdate), as.character(after.rdate))
      calendar
    }
  })
  
  ### DISPLAY MESSAGES FOR MISSING / INVALID INPUT
  
  # Display message in case of missing rdate 
  output$rdate_missing_check <- renderText({
    if (rdate_missing()) {
      c("Please provide a reference date")
    }
  })
  
  # Display message in case of invalid rdate 
  output$rdate_validity_check <- renderText({
    if (!rdate_valid() & !rdate_missing()) {
      c(rdate(), " is not a valid reference date")
    }
  })
  
  # Display message in case of missing rday
  output$rday_missing_check <- renderText({
    if (rday_missing()) {
      c("Please provide a reference day")
    }
  })
  
  # Display message in case of invalid rday 
  output$rday_validity_check <- renderText({
    if (!rday_valid() & !rday_missing()) {
      c(rday(), " is not a valid reference day")
    }
  })
  
  # Display message in case of invalid tdate 
  output$tdate_validity_check <- renderText({
    if (!tdate_valid() & !tdate_missing()) {
      c(tdate(), " is not a valid target date")
    }
  })
  
  # Display message in case of invalid tday 
  output$tday_validity_check <- renderText({
    if (!tday_valid() & !tday_missing()) {
      c(tday(), " is not a valid target day")
    }
  })
  
  # Display message in case of invalid tdate and tday combination
  output$target_combination_check <- renderText({
    if (tdate_missing() & tday_missing() & !cal()) {
      c("\nPlease provide at least one target, or tick \"calendar\" box")
    } else if (!tdate_missing() & !tday_missing()) {
      c("\nPlease provide either target date or target day, not both")
    }
  })
  
  # Display message in case expdur is set to default
  output$signal_expdur_default <- renderText({
    if (exp() & (!spec_exp() | expansion_duration() == "")) {
      c("Expansion duration set to default = 21 days")
    } 
  })
  
  # Display message in case of invalid expdur
  output$expdur_validity_check <- renderText({
    if (exp() & !expdur_valid()) {
      c(expdur(), " is not a valid expansion duration")
    }
  })
  
  # Display message in case of incompatibility between day(s) and exp or expdur 
  output$day_exp_incomp_signal <- renderText({
    if (!days_noexp_compatible()) {
      if (!rday_noexp_compatible() & tday_noexp_compatible()) {
        c("Problem: input reference day (", rday(), ") is in expansion style format while expansion is set to \"no\"")
      } else if (rday_noexp_compatible() & !tday_noexp_compatible()) {
        c("Problem: input target day (", tday(), ") is in expansion style format while expansion is set to \"no\"")
      } else {
        c("Problem: input reference and target days (", rday(), " and ", tday(), ") are in expansion style format while expansion is set to \"no\"")
      }
    } else if (expdur_valid() & !days_expdur_compatible()) {
      if (!rday_expdur_compatible() & tday_expdur_compatible()) {
        c("Problem: input reference day (", rday(), ") is superior to expansion phase duration (", expdur(),")")
      } else if (rday_expdur_compatible() & !tday_expdur_compatible()) {
        c("Problem: input target day (", tday(), ") is superior to expansion phase duration (", expdur(),")")
      } else {
        c("Problem: input reference and target days (", rday(), " and ", tday(), ") are superior to expansion phase duration (", expdur(),")")
      }
    }
  })
  
  ### RETURN REQUESTED TARGET AND/OR CALENDAR
  
  # Return requested tday or tdate
  output$deliver_target <- renderText({
    if(all_ok() & one_target()) {
      if(target_id() == "tday") {
        # Return target day to standard output:
        c("Your target day is ", solved_tday())
      } else if (target_id() == "tdate") {
        # Return target date to standard output:
        c("Your target date is ", solved_tdate())
      }
    } else if (all_ok() & zero_target_but_calendar()) {
      c("You provided no target")
    }
  })
  
  # Announce calendar
  output$calendar_signal <- renderText({ if(all_ok() & cal()) { c("See below your differentiation calendar:") }})
  
  # Return calendar
  output$deliver_calendar <- renderTable({ if (all_ok() & cal()) { cal_table() }})
  
}

# Run the app ----

shinyApp(ui = ui, server = server)

