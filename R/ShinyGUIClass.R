ShinyGUI <- setRefClass("ShinyGUI", 
                      
                      fields = list(title = 'character',
                                    author = 'character',
                                    questions = 'list',
                                    df = 'list',
                                    firstpage = 'list',
                                    demographics = 'list',
                                    lastpage = 'function',
                                    instructions = 'character',
                                    begin_message = 'character',
                                    stem_locations = 'character',
                                    stem_expressions = 'character',
                                    demographic_inputIDs = 'character',
                                    temp_file = 'character',
                                    customTypes = 'list',
                                    choiceNames = 'list',
                                    choiceValues = 'list',
                                    timer = 'numeric',
                                    width = 'numeric',
                                    height = 'numeric',
                                    forced_choice = 'logical',
                                    time_before_answer = 'numeric',
                                    password='data.frame',
                                    css = 'character',
                                    stopApp = 'logical',
                                    ui = 'function',
                                    theme = 'character'),
                      
                      methods = list(
                          initialize = function(questions, df, shinyGUI, adaptive, CustomTypes, Timer){
                              'Initialize the shiny GUI given questions, df, and shinyGUI list'
                              ui <<- default_UI
                              questions <<- questions
                              df <<- df
                              forced_choice <<- TRUE
                              stopApp <<- TRUE
                              theme <<- ''
                              Timer <- as.numeric(Timer)
                              timer <<- ifelse(is.finite(Timer), Timer, as.numeric(NA))
                              if(length(CustomTypes)){
                                  if(length(CustomTypes) != length(unique(names(CustomTypes))))
                                      stop('customTypes list requires unique names for each function', call.=FALSE)
                              }
                              customTypes <<- CustomTypes
                              choiceNames <<- shinyGUI$choiceNames
                              choiceValues <<- shinyGUI$choiceValues
                              if(is.null(shinyGUI$stem_locations)){
                                  stem_locations <<- as.character(rep(NA, length(questions)))
                              } else {
                                  stem_locations <<- as.character(sapply(shinyGUI$stem_locations, 
                                    function(x){                                        
                                        ret <- if(!is.na(x)){
                                            org <- x
                                            exsts <- file.exists(x)
                                            if(!exsts){
                                                x <- paste0(getwd(), '/', x)
                                                exsts <- file.exists(x)
                                            }
                                            if(!exsts) 
                                                stop(sprintf('The following file cannot be located: %s', org), call.=FALSE)
                                            normalizePath(x, mustWork = TRUE)
                                        } else NA
                                        return(ret)
                                  }))
                              }
                              if(is.null(shinyGUI$stem_expressions)){
                                  stem_expressions <<- as.character(rep(NA, length(questions)))
                              } else {
                                  stem_expressions <<- as.character(ifelse(shinyGUI$stem_expressions == "",
                                                                         NA, shinyGUI$stem_expressions))
                              }
                              
                              title <<- 'mirtCAT'
                              author <<- 'Author information'
                              instructions <<- c("To progress through the interface, click on the action button below.",
                                                 "Next")
                              demographic_inputIDs <<- character(0)
                              if(adaptive){
                                begin_message <<- "Click the action button to begin."
                              } else begin_message <<- ""
                              firstpage <<- list(h1('Welcome to the mirtCAT interface'),
                                                 sprintf('The following interface was created using the mirtCAT package v%s. 
                                                 To cite the package use citation(\'mirtCAT\') in R.', packageVersion("mirtCAT")))
                              demographics <<- list()
                              lastpage <<- function(person) 
                                            return(list(h5("You have successfully completed the interface.
                                                   Click the action button to terminate the application.")))
                              if(!is.null(shinyGUI$stopApp) && !shinyGUI$stopApp)
                                  lastpage <<- function(person) 
                                      return(list(h5("You have successfully completed the interface.
                                                   Please close the tab/web browser to terminate the application.")))
                              temp_file <<- ''
                              css <<- ''
                              password <<- data.frame()
                              time_before_answer <<- 1
                                                 
                              if(length(shinyGUI)){
                                  dnames <- names(shinyGUI)
                                  gnames <- c('title', 'authors', 'instructions', 'firstpage', 'demographics',
                                              'demographics_inputIDs', 'temp_file', 
                                              'lastpage', 'css', 'stem_dims', 'forced_choice', 'stem_locations',
                                              'begin_message', 'stopApp', 'ui', 'password', 'stem_default_format',
                                              'stem_expressions', 'theme', 'time_before_answer', 'choiceNames',
                                              'choiceValues')
                                  if(!all(dnames %in% gnames))
                                      stop('The following inputs to shinyGUI are invalid: ',
                                           paste0(dnames[!(dnames %in% gnames)], ' '), call.=FALSE)
                                  if(!is.null(shinyGUI$ui))
                                      ui <<- shinyGUI$ui
                                  if(!is.null(shinyGUI$theme))
                                      theme <<- shinyGUI$theme
                                  if(!is.null(shinyGUI$instructions))
                                      instructions <<- shinyGUI$instructions
                                  if(!is.null(shinyGUI$begin_message))
                                      begin_message <<- shinyGUI$begin_message
                                  if(!is.null(shinyGUI$title))
                                      title <<- shinyGUI$title
                                  if(!is.null(shinyGUI$authors))
                                      author <<- shinyGUI$authors
                                  if(!is.null(shinyGUI$firstpage)) 
                                      firstpage <<- shinyGUI$firstpage
                                  if(!is.null(shinyGUI$demographics)){
                                      demographics <<- shinyGUI$demographics
                                      demographic_inputIDs <<- shinyGUI$demographics_inputIDs
                                  }
                                  if(!is.null(shinyGUI$forced_choice))
                                      forced_choice <<- shinyGUI$forced_choice
                                  if(!is.null(shinyGUI$stopApp))
                                      stopApp <<- shinyGUI$stopApp
                                  if(!is.null(shinyGUI$lastpage)) 
                                      lastpage <<- shinyGUI$lastpage
                                  if(!is.null(shinyGUI$temp_file))
                                      temp_file <<- shinyGUI$temp_file
                                  if(!is.null(shinyGUI$css))
                                      css <<- shinyGUI$css
                                  if(!is.null(shinyGUI$password))
                                      password <<- shinyGUI$password
                                  if(!is.null(shinyGUI$time_before_answer))
                                      time_before_answer <<- shinyGUI$time_before_answer
                              }
                              if(any(!is.na(timer)) && forced_choice) 
                                  stop('Timer inputs cannot be combined with shinyGUI$forced_choice = TRUE', 
                                       call.=FALSE)
                          })
                      
)