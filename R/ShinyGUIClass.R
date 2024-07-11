ShinyGUI <- setRefClass("ShinyGUI", 
                      
                      fields = list(title = 'character',
                                    author = 'character',
                                    questions = 'list',
                                    df = 'list',
                                    firstpage = 'list',
                                    demographics = 'list',
                                    lastpage = 'function',
                                    instructions = 'character',
                                    itemtimer = 'character',
                                    incorrect = 'character',
                                    failpass = 'character',
                                    timemsg = 'character',
                                    begin_message = 'character',
                                    stem_locations = 'character',
                                    stem_expressions = 'character',
                                    time_remaining = 'character',
                                    response_msg = 'character',
                                    demographic_inputIDs = 'character',
                                    temp_file = 'character',
                                    max_password_attempts = 'integer',
                                    customTypes = 'list',
                                    choiceNames = 'list',
                                    choiceValues = 'list',
                                    timer = 'numeric',
                                    width = 'numeric',
                                    height = 'numeric',
                                    time_before_answer = 'numeric',
                                    password='data.frame',
                                    css = 'character',
                                    ui = 'function',
                                    theme = 'character'),
                      
                      methods = list(
                          initialize = function(questions, df, shinyGUI, adaptive, CustomTypes, Timer){
                              'Initialize the shiny GUI given questions, df, and shinyGUI list'
                              ui <<- default_UI
                              questions <<- questions
                              df <<- df
                              theme <<- ''
                              Timer <- as.numeric(Timer)
                              timer <<- ifelse(is.finite(Timer), Timer, 0)
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
                                        ret <- if(!is.na(x) && x != ""){
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
                              itemtimer <<- 'Item timer: '
                              incorrect <<- 'The answer provided was incorrect. Please select an alternative.'
                              failpass <<- 'Incorrect Login Name/Password. Please try again (you have %s attempts remaining).'
                              timemsg <<- c('hour ','minutes ','seconds ', 'and ')
                              time_remaining <<- 'Time remaining: '
                              response_msg <<- "Please provide a suitable response"
                              demographic_inputIDs <<- character(0)
                              max_password_attempts <<- 3L
                              if(adaptive){
                                begin_message <<- "Click the action button to begin."
                              } else begin_message <<- ""
                              firstpage <<- list(h1('Welcome to the mirtCAT interface'),
                                                 sprintf('The following interface was created using the mirtCAT package v%s. 
                                                 To cite the package use citation(\'mirtCAT\') in R.', packageVersion("mirtCAT")))
                              demographics <<- list()
                              lastpage <<- function(person) 
                                            return(list(h5("You have successfully completed the interface.
                                                   It is now safe to leave the session.")))
                              temp_file <<- ''
                              css <<- ''
                              password <<- data.frame()
                              time_before_answer <<- 1
                                                 
                              if(length(shinyGUI)){
                                  dnames <- names(shinyGUI)
                                  gnames <- c('title', 'authors', 'instructions', 'itemtimer', 'incorrect','failpass','timemsg', 'firstpage', 'demographics',
                                              'demographics_inputIDs', 'temp_file', "time_remaining", "response_msg",
                                              'lastpage', 'css', 'stem_dims', 'forced_choice', 'stem_locations',
                                              'begin_message', 'ui', 'password', 'stem_default_format',
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
                                  if(!is.null(shinyGUI$itemtimer))
                                      itemtimer <<- shinyGUI$itemtimer
                                  if(!is.null(shinyGUI$incorrect))
                                      incorrect <<- shinyGUI$incorrect
                                  if(!is.null(shinyGUI$failpass))
                                      failpass <<- shinyGUI$failpass
                                  if(!is.null(shinyGUI$timemsg))
                                      timemsg <<- shinyGUI$timemsg
                                  if(!is.null(shinyGUI$begin_message))
                                      begin_message <<- shinyGUI$begin_message
                                  if(!is.null(shinyGUI$title))
                                      title <<- shinyGUI$title
                                  if(!is.null(shinyGUI$authors))
                                      author <<- shinyGUI$authors
                                  if(!is.null(shinyGUI$firstpage)) 
                                      firstpage <<- shinyGUI$firstpage
                                  if(!is.null(shinyGUI$time_remaining)) 
                                      time_remaining <<- shinyGUI$time_remaining
                                  if(!is.null(shinyGUI$response_msg)) 
                                      response_msg <<- shinyGUI$response_msg
                                  if(!is.null(shinyGUI$demographics)){
                                      demographics <<- shinyGUI$demographics
                                      demographic_inputIDs <<- shinyGUI$demographics_inputIDs
                                  }
                                  if(!is.null(shinyGUI$forced_choice))
                                      warning('forced_choice global option has been deprecated. 
                                           Please use the \"Forced\" column in df argument instead')
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
                                  if(!is.null(shinyGUI$max_password_attempts))
                                      max_password_attempts <<- shinyGUI$max_password_attempts
                              }
                          })
                      
)
