ShinyGUI <- setRefClass("ShinyGUI", 
                      
                      fields = list(title = 'character',
                                    author = 'character',
                                    questions = 'list',
                                    df = 'list',
                                    firstpage = 'list',
                                    demographics = 'list',
                                    lastpage = 'function',
                                    instructions = 'character',
                                    stem_locations = 'character',
                                    delete_png = 'logical',
                                    demographic_inputIDs = 'character',
                                    temp_file = 'character',
                                    width = 'numeric',
                                    height = 'numeric',
                                    forced_choice = 'logical',
                                    css = 'character'),
                      
                      methods = list(
                          initialize = function(questions, df, shinyGUI){
                              'Initialize the shiny GUI given questions, df, and shinyGUI list'
                              questions <<- questions
                              df <<- df
                              forced_choice <<- TRUE
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
                              delete_png <<- c(TRUE, TRUE, TRUE, is.na(stem_locations), 
                                               rep(TRUE, 20L))
                              title <<- 'mirtCAT'
                              author <<- 'Author information'
                              instructions <<- c("Instructions:",
                                                 "To progress through the interface, click on the action button below.",
                                                 "Next")
                              demographic_inputIDs <<- character(0)
                              firstpage <<- list(h1('Welcome to the mirtCAT interface'),
                                                 'The following interface was created using the mirtCAT package. 
                                                 To cite the package use citation(\'mirtCAT\') in R.')
                              demographics <<- list()
                              lastpage <<- function(person) 
                                            return(list(h5("You have successfully completed the interface.
                                                   Click the action button to terminate the application.")))
                              temp_file <<- ''
                              css <<- ''
                                                 
                              if(length(shinyGUI)){
                                  dnames <- names(shinyGUI)
                                  gnames <- c('title', 'authors', 'instructions', 'firstpage', 'demographics',
                                              'demographics_inputIDs', 'max_time', 'temp_file', 'resume_file',
                                              'lastpage', 'css', 'stem_dims', 'forced_choice', 'stem_locations')
                                  if(!all(dnames %in% gnames))
                                      stop('The following inputs to shinyGUI are invalid: ',
                                           paste0(dnames[!(dnames %in% gnames)], ' '), call.=FALSE)
                                  if(!is.null(shinyGUI$instructions))
                                      instructions <<- shinyGUI$instructions
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
                                  if(!is.null(shinyGUI$lastpage)) 
                                      lastpage <<- shinyGUI$lastpage
                                  if(!is.null(shinyGUI$temp_file))
                                      temp_file <<- shinyGUI$temp_file
                                  if(!is.null(shinyGUI$css))
                                      css <<- shinyGUI$css
                              }
                          })
                      
)