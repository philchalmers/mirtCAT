ShinyGUI <- setRefClass("ShinyGUI", 
                      
                      fields = list(title = 'character',
                                    author = 'character',
                                    questions = 'list',
                                    firstpage = 'list',
                                    demographics = 'list',
                                    lastpage = 'list',
                                    stem_locations = 'character',
                                    delete_png = 'logical',
                                    demographic_tags = 'character'),
                      
                      methods = list(
                          initialize = function(questions, stem_locations_in = NULL,
                                                shinyGUI_list){
                              questions <<- questions
                              if(is.null(stem_locations_in))
                                  stem_locations_in <- as.character(rep(NA, length(questions)))
                              stem_locations <<- stem_locations_in
                              delete_png <<- c(TRUE, TRUE, is.na(stem_locations_in), 
                                               rep(TRUE, 20L))
                              title <<- 'Title of survery'
                              author <<- 'Author of survery'
                              demographic_tags <<- c('gender')
                              firstpage <<- list(h1('Welcome to the mirtCAT interface'),
                                                 'The following interface was created using the mirtCAT package. 
                                                 To cite the package use citation(\'mirtCAT\') in R.')
                              demographics <<- list(selectInput(inputId = 'gender',
                                                             label = 'Please select your gender.',
                                                             choices = c('', 'Male', 'Female', 'Other'),
                                                             selected = ''))
                              lastpage <<- list(h5("End of survey. Click \'Next\' to save
                                                       results and close application."))
                                                 
                              if(length(shinyGUI_list)){
                                  if(!is.null(shinyGUI_list$firstpage)) 
                                      firstpage <<- shinyGUI_list$firstpage
                                  if(!is.null(shinyGUI_list$demographics)){
                                      demographics <<- shinyGUI_list$demographics
                                      demographic_tags <<- shinyGUI_list$demographics_tags
                                  }
                                  if(!is.null(shinyGUI_list$lastpage)) 
                                      lastpage <<- shinyGUI_list$lastpage
                              }
                          })
                      
)