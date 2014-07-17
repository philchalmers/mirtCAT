ShinyGUI <- setRefClass("ShinyGUI", 
                      
                      fields = list(title = 'character',
                                    author = 'character',
                                    questions = 'list',
                                    firstpage = 'list',
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
                              demographic_tags <<- c('name', 'gender')
                              firstpage <<- list(textInput(inputId = 'name', 
                                                           label = 'What is your name?',
                                                           value = ''),
                                                 selectInput(inputId = 'gender',
                                                             label = 'Please select your gender.',
                                                             choices = c('', 'Male', 'Female', 'Other'),
                                                             selected = ''))
                              lastpage <<- list(h5("End of survey. Click \'Next\' to save
                                                       results and close application."))
                                                 
                              if(length(shinyGUI_list)){
                                  if(!is.null(shinyGUI_list$firstpage)){
                                      firstpage <<- shinyGUI_list$firstpage
                                      demographic_tags <<- do.call(c, lapply(shinyGUI_list$firstpage, 
                                            function(x) x[[1L]][[1L]]$attribs$`for`))
                                  }
                                  if(is.null(shinyGUI_list$lastpage)) 
                                      lastpage <<- shinyGUI_list$lastpage
                              }
                          })
                      
)