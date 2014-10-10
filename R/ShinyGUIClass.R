ShinyGUI <- setRefClass("ShinyGUI", 
                      
                      fields = list(title = 'character',
                                    author = 'character',
                                    questions = 'list',
                                    firstpage = 'list',
                                    demographics = 'list',
                                    lastpage = 'list',
                                    stem_locations = 'character',
                                    delete_png = 'logical',
                                    demographic_inputIDs = 'character',
                                    temp_file = 'character',
                                    css = 'character'),
                      
                      methods = list(
                          initialize = function(questions, shinyGUI){
                              questions <<- questions
                              if(is.null(shinyGUI$stem_locations)){
                                  stem_locations <<- as.character(rep(NA, length(questions)))
                              } else stem_locations <<- shinyGUI$stem_locations
                              delete_png <<- c(TRUE, TRUE, TRUE, is.na(stem_locations), 
                                               rep(TRUE, 20L))
                              title <<- 'mirtCAT'
                              author <<- 'Authors of survey'
                              demographic_inputIDs <<- c('gender')
                              firstpage <<- list(h1('Welcome to the mirtCAT interface'),
                                                 'The following interface was created using the mirtCAT package. 
                                                 To cite the package use citation(\'mirtCAT\') in R.')
                              demographics <<- list(selectInput(inputId = 'gender',
                                                             label = 'Please select your gender.',
                                                             choices = c('', 'Male', 'Female', 'Other'),
                                                             selected = ''))
                              lastpage <<- list(h5("End of survey/test. Click \'Next\' to save
                                                       results and close application."))
                              temp_file <<- ''
                              css <<- ''
                                                 
                              if(length(shinyGUI)){
                                  if(!is.null(shinyGUI$title))
                                      title <<- shinyGUI$title
                                  if(!is.null(shinyGUI$authors))
                                      author <<- shinyGUI$authors
                                  if(!is.null(shinyGUI$firstpage)) 
                                      firstpage <<- shinyGUI$firstpage
                                  if(!is.null(shinyGUI$demographics)){
                                      demographics <<- shinyGUI$demographics
                                      if(length(shinyGUI$demographics)){
                                          demographic_inputIDs <<- shinyGUI$demographics_inputIDs 
                                      } else {
                                          demographic_inputIDs <<- character(0)
                                      }
                                  }
                                  if(!is.null(shinyGUI$lastpage)) 
                                      lastpage <<- shinyGUI$lastpage
                                  if(!is.null(shinyGUI$temp_file))
                                      temp_file <<- shinyGUI$temp_file
                                  if(!is.null(shinyGUI$css))
                                      css <<- shinyGUI$css
                              }
                          })
                      
)