ShinyGUI <- setRefClass("ShinyGUI", 
                      
                      fields = list(title = 'character',
                                    author = 'character',
                                    questions = 'list',
                                    firstpage = 'list',
                                    lastpage = 'list',
                                    demographic_tags = 'character'),
                      
                      methods = list(
                          initialize = function(questions, firstpage=NULL, lastpage=NULL){
                              questions <<- questions
                              if(is.null(firstpage)){
                                  demographic_tags <<- c('name', 'gender')
                                  firstpage <<- list(textInput(inputId = 'name', 
                                                               label = 'What is your name?',
                                                               value = ''),
                                                     selectInput(inputId = 'gender',
                                                                 label = 'Please select your gender.',
                                                                 choices = c('', 'Male', 'Female', 'Other'),
                                                                 selected = '')                                                     
                                  )
                              } else {
                                  firstpage <<- firstpage
                                  demographic_tags <<- do.call(c, lapply(firstpage, 
                                                                         function(x) x[[1L]][[1L]]$attribs$`for`))
                              }
                              if(is.null(lastpage)){
                                  lastpage <<- list(h5("End of test/survey"))
                              } else lastpage <<- lastpage
                              title <<- 'Title of survery'
                              author <<- 'Author of survery'
                          })
                      
)