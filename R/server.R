server <- function(input, output) {    
    
    output$Main <- renderUI({
        dynamicUi()
    })
    
    dynamicUi <- reactive({
        
        #first page, ask for demographics, etc
        if(input$Next == 0L){
            return(MCE$shinyGUI$firstpage)
        }
        
        #store demographic results
        if(input$Next == 1L){
            tmp <- list()
            for(tag in MCE$shinyGUI$demographic_tags)
                tmp[[length(tmp) + 1L]] <- input[[tag]]
            names(tmp) <- MCE$shinyGUI$demographic_tags
            MCE$person$field("demographics", as.data.frame(tmp))
            return(list(h5("Click \'Next\' to start the survey.")))
        }
        
        # run survey
        if(input$Next > 1L && input$Next < (MCE$test$length + 2L) && !MCE$person$stop_now){
            if(input$Next > 2L){
                pick <- MCE$person$items_answered[input$Next-2L]
                name <- MCE$test$itemnames[pick]
                ip <- input[[name]]
                if(length(MCE$test$item_options[[pick]]) > 1L){
                    response <- which(MCE$test$item_options[[pick]] %in% ip)
                } else {
                    response <- as.integer(ip == MCE$test$item_answers[[pick]])
                    if(is.na(response)) response <- NaN
                }
                MCE$person$responses[pick] <- response
            }            
            if(MCE$test$adaptive){
                
            } else {
                item <- as.integer(input$Next - 1L)
            }
            MCE$person$items_answered[input$Next-1L] <- item
            return(MCE$shinyGUI$questions[[item]])
        }
        
        #cleanup last response 
        if((input$Next-2L) <= MCE$test$length){
            pick <- MCE$person$items_answered[input$Next-2L]
            name <- MCE$test$itemnames[pick]
            ip <- input[[name]]
            if(length(MCE$test$item_options[[pick]]) > 1L){
                response <- which(MCE$test$item_options[[pick]] %in% ip)
            } else {
                response <- as.integer(ip == MCE$test$item_answers[[pick]])
                if(is.na(response)) response <- NaN
            }
            MCE$person$responses[pick] <- response
        }
        
        #last page
        return(MCE$shinyGUI$lastpage)
    }) 
}