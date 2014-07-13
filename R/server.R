server <- function(input, output) {    
    
    output$item_stem <- renderImage({
        
        outfile <- tempfile(fileext='.png')
        
        if(input$Next > 1L && (input$Next-1L) < MCE$test$length){
            empty <- is.na(MCE$shinyGUI$stem_locations[[input$Next-1L]])
        } else empty <- TRUE
        
        if(empty){
            png(outfile, width=1, height=1)
            dev.off()
            return(list(src = outfile,
                        contentType = 'image/png',
                        width = 1,
                        height = 1,
                        alt = ""))
        } else {
            return(list(src = MCE$shinyGUI$stem_locations[[input$Next-1L]],
                        contentType = 'image/png',
                        width = 400,
                        height = 400,
                        alt = ""))
        }
        
    }, deleteFile = if(input$Next > 1L && (input$Next-1L) < MCE$test$length)
        is.na(MCE$shinyGUI$stem_locations[[input$Next-1L]]) else TRUE)
    
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
                MCE$person$raw_responses[pick] <- MCE$person$responses[pick] <- 
                    which(MCE$test$item_options[[pick]] %in% ip) - 1L
                if(!is.na(MCE$test$item_answers[[pick]]))
                    MCE$person$responses[pick] <- as.integer(ip == MCE$test$item_answers[[pick]])
                
                #update Thetas
                MCE$person$Update.thetas(MCE$test)
                if(MCE$test$adaptive) 
                    MCE$person$Update.stop_now()
            } 
            
            if(MCE$test$adaptive){
                item <- findNextCATItem(person=MCE$person, test=MCE$test)
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
            MCE$person$raw_responses[pick] <- MCE$person$responses[pick] <- 
                which(MCE$test$item_options[[pick]] %in% ip) - 1L
            if(!is.na(MCE$test$item_answers[[pick]]))
                MCE$person$responses[pick] <- as.integer(ip == MCE$test$item_answers[[pick]])
            
            #update Thetas
            MCE$person$Update.thetas(MCE$test)
        }
        
        #last page
        return(MCE$shinyGUI$lastpage)
    }) 
}