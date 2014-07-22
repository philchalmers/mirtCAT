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
        
        itemclick <- input$Next - 2L
        
        # run survey
        if(input$Next > 1L && !MCE$design$stop_now){
            if(itemclick >= 1L){
                pick <- MCE$person$items_answered[itemclick]
                name <- MCE$test$itemnames[pick]
                ip <- input[[name]]
                MCE$person$raw_responses[pick] <- MCE$person$responses[pick] <- 
                    which(MCE$test$item_options[[pick]] %in% ip) - 1L
                if(!is.na(MCE$test$item_answers[[pick]]))
                    MCE$person$responses[pick] <- as.integer(ip == MCE$test$item_answers[[pick]])
                
                #update Thetas
                MCE$person$Update.thetas()
                if(itemclick > MCE$design$preCAT_nitems)
                    MCE$design$Update.stop_now()
            } 
            
            MCE$design$Next.stage(item=itemclick)
            
            if(!MCE$design$stop_now){
                item <- findNextCATItem(person=MCE$person, test=MCE$test, 
                                        lastitem=itemclick, criteria=MCE$design$criteria)
                MCE$person$items_answered[itemclick+1L] <- item
                return(MCE$shinyGUI$questions[[item]])
            }
        }
        
        #last page
        if(!MCE$STOP){
            MCE$STOP <- TRUE
            return(MCE$shinyGUI$lastpage)
        } else {
            stopApp()
            return(NULL)
        }
        
    }) 
    
    output$item_stem <- renderImage({
        
        outfile <- tempfile(fileext='.png')
        
        if(!MCE$STOP){
            if(input$Next > 1L && (input$Next-1L) < MCE$test$length){
                empty <- is.na(MCE$shinyGUI$stem_locations[[
                    MCE$person$items_answered[[input$Next-1L]]]])
            } else empty <- TRUE
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
        
    }, deleteFile = if(!MCE$STOP){ if(input$Next > 1L && (input$Next-1L) < MCE$test$length)
            is.na(MCE$shinyGUI$stem_locations[[MCE$person$items_answered[[input$Next-1L]]]]) 
        else TRUE} else TRUE)
    
}