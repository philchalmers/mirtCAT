server <- function(input, output) {    
    
    output$Main <- renderUI({
        dynamicUi()
    })
    
    dynamicUi <- reactive({
        
        click <- input$Next
        
        #first page, ask for demographics, etc
        if(click == 0L){
            return(MCE$shinyGUI$firstpage)
        }
        
        #skip demographics page?
        if(!length(MCE$shinyGUI$demographics)) click <- click + 1L
        
        if(click == 1L){
            return(MCE$shinyGUI$demographics)
        }
        
        #store demographic results
        if(click == 2L){
            tmp <- list()
            for(tag in MCE$shinyGUI$demographic_inputIDs)
                tmp[[length(tmp) + 1L]] <- input[[tag]]
            names(tmp) <- MCE$shinyGUI$demographic_inputIDs
            MCE$person$field("demographics", as.data.frame(tmp))
            if(!is.null(MCE$last_demographics))
                MCE$person$demographics <- MCE$last_demographics
            if(MCE$shinyGUI$temp_file != '')
                saveRDS(MCE$person, MCE$shinyGUI$temp_file)
            return(list(h5("Click \'Next\' to start the survey.")))
        }
        
        if(click == 3L) MCE$start_time <- proc.time()[3L]
        
        itemclick <- sum(!is.na(MCE$person$items_answered))
        
        # run survey
        if(click > 2L && !MCE$design$stop_now){
            if(itemclick >= 1L){
                pick <- MCE$person$items_answered[itemclick]
                name <- MCE$test$itemnames[pick]
                ip <- input[[name]]
                if(!is.null(ip)){
                    MCE$person$raw_responses[pick] <- MCE$person$responses[pick] <- 
                        which(MCE$test$item_options[[pick]] %in% ip) - 1L
                    if(!is.na(MCE$test$item_answers[[pick]]) && 
                           MCE$test$item_class[pick] != 'nestlogit')
                        MCE$person$responses[pick] <- as.integer(ip == MCE$test$item_answers[[pick]])
                    
                    MCE$person$item_time[pick] <- proc.time()[3L] - MCE$start_time - 
                        sum(MCE$person$item_time)
                    
                    #update Thetas
                    MCE$person$Update.thetas()
                    if(MCE$shinyGUI$temp_file != '')
                        saveRDS(MCE$person, MCE$shinyGUI$temp_file)
                    if(itemclick > MCE$design$preCAT_nitems)
                        MCE$design$Update.stop_now()
                }
            } 
            
            MCE$design$Next.stage(item=itemclick)
            
            if(!MCE$design$stop_now){
                item <- findNextCATItem(person=MCE$person, test=MCE$test, design=MCE$design)
                MCE$person$items_answered[itemclick+1L] <- item
                return(MCE$shinyGUI$questions[[item]])
            }
        }
        
        #last page
        if(!MCE$STOP){
            MCE$STOP <- TRUE
            if(MCE$shinyGUI$temp_file != '')
                saveRDS(MCE$person, MCE$shinyGUI$temp_file)
            return(MCE$shinyGUI$lastpage)
        } else {
            stopApp()
            return(NULL)
        }
        
    }) 
        
    output$item_stem <- renderImage({
        
        click <- input$Next
        if(click > 0L)
            if(!length(MCE$shinyGUI$demographics)) click <- click + 1L
            
        if(!MCE$STOP){
            if(click > 2L && (click-2L) < MCE$test$length){
                empty <- is.na(MCE$shinyGUI$stem_locations[[
                    MCE$person$items_answered[[click-2L]]]])
            } else empty <- TRUE
        } else empty <- TRUE
        
        if(empty){
            outfile <- MCE$outfile
            png(outfile, width=1, height=1)
            dev.off()
            return(list(src = outfile,
                        contentType = 'image/png',
                        width = 1,
                        height = 1,
                        alt = ""))
        } else {
            return(list(src = MCE$shinyGUI$stem_locations[[click-2L]]))
        }
        
    }, deleteFile = FALSE)
    
}