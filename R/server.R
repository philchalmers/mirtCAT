server <- function(input, output) {    
    
    output$Main <- renderUI({
        dynamicUi()
    })
    
    dynamicUi <- reactive({
        
        click <- input$Next
        
        #skip first page?
        if(!length(MCE$shinyGUI$firstpage)) click <- click + 1L
        
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
            return(list(h5("Click the action button to begin.")))
        }
        
        if(click == 3L) MCE$start_time <- proc.time()[3L]
        
        itemclick <- sum(!is.na(MCE$person$items_answered))
        
        # run survey
        if(click > 2L && !MCE$design@stop_now){
            if(itemclick >= 1L){
                pick <- MCE$person$items_answered[itemclick]
                name <- MCE$test@itemnames[pick]
                ip <- input[[name]]
                if(is.null(ip)) ip <- input[[paste0(MCE$invalid_count, '.TeMpInTeRnAl',name)]]
                if(!is.null(ip)){
                    MCE$person$raw_responses[pick] <- MCE$person$responses[pick] <- 
                        which(MCE$test@item_options[[pick]] %in% ip) - 1L
                    if(!is.na(MCE$test@item_answers[[pick]]) && 
                           MCE$test@item_class[pick] != 'nestlogit')
                        MCE$person$responses[pick] <- as.integer(ip %in% MCE$test@item_answers[[pick]])
                    
                    MCE$person$item_time[pick] <- proc.time()[3L] - MCE$start_time - 
                        sum(MCE$person$item_time)
                    
                    #update Thetas
                    MCE$person$Update.thetas(MCE$design, MCE$test)
                    if(MCE$shinyGUI$temp_file != '')
                        saveRDS(MCE$person, MCE$shinyGUI$temp_file)
                    MCE$design <- Update.stop_now(MCE$design, MCE$person)
                } else {
                    if(MCE$shinyGUI$forced_choice){
                        MCE$shift_back <- MCE$shift_back + 1L
                        MCE$invalid_count <- MCE$invalid_count + 1L
                        tmp <- buildShinyElements(MCE$shinyGUI$df[pick,], 
                                                  paste0(MCE$invalid_count, '.TeMpInTeRnAl',name))
                        return(list(p(MCE$shinyGUI$df[pick, 'Question']), tmp$questions))
                    } else {
                        MCE$person$item_time[pick] <- proc.time()[3L] - MCE$start_time - 
                            sum(MCE$person$item_time)
                        #update Thetas (same as above)
                        MCE$person$Update.thetas(MCE$design, MCE$test)
                        if(MCE$shinyGUI$temp_file != '')
                            saveRDS(MCE$person, MCE$shinyGUI$temp_file)
                        MCE$design <- Update.stop_now(MCE$design, MCE$person)
                        MCE$person$valid_item[pick] <- FALSE
                    }
                }
            } 
            
            MCE$design <- Next.stage(MCE$design, person=MCE$person, test=MCE$test, item=itemclick)
            
            if(!MCE$design@stop_now){
                item <- findNextCATItem(person=MCE$person, test=MCE$test, design=MCE$design,
                                        start=FALSE)
                MCE$person$items_answered[itemclick+1L] <- item
                return(list(p(MCE$shinyGUI$df[item,'Question']), MCE$shinyGUI$questions[[item]]))
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
        
        click <- input$Next - MCE$shift_back
        if(click > 0L)
            if(!length(MCE$shinyGUI$demographics)) click <- click + 1L
            
        if(!MCE$STOP){
            if(click > 2L && (click-2L) < MCE$test@length){
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
            return(list(src = MCE$shinyGUI$stem_locations[[click-2L]],
                        width = MCE$shinyGUI$width,
                        height = MCE$shinyGUI$height))
        }
        
    }, deleteFile = FALSE)
    
}