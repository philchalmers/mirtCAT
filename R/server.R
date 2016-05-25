server <- function(input, output) {    
    
    output$Main <- renderUI({
        dynamicUi()
    })
    
    dynamicUi <- reactive({
        
        click <- input$Next
        
        #skip first page?
        if(!length(.MCE$shinyGUI$firstpage)) click <- click + 1L
        
        #first page, ask for demographics, etc
        if(click == 0L){
            return(.MCE$shinyGUI$firstpage)
        }
        
        #skip demographics page?
        if(!length(.MCE$shinyGUI$demographics)) click <- click + 1L
        
        if(click == 1L){
            return(.MCE$shinyGUI$demographics)
        }
        
        #store demographic results
        if(click == 2L){
            tmp <- list()
            for(tag in .MCE$shinyGUI$demographic_inputIDs)
                tmp[[length(tmp) + 1L]] <- input[[tag]]
            names(tmp) <- .MCE$shinyGUI$demographic_inputIDs
            .MCE$person$field("demographics", as.data.frame(tmp))
            if(!is.null(.MCE$last_demographics))
                .MCE$person$demographics <- .MCE$last_demographics
            if(.MCE$shinyGUI$temp_file != '')
                saveRDS(.MCE$person, .MCE$shinyGUI$temp_file)
            return(list(h5(.MCE$shinyGUI$begin_message)))
        }
        
        if(click == 3L) .MCE$start_time <- proc.time()[3L]
        
        if(.MCE$resume_file){
            .MCE$resume_file <- FALSE
            item <- max(which(!is.na(.MCE$person$items_answered)))
            return(list(.MCE$shinyGUI$df$Question[[item]], .MCE$shinyGUI$questions[[item]]))
        }
        
        itemclick <- sum(!is.na(.MCE$person$items_answered))
        
        # run survey
        if(click > 2L && !.MCE$design@stop_now){
            if(itemclick >= 1L){
                pick <- .MCE$person$items_answered[itemclick]
                name <- .MCE$test@itemnames[pick]
                ip <- unname(input[[name]])
                if(is.null(ip)) ip <- input[[paste0(.MCE$invalid_count, '.TeMpInTeRnAl',name)]]
                if(!is.null(ip)){
                    ip <- as.character(ip)
                    nanswers <- length(ip)
                    .MCE$person$raw_responses[pick] <- paste0(ip, collapse = '; ')
                    if(!is.null(.MCE$test@item_options[[pick]])){
                        if(nanswers > 1L)
                            .MCE$person$responses[pick] <- sum(.MCE$test@item_options[[pick]] %in% ip)
                        else .MCE$person$responses[pick] <- which(.MCE$test@item_options[[pick]] %in% ip) - 1L
                    }
                    if(!is.na(.MCE$test@item_answers[[pick]]) && 
                           .MCE$test@item_class[pick] != 'nestlogit'){
                        if(nanswers > 1L)
                            .MCE$person$responses[pick] <- as.integer(sum(ip %in% .MCE$test@item_answers[[pick]]))
                        else .MCE$person$responses[pick] <- as.integer(ip %in% .MCE$test@item_answers[[pick]])
                    }
                    .MCE$person$item_time[pick] <- proc.time()[3L] - .MCE$start_time - 
                        sum(.MCE$person$item_time)
                    
                    #update Thetas
                    .MCE$person$Update.thetas(.MCE$design, .MCE$test)
                    if(.MCE$shinyGUI$temp_file != '')
                        saveRDS(.MCE$person, .MCE$shinyGUI$temp_file)
                    .MCE$design <- Update.stop_now(.MCE$design, .MCE$person)
                } else {
                    if(.MCE$shinyGUI$forced_choice && df$Type[pick] != 'none'){
                        .MCE$shift_back <- .MCE$shift_back + 1L
                        .MCE$invalid_count <- .MCE$invalid_count + 1L
                        tmp <- lapply(.MCE$shinyGUI$df, function(x, pick) x[pick], pick=pick)
                        tmp <- buildShinyElements(tmp, paste0(.MCE$invalid_count, '.TeMpInTeRnAl', name))
                        return(list(.MCE$shinyGUI$df$Question[[pick]], tmp$questions))
                    } else {
                        .MCE$person$item_time[pick] <- proc.time()[3L] - .MCE$start_time - 
                            sum(.MCE$person$item_time)
                        #update Thetas (same as above)
                        .MCE$person$Update.thetas(.MCE$design, .MCE$test)
                        if(.MCE$shinyGUI$temp_file != '')
                            saveRDS(.MCE$person, .MCE$shinyGUI$temp_file)
                        .MCE$design <- Update.stop_now(.MCE$design, .MCE$person)
                        .MCE$person$valid_item[pick] <- FALSE
                    }
                }
            } 
            
            .MCE$design <- Next.stage(.MCE$design, person=.MCE$person, test=.MCE$test, item=itemclick)
            
            if(!.MCE$design@stop_now){
                item <- findNextCATItem(person=.MCE$person, test=.MCE$test, design=.MCE$design,
                                        criteria=.MCE$design@criteria, start=FALSE)
                if(is.na(item)){
                    .MCE$design@stop_now <- TRUE
                } else {
                    .MCE$person$items_answered[itemclick+1L] <- item
                    if(.MCE$shinyGUI$temp_file != '')
                        saveRDS(.MCE$person, .MCE$shinyGUI$temp_file)
                    return(list(.MCE$shinyGUI$df$Question[[item]], .MCE$shinyGUI$questions[[item]]))
                }
            }
        }
        
        #last page
        if(!.MCE$STOP){
            .MCE$STOP <- TRUE
            if(!is.null(.MCE$final_fun)){
                ret <- mirtCAT_post_internal(person=.MCE$person, design=.MCE$design)
                .MCE$final_fun(person = ret)
            }
            if(.MCE$shinyGUI$temp_file != '')
                file.remove(.MCE$shinyGUI$temp_file)
            return(.MCE$shinyGUI$lastpage(person=.MCE$person))
        } else {
            if(.MCE$shinyGUI$stopApp) stopApp()
            else return(.MCE$shinyGUI$lastpage(person=.MCE$person))
            return(NULL)
        }
        
    }) 
        
    output$item_stem_html <- renderUI({
        
        click <- input$Next - .MCE$shift_back
        if(click > 0L)
            if(!length(.MCE$shinyGUI$demographics)) click <- click + 1L
        
        if(!.MCE$STOP){
            if(click > 2L && (click-2L) < .MCE$test@length){
                pick <- force(.MCE$person$items_answered[[click-2L]])
                file <- .MCE$shinyGUI$stem_locations[pick]
                empty <- is.na(file)
                if(!empty){
                    if(grepl('\\.[mM][dD]$', file)){
                        suppressWarnings(markdown::markdownToHTML(file=file, output=.MCE$outfile2, 
                                                 fragment.only = TRUE))
                        contents <- readLines(.MCE$outfile2, warn = FALSE)
                        return(HTML(contents))
                    } else if(grepl('\\.[hH][tT][mM][lL]$', file)){
                        contents <- readLines(file, warn = FALSE)
                        return(HTML(contents))
                    } else empty <- TRUE
                }
            } else empty <- TRUE
        } else empty <- TRUE
        
        return(' ')
        
    })
    
}