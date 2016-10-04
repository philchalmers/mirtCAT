server <- function(input, output) {    
    
    output$Main <- renderUI({
        dynamicUi()
    })
    
    dynamicUi <- reactive({
        
        click <- input$Next
        
        if(length(.MCE$shinyGUI$password)){
            if(click == 0L){
                if(nrow(.MCE$shinyGUI$password) > 1L)
                    return(list(textInput('UsErNaMe', label = "Login Name:"),
                                passwordInput('PaSsWoRd', 'Password:')))
                else return(passwordInput('PaSsWoRd', 'Password:'))
            } else if(click == 1L){
                .MCE$verified <- verifyPassword(input, .MCE$shinyGUI$password)
            }
            click <- click - 1L
        }
        
        if(!.MCE$verified)
            return(h3('Incorrect Login Name/Password. Please restart the application and try again.'))
        
        if(.MCE$resume_file && click < 1L){
            return(list(h5("Click the action button to continue with your session.")))
        } else {
            #skip first page? Demographics, etc
            if(!length(.MCE$shinyGUI$firstpage)) click <- click + 1L
            if(click == 0L)
                return(.MCE$shinyGUI$firstpage)
            
            #skip demographics page?
            if(!length(.MCE$shinyGUI$demographics)) click <- click + 1L
            if(click == 1L)
                return(.MCE$shinyGUI$demographics)
            
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
            }
            
            if(.MCE$shinyGUI$begin_message == "") click <- click + 1L
            if(click == 2L)
                return(list(h5(.MCE$shinyGUI$begin_message)))
        } #end normal start
        
        if(is.null(.MCE$start_time))
            .MCE$start_time <- proc.time()[3L]
        
        if(.MCE$resume_file){
            .MCE$resume_file <- FALSE
            item <- max(which(!is.na(.MCE$person$items_answered)))
            stemOutput <- stemContent(item)
            return(list(stemOutput,.MCE$shinyGUI$df$Question[[item]], 
                        .MCE$shinyGUI$questions[[item]]))
        }
        
        itemclick <- sum(!is.na(.MCE$person$items_answered))
        
        if(FALSE){
            cat('\nclick = ', click)
            cat('\titemclick = ', itemclick)
        }
        
        # run survey
        if(click > 2L && !.MCE$design@stop_now){
            if(itemclick >= 1L){
                pick <- .MCE$person$items_answered[itemclick]
                name <- .MCE$test@itemnames[pick]
                ip <- unname(input[[name]])
                if(.MCE$shinyGUI$df$Type[pick] == 'select' && .MCE$shinyGUI$forced_choice && ip == "")
                    ip <- NULL
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
                    .MCE$person$item_time[pick] <- proc.time()[3L] - .MCE$start_time
                    .MCE$start_time <- NULL
                    
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
                        stemOutput <- stemContent(pick)
                        return(list(stemOutput, .MCE$shinyGUI$df$Question[[pick]], 
                                    tmp$questions))
                    } else {
                        .MCE$person$item_time[pick] <- proc.time()[3L] - .MCE$start_time
                        .MCE$start_time <- NULL
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
                item <- findNextCATItem(person=.MCE$person, test=.MCE$test, 
                                        design=.MCE$design, start=FALSE)
                if(!is.null(attr(item, 'design'))) .MCE$design <- attr(item, 'design')
                if(is.na(item)){
                    .MCE$design@stop_now <- TRUE
                } else {
                    if(is.null(.MCE$start_time))
                        .MCE$start_time <- proc.time()[3L]
                    .MCE$person$items_answered[itemclick+1L] <- as.integer(item)
                    if(.MCE$shinyGUI$temp_file != '')
                        saveRDS(.MCE$person, .MCE$shinyGUI$temp_file)
                    stemOutput <- stemContent(item)
                    return(list(stemOutput, 
                                .MCE$shinyGUI$df$Question[[item]], 
                                .MCE$shinyGUI$questions[[item]]))
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
}