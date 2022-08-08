server <- function(input, output, session) {    
    
    setDebug(level = 0)
    printDebug("Top of server")
    
    sessionName <- createSessionName()
    
    # for local use
    .MCE$currentSessionName <- sessionName
    
    .MCE[[sessionName]] <- as.environment(as.list(.MCE[['MASTER']], all.names=TRUE))
    .MCE[[sessionName]]$person <- deepCopyPerson(.MCE[['MASTER']]$person)
    .MCE[[sessionName]]$initial_start_time <- Sys.time()
    
    session$onSessionEnded(function() {
        printDebug("onSessionEnded")
        if(!.MCE[[sessionName]]$design@stop_now){
            message('WARNING: mirtCAT GUI session unexpectedly terminated early')
            .MCE[[sessionName]]$person$terminated_sucessfully <- FALSE
        } else .MCE[[sessionName]]$person$terminated_sucessfully <- TRUE
        .MCE[['COMPLETED']] <- .MCE[[sessionName]]
        .MCE[['COMPLETED']]$person <- deepCopyPerson(.MCE[[sessionName]]$person) 
        .MCE[[sessionName]] <- NULL
        if(!.MCE[['MASTER']]$host_server) stopApp()
        invisible(NULL)
    })
    
    output$Main <- renderUI({
        dynamicUi()
    })
    
    output$currentTime <- renderText({
        printDebug("currentTime", 3)
        invalidateLater(1000, session)
        if(.MCE[[sessionName]]$person$terminated_sucessfully)
            return(NULL)
        delta_time <- as.integer(Sys.time() - .MCE[[sessionName]]$initial_start_time)
        delta_msg <- .MCE[[sessionName]]$shinyGUI$timemsg
        if(is.finite(.MCE[[sessionName]]$design@max_time)){
            diff <- .MCE[[sessionName]]$design@max_time - delta_time
            if(diff < 0){
                diff <- 0
                .MCE[[sessionName]]$design@stop_now <- TRUE
            }
            return(paste0(.MCE[[sessionName]]$shinyGUI$time_remaining,
                      formatTime(diff, delta_msg)))
        } else return(NULL)
    })
    
    output$itemTime <- renderText({
        printDebug("itemTime", 3)
        invalidateLater(200, session)
        if(.MCE[[sessionName]]$person$terminated_sucessfully || .MCE[[sessionName]]$STOP)
            return(NULL)
        item <- .MCE[[sessionName]]$item
        delta_msg <- .MCE[[sessionName]]$shinyGUI$timemsg
        if(!is.null(item) && !is.na(item) && .MCE[[sessionName]]$shinyGUI$timer[item] > 0){
            delta_time <- .MCE[[sessionName]]$shinyGUI$timer[item] - 
                                  as.numeric(Sys.time() - .MCE[[sessionName]]$item_start_time,units = 'secs')
            if(delta_time < .3 && is.na(.MCE[[sessionName]]$person$raw_responses[item]) && 
               .MCE[[sessionName]]$test@has_answers[item])
                .MCE[[sessionName]]$person$responses[item] <- 0L
            if(delta_time < 0){
                delta_time <- 0
                if(is.na(.MCE[[sessionName]]$person$raw_responses[item]) && 
                   .MCE[[sessionName]]$test@has_answers[item])
                    .MCE[[sessionName]]$person$responses[item] <- 0L
            }
            return(paste0(.MCE[[sessionName]]$shinyGUI$itemtimer,
                          formatTime(as.integer(delta_time), delta_msg)))
        } else return(NULL)
    })
    
    dynamicUi <- reactive({
        
        printDebug("Top of dynamicUI")
        
        click <- input$Next
        
        if(!length(.MCE[[sessionName]]$person$clientData)){
            .MCE[[sessionName]]$person$clientData <- 
                list(url_hostname = session$clientData$url_hostname, 
                     url_port = session$clientData$url_port, 
                     url_pathname = session$clientData$url_pathname, 
                     url_search = session$clientData$url_search, 
                     url_hash_initial = session$clientData$url_hash_initial,
                     url_hash = session$clientData$url_hash 
                )
        }
        
        if(length(.MCE[[sessionName]]$shinyGUI$password)){
            printDebug("Password block")
            if(click == 0L){
                .MCE[[sessionName]]$verified <- FALSE
                if(nrow(.MCE[[sessionName]]$shinyGUI$password) > 1L)
                    return(list(textInput('UsErNaMe', label = "Login Name:"),
                                passwordInput('PaSsWoRd', 'Password:')))
                else return(passwordInput('PaSsWoRd', 'Password:'))
            } else if(!.MCE[[sessionName]]$verified){
                .MCE[[sessionName]]$person$password_attempts <- 
                    .MCE[[sessionName]]$person$password_attempts + 1L
                .MCE[[sessionName]]$verified <- verifyPassword(input, 
                                                               .MCE[[sessionName]]$shinyGUI$password,
                                                               sessionName)
                if(!.MCE[[sessionName]]$verified && .MCE[[sessionName]]$person$password_attempts < 
                   .MCE[[sessionName]]$shinyGUI$max_password_attempts){
                    attempts_remaining <- .MCE[[sessionName]]$shinyGUI$max_password_attempts - 
                        .MCE[[sessionName]]$person$password_attempts
                    if(nrow(.MCE[[sessionName]]$shinyGUI$password) > 1L)
                        return(list(textInput("UsErNaMe", label = "Login Name:"),
                                    passwordInput("PaSsWoRd", 'Password:'),
                                    HTML(paste0("<p style='color:red;'> <em>", 
                                                sprintf(.MCE[[sessionName]]$shinyGUI$failpass,
                                                        attempts_remaining)), "</em> </p>")))
                    else {
                        return(list(passwordInput("PaSsWoRd", 'Password:'),
                                    HTML(paste0("<p style='color:red;'> <em>", 
                                                sprintf(.MCE[[sessionName]]$shinyGUI$failpass,
                                                        attempts_remaining)), "</em> </p>")))
                    }
                }
            }
            click <- click - .MCE[[sessionName]]$person$password_attempts
        }
        
        if(!.MCE[[sessionName]]$verified)
            return(h3('Login Name/Password were incorrect. Please restart the application and try again.'))
        
        if(.MCE[[sessionName]]$resume_file && click < 1L){
            return(list(h5("Click the action button to continue with your session.")))
        } else {
            printDebug("Demographics")
            #skip first page? Demographics, etc
            if(!length(.MCE[[sessionName]]$shinyGUI$firstpage)) click <- click + 1L
            if(click == 0L)
                return(.MCE[[sessionName]]$shinyGUI$firstpage)
            
            #skip demographics page?
            if(!length(.MCE[[sessionName]]$shinyGUI$demographics)) click <- click + 1L
            if(click == 1L)
                return(.MCE[[sessionName]]$shinyGUI$demographics)
            
            #store demographic results
            if(click == 2L){
                tmp <- list()
                for(tag in .MCE[[sessionName]]$shinyGUI$demographic_inputIDs)
                    tmp[[length(tmp) + 1L]] <- input[[tag]]
                names(tmp) <- .MCE[[sessionName]]$shinyGUI$demographic_inputIDs
                .MCE[[sessionName]]$person$field("demographics", as.data.frame(tmp))
                if(!is.null(.MCE[[sessionName]]$last_demographics))
                    .MCE[[sessionName]]$person$demographics <- .MCE[[sessionName]]$last_demographics
                if(.MCE[[sessionName]]$shinyGUI$temp_file != '')
                    saveRDS(.MCE[[sessionName]]$person, .MCE[[sessionName]]$shinyGUI$temp_file)
            }
            
            if(.MCE[[sessionName]]$shinyGUI$begin_message == "") click <- click + 1L
            if(click == 2L)
                return(list(h5(.MCE[[sessionName]]$shinyGUI$begin_message)))
        } #end normal start
        
        if(is.null(.MCE[[sessionName]]$start_time))
            .MCE[[sessionName]]$start_time <- proc.time()[3L]
        
        if(.MCE[[sessionName]]$resume_file){
            .MCE[[sessionName]]$prevClick <- -999L
            .MCE[[sessionName]]$resume_file <- FALSE
            item <- max(which(!is.na(.MCE[[sessionName]]$person$items_answered)))
            stemOutput <- stemContent(item, sessionName=sessionName)
            return(list(stemOutput,.MCE[[sessionName]]$shinyGUI$df$Rendered_Question[[item]], 
                        .MCE[[sessionName]]$shinyGUI$questions[[item]]))
        }
        
        itemclick <- sum(!is.na(.MCE[[sessionName]]$person$items_answered))
        
        if(FALSE){
            cat('\nclick = ', click)
            cat('\titemclick = ', itemclick)
        }
        
        # run survey
        printDebug("Response block")
        outmessage <- HTML(paste0("<p style='color:red;'> <em>", 
                                  .MCE[[sessionName]]$shinyGUI$response_msg, "</em> </p>"))
        if(click > 2L && !.MCE[[sessionName]]$design@stop_now && !.MCE[[sessionName]]$STOP){
            if(itemclick >= 1L){
                printDebug("Collect response")
                pick <- .MCE[[sessionName]]$person$items_answered[itemclick]
                name <- .MCE[[sessionName]]$test@itemnames[pick]
                ip <- unname(input[[name]])
                if(.MCE[[sessionName]]$shinyGUI$df$Type[pick] %in% c('select', 'rankselect') && 
                   .MCE[[sessionName]]$shinyGUI$forced_choice && !is.null(ip) && ip == "")
                    ip <- NULL
                if(.MCE[[sessionName]]$invalid_count > 0L)
                    ip <- input[[paste0(.MCE[[sessionName]]$invalid_count, '.TeMpInTeRnAl',name)]]
                if(!is.null(ip) && .MCE[[sessionName]]$prevClick != click && 
                   .MCE[[sessionName]]$shinyGUI$df$Type[pick] == "rankselect"){
                    nopts <- length(.MCE[[sessionName]]$test@item_options[[pick]]) - 1L
                    for(opt in 2L:nopts){
                        if(.MCE[[sessionName]]$invalid_count > 0L) ip <- 
                                c(ip, input[[paste0(.MCE[[sessionName]]$invalid_count, '.TeMpInTeRnAl',name,"_", opt)]])
                        else ip <- c(ip, input[[paste0(name, "_", opt)]])
                    }
                    if(length(ip) != length(unique(ip))){
                        outmessage <- HTML("<p style='color:red;'><em>Please provide unique rankings for each response.</em></p>")
                        ip <- NULL
                    } 
                }
                if(.MCE[[sessionName]]$shinyGUI$forced_choice && .MCE[[sessionName]]$shinyGUI$df$Type[pick] %in% c('text', 'textArea'))
                    if(ip == "") ip <- NULL
                diff_item_time <- (proc.time()[3L] - .MCE[[sessionName]]$start_time)
                item_time_valid <- .MCE[[sessionName]]$shinyGUI$time_before_answer <= diff_item_time
                # clickedNext <- .MCE[[sessionName]]$prevClick != click
                if(!is.null(ip) && item_time_valid){
                    printDebug("Observed response collected")
                    ip <- as.character(ip)
                    nanswers <- length(ip)
                    .MCE[[sessionName]]$person$raw_responses[pick] <- paste0(ip, collapse = '; ')
                    if(!is.null(.MCE[[sessionName]]$test@item_options[[pick]])){
                        if(nanswers > 1L)
                            .MCE[[sessionName]]$person$responses[pick] <- sum(.MCE[[sessionName]]$test@item_options[[pick]] %in% ip)
                        else .MCE[[sessionName]]$person$responses[pick] <- which(.MCE[[sessionName]]$test@item_options[[pick]] %in% ip) - 1L
                    }
                    if(.MCE[[sessionName]]$test@item_class[pick] != 'nestlogit'){
                        if(is.function(.MCE[[sessionName]]$test@AnswerFuns[[pick]])){
                            .MCE[[sessionName]]$person$responses[pick] <- as.integer(.MCE[[sessionName]]$test@AnswerFuns[[pick]](ip))
                        } else if(!is.na(.MCE[[sessionName]]$test@item_answers[[pick]])){
                            if(nanswers > 1L)
                                .MCE[[sessionName]]$person$responses[pick] <- as.integer(sum(ip %in% .MCE[[sessionName]]$test@item_answers[[pick]]))
                            else .MCE[[sessionName]]$person$responses[pick] <- as.integer(ip %in% .MCE[[sessionName]]$test@item_answers[[pick]])
                        } 
                    }
                    if(!is.null(.MCE[[sessionName]]$shinyGUI$df$Mastery)){
                        mastery <- as.logical(.MCE[[sessionName]]$shinyGUI$df$Mastery[pick])
                        if(isTRUE(mastery) && .MCE[[sessionName]]$person$responses[pick] == 0L){
                            outmessage <- HTML(paste0("<p style='color:red;'><em>",.MCE[[sessionName]]$shinyGUI$incorrect,"</em></p>"))
                            .MCE[[sessionName]]$shift_back <- .MCE[[sessionName]]$shift_back + 1L
                            .MCE[[sessionName]]$invalid_count <- .MCE[[sessionName]]$invalid_count + 1L
                            tmp <- lapply(.MCE[[sessionName]]$shinyGUI$df, function(x, pick) x[pick], pick=pick)
                            tmp <- buildShinyElements(questions=tmp, customTypes=.MCE[[sessionName]]$shinyGUI$customTypes, 
                                                      itemnames=paste0(.MCE[[sessionName]]$invalid_count, '.TeMpInTeRnAl', name),
                                                      choiceNames=.MCE[[sessionName]]$shinyGUI$choiceNames[pick],
                                                      choiceValues=.MCE[[sessionName]]$shinyGUI$choiceValues[pick],
                                                      default = ip)
                            stemOutput <- stemContent(pick, sessionName=sessionName)
                            .MCE[[sessionName]]$prevClick <- click
                            return(list(stemOutput, 
                                        .MCE[[sessionName]]$shinyGUI$df$Rendered_Question[[pick]], 
                                        tmp$questions, outmessage))
                        }
                    }
                    
                    .MCE[[sessionName]]$person$item_time[pick] <- min(diff_item_time, 
                                ifelse(.MCE[[sessionName]]$shinyGUI$timer[pick] > 0,
                                       .MCE[[sessionName]]$shinyGUI$timer[pick], Inf))
                    .MCE[[sessionName]]$start_time <- NULL
                    
                    #update Thetas
                    .MCE[[sessionName]]$design@Update.thetas(design=.MCE[[sessionName]]$design, 
                                                             person=.MCE[[sessionName]]$person, test=.MCE[[sessionName]]$test)
                    .MCE[[sessionName]]$person$Update.info_mats(design=.MCE[[sessionName]]$design, test=.MCE[[sessionName]]$test)
                    if(.MCE[[sessionName]]$shinyGUI$temp_file != '')
                        saveRDS(.MCE[[sessionName]]$person, .MCE[[sessionName]]$shinyGUI$temp_file)
                    .MCE[[sessionName]]$design <- Update.stop_now(.MCE[[sessionName]]$design, 
                                                                  person=.MCE[[sessionName]]$person,
                                                                  test=.MCE[[sessionName]]$test)
                } else {
                    printDebug("No observed response")
                    if(!item_time_valid || (.MCE[[sessionName]]$shinyGUI$forced_choice && 
                                            .MCE[[sessionName]]$shinyGUI$df$Type[pick] != 'none')){
                        printDebug("Invalid time/none type/forced", level = 2)
                        if(!item_time_valid) outmessage <- NULL
                        .MCE[[sessionName]]$shift_back <- .MCE[[sessionName]]$shift_back + 1L
                        .MCE[[sessionName]]$invalid_count <- .MCE[[sessionName]]$invalid_count + 1L
                        tmp <- lapply(.MCE[[sessionName]]$shinyGUI$df, function(x, pick) x[pick], pick=pick)
                        default <- if(tmp$Type %in% nativeTypes()) ip else NULL
                        tmp <- buildShinyElements(questions=tmp, customTypes=.MCE[[sessionName]]$shinyGUI$customTypes, 
                                                  itemnames=paste0(.MCE[[sessionName]]$invalid_count, '.TeMpInTeRnAl', name),
                                                  choiceNames=.MCE[[sessionName]]$shinyGUI$choiceNames[pick],
                                                  choiceValues=.MCE[[sessionName]]$shinyGUI$choiceValues[pick],
                                                  default = default)
                        stemOutput <- stemContent(pick, sessionName=sessionName)
                        .MCE[[sessionName]]$prevClick <- click
                        if(!item_time_valid && .MCE[[sessionName]]$shinyGUI$timer[pick] > 0)
                            invalidateLater((.MCE[[sessionName]]$shinyGUI$timer[pick] - diff_item_time)*1000)
                        return(list(stemOutput, 
                                    .MCE[[sessionName]]$shinyGUI$df$Rendered_Question[[pick]], 
                                    tmp$questions, outmessage))
                    } else {
                        printDebug("No response, updating", level = 2)
                        .MCE[[sessionName]]$person$item_time[pick] <- min(diff_item_time, 
                                                                          .MCE[[sessionName]]$shinyGUI$timer[pick])
                        .MCE[[sessionName]]$start_time <- NULL
                        if(.MCE[[sessionName]]$test@has_answers[pick])
                            .MCE[[sessionName]]$person$responses[pick] <- 0L
                        #update Thetas (same as above)
                        .MCE[[sessionName]]$design@Update.thetas(design=.MCE[[sessionName]]$design, 
                                                                 person=.MCE[[sessionName]]$person, test=.MCE[[sessionName]]$test)
                        .MCE[[sessionName]]$person$Update.info_mats(design=.MCE[[sessionName]]$design, test=.MCE[[sessionName]]$test)
                        if(.MCE[[sessionName]]$shinyGUI$temp_file != '')
                            saveRDS(.MCE[[sessionName]]$person, .MCE[[sessionName]]$shinyGUI$temp_file)
                        .MCE[[sessionName]]$design <- Update.stop_now(.MCE[[sessionName]]$design, 
                                                                      person=.MCE[[sessionName]]$person,
                                                                      test=.MCE[[sessionName]]$test)
                        .MCE[[sessionName]]$person$valid_item[pick] <- FALSE
                    }
                }
            } 
            
            printDebug("Reset item")
            .MCE[[sessionName]]$invalid_count <- 0
            .MCE[[sessionName]]$design <- Next.stage(.MCE[[sessionName]]$design, person=.MCE[[sessionName]]$person, 
                                                     test=.MCE[[sessionName]]$test, item=itemclick)
            
            if(!.MCE[[sessionName]]$design@stop_now){
                printDebug("Find next item", level = 2)
                item <- if(all(is.na(.MCE[[sessionName]]$person$items_answered))) .MCE[[sessionName]]$design@start_item
                    else findNextCATItem(person=.MCE[[sessionName]]$person, test=.MCE[[sessionName]]$test, 
                                        design=.MCE[[sessionName]]$design, start=FALSE)
                .MCE[[sessionName]]$item <- item
                .MCE[[sessionName]]$item_start_time <- Sys.time()
                if(!is.null(attr(item, 'design'))) .MCE[[sessionName]]$design <- attr(item, 'design')
                if(is.na(item)){
                    .MCE[[sessionName]]$design@stop_now <- TRUE
                } else {
                    if(is.null(.MCE[[sessionName]]$start_time))
                        .MCE[[sessionName]]$start_time <- proc.time()[3L]
                    .MCE[[sessionName]]$person$items_answered[itemclick+1L] <- as.integer(item)
                    if(.MCE[[sessionName]]$shinyGUI$temp_file != '')
                        saveRDS(.MCE[[sessionName]]$person, .MCE[[sessionName]]$shinyGUI$temp_file)
                    stemOutput <- stemContent(pick=item, sessionName=sessionName)
                    .MCE[[sessionName]]$prevClick <- click
                    if(.MCE[[sessionName]]$shinyGUI$timer[item] > 0)
                        invalidateLater(.MCE[[sessionName]]$shinyGUI$timer[item] * 1000)
                    return(list(stemOutput, 
                                .MCE[[sessionName]]$shinyGUI$df$Rendered_Question[[item]], 
                                .MCE[[sessionName]]$shinyGUI$questions[[item]]))
                }
            }
        }
        
        #last page
        if(!.MCE[[sessionName]]$STOP){
            printDebug("Last page")
            .MCE[[sessionName]]$STOP <- TRUE
            if(!is.null(.MCE[[sessionName]]$final_fun)){
                ret <- mirtCAT_post_internal(person=.MCE[[sessionName]]$person, design=.MCE[[sessionName]]$design,
                                             has_answers=.MCE[[sessionName]]$test@has_answers, GUI=TRUE)
                .MCE[[sessionName]]$final_fun(person = ret)
            }
            if(.MCE[[sessionName]]$shinyGUI$temp_file != '')
                file.remove(.MCE[[sessionName]]$shinyGUI$temp_file)
            removeUI(selector = "div:has(> #Next)", immediate = TRUE)
            return(.MCE[[sessionName]]$shinyGUI$lastpage(person=.MCE[[sessionName]]$person))
        }
        
    })
}