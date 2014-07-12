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
        if(input$Next > 1L && input$Next < (MCE$test$length + 3L) && !MCE$person$stop_now){
            
            # evaluate and store results from last item input
            if(input$Next > 2L && (input$Next-2L) > sum(!is.na(MCE$person$responses))){
                item <- MCE$person$items_answered[length(MCE$person$items_answered)]
                response <- which(MCE$test$item_options[[item]] %in% input$choice)
                if(input$choice != '')
                    MCE$person$responses[item] <- response
            } else {
                if(input$Next < (MCE$test$length + 2L)){
                    item <- nextItem()
                    MCE$person$items_answered <- c(MCE$person$items_answered, item)
                } else return(MCE$shinyGUI$lastpage) #print last page if not stopped early
            }
            
            return(MCE$shinyGUI$questions[[item]]$item)
        }

        # last page default return if stopped early
        return(MCE$shinyGUI$lastpage)
        
    }) 
}