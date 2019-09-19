default_UI <- function(){
    
    sessionName <- 'MASTER'
    
    fluidPage(theme = if(.MCE[[sessionName]]$shinyGUI$theme != '') 
        if(requireNamespace("shinythemes", quietly = TRUE)){
            shinythemes::shinytheme(.MCE[[sessionName]]$shinyGUI$theme)
        } else NULL, 
        
        shiny::withMathJax(),
        
        tags$head(
            tags$style(HTML(.MCE[[sessionName]]$shinyGUI$css))
        ),
        
        #  Application title
        headerPanel(.MCE[[sessionName]]$shinyGUI$title),
        
        # FIXME can't access person properties if using MASTER enviroment
        # if(is.finite(.MCE[[sessionName]]$design@max_time)){
        #     h6(paste0(.MCE[[sessionName]]$shinyGUI$time_remaining, 
        #               formatTime(.MCE[[sessionName]]$design@max_time - 
        #                              sum(.MCE[[sessionName]]$person$item_time))))
        # } else NULL,
        
        sidebarPanel(
            if(.MCE[[sessionName]]$shinyGUI$author != '') h4("Authors:") else NULL,
            if(.MCE[[sessionName]]$shinyGUI$author != '') h5(.MCE[[sessionName]]$shinyGUI$author) else NULL,
            div(
                if(.MCE[[sessionName]]$shinyGUI$author != '') hr() else NULL,
                helpText("\n", .MCE[[sessionName]]$shinyGUI$instructions[1L]),            
                actionButton("Next", .MCE[[sessionName]]$shinyGUI$instructions[2L])
            )
        ),
        
        mainPanel(
            htmlOutput("item_stem_html"),
            uiOutput("Main")    
        )
        
    ) #end bootstrapPage
    
}