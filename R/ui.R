default_UI <- function(){
    
    fluidPage(theme = if(.MCE$shinyGUI$theme != '') 
        if(requireNamespace("shinythemes", quietly = TRUE)){
            shinythemes::shinytheme(.MCE$shinyGUI$theme)
        } else NULL, 
        
        shiny::withMathJax(),
        
        tags$head(
            tags$style(HTML(.MCE$shinyGUI$css))
        ),
        
        #  Application title
        headerPanel(.MCE$shinyGUI$title),
        
        if(is.finite(.MCE$design@max_time)){
            h6(paste0('Time remaining: ', 
                      formatTime(.MCE$design@max_time - sum(.MCE$person$item_time))))
        } else NULL,
        
        sidebarPanel(
            if(.MCE$shinyGUI$author != '') h4("Authors:") else NULL,
            if(.MCE$shinyGUI$author != '') h5(.MCE$shinyGUI$author) else NULL,
            if(.MCE$shinyGUI$author != '') hr() else NULL,
            helpText("\n", .MCE$shinyGUI$instructions[1L]),            
            actionButton("Next", .MCE$shinyGUI$instructions[2L])
        ),
        
        mainPanel(
            htmlOutput("item_stem_html"),
            uiOutput("Main")    
        )
        
    ) #end bootstrapPage
    
}