default_UI <- function(){
    
    return(fluidPage(
        
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
            h4(paste0("\n", .MCE$shinyGUI$instructions[1L])),
            h5(.MCE$shinyGUI$instructions[2L]),            
            actionButton("Next", .MCE$shinyGUI$instructions[3L])
        ),
        
        mainPanel(
            htmlOutput("item_stem_html"),
            uiOutput("Main")    
        )
        
    )) #end bootstrapPage
    
}