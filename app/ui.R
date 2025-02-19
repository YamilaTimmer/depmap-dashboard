library(shiny)
library(bslib)
library(bsicons)

ui <- page_fillable(
    
    # Sidebar
    page_navbar(
        
        
        title = "DepMap Data Dashboard",
        sidebar = sidebar(width = 350,
                          
                          # Input dropdown menus for all list variables
                          accordion(
                              accordion_panel("Select use-case",
                                              open = TRUE,
                                              icon = bsicons::bs_icon("clipboard-check", size = "2rem"),
                                              selectInput('use_case',
                                                          label = NULL,
                                                          choices = c("Compare Genes", "Compare Cancer Types")
                                              ))),
                          accordion(
                              accordion_panel("Select gene",
                                              selectizeInput('gene_name', 
                                                             label = NULL, 
                                                             choices = NULL, 
                                                             multiple = TRUE
                                              )),
                              
                              accordion_panel("Select cancer type",
                                              selectizeInput("onco_type", 
                                                             label = NULL, 
                                                             choices = NULL, 
                                                             multiple = TRUE
                                              )),
                              accordion_panel("Select other metadata",
                                              selectizeInput("sex", 
                                                             label = "Select sex", 
                                                             choices = NULL, 
                                                             multiple = TRUE),
                                              
                                              selectizeInput("race", 
                                                             label = "Select ethnic background", 
                                                             choices = NULL,
                                                             multiple = TRUE),
                                              
                                              selectizeInput("age_category", 
                                                             label = "Select age category", 
                                                             choices = NULL, 
                                                             multiple = TRUE),
                                              
                              ))),
        
        # Main part of the dashboard, containing the plots/table/statistics
        layout_columns(
            card(full_screen = TRUE, 
                 navset_card_tab(
                     # Tab for summary plots
                     nav_panel("Summary plots", 
                               layout_sidebar(sidebar = sidebar(
                                   accordion(accordion_panel("Select plot type",
                                                             radioButtons("summary_type", 
                                                                          label = NULL, 
                                                                          choices = c("Bar Plot", "Box Plot", "Violin Plot"), 
                                                                          selected = "Bar Plot")
                                   )
                                   )
                               )
                               )
                     )
                 )
            ),
            
            layout_columns(
                card(full_screen = TRUE, 
                     navset_card_tab(
                         nav_panel("Table")
                     )  
                ),
                
                card(full_screen = TRUE, 
                     navset_card_tab(
                         nav_panel("Statistics")
                     )
                ),
                col_widths = c(12, 12)
            ), col_widths = c(7, 5)
        ) 
        ,
        
        # Dark mode button, clicking switches between light/dark mode
        nav_item(input_dark_mode(id = "dark_mode", 
                                 mode = "light")
        ),
        
        # Adds github logo to navbar that links to repo
        nav_item(tags$a(
            href = "https://github.com/YamilaTimmer/depmap-dashboard", 
            target = "_blank", 
            bsicons::bs_icon("github", size = "2rem")
        )
        ), 

    )
)
