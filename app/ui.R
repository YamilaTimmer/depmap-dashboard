source("functions.R")

ui <- page_fillable(
    
    # Used to dynamically show accordion filters
    useShinyjs(),
    
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
                                                          choices = c("Explore Expression" = "explore_expression", "Explore Genes" = "explore_genes")
                                              ))),
                          
                          # Shown when selected use-case is "compare genes"
                          accordion(id = "genes_accordion",
                                    accordion_panel("Select Gene(s)",
                                                    selectizeInput('gene_name', 
                                                                   label = NULL, 
                                                                   choices = NULL, 
                                                                   multiple = TRUE)
                                    )),
                          
                          # Shown when selected use-case is "compare cancer types"
                          accordion(id = "cancer_types_accordion",
                                    accordion_panel("Select Cancer Type(s)",
                                                    selectizeInput("onco_type", 
                                                                   label = NULL, 
                                                                   choices = NULL, 
                                                                   multiple = TRUE
                                                    ))
                          ),
                          
                          # Filter panel for metadata
                          accordion(accordion_panel("Select metadata", 
                                                    
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
                                                             selectInput("summary_type", 
                                                                         label = NULL, 
                                                                         choices = c("Bar Plot", "Box Plot", "Violin Plot"), 
                                                                         selected = "Bar Plot")
                                   )
                                   )
                               ),
                               
                               shinycssloaders::withSpinner((jqui_resizable(plotlyOutput("plot"))))
                               )
                     ),
                     nav_panel("Heatmap",                               
                               layout_sidebar(sidebar = sidebar(
                                   accordion(accordion_panel("Select options",
                                                             selectInput("heatmap_options", 
                                                                         label = NULL, 
                                                                         choices = c("Option"), 
                                                                         selected = "Option")
                                   )
                                   )
                               ),
                               
                               shinycssloaders::withSpinner((jqui_resizable(plotlyOutput("heatmap"))))
                               ))
                 )
            ),
            
            layout_columns(
                card(full_screen = TRUE, 
                     navset_card_tab(
                         nav_panel("Table", shinycssloaders::withSpinner(DT::DTOutput("data")))
                     )  
                ),
                
                card(full_screen = TRUE, 
                     navset_card_tab(
                         nav_panel("Statistics")
                     )
                ),
                col_widths = c(12, 12)
            ), 
            
            col_widths = c(7, 5)
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
        )
        
    )
)
