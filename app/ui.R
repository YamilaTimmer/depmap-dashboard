source("functions.R")

ui <- page_fillable(
    
    # Used to dynamically show accordion filters
    useShinyjs(),
    
    # Sidebar and Navbar
    page_navbar(
        
        title = "DepMap Data Dashboard",
        
        nav_item(
            text = "Home",
            icon = bsicons::bs_icon("house-door"),
            id = "home_nav"
        ),
        
        nav_item(
            text = "Dashboard",
            icon = bsicons::bs_icon("app"),
            id = "dashboard_nav"
        ),
        
        # Dark mode button, clicking switches between light/dark mode
        nav_item(input_dark_mode(id = "dark_mode", mode = "light")),
        
        # Adds github logo to navbar that links to repo
        nav_item(tags$a(
            href = "https://github.com/YamilaTimmer/depmap-dashboard", 
            target = "_blank", 
            bsicons::bs_icon("github", size = "2rem")
        )),
        
        # Home page
        nav_panel(
            id = "home_nav",
            "Home",
            tags$div(
                style = "max-width: 900px; margin: auto; padding: 50px;",
                
                # Title and description
                tags$h1(style = "text-align: center; font-size: 36px; 
                        font-weight: bold; margin-bottom: 10px;", 
                        "Depmap data explorer"),
                tags$p(style = "font-size: 16px;", 
                       "The Dependency Map",
                       a("(DepMap)", href = "https://depmap.org/portal/"),
                       " project provides insight 
                       into genetic dependecies and vulnerabilities in cancer. 
                       This dashboard helps in exploring expression, finding 
                       similiar genes in cancer types, and comparing pathways 
                       across different cancer types."),
                tags$hr(style = "margin: 40px 0;"),
                
                # Dataset summary
                tags$div(
                    tags$h3(bs_icon("bar-chart-line"),
                            "Dataset summary"),
                    tags$p(style = "font-size: 16px;",
                           "On the dashboard the following data can be found:"),
                    tags$ul(
                        style = "font-size: 16px;",
                        tags$li("17,000+ genes"),
                        tags$li("80+ cancer types"),
                        tags$li("Metadata consisting of sex, race, age, ethnicity, and more.")
                    )
                ),
                tags$hr(style = "margin: 40px 0;"),
                
                # About us
                tags$div(
                    tags$h3(bs_icon("people-fill"),
                            "About us"),
                    tags$p(style = "font-size: 16px;",
                           "The dashboard was created by a small team of
                         enthusiastic bio-informaticians. Yamila Timmer and 
                         Mirte Draaijer, two students from the bio-informatics
                         programme of the Hanze university, worked on developing
                         the dashboard. This happened under the supervision
                         of lector Wynand Alkema, who works for the 
                         Kenniscentrum BioBased Economy (KCBBE).")
                )
            )
        ),
        
        # Dashboard page
        nav_panel(
            id = "dashboard_nav", 
            "Dashboard",
            
            layout_sidebar(
                sidebar = sidebar(width = 350,
                                  
                                  # Input dropdown menus for all list variables
                                  accordion(
                                      accordion_panel("Select use-case",
                                                      open = TRUE,
                                                      icon = bsicons::bs_icon("clipboard-check", size = "2rem"),
                                                      selectInput('use_case',
                                                                  label = NULL,
                                                                  choices = c("Explore Expression" = "explore_expression", 
                                                                              "Gene Clustering" = "gene_clustering",
                                                                              "Compare Pathway Across Cancer Types" = "compare_pathway")
                                                      ))),
                                  
                                  # Shown when selected use-case is "compare genes"
                                  accordion(open=FALSE, id = "genes_accordion",
                                            accordion_panel("Select Gene(s)",
                                                            selectizeInput('gene_names', 
                                                                           label = NULL, 
                                                                           choices = NULL, 
                                                                           multiple = TRUE)
                                            )),
                                  
                                  # Shown when selected use-case is "compare genes"
                                  accordion(open=FALSE, id = "individual_gene",
                                            accordion_panel("Select Gene",
                                                            selectizeInput('gene_name', 
                                                                           label = NULL, 
                                                                           choices = NULL, 
                                                                           multiple = FALSE)
                                            )),
                                  
                                  # Shown when selected use-case is "compare pathways"
                                  accordion(open=FALSE, id = "pathway",
                                            accordion_panel("Select Pathway",
                                                            selectizeInput('pathway_name', 
                                                                           label = NULL, 
                                                                           choices = NULL, 
                                                                           multiple = FALSE)
                                            )),
                                  
                                  
                                  # Shown when selected use-case is "compare cancer types"
                                  accordion(open=FALSE, id = "cancer_types_accordion",
                                            accordion_panel("Select Cancer Type(s)",
                                                            selectizeInput("onco_types", 
                                                                           label = NULL, 
                                                                           choices = NULL, 
                                                                           multiple = TRUE,
                                                                           options = list(maxItems = 7)
                                                            ))
                                  ),
                                  
                                  # Shown when selected use-case is "compare cancer types"
                                  accordion(open=FALSE, id = "singular_cancer_type",
                                            accordion_panel("Select Cancer Type",
                                                            selectizeInput("onco_type", 
                                                                           label = NULL, 
                                                                           choices = NULL, 
                                                                           multiple = FALSE
                                                            ))
                                  ),
                                  
                                  
                                  # Shown when selected use-case is "compare cancer types"
                                  accordion(open=FALSE, id = "compare_pathway_cancertypes",
                                            accordion_panel("Select 2 Cancer Types",
                                                            selectizeInput("compare_pathway_onco_type", 
                                                                           label = NULL, 
                                                                           choices = NULL, 
                                                                           multiple = TRUE,
                                                                           # allows selection of max 2 onco types
                                                                           options = list(maxItems = 2)
                                                            ))
                                  ),
                                  
                                  # Filter panel for metadata
                                  accordion(open=FALSE, accordion_panel("Select metadata", 
                                                                        
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
                                                                        
                                  )),
                                  
                                  # When clicked, plots and table will be updated based on user-chosen parameters
                                  #submitButton(text = "Apply Changes", icon = NULL, width = NULL)
                                  
                ),
                
                # Main part of the dashboard, containing the plots/table/statistics
                layout_columns(
                    card(full_screen = TRUE, 
                         navset_card_tab(id = "navcards",
                                         # Tab for summary plots
                                         nav_panel("Summary plots", 
                                                   layout_sidebar(sidebar = sidebar(
                                                       accordion(accordion_panel("Select plot type",
                                                                                 selectInput("summary_type", 
                                                                                             label = NULL, 
                                                                                             choices = c("Bar Plot", "Box Plot", "Violin Plot"), 
                                                                                             selected = "Bar Plot")
                                                       ),
                                                       
                                                       accordion_panel("Other options",
                                                                       selectInput("xyplot_palette", 
                                                                                   label = "Select color palette", 
                                                                                   choices <- palettes_d_names$palette[palettes_d_names$package == "colorBlindness"], 
                                                                                   selected = "PairedColor12Steps"),
                                                                       checkboxInput("geom_point_checkbox", 
                                                                                     label = "Show individual points?", 
                                                                                     value = FALSE)
                                                                       
                                                       ),
                                                       accordion(accordion_panel("Size settings",
                                                                                 sliderInput("plot_height",
                                                                                             label = "Adjust height",
                                                                                             min = 500,
                                                                                             max = 1500,
                                                                                             value = 750,
                                                                                             step = 50,
                                                                                             ticks = FALSE),
                                                                                 
                                                                                 sliderInput("plot_width",
                                                                                             label = "Adjust width",
                                                                                             min = 700,
                                                                                             max = 1500,
                                                                                             value = 1200,
                                                                                             step = 50,
                                                                                             ticks = FALSE))
                                                                 
                                                       )
                                                       
                                                       
                                                       )
                                                   ),
                                                   
                                                   shinycssloaders::withSpinner((jqui_resizable(plotlyOutput("plot"))))
                                                   )
                                         ),
                                         nav_panel("Heatmap",                               
                                                   layout_sidebar(sidebar = sidebar(
                                                       accordion(accordion_panel("Select options",
                                                                                 
                                                                                 selectInput("heatmap_palette", 
                                                                                             label = "Select color scheme", 
                                                                                             choices <- palettes_c_names$palette[palettes_c_names$package == "ggthemes"], 
                                                                                             selected = "Blue"),
                                                                                 checkboxInput("p_value_checkbox", 
                                                                                               label = "Only show genes with p < 0.05?", 
                                                                                               value = FALSE)),
                                                                 accordion(accordion_panel("Size settings",
                                                                                           sliderInput("heatmap_height",
                                                                                                       label = "Adjust height",
                                                                                                       min = 500,
                                                                                                       max = 1500,
                                                                                                       value = 750,
                                                                                                       step = 50,
                                                                                                       ticks = FALSE),
                                                                                           
                                                                                           sliderInput("heatmap_width",
                                                                                                       label = "Adjust width",
                                                                                                       min = 700,
                                                                                                       max = 1500,
                                                                                                       value = 1200,
                                                                                                       step = 50,
                                                                                                       ticks = FALSE))
                                                                 )
                                                                 )
                                                       ),
                                                       
                                                       shinycssloaders::withSpinner((jqui_resizable(plotlyOutput("heatmap"))))
                                                   )),
                                                   
                                                   nav_panel("Gene Clustering",                               
                                                             layout_sidebar(sidebar = sidebar(
                                                                 accordion(accordion_panel("Select clustering options",
                                                                                           selectInput("clustering_options", 
                                                                                                       label = "Select correlation to show:", 
                                                                                                       choices = c("Positive correlation", "Negative correlation"), 
                                                                                                       selected = "Positive correlation"),
                                                                                           sliderInput("top_n_genes", 
                                                                                                       label = "Number of genes to show:", 
                                                                                                       min = 1, 
                                                                                                       max = 10, 
                                                                                                       value = 5)
                                                                 )
                                                                 )
                                                             ),
                                                             shinycssloaders::withSpinner((jqui_resizable(plotlyOutput("clusterplot"))))
                                                             )),
                                                   
                                                   nav_panel("Correlation Plot",                               
                                                             layout_sidebar(sidebar = sidebar(
                                                                 accordion(accordion_panel("Select correlation options",
                                                                                           selectizeInput("correlation_gene", 
                                                                                                          label = "Select gene to compare with:",
                                                                                                          choices = NULL,
                                                                                                          multiple = FALSE),
                                                                 ),
                                                                 accordion(accordion_panel("Other options",
                                                                                           checkboxInput("label_checkbox", 
                                                                                                          label = "Display cell line labels?",
                                                                                                         value = TRUE),
                                                                 
                                                                 )))
                                                             ),
                                                             shinycssloaders::withSpinner((jqui_resizable(plotlyOutput("corr_plot"))))
                                                             )),
                                                   
                                                   nav_panel("Data", shinycssloaders::withSpinner(DT::DTOutput("data")))
                                                   
                                                   
                                         )
                         )
                    )
                )
            )
        )
    )