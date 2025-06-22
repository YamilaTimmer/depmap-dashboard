source("R/functions.R")

app_ui <- function(){
  usethis::use_package("bslib")
  bslib::page_fillable(
    # Used to dynamically show bslib::accordion filters
    shinyjs::useShinyjs(),
    usethis::use_package("shinyjs"),

    # Sidebar and Navbar
    bslib::page_navbar(

        title = "DepMap Data Dashboard",

        bslib::nav_item(
            text = "Home",
            icon = bsicons::bs_icon("house-door"),
            id = "home_nav"
        ),

        bslib::nav_item(
            text = "Dashboard",
            icon = bsicons::bs_icon("app"),
            id = "dashboard_nav"
        ),


        # Adds github logo to navbar that links to repo
        bslib::nav_item(shiny::tags$a(
            href = "https://github.com/YamilaTimmer/depmap-dashboard",
            target = "_blank",
            bsicons::bs_icon("github", size = "2rem")
        )),

        # Home page
        bslib::nav_panel(
            id = "home_nav",
            "Home",
            shiny::tags$div(
                style = "max-width: 900px; margin: auto; padding: 50px;",

                # Title and description
                shiny::tags$h1(style = "text-align: center; font-size: 36px;
                        font-weight: bold; margin-bottom: 10px;",
                        "Depmap data explorer"),
                shiny::tags$p(style = "font-size: 16px;",
                       "The Dependency Map",
                       shiny::a("(DepMap)", href = "https://depmap.org/portal/"),
                       " project provides insight
                       into genetic dependecies and vulnerabilities in cancer.
                       This dashboard helps in exploring expression, finding
                       similiar genes in cancer types, and comparing pathways
                       across different cancer types."),
                shiny::tags$hr(style = "margin: 40px 0;"),

                # Dataset summary
                shiny::tags$div(
                  shiny::tags$h3(bsicons::bs_icon("bar-chart-line"),
                          "Dataset summary"),
                  shiny::tags$p(style = "font-size: 16px;",
                         "On the dashboard the following data can be found:"),
                  shiny::tags$ul(
                    style = "font-size: 16px;",
                    shiny::tags$li("17,000+ genes"),
                    shiny::tags$li("1900+ different cell lines"),
                    shiny::tags$li("60+ cancer types"),
                    shiny::tags$li("Metadata consisting of sex, age, ethnic background, and more.")
                  ),
                  shiny::tags$p(style = "font-size: 16px;",
                         "The dashboard includes data on over 60 cancer types,
                         the chart below shows the top 10 cancer types that
                         have the most available data.")
                ),
                shiny::plotOutput("homepage_plot", height = "400px"),
                shiny::tags$hr(style = "margin: 40px 0;"),

                # About us
                shiny::tags$div(
                  shiny::tags$h3(bsicons::bs_icon("people-fill"),
                          "About us"),
                  shiny::tags$p(style = "font-size: 16px;",
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
        bslib::nav_panel(
            id = "dashboard_nav",
            "Dashboard",

            bslib::layout_sidebar(
                sidebar = bslib::sidebar(width = 350,
                                  style = "background-color: #f0f0f0;",
                                  # Input dropdown menus for all list variables
                                  bslib::accordion(
                                      bslib::accordion_panel("Select use-case",
                                                      style = "background-color: #f0f0f0;",
                                                      shiny::tags$style(shiny::HTML("
                                                                      .bslib::accordion-item .bslib::accordion-header {
                                                                      background-color: #f0f0f0 !important;}

                                                                      .bslib::accordion-item .bslib::accordion-button:not(.collapsed){
                                                                      background-color: #f0f0f0 !important;}

                                                                      .bslib::accordion-item .bslib::accordion-button.collapsed {
                                                                      background-color: #f0f0f0 !important;")),
                                                      open = TRUE,
                                                      icon = bsicons::bs_icon("clipboard-check", size = "2rem"),
                                                      bslib::tooltip(shiny::span(bsicons::bs_icon("info-circle")),
                                                        shiny::HTML("<strong>Explore expression:</strong><br/>
                                                             Explore the expression of one or more genes in one cancer type,<br/>
                                                             or compare the expression of one or more genes across multiple cancer types.<br/><br/>

                                                             <strong>Gene clustering:</strong><br/>
                                                             explore the correlation between expression profiles and find positively/negatively correlated genes.<br/><br/>

                                                             <strong>Compare pathways: </strong><br/>
                                                             select multiple cancer types and one pathway to compare the<br/>
                                                             differences in expression in genes from the chosen pathway."),
                                                        placement = "auto",
                                                        options = list(trigger = "click")),
                                                      shiny::selectInput('use_case',
                                                                  label = NULL,
                                                                  choices = c("Explore Expression" = "explore_expression",
                                                                              "Gene Clustering" = "gene_clustering",
                                                                              "Compare Pathway Across Cancer Types" = "compare_pathway")
                                                      )),


                                      # Shown when selected use-case is "compare genes"
                                     shiny::div(id = "genes_bslib::accordion",
                                          bslib::accordion_panel("Select Gene(s)",
                                                          style = "background-color: #f0f0f0;",

                                                          shiny::selectizeInput('gene_names',
                                                                         label = NULL,
                                                                         choices = NULL,
                                                                         multiple = TRUE))),


                                      # Shown when selected use-case is "compare genes"
                                     shiny::div(id = "individual_gene",
                                          bslib::accordion_panel("Select Gene",
                                                          style = "background-color: #f0f0f0;",

                                                          shiny::selectizeInput('gene_name',
                                                                         label = NULL,
                                                                         choices = NULL,
                                                                         multiple = FALSE))),


                                      # Shown when selected use-case is "compare pathways"
                                     shiny::div(id = "pathway",
                                          bslib::accordion_panel("Select Pathway",
                                                          style = "background-color: #f0f0f0;",

                                                          shiny::selectizeInput('pathway_name',
                                                                         label = NULL,
                                                                         choices = NULL,
                                                                         multiple = FALSE))),



                                      # Shown when selected use-case is "compare cancer types"
                                     shiny::div(id = "cancer_types_bslib::accordion",
                                          bslib::accordion_panel("Select Cancer Type(s)",
                                                          style = "background-color: #f0f0f0;",

                                                          shiny::selectizeInput("onco_types",
                                                                         label = NULL,
                                                                         choices = NULL,
                                                                         multiple = TRUE,
                                                                         options = list(maxItems = 7)))),


                                      # Shown when selected use-case is "compare cancer types"
                                     shiny::div(id = "singular_cancer_type",
                                          bslib::accordion_panel("Select Cancer Type",
                                                          style = "background-color: #f0f0f0;",

                                                          shiny::selectizeInput("onco_type",
                                                                         label = NULL,
                                                                         choices = NULL,
                                                                         multiple = FALSE))),



                                      # Shown when selected use-case is "compare cancer types"
                                     shiny::div(id = "compare_pathway_cancertypes",
                                          bslib::accordion_panel("Select 2 Cancer Types",
                                                          style = "background-color: #f0f0f0;",

                                                          shiny::selectizeInput("compare_pathway_onco_type",
                                                                         label = NULL,
                                                                         choices = NULL,
                                                                         multiple = TRUE,
                                                                         # allows selection of max 2 onco types
                                                                         options = list(maxItems = 2)))),


                                      # Filter panel for metadata
                                      bslib::accordion_panel("Select metadata",
                                                      style = "background-color: #f0f0f0;",

                                                      shiny::selectizeInput("sex",
                                                                     label = "Select sex",
                                                                     choices = NULL,
                                                                     multiple = TRUE),

                                                      shiny::selectizeInput("race",
                                                                     label = "Select ethnic background",
                                                                     choices = NULL,
                                                                     multiple = TRUE),

                                                      shiny::selectizeInput("age_category",
                                                                     label = "Select age category",
                                                                     choices = NULL,
                                                                     multiple = TRUE))
                                  )


                                  # When clicked, plots and table will be updated based on user-chosen parameters
                                  #submitButton(text = "Apply Changes", icon = NULL, width = NULL)

                ),

                # Main part of the dashboard, containing the plots/table/statistics
                bslib::layout_columns(
                    bslib::card(full_screen = TRUE,
                         bslib::navset_card_tab(id = "navcards",
                                         # Tab for summary plots
                                         bslib::nav_panel("Summary plots",
                                                   bslib::layout_sidebar(sidebar = bslib::sidebar(
                                                       bslib::accordion(bslib::accordion_panel("Select plot type",
                                                                                 shiny::selectInput("summary_type",
                                                                                             label = NULL,
                                                                                             choices = c("Bar Plot", "Box Plot", "Violin Plot"),
                                                                                             selected = "Bar Plot")
                                                       ),

                                                       bslib::accordion_panel("Other options",
                                                                       shiny::selectInput("xyplot_palette",
                                                                                   label = "Select color palette",
                                                                                   choices <- paletteer::palettes_d_names$palette[paletteer::palettes_d_names$package == "colorBlindness"],
                                                                                   selected = "PairedColor12Steps"),
                                                                       shiny::checkboxInput("geom_point_checkbox",
                                                                                     label = "Individual points",
                                                                                     value = FALSE),
                                                                       shiny::checkboxInput("border_checkbox",
                                                                                     label = "Add border",
                                                                                     value = FALSE),
                                                                       shiny::checkboxInput("y_labs_checkbox",
                                                                                     label = "Use same Y-axis",
                                                                                     value = FALSE)


                                                       ),
                                                       bslib::accordion_panel("Size settings",
                                                                       shiny::sliderInput("plot_height",
                                                                                   label = "Adjust height",
                                                                                   min = 100,
                                                                                   max = 1500,
                                                                                   value = 450,
                                                                                   step = 50,
                                                                                   ticks = FALSE),

                                                                       shiny::sliderInput("plot_width",
                                                                                   label = "Adjust width",
                                                                                   min = 100,
                                                                                   max = 1500,
                                                                                   value = 800,
                                                                                   step = 50,
                                                                                   ticks = FALSE))





                                                       )
                                                   ),

                                                   shinycssloaders::withSpinner(plotly::plotlyOutput("plot"))
                                                   )
                                         ),
                                         bslib::nav_panel("Heatmap",
                                                  bslib::layout_sidebar(sidebar = bslib::sidebar(
                                                       bslib::accordion(bslib::accordion_panel("Select options",

                                                                                 shiny::selectInput("heatmap_palette",
                                                                                             label = "Select color scheme",
                                                                                             choices <- (palettes_c_names$palette[palettes_c_names$package == "ggthemes"][4:10]),
                                                                                             selected = "Blue"),
                                                                                 shiny::checkboxInput("border_checkbox_heatmap",

                                                                                               label = "Add border",
                                                                                               value = FALSE),

                                                                                 shiny::checkboxInput("p_value_checkbox",
                                                                                               label = "Only show genes with p < 0.05?",
                                                                                               value = FALSE)),



                                                                 bslib::accordion_panel("Size settings",
                                                                                 shiny::sliderInput("heatmap_height",
                                                                                             label = "Adjust height",
                                                                                             min = 100,
                                                                                             max = 1500,
                                                                                             value = 450,
                                                                                             step = 50,
                                                                                             ticks = FALSE),

                                                                                 shiny::sliderInput("heatmap_width",
                                                                                             label = "Adjust width",
                                                                                             min = 100,
                                                                                             max = 1500,
                                                                                             value = 800,
                                                                                             step = 50,
                                                                                             ticks = FALSE))

                                                       )
                                                   ),

                                                   shinycssloaders::withSpinner(plotly::plotlyOutput("heatmap"))
                                                   )),

                                         bslib::nav_panel("Clustering Plot",
                                                   bslib::layout_sidebar(sidebar = bslib::sidebar(
                                                       bslib::accordion(bslib::accordion_panel("Select clustering options",
                                                                                 shiny::selectInput("clustering_options",
                                                                                             label = "Select correlation to show:",
                                                                                             choices = c("Positive correlation", "Negative correlation"),
                                                                                             selected = "Positive correlation"),
                                                                                 shiny::sliderInput("top_n_genes",
                                                                                             label = "Number of genes to show:",
                                                                                             min = 1,
                                                                                             max = 10,
                                                                                             value = 5)
                                                       ),
                                                       bslib::accordion_panel("Other options",
                                                                       shiny::checkboxInput("border_checkbox_cluster",

                                                                                     label = "Add border",
                                                                                     value = FALSE)),


                                                       bslib::accordion_panel("Size settings",
                                                                       shiny::sliderInput("cluster_height",
                                                                                   label = "Adjust height",
                                                                                   min = 100,
                                                                                   max = 1500,
                                                                                   value = 450,
                                                                                   step = 50,
                                                                                   ticks = FALSE),

                                                                       shiny::sliderInput("cluster_width",
                                                                                   label = "Adjust width",
                                                                                   min = 100,
                                                                                   max = 1500,
                                                                                   value = 800,
                                                                                   step = 50,
                                                                                   ticks = FALSE))


                                                       )
                                                   ),
                                                   shinycssloaders::withSpinner(plotly::plotlyOutput("clusterplot"))
                                                   )),

                                         bslib::nav_panel("Correlation Plot",
                                                  bslib::layout_sidebar(sidebar = bslib::sidebar(
                                                       bslib::accordion(bslib::accordion_panel("Select correlation options",
                                                                                 shiny::selectizeInput("correlation_gene",
                                                                                                label = "Select gene to compare with:",
                                                                                                choices = NULL,
                                                                                                multiple = FALSE),
                                                       ),
                                                       bslib::accordion_panel("Other options",
                                                                       shiny::checkboxInput("border_checkbox_correlation",

                                                                                     label = "Add border",
                                                                                     value = FALSE),

                                                                       shiny::checkboxInput("label_checkbox",
                                                                                     label = "Display cell line labels?",
                                                                                     value = TRUE)

                                                       ),

                                                       bslib::accordion_panel("Size settings",
                                                                       shiny::sliderInput("corr_height",
                                                                                   label = "Adjust height",
                                                                                   min = 100,
                                                                                   max = 1500,
                                                                                   value = 450,
                                                                                   step = 50,
                                                                                   ticks = FALSE),

                                                                       shiny::sliderInput("corr_width",
                                                                                   label = "Adjust width",
                                                                                   min = 100,
                                                                                   max = 1500,
                                                                                   value = 800,
                                                                                   step = 50,
                                                                                   ticks = FALSE)
                                                       )

                                                       )
                                                   ),
                                                   shinycssloaders::withSpinner(plotly::plotlyOutput("corr_plot"))
                                                   )),

                                        bslib::nav_panel("Data", shinycssloaders::withSpinner(DT::DTOutput("data"))),

                                        bslib::nav_panel("Help",
                                                   value = "help_explore",
                                                   shiny::fluidPage(
                                                    shiny::h3("Explore expression"),
                                                    shiny::p("This use case allows the
                                                       user to generate visualizations
                                                       of expression across genes.
                                                       Possible plot types include
                                                       bar plots, box plots, violin plots,
                                                       and heat maps. Expression is
                                                       shown as log2 TPM. TPM represents
                                                       the number of transcripts
                                                       from the gene, per one million
                                                       RNA molecules in the RNA sample.
                                                       The log2 transformation is then
                                                       applied to normalize the data. "),

                                                    shiny::h3("Selection"),
                                                    shiny::p("On the left side select
                                                     the gene(s) and cancer type(s)
                                                     you are interested in. If
                                                     you want you can select sex,
                                                     ethnic background, and age
                                                     category as well."),

                                                    shiny::h3("Summary plots"),
                                                    shiny::p("After making your selections,
                                                       you can choose between a
                                                       bar plot, a box plot, and
                                                       a violin plot on the first
                                                       tab. Additionally, you can
                                                       pick a color palette and
                                                       decide whether or not to
                                                       show the individual data
                                                       points. Gene expression is
                                                       shown on the y-axis and
                                                       cancer type on the x-axis."),

                                                    shiny::h3("Heatmap"),
                                                    shiny::p("On the next tab, a heatmap
                                                       is shown. For the heatmap,
                                                       you can pick a color
                                                       palette. Here the gene
                                                       expression is shown across
                                                       the selected cell lines."),

                                                    shiny::h3("Data"),
                                                    shiny::p("On the 'data' tab a data
                                                       table containing the data
                                                       is shown. At the top, you
                                                       can select which columns
                                                       to show. Furthermore, you
                                                       can download the raw data
                                                       as a .csv or .xlsx file
                                                       through the download button.
                                                       Additional information on
                                                       the gene(s) can be found
                                                       by clicking on the gene
                                                       name in the data table.
                                                       Additional information on
                                                       the gene + cancer type
                                                       can be found by clicking
                                                       on 'PubMed' in the
                                                       'research' column.")
                                                   )),

                                        bslib::nav_panel("Help",
                                                   value = "help_cluster",
                                                  shiny::fluidPage(
                                                    shiny::h3("Gene clustering"),
                                                    shiny::p("This use case can be used
                                                       to find the top 10 positively
                                                       or negatively correlated
                                                       genes with a selected gene.
                                                       Furthermore, this use case
                                                       can be used to find
                                                       correlations between
                                                       expression profiles of two
                                                       genes. A correlation in
                                                       this situation means that
                                                       genes influence each other’s
                                                       expression, a positive
                                                       correlation means that if
                                                       gene A is increased in
                                                       expression, gene B will
                                                       also be increased in
                                                       expression, however a close
                                                       positive correlation does
                                                       not mean that the genes have
                                                       an equal expression, it
                                                       just indicates their, ",
                                                       shiny::strong("expression profile "),
                                                       "is similar. The same goes
                                                       for a negative correlation,
                                                       if gene A has a lowered
                                                       expression in a cell line,
                                                       then gene B is also
                                                       lowered in expression."),

                                                    shiny::h3("Selection"),
                                                    shiny::p("On the left side, select
                                                       the gene and the cancer
                                                       type you are interested in.
                                                       If you want you can select
                                                       sex, ethnic background,
                                                       and age category as well."),

                                                    shiny::h3("Clustering plot"),
                                                    shiny::p("Automatically the top 5
                                                       positively correlated genes
                                                       show up. Using the
                                                       drop-down menu, you can
                                                       switch between positively
                                                       and negatively correlated
                                                       genes. Using the slider
                                                       on the left the number of
                                                       genes that are shown can
                                                       be changed from the top 1
                                                       all the way to the top 10."),

                                                    shiny::h3("Correlation plot"),
                                                    shiny::p("The correlation plot gives
                                                       you the ability to compare
                                                       the expression of your
                                                       chosen gene to any other
                                                       gene. Using the dropdown
                                                       menu, you can pick one
                                                       gene at a time to compare
                                                       your original gene to.
                                                       Using the checkbox under
                                                       ‘other options’ you can
                                                       turn on/off the cell line
                                                       labels. Hovering over the
                                                       dots will also provide you
                                                       with information on exact
                                                       expression in both chosen
                                                       genes and which cell line
                                                       you are looking at. Some
                                                       combinations of genes will
                                                       display a linear line,
                                                       indicating a correlation
                                                       between the two genes."),

                                                    shiny::h3("Data"),
                                                    shiny::p("On the 'data' tab a data
                                                       table containing the data
                                                       is shown. At the top, you
                                                       can select which columns
                                                       to show. Furthermore, you
                                                       can download the raw data
                                                       as a .csv or .xlsx file
                                                       through the download button.
                                                       Additional information on
                                                       the gene(s) can be found
                                                       by clicking on the gene
                                                       name in the data table.
                                                       Additional information on
                                                       the gene + cancer type
                                                       can be found by clicking
                                                       on 'PubMed' in the
                                                       'research' column.")
                                                   )),

                                        bslib::nav_panel("Help",
                                                   value = "help_compare",
                                                  shiny::fluidPage(
                                                    shiny::h3("Compare pathways"),
                                                    shiny::p("This use case can be used
                                                       to compare humane biological
                                                       pathways, which are
                                                       collections of genes working
                                                       together in order to fulfill
                                                       a biological process.
                                                       Expression is shown as
                                                       log2 TPM. TPM represents
                                                       the number of transcripts
                                                       from the gene, per one
                                                       million RNA molecules in
                                                       the RNA sample. The log2
                                                       transformation is then
                                                       applied to normalize
                                                       the data."),

                                                    shiny::h3("Selection"),
                                                    shiny::p("On the left side, select
                                                       the pathway you are
                                                       interested in. The dropdown
                                                       selection for 'select pathways'
                                                       contains all humane pathways
                                                       from the",
                                                       shiny::a("KEGG pathway database",
                                                         href = "https://www.kegg.jp/kegg/kegg2.html",
                                                         target = "_blank")
                                                       ,"
                                                       Selecting a pathway will
                                                       auto-select all genes
                                                       that belong to the chosen
                                                       pathway. Select one or
                                                       more types of cancer to
                                                       compare expression of genes
                                                       in the chosen pathway,
                                                       across cancer types."),

                                                    shiny::h3("Heatmap"),
                                                    shiny::p("The result can be viewed
                                                       in the heatmap, with genes
                                                       on the x-axis and cancer
                                                       types on the y-axis.
                                                       Different color palettes
                                                       can be chosen and the colors
                                                       resemble the expression
                                                       rates in log2 TPM"),

                                                    shiny::h3("Data"),
                                                    shiny::p("On the 'data' tab a data
                                                       table containing the data
                                                       is shown. At the top, you
                                                       can select which columns
                                                       to show. Furthermore, you
                                                       can download the raw data
                                                       as a .csv or .xlsx file
                                                       through the download button.
                                                       Additional information on
                                                       the gene(s) can be found
                                                       by clicking on the gene
                                                       name in the data table.
                                                       Additional information on
                                                       the gene + cancer type
                                                       can be found by clicking
                                                       on 'PubMed' in the
                                                       'research' column.")
                                                   ))


                         )
                    )
                )
            )
        )
    )
)
}
