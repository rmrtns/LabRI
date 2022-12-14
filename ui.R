# Libraries.
library(shiny)
library(shinyFeedback)
library(DT)
library(waiter)

# Define UI
shinyUI(navbarPage("LabRI", id = "tabs",
    # Application title
    tabPanel("Data",
        sidebarLayout(
          
            # Sidebar with a menu to select file
            sidebarPanel(
                useShinyFeedback(),
                fileInput("upload", "Choose excel file", accept = c(".xls", ".xlsx")),
                uiOutput("var_list")
            ),
            mainPanel(
                dataTableOutput("data")
            )
        )
    ),
    tabPanel("Pre-processing", id = "Pre-processing",
             
        h3(textOutput("pre_processing_header")),
        sidebarLayout(
            sidebarPanel(
                useShinyFeedback(),
                numericInput("bins1", "Preferred number of bins",
                             min = 1, value = 25, step = 1),
                h5(strong("Range of data included in subsequent analyses")),
                splitLayout(cellWidths = c("50%", "50%"), uiOutput("lower_value_input"), uiOutput("upper_value_input")),
                uiOutput("transform_list"),
                actionButton("proc_reset", "Reset")
            ),
            mainPanel(
                fluidRow(
                    column(6,
                           plotOutput("hist_untransformed")
                    ),
                    column(6,
                           plotOutput("qq_untransformed")
                    )
                ),
                fluidRow(
                    column(6,
                           conditionalPanel(condition = "input.bins1 > 0",
                                            h5(strong("Log likelihood of data with power transformation"), align = "center"),
                                            plotOutput("box_cox")
                           )
                    ),
                    column(6, align = "center",
                           conditionalPanel(condition = "input.bins1 > 0",
                                            h5(strong("Explanation of Box-Cox power transformation"), align = "center"),
                                            br(),
                                            br(),
                                            br(),
                                            tableOutput("box_cox_table")
                           )
                    )
                ),
                fluidRow(
                    column(6,
                           conditionalPanel(condition = "input.transformation != 'none'", 
                                            plotOutput("hist_transformed1"))
                    ),
                    column(6,
                           conditionalPanel(condition = "input.transformation != 'none'", 
                                            plotOutput("qq_transformed"))
                    )
                ),
                fluidRow(
                    column(12, align = "center",
                           h5("Analyses based on n = ", textOutput("n_var", inline = TRUE), " measurements"))
                )
            )
        )
    ),
    tabPanel("Bhattacharya", id = "Bhattacharya",
             h3(textOutput("bhattacharya_page_header")),
             
             tags$head(
               tags$style(HTML("
                    .shiny-output-error-validation {
                    color: red;
                    font-weight: bold;
                    }
                "))
             ),
             
             sidebarLayout(
                 sidebarPanel(
                     useShinyFeedback(),
                     radioButtons("bhat_ri_width", "Reference interval",
                                  c("90%", "95%", "99%"), selected = "95%", inline = TRUE),
                     numericInput("bhat_decimal", "Decimal places",
                                  value = 2, min = 0, max = 10, step = 1),
                     radioButtons("bhat_nr_distr", "Number of distributions",
                                 c(1, 2, 3), selected = 1, inline = TRUE),
                     conditionalPanel(condition = "input.bhat_init", numericInput("bins2", "Preferred number of bins",
                                                                                  min = 1, value = 25, step = 1)),
                     conditionalPanel(condition = "input.bhat_init & (input.bhat_nr_distr == '1' | input.bhat_nr_distr == '2' | input.bhat_nr_distr == `3`)",
                                      h5(strong("Points included in Bhattacharya analysis"))),
                     conditionalPanel(condition = "input.bhat_init & (input.bhat_nr_distr == '1' | input.bhat_nr_distr == '2' | input.bhat_nr_distr == '3')",
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  numericInput("section_1_lower",
                                                               label = "Lower end section 1",
                                                               min = 1, value = 1, step = 1), 
                                                  numericInput("section_1_upper",
                                                               label = "Upper end section 1",
                                                               min = 1, value = 1, step = 1))),
                     conditionalPanel(condition = "input.bhat_init & (input.bhat_nr_distr == '2' | input.bhat_nr_distr == '3')",
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  numericInput("section_2_lower",
                                                               label = "Lower end section 2",
                                                               min = 1, value = 1, step = 1), 
                                                  numericInput("section_2_upper",
                                                               label = "Upper end section 2",
                                                               min = 1, value = 1, step = 1))),
                     conditionalPanel(condition = "input.bhat_init & input.bhat_nr_distr == '3'",
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  numericInput("section_3_lower",
                                                               label = "Lower end section 3",
                                                               min = 1, value = 1, step = 1), 
                                                  numericInput("section_3_upper",
                                                               label = "Upper end section 3",
                                                               min = 1, value = 1, step = 1))),
                     uiOutput("bhat_options")
                 ),
                 mainPanel(
                     conditionalPanel(condition = "input.bhat_init", 
                                      fluidRow(column(6, plotOutput("hist_transformed2")),
                                               column(6, plotOutput("bhat_regplot"))
                                      )
                     ),
                     fluidRow(
                         align = "center",
                         plotOutput("bhat_distr_plot")
                     ),
                     fluidRow(
                         align = "center",
                         plotOutput("bhat_cdf_plot")
                     ),
                     fluidRow(
                         align = "center",
                        tableOutput("bhat_table")
                     )
                 )
             )),
    tabPanel("Mixtools",
             h3(textOutput("mixtools_page_header")),
             
             tags$head(
                 tags$style(HTML("
                    .shiny-output-error-validation {
                    color: red;
                    font-weight: bold;
                    }
                "))
             ),
             
             sidebarLayout(
                 sidebarPanel(
                     useShinyFeedback(),
                     radioButtons("mixt_ri_width", "Reference interval",
                                  c("90%", "95%", "99%"), selected = "95%", inline = TRUE),
                     numericInput("mixt_decimal", "Decimal places",
                                  value = 2, min = 0, max = 10, step = 1),
                     radioButtons("mixt_nr_distr", "Number of distributions",
                                  c(2, 3), selected = 2, inline = TRUE), # option 1 removed
                     radioButtons("mixt_type_distr", "Applied probability distrubution",
                                  c("Normal", "Gamma"), selected = "Normal", inline = TRUE),
                     splitLayout(actionButton("mixt_start", "Estimate"), actionButton("mixt_reset", "Reset"))
                 ),
                 mainPanel(
                     fluidRow(
                         align = "center",
                         plotOutput("mixt_distr_plot")
                     ),
                     fluidRow(
                         align = "center",
                         plotOutput("mixt_cdf_plot")
                     ),
                     fluidRow(
                         align = "center",
                         tableOutput("mixt_table")
                     )
                 )
             )
    ),
    tabPanel("RefineR",
             h3(textOutput("refiner_page_header")),
             sidebarLayout(
               sidebarPanel(
                 useShinyFeedback(),
                 radioButtons("refiner_ri_width", "Reference interval",
                            c("90%", "95%", "99%"), selected = "95%", inline = TRUE),
                 radioButtons("refiner_ci_width", "Confidence interval",
                            c("None", "90%", "95%", "99%"), selected = "None", inline = TRUE),
                 numericInput("refiner_decimal", "Decimal places",
                              value = 2, min = 0, max = 10, step = 1),
                 radioButtons("refiner_model", "Applied model",
                              c("BoxCox", "modBoxCoxFast", "modBoxCox"), selected = "BoxCox", inline = TRUE),
                 radioButtons("refiner_point", "Method for point estimates",
                            c("fullDataEst", "medianBS", "meanBS"), selected = "fullDataEst", inline = TRUE),
                 conditionalPanel(condition = "input.refiner_ci_width != 'None' | input.refiner_point != 'fullDataEst'",
                   numericInput("refiner_nbootstrap", "Number of bootstrap repititions",
                                value = 20, min = 1, max = 1000, step = 1)
                   ),
                 splitLayout(actionButton("refiner_start", "Estimate"), actionButton("refiner_reset", "Reset"))
               ),
               mainPanel(
                 useWaiter(),
                 fluidRow(
                   align = "center",
                   plotOutput("refiner_plot")
                 ),
                 fluidRow(
                   align = "center",
                   tableOutput("refiner_table")
                 )
               )
            )
    ),
    tabPanel("Report",
             h3("Download report"),
             sidebarLayout(
                 sidebarPanel(
                     checkboxGroupInput("analyses_in_report", "Analyses to include in report",
                                        c("Bhattacharya", "Mixtools", "RefineR"), inline = TRUE),
                     downloadButton("download_report")
                 ),
                 mainPanel()
             )
    ),
    tabPanel("About",
             h3("Intended use"),
             h4("LabRI has been developed as a research tool for the exploration of reference intervals in populations consisting of a mixture of distributions. For that, it provides an interactive user interface to several indirect methods for reference interval determination."),
             h4("Please review the scientific literature and package documentation of the individual R packages included in this tool (listed below) for an appreciation of the advantages and disadvantages of the available techniques as well as their correct application."),
             br(),
             h3("How to"),
             h4("1. Upload excel file (.xls or .xlsx) using the 'Data' panel and select continuous variable of interest."),
             h4("2. Select data range for subsequent analyses and transformation if applicable. Box-Cox analysis is only valid for non-negative values and if a single distribution underlies the observed data."),
             h4("3. Perform Bhattacharya analysis using the 'Bhattacharya' panel (if desirable). Density plots and the cumulative distribution function (CDF) allow a visual assessment of the fit of the model to the original data. Densities of the modeled distributions are adjusted to their estimated mixing proportions and add up to 1."),
             h4("4. Perform mixtools analysis using the 'Mixtools' panel (if desirable). Density plots and the cumulative distribution function (CDF) allow a visual assessment of the fit of the model to the original data. Densities of the modeled distributions are adjusted to their estimated mixing proportions and add up to 1."),
             h4("5. Perform refineR analysis using the 'RefineR' panel (if desirable). The density plot allows a visual assessment of the fit of the model to the original data. Only the density of the 'Non-pathological / most-prevalent' distribution is shown."),
             h4("6. Download report of Bhattacharya, mixtools and/or refineR analysis using the 'Report' panel."),
             br(),
             h3("Information"),
             h4(HTML(paste0("Data are processed and presented on an interactive website using R language", tags$sup("1"), ", the shiny web application framework", tags$sup("2"), " and the following packages: dplyr", tags$sup("3"), ", DT", tags$sup("4"), ", ggplot2", tags$sup("5"), ", kableExtra", tags$sup("6"), ", knitr", tags$sup("7"), ", mixtools", tags$sup("8"), ", readxl", tags$sup("9"), ", refineR", tags$sup("10"), ", shinyFeedback", tags$sup("11"), ", stringr", tags$sup("12"), " and waiter", tags$sup("13"), ". Box-Cox analysis is performed with an adapted form of the gg_boxcox application of the lindia", tags$sup("14"), " package and the boxcox function of the MASS", tags$sup("15"), " package. In addition, the Bhattacharya analysis is based on code developed by The Lab-R-Torian", tags$sup("16"), ". Results are converted into pdf output with R Markdown", tags$sup("17"), ".", " The code of this Shiny application is available on ", a("GitHub", href = "https://github.com/rmrtns/LabRI", target="_blank"), "."))),
             br(),
             h3("References and packages"),
             h4(HTML(paste0("1. R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria ",
                a("(link)", href = "https://www.R-project.org", target="_blank"), "."))),
             h4(HTML(paste0("2. shiny: Web Application Framework for R ",
                a("(link)", href = "http://CRAN.R-project.org/package=shiny", target="_blank"), "."))),
             h4(HTML(paste0("3. dplyr: A Grammar of Data Manipulation ",
                a("(link)", href = "https://cran.r-project.org/web/packages/dplyr", target="_blank"), "."))),
             h4(HTML(paste0("4. DT: A Wrapper of the JavaScript Library 'DataTables' ",
                a("(link)", href = "https://cran.r-project.org/web/packages/DT", target="_blank"), "."))),
             h4(HTML(paste0("5. ggplot2: Create Elegant Data Visualisations Using the Grammar of Graphics ",
                a("(link)", href = "https://cran.r-project.org/web/packages/ggplot2", target="_blank"), "."))),
             h4(HTML(paste0("6. kableExtra: Construct Complex Table with 'kable' and Pipe Syntax ",
                a("(link)", href = "https://cran.r-project.org/web/packages/kableExtra/index.html", target="_blank"), "."))),
             h4(HTML(paste0("7. knitr: A General-Purpose Package for Dynamic Report Generation in R ",
                a("(link)", href = "https://cran.r-project.org/web/packages/knitr/index.html", target="_blank"), "."))),
             h4(HTML(paste0("8. mixtools: Tools for Analyzing Finite Mixture Models ",
                a("(link)", href = "https://cran.r-project.org/package=mixtools", target="_blank"), "."))),
             h4(HTML(paste0("9. readxl: Read Excel Files ",
                a("(link)", href = "https://cran.r-project.org/package=readxl", target="_blank"), "."))),
             h4(HTML(paste0("10. refineR: Reference Interval Estimation using Real-World Data ",
                a("(link)", href = "https://cran.r-project.org/web/packages/refineR/index.html", target="_blank"), "."))),
             h4(HTML(paste0("11. shinyFeedback: Display User Feedback in Shiny Apps ",
                a("(link)", href = "https://cran.rstudio.com/web/packages/shinyFeedback", target="_blank"), "."))),
             h4(HTML(paste0("12. stringr: Simple, Consistent Wrappers for Common String Operations ",
                a("(link)", href = "https://cran.r-project.org/package=stringr", target="_blank"), "."))),
             h4(HTML(paste0("13. waiter: Loading Screen for 'Shiny' ",
                a("(link)", href = "https://cran.r-project.org/web/packages/waiter/index.html", target="_blank"), "."))),
             h4(HTML(paste0("14. lindia: Automated Linear Regression Diagnostic ",
                a("(link)", href = "https://cran.r-project.org/package=lindia", target="_blank"), "."))),
             h4(HTML(paste0("15. MASS: Support Functions and Datasets for Venables and Ripley's MASS ",
                a("(link)", href = "https://cran.r-project.org/package=MASS", target="_blank"), "."))),
             h4(HTML(paste0("16. The Lab-R-Torian: Applications of the R programming Language to Laboratory Medicine ",
                a("(link)", href = "https://labrtorian.com/2017/09/05/mining-your-routine-data-for-reference-intervals-hoffman-bhattacharya-and-maximum-likelihood/", target="_blank"), "."))),
             h4(HTML(paste0("17. rmarkdown: Dynamic Documents for R ",
                a("(link)", href = "http://rmarkdown.rstudio.com", target="_blank"), "."))),
             br(),
             h3("Changelog"),
             h4("Version 1.0 (July, 2022)"),
             tags$ul(
               tags$li("Box-Cox, Bhattacharya and mixtools analysis")
             ),
             h4("Version 1.1 (December, 2022)"),
             tags$ul(
               tags$li("Addition of refineR analysis"),
               tags$li("Improved user interactivity"),
               tags$li("Bug fixes")
             ),
             br(),
             h3("Disclaimer"),
             h4("The use of this application shall constitute acceptance of the terms of the following disclaimers:"),
             tags$ul(
               tags$li("This application is intended for research purposes only."),
               tags$li("All data uploaded to this application are processed in the cloud. Click ",  a("here", href = "https://docs.posit.co/shinyapps.io/security-and-compliance.html", target="_blank"), " for further information. It is the user's responsibility to conform with local data protection regulations."), 
               tags$li("All results computed by and published within this application are designed to  assist, but not to replace professional judgment. Therefore, the user shall review the results of this application carefully and may not rely solely on the information."),
               tags$li("The information herein is provided 'AS IS' and 'AS AVAILABLE' and no representation, warranty or guarantee, explicit or implied, is made regarding the application's result accuracy, timeliness, sequence, completeness or suitability to a specific purpose or that the result will be error free or that any defects or omissions can or will be corrected."),
               tags$li("The creator of this application will not be liable for any direct, indirect, incidental, consequential, special, exemplary, punitive or any damages whatsoever resulting in whole or part from any user's use of or reliance upon the application whether or not advised of the possibility of damage under any theory of liability, arising out of or in connection with the use or performance of the application.")
             ),
             br(),
             h4("LabRI, beta version 1.1, 2022"),
             h4("Developed by RJH Martens"),
             h4("For questions, please contact via: ", a("Email", href = "mailto:rmrtns@gmail.com"), "or ", a("GitHub", href = "https://github.com/rmrtns/", target="_blank"))
             )
    
))
