# Libraries.
library(shiny)
library(shinyFeedback)
library(DT)

# Define UI
shinyUI(navbarPage("LabRI",

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
    tabPanel("Box-Cox",
        h3(textOutput("box_cox_page_header")),
        sidebarLayout(
            sidebarPanel(
                useShinyFeedback(),
                uiOutput("bin_input1"),
                h5(strong("Range of data included in subsequent analyses")),
                splitLayout(cellWidths = c("50%", "50%"), uiOutput("lower_value_input"), uiOutput("upper_value_input")),
                uiOutput("transform_list")
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
                           plotOutput("hist_transformed1")
                    ),
                    column(6,
                           plotOutput("qq_transformed")
                    )
                ),
                fluidRow(
                    column(12, align = "center",
                           h5("Analyses based on n = ", textOutput("n_var", inline = TRUE), " measurements"))
                )
            )
        )
    ),
    tabPanel("Bhattacharya",
             h3(textOutput("bhattacharya_page_header")),
             sidebarLayout(
                 sidebarPanel(
                     useShinyFeedback(),
                     radioButtons("bhat_nr_distr", "Number of distributions",
                                 c(0, 1, 2, 3), selected = 0, inline = TRUE),
                     conditionalPanel(condition = "input.bhat_nr_distr == '1' | input.bhat_nr_distr == '2' | input.bhat_nr_distr == `3`",
                                      uiOutput("bin_input2")),
                     conditionalPanel(condition = "input.bhat_nr_distr == '1' | input.bhat_nr_distr == '2' | input.bhat_nr_distr == `3`",
                                      h5(strong("Points included in Bhattacharya analysis"))),
                     conditionalPanel(condition = "input.bhat_nr_distr == '1' | input.bhat_nr_distr == '2' | input.bhat_nr_distr == '3'",
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  numericInput("section_1_lower",
                                                               label = "Lower end section 1",
                                                               min = 1, value = 1, step = 1), 
                                                  numericInput("section_1_upper",
                                                               label = "Upper end section 1",
                                                               min = 1, value = 1, step = 1))),
                     conditionalPanel(condition = "input.bhat_nr_distr == '2' | input.bhat_nr_distr == '3'",
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  numericInput("section_2_lower",
                                                               label = "Lower end section 2",
                                                               min = 1, value = 1, step = 1), 
                                                  numericInput("section_2_upper",
                                                               label = "Upper end section 2",
                                                               min = 1, value = 1, step = 1))),
                     conditionalPanel(condition = "input.bhat_nr_distr == '3'",
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  numericInput("section_3_lower",
                                                               label = "Lower end section 3",
                                                               min = 1, value = 1, step = 1), 
                                                  numericInput("section_3_upper",
                                                               label = "Upper end section 3",
                                                               min = 1, value = 1, step = 1)))
                 ),
                 mainPanel(
                     fluidRow(
                         column(6,
                                plotOutput("hist_transformed2")),
                         column(6,
                                plotOutput("bhat_regplot"))
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
                         column(12, align = "center",
                                conditionalPanel(condition = "input.bhat_nr_distr > 0",
                                                 h5(strong("Lower and upper reference limits")),
                                                 tableOutput("bhat_table")
                                )
                         )
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
                     radioButtons("mixt_type_distr", "Distrubution",
                                  c("Normal", "Gamma"), selected = "Normal", inline = TRUE),
                     radioButtons("mixt_nr_distr", "Number of distributions",
                                  c(0, 2, 3), selected = 0, inline = TRUE) # option 1 removed
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
                         column(12, align = "center",
                                conditionalPanel(condition = "input.mixt_nr_distr > 0",
                                h5(strong("Lower and upper reference limits")),
                                tableOutput("mixt_table")
                                )
                         )
                     )
                 )
             )
    ),
    tabPanel("Report",
             h3("Download report"),
             sidebarLayout(
                 sidebarPanel(
                     checkboxGroupInput("analyses_in_report", "Analyses to include in report",
                                        c("Bhattacharya", "Mixtools"), inline = TRUE),
                     radioButtons("format", "Document format", 
                                  c("PDF", "Word", "HTML"), selected = "PDF", inline = TRUE),
                     downloadButton("download_report")
                 ),
                 mainPanel()
             )
    ),
    tabPanel("About",
             h3("Intended use"),
             h4("LabRI has been developed as a research tool for the exploration of reference intervals in populations consisting of a mixture of distributions."),
             h4("Please review the scientific literature and package documentation of the individual R packages included in this tool (listed below) for an appreciation of the advantages and disadvantages of the available techniques as well as their correct application."),
             br(),
             h3("How to"),
             h4("1. Upload excel file (.xls or .xlsx) using the 'Data' panel and select continuous variable of interest."),
             h4("2. Select data range for subsequent analyses and transformation if applicable. Box-Cox analysis only valid for non-negative values and if a single distribution underlies the observed data."),
             h4("3. Perform Bhattacharya analysis using the 'Bhattacharya' panel (if desirable). Density plots and the cumulative distribution function (CDF) allow a visual assessment of the fit of the model to the original data. Densities of the modeled distributions are adjusted to their estimated mixing proportions and add up to 1."),
             h4("4. Perform mixtools analysis using the 'Mixtools' panel (if desirable). Density plots and the cumulative distribution function (CDF) allow a visual assessment of the fit of the model to the original data. Densities of the modeled distributions are adjusted to their estimated mixing proportions and add up to 1."),
             h4("5. Download report of Bhattacharya and/or mixtools analysis using the 'Report' panel."),
             br(),
             h3("Information"),
             h4(HTML(paste0("Data are processed and presented on an interactive website using R language", tags$sup("1"), ", the shiny web application framework", tags$sup("2"), " and the following packages: dplyr", tags$sup("3"), ", DT", tags$sup("4"), ", ggplot2", tags$sup("5"), ", mixtools", tags$sup("6"), ", readxl", tags$sup("7"),  ", shinyFeedback", tags$sup("8"), " and stringr", tags$sup("9"), ". Box-Cox analysis is performed with an adapted form of the gg_boxcox application of the lindia", tags$sup("10"), " package and the boxcox function of the MASS", tags$sup("11"), " package. In addition, the Bhattacharya analysis is based on code developed by The Lab-R-Torian", tags$sup("12"), ". Results are converted into pdf and html output with R Markdown", tags$sup("13"), ".", " The code of this Shiny application is available on ", a("GitHub", href = "https://github.com/rmrtns/LabRI"), "."))),
             br(),
             h3("References and packages"),
             h4("1. R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria.",
                a("https://www.R-project.org",
                  href = "https://www.R-project.org", target="_blank")),
             h4("2. shiny: Web Application Framework for R.",
                a("https://CRAN.R-project.org/package=shiny",
                  href = "http://CRAN.R-project.org/package=shiny", target="_blank")),
             h4("3. dplyr: A Grammar of Data Manipulation.",
                a("https://cran.r-project.org/web/packages/dplyr",
                  href = "https://cran.r-project.org/web/packages/dplyr", target="_blank")),
             h4("4. DT: A Wrapper of the JavaScript Library 'DataTables'.",
                a("https://cran.r-project.org/web/packages/DT",
                  href = "https://cran.r-project.org/web/packages/DT", target="_blank")),
             h4("5. ggplot2: Create Elegant Data Visualisations Using the Grammar of Graphics.",
                a("https://cran.r-project.org/web/packages/ggplot2",
                  href = "https://cran.r-project.org/web/packages/ggplot2", target="_blank")),
             h4("6. mixtools: Tools for Analyzing Finite Mixture Models.",
                a("https://cran.r-project.org/package=mixtools",
                  href = "https://cran.r-project.org/package=mixtools", target="_blank")),
             h4("7. readxl: Read Excel Files.",
                a("https://cran.r-project.org/package=readxl",
                  href = "https://cran.r-project.org/package=readxl", target="_blank")),
             h4("8. shinyFeedback: Display User Feedback in Shiny Apps.",
                a("https://cran.rstudio.com/web/packages/shinyFeedback",
                  href = "https://cran.rstudio.com/web/packages/shinyFeedback", target="_blank")),
             h4("9. stringr: Simple, Consistent Wrappers for Common String Operations.",
                a("https://cran.r-project.org/package=stringr",
                  href = "https://cran.r-project.org/package=stringr", target="_blank")),
             h4("10. lindia: Automated Linear Regression Diagnostic.",
                a("https://cran.r-project.org/package=lindia",
                  href = "https://cran.r-project.org/package=lindia", target="_blank")),
             h4("11. MASS: Support Functions and Datasets for Venables and Ripley's MASS.",
                a("https://cran.r-project.org/package=MASS",
                  href = "https://cran.r-project.org/package=MASS", target="_blank")),
             h4("12. The Lab-R-Torian: Applications of the R programming Language to Laboratory Medicine.",
                a("https://labrtorian.com/2017/09/05/mining-your-routine-data-for-reference-intervals-hoffman-bhattacharya-and-maximum-likelihood/",
                  href = "https://labrtorian.com/2017/09/05/mining-your-routine-data-for-reference-intervals-hoffman-bhattacharya-and-maximum-likelihood/", target="_blank")),
             h4("13. rmarkdown: Dynamic Documents for R.",
                a("http://rmarkdown.rstudio.com",
                  href = "http://rmarkdown.rstudio.com", target="_blank")),
             br(),
             h3("Disclaimer"),
             h4("This application was developed as a research tool for the exploration of reference intervals in populations consisting of a mixture of distributions. The use of this application shall constitute acceptance of the terms of the following disclaimers:"),
             h4("- This application is intended for research purposes only."),
             h4("- All results computed by and published within this application are designed to  assist, but not to replace professional judgment. Therefore, the user shall review the results of this application carefully and may not rely solely on the information."),
             h4("- The information herein is provided 'AS IS' and 'AS AVAILABLE' and no representation, warranty or guarantee, explicit or implied, is made regarding the application's result accuracy, timeliness, sequence, completeness or suitability to a specific purpose or that the result will be error free or that any defects or omissions can or will be corrected."),
             h4("- The creator of this application will not be liable for any direct, indirect, incidental, consequential, special, exemplary, punitive or any damages whatsoever resulting in whole or part from any user's use of or reliance upon the application whether or not advised of the possibility of damage under any theory of liability, arising out of or in connection with the use or performance of the application."),
             br(),
             h4("LabRI, beta version 1.0, 2022"),
             h4("Developed by RJH Martens"),
             h4("For questions, please contact via: ", a("Email", href = "mailto:rmrtns@gmail.com"), "or ", a("GitHub", href = "https://github.com/rmrtns/", target="_blank"))
             )
    
))
