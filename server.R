# Libraries.
library(shiny)
library(shinyFeedback)
library(dplyr)
library(stringr)
library(readxl)
library(DT)
library(ggplot2)
library(mixtools)
library(rmarkdown)
library(lindia)
# Adapted form of lindia's gg_boxcox

shinyServer(function(input, output) {

    # Import data.
    
    ## Select excel file (either .xls or .xlsx).
    df <- reactive({
        req(input$upload)
        file <- input$upload
        ext <- tools::file_ext(file$datapath)
        ext_excel <- ext %in% c(".xls", "xlsx")
        feedbackDanger("upload", !ext_excel, "Please select .xls or .xlsx file")
        req(ext %in% c(".xls", "xlsx"))
        read_excel(file$datapath)
    })
    
    ## Show data from uploaded excel file.
    output$data <- renderDataTable({
        datatable(df(), options = list(scrollX = TRUE))
    })
    
    ## Acquire list of continuous variables from uploaded excel file and use to select variable via ui.
    output$var_list <- renderUI({
        selectInput("variable",
                    label = "Select variable",
                    choices = names(select_if(df(), is.numeric)))
    })

    ## Select variable for further analysis.
    var_selected_original <- reactive({
        req(input$variable)
        df() %>% pull(., input$variable)
    })
    
    # Box-Cox analysis.
    
    ## Server side function for input of lower and upper value of selected variable.
    output$lower_value_input <- renderUI({
        numericInput("lower_value",
                     label = "Lower value (>=)",
                     value = min(var_selected_original(), na.rm = TRUE))
    })
    
    output$upper_value_input <- renderUI({
        numericInput("upper_value",
                     label = "Upper value (<=)",
                     value = max(var_selected_original(), na.rm = TRUE))
    })
    
    ## Create derivatives of selected variable based on selected range.
    ### Some functions require input as dataframe, others as flattened list.
    
    var_selected <- reactive({
        req(input$variable, input$lower_value, input$upper_value)
        correct_range_values <- input$lower_value < input$upper_value
        feedbackDanger("upper_value", !correct_range_values, "Upper value not > lower value")
        req(correct_range_values)
        df() %>% filter(.data[[input$variable]] >= input$lower_value & .data[[input$variable]] <= input$upper_value) %>% pull(., input$variable)
    })
    
    var_selected_df <- reactive({
        req(input$variable, input$lower_value, input$upper_value)
        correct_range_values <- input$lower_value < input$upper_value
        feedbackDanger("upper_value", !correct_range_values, "Upper value not > lower value")
        req(correct_range_values)
        var_selected_df <- df() %>% select(input$variable) %>% filter(.data[[input$variable]] >= input$lower_value & .data[[input$variable]] <= input$upper_value) 
    })
    
    n_var_selected <- reactive({
        req(var_selected)
        length(var_selected())
    })
    
    output$n_var <- renderText({
        n_var_selected()
    })
    
    ## Header for Box-Cox analysis.
    output$box_cox_page_header <- renderText({
        str_c("Box-Cox analysis for ", input$variable)
    })
    
    ## Server side function for selection of number of bins.
    ### Input for bins1 is used in Box-Cox section, whereas bins2 is used in Bhattacharya section.
    ### Inputs for bins1 and bins2 are linked and updated upon change of either of inputs.
    ### An additional function using observerEvent is used to prevent a loop when changeing and updateing bins1 or bins2.
    bin_standard <- reactive({
        req(input$variable)
        25
    })
    
    output$bin_input1 <- renderUI({
        numericInput("bins1",
                    label = "Preferred number of bins",
                    min = 1, value = bin_standard(), step = 1)
    })
    
    output$bin_input2 <- renderUI({
        req(input$bins1)
        numericInput("bins2",
                     label = "Preferred number of bins",
                     min = 1, value = input$bins1, step = 1)
    })
    
    bin_input <- reactiveValues(x = NULL)
    
    observeEvent(input$bins1, {
        bin_input$x <- input$bins1
    })
    observeEvent(input$bins2, {
        bin_input$x <- input$bins2
    })
    
    observeEvent(bin_input$x, {
        req(input$bins1, input$bins2)
        correct_bins2 <- input$bins2 > 0
        feedbackDanger("bins2", !correct_bins2, "Please select a number >= 1")
        req(correct_bins2)
        if (bin_input$x != input$bins1){
            updateNumericInput(inputId = "bins1", value = bin_input$x)
        }
    })
    
    observeEvent(bin_input$x, {
        req(input$bins1, input$bins2)
        correct_bins1 <- input$bins1 > 0
        feedbackDanger("bins1", !correct_bins1, "Please select a number >= 1")
        req(correct_bins1)
        if (bin_input$x != input$bins2){
            updateNumericInput(inputId = "bins2", value = bin_input$x)
        }
        
    })
    
    ## Histogram based on untransformed data.
    hist_untransformed <- reactive({
        req(var_selected_df(), var_selected(), input$bins1 >= 1)
        ggplot(data = var_selected_df(), mapping = aes(x = var_selected_df()[[input$variable]])) +
            geom_histogram(breaks = seq(min(var_selected(), na.rm = TRUE), max(var_selected(), na.rm = TRUE), length.out = (input$bins1 + 1)),
                           color = "black", fill = "grey") +
            labs(title = str_c("Histogram of ", input$variable, " without data transformation"),
                 x = input$variable,
                 y = "Frequency") +
            theme_classic() +
            theme(plot.title = element_text(face = "bold", hjust = 0.5),
                  panel.border = element_rect(fill = NA))
    })
    
    output$hist_untransformed <- renderPlot({hist_untransformed()})
    
    ## QQ plot based on untransformed data.
    qq_untransformed <- reactive({
        req(var_selected_df(), input$bins1 >= 1)
        ggplot(data = var_selected_df(), mapping = aes(sample = var_selected_df()[[input$variable]])) +
            geom_qq() +
            geom_qq_line() +
            scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
            labs(title = str_c("Normal QQ-plot of ", input$variable, " without data transformation"),
                 x = "Theoretical quantiles",
                 y = "Sample quantiles") +
            theme_classic() +
            theme(plot.title = element_text(face = "bold", hjust = 0.5),
                  panel.border = element_rect(fill = NA))
    })
    
    output$qq_untransformed <- renderPlot({qq_untransformed()})
    
    ## Box-Cox analysis.
    ### Statistical analysis.
    box_cox <- reactive({
        req(input$variable, var_selected_df(), input$bins1 >= 1)
        
        if (input$lower_value < 0 | input$upper_value < 0){
            validate("Error: Select lower and upper value >= 0 for Box-Cox analysis")
            box_cox_plot < NA
        } else if (input$lower_value >= 0 | input$upper_value >= 0){
            box_cox_object <- MASS::boxcox(as.formula(str_c(input$variable, " ~ 1")), data = var_selected_df(), plotit = FALSE)
            x <- unlist(box_cox_object$x)
            y <- unlist(box_cox_object$y)
            xstart <- x[-1]
            ystart <- y[-1]
            xend <- x[-(length(x))]
            yend <- y[-(length(y))]
            box_cox_unlist <- data.frame(xstart, ystart, xend, yend)
            best_lambda <- x[which.max(y)]
            rounded_lambda <- round(best_lambda, 3)
            min_y <- min(y)
            accept_inds <- which(y > max(y) - 1/2 * qchisq(0.95, 1))
            accept_range <- x[accept_inds]
            conf_lo <- round(min(accept_range), 3)
            conf_hi <- round(max(accept_range), 3)
            
            box_cox_plot <- ggplot(data = box_cox_unlist) + 
                geom_segment(aes(x = xstart, y = ystart, xend = xend, yend = yend), size = 0.5) + 
                geom_vline(xintercept = best_lambda, linetype = "dotted", size = 0.5/2) + 
                geom_vline(xintercept = conf_lo, linetype = "dotted", size = 0.5/2) + 
                geom_vline(xintercept = conf_hi, linetype = "dotted", size = 0.5/2) + 
                geom_hline(yintercept = y[min(accept_inds)], linetype = "dotted", size = 0.5/2) + 
                labs(title = "Box-Cox plot", x = "Lambda", y = "Log-likelihood") + 
                theme_classic() +
                theme(plot.title = element_text(face = "bold", hjust = 0.5),
                      panel.border = element_rect(fill = NA))
        }
        
        return(box_cox_plot)
    })

    output$box_cox <- renderPlot({box_cox()})
    
    ### Accompanying data table for interpretation of results.
    box_cox_table_lambda <- c(-2, -1, -0.5, 0, 0.5, 1, 2)
    box_cox_table_explanation <- c("Inverse square", "Inverse", "Inverse square root", "Natural log", "Square root", "No transformation", "Square")
    box_cox_table_combined <- data.frame("Lambda" = box_cox_table_lambda, "Transformation" = box_cox_table_explanation)
    
    output$box_cox_table <- renderTable({
        req(input$variable, var_selected_df(), input$bins1 >= 1)
        box_cox_table_combined
    })
    
    ## Data transformation.
    ### Server side function for selection of data transformation for subsequent analyses.
    transform_standard <- reactive({
        req(input$variable)
        "none"
    })
    
    output$transform_list <- renderUI({
        selectInput("transformation", 
                    label = "Select transformation", 
                    choices = c("inverse square" = "invsqr",
                                "inverse" = "inv",
                                "inverse square root" = "invsqrt",
                                "natural log" = "log",
                                "square root" = "sqrt",
                                "none" = "none",
                                "square" = "sqr"),
                    selected = transform_standard())
    })
    
    ### Function for labeling in, for example, figure captions after transformation of data for subsequent analyses.
    transform_hist_label <- function(transformation){
        output <- case_when(transformation == "invsqr" ~ "Inverse square ",
                            transformation == "inv" ~ "Inverse ",
                            transformation == "invsqrt" ~ "Inverse square root ",
                            transformation == 'log' ~ "Natural log ",
                            transformation == "sqrt" ~ "Square root ",
                            transformation == "none" ~ "Untransformed ",
                            transformation == "sqr" ~ "Square ")
    }
    
    transform_hist_label_selected <- reactive({
        req(input$transformation)
        transform_hist_label(input$transformation)
    })
    
    ### Function for actual data transformation for subsequent analyses.
    transform_variable <- function(variable, transformation){
        output <- case_when(transformation == "invsqr" ~ 1 / (variable ** 2),
                            transformation == "inv" ~ 1 / variable,
                            transformation == "invsqrt" ~ 1 / sqrt(variable),
                            transformation == "log" ~ log(variable),
                            transformation == "sqrt" ~ sqrt(variable),
                            transformation == "none" ~ variable,
                            transformation == "sqr" ~ variable ** 2)
    }
    
    ### Transformation of derivatives of selected variable for subsequent analyses.
    var_selected_transformed <- reactive({
        req(input$transformation)
        transform_variable(var_selected(), input$transformation)
    })

    var_selected_transformed_df <- reactive({
        req(input$transformation)
        var_selected_df() %>% mutate(across(input$variable, ~ transform_variable(var_selected_df()[[input$variable]], input$transformation)))
    })
    
    ## Histogram based on transformed data.
    ### Histogram 1 is for use in Box-Cox section and histogram 2 for use in Bhattacharya section.
    hist_transformed1 <- reactive({
        req(var_selected_transformed(), input$bins1 >= 1)
        ggplot(data = var_selected_transformed_df(), mapping = aes(x = var_selected_transformed_df()[[input$variable]])) +
            geom_histogram(breaks = seq(min(var_selected_transformed(), na.rm = TRUE), max(var_selected_transformed(), na.rm = TRUE), length.out = (input$bins1 + 1)),
                           color = "black", fill = "grey") +
            labs(title = str_c("Histogram of ", tolower(transform_hist_label_selected()), input$variable),
                 x = str_c(tolower(transform_hist_label_selected()), input$variable),
                 y = "Frequency") +
            theme_classic() +
            theme(plot.title = element_text(face = "bold", hjust = 0.5),
                  panel.border = element_rect(fill = NA))
    })
    
    
    output$hist_transformed1 <- renderPlot({hist_transformed1()})
    
    hist_transformed2 <- reactive({
        req(var_selected_transformed(), input$bhat_nr_distr > 0, input$bins2 >= 1)
        ggplot(data = var_selected_transformed_df(), mapping = aes(x = var_selected_transformed_df()[[input$variable]])) +
            geom_histogram(breaks = seq(min(var_selected_transformed(), na.rm = TRUE), max(var_selected_transformed(), na.rm = TRUE), length.out = (input$bins1 + 1)),
                           color = "black", fill = "grey") +
            labs(title = str_c("Histogram of ", tolower(transform_hist_label_selected()), input$variable),
                 x = str_c(tolower(transform_hist_label_selected()), input$variable),
                 y = "Frequency") +
            theme_classic() +
            theme(plot.title = element_text(face = "bold", hjust = 0.5),
                  panel.border = element_rect(fill = NA))
    })
    
    output$hist_transformed2 <- renderPlot({hist_transformed2()})
    
    ## QQ plot based on transformed data.
    qq_transformed <- reactive({
        req(var_selected_transformed_df(), input$bins1 >= 1)
        ggplot(data = var_selected_transformed_df(), mapping = aes(sample = var_selected_transformed_df()[[input$variable]])) +
            geom_qq() +
            geom_qq_line() +
            scale_x_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
            labs(title = str_c("Normal Q-Q plot of ", tolower(transform_hist_label_selected()), input$variable),
                 x = "Theoretical quantiles",
                 y = "Sample quantiles") +
            theme_classic() +
            theme(plot.title = element_text(face = "bold", hjust = 0.5),
                  panel.border = element_rect(fill = NA))
    })
    
    output$qq_transformed <- renderPlot({qq_transformed()})
    
    # Bhattacharya analysis.
    
    ## Header for Bhattacharya analysis.
    output$bhattacharya_page_header <- renderText({
        str_c("Bhattacharya analysis for ", input$variable)
    })
    
    ## Functions for Bhattacharya analysis.

    ### Function for calculation of mu, sigma, lrl and url based on the selected linear section.
    #### This function is required for the reactive function bhattacharya.
    function_linear_section <- function(df_bhat, h, section_lower, section_upper){
        linear.bit <- as.data.frame(df_bhat[c(section_lower:section_upper),])
        lm <- lm(dlog_count ~ mid, data = linear.bit, weights = linear.bit$counts)
        lambda <- -coef(lm)[1]/coef(lm)[2]
        mu <- lambda + h/2
        sigma <- sqrt(-h/coef(lm)[2] - h^2/12)
        lrl_bhat <- qnorm(0.025, mu, sigma)
        url_bhat <- qnorm(0.975, mu, sigma)
        linear_section <- list(lm = lm, mu = mu, sigma = sigma, lrl_bhat = lrl_bhat, url_bhat = url_bhat)
        return(linear_section)
    }
    
    ### Function for transformation of reference interval limits if applicable.
    #### This function is required for the reactive function bhattacharya.
    function_transform_ci <- function(lrl, url, transformation){
        lrl_transformed <- numeric(0)
        url_transformed <- numeric(0)
        if (transformation == "invsqr"){
            output <- c(output_1 = sqrt(1 / lrl), output_2 = sqrt(1 / url))
            lrl_transformed <- min(output)
            url_transformed <- max(output)
        } else if (transformation == "inv"){
            output <- c(output_1 = 1 / lrl, output_2 = 1 / url)
            lrl_transformed <- min(output)
            url_transformed <- max(output)
        } else if (transformation == "invsqrt"){
            output <- c(output_1 = (1 / lrl) ** 2, output_2 = (1 / url) ** 2)
            lrl_transformed <- min(output)
            url_transformed <- max(output)
        } else if (transformation == "log"){
            lrl_transformed <- exp(lrl)
            url_transformed <- exp(url)
        } else if (transformation == "none"){
            lrl_transformed <- lrl
            url_transformed <- url
        } else if (transformation == "sqr"){
            lrl_transformed <- sqrt(lrl)
            url_transformed <- sqrt(url)
        } else if (transformation == "sqrt"){
            lrl_transformed <- lrl ** 2
            url_transformed <- url ** 2
        } else {
            print("Transformation not allowed")
        }
        ci <- list(lrl_transformed = lrl_transformed, url_transformed = url_transformed)
        return(ci)
    }
    
    ### Reactive function for Bhattacharya analysis, including the above functions for calculations based on the selected linear section and transformation of reference interval limits.
    bhattacharya <- reactive({
        req(hist_transformed2(), input$bhat_nr_distr > 0)
        
        # Histogram data and calculation of mids and differences in log counts.
        hist_transformed2_build <- ggplot_build(hist_transformed2())
        hist_transformed2_build_data <- hist_transformed2_build$data[[1]][c("count", "xmin", "xmax")]
        hist_transformed2_build_data <- hist_transformed2_build_data %>% mutate(mid = xmin + ((xmax - xmin) / 2),
                                                                                log_count = log(count))
        dlog_count <- diff(hist_transformed2_build_data[["log_count"]])
        df_bhat <- data.frame(mid = hist_transformed2_build_data[["mid"]][-length(hist_transformed2_build_data[["mid"]])], 
                              count = hist_transformed2_build_data[["count"]][-length(hist_transformed2_build_data[["mid"]])],
                              log_count = hist_transformed2_build_data[["log_count"]][-length(hist_transformed2_build_data[["mid"]])],
                              dlog_count = dlog_count)
        df_bhat <- df_bhat %>% mutate(row = row_number(mid))
        h <- diff(df_bhat[["mid"]])[[1]]
        
        # Provide warning if >= 1 bins have count = 0 and, thus, dlog_count will be infinity.
        empty_bins <- min(df_bhat[["count"]]) > 0
        feedbackWarning("bins2", !empty_bins, "Histogram contains >= 1 empy bins. Consider reducing the number of bins or trimming the variable range")
        
        # Linear section(s).
        if (input$bhat_nr_distr == "1" | input$bhat_nr_distr == "2" | input$bhat_nr_distr == "3"){
            correct_section_1_lower <- input$section_1_lower >= 1
            feedbackDanger("section_1_lower", !correct_section_1_lower, "Please select lower end > = 1")
            correct_section_1_upper <- input$section_1_upper <= input$bins2
            feedbackDanger("section_1_upper", !correct_section_1_upper, "Please select upper end <= bins")
            correct_range_section1 <- input$section_1_lower < input$section_1_upper
            feedbackDanger("section_1_upper", !correct_range_section1, "Upper end not > lower end")
            req(correct_section_1_lower)
            linear_section_1 <- function_linear_section(df_bhat, h, input$section_1_lower, input$section_1_upper)
        } else {
            linear_section_1 <- list(lm = NA, mu = NA, sigma = NA, lrl_bhat = NA, url_bhat = NA)
        }
        
        if (input$bhat_nr_distr == "2" | input$bhat_nr_distr == "3") {
            correct_section_2_lower <- input$section_2_lower >= 1
            feedbackDanger("section_2_lower", !correct_section_2_lower, "Please select lower end > = 1")
            correct_section_2_upper <- input$section_2_upper <= input$bins2
            feedbackDanger("section_2_upper", !correct_section_2_upper, "Please select upper end <= bins")
            correct_range_section2 <- input$section_2_lower < input$section_2_upper
            feedbackDanger("section_2_upper", !correct_range_section2, "Upper end not > lower end")
            req(correct_section_2_lower)
            linear_section_2 <- function_linear_section(df_bhat, h, input$section_2_lower, input$section_2_upper)
        } else {
            linear_section_2 <- list(lm = NA, mu = NA, sigma = NA, lrl_bhat = NA, url_bhat = NA)
        }
        
        if (input$bhat_nr_distr == "3"){
            correct_section_3_lower <- input$section_3_lower >= 1
            feedbackDanger("section_3_lower", !correct_section_3_lower, "Please select lower end > = 1")
            correct_section_3_upper <- input$section_3_upper <= input$bins2
            feedbackDanger("section_3_upper", !correct_section_3_upper, "Please select upper end <= bins")
            correct_range_section3 <- input$section_3_lower < input$section_3_upper
            feedbackDanger("section_3_upper", !correct_range_section3, "Upper end not > lower end")
            req(correct_section_3_lower)
            linear_section_3 <- function_linear_section(df_bhat, h, input$section_3_lower, input$section_3_upper)
        } else {
            linear_section_3 <- list(lm = NA, mu = NA, sigma = NA, lrl_bhat = NA, url_bhat = NA)
        }
        
        # Transformation of ci limits if applicable.
        if (input$bhat_nr_distr == "1" | input$bhat_nr_distr == "2" | input$bhat_nr_distr == "3"){
            ci1 <- function_transform_ci(linear_section_1[["lrl_bhat"]], linear_section_1[["url_bhat"]], input$transformation)
        } else {
            ci1 <- list(NA, NA)
        }
        
        if (input$bhat_nr_distr == "2" | input$bhat_nr_distr == "3") {
            ci2 <- function_transform_ci(linear_section_2[["lrl_bhat"]], linear_section_2[["url_bhat"]], input$transformation)
        } else {
            ci2 <- list(NA, NA)
        }
        
        if (input$bhat_nr_distr == "3") {
            ci3 <- function_transform_ci(linear_section_3[["lrl_bhat"]], linear_section_3[["url_bhat"]], input$transformation)
        } else {
            ci3 <- list(NA, NA)
        }
        
        # Output.
        list(df_bhat = df_bhat, 
             lm1 = linear_section_1[["lm"]], mu1 = linear_section_1[["mu"]], sigma1 = linear_section_1[["sigma"]], lrl_transformed_bhat1 = ci1[["lrl_transformed"]], url_transformed_bhat1 = ci1[["url_transformed"]], 
             lm2 = linear_section_2[["lm"]], mu2 = linear_section_2[["mu"]], sigma2 = linear_section_2[["sigma"]], lrl_transformed_bhat2 = ci2[["lrl_transformed"]], url_transformed_bhat2 = ci2[["url_transformed"]],
             lm3 = linear_section_3[["lm"]], mu3 = linear_section_3[["mu"]], sigma3 = linear_section_3[["sigma"]], lrl_transformed_bhat3 = ci3[["lrl_transformed"]], url_transformed_bhat3 = ci3[["url_transformed"]])
    })
    
    ## Function to plot mids versus difference in log counts including regression line.
    bhattacharya_plot_reg <- reactive({
        req(input$bhat_nr_distr > 0)
        regr_plot <- ggplot(data = bhattacharya()[["df_bhat"]], mapping = aes(x = mid, y = dlog_count, label = row)) +
            geom_point(color = "black", alpha = 1) +
            geom_text(hjust = -0.2, vjust = -0.2, color = "blue") +
            geom_hline(yintercept = 0) +
            geom_vline(mapping = aes(xintercept = mid), linetype = "dashed", color = "grey", alpha = 0.5) +
            labs(title = "Mids versus difference in log counts",
                 x = "Mid",
                 y = "Difference in log counts") +
            theme_classic() +
            theme(plot.title = element_text(face = "bold", hjust = 0.5),
                  panel.border = element_rect(fill = NA))
        
        if (input$bhat_nr_distr == 1 & (input$section_1_upper - input$section_1_lower > 0)) {
            regr_plot +
                geom_abline(intercept = coef(bhattacharya()[["lm1"]])[[1]], slope = coef(bhattacharya()[["lm1"]])[[2]], color = "green")
        } else if (input$bhat_nr_distr == 2 & (input$section_1_upper - input$section_1_lower > 0) & (input$section_2_upper - input$section_2_lower > 0)) {
            regr_plot + 
                geom_abline(intercept = coef(bhattacharya()[["lm1"]])[[1]], slope = coef(bhattacharya()[["lm1"]])[[2]], color = "green") +
                geom_abline(intercept = coef(bhattacharya()[["lm2"]])[[1]], slope = coef(bhattacharya()[["lm2"]])[[2]], color = "orange")
        } else if (input$bhat_nr_distr == 3 & (input$section_1_upper - input$section_1_lower > 0) & (input$section_2_upper - input$section_2_lower > 0) & (input$section_3_upper - input$section_3_lower > 0)) {
            regr_plot + 
                geom_abline(intercept = coef(bhattacharya()[["lm1"]])[[1]], slope = coef(bhattacharya()[["lm1"]])[[2]], color = "green") +
                geom_abline(intercept = coef(bhattacharya()[["lm2"]])[[1]], slope = coef(bhattacharya()[["lm2"]])[[2]], color = "orange") +
                geom_abline(intercept = coef(bhattacharya()[["lm3"]])[[1]], slope = coef(bhattacharya()[["lm3"]])[[2]], color = "red")
        } else {regr_plot}
        
    })
    
    output$bhat_regplot <- renderPlot({bhattacharya_plot_reg()})
    
    ## Table with lower and upper ends of sections used for regression plot (only for downloadable report).
    bhat_sections <- reactive({
        req(input$bhat_nr_distr > 0)
        if (input$bhat_nr_distr == 1){
            tibble("Regression line" = c("1"),
                   "Lower end" = c(input$section_1_lower),
                   "Upper end" = c(input$section_1_upper))
        } else if (input$bhat_nr_distr == 2){
            tibble("Regression line" = c("1", "2"),
                   "Lower end" = c(input$section_1_lower, input$section_2_lower),
                   "Upper end" = c(input$section_1_upper, input$section_2_upper))
        } else if (input$bhat_nr_distr == 3){
            tibble("Regression line" = c("1", "2", "3"),
                   "Lower end" = c(input$section_1_lower, input$section_2_lower, input$section_3_lower),
                   "Upper end" = c(input$section_1_upper, input$section_2_upper, input$section_3_upper))
        }
    })
    
    ## Table with reference interval based on Bhattacharya analysis.
    bhat_table <- reactive({
        req(input$bhat_nr_distr > 0)
        if (input$bhat_nr_distr == 1){
            tibble(Distribution = c("1"),
                   LRL = c(bhattacharya()[["lrl_transformed_bhat1"]]),
                   URL = c(bhattacharya()[["url_transformed_bhat1"]]))
        } else if (input$bhat_nr_distr == 2){
            tibble(Distribution = c("1", "2"),
                   LRL = c(bhattacharya()[["lrl_transformed_bhat1"]], bhattacharya()[["lrl_transformed_bhat2"]]),
                   URL = c(bhattacharya()[["url_transformed_bhat1"]], bhattacharya()[["url_transformed_bhat2"]]))
        } else if (input$bhat_nr_distr == 3){
            tibble(Distribution = c("1", "2", "3"),
                   LRL = c(bhattacharya()[["lrl_transformed_bhat1"]], bhattacharya()[["lrl_transformed_bhat2"]], bhattacharya()[["lrl_transformed_bhat3"]]),
                   URL = c(bhattacharya()[["url_transformed_bhat1"]], bhattacharya()[["url_transformed_bhat2"]], bhattacharya()[["url_transformed_bhat3"]]))
        }
    })
    
    output$bhat_table <- renderTable({bhat_table()})
    
    ## Graphical presentation of results.
    ### Function for plot of distributions of original and modeled data (for both Bhattacharya and mixtools analysis).
    #### This function is required for the reactive function bhat_distr_plot.
    #### This function is required for the reactive function mixt_distr_plot.
    
    function_plot_distr <- function(variable_name, variable_data, bins, nr_distr, comb_org_model, list_lambda){
        modeled_distr <- ggplot() +
            geom_histogram(data = subset(comb_org_model, source == "original"),
                           breaks = seq(min(variable_data, na.rm = TRUE), max(variable_data, na.rm = TRUE), length.out = (bins + 1)),
                           aes(x = value, y = ..density..), 
                           color = "black", 
                           fill = "gray") +
            lims(x = c(0, ceiling(max(variable_data)))) +
            labs(title = str_c("Density plots original and modeled data (original scale)"),
                 x = str_c(variable_name),
                 y = "Density") +
            theme_classic() +
            theme(plot.title = element_text(face = "bold", hjust = 0.5),
                  panel.border = element_rect(fill = NA))
        
        if (nr_distr == 1) {
            modeled_distr +
                geom_density(data = subset(comb_org_model, source == "distribution 1"), aes(x = value, y = ..density.., color = source), size = 1)
        } else if (nr_distr == 2) {
            modeled_distr + 
                geom_density(data = subset(comb_org_model, source == "distribution 1"), aes(x = value, y = ..density.. * list_lambda[["lambda1"]], color = source), size = 1) +
                geom_density(data = subset(comb_org_model, source == "distribution 2"), aes(x = value, y = ..density.. * list_lambda[["lambda2"]], color = source), size = 1) +
                geom_density(data = subset(comb_org_model, source == "sum"), aes(x = value, y = ..density.., color = source), size = 1, bw = "ucv")
        } else if (nr_distr == 3) {
            modeled_distr + 
                geom_density(data = subset(comb_org_model, source == "distribution 1"), aes(x = value, y = ..density.. * list_lambda[["lambda1"]], color = source), size = 1) +
                geom_density(data = subset(comb_org_model, source == "distribution 2"), aes(x = value, y = ..density.. * list_lambda[["lambda2"]], color = source), size = 1) +
                geom_density(data = subset(comb_org_model, source == "distribution 3"), aes(x = value, y = ..density.. * list_lambda[["lambda3"]], color = source), size = 1) +
                geom_density(data = subset(comb_org_model, source == "sum"), aes(x = value, y = ..density.., color = source), size = 1, bw = "ucv")
        }
    }  
    
    ### Function for CDF plot of original and modeled data (for both Bhattacharya and mixtools analysis).
    #### This function is required for the reactive function bhat_cdf_plot.
    #### This function is required for the reactive function mixt_cdf_plot.
    function_plot_cdf <- function(variable_name, variable_data, comb_org_model){
        ggplot(data = comb_org_model, mapping = aes(x = value, color = source)) +
            stat_ecdf(size = 1) +
            geom_hline(yintercept = 0.025, color = "grey") +
            geom_hline(yintercept = 0.975, color = "grey") +
            lims(x = c(0, ceiling(max(variable_data)))) +
            labs(title = str_c("CDF original and modeled data (original scale)"),
                 x = as.character(variable_name),
                 y = "Probability") +
            theme_classic() +
            theme(plot.title = element_text(face = "bold", hjust = 0.5),
                  panel.border = element_rect(fill = NA))
    }
    
    ### Function for back transformation of distributions.
    #### This function is required for the reactive function plot_distr_bhat_data
    #### This function is required for the reactive function plot_distr_mixt_data.
    function_transform_distr <- function(input_distr, transformation){
        output_distr <- numeric(0)
        if (transformation == "invsqr"){
            output_distr <- sqrt(1 / input_distr)
        } else if (transformation == "inv"){
            output_distr <- 1 / input_distr
        } else if (transformation == "invsqrt"){
            output_distr <- (1 / input_distr) ** 2
        } else if (transformation == "log"){
            output_distr <- exp(input_distr)
        } else if (transformation == "none"){
            output_distr <- input_distr
        } else if (transformation == "sqr"){
            output_distr <- sqrt(input_distr)
        } else if (transformation == "sqrt"){
            output_distr <- input_distr ** 2
        } else {
            print("Transformation not allowed")
        }
        return(output_distr)
    }
    
    ### Reactive function to estimate mixing proportions of modeled distributions.
    #### This function is required for the reactive functions bhat_distr_plot and bhat_cdf_plot.
    
    # Lambda bhattacharya.
    # x * distr1(p10) + y * distr2(p10) + z * distr3(p10) = original(p10)
    # x * distr1(p25) + y * distr2(p25) + z * distr3(p25) = original(p25)
    # x * distr1(p50) + y * distr2(p50) + z * distr3(p50) = original(p50)
    # x * distr1(p75) + y * distr2(p75) + z * distr3(p75) = original(p75)
    # x * distr1(p90) + y * distr2(p90) + z * distr3(p90) = original(p90)
    # x +Y + Z = 1
    # https://statisticsglobe.com/solve-system-of-equations-in-r/
    # quantile function
    
    bhat_lambda <- reactive({
        req(input$bhat_nr_distr > 0)
        
        density_original <- density(var_selected_transformed(), bw = "ucv")
        function_density_original <- approxfun(density_original$x, density_original$y)
        
        if (input$bhat_nr_distr == 1){
            list_lambda <- c(lambda1 = 1)
        } else if (input$bhat_nr_distr == 2){
            matrix_a <- matrix(c(dnorm(bhattacharya()[["mu1"]], bhattacharya()[["mu1"]], bhattacharya()[["sigma1"]]),
                                 dnorm(bhattacharya()[["mu2"]], bhattacharya()[["mu1"]], bhattacharya()[["sigma1"]]),
                                 dnorm(bhattacharya()[["mu1"]], bhattacharya()[["mu2"]], bhattacharya()[["sigma2"]]),
                                 dnorm(bhattacharya()[["mu2"]], bhattacharya()[["mu2"]], bhattacharya()[["sigma2"]])),
                               ncol = 2)
            
            matrix_b <- matrix(c(function_density_original(bhattacharya()[["mu1"]]),
                                 function_density_original(bhattacharya()[["mu2"]])),
                               ncol = 1)
            
            matrix_solved <- solve(matrix_a, matrix_b)
            lambda1 <- matrix_solved[[1]] / (matrix_solved[[1]] + matrix_solved[[2]])
            lambda2 <- matrix_solved[[2]] / (matrix_solved[[1]] + matrix_solved[[2]])
            list_lambda <- c(lambda1 = lambda1, lambda2 = lambda2)
        } else if (input$bhat_nr_distr == 3){
            matrix_a <- matrix(c(dnorm(bhattacharya()[["mu1"]], bhattacharya()[["mu1"]], bhattacharya()[["sigma1"]]),
                                 dnorm(bhattacharya()[["mu2"]], bhattacharya()[["mu1"]], bhattacharya()[["sigma1"]]),
                                 dnorm(bhattacharya()[["mu3"]], bhattacharya()[["mu1"]], bhattacharya()[["sigma1"]]),
                                 dnorm(bhattacharya()[["mu1"]], bhattacharya()[["mu2"]], bhattacharya()[["sigma2"]]),
                                 dnorm(bhattacharya()[["mu2"]], bhattacharya()[["mu2"]], bhattacharya()[["sigma2"]]),
                                 dnorm(bhattacharya()[["mu3"]], bhattacharya()[["mu2"]], bhattacharya()[["sigma2"]]),
                                 dnorm(bhattacharya()[["mu1"]], bhattacharya()[["mu3"]], bhattacharya()[["sigma3"]]),
                                 dnorm(bhattacharya()[["mu2"]], bhattacharya()[["mu3"]], bhattacharya()[["sigma3"]]),
                                 dnorm(bhattacharya()[["mu3"]], bhattacharya()[["mu3"]], bhattacharya()[["sigma3"]])),
                               ncol = 3)
            
            matrix_b <- matrix(c(function_density_original(bhattacharya()[["mu1"]]),
                                 function_density_original(bhattacharya()[["mu2"]]),
                                 function_density_original(bhattacharya()[["mu3"]])),
                               ncol = 1)
            
            matrix_solved <- solve(matrix_a, matrix_b)
            lambda1 <- matrix_solved[[1]] / (matrix_solved[[1]] + matrix_solved[[2]] + matrix_solved[[3]])
            lambda2 <- matrix_solved[[2]] / (matrix_solved[[1]] + matrix_solved[[2]] + matrix_solved[[3]])
            lambda3 <- matrix_solved[[3]] / (matrix_solved[[1]] + matrix_solved[[2]] + matrix_solved[[3]])
            list_lambda <- c(lambda1 = lambda1, lambda2 = lambda2, lambda3 = lambda3)
        } else (list_lambda <- c(lambda1 = 1, lambda2 = 1, lambda3 = 1))
        
        return(list_lambda)
    })
    
    ### Reactive function to create a dataframe with original and modeled data.
    #### This function is required for the reactive functions bhat_distr_plot and bhat_cdf_plot.
    
    plot_distr_bhat_data <- reactive({
        req(input$bhat_nr_distr > 0)

        original <- tibble(value = var_selected()) %>% mutate(source = "original")
        
        if (input$bhat_nr_distr == 1 | input$bhat_nr_distr == 2 | input$bhat_nr_distr == 3){
            x1 <- rnorm(bhat_lambda()[["lambda1"]] * 10000, bhattacharya()[["mu1"]], bhattacharya()[["sigma1"]])
        }
        
        if (input$bhat_nr_distr == 2 | input$bhat_nr_distr == 3){
            x2 <- rnorm(bhat_lambda()[["lambda2"]] * 10000, bhattacharya()[["mu2"]], bhattacharya()[["sigma2"]])
        }
        
        if (input$bhat_nr_distr == 3){
            x3 <- rnorm(bhat_lambda()[["lambda3"]] * 10000, bhattacharya()[["mu3"]], bhattacharya()[["sigma3"]])
        }
        
        if (input$bhat_nr_distr == 1 | input$bhat_nr_distr == 2 | input$bhat_nr_distr == 3){
            distr1 <- tibble(value = function_transform_distr(x1, input$transformation)) %>% mutate(source = "distribution 1")
        }
        
        if (input$bhat_nr_distr == 2 | input$bhat_nr_distr == 3){
            distr2 <- tibble(value = function_transform_distr(x2, input$transformation)) %>% mutate(source = "distribution 2")
        }
        
        if (input$bhat_nr_distr == 3){
            distr3 <- tibble(value = function_transform_distr(x3, input$transformation)) %>% mutate(source = "distribution 3")
        }
        
        if (input$bhat_nr_distr == 1){
            comb_org_model <- bind_rows(original, distr1)
        } else if (input$bhat_nr_distr == 2){
            sum <- bind_rows(distr1, distr2) %>% mutate(source = "sum")
            comb_org_model <- bind_rows(original, distr1, distr2, sum)
        } else if (input$bhat_nr_distr == 3){
            sum <- bind_rows(distr1, distr2, distr3) %>% mutate(source = "sum")
            comb_org_model <- bind_rows(original, distr1, distr2, distr3, sum)
        }
        
        return(comb_org_model)
    })
    
    bhat_distr_plot <- reactive({
        function_plot_distr(input$variable, var_selected(), input$bins1, input$bhat_nr_distr, plot_distr_bhat_data(), bhat_lambda())
    })
    
    output$bhat_distr_plot <- renderPlot({bhat_distr_plot()})
    
    bhat_cdf_plot <- reactive({
        function_plot_cdf(input$variable, var_selected(), plot_distr_bhat_data())
    })
    
    output$bhat_cdf_plot <- renderPlot({bhat_cdf_plot()})
    
    # Mixtools analysis
    
    ## Reactive header for mixtools analysis.
    
    output$mixtools_page_header <- renderText({
        str_c("Mixtools analysis for ", input$variable)
    })
    
    ## Reactive functions for mixtools analysis,
    
    ### Distribution mode.
    
    mixt_distr_mode = reactive({
        str_to_lower(input$mixt_type_distr)
    })
    
    ### Reactive function for mixtools analysis (normal distribution).
    mixtools_normal <- reactive({
        req(input$mixt_type_distr == "Normal")
        
        fit <- normalmixEM(var_selected_transformed(), k = as.numeric(input$mixt_nr_distr), maxit = 3000)
        
        # Results distribution(s).
        if (input$mixt_nr_distr == "1" | input$mixt_nr_distr == "2" | input$mixt_nr_distr == "3"){
            lrl_mixt_distr1 <- qnorm(0.025, fit$mu[1], fit$sigma[1])
            url_mixt_distr1 <- qnorm(0.975, fit$mu[1], fit$sigma[1])
            results_distr1 <- list(mu = fit$mu[1], sigma = fit$sigma[1], lambda = fit$lambda[1], lrl_mixt = lrl_mixt_distr1, url_mixt = url_mixt_distr1)
        } else {
            results_distr1 <- list(mu = NA, sigma = NA, lambda = NA, lrl_mixt = NA, url_mixt = NA)
        }
        
        if (input$mixt_nr_distr == "2" | input$mixt_nr_distr == "3") {
            lrl_mixt_distr2 <- qnorm(0.025, fit$mu[2], fit$sigma[2])
            url_mixt_distr2 <- qnorm(0.975, fit$mu[2], fit$sigma[2])
            results_distr2 <- list(mu = fit$mu[2], sigma = fit$sigma[2], lambda = fit$lambda[2], lrl_mixt = lrl_mixt_distr2, url_mixt = url_mixt_distr2)
        } else {
            results_distr2 <- list(mu = NA, sigma = NA, lambda = NA, lrl_mixt = NA, url_mixt = NA)
        }
        
        if (input$mixt_nr_distr == "3") {
            lrl_mixt_distr3 <- qnorm(0.025, fit$mu[3], fit$sigma[3])
            url_mixt_distr3 <- qnorm(0.975, fit$mu[3], fit$sigma[3])
            results_distr3 <- list(mu = fit$mu[3], sigma = fit$sigma[3], lambda = fit$lambda[3], lrl_mixt = lrl_mixt_distr3, url_mixt = url_mixt_distr3)
        } else {
            results_distr3 <- list(mu = NA, sigma = NA, lambda = NA, lrl_mixt = NA, url_mixt = NA)
        }
        
        # Transformation of ci limits if applicable.
        if (input$mixt_nr_distr == "1" | input$mixt_nr_distr == "2" | input$mixt_nr_distr == "3"){
            ci1 <- function_transform_ci(results_distr1[["lrl_mixt"]], results_distr1[["url_mixt"]], input$transformation)
        } else {
            ci1 <- list(NA, NA)
        }
        
        if (input$mixt_nr_distr == "2" | input$mixt_nr_distr == "3") {
            ci2 <- function_transform_ci(results_distr2[["lrl_mixt"]], results_distr2[["url_mixt"]], input$transformation)
        } else {
            ci2 <- list(NA, NA)
        }
        
        if (input$mixt_nr_distr == "3") {
            ci3 <- function_transform_ci(results_distr3[["lrl_mixt"]], results_distr3[["url_mixt"]], input$transformation)
        } else {
            ci3 <- list(NA, NA)
        }
        
        # Output.
        list(mu1 = results_distr1[["mu"]], sigma1 = results_distr1[["sigma"]], lambda1 = results_distr1[["lambda"]], lrl_transformed_mixt1 = ci1[["lrl_transformed"]], url_transformed_mixt1 = ci1[["url_transformed"]], 
             mu2 = results_distr2[["mu"]], sigma2 = results_distr2[["sigma"]], lambda2 = results_distr2[["lambda"]], lrl_transformed_mixt2 = ci2[["lrl_transformed"]], url_transformed_mixt2 = ci2[["url_transformed"]],
             mu3 = results_distr3[["mu"]], sigma3 = results_distr3[["sigma"]], lambda3 = results_distr3[["lambda"]], lrl_transformed_mixt3 = ci3[["lrl_transformed"]], url_transformed_mixt3 = ci3[["url_transformed"]])
    })
    
    ### Table with reference interval based on mixtools analysis (normal distribution).
    mixt_table_normal <- reactive({
        req(input$mixt_nr_distr > 0)
        if (input$mixt_nr_distr == 1){
            tibble(Distribution = c("1"),
                   LRL = c(mixtools_normal()[["lrl_transformed_mixt1"]]),
                   URL = c(mixtools_normal()[["url_transformed_mixt1"]]))
        } else if(input$mixt_nr_distr == 2){
            tibble(Distribution = c("1", "2"),
                   LRL = c(mixtools_normal()[["lrl_transformed_mixt1"]], mixtools_normal()[["lrl_transformed_mixt2"]]),
                   URL = c(mixtools_normal()[["url_transformed_mixt1"]], mixtools_normal()[["url_transformed_mixt2"]]))
        } else if (input$mixt_nr_distr == 3){
            tibble(Distribution = c("1", "2", "3"),
                   LRL = c(mixtools_normal()[["lrl_transformed_mixt1"]], mixtools_normal()[["lrl_transformed_mixt2"]], mixtools_normal()[["lrl_transformed_mixt3"]]),
                   URL = c(mixtools_normal()[["url_transformed_mixt1"]], mixtools_normal()[["url_transformed_mixt2"]], mixtools_normal()[["url_transformed_mixt3"]]))
        }
    })
    
    ### Reactive function to create a dataframe with original and modeled data (normal distribution).
    plot_distr_mixt_data_normal <- reactive({
        req(input$mixt_nr_distr > 0)
        
        original <- tibble(value = var_selected()) %>% mutate(source = "original")
        
        if (input$mixt_nr_distr == 1 | input$mixt_nr_distr == 2 | input$mixt_nr_distr == 3){
            x1 <- rnorm(mixtools_normal()[["lambda1"]] * 10000, mixtools_normal()[["mu1"]], mixtools_normal()[["sigma1"]])
        }
        
        if (input$mixt_nr_distr == 2 | input$mixt_nr_distr == 3){
            x2 <- rnorm(mixtools_normal()[["lambda2"]] * 10000, mixtools_normal()[["mu2"]], mixtools_normal()[["sigma2"]])
        }
        
        if (input$mixt_nr_distr == 3){
            x3 <- rnorm(mixtools_normal()[["lambda3"]] * 10000, mixtools_normal()[["mu3"]], mixtools_normal()[["sigma3"]])
        }
        
        if (input$mixt_nr_distr == 1 | input$mixt_nr_distr == 2 | input$mixt_nr_distr == 3){
            distr1 <- tibble(value = function_transform_distr(x1, input$transformation)) %>% mutate(source = "distribution 1")
        }
        
        if (input$mixt_nr_distr == 2 | input$mixt_nr_distr == 3){
            distr2 <- tibble(value = function_transform_distr(x2, input$transformation)) %>% mutate(source = "distribution 2")
        }
        
        if (input$mixt_nr_distr == 3){
            distr3 <- tibble(value = function_transform_distr(x3, input$transformation)) %>% mutate(source = "distribution 3")
        }
        
        if (input$mixt_nr_distr == 1){
            comb_org_model <- bind_rows(original, distr1)
        } else if (input$mixt_nr_distr == 2){
            sum <- bind_rows(distr1, distr2) %>% mutate(source = "sum")
            comb_org_model <- bind_rows(original, distr1, distr2, sum)
        } else if (input$mixt_nr_distr == 3){
            sum <- bind_rows(distr1, distr2, distr3) %>% mutate(source = "sum")
            comb_org_model <- bind_rows(original, distr1, distr2, distr3, sum)
        }
        
        return(comb_org_model)
    })
    
    ### Reactive function for mixtools analysis (gamma distribution).
    mixtools_gamma <- reactive({
        req(input$mixt_type_distr == "Gamma", input$mixt_nr_distr > 0, input$transformation == "none")
        
        fit <- gammamixEM(var_selected_transformed(), k = as.numeric(input$mixt_nr_distr), maxit = 3000)
        
        # Results distribution(s).
        if (input$mixt_nr_distr == "1" | input$mixt_nr_distr == "2" | input$mixt_nr_distr == "3"){
            lrl_mixt_distr1 <- qgamma(0.025, shape = fit$gamma.pars[1], scale = fit$gamma.pars[2])
            url_mixt_distr1 <- qgamma(0.975, shape = fit$gamma.pars[1], scale = fit$gamma.pars[2])
            results_distr1 <- list(alpha = fit$gamma.pars[1], beta = fit$gamma.pars[2], lambda = fit$lambda[1], lrl_mixt = lrl_mixt_distr1, url_mixt = url_mixt_distr1)
        } else {
            results_distr1 <- list(alpha = NA, beta = NA, lambda = NA, lrl_mixt = NA, url_mixt = NA)
        }
        
        if (input$mixt_nr_distr == "2" | input$mixt_nr_distr == "3") {
            lrl_mixt_distr2 <- qgamma(0.025, shape = fit$gamma.pars[3], scale = fit$gamma.pars[4])
            url_mixt_distr2 <- qgamma(0.975, shape = fit$gamma.pars[3], scale = fit$gamma.pars[4])
            results_distr2 <- list(alpha = fit$gamma.pars[3], beta = fit$gamma.pars[4], lambda = fit$lambda[2], lrl_mixt = lrl_mixt_distr2, url_mixt = url_mixt_distr2)
        } else {
            results_distr2 <- list(alpha = NA, beta = NA, lambda = NA, lrl_mixt = NA, url_mixt = NA)
        }

        if (input$mixt_nr_distr == "3") {
            lrl_mixt_distr3 <- qgamma(0.025, shape = fit$gamma.pars[5], scale = fit$gamma.pars[6])
            url_mixt_distr3 <- qgamma(0.975, shape = fit$gamma.pars[5], scale = fit$gamma.pars[6])
            results_distr3 <- list(shape = fit$gamma.pars[5], beta = fit$gamma.pars[6], lambda = fit$lambda[3], lrl_mixt = lrl_mixt_distr3, url_mixt = url_mixt_distr3)
        } else {
            results_distr3 <- list(alpha = NA, beta = NA, lambda = NA, lrl_mixt = NA, url_mixt = NA)
        }
        
        # Output.
        list(alpha1 = results_distr1[["alpha"]], beta1 = results_distr1[["beta"]], lambda = results_distr1[["lambda"]], lrl_mixt1 = results_distr1[["lrl_mixt"]], url_mixt1 = results_distr1[["url_mixt"]], 
             alpha2 = results_distr2[["alpha"]], beta2 = results_distr2[["beta"]], lambda = results_distr2[["lambda"]], lrl_mixt2 = results_distr2[["lrl_mixt"]], url_mixt2 = results_distr2[["url_mixt"]],
             alpha3 = results_distr3[["alpha"]], beta3 = results_distr3[["beta"]], lambda = results_distr3[["lambda"]], lrl_mixt3 = results_distr3[["lrl_mixt"]], url_mixt3 = results_distr3[["url_mixt"]])
    })
    
    ### Table with reference interval based on mixtools analysis (gamma distribution).
    mixt_table_gamma <- reactive({
        req(input$mixt_nr_distr > 0)
        if (input$mixt_nr_distr == 1){
            tibble(Distribution = c("1"),
                   LRL = c(mixtools_gamma()[["lrl_mixt1"]]),
                   URL = c(mixtools_gamma()[["url_mixt1"]]))
        } else if(input$mixt_nr_distr == 2){
            tibble(Distribution = c("1", "2"),
                   LRL = c(mixtools_gamma()[["lrl_mixt1"]], mixtools_gamma()[["lrl_mixt2"]]),
                   URL = c(mixtools_gamma()[["url_mixt1"]], mixtools_gamma()[["url_mixt2"]]))
        } else if (input$mixt_nr_distr == 3){
            tibble(Distribution = c("1", "2", "3"),
                   LRL = c(mixtools_gamma()[["lrl_mixt1"]], mixtools_gamma()[["lrl_mixt2"]], mixtools_gamma()[["lrl_mixt3"]]),
                   URL = c(mixtools_gamma()[["url_mixt1"]], mixtools_gamma()[["url_mixt2"]], mixtools_gamma()[["url_mixt3"]]))
        }
    })
    
    ### Reactive function to create a dataframe with original and modeled data (gammma distribution).
    plot_distr_mixt_data_gamma <- reactive({
        req(input$mixt_nr_distr > 0)
        original <- tibble(value = var_selected()) %>% mutate(source = "original")
        
        if (input$mixt_nr_distr == 1 | input$mixt_nr_distr == 2 | input$mixt_nr_distr == 3){
            distr1 <- tibble(value = rgamma(mixtools_normal()[["lambda1"]] * 10000, shape = mixtools_gamma()[["alpha1"]], scale = mixtools_gamma()[["beta1"]]))
            distr1 <- distr1 %>% mutate(source = "distribution 1")
        }
        
        if (input$mixt_nr_distr == 2 | input$mixt_nr_distr == 3){
            distr2 <- tibble(value = rgamma(mixtools_normal()[["lambda2"]] * 10000, shape = mixtools_gamma()[["alpha2"]], scale = mixtools_gamma()[["beta2"]]))
            distr2 <- distr2 %>% mutate(source = "distribution 2")
        }
        
        if (input$mixt_nr_distr == 3){
            distr3 <- tibble(value = rgamma(mixtools_normal()[["lambda3"]] * 10000, shape = mixtools_gamma()[["alpha3"]], scale = mixtools_gamma()[["beta3"]]))
            distr3 <- distr3 %>% mutate(source = "distribution 3")
        }
        
        if (input$mixt_nr_distr == 1){
            comb_org_model <- bind_rows(original, distr1)
        } else if (input$mixt_nr_distr == 2){
            sum <- bind_rows(distr1, distr2) %>% mutate(source = "sum")
            comb_org_model <- bind_rows(original, distr1, distr2, sum)
        } else if (input$mixt_nr_distr == 3){
            sum <- bind_rows(distr1, distr2, distr3) %>% mutate(source = "sum")
            comb_org_model <- bind_rows(original, distr1, distr2, distr3, sum)
        }

        return(comb_org_model)
    })
    
    ## Presentation of results of mixtools analysis.
    
    ### Tabulated presentation of results.
    
    mixt_table <- reactive({
        if (input$mixt_type_distr == "Normal"){
            mixt_table_normal()
        } else if (input$mixt_type_distr == "Gamma"){
            mixt_table_gamma()
        }
    })
    
    output$mixt_table <- renderTable({mixt_table()})
    
    ### Graphical presentation of results.
    
    mixt_distr_plot <- reactive({
        if (input$mixt_type_distr == "Normal"){
            function_plot_distr(input$variable, var_selected(), input$bins1, input$mixt_nr_distr, plot_distr_mixt_data_normal(), mixtools_normal())
        } else if (input$mixt_type_distr == "Gamma" & input$transformation != "none"){
            validate("Error: Transformation not allowed for gamma distribution. Select 'none' in Box-Cox tab.")
        } else if (input$mixt_type_distr == "Gamma" & input$transformation == "none"){
            function_plot_distr(input$variable, var_selected(), input$bins1, input$mixt_nr_distr, plot_distr_mixt_data_gamma(), mixtools_gamma())
        }
    })
    
    output$mixt_distr_plot <- renderPlot({mixt_distr_plot()})
    
    mixt_cdf_plot <- reactive({
        if (input$mixt_type_distr == "Normal"){
            function_plot_cdf(input$variable, var_selected(), plot_distr_mixt_data_normal())
        } else if (input$mixt_type_distr == "Gamma"){
            function_plot_cdf(input$variable, var_selected(), plot_distr_mixt_data_gamma())
        }
    })
    
    output$mixt_cdf_plot <- renderPlot({mixt_cdf_plot()})
    
    # Downloadable report.
    ## List of analyses included in report.
    list_analyses <- reactive({
        list_always <- list(var = input$variable,
                            n = n_var_selected(),
                            bins = input$bins1, 
                            range_lower = input$lower_value, 
                            range_upper = input$upper_value, 
                            hist_untransformed = hist_untransformed(), 
                            qq_untransformed = qq_untransformed(), 
                            hist_transformed = hist_transformed1(),
                            qq_transformed = qq_transformed(),
                            transformation = transform_hist_label_selected())
        
        if (input$lower_value < 0 | input$upper_value < 0){
            list_box <- list(box_print_no = TRUE,
                             box_print_yes = FALSE,
                             box_cox_plot = NA)
        } else {
            list_box <- list(box_print_no = FALSE,
                             box_print_yes = TRUE,
                             box_cox_plot = box_cox())
        }
        
        if ("Bhattacharya" %in% input$analyses_in_report){
            list_bhat <- list(bhat_print = TRUE,
                              bhat_dlogcount = bhattacharya_plot_reg(),
                              bhat_sections = bhat_sections(),
                              bhat_density = bhat_distr_plot(),
                              bhat_cdf = bhat_cdf_plot(),
                              bhat_ri = bhat_table())
        } else {
            list_bhat <- list(bhat_print = FALSE,
                              bhat_dlogcount = NA,
                              bhat_sections = NA,
                              bhat_density = NA,
                              bhat_cdf = NA,
                              bhat_ri = NA)
        }
        
        if ("Mixtools" %in% input$analyses_in_report){
            list_mixt <- list(mixt_print = TRUE,
                              mixt_mode = mixt_distr_mode(),
                              mixt_density = mixt_distr_plot(),
                              mixt_cdf = mixt_cdf_plot(),
                              mixt_ri = mixt_table())
        } else {
            list_mixt <- list(mixt_print = FALSE,
                              mixt_mode = NA,
                              mixt_density = NA,
                              mixt_cdf = NA,
                              mixt_ri = NA)
        }
        
        list_combined <-  c(list_always, list_box, list_bhat, list_mixt)
    })
    
    ## Generate report.
    output$download_report <- downloadHandler(
        filename = function() {
            str_c(str_c("Reference interval ", input$variable, " [created ", Sys.Date(), "]"), sep = ".", switch(
                input$format, PDF = "pdf", Word = "docx", HTML = "html"
            ))
        },
        
        content = function(file) {
            # Busy indicator while compiling file.
            showModal(modalDialog("Compiling report...", footer=NULL))
            on.exit(removeModal())
            
            # Copy the report file to a temporary directory before processing it, in case we don't have write permissions to the current working dir (which can happen when deployed).
            temp_report <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", temp_report, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- as.list(list_analyses())
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(temp_report, output_format = as.character(switch(input$format, PDF = "pdf_document", Word = "word_document", HTML = "html_document")), output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
    
})
