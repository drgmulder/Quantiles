
# This file is a generated template, your changes will not be overwritten

quantsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "quantsClass",
    inherit = quantsBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
        if (self$options$distr == "normdist" ) {
            results <- c("z-value"=qnorm(self$options$quant))
            self$results$text$setContent(results)
        } # end if norm 
        if (self$options$distr == "tdist") {
            if (self$options$N == 0 | self$options$grp == 0) {
                results <- "Please fill in total sample size and/or number of groups"
                self$results$text$setContent(results)
            } else {
            df <- self$options$N - self$options$grp
            results <- c("t-value"=qt(self$options$quant, df), "df"=df)
            self$results$text$setContent(results) }
        } # end if t
        
        if (self$options$distr == "chidist") {
            if (self$options$numRows == 0 | self$options$numCols == 0) {
                results <- "Please fill number of Rows and/or Columns."
                self$results$text$setContent(results)
            } else {
                if (self$options$numRows == 1) { 
                    df <- self$options$numCols - 1 
                    results <- c("chisq-value"=qchisq(self$options$quant, df), "df"=df)
                    self$results$text$setContent(results) 
                } else {
                df <- (self$options$numRows - 1)*(self$options$numCols - 1)
                results <- c("chisq-value"=qchisq(self$options$quant, df), "df"=df)
                self$results$text$setContent(results) }
                }
            
        } # end if chisq
            if (self$options$distr == "fdist") {
                if (self$options$N == 0 | self$options$grp == 0) {
                    results <- "Please fill in total sample size and/or number of groups"
                    self$results$text$setContent(results)
                } else {
                    df1 <- self$options$grp - 1
                    if (df1 == 0) df1 <- 1
                    df2 <- self$options$N - self$options$grp 
                    results <- c("F-value"=qf(self$options$quant, df1, df2), "df1"=df1, "df2"=df2)
                    self$results$text$setContent(results) }
                
            } # end if F
          plotData <- data.frame(x=c(-3, 3))
          
          image <- self$results$plot
          image$setState(plotData)
          
        },
        .plot = function(image, ...) {
          # the plot function 
          plotData <- image$state
          
          thePlot <- ggplot(plotData, aes(x)) + 
            stat_function(fun=dnorm) +
            scale_x_continuous(name="Quantile", breaks=seq(-3, 3, by=.50)) + 
            scale_y_continuous(name="Density") + 
            geom_vline(xintercept = qnorm(.975), linetype="dashed") +
            ggtitle("Standard Normal Distribution") +
            theme(plot.title = element_text(hjust = 0.5))
          print(thePlot)
          TRUE
        })
)
