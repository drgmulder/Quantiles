
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
            plotData <- data.frame(x=c(-3, 3))
            } # end if norm 
        if (self$options$distr == "tdist") {
            if (self$options$N == 0 | self$options$grp == 0) {
                results <- "Please fill in total sample size and/or number of groups"
                self$results$text$setContent(results)
            } else {
            df <- self$options$N - self$options$grp
            results <- c("t-value"=qt(self$options$quant, df), "df"=df)
            self$results$text$setContent(results) 
            plotData <- data.frame(x=c(-3, 3))}
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
                    plotData <- data.frame(x=c(0, ceiling(qchisq(.999, df))))
                } else {
                df <- (self$options$numRows - 1)*(self$options$numCols - 1)
                results <- c("chisq-value"=qchisq(self$options$quant, df), "df"=df)
                self$results$text$setContent(results)
                plotData <- data.frame(x=c(0, ceiling(qchisq(.995, df))))}
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
                    self$results$text$setContent(results) 
                    plotData <- data.frame(x=c(0, ceiling(qf(.995, df1, df2))))}
                
            } # end if F
          
          
          image <- self$results$plot
          image$setState(plotData)
          
        },
        .plot = function(image, ...) {
          # the plot function 
          plotData <- image$state
          
          if (self$options$distr == "normdist") {
            densFunc <- stat_function(fun = dnorm)
            xint <- qnorm(self$options$quant)
            theTitle <- ggtitle("Standard Normal Distribution")
          }
          if (self$options$distr == "tdist") {
            df = self$options$N - self$options$grp
            densFunc <- stat_function(fun = dt, args = list(df = df))
            xint <- qt(self$options$quant, df)
            theTitle <- ggtitle("Student t Distribution")
          }
          
          if (self$options$distr == "chidist") {
            if (self$options$numRows == 1) { 
              df <- self$options$numCols - 1 
            } else {
              df <- (self$options$numRows - 1)*(self$options$numCols - 1)
              }
            
            densFunc <- stat_function(fun = dchisq, args = list(df = df))
            xint <- qchisq(self$options$quant, df)
            theTitle <- ggtitle("Chi-squared Distribution")
          }
          
          if (self$options$distr == "fdist") {
            df1 <- self$options$grp - 1
            if (df1 == 0) df1 <- 1
            df2 <- self$options$N - self$options$grp 
            
            densFunc <- stat_function(fun = df, args = list(df1 = df1, df2 = df2))
            xint <- qf(self$options$quant, df1, df2)
            theTitle <- ggtitle("F Distribution")
          }
          
          thePlot <- ggplot(plotData, aes(x)) + 
            densFunc +
            scale_x_continuous(name="Quantile", breaks=seq(min(plotData$x), max(plotData$x), by=.50)) + 
            scale_y_continuous(name="Density") + 
            geom_vline(xintercept = xint, linetype="dashed") +
            theTitle +
            theme(plot.title = element_text(hjust = 0.5))
          print(thePlot)
          TRUE
        })
)
