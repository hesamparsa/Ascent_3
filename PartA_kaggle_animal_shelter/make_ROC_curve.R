make_ROC <- function(res = res_LM){
  
  #predict probabilities
  trainPred <- predict(res$model, res$train, type = "prob")
  
  library(pROC)
  # make ROC object
  rocCurve   <- roc(response = res$train$outcome,
                    predictor = trainPred[, "yes"])
  plot(rocCurve)
  
  # Structure of rocCurve obj.
  names(rocCurve)
  rocCurve$thresholds
  rocCurve$sensitiviti
  
  ####################### cut-off
  # plot and choose threshold based on desired sens or specif
  
  library(dplyr); library(tidyr)
  
  resDF <- data.frame(pr_threshold = rocCurve$thresholds, 
                      sensitivity = rocCurve$sensitiviti, 
                      specificity = rocCurve$specificiti)
  
  # ROC curve
  p_roc <- ggplot(data=resDF, aes(x=sensitivity, y = specificity)) +
    #geom_point() + 
    geom_line() +
    #reference line
    geom_line(data = data.frame(sensitivity = 0:1, specificity = 1:0), linetype = "dashed")
  
  resDF <- 
    resDF %>% 
    filter(! is.infinite(pr_threshold)) %>%
    gather(metrics, value, -pr_threshold)
   
  p <- ggplot(data=resDF, aes(x=pr_threshold, y = value, col = metrics)) +
    geom_point() + geom_line()
  
  # use plotly for interactive graph  
  
  
  ########### Use a "best" cut-offs
  
  
  #plot(rocCurve, print.thres = "best") # shows "best" cut-off value for p and sens-spec coordinates in parentecies
  # best means highest sum sensitivity + specificity - see ?plot.roc
  
  resDF_best <- resDF %>% 
    group_by(pr_threshold) %>%
    mutate(sumSS = sum(value)) %>%
    ungroup() %>%
    top_n(1, sumSS)
  
  print(resDF_best)
  
  best.cut.off <- resDF_best$pr_threshold[1]
  
  p_roc <- p_roc + geom_point(data = spread(resDF_best, metrics, value), col = 2, size = 2) +
    annotate("text", x = resDF_best$value[1] + 0.05, y = resDF_best$value[2]+ .05,
             label = round(resDF_best$pr_threshold[1], digits = 2), col = 2)
    
  
  library(ggplot2); library(grid); library(gridExtra)
  grid.arrange(p_roc, p, ncol=2, nrow =1)
  
  #best.cut.off <<- best.cut.off
  #trainPred <<- trainPred
  list(best.cut.off = best.cut.off, trainPred = trainPred)
}

