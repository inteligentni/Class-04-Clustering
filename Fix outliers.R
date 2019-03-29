# Fix the outlier values for a variable (dataframe column) with outliers - 
# replace each outlier value with a specific percentile value 
# of the data, typically 90th or 95th at the upper end, and 5th or 10th at the lower end:

# boxplot(<dataset>$<column name>)$stats[c(1, 5), ]           # get the whiskers
# <sorted outliers> <-                                        # get and sort the outliers
#   sort(boxplot.stats(<dataset>$<column name>)$out)
# <quantiles> <- quantile(<dataset>$<column name>,            # examine the 90th, 95th, ..., percentile
#                         probs = seq(from = 0.9, to = 1, by = 0.025))
# <new max value> <-                                          # the value to replace the outliers
#   as.numeric(quantile(<dataset>$<column name>,              # pick <percentile> closest to 
#                       probs = <percentile>))                # the upper whisker
# <dataset>$<column name>[<dataset>$<column name> > <new max value>] <- # replace the outliers
#   <new max value>
# <quantiles> <- quantile(<dataset>$<column name>,            # examine the 0th, 5th, ..., percentile
#                         probs = seq(from = 0.0, to = 0.1, by = 0.025))
# <new min value> <-                                          # the value to replace the outliers
#   as.numeric(quantile(<dataset>$<column name>,              # pick <percentile> closest to 
#                       probs = <percentile>))                # the lower whisker
# <dataset>$<column name>[<dataset>$<column name> < <new min value>] <- # replace the outliers
#   <new min value>

fixOutliers <- function(dataframe,
                        column) {
  if (length(boxplot.stats(dataframe[[column]])$out) > 0) {   # if there ARE outliers...
    outliers.sorted <- 
      sort(boxplot.stats(dataframe[[column]])$out)            # get and sort the outliers
    lower.whisker <- 
      boxplot.stats(dataframe[[column]])$stats[1]             # get the lower whisker
    upper.whisker <- 
      boxplot.stats(dataframe[[column]])$stats[5]             # get the upper whisker
    if (outliers.sorted[1] < lower.whisker) {                 # if there are lower outliers...
      lower.outliers <-                                       # find them as the values less than
        outliers.sorted[outliers.sorted < lower.whisker]      #   the lower whisker
      quantiles <- quantile(dataframe[[column]],              # find 0th, 2.5th, ..., 10th quantiles 
                            probs = seq(from = 0, to = 0.1, by = 0.025))
      indices <- which(as.numeric(quantiles) > lower.whisker) # select those that are > lower whisker
      new.min <- as.numeric(quantiles[indices[1]])                    # new min is the quantile 
                                                                      # closest to the lower whisker
      if (length(new.min) == 0) {                             # this happens when the selected quantile 
        new.min <- lower.whisker                              # is still less than the lower whisker
      }
      dataframe[[column]][dataframe[[column]] < new.min] <- new.min
    }
    if (outliers.sorted[length(outliers.sorted)] > 
        upper.whisker) {                                      # if there are upper outliers...
      upper.outliers <-                                       # find them as the values greater than 
        outliers.sorted[outliers.sorted > upper.whisker]      #   the upper whisker
      quantiles <- quantile(dataframe[[column]],              # find 90th, 92.5th, ..., 100th quantiles
                            probs = seq(from = 0.9, to = 1.0, by = 0.025))
      indices <- which(as.numeric(quantiles) < upper.whisker) # select those that are < upper whisker
      new.max <- as.numeric(quantiles[indices[length(indices)]])      # new max is the quantile 
                                                                      # closest to the upper whisker
      if (length(new.max) == 0) {                             # this happens when the selected quantile 
        new.max <- upper.whisker                              # is still greater than the upper whisker
      }
      dataframe[[column]][dataframe[[column]] > new.max] <- new.max   
    }
  }
  dataframe
}

# Fix the outlier values for a variable (dataframe column) with outliers - 
# all values in the variable which take values above the 95th and below the 5th percentile 
# with those percentile values respectively:

fixOutliers0595Simplest <- function(dataframe,                  # dataframe
                                    column) {                   # its column, possibly with outliers
  quantiles <- quantile(dataframe[[column]], c(0.05, 0.95))
  dataframe[[column]][dataframe[[column]] < quantiles[1]] <- quantiles[1]
  dataframe[[column]][dataframe[[column]] > quantiles[2]] <- quantiles[2]
  dataframe
}
