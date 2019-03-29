# Utility functions for using the Elbow method for selecting the optimal K

# The Elbow method is based on the sum of squared differences between data points and cluster centres,
# that is, the sum of within_SS for all the clusters (tot.withinss) 

getElbowMethodParameters <- function(x) {     # x: dataset (numeric variables only, no clusters)
  elbow.df <- data.frame()                    # create an empty dataframe to contain elbow parameters
  for(k in 2:8) {                             # run kmeans for all K values in the range 2:8
    set.seed(3108)
    km <- kmeans(x, 
                 centers = k, 
                 iter.max=20, 
                 nstart = 1000)
    elbow.df <- rbind(elbow.df,                     # insert into the elbow dataframe:
                      c(k,                          # number of clusters in this iteration
                        km$tot.withinss,            # tot.withinss in this iteration
                        km$betweenss / km$totss))   # between_SS / total_SS in this iteration
  }
  names(elbow.df) <- c("cluster", "tot.withinss", "between_SS / total_SS")
  elbow.df
}

# Plot the Elbow curve
plotElbow <- function(x) {                    # x: the Elbow dataframe from getElbowMethodParameters()
  ggplot(data = x, aes(x = cluster, y = tot.withinss, group = 1)) + 
    theme_bw() + 
    geom_line(color = "darkgreen") +
    theme(text = element_text(size = 15)) +
    ggtitle("Error reduction for different values of K\n") +
    xlab("\nK (No. of clusters)") + 
    ylab("Total Within Cluster Sum of Squares\n") +
    scale_x_continuous(breaks = seq(from = 0, to = 8, by = 1))
}

## Get differences between each two subsequent values of an input numeric vector
getDifferences <- function(values) {
  differences <- vector(mode = "numeric", length = length(values))
  differences[1] <- NA
  for(i in 1:(length(values) - 1)) {
    differences[i+1] <- abs(values[i+1] - values[i])
  }
  differences
}

