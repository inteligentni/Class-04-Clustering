# 
# Clustering
# Vladan Devedzic, Sep 15, 2017
# 


# (Installing and) Loading the required R packages:
# install.packages("ggplot2")
library(ggplot2)


###########
# Dataset #
###########

# Read the dataset:
source("Prepare The Beatles song dataset for clustering.R")
prepareTheBeatlesDatasetForClustering()
# saveRDS(object = <dataframe or another R object>, file = "<filename>")  # save R object for the next session
# <dataframe or another R object> <- readRDS(file = "<filename>")         # restore R object in the next session
the.beatles.songs.num <- 
  readRDS("The Beatles songs dataset (numeric), v4.1.RData")
  
###########
# K-Means #
###########

# Check if there are outliers in the data, using boxplots:
# boxplot(<dataset>$<column name>, xlab = "<column name>")                    # basic boxplot for <column name>
# boxplot(<dataset>)                                                          # basic boxplots for all columns
# boxplot(<dataset>)$stats                                                    # basic boxplots for all columns, stats
# boxplot(<dataset>)$stats[c(1, 5), ]                                         # basic boxplots - whiskers
# <output var> <- ggplot(<dataset>,                      # ggplot2 boxplots
#                        aes(x = "", 
#                            y = <column name>)) +       # show boxplot of <column name>
#   geom_boxplot(width = 0.5, fill = "<color>") +        # boxplot width and color
#   stat_boxplot(geom ='errorbar', width = 0.15) +       # show whiskers, control their width
#   guides(fill = FALSE) +                               # no legend (it makes no sense here)
#   xlab("")                                             # no x-axis label (it makes no sense here)
# Compare outputs of boxplot(<dataset>)$stats and summary(dataset)
boxplot(the.beatles.songs.num)$stats
boxplot(the.beatles.songs.num$Duration, xlab = "Duration")

# Boxplotting all numeric features using the boxplotFeature() utility function:
# source("Boxplotting.R")
# boxplotFeature(<dataset with numeric features>,   # dataframe with numeric features
#                "<numeric feature>",               # numeric feature to boxplot (its name, passed as a string)
#                "<color>")                         # boxplot fill color
source("Boxplotting.R")
boxplotFeature(the.beatles.songs.num, "Duration", "red")
boxplotFeature(the.beatles.songs.num, "Other.releases", "chartreuse")
boxplotFeature(the.beatles.songs.num, "Covered.by", "orange")
boxplotFeature(the.beatles.songs.num, "Top.50.Billboard", "yellow")

# Fix the outlier values for each variable with outliers - 
# replace each outlier value with a specific percentile value 
# of the data, typically 90th or 95th:
# boxplot(<dataset>$<column name>, xlab = "<column name>")    # basic boxplot for <column name>
# boxplot.stats(<dataset>$<column name>)                      # examine the boxplot more closely
# boxplot.stats(<dataset>$<column name>)$out                  # examine the outliers more closely
# boxplot.stats(<dataset>$<column name>)$stats[c(1, 5)]       # get the whiskers
# boxplot.stats(<dataset>$<column name>)$stats[1]             # get the lower whisker
# boxplot.stats(<dataset>$<column name>)$stats[5]             # get the upper whisker
# sort(boxplot.stats(<dataset>$<column name>)$out)            # get and sort the outliers
# <quantiles> <- quantile(<dataset>$<column name>,            # examine the 90th, 95th, ..., percentile
#                         probs = seq(from = 0.9, to = 1, by = 0.025))
# <new max value> <-                                          # the value to replace the outliers
#   as.numeric(quantile(<dataset>$<column name>,              # pick <percentile> closest to 
#                       probs = <percentile>))                # the upper whisker
# <dataset>$<column name>[<dataset>$<column name> > 
#                         <new max value>] <-                 # replace the outliers
#   <new max value>
# <quantiles> <- quantile(<dataset>$<column name>),           # examine the 0th, 5th, ..., percentile
#                         probs = seq(from = 0.0, to = 0.1, by = 0.025))
# <new min value> <-                                          # the value to replace the outliers
#   as.numeric(quantile(<dataset>$<column name>,              # pick <percentile> closest to 
#                       probs = <percentile>))                # the lower whisker
# <dataset>$<column name>[<dataset>$<column name> < <new min value>] <- # replace the outliers
#   <new min value>
# boxplot(<dataset>$<column name>, xlab = "<column name>")    # check if outliers are still present

# Fix the outliers in the first variable manually
boxplot(the.beatles.songs.num$Duration, xlab = "Duration")
boxplot.stats(the.beatles.songs.num$Duration)
boxplot.stats(the.beatles.songs.num$Duration)$out
boxplot.stats(the.beatles.songs.num$Duration)$stats[c(1, 5)]
sort(boxplot.stats(the.beatles.songs.num$Duration)$out)
quantile(the.beatles.songs.num$Duration, 
         probs = seq(from = 0.9, to = 1, by = 0.025))
new.max.duration <- 
  as.numeric(quantile(the.beatles.songs.num$Duration,         # the 92.5th percentile seems to be 
                      probs = 0.925))                         # a good cut-off point
the.beatles.songs.num$Duration[the.beatles.songs.num$Duration > new.max.duration] <- new.max.duration
quantile(the.beatles.songs.num$Duration, 
         probs = seq(from = 0, to = 0.1, by = 0.025))
new.min.duration <- 
  as.numeric(quantile(the.beatles.songs.num$Duration,         # the 2.5th percentile seems to be 
                      probs = 0.025))                         # a good cut-off point
the.beatles.songs.num$Duration[the.beatles.songs.num$Duration < new.min.duration] <- new.min.duration
boxplot(the.beatles.songs.num$Duration, xlab = "Duration")
# no more outliers in Duration

# Call fixOutliers() for the other columns:
# source("Fix outliers.R")
# the.beatles.songs.num <- fixOutliers(the.beatles.songs.num, "<column name>")
source("Fix outliers.R")

boxplot(the.beatles.songs.num$Other.releases, xlab = "Other.releases")
boxplot.stats(the.beatles.songs.num$Other.releases)
boxplot.stats(the.beatles.songs.num$Other.releases)$out
boxplot.stats(the.beatles.songs.num$Other.releases)$stats[c(1, 5)]
the.beatles.songs.num <- fixOutliers(the.beatles.songs.num, "Other.releases")
boxplot(the.beatles.songs.num$Other.releases, xlab = "Other.releases")
# no more outliers in Other.releases

the.beatles.songs.num <- fixOutliers(the.beatles.songs.num, "Covered.by")
boxplot(the.beatles.songs.num$Covered.by, xlab = "Covered.by")
# no more outliers in Covered.by

# Demonstrate an attempt to fix outliers with highly skewed data:
boxplot(the.beatles.songs.num$Top.50.Billboard, xlab = "Top.50.Billboard")
boxplot.stats(the.beatles.songs.num$Top.50.Billboard)
boxplot.stats(the.beatles.songs.num$Top.50.Billboard)$out
boxplot.stats(the.beatles.songs.num$Top.50.Billboard)$stats[c(1, 5)]
temp <- the.beatles.songs.num$Top.50.Billboard # save current Top.50.Billboard, for restoring it later
the.beatles.songs.num <- fixOutliers(the.beatles.songs.num, "Top.50.Billboard")
boxplot(the.beatles.songs.num$Top.50.Billboard, xlab = "Top.50.Billboard")
the.beatles.songs.num$Top.50.Billboard <- temp # restore Top.50.Billboard

#Summarize the results so far:
summary(the.beatles.songs.num)

# See if there are some patterns in the data, pairwise, 
# to possibly indicate clusters:
# pairs(~ <column 1 name> + <column 2 name> + ..., 
#       data = <dataframe>)
pairs(~ Duration + Other.releases + Covered.by + Top.50.Billboard,  # no any striking pattern, i.e. 
      the.beatles.songs.num)                                        # no visual indication of clusters

# Try K-Means with 2 variables 

# Plot the data first:
# <scatterplot> <- 
#   ggplot(<dataset>, aes(x = <num.var.1>, y = <num.var.2>)) +
#     geom_point(shape = <n>,         # <n> = 1: hollow circle, no fill; 
#                                     # <n> = 21: circle that can be filled
#                fill = <color 1>,    # color of point fill (optional)
#                color = <color 2>,   # color of point line (optional)
#                size = <s>)          # size  of point line (optional)
# <scatterplot> <- <scatterplot> + xlab("<x label>")                    # label/caption on x-axis
# <scatterplot> <- <scatterplot> + ylab("<y label>")                    # label/caption on y-axis
# <scatterplot> <- <scatterplot> + ggtitle("<scatterplot title>")       # scatterplot title
# <scatterplot>                                                         # plot it
# Alternatively:
# <scatterplot> <- 
#   ggplot(<dataset>, aes(x = <num.var.1>, y = <num.var.2>)) +
#     geom_point(shape = <n>,         # <n> = 1: hollow circle, no fill; 
#                                     # <n> = 21: circle that can be filled
#                fill = <color 1>,    # color of point fill (optional)
#                color = <color 2>,   # color of point line (optional)
#                size = <s>)          # size  of point line (optional)
# <scatterplot> <- <scatterplot> + 
#   labs(x = "<x label>",                     # label/caption on x-axis
#        y = "<y label>",                     # label/caption on y-axis
#        title = "<scatterplot title>") +     # scatterplot title
# <scatterplot>                                                         # plot it
scatterplot.Other.releases.vs.Covered.by <- 
  ggplot(the.beatles.songs.num, aes(x = Other.releases, y = Covered.by)) +
  geom_point(shape = 21, fill = "yellow", size = 2) + 
  labs(x = "Other.releases", y = "Covered.by", title = "Covered.by vs. Other.releases") +
  theme_bw()
scatterplot.Other.releases.vs.Covered.by

# Subset the original data to include only the variables to be used in K-Means:
# <new dataframe> <- <dataframe>[, c("<col1 name>", "<col2 name>")]
# <new dataframe> <- <dataframe>[, <col1 index>:<col2 index>])
# Alternatively:
# <new dataframe> <- subset(<dataframe>, select = c("<col1 name>", "<col2 name>"))
# <new dataframe> <- subset(<dataframe>, select = c(<col1 index>:<col2 index>))
the.beatles.songs.num.1 <- the.beatles.songs.num[, c("Other.releases", "Covered.by")]
# the.beatles.songs.num.1 <- the.beatles.songs.num[, 2:3]
# the.beatles.songs.num.1 <- the.beatles.songs.num[, c(2, 3)]
# Alternatively:
# the.beatles.songs.num.1 <- subset(the.beatles.songs.num, select = c("Other.releases", "Covered.by"))
# the.beatles.songs.num.1 <- subset(the.beatles.songs.num, select = 2:3)
# the.beatles.songs.num.1 <- subset(the.beatles.songs.num, select = c(2, 3))
summary(the.beatles.songs.num.1)
head(the.beatles.songs.num.1)

# Data normalization: required by K-Means when the variables have different ranges 
# range(<dataframe with numeric columns>$<numeric column 1> # check the range of <numeric column 1>
# range(<dataframe with numeric columns>$<numeric column 2> # check the range of <numeric column 2>
# ...
# install.packages("clusterSim")
# library(clusterSim)
# <dataframe with numeric columns> <-                       # works with vectors and matrices as well
#   data.Normalization(<dataframe with numeric columns>,
#                      type = "n4",                         # normalization: (x - min(x)) / (max(x) - min(x))
#                      normalization = "column")            # normalization by columns
range(the.beatles.songs.num.1$Other.releases)
range(the.beatles.songs.num.1$Covered.by)
library(clusterSim)
the.beatles.songs.num.2 <- 
  data.Normalization(the.beatles.songs.num.1, 
                     type = "n4", 
                     normalization = "column")
tail(the.beatles.songs.num.2)

# Run K-Means for K = 3:
# set.seed(<seed>)
# <clusters> <- kmeans(x = <normalized dataframe>, 
#                      centers = 3,                         # K = 3
#                      iter.max = 20,                       # max number of iterations allowed
#                      nstart = 1000)                       # no. of initial configurations 
#                                                           # (report generated based on the best one)
# <clusters>
set.seed(888)
clusters.K3 <- kmeans(x = the.beatles.songs.num.2, centers = 3, iter.max = 20, nstart = 1000)
clusters.K3

# The meaning of parameters in the report:
# withinss (within_SS) - within cluster sum of squares, i.e., sum of squared differences between 
#   individual data points in a cluster and the cluster center; it is computed for each cluster
# totss (total_SS) - the sum of squared differences of each data point to the global sample mean
# betweenss (between_SS) - the sum of squared differences of each cluster center to 
#   the global sample mean; the squared difference of each cluster center to 
#   the global sample mean is multiplied by the number of data points in that cluster
# tot.withinss - the sum of squared differences between data points and cluster centers 
#   (the sum of within_SS for all the clusters)
# between_SS / total_SS - indicates how well the sample splits into clusters;
#   the higher the ratio, the better clustering

# Add the vector of clusters to the dataframe:
# <normalized dataframe>$<new column> <- factor(<clusters>$cluster)   # <clusters>: from the previous step
# head(<normalized dataframe>)
the.beatles.songs.num.2$Cluster <- factor(clusters.K3$cluster)
head(the.beatles.songs.num.2)

# Plot the clusters in a new scatterplot, using plotClusters() utility function:
# plotClusters() <- function(dataset,        # dataset with the cluster column
#                            xcol,           # dataset column for the x-axis, passed as a string
#                            ycol,           # dataset column for the y-axis, passed as a string
#                            clustercol,     # dataset column showing the clusters, passed as a string
#                            title,          # plot title
#                            x.label,        # x-axis label
#                            y.label,        # y-axis label
#                            legend.label)   # plot legend label
source("Plot clusters.R")
plotClusters(the.beatles.songs.num.2,
             "Other.releases",
             "Covered.by",
             "Cluster",
             "Clusters: (Other.releases, Covered.by), normalized",
             "Other.releases",
             "Covered.by",
             "Cluster",
             TRUE,
             clusters.K3)

# Find the optimal value for K, using the Elbow method (a call to the appropriate utility function); 
# an appropriate dataframe should be passed as the parameter (just numeric variables, no clusters):
# source("Elbow method.R")
# <elbow parameters> <- getElbowMethodParameters(<dataframe>[, c(<n1>, <n2>, ...)])   # leave out the cluster column
# <elbow parameters>
# plotElbow(<elbow parameters>)
source("Elbow method.R")
elbow.2 <- getElbowMethodParameters(the.beatles.songs.num.2[, c(1,2)])   # remove the Cluster column
elbow.2
plotElbow(elbow.2)

# Show differences in tot.withinss for different values of K more precisely, 
# using the getDifferences() utility function:
# source("Elbow method.R")
# <diff dataframe> <- 
#   data.frame(K = <n1>:<n2>, 
#              diff.tot.withinss = 
#                getDifferences(<elbow stats>$<tot.withinss>),                 # from the previous step
#              diff.ratio = 
#                 getDifferences(<elbow stats>$<ratio between_SS / total_SS>)) # from the previous step
# names(<diff dataframe>) <- c("K", "Difference in tot.withinss", "Difference in between_SS / total_SS")
# <diff dataframe>
df.differences <- data.frame(K = 2:8, 
                             getDifferences(elbow.2[, 2]), 
                             getDifferences(elbow.2[, 3]))
names(df.differences) <- c("K", "Difference in tot.withinss", "Difference in between_SS / total_SS")
df.differences

# Run K-Means also for K = 4:
set.seed(818)
the.beatles.songs.num.2 <- the.beatles.songs.num.2[, -3]    # remove the Cluster column for the new run
clusters.K4 <- kmeans(x = the.beatles.songs.num.2, centers = 4, iter.max = 20, nstart = 1000)
clusters.K4
the.beatles.songs.num.2$Cluster <- factor(clusters.K4$cluster)
head(the.beatles.songs.num.2)
source("Plot clusters.R")
plotClusters(the.beatles.songs.num.2,
             "Other.releases",
             "Covered.by",
             "Cluster",
             "Clusters: (Other.releases, Covered.by), normalized",
             "Other.releases",
             "Covered.by",
             "Cluster",
             TRUE,
             clusters.K4)

# Examine clusters more closely by looking into 
# the cluster centers (means) and standard deviations from the centers. 
# In doing so, use 'regular' (not normalized) features and the summarizeClusterStats() utility function:
# source("Summary statistics about clusters.R")
# <stats dataframe> <- 
#   summarizeClusterStats(feature.set = <dataframe with 'regular' (not normalized) features>, 
#                         clusters = <clusters>$cluster,   # <clusters>: result of kmeans()
#                         cl.num = <K>)                    # <K>: number of clusters
# Compare dispersion (sd) in this representation of the data and in the corresponding plots.
source("Summary statistics about clusters.R")
clusters.K3.stats <- summarizeClusterStats(feature.set = the.beatles.songs.num.1, 
                                           clusters = clusters.K3$cluster, cl.num = 3)
clusters.K4.stats <- summarizeClusterStats(feature.set = the.beatles.songs.num.1, 
                                           clusters = clusters.K4$cluster, cl.num = 4)

# Try K-Means with all variables 

# Data normalization 
library(clusterSim)
the.beatles.songs.num.4 <- 
  data.Normalization(the.beatles.songs.num, 
                     type = "n4", 
                     normalization = "column")
tail(the.beatles.songs.num.4)

# Find the optimal value for K, using the Elbow method (a call to the appropriate utility function):
source("Elbow method.R")
elbow.4 <- getElbowMethodParameters(the.beatles.songs.num.4)
elbow.4
plotElbow(elbow.4)

# Show differences in tot.withinss for different values of K more precisely, 
# using the getDifferences() utility function:
df.differences <- data.frame(K = 2:8, 
                             getDifferences(elbow.4[, 2]), 
                             getDifferences(elbow.4[, 3]))
names(df.differences) <- c("K", "Difference in tot.withinss", "Difference in between_SS / total_SS")
df.differences

# Run K-Means for K = 3, since K = 3 seems to be the best value for K:
set.seed(888)
clusters.K3.all.vars <- kmeans(x = the.beatles.songs.num.4, centers = 3, iter.max = 20, nstart = 1000)
clusters.K3.all.vars

# Run K-Means also for K = 4, since K = 4 seems to be the next best value for K:
set.seed(888)
clusters.K4.all.vars <- kmeans(x = the.beatles.songs.num.4, centers = 4, iter.max = 20, nstart = 1000)
clusters.K4.all.vars

# Examine and compare the cluster centers for K = 3 and K = 4:
clusters.K3.all.vars.stats <- summarizeClusterStats(feature.set = the.beatles.songs.num, 
                                                    clusters = clusters.K3.all.vars$cluster, cl.num = 3)
clusters.K3.all.vars.stats

clusters.K4.all.vars.stats <- summarizeClusterStats(feature.set = the.beatles.songs.num, 
                                                    clusters = clusters.K4.all.vars$cluster, cl.num = 4)
clusters.K4.all.vars.stats

# Compare multiple clustering results/schemes:
# install.packages("fpc")
# library(fpc)
# ?cluster.stats
# <comparison criteria> <-              # specify criteria (from cluster.stats()) for comparing 
#   c("<criterion 1>",                  # different clusterings (e.g., "max.diameter", "min.separation", 
#     "<criterion 2>", ...)             # "average.between", "average.within", "within.cluster.ss", ...)
# <distance matrix> <- 
#   dist(x = <normalized dataset>)
# <comparison> <- sapply(list(<clustering 1 name> = <clustering 1>$cluster, # <clustering 1> computed by kmeans()
#                             <clustering 2 name> = <clustering 2>$cluster, # <clustering 2> computed by kmeans()
#                             ...),
#                        FUN = function(x)
#                          cluster.stats(<distance matrix>, x))[<comparison criteria>, ]
# Alternative 1:
# install.packages("knitr")
# library(knitr)
# kable(x = <comparison>, format = "rst")         # show comparison as a table in the console
# Alternative 2:
# source("Summary statistics about clusters.R")
# <comparison.df> <- 
#   compareMultipleClusterings(<comparison>)      # show comparison as a dataframe, using a corresponding utility function
# <comparison.df>
library(fpc)
comparison.criteria <- c("max.diameter", "min.separation", "average.between", 
                         "average.within", "within.cluster.ss")  
d <- dist(x = the.beatles.songs.num.4)
comparison <- sapply(list(c.K3.var2 = clusters.K3$cluster, # clustering: 3 clusters, 2 variables
                          c.K4.var2 = clusters.K4$cluster, # clustering: 4 clusters, 2 variables
                          c.K3.var4 = clusters.K3.all.vars$cluster,  # 3 clusters, 4 variables
                          c.K4.var4 = clusters.K4.all.vars$cluster), # 4 clusters, 4 variables
                     FUN = function(x) cluster.stats(d, x))[comparison.criteria, ]
library(knitr)
kable(x = comparison, format = "rst")

comparison.df <- compareMultipleClusterings(comparison)
comparison.df
