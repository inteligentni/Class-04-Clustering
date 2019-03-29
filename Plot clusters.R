# Plotting clusters in a scatterplot
#                                                 # Generally:
# <scatterplot> <- 
#   ggplot(<dataset with the cluster column>, 
#          aes(x = <num.var.1>, y = <num.var.2>,  
#              color = <cluster column>)) +       # color clusters differently
# <scatterplot> <- <scatterplot> + geom_point()   # fill colors can be added subsequently, see below

# <scatterplot> <- <scatterplot> + xlab("<x label>")                  # label/caption on x-axis
# <scatterplot> <- <scatterplot> + ylab("<y label>")                  # label/caption on y-axis
# <scatterplot> <- <scatterplot> + ggtitle("<scatterplot title>")     # scatterplot title
# <scatterplot> <- <scatterplot> + 
#   scale_fill_brewer(palette = "Set1",           # palettes: http://ggplot2.tidyverse.org/reference/scale_brewer.html
#                     name = "<cluster column>")  # legend title

# Alternative to lines 9-14:
# <scatterplot> <- <scatterplot> + 
#   labs(title = title,                   # scatterplot title
#        x = x.label,                     # label/caption on x-axis
#        y = y.label,                     # label/caption on y-axis
#        color = legend.label)            # # legend title
# scatterplot <- scatterplot +
#   scale_fill_brewer(palette = "Set1")   # palettes: http://ggplot2.tidyverse.org/reference/scale_brewer.html

# <scatterplot> <- <scatterplot> + theme_bw()     # white background
# <scatterplot> <- <scatterplot> +                # add cluster centers
#   geom_point(as.data.frame(<clusters>$centers), # <clusters> computed by kmeans() in a previous step
#              color = "<line color>",
#              fill = "<fill color>",
#              size = <size>,                     # frequently used <size>s: 3, 4
#              shape = <shape>)                   # diamond: 23; triangle: 24; circle: 21; ...

plotClusters <- function(dataset,           # dataset with the cluster column
                         xcol,              # dataset column for the x-axis, passed as a string
                         ycol,              # dataset column for the y-axis, passed as a string
                         clustercol,        # dataset column showing the clusters, passed as a string
                         title,             # plot title
                         x.label,           # x-axis label
                         y.label,           # y-axis label
                         legend.label,      # plot legend label
                         show.centers.flag, # plot cluster centers if TRUE
                         clusters) {        # clusters computed by kmeans() in a previous step
  library(ggplot2)
  scatterplot <- ggplot(dataset,
                        aes(x = dataset[[xcol]], y = dataset[[ycol]], color = dataset[[clustercol]]))
  scatterplot <- scatterplot + geom_point(size = 2)
  scatterplot <- scatterplot +
    labs(title = title,
         x = x.label,
         y = y.label,
         color = legend.label)
  scatterplot <- scatterplot +
    scale_fill_brewer(palette = "Set1")
  scatterplot <- scatterplot + theme_bw()
  if (show.centers.flag) {                                  # add cluster centers
    clusters.centers <- as.data.frame(clusters$centers)
    scatterplot + geom_point(data = clusters.centers, 
                             aes(x = clusters.centers[, 1], y = clusters.centers[, 2]), 
                             color = "black",
                             fill = "black",
                             size = 4,
                             shape = 24)
  } else {
    scatterplot                                             # don't add cluster centers
  }
}

