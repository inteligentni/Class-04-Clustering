# Boxplot a numeric feature from a dataframe
                                                      # Generally:
# <output var> <- ggplot(<dataset>,                                 # ggplot2 boxplots
#                        aes(x = "", y = <column name>)) +          # show boxplot of <column name>
#   geom_boxplot(width = 0.5, fill = "<color>") +                   # boxplot width and color
#   stat_boxplot(geom ='errorbar', width = 0.15) +                  # show whiskers, control their width
#   guides(fill = FALSE) +                                          # no legend (it makes no sense here)
#   xlab("") +                                                      # no x-axis label (it makes no sense here)
#   ylab("<column name>")                                           # y-axis label
# <output var>                                                      # plot it

boxplotFeature <- function(df, feature, boxplot.fill.color) {       # feature must be passed as a string (feature's name)!
  gg.boxplot.feature <- 
    ggplot(df, aes(x = "", y = df[[feature]])) + 
    geom_boxplot(width = 0.5, fill = boxplot.fill.color) + 
    stat_boxplot(geom ='errorbar', width = 0.15) + 
    guides(fill = FALSE) + 
    xlab("") + 
    ylab(feature)
  gg.boxplot.feature
}
