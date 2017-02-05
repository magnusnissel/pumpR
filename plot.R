require(readr)
require(dplyr)
require(tidyr)
require(purrr)
require(lubridate)
require(ggplot2)
require(ggthemes)
require(scales)
source("config.R")

# set up plotting (colorblind friendly colors, number formatting)
options(scipen=999)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
scale_fill_manual(values=cbPalette)

num_formatter <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)

theme_set(theme_tufte(base_family="DejaVu Sans"))



heat_plotter <- function(df, xcol, ycol, fcol, fill_name, title) {
p <- ggplot(df, aes_string(x=xcol, y=ycol)) 
p <- p + theme_tufte(ticks=FALSE, base_size=14)
p <- p + geom_tile(stat="identity", aes_string(fill=fcol)) 
p <- p + scale_fill_gradient(low="white", high="steelblue", label=num_formatter, name=fill_name)
p <- p + geom_text(aes_string(fill = fcol, label = num_formatter(fcol)))
p <- p + ggtitle(title) + xlab("") + ylab("") + scale_y_reverse()
}


