library(ggplot2)
library(readr)

PlotEmploymentTrends <- function (employment.trends) {
  # Plots the employment trends as per Q1.1
  qplot(x=time, y=employment, data=employment.trends, geom="line",
        xlab="Quarterly Figures", ylab="Trend",
        main="Dublin Employment Trends Per Sector: 2006-2016") +
    facet_grid(. ~ sector) +
    theme(axis.title.y = element_text(angle=0, vjust = 0.5))
}

PlotPropertyTrends <- function (property.trends) {
  # Plots the property trends as per Q1.2
  qplot(x=time, y=trend, data=property.trends, geom="line",
        colour=category, xlab="Time", ylab="Trend",
        main="Dublin Property Trends: 2007 - 2016") +
    labs(colour = 'Category') +
    theme(axis.title.y = element_text(angle=0, vjust = 0.5))
}

employment.trends <- read_delim("dublin employment trends.txt", delim=':')
colnames(employment.trends) <- tolower(colnames(employment.trends))

property.trends <- read_tsv("dublin property trends.txt")
colnames(property.trends) <- tolower(colnames(property.trends))
