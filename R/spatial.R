#' Plot ERRDAP time series data
#'
#' The purpose of this function is to generate time series plots of ERRDAP data.
#' Currently, this function can plot sea surface temperature and chlorophyll data.
#'
#' @param csv The csv file containing the ERRDAP data to be plotted.
#' @param metric The metric to be plotted.
#' @param ... additional parameters to pass to \link[dygraphs]{dygraph}
#' @return This function outputs a dygraph object of the time series plot.
#' @export
#' @examples \dontrun{
#' csv_SST <-here::here("data/oceano/statistics_sst_cinms.csv")
#' plot_metric_timeseries(csv_SST, "sst")
#' }
#'
plot_metric_timeseries <- function(csv, metric, ...){

  # Read in the csv file
  data_history <- read.csv(csv, header = TRUE)
  dates<- data_history[,1]
  average_value <- data_history[,2]
  lower_value <- data_history[,5]
  upper_value <- data_history[,6]

  # create a data frame which lines up the data in the way that dygraph needs it
  history <- data.frame(date = as.Date(dates, "%Y-%m-%d"), avg_value = average_value, lower = lower_value, upper = upper_value)
  history <- xts::xts(x = history[,-1], order.by = history$date)

  # create the figure
  if (metric == "sst"){ # plotting sea surface temperature
    dygraphs::dygraph(
      history,
      main = "Sea Surface Temperature",
      xlab = "Date", ylab = "Temperature (°C)",
      ...)%>%
      dygraphs::dySeries(c("lower", "avg_value", "upper"), label = "Temperature (°C)", color = "Red")%>%
      dygraphs::dyRangeSelector(fillColor = " #FFFFFF", strokeColor = "#FFFFFF")
  } else if (metric == "chl") { # plotting chlorophyll
    dygraphs::dygraph(
      history,
      main = "Chlorophyll Concentration",
      xlab = "Date", ylab = "Chlorophyll Concentration, OC3 Algorithm (mg/m<sup>3</sup>)",
      ...)%>%
      dygraphs::dySeries(c("lower", "avg_value", "upper"), label = "Chlorophyll concentration", color = "Green")%>%
      dygraphs::dyRangeSelector(fillColor = " #FFFFFF", strokeColor = "#FFFFFF")
  } else { # if any other metric is called, stop everything
    stop("Error in metric: the function plot_metric_timeseries only currently knows how to handle the metrics sst and chl")
  }
}

#' Plot SST anomaly data
#'
#' The purpose of this function is to plot the sea surface temperature anomaly
#' data.
#'
#' @param csv_SST The csv file containing the SST anomaly data to be plotted.
#' @return This function outputs a dygraph object of the time series plot.
#' @export
#' @examples \dontrun{
#' csv_SST <-here::here("data/oceano/sst_anomaly_cinms.csv")
#' plot_metric_timeseries(csv_SST)
#' }
#'
plot_SST_anomaly <- function(csv_SST){

  # We want to plot the SST anomaly data with values below zero colored differently
  # than those above zero. The strategy here is to split the SST anomaly data (which
  # is 1 column of data) into 2 columns of data: values below zero and values above
  # zero. Each of those lines will be plotted separately as a different filled-in
  # color. We also need to interpolate some points. Every time the anomaly time
  # series data crosses the zero anomaly line (0 on the y axis), that "zero day"
  # needs to be inserted into the 2 columns of data. This is necessary to have a
  # figure that doesn't have gaps in the color fill.

  # load in the SST anomaly data
  SST_data <- read.csv(csv_SST, header = T, colClasses = c("Date", "numeric"))

  # create an output data frame and initialize the row of the data frame to write
  # on to 1
  output_data <- data.frame(date= SST_data$date, below=NA,above=NA )
  write_line <-1

  # Let's go through every row of the anomaly data
  for (i in 1:nrow(SST_data)){

    # first let's write the row of data to the output data frame, putting below
    # zero and above zero values in different columns
    if (SST_data$sst_anomaly[i] <= 0){
      output_data[write_line,]<- c(as.character(SST_data$date[i]),
                                   as.numeric(SST_data$sst_anomaly[i]), NA)
    }
    else {
      output_data[write_line,]<- c(as.character(SST_data$date[i]), NA,
                                   as.numeric(SST_data$sst_anomaly[i]))
    }
    write_line <- write_line +1

    # Next, let's see if the anomaly value AFTER the current one changes sign.
    # If it does, we need to calculate the date at which the anomaly was zero and
    # insert that "zero day" into the output file after the current anomaly value.
    # We want to skip this whole procedure if we are at the last record of the file.
    if ((i < nrow(SST_data)) && (sign(SST_data$sst_anomaly[i]) !=
                                 sign(SST_data$sst_anomaly[i+1])) ){
      first_SST <- SST_data$sst_anomaly[i]
      second_SST <- SST_data$sst_anomaly[i+1]
      first_date <- SST_data$date[i]
      second_date <- SST_data$date[i+1]
      day_num <- as.numeric(second_date - first_date)
      daily_increment <- (second_SST - first_SST)/day_num
      days_more <- round(abs(first_SST/daily_increment),0)
      output_data[write_line, ] <- c(as.character(first_date + days_more), 0, 0)
      write_line <- write_line +1
    }
  }

  # create the data frame to be read by dygraphs and then produce the dygraph figure
  history <- data.frame(date = as.Date(output_data$date, "%Y-%m-%d"),
                        below = as.numeric(output_data$below),
                        above = as.numeric(output_data$above))
  history <- xts::xts(x = history[,-1], order.by = history$date)

  dygraphs::dygraph(history, main = "Sea Surface Temperature Anomaly", xlab = "Date", ylab = "Sea Surface Temperature Anomaly (°C)") %>%
    dygraphs::dyOptions(fillGraph = TRUE, fillAlpha = 0.4) %>%
    dygraphs::dySeries("below", label = "°C Below Average", color = "Blue") %>%
    dygraphs::dySeries("above", label = "°C Above Average", color = "Red") %>%
    dygraphs::dyLegend(show = "auto", showZeroValues = FALSE) %>%
    dygraphs::dyLimit(0, strokePattern = "solid", color = "black") %>%
    dygraphs::dyRangeSelector(fillColor = " #FFFFFF", strokeColor = "#FFFFFF")
}



