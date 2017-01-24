#' datetimeValorisation2
#'
#' Function to enrich a datetime object
#' @param input a data frame or a vector : data frame must be in this format : id, datetimesee. See simplifiedVector for info on the format of the vector option
#' @param variableName single length vector to indicate the prefix of the newly create variable in this format : paste0(variableName, "dependingOnWorkOrder")
#' @param workOrder vector to include all transformation to do on the datetime object : c("year", "month", "day", "week", "wday", "wdayLabel", "ymd", "ym", "yw")
#' @param simplifiedVector if simplifiedVector is TRUE, then the data frame is in a vector format containing the datetimes
#' @export
#' @examples
#' x <- data.frame(x1 = 1:3, x2 = as.Date(c("2013-06-20", "2013-08-15", "2015-01-02")))
#' y <- datetimeValorisation2(x, "TypeA", c("year", "month", "day", "week", "wday", "wdayLabel", "ymd", "ym", "yw"), simplifiedVector = FALSE)

datetimeValorisation2 <- function(input, variableName, workOrder, simplifiedVector = FALSE){
  # data frame must be in this format : id, datetime
  # if simplifiedVector is TRUE, then the data frame is in a vector format containing the datetimes
  # data frame variables names are not constrained
  # exemple
  # x <- data.frame(x1 = 1:3, x2 = as.Date(c("2013-06-20", "2013-08-15", "2015-01-02")))
  # y <- datetimeValorisation2(x, "TypeA", c("year", "month", "day", "week", "wday", "ymd", "ym", "yw"), simplifiedVector = FALSE)


  # rename or reshape data frame or vector for standardization
  if(simplifiedVector == FALSE){
    names(input) <- c("id", "datetime")
  }else{
      input <- data.frame(datetime = input)
  }

  # Apply those functions if it is inside the workOrder

    # year
    if("year" %in% workOrder){
      input[[paste0(variableName, "Year")]] <- year(input$datetime)
    }

    # month
    if("month" %in% workOrder){
      input[[paste0(variableName, "Month")]] <- month(input$datetime)
    }

    # day
    if("day" %in% workOrder){
      input[[paste0(variableName, "Day")]] <- day(input$datetime)
    }

    # week
    if("week" %in% workOrder){
      input[[paste0(variableName, "Week")]] <- week(input$datetime)
    }

    # wday
    if("wday" %in% workOrder){
      input[[paste0(variableName, "Wday")]] <- wday(input$datetime)
    }
  
    # wdayLabel
    if("wdayLabel" %in% workOrder){
      input[[paste0(variableName, "WdayLabel")]] <- wday(input$datetime, label = TRUE)
    }

    # YearMonthDay
    if("ymd" %in% workOrder){
      input[[paste0(variableName, "YMD")]] <- floor_date(input$datetime, unit="day")
    }

    # YearMonth
    if("ym" %in% workOrder){
      input[[paste0(variableName, "YM")]] <- floor_date(input$datetime, unit="month")
    }

    # YearWeek
    if("yw" %in% workOrder){
      input[[paste0(variableName, "YW")]] <- floor_date(input$datetime, unit="week")
    }

  if(simplifiedVector == FALSE){
    input$datetime <- NULL
  }
  
  return(input)

} # end function
