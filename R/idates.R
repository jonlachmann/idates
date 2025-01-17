#' IDate class, a date represented by three integers, Year, Month and Day.
#' @export
setClass("IDate", slots = list(
  Year = "integer",
  Month = "integer",
  Day = "integer"
))

#' Convert a list of string on the format YYYY-MM-DD to a list of IDate
#' @param x The list of strings
#' @return A list of IDate
#' @export
as.IDates <- function (x) {
  parts <- strsplit(x, "-", fixed = TRUE)
  parts <- lapply(parts, as.integer)

  months <- sapply(parts, function (p) p[2])
  days <- sapply(parts, function (p) p[3])

  if (any(months > 12 | months < 1)) {
    stop("Invalid date provided.")
  }

  if (any(days > 31 | days < 1)) {
    stop("Invalid date provided.")
  }

  dates <- lapply(parts, function (p) new("IDate", Year = p[1], Month = p[2], Day = p[3]))

  return(dates)
}

#' Show a IDate, used for printing etc.
#' @export
setMethod("show", "IDate", function (object) {
  print(toString(object))
})

#' S3 Method to convert an IDate to string for printing or further parsing, calls the S4 method
#' @method toString IDate
#' @export
toString.IDate <- function(x, ...) {
  toString(as(x, "IDate"))
}

#' S4 Method to convert an IDate to string for printing or further parsing
#' @export
setMethod("toString", "IDate", function (x, ...) {
  paste(sprintf("%04d", x@Year), sprintf("%02d", x@Month), sprintf("%02d", x@Day), sep = "-")
})

#' Add a number of periods in a certain frequency to an IDate
#' @param x The IDate
#' @param n The number of periods to add
#' @param frequency Integer representing the frequency of the n periods to add
#' @export
addPeriods <- function (x, n, frequency) {
  if (frequency == 1) {
    x@Year <- as.integer(x@Year + n)
  }
  if (frequency %in% c(2, 4, 12)) {
    months <- as.integer(x@Month - 1 + n * (12 / frequency))
    x@Year <- as.integer(x@Year + floor(months / 12))
    x@Month <- as.integer(months %% 12 + 1)
  }
  if (frequency %in% c(52, 365)) {
    x_posix <- as.POSIXlt(x)
    x_posix$mday <- x_posix$mday + as.integer(n * round(365 / frequency))
    x <- as.IDate.POSIXlt(x_posix)
  }
  return(x)
}

#' Calculate the difference in months between two IDate
#' @export
monthDiff <- function (x, y) {
  return((x@Year - y@Year) * 12 + x@Month - y@Month)
}

#' Set an IDate to the start of period
#' @param x The IDate
#' @param frequency The frequency to set the IDate to the start of period of
#' @export
startOfPeriod <- function (x, frequency) {
  if (frequency == 1) {
    return(new("IDate", Year = x@Year, Month = 1, Day = 1))
  }
  if (frequency == 2) {

  }
}

#' Convert an IDate to POSIXlt
#' @export
as.POSIXlt.IDate <- function(x, ...) {
  date_string <- toString.IDate(x)
  as.POSIXlt(date_string)
}

#' Convert POSIXlt to IDate
#' @export
as.IDate.POSIXlt <- function (x) {
  date_string <- format(x, "%Y-%m-%d")
  as.IDates(date_string)[[1]]
}

#' Shift an IDate to a certain day of week
#' @export
toDoW.IDate <- function (x, dow) {
  c_dow <- day_of_week(x@Year, x@Month, x@Day)
  return(addPeriods(x, dow - c_dow + 1, 365))
}

#' Comparison operator
#' @export
setMethod(">=", signature(e1 = "IDate", e2 = "IDate"), function(e1, e2) {
  return(
    (e1@Year > e2@Year) ||
    (e1@Year == e2@Year && e1@Month > e2@Month) ||
    (e1@Year == e2@Year && e1@Month == e2@Month && e1@Day >= e2@Day))
})
