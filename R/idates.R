#' @export
setClass("IDate", slots = list(
  Year = "integer",
  Month = "integer",
  Day = "integer"
))

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

#' @export
setMethod("show", "IDate", function (object) {
  print(toString(object))
})

#' @method toString IDate
#' @export
toString.IDate <- function(x, ...) {
  toString(as(x, "IDate"))
}

#' @export
setMethod("toString", "IDate", function (x, ...) {
  paste(sprintf("%04d", x@Year), sprintf("%02d", x@Month), sprintf("%02d", x@Day), sep = "-")
})

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

#' @export
monthDiff <- function (x, y) {
  return((x@Year - y@Year) * 12 + x@Month - y@Month)
}

#' @export
startOfPeriod <- function (x, frequency) {
  if (frequency == 1) {
    return(new("IDate", Year = x@Year, Month = 1, Day = 1))
  }
  if (frequency == 2) {

  }
}

#' @export
as.POSIXlt.IDate <- function(x, ...) {
  date_string <- toString.IDate(x)
  as.POSIXlt(date_string)
}

#' @export
as.IDate.POSIXlt <- function (x) {
  date_string <- format(x, "%Y-%m-%d")
  as.IDates(date_string)[[1]]
}

#' @export
toDoW.IDate <- function (x, dow) {
  c_dow <- day_of_week(x@Year, x@Month, x@Day)
  return(addPeriods(x, dow - c_dow + 1, 365))
}

#' @export
setMethod(">=", signature(e1 = "IDate", e2 = "IDate"), function(e1, e2) {
  return(
    (e1@Year > e2@Year) ||
    (e1@Year == e2@Year && e1@Month > e2@Month) ||
    (e1@Year == e2@Year && e1@Month == e2@Month && e1@Day >= e2@Day))
})
