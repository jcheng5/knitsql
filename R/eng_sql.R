#' DBI/SQL knitr engine for R
#'
#' Call \code{library(knitsql)} to automatically register a \code{sql} knitr
#' engine. This allows you to create R Markdown code chunks that contain SQL
#' queries.
#'
#' TODO: Improve these docs
#'
#' Specify the connection either in R Markdown metadata, or by creating a DBI
#' connection object in a prior R code chunk and pass it as the \code{conn}
#' object.
#'
#' Use the knitr chunk name to assign the results to a variable.
#'
#' You can bind R variables to parameters in the SQL query by using
#' \code{?varname}.
#'
#' @docType package
#' @name knitsql
NULL

# Create a database connection from a metadata list.
# The list must contain a named argument "driver",
# and either other named arguments, or a single
# named argument "args" with named or unnamed
# arguments. (This lets you either specify the
# additional dbConnect arguments using named or
# positional argument passing.)
dbconn_from_meta <- function(lst) {
  drv <- lst$driver
  connectArgs <- if (!is.null(lst$args)) {
    lst$args
  } else {
    lst$driver <- NULL
    lst
  }
  do.call(DBI::dbConnect, c(list(drv), connectArgs))
}

# Return char vector of sql interpolation param names
varnames_from_sql <- function(conn, sql) {
  varPos <- DBI::sqlParseVariables(conn, sql)
  if (length(varPos$start) > 0) {
    varNames <- substring(sql, varPos$start, varPos$end)
    sub("^\\?", "", varNames)
  }
}

# Vectorized version of exists
mexists <- function(x, env = knitr::knit_global(), inherits = TRUE) {
  vapply(x, exists, logical(1), where = env, inherits = inherits)
}

# Interpolate a sql query based on the variables in an environment
interpolate_from_env <- function(conn, sql, env = knitr::knit_global(), inherits = TRUE) {
  names <- unique(varnames_from_sql(conn, sql))
  names_missing <- names[!mexists(names, env, inherits)]
  if (length(names_missing) > 0) {
    stop("Object(s) not found: ",
      paste('"', names_missing, '"', collapse = ", "))
  }

  args <- if (length(names) > 0) {
    setNames(
      mget(names, inherits = inherits),
      names
    )
  }

  do.call(DBI::sqlInterpolate, c(list(conn, sql), args))
}


eng_sql <- function(options) {
  conn <- options$conn

  if (!is.null(conn)) {
    dbname <- options$dbname
    if (is.null(dbname)) {
      dbname <- "default"
    }
    if (is.null(rmarkdown::metadata$database[[dbname]])) {
      stop(dbname, " database definition not found")
    }
    conn <- dbconn_from_meta(rmarkdown::metadata$database$default)
  }

  varname <- options$label
  sql <- options$code

  on.exit(DBI::dbDisconnect(conn))

  query <- interpolate_from_env(conn, sql)
  result <- DBI::dbGetQuery(conn, query)
  output <- capture.output(print(result))

  if (!is.null(varname)) {
    assign(varname, result, envir = knitr::knit_global())
  }

  knitr::engine_output(options, options$code, output)
}
