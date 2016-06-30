.onLoad <- function(libname, pkgname) {
  knitr::knit_engines$set(sql = eng_sql)
}
