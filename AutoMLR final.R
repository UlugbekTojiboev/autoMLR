install.packages(c("usethis", "devtools", "roxygen2", "testthat", "R6", "Rcpp"))
usethis::create_package("path/to/your/projects/autoMLR")
usethis::use_roxygen_md() # Setup markdown-style documentation
usethis::use_testthat()   # Create the tests/ directory
usethis::use_rcpp()
