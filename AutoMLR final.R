install.packages(c("usethis", "devtools", "roxygen2", "testthat", "R6", "Rcpp"))
usethis::create_package("path/to/your/projects/autoMLR")
usethis::use_roxygen_md() # Setup markdown-style documentation
usethis::use_testthat()   # Create the tests/ directory
usethis::use_rcpp()

library(R6)

AutoCleaner <- R6Class("AutoCleaner",
                       public = list(
                         data = NULL,initialize = function(my_data) {
                           if (!is.data.frame(my_data)) {
                             stop("Hey, you need to give me a data frame!")
                           }
                           self$data <- my_data
                           message("Data loaded successfully.")
                         },

                         simple_impute = function() {
                           message("Cleaning your data...")

                           self$data[] <- lapply(self$data, function(x) {
                             if (is.numeric(x)) {
                               x[is.na(x)] <- mean(x, na.rm = TRUE)
                             }
                             return(x)
                           })

                           message("Done! All numbers are filled.")
                         }
                       )
)


test_df <- data.frame(age = c(20, 30, NA, 50), score = c(10, NA, 30, 40))

my_pipe <- AutoCleaner$new(test_df)


my_pipe$simple_impute()


print(my_pipe$data)
