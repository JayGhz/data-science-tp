load_data <- function(file_path = "data/hotel_bookings.csv") {
  read.csv(file_path, header = TRUE, na.strings = c("NA", "NULL", "Undefined"))
}