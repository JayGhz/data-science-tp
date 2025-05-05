load_data <- function(file_path = "data/hotel_bookings.csv") {
  read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
}