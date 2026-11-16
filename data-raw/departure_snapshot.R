# Load our package functions
devtools::load_all()

# Five boarding stations
station_codes <- c("BKY", "EMY", "RIC", "MTZ", "DAV")

results <- vector("list", length(station_codes))
query_log <- vector("list", length(station_codes))

# Collect departure information at each station
for (i in seq_along(station_codes)) {

  queried_at <- Sys.time()

  result <- find_trains(
    station = station_codes[i],
    include_departed = FALSE
  )

  result$queried_at <- rep(queried_at, nrow(result))
  results[[i]] <- result

  query_log[[i]] <- data.frame(
    station_code = station_codes[i],
    queried_at = queried_at,
    rows_returned = nrow(result),
    api_requests = 2L
  )

  Sys.sleep(2)
}

# Combine the five results into one data frame
departure_snapshot <- do.call(rbind, results)
rownames(departure_snapshot) <- NULL

query_log <- do.call(rbind, query_log)

# Save the collection log
write.csv(
  query_log,
  "data-raw/departure_query_log.csv",
  row.names = FALSE
)

# Display the collection results
print(query_log)
print(dim(departure_snapshot))
print(head(departure_snapshot))

# Do not save an empty dataset
if (nrow(departure_snapshot) == 0L) {
  stop("No matching trains were returned. Try collecting again when trains are running.")
}

# Save the completed dataset inside the package
usethis::use_data(departure_snapshot, overwrite = TRUE)