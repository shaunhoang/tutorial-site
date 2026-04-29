# Helper for building a simple adjacency matrix from polygon boundaries.

library(sf)
library(spdep)

build_adjacency <- function(boundary_file, domain_id, queen = TRUE) {
  domains_sf <- st_read(boundary_file, quiet = TRUE)

  if (!domain_id %in% names(domains_sf)) {
    stop("domain_id column was not found in the boundary file.")
  }

  neighbors <- poly2nb(domains_sf, queen = queen)
  adjacency <- nb2mat(neighbors, style = "B", zero.policy = TRUE)

  rownames(adjacency) <- domains_sf[[domain_id]]
  colnames(adjacency) <- domains_sf[[domain_id]]

  list(
    domains = domains_sf,
    neighbors = neighbors,
    adjacency = adjacency
  )
}
