# Tutorial data setup helpers

# Replace this URL after hosting downloads/tutorial-raw-data.zip.
default_tutorial_raw_data_url <- "https://github.com/shaunhoang/tutorial-site/releases/download/tutorial-data-v1/tutorial-raw-data.zip"

tutorial_project_root <- function(path = getwd()) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  repeat {
    if (file.exists(file.path(path, "_quarto.yml"))) {
      return(path)
    }

    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Could not find _quarto.yml. Run this from inside the tutorial project.", call. = FALSE)
    }
    path <- parent
  }
}

copy_if_found <- function(from, to, overwrite = FALSE) {
  if (!file.exists(from)) {
    return(FALSE)
  }
  if (file.exists(to) && !overwrite) {
    return(TRUE)
  }
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  file.copy(from, to, overwrite = TRUE)
}

standardise_raw_data_layout <- function(raw_dir, files = NULL, overwrite = FALSE) {
  if (is.null(files)) {
    files <- c(
      "nsw_2223_msoa_la.csv",
      "census2021-ts007-msoa.csv",
      "census2021-ts007-ltla.csv",
      "boundaries-msoa.geojson",
      "boundaries-ltla.geojson"
    )
  }

  known_nested_paths <- c(
    file.path("survey", "nsw_2223_msoa_la.csv"),
    file.path("census", "census2021-ts007-msoa.csv"),
    file.path("census", "census2021-ts007-ltla.csv"),
    file.path("boundaries", "boundaries-msoa.geojson"),
    file.path("boundaries", "boundaries-ltla.geojson")
  )

  for (file in files) {
    target <- file.path(raw_dir, file)
    nested_matches <- known_nested_paths[basename(known_nested_paths) == file]
    nested_path <- if (length(nested_matches) > 0) nested_matches[[1]] else file
    raw_source <- file.path(raw_dir, "raw", file)
    data_raw_source <- file.path(raw_dir, "data", "raw", file)
    nested_source <- file.path(raw_dir, nested_path)
    nested_raw_source <- file.path(raw_dir, "raw", nested_path)
    nested_data_raw_source <- file.path(raw_dir, "data", "raw", nested_path)

    copy_if_found(raw_source, target, overwrite = overwrite)
    copy_if_found(nested_source, target, overwrite = overwrite)
    copy_if_found(data_raw_source, target, overwrite = overwrite)
    copy_if_found(nested_raw_source, target, overwrite = overwrite)
    copy_if_found(nested_data_raw_source, target, overwrite = overwrite)

    if (!file.exists(target)) {
      possible_sources <- list.files(
        raw_dir,
        pattern = paste0("^", file, "$"),
        recursive = TRUE,
        full.names = TRUE
      )
      if (length(possible_sources) > 0) {
        copy_if_found(possible_sources[[1]], target, overwrite = overwrite)
      }
    }
  }
}

get_tutorial_data <- function(
    url = default_tutorial_raw_data_url,
    project_root = tutorial_project_root(),
    raw_dir = file.path(project_root, "data", "raw"),
    required_files = c(
      "nsw_2223_msoa_la.csv",
      "census2021-ts007-msoa.csv",
      "census2021-ts007-ltla.csv",
      "boundaries-msoa.geojson"
    ),
    overwrite = FALSE) {
  missing_files <- required_files[!file.exists(file.path(raw_dir, required_files))]
  if (length(missing_files) == 0 && !overwrite) {
    message("Raw tutorial data already exists in ", raw_dir)
    return(invisible(file.path(raw_dir, required_files)))
  }

  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  temp_zip <- tempfile(fileext = ".zip")
  message("Downloading tutorial raw data from: ", url)
  utils::download.file(url, temp_zip, mode = "wb", quiet = FALSE)

  utils::unzip(temp_zip, exdir = raw_dir)
  standardise_raw_data_layout(raw_dir, files = required_files, overwrite = overwrite)

  missing_files <- required_files[!file.exists(file.path(raw_dir, required_files))]
  if (length(missing_files) > 0) {
    stop(
      "The raw data download completed, but these files were not found: ",
      paste(missing_files, collapse = ", "),
      call. = FALSE
    )
  }

  message("Raw tutorial data is ready in ", raw_dir)
  invisible(file.path(raw_dir, required_files))
}

get_sae_ready_inputs <- function(
    project_root = tutorial_project_root(),
    source_dir = file.path(project_root, "downloads", "sae-ready-inputs"),
    output_dir = file.path(project_root, "data", "clean"),
    overwrite = FALSE) {
  required_files <- c(
    "domain_sizes_adult_msoa.csv",
    "domain_sizes_population_la.csv",
    "ind_weight_commute_distance.csv",
    "ind_weight_poor_bus_satisfaction.csv",
    "ind_weight_walk_weekly.csv"
  )

  missing_source_files <- required_files[!file.exists(file.path(source_dir, required_files))]
  if (length(missing_source_files) > 0) {
    stop(
      "The SAE-ready input folder is missing: ",
      paste(missing_source_files, collapse = ", "),
      call. = FALSE
    )
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  for (file in required_files) {
    copy_if_found(
      file.path(source_dir, file),
      file.path(output_dir, file),
      overwrite = overwrite
    )
  }

  message("SAE-ready inputs are available in ", output_dir)
  invisible(file.path(output_dir, required_files))
}
