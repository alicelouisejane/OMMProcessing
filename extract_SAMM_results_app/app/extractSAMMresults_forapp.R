library(dplyr)
library(tidyr)
library(rio)
library(stringr)
library(stringi)

extractSAMMresults <- function(file_phi_path, file_si_path) {

  final_data_phi <- NULL
  final_data_si <- NULL
  # Load and process phi data if present
  if (!is.null(file_phi_path)) {
  data_phi <- rio::import(file_phi_path, fill = TRUE)[-1, ]

  parameters_to_keep_phi <- c("phib", "phid", "phis", "phitotOB")
  variable_names <- c("parameter", "value", "sd", "cv", "ci_lower", "ci_upper")

  # Check if data contains expected parameters for phi
  if (!any(grepl(paste(parameters_to_keep_phi, collapse = "|"), data_phi$V1))) {
    stop("The uploaded phi data file does not contain the expected parameters. Please check your upload.")
  }

  subject_ids_phi <- data_phi %>%
    filter(grepl(paste(c(parameters_to_keep_phi,"Results for","Skipping"), collapse = "|"), V1)) %>%
    select(V1, V2, V3, V4, V5, V6) %>%
    mutate(across(c(2:6), as.numeric)) %>%
    # Step 1: Identify rows with "Results for" and create a group ID
    mutate(
      is_result = grepl("Results for", V1), # Identify "Results for" rows
      is_skipped = grepl("Skipping file", V1),
    ) %>%
    # Step 2: Extract subject IDs from "Results for" rows
    mutate(
      # Removing "_cpepmm" from the id in both cases
      id = ifelse(is_result, trimws(gsub("Results for (.*)_cpepMM", "\\1", V1)), NA),
      id = ifelse(is_skipped, trimws(gsub("Skipping file (.*)_cpepMM", "\\1", V1)), id)
    ) %>%
    fill(id, .direction = "down") %>%               # Fill subject ID down for each group
    # Step 3: Identify rows with phi-related values
    mutate(
      is_phi = grepl(paste(parameters_to_keep_phi, collapse = "|"), V1)  # Flag rows with phi parameters
    ) %>%

    # Step 4: Summarize group information and determine if phi exists in the group
    group_by(id) %>%
    mutate(
      has_parameter = any(is_phi)                                 # Check if any phi exists in the group
    ) %>%
    ungroup() %>%
    select(id, has_parameter,is_skipped) %>%
    unique()

  warning_messages_phi <- data_phi %>%
    filter(grepl(paste(c(".stu","Warning","Notice","error","hit lower limit","hit upper limit"), collapse = "|"), V1,ignore.case=T)) %>%
    select(V1) %>%
    mutate(is_id=grepl(".stu",V1)) %>%
    mutate(is_message=!grepl(".stu",V1)) %>%
    mutate(id = ifelse(is_id,
                       gsub("_cpepMM", "",          # Remove "_cpepMM" suffix if it exists
                            gsub("^.*\\\\", "", V1)  # Remove directory path before the filename
                       ), NA)) %>%
    mutate(id = gsub("\\..*$", "", id)) %>%  # Remove the file extension from the ID
    fill(id, .direction = "down") %>%
    group_by(id) %>%
    mutate(
      has_message = any(is_message)
    ) %>%
    filter(is_message) %>%
    select(id,has_message,V1) %>%
    mutate(
      V1 = trimws(V1)
    ) %>%
    mutate(
      V1 = gsub("\\s+", " ", V1)
    ) %>%
    group_by(id) %>%
    summarise(
      message = paste(V1, collapse = " ")
    ) %>%
    ungroup()

  parameter_data_phi <- data_phi %>%
    filter(grepl(paste(c(parameters_to_keep_phi,"Results for","Skipping"), collapse = "|"), V1)) %>%
    select(V1, V2, V3, V4, V5, V6) %>%
    mutate(across(c(2:6), as.numeric)) %>%
    mutate(
      is_result = grepl("Results for", V1), # Identify "Results for" rows
      is_skipped = grepl("Skipping file", V1),
    ) %>%
    mutate(
      id = ifelse(is_result, trimws(gsub("Results for (.*)_cpepMM", "\\1", V1)), NA),
      id = ifelse(is_skipped, trimws(gsub("Skipping file (.*)_cpepMM", "\\1", V1)), id)
    ) %>%
    fill(id, .direction = "down") %>%
    mutate(
      is_phi = grepl(paste(parameters_to_keep_phi, collapse = "|"), V1)
    ) %>%
    group_by(id) %>%
    mutate(
      has_parameter = any(is_phi)
    ) %>%
    ungroup() %>%
    filter(has_parameter) %>%
    filter(!is.na(V2)) %>%
    select(id,everything(),-is_phi,-has_parameter,-is_result,-is_skipped) %>%
    rename_with(~ variable_names, .cols = V1:V6)

  if (nrow(filter(subject_ids_phi,has_parameter == FALSE)) == 0) {
    # Return an empty dataframe with the expected structure
    missing_parameter_data_phi <- tibble(
      id = character(),
      !!!setNames(lapply(variable_names, function(x) numeric()), variable_names)
    )
  } else {
    # Proceed with creating missing data for each subject
    missing_parameter_data_phi <- subject_ids_phi %>%
      filter(has_parameter == FALSE) %>%
      select(id) %>%
      distinct() %>%
      rowwise() %>%
      mutate(
        missing_data = list(
          tibble(!!!setNames(
            lapply(variable_names, function(x) rep(NA, length(parameters_to_keep_phi))),
            variable_names
          )) %>%
            mutate(id = id, parameter = parameters_to_keep_phi) # Add the `id` column
        )
      ) %>%
      select(missing_data) %>%
      unnest(cols = c(missing_data))
  }


  final_parameter_data_phi<-merge(parameter_data_phi,missing_parameter_data_phi,all=T)

  final_data_phi <- subject_ids_phi %>%
    merge(warning_messages_phi,by = "id",all=T) %>%
    merge(final_parameter_data_phi, by = "id",all=T) %>%
    mutate(across(where(is.character), str_trim)) %>%
    mutate(is_zero= ifelse(value<=0,T,F))

  }

  # load and process SI data  if present
  if (!is.null(file_si_path)) {

      data_si <- rio::import(file_si_path, fill = TRUE)[-1, ]

      parameters_to_keep_si <- c("GE", "SI2")
      variable_names <- c("parameter", "value", "sd", "cv", "ci_lower", "ci_upper")

      # Check if data contains expected parameters for SI
      if (!any(grepl(paste(parameters_to_keep_si, collapse = "|"), data_si$V1))) {
        stop("The uploaded SI data file does not contain the expected parameters. Please check your upload.")
      }

      subject_ids_si <- data_si %>%
        filter(grepl(paste(c(parameters_to_keep_si,"Results for","Skipping"), collapse = "|"), V1)) %>%
        select(V1, V2, V3, V4, V5, V6) %>%
        mutate(across(c(2:6), as.numeric)) %>%
        # Step 1: Identify rows with "Results for" and create a group ID
        mutate(
          is_result = grepl("Results for", V1), # Identify "Results for" rows
          is_skipped = grepl("Skipping file", V1),
        ) %>%
        # Step 2: Extract subject IDs from "Results for" rows
          mutate(
            id = ifelse(is_result, trimws(gsub("Results for (.*)", "\\1", V1)), NA),
            id = ifelse(is_skipped, trimws(gsub("Skipping file (.*)", "\\1", V1)), id)
          ) %>%
        fill(id, .direction = "down") %>%               # Fill subject ID down for each group
        # Step 3: Identify rows with si related values
        mutate(
          is_si = grepl(paste(parameters_to_keep_si, collapse = "|"), V1)  # Flag rows with si parameters
        )%>%
        # Step 4: Summarize group information and determine if si exists in the group
        group_by(id) %>%
        mutate(
          has_parameter = any(is_si)                                 # Check if any si exists in the group
        ) %>%
        ungroup() %>%
        select(id, has_parameter,is_skipped) %>%
        unique()


      warning_messages_si <- data_si %>%
        filter(grepl(paste(c(".stu","Warning","Notice","error","hit lower limit","hit upper limit"), collapse = "|"), V1,ignore.case=T)) %>%
        select(V1) %>%
        mutate(is_id=grepl(".stu",V1)) %>%
        mutate(is_message=!grepl(".stu",V1)) %>%
        mutate(id = ifelse(is_id,
                           gsub("_gluMM", "",          # Remove "_cpepMM" suffix if it exists
                                gsub("^.*\\\\", "", V1)  # Remove directory path before the filename
                           ), NA)) %>%
        mutate(id = gsub("\\..*$", "", id)) %>% # Remove the file extension from the ID        fill(id, .direction = "down") %>%
        fill(id, .direction = "down") %>%
        group_by(id) %>%
        mutate(
          has_message = any(is_message)
        ) %>%
        filter(is_message) %>%
        select(id,has_message,V1) %>%
        mutate(
          V1 = trimws(V1)
        ) %>%
        mutate(
          V1 = gsub("\\s+", " ", V1)
        ) %>%
        group_by(id) %>%
        summarise(
          message = paste(V1, collapse = " ")
        ) %>%
        ungroup()


      parameter_data_si <- data_si %>%
        filter(grepl(paste(c(parameters_to_keep_si,"Results for","Skipping"), collapse = "|"), V1)) %>%
        select(V1, V2, V3, V4, V5, V6) %>%
        mutate(across(c(2:6), as.numeric)) %>%
        mutate(
          is_result = grepl("Results for", V1), # Identify "Results for" rows
          is_skipped = grepl("Skipping file", V1),
        ) %>%
        mutate(
          id = ifelse(is_result, trimws(gsub("Results for (.*)", "\\1", V1)), NA),
          id = ifelse(is_skipped, trimws(gsub("Skipping file (.*)", "\\1", V1)), id)
        ) %>%
        fill(id, .direction = "down") %>%
        mutate(
          is_si = grepl(paste(parameters_to_keep_si, collapse = "|"), V1)
        ) %>%
        group_by(id) %>%
        mutate(
          has_parameter = any(is_si)
        ) %>%
        ungroup() %>%
        filter(has_parameter) %>%
        filter(!is.na(V2)) %>%
        select(id,everything(),-is_si,-has_parameter,-is_result,-is_skipped) %>%
        rename_with(~ variable_names, .cols = V1:V6)


      if (nrow(filter(subject_ids_si,has_parameter == FALSE)) == 0) {
        # Return an empty dataframe with the expected structure
        missing_parameter_data_si <- tibble(
          id = character(),
          !!!setNames(lapply(variable_names, function(x) numeric()), variable_names)
        )
      } else {
        # Proceed with creating missing data for each subject
        missing_parameter_data_si <- subject_ids_si %>%
          filter(has_parameter == FALSE) %>%
          select(id) %>%
          distinct() %>%
          rowwise() %>%
          mutate(
            missing_data = list(
              tibble(!!!setNames(
                lapply(variable_names, function(x) rep(NA, length(parameters_to_keep_si))),
                variable_names
              )) %>%
                mutate(id = id, parameter = parameters_to_keep_si) # Add the `id` column
            )
          ) %>%
          select(missing_data) %>%
          unnest(cols = c(missing_data))
      }

      final_parameter_data_si<-merge(parameter_data_si,missing_parameter_data_si,all=T)

      final_data_si <- subject_ids_si %>%
        merge(warning_messages_si,by = "id",all=T) %>%
        merge(final_parameter_data_si, by = "id",all=T) %>%
        mutate(across(where(is.character), str_trim)) %>%
        mutate(GE=ifelse(parameter=="GE",value,NA)) %>%
        group_by(id) %>%
        fill(GE,.direction = "updown") %>%
        ungroup() %>%
        filter(parameter!="GE") %>%
        mutate(is_zero= ifelse(value<=0,T,F))
  }

  # Merge data if both files are provided, else return whichever is available
  if (!is.null(final_data_phi) && !is.null(final_data_si)) {
    final_output_long <- merge(final_data_phi, final_data_si, all = TRUE) %>%
      select(id, contains("has_parameter"),contains("is_skipped"),contains("is_zero"),contains("message"),everything()) %>%
      unique()

    final_output <- merge(final_data_phi, final_data_si, all = TRUE) %>%
      unique() %>%
      pivot_wider(names_from = parameter,values_from = -c(id,parameter),names_glue = "{parameter}_{.value}") %>%
      mutate(
        phi_has_parameter = coalesce(!!!select(., matches("phi.*_has_parameter$"))),
        si_has_parameter = coalesce(!!!select(., matches("si.*_has_parameter$"))),
        phi_is_skipped = coalesce(!!!select(., matches("phi.*_is_skipped$"))),
        si_is_skipped = coalesce(!!!select(., matches("si.*_is_skipped$"))),
        phi_message = coalesce(!!!select(., matches("phi.*_message$"))),
        si_message = coalesce(!!!select(., matches("si.*_message$"))),
      ) %>%
      select(-matches("phi.*_has_parameter$"),phi_has_parameter,
             -matches("si.*_has_parameter$"),si_has_parameter,
            -matches("phi.*_is_skipped$"),phi_is_skipped,
             -matches("si.*_is_skipped$"),si_is_skipped,
             -matches("phi.*_message$"),phi_message,
             -matches("si.*_message$"),si_message,-matches("phi.*_GE$")) %>%
      select(id,contains("_has_parameter"),contains("is_skipped"),contains("is_zero"),contains("message"),
             contains(parameters_to_keep_phi),contains(parameters_to_keep_si))

  } else if (!is.null(final_data_phi) && is.null(final_data_si)) {
    final_output_long <- final_data_phi %>%
      unique()

    final_output <- final_data_phi %>%
      unique() %>%
      pivot_wider(names_from = parameter,values_from = -c(id,parameter),names_glue = "{parameter}_{.value}") %>%
      mutate(
        phi_has_parameter = coalesce(!!!select(., matches("phi.*_has_parameter$"))),
        phi_is_skipped = coalesce(!!!select(., matches("phi.*_is_skipped$"))),
        phi_message = coalesce(!!!select(., matches("phi.*_message$"))),
      ) %>%
      select(-matches("phi.*_has_parameter$"),phi_has_parameter,
             -matches("phi.*_is_skipped$"),phi_is_skipped,
             -matches("phi.*_message$"),phi_message) %>%
      select(id,contains("_has_parameter"),contains("is_skipped"),contains("is_zero"),contains("message"),
             contains(parameters_to_keep_phi))

  } else if (!is.null(final_data_si) && is.null(final_data_phi)) {
    final_output_long <- final_data_si %>%
      unique()

    final_output <- final_data_si %>%
      unique() %>%
      pivot_wider(names_from = parameter,values_from = -c(id,parameter),names_glue = "{parameter}_{.value}") %>%
      mutate(
        si_has_parameter = coalesce(!!!select(., matches("si.*_has_parameter$"))),
        si_is_skipped = coalesce(!!!select(., matches("si.*_is_skipped$"))),
        si_message = coalesce(!!!select(., matches("si.*_message$"))),
      ) %>%
      select(
             -matches("si.*_has_parameter$"),si_has_parameter,
             -matches("si.*_is_skipped$"),si_is_skipped,
             -matches("si.*_message$"),si_message,-matches("phi.*_GE$")) %>%
      select(id,contains("_has_parameter"),contains("is_skipped"),contains("is_zero"),contains("message"),
             contains(parameters_to_keep_si))

  } else {
    stop("No input files were provided.")
  }

  # Return both outputs in a list
  return(list(final_output_long = final_output_long, final_output = final_output))

}
