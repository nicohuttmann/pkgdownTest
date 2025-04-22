#' Helping function to get started 
#'
#' @returns
#' @export
#'
#' @examples
hello_I_want_to_analyse_my_PELSA_data <- function() {
  
  message("\nHi, no worries! We'll get this done in no time.")
  
  Sys.sleep(3)
  
  message("\nLet's start with selecting the DIA-NN report output file.")
  
  Sys.sleep(3.5)
  
  message("\nWe'll start in")
  
  Sys.sleep(1.5)
  
  message("3")
  
  Sys.sleep(1)
  
  message("2")
  
  Sys.sleep(0.75)
  
  message("1")
  
  Sys.sleep(0.5)
  
  file <- choose.files(multi = F)
  
  message("\nGood job! Now you can copy the code below and continue with importing your data.\n")
  
  Sys.sleep(2)
  to_cat <- paste0('data_raw <- import_data_long("', 
                   gsub("\\\\", "\\\\\\\\", file), 
                   '")')
  
  for (i in unlist(strsplit(to_cat, split = ""))) {
    cat(i)
    Sys.sleep(0.001)
  }
  # cat(paste0('data_raw <- import_diann_report("', 
  #            gsub("\\\\", "\\\\\\\\", file), 
  #            '")'))
  
}


#' Import data and prepare the processing command 
#'
#' @param file file location (leave out to select manually)
#' @param silent prevent next command to be printed 
#' @param data.origin by which software the data was searched 
#' @param experiment.type whether the experimental design was group-wise ("gw") 
#' or dose-response ("dr") 
#' @param ... further arguments if data.origin is "other" like <sample_names>
#' 
#' @importFrom magrittr %>%
#' @returns
#' @export
#'
#' @examples
import_data_long <- function(file, 
                             silent = F, 
                             data.origin = c("diann", "other"), 
                             experiment.type = c("gw", "dr"), 
                             ...) {
  
  # Check file argument 
  if (!hasArg(file)) 
    file <- choose.files()
  
  # Check if file exists 
  if (!file.exists(file)) 
    stop(paste0('File "', file, '" could not be found.'))
  
  # Load .parquet or .tsv
  if (tools::file_ext(file) == "parquet") 
    data_import <- arrow::read_parquet(file)
  else if (tools::file_ext(file) %in% c("tsv", "csv", "txt"))
    data_import <- vroom::vroom(file, col_types = cols()) 
  else {
    data_import <- vroom::vroom(file, col_types = cols())
    message("Please check that the data was imported correctly. If not, please import the data manually and proceed with the process_data_... function.")
  }
  
  
  # Check other arguments 
  data.origin <- match.arg(data.origin, c("diann", "other"))
  
  experiment.type <- match.arg(experiment.type, c("gw", "dr"))
  
  additional.args <- list(...)
  
  # Identify file names 
  if (data.origin == "diann") {
    sample_names <- data_import$Run %>% 
      unique() %>% 
      sort()
  } else if ("sample_names" %in% names(additional.args)) {
    
  } else {
    stop('Please provide the sample names column via the <sample_names> argument:\n\nimport_data_long(file = "your file.csv", data.origin = "other", sample_names = "Runs"')
  }
  
  
  # Output processing command (if silent = F)
  if (!silent) {
    
    # Prepare vector for sample names 
    sample_names <- sample_names %>% 
      setNames(stringr::str_remove(., .longest_common_prefix(.)), .)
    
    samples_n <- paste0(names(sample_names), '" = "', sample_names, '"')
    samples_v <- paste0('c("', paste(samples_n, collapse = ',\n\t"'), ')')
    
    
    # Prepare vector for sample groups  
    sample_groups <- names(sample_names) %>% 
      setNames(rep("Control", length(sample_names)), .)
    
    groups_n <- paste0(names(sample_groups), '" = "', sample_groups, '"')
    groups_v <- paste0('c("', paste(groups_n, collapse = ',\n\t"'), ')')
    
    
    # Prepare vector for sample concentrations  
    sample_concentrations <- names(sample_names) %>% 
      setNames(rep("1e-0", length(sample_names)), .)
    
    concentrations_n <- paste0(names(sample_concentrations), '" = ', sample_concentrations)
    concentrations_v <- paste0('c("', paste(concentrations_n, collapse = ',\n\t"'), ')')
    
    
    # Prepare vector for sample repicates annotation 
    sample_replicates <- names(sample_names) %>% 
      setNames(rep("rep1", length(sample_names)), .)
    
    replicates_n <- paste0(names(sample_replicates), '" = "', sample_replicates, '"')
    replicates_v <- paste0('c("', paste(replicates_n, collapse = ',\n\t"'), ')')
    
    
    
    # Different commands for gr and dr experiments 
    if (experiment.type == "gw") {
      
      # Suggest process function
      
      to_cat1 <- paste0('data_processed <- process_data_gw(data_raw,\n', 
                        "\t# You can change the name for each sample here below\n", 
                        '\tsample_names = ', samples_v, ',\n', 
                        "\t# You need to change the groups for each sample here below\n", 
                        '\tsample_groups = ', groups_v, ',\n')
      
      
    } else if (experiment.type == "dr") {
      
      to_cat1 <- paste0('data_processed <- process_data_dr(data_raw,\n', 
                        "\t# You can change the name for each sample here below\n", 
                        '\tsample_names = ', samples_v, ',\n', 
                        "\t# You need to change the treatment concentrations for each sample here below\n", 
                        '\tsample_concentrations = ', concentrations_v, ',\n', 
                        "\t# You need to change the treatment replicates for each sample here below\n", 
                        '\tsample_replicates = ', replicates_v, ',\n')
      
    }
    
    # Different arguments for diann and other result origins 
    if (data.origin == "diann") {
      
      to_cat2 <- paste0('\tquant_column = "Precursor.Normalised",\n', 
                        '\tmin_val_per_group = 1,\n', 
                        '\tnorm.method = "none",\n', 
                        '\tLib.Q.Value = 0.01,\n', 
                        '\tLib.PG.Q.Value = 0.01)')
      
    } else if (data.origin == "other") {
      
      to_cat2 <- paste0('\tquant_column = "Precursor.Normalised",\n', 
                        '\tmin_val_per_group = 1,\n', 
                        '\tnorm.method = "none")')
      
    }
    
    
    # Output command
    
    message("Your data was imported successfully.")
    
    Sys.sleep(2.5)
    
    message("\nPlease copy the code below to prepare the precursor data for limma analysis.\n")
    
    Sys.sleep(1.5)
    
    
    to_cat <- paste0(to_cat1, to_cat2)
    
    cat_chars <- paste0(unlist(strsplit(to_cat, split = "\n")), "\n")
    
    cat_final <- c(unlist(strsplit(cat_chars[c(1:4)], split = "")), 
                   cat_chars[-c(1:4)])
    
    cat_speed <- rep(0.05, nchar(to_cat))
    cat_speed[1:which(cat_final == "\n")[4]] <- 0.002
    
    
    for (i in seq_along(cat_final)) {
      cat(cat_final[i])
      Sys.sleep(cat_speed[i])
    }
    
    
    
    # to_cat <- paste0(to_cat1, to_cat2)
    # 
    # cat_speed <- rep(0.0001, nchar(to_cat))
    # cat_speed[1:str_locate_all(to_cat, "\n")[[1]][4, 1]] <- 0.001
    # 
    # cat_chars <- unlist(strsplit(to_cat, split = ""))
    # 
    # for (i in seq_along(cat_chars)) {
    #   cat(cat_chars[i])
    #   Sys.sleep(cat_speed[i])
    # }
    
    # cat(paste0('data_processed <- process_diann_report(data_raw,\n', 
    #            '\tsample_names = ', samples_v, ',\n', 
    #            '\tsample_groups = ', groups_v, ',\n',
    #            '\tquant_column = "Precursor.Normalised",\n', 
    #            '\tmin_val_per_group = 1,\n', 
    #            '\tnorm.method = "none",\n', 
    #            '\tLib.Q.Value = 0.01,\n', 
    #            '\tLib.PG.Q.Value = 0.01)'))
    
    message("\nYou can copy the code above and adjust names and sample information to match your experimental design.")
    
  }
  
  return(data_import)
  
}




#' Processs the imported DIA-NN precursor data 
#'
#' @param data_raw 
#' @param sample_names 
#' @param sample_groups 
#' @param peptide.id 
#' @param quant_column 
#' @param min_val_per_group 
#' @param norm.method 
#' @param proteotypic.only 
#' @param Q.Value 
#' @param PG.Q.Value 
#' @param Lib.Q.Value 
#' @param Lib.PG.Q.Value 
#' @param protein.q 
#' @param gg.q 
#'
#' @importFrom magrittr %>%
#' @returns
#' @export
#'
#' @examples
process_data_gw <- function(data_raw, 
                                 sample_names, 
                                 sample_groups, 
                                 peptide.id = "Stripped.Sequence", 
                                 quant_column = "Precursor.Normalised", 
                                 min_val_per_group = 1, 
                                 norm.method = "none", 
                                 proteotypic.only = F, 
                                 Q.Value = 1, 
                                 PG.Q.Value = 1, 
                                 Lib.Q.Value = 1, 
                                 Lib.PG.Q.Value = 1, 
                                 protein.q = 1, 
                                 gg.q = 1) {
  
  
  # Check input
  if (!hasArg(data_raw)) 
    stop("Please provide the imported data with <data_raw>.")
  
  
  # Filter data 
  if (hasArg(filter_rows)) {
    data_filtered <- dplyr::filter(data_raw, !!rlang::enquo(filter_rows)) 
  } else {
    data_filtered <- data_raw
  }
  
  
  
  # Check groups 
  if (length(unique(names(sample_groups))) < 2) 
    stop("Please define the sample groups by changing the names of the <sample_groups> argument vector. There must by minimum two groups.")
  
  
  
  
  # Extract quantitative data
  
  # Rename sample names 
  data_filtered <- data_filtered %>% 
    dplyr::rename(Samples = !!sample.column) %>% 
    dplyr::mutate(Samples = sample.names[Samples]) %>% 
    dplyr::arrange(Samples)
  
  # Summarise precursors to modified peptides 
  data_quant <- data_filtered %>%  
    dplyr::summarise(!!quant.column := sum(!!rlang::sym(quant.column)), 
                     .by = all_of(c("Samples", peptide.id))) 
  
  # Add group information 
  data_quant <- data_quant %>% 
    dplyr::mutate(Group = setNames(sample_groups, names(sample_names))[Run], 
                  .after = "Run") %>% 
    dplyr::mutate(n = sum(!!rlang::sym(quant_column) > 0), .by = c(peptide.id, "Group")) %>% 
    dplyr::mutate(p = n / max(n), .by = "Group") %>% 
    dplyr::filter(all(p >= min_val_per_group), .by = peptide.id) %>% ##### Check this 
    dplyr::mutate(n_peptide = length(Run), .by = peptide.id) %>% 
    dplyr::filter(n_peptide == max(n_peptide)) %>% 
    tidyr::pivot_wider(id_cols = "Run", 
                       names_from = peptide.id, 
                       values_from = quant_column)
  
  
  # Check sample groups 
  sample_groups_m <- names(sample_groups)[match(sample_names[data_quant$Run], sample_groups)]
  
  
  sample_groups_m <- tibble(sample_name_original = unname(sample_names)) %>% 
    dplyr::mutate(sample_name = (sample_names %>% 
                                   set_names(names(.), .))[sample_name_original], 
                  .before = 1) %>% 
    dplyr::mutate(condition)
  
  list_output <- list(data_raw_filtered = data_raw_filtered, 
                      data_processed = data_quant, 
                      sample_groups = sample_groups_m)
  
  
  
  # Message 
  message("/nYour data was successfully processed and can be used for the limma analysis. Please copy the code below./n/n")
  
  to_cat <- paste0('data_limma <- limma_diann_report(data_processed,\n', 
                   'conditions = c("', 
                   unique(sample_groups_m)[1], 
                   '", "', 
                   unique(sample_groups_m)[2], 
                   '"),\n', 
                   '\tp.adjust.method = "BH",\n', 
                   '\tp.threshold = 0.01,\n', 
                   '\tfc.threshold = log2(1.2))')
  
  for (i in unlist(strsplit(to_cat, split = ""))) {
    cat(i)
    Sys.sleep(0.001)
  }
  
  # Return output list 
  return(list_output)
}



#' Processs the imported DIA-NN precursor data 
#'
#'
#' @importFrom magrittr %>%
#' @returns
#' @export
#'
#' @examples
process_data_dr <- function(data_raw, 
                            peptide.id = "Stripped.Sequence", 
                            sample.column = "Run", 
                            quant.column = "Precursor.Normalised", 
                            filter.rows, 
                            min_fraction_per_rep = 1, 
                            min_reps_per_peptide = 3, 
                            norm.method = "none", 
                            sample.names, 
                            sample.concentrations, 
                            sample.replicates) {
  
  # Check input
  if (!hasArg(data_raw)) 
    stop("Please provide the imported data with <data_raw>.")
  
  
  # Filter data 
  if (hasArg(filter_rows)) {
    data_filtered <- dplyr::filter(data_raw, !!rlang::enquo(filter_rows)) 
  } else {
    data_filtered <- data_raw
  }
  
  
  
  # Check groups 
  # if (length(unique(names(sample_groups))) < 2) 
  #   stop("Please define the sample groups by changing the names of the <sample_groups> argument vector. There must by minimum two groups.")
  # 
  
  
  # Extract quantitative data
  
  # Rename sample names 
  data_filtered <- data_filtered %>% 
    dplyr::rename(Samples = !!sample.column) %>% 
    dplyr::mutate(Samples = sample.names[Samples]) %>% 
    dplyr::arrange(Samples)
  
  # Also rename concentrations and replicates 
  names(sample.concentrations) <- sample.names[names(sample.concentrations)]
  names(sample.replicates) <- sample.names[names(sample.replicates)]
  
  # Summarise precursors to modified peptides 
  data_quant <- data_filtered %>%  
    dplyr::rename(Peptide = !!peptide.id) %>% 
    dplyr::summarise(Peptide.quant := sum(!!rlang::sym(quant.column)), 
                     .by = c("Samples", "Peptide")) %>% 
    # Add group information 
    dplyr::mutate(Concentration = sample.concentrations[Samples], 
                  Replicate = sample.replicates[Samples], 
                  .after = "Samples") 
  
  # Filter for number of replicates with valid values 
  data_output <- data_quant %>% 
    dplyr::mutate(n = sum(Peptide.quant > 0), 
                  .by = c("Peptide", "Replicate")) %>% 
    dplyr::mutate(p = n / max(n), .by = c("Replicate")) %>% 
    dplyr::filter(all(p >= min_fraction_per_rep), 
                  .by = c("Peptide", "Replicate")) %>% 
    dplyr::filter(length(unique(Replicate)) >= min_reps_per_peptide, 
                  .by = "Peptide") %>% 
    dplyr::select(-c(n, p)) %>% 
    # Pivot to wide format data
    tidyr::pivot_wider(id_cols = c("Samples", "Concentration", "Replicate"),
                       names_from = "Peptide",
                       values_from = "Peptide.quant") %>% 
    dplyr::arrange(Replicate, Concentration)
  
  
  # Message 
  message("/nYour data was successfully processed and can be used for the limma analysis. Please copy the code below./n/n")
  
  to_cat <- paste0('data_limma <- fit_ll.4(data_processed_dr,\n', 
                   'conditions = c("', 
                   #unique(sample_groups_m)[1], 
                   '", "', 
                   #unique(sample_groups_m)[2], 
                   '"),\n', 
                   '\tp.adjust.method = "BH",\n', 
                   '\tp.threshold = 0.01,\n', 
                   '\tfc.threshold = log2(1.2))')
  
  for (i in unlist(strsplit(to_cat, split = ""))) {
    cat(i)
    Sys.sleep(0.001)
  }
  
  # Return output list 
  return(data_output)
}




#' Limma function 
#'
#' @param data_processed 
#' @param conditions 
#' @param p.adjust.method 
#' @param p.threshold 
#' @param fc.threshold 
#' @param eBayes.trend abundance-dependant variance estimation in the 
#' limma::eBayes function (default = TRUE)
#'
#' @importFrom magrittr %>%
#' @returns
#' @export
#'
#' @examples
limma_diann_report <- function(data_processed, 
                               conditions = c("Control", "Treatment"), 
                               p.adjust.method = "BH", 
                               p.threshold = 0.01, 
                               fc.threshold = log2(1.2), 
                               eBayes.trend = TRUE) {
  
  if (hasArg(data_processed)) {
    data_quant <- data_processed[["data_processed"]]
    sample_groups <- data_processed[["sample_groups"]]
  } else {
    stop("Please use the output of the process_diann_report() function as input for <data_processed>.")
  }
  
  
  # Check packages 
  
  ## limma 
  if (!require(limma)) {
    if(menu(c("Yes, please install the limma package now.", "No, I will install it myself."), title = ("The limma package is not installed yet, but required for the analysis. Would you like to install it?")) == 1) {
      if (!require("BiocManager", quietly = TRUE))
        install.packages("BiocManager")
      
      BiocManager::install("limma")
    } else {
      return(invisible(FALSE))
    }
  }
  
  ## biobroom 
  if (!require(biobroom)) {
    if(menu(c("Yes, please install the biobroom package now.", "No, I will install it myself."), title = ("The biobroom package is not installed yet, but required for the analysis. Would you like to install it?")) == 1) {
      if (!require("BiocManager", quietly = TRUE))
        install.packages("BiocManager")
      
      BiocManager::install("biobroom")
    } else {
      return(invisible(FALSE))
    }
  }
  
  
  # Add limma input 
  results_list <- list()
  
  ## Add base data frame 
  results_list[["data"]] <- data_quant %>% 
    tidyr::pivot_longer(-1) %>% 
    dplyr::mutate(value = log2(value)) %>% 
    tidyr::pivot_wider()
  
  ## Add eset 
  results_list[["eset"]] <- results_list[["data"]] %>%
    dplyr::rename(rowname = 1) %>% 
    tibble::column_to_rownames() %>%
    as.matrix() %>% 
    t() %>% 
    Biobase::ExpressionSet()
  
  ## Describe experimental groups
  sample_groups_mod <- setNames(c("Control", "Treatment"), conditions)[sample_groups]
  
  results_list[["design"]] <- 
    model.matrix(
      ~0+factor(sample_groups_mod, 
                unique(sample_groups_mod)))
  
  ## Correct column names
  colnames(results_list[["design"]]) <- unique(sample_groups_mod)
  
  
  # Do first linear model fit
  results_list[["fit"]] <- limma::lmFit(results_list[["eset"]], results_list[["design"]])
  
  # Describe comparisons (contrasts)
  results_list[["contrast.matrix"]] <- limma::makeContrasts(Treatment-Control, 
                                                            levels=results_list[["design"]])
  
  # Compute contrasts between groups
  results_list[["fit2"]] <- limma::contrasts.fit(fit = results_list[["fit"]], 
                                                 contrasts = results_list[["contrast.matrix"]])
  
  # Apply empirical ebayes to estimate t-statistic and calculate p-values
  results_list[["fit2_eBayes"]] <- limma::eBayes(results_list[["fit2"]], 
                                                 trend = eBayes.trend)
  
  
  # Define thresholds 
  results_list[["par"]] <- list(p.threshold = p.threshold, 
                                p.adjust.method = p.adjust.method, 
                                fc.threshold = fc.threshold)
  
  # Extract results 
  require(biobroom)
  results_list[["results"]] <- biobroom::tidy.MArrayLM(results_list[["fit2_eBayes"]]) %>% 
    # rename contrasts 
    dplyr::mutate(term = paste(rev(conditions), collapse = " - ")) %>% 
    dplyr::rename(id = gene) %>% 
    dplyr::arrange(p.value) %>% 
    # Adjust p-values
    dplyr::mutate(p.adjust = 
                    p.adjust(p.value, 
                             method = results_list[["par"]]$p.adjust.method), 
                  .after = "p.value") %>% 
    # Apply thresholds
    dplyr::mutate(regulation = dplyr::case_when(
      p.adjust < results_list[["par"]]$p.threshold & 
        estimate >= results_list[["par"]]$fc.threshold ~ "up", 
      p.adjust < results_list[["par"]]$p.threshold & 
        estimate <= - results_list[["par"]]$fc.threshold ~ "down", 
      .default = "none"
    ))
  
  require(ggplot2)
  
  results_list[["p"]] <- results_list[["results"]] %>%
    dplyr::filter(!is.na(regulation)) %>%
    ggplot(aes(x = estimate,
               y = -log10(p.value),
               color = regulation)) +
    geom_point(shape = 16, alpha = 0.5) +
    scale_color_manual(values = c(none = "grey",
                                  up = "red",
                                  down = "blue")) +
    theme_classic() +
    coord_cartesian(expand = F)
  
  print(results_list[["p"]])
  
  return(results_list)
  
}




#' Title
#'
#' @param strs 
#'
#' @returns
#' @export
#'
#' @examples
.longest_common_prefix <- function(strs) {
  if (length(strs) == 0) return("")
  
  min_len <- min(nchar(strs))
  if (min_len == 0) return("")
  
  prefix <- character(0)
  
  for (i in 1:min_len) {
    current_char <- substr(strs[1], i, i)
    if (all(substr(strs, i, i) == current_char)) {
      prefix <- c(prefix, current_char)
    } else {
      break
    }
  }
  
  return(paste(prefix, collapse = ""))
}
