
#' 2-dimensional data-interpolation
#'
#' Interpolates one dependent variable over two explainatory variables
#' @param data A data frame or tibble
#' @param x1 Variable name of the first explanatory variable
#' @param x2 Variable name of the second explanatory variable
#' @param y the dependent variable to be interpolated
#' @param x1res the resolution of interpolation along x1
#' @param x2res the resolution of interpolation along x2
#' @return A data frame containing variables X1, x2, and y
#' @examples 
#' CTD %>% 
#'  interpolate_2D(x1 = Depth, x2 = Date, y = Temperature, 1, 1) %>% 
#'  ggplot(aes(x = Depth, y = Date)) +
#'  geom_point(aes(colour = Temperature)) +
#'  scale_y_reverse()
#'  
#' @export

interpolate_2D <- function(data, x1, x2, y, x1res, x2res) {
  
  # First level interpolation of x1 parameter
  interpolate_x1 <- function(x1_target, x2_target) {
    data_filt <- data %>% 
      filter(x2 == x2_target) %>%
      arrange(x1)
    
    approx(data_filt$x1, data_filt$y, xout = x1_target)$y
  } # END interpolate_x1 function
  
  # Secon level interpolation of x2 parameter
  interpolate_x2 <- function(x1_target, x2_target) {
    data_filt <- interp_x1 %>% 
      dplyr::filter(x1 == x1_target) %>%
      arrange(x2)
    
    approx(data_filt$x2, data_filt$y, xout = x2_target, na.rm = TRUE)$y
  } # END interpolate_x2 function
  
  data <- data %>% 
    transmute(x1 = {{x1}}, x2 = {{x2}}, y = {{y}})
  
  # Execute depth interpolation
  interp_x1 <-
    crossing(
      tibble(x1 = seq(min(data$x1), max(data$x1), by = x1res)),
      tibble(x2 = unique(data$x2))) %>% 
    group_by(x2) %>% 
    mutate(y = interpolate_x1(x1, x2[1])) %>% 
    filter(is.na(y) == FALSE) %>% 
    group_by(x1) %>% 
    filter(length(x1)>1)
  
  interp_x2 <-
    crossing(
      tibble(x2 = seq(min(interp_x1$x2), max(interp_x1$x2), by = x2res)),
      tibble(x1 = unique(interp_x1$x1))) %>%
    group_by(x1) %>%
    mutate(y = interpolate_x2(x1[1], x2))
  
  out <- interp_x2 %>% 
    transmute("{{x1}}" := x1,
              "{{x2}}" := x2,
              "{{y}}" := y)
  
  return(out)
}



# Wrapper plot function:
plot_ocean <- function(Data, Parameter, option = "H", direction = 1) {
  
  Data %>%
    interpolate_2D(Depth, Date, {{Parameter}}, 1, 1) %>%
    filter(Depth < 250) %>% 
    ggplot() +
    geom_point(aes(Date, Depth, colour = {{Parameter}}), shape = 15) +
    scale_y_reverse() +
    scale_color_viridis(option = option, direction = direction) +
    theme_cowplot()
}









#' 2-dimensional data-interpolation with rolling means
#' @description
#' Two step interpolation of one dependent variable over two explanatory
#' variables, with the possibility of rolling mean of first variable.
#' 
#' @author Andreas Novotny
#' @param data A data frame or tibble
#' @param x1 Variable name of the first explanatory variable
#' @param x2 Variable name of the second explanatory variable
#' @param y the dependent variable to be interpolated
#' @param x1res (integer) the resolution of interpolation along x1
#' @param x2res (integer)the resolution of interpolation along x2
#' @param k (integer) the frame of rolling mean along x1. Default is 1, no rolling mean.
#' @param 2dim (default TRUE). If false, only interpolation along x1.
#' @return A data frame containing variables X1, x2, and y
#' @examples 
#' CTD %>% 
#'  interpolate_2D(x1 = Depth, x2 = Date, y = Temperature, 1, 1) %>% 
#'  ggplot(aes(x = Depth, y = Date)) +
#'  geom_point(aes(colour = Temperature)) +
#'  scale_y_reverse()
#'  
#' @export

interpolate_rollmean <- function(data, x1, x2, y,
                                 x1res, x2res,
                                 k = 1, dim = 2) {
  
  # First level interpolation of x1 parameter
  interpolate_x1 <- function(x1_target, x2_target) {
    data_filt <- data %>% 
      dplyr::filter(x2 == x2_target) %>%
      dplyr::arrange(x1)
    
    approx(data_filt$x1, data_filt$y, xout = x1_target)$y
  } # END interpolate_x1 function
  
  # Second level interpolation of x2 parameter
  interpolate_x2 <- function(x1_target, x2_target) {
    data_filt <- interp_x1 %>% 
      dplyr::filter(x1 == x1_target) %>%
      dplyr::arrange(x2)
    
    approx(data_filt$x2, data_filt$y, xout = x2_target, na.rm = TRUE)$y
  } # END interpolate_x2 function
  
  # Generalize datasets
  data <- data %>% 
    dplyr::transmute(x1 = {{x1}}, x2 = {{x2}}, y = {{y}})
  
  # Execute first interpolation along x1
  interp_x1 <-
    tidyr::crossing(
      dplyr::tibble(x1 = seq(min(data$x1), max(data$x1), by = x1res)),
      dplyr::tibble(x2 = unique(data$x2))) %>% 
    dplyr::group_by(x2) %>% 
    dplyr::mutate(y = interpolate_x1(x1, x2[1])) %>% 
    dplyr::filter(is.na(y) == FALSE) %>% 
    dplyr::group_by(x1) %>% 
    dplyr::filter(length(x1)>1) %>%
    #RollingMean starts here:
    dplyr::group_by(x2) %>% 
    dplyr::mutate(y = zoo::rollmean(y, k = k, fill = NA)) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(is.na(y) == FALSE)
  
  if (dim == 1) {
    
    out <- interp_x1 %>% 
      dplyr::transmute("{{x1}}" := x1,
                "{{x2}}" := x2,
                "{{y}}" := y)
    return(out)
    
    }
  if (dim == 2) {
    interp_x2 <-
      crossing(
        dplyr::tibble(x2 = seq(min(interp_x1$x2), max(interp_x1$x2), by = x2res)),
        dplyr::tibble(x1 = unique(interp_x1$x1))) %>%
      dplyr::group_by(x1) %>%
      dplyr::mutate(y = interpolate_x2(x1[1], x2))
    
    out <- interp_x2 %>% 
      dplyr::transmute("{{x1}}" := x1,
                "{{x2}}" := x2,
                "{{y}}" := y)
    return(out)
  }
}





# From ZoopMethodsComp

#' Calculate and Index Relative Abundances (eDNA Index)
#'
#' @description
#' This function calculates relative abundance (RA) and the index of relative
#' abundance (RA_Index) for any dataset containing ecological abundance data. The
#' RA_Index is equivalent to the eDNA Index, double transformation, and Wisconsin 
#' transformation methods. The function addresses extreme values by replacing them 
#' with -1 if they fall outside a specified percentile threshold. This adjustment 
#' helps mitigate the impact of outliers on the relative abundance calculations, 
#' particularly when the index is based on the maximum population value.
#'
#' @param data (Required) A tidy data frame containing ecological data.
#' 
#' @param sample (Required) A variable in the data frame representing the sample 
#' identifier. This will be used to group the data.
#' 
#' @param taxa (Required) A variable in the data frame representing the taxa 
#' identifier. This will be used for calculating relative abundances by taxa.
#' 
#' @param abundance (Required) A variable in the data frame containing sequence 
#' read counts (either raw or rarefied) for the taxa.
#' 
#' @param ... (Optional) Additional sample data variables to be preserved in the output.
#' 
#' @param na.rep (Optional) A numeric value to replace NA values in the abundance 
#' variable. Default is 0. This allows the function to handle missing data appropriately.
#' 
#' @param extreme.perc (Optional) A numeric value defining the upper percentile for 
#' identifying extreme values. Default is 0.99. If no adjustments for extreme values 
#' are desired, set this parameter to 1.
#' 
#' @return A data frame of the same class as the input, including the following columns:
#' - sample: The sample identifier.
#' - taxa: The taxa identifier.
#' - abundance: The total abundance count per sample and taxa.
#' - RA: The relative abundance of each taxon per sample.
#' - RA_Index: The indexed relative abundance, scaled to the maximum RA for each taxon.
#' Any extreme values are indicated by a specific adjustment based on the specified 
#' percentile.
#' 
#' @examples
#' # Example usage of the index_RA function:
#' df_18S %>% index_RA(
#'   sample = Library_ID,
#'   taxa = Genus,
#'   abundance = Abundance,
#'   Depth, Date
#' )
#' 
#' @import dplyr
#' @import rlang
#' 
#' @export
index_RA <- function(data, sample, taxa, abundance, ...,
                     na.rep = 0, extreme.perc = 0.99) {
  
  output <- data %>%
    
    # 1. Replace NA
    mutate("{{abundance}}" := ifelse(is.na({{abundance}}),
                                     na.rep,
                                     {{abundance}})) %>% 
    
    # 2. Summaries abundance per sample and taxa
    group_by({{sample}}, {{taxa}}, ...) %>% 
    summarise("{{abundance}}" := sum({{abundance}})) %>% 
    
    # 3. Calculate relative abundance (RA) for each sample
    group_by({{sample}}, ...) %>% 
    reframe(RA = {{abundance}} / sum({{abundance}}),
            {{taxa}}, {{abundance}}) %>%
    filter(is.na(RA) == FALSE) %>% 
    
    # 4. Remove extreme values of relative abundance
    # Extreme values are defined by percentile, and temporally assigned -1
    group_by({{taxa}}) %>%
    reframe(RA = ifelse(RA <= quantile(RA, extreme.perc), RA, -1),
            {{sample}}, {{abundance}}, ...) %>%
    # Extreme values (assigned -1) are reassigned to the new maximum value.
    group_by({{taxa}}) %>%
    reframe(RA = ifelse(RA == -1, max(RA), RA),
            {{sample}}, {{abundance}}, ...) %>% 
    
    # 5. Calculate indexed relative abundance per taxa.
    group_by({{taxa}}) %>%
    reframe({{abundance}}, RA, RA_Index = RA/max(RA),
            {{sample}}, ...) %>% 
    filter(is.na(RA_Index) == FALSE) %>% 
    
    # 6. Arrange columns:
    select({{sample}}, ..., {{taxa}}, {{abundance}}, RA, RA_Index)
  
}



#' Rarefy Sequence Depth
#'
#' This function performs rarefaction of sequence data to a specified sample size,
#' allowing for the comparison of species abundance across samples. Rarefaction is a 
#' technique used to standardize the number of individuals sampled from each sample, 
#' helping to mitigate the effects of sequencing depth on species diversity estimates.
#'
#' @param data A data frame containing the sequence data. The data frame must 
#' contain columns for samples, taxa, and their respective abundances.
#' 
#' @param sample A column name in the data frame representing the sample identifier.
#' This column will be used as the row identifier after pivoting the data.
#' 
#' @param taxa A column name in the data frame representing the taxa of interest.
#' This column will be pivoted to create separate columns for each taxon.
#' 
#' @param abundance A column name in the data frame representing the abundance 
#' values of each taxon in the samples. This data will be rarefied.
#' 
#' @param sample_size An integer specifying the target sample size for rarefaction. 
#' Default is set to 10,000. The function will rarefy each sample to this number of 
#' individuals. Adjust this value based on your data and analysis needs.
#' 
#' @return A data frame in long format that includes the original sample identifiers, 
#' taxa, and their rarefied abundances. The data frame maintains the same taxa 
#' and sample structure as the input but with rarefied abundance values.
#' 
#' @import rlang
#' @import vegan
#' @import tidyverse
#' 
#' @examples
#' # Example usage:
#' rarefied_data <- rarefy_sequence_depth(
#'   data = my_data,
#'   sample = sample_id,
#'   taxa = taxon_name,
#'   abundance = abundance_count,
#'   sample_size = 5000
#' )
#' 
#' @export

rarefy_sequence_depth <- function(data, sample, taxa, abundance,
                                  sample_size = 10000) {
  
  require(rlang)
  require(vegan)
  require(tidyverse)
  
  rarefied_data <- data %>%
    
    # 1. Pivot the data to wide format, where taxa become column names 
    # and their corresponding abundance values fill these columns, filling 
    # missing values with 0
    select({{sample}}, {{taxa}}, {{abundance}}) %>% 
    pivot_wider(names_from = {{taxa}}, values_from = {{abundance}},
                values_fill = 0) %>% 
    column_to_rownames(as_name(ensym(sample))) %>%
    
    # 2. Apply rarefaction to the data using the specified sample size
    # This function will randomly subsample the abundance data to the desired depth
    rrarefy(sample = sample_size) %>%
    
    # 3. Reshape the data back to long format, keeping the sample identifier 
    # and creating columns for taxa and their abundances
    as.data.frame() %>% 
    rownames_to_column(as_name(ensym(sample))) %>%
    pivot_longer(!1, names_to = as_name(ensym(taxa)),
                 values_to = as_name(ensym(abundance))) %>%
    
    # 4. Join the rarefied data back with the original data to retain 
    # other columns (excluding abundance) for further analysis
    right_join(select(data, !{{abundance}}),
               by = c(as_name(ensym(taxa)), as_name(ensym(sample))))
}






#' Convert Tidy Data to Vegan-Compatible Format
#'
#' @description
#' The `tidy_to_vegan` function converts a tidy data frame into a matrix and a data frame format 
#' suitable for use with the `vegan` package in R, allowing for analyses that
#' require a matrix format for species abundance data, while preserving other
#' variables for further analyses.
#'
#' @param data A tidy data frame containing ecological data.
#' @param sample (Required) A variable representing sample IDs (e.g., location, time point).
#' @param taxa (Required) A variable representing taxa identifiers (e.g., species names).
#' @param abundance (Required) A variable representing abundance values (e.g., count data).
#' @param ... Additional arguments that can be passed to the `select` function to retain other variables.
#'
#' @return A list containing:
#'   \item{mat}{A matrix where rows correspond to samples and columns correspond to taxa, 
#'               with abundance values filled in.}
#'   \item{dat}{A tidy data frame that retains the selected variables including samples, taxa, 
#'               and abundance, reshaped to wide format.}
#'
#' @example
#' # Example usage of the tidy_to_vegan function
#' result <- tidy_to_vegan(data = Indexed_UO_12S, 
#'                          sample = Sample_ID, 
#'                          taxa = scientificName, 
#'                          abundance = RA_Index, 
#'                          Month)
#' 
#' # Access the matrix and data frame
#' abundance_matrix <- result$mat
#' tidy_data <- result$dat
#'
#' @importFrom rlang ensym
#' @importFrom rlang as_name
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select
#' @importFrom tibble column_to_rownames
#' @importFrom base as.matrix
#'
#' @export

tidy_to_vegan <- function(data, sample, taxa, abundance, ...) {
  
  require(rlang)
  require(tidyverse)
  
  mat <- data %>%
    select({{sample}}, {{taxa}}, {{abundance}}) %>% 
    pivot_wider(names_from = {{taxa}}, values_from = {{abundance}}) %>% 
    column_to_rownames(as_name(ensym(sample))) %>% 
    as.matrix()
  
  dat <- data %>%
    select({{sample}}, {{taxa}}, {{abundance}}, ...) %>% 
    pivot_wider(names_from = {{taxa}}, values_from = {{abundance}}) %>% 
    select(!colnames(mat))
  
  
  list(mat = mat,
       dat = dat)
}

# Test
#tmp <- Indexed_UO_12S %>% 
#  tidy_to_vegan(Sample_ID, scientificName, RA_Index, Month, Location, Station_ID)
#tmp$dat

#' Perform Non-metric Multidimensional Scaling (NMDS) on Vegan-Compatible Data
#'
#' @description
#' The `veglist_NMDS` function performs Non-metric Multidimensional Scaling (NMDS) 
#' a list produced by the `tidy_to_vegan` function, which contains a matrix of
#' species abundance data and associated sample metadata. This function uses the 
#' `metaMDS` function from the `vegan` package to calculate NMDS scores for both species 
#' and samples, enhancing the output list with NMDS results.
#'
#' @param veglist A list containing:
#'   - `mat`: A matrix of species abundance data (samples as rows, species as columns).
#'   - `dat`: A tidy data frame with sample metadata (one of the columns should match the sample IDs).
#' @param trymax (Optional) An integer specifying the maximum number of random starts 
#'                for the NMDS calculation. Default is 100. Higher values can help find 
#'                better solutions but increase computation time.
#' @param trace (Optional) A logical value indicating whether to display trace output 
#'                from the NMDS algorithm. Default is FALSE. Setting to TRUE can help 
#'                diagnose convergence issues.
#'
#' @return A list containing:
#'   \item{mat}{The original matrix of species abundance data.}
#'   \item{dat}{The original tidy data frame of sample metadata.}
#'   \item{mod_nmds}{The NMDS model object produced by `metaMDS`.}
#'   \item{species_scores}{A data frame containing NMDS scores for each species, with species names as a column.}
#'   \item{sample_scores}{A data frame containing NMDS scores for each sample, along with associated metadata from the original data.}
#'
#' @example
#' # Example usage of the veglist_NMDS function
#' # Assuming veglist is a pre-existing list from tidy_to_vegan
#' result <- veglist_NMDS(veglist)
#'
#' @export
veglist_NMDS <- function(veglist, trymax = 100, trace = FALSE) {
  
  # Run NMDS
  mod <- metaMDS(veglist$mat, trymax = trymax, trace = trace)
  
  Species_scores <- 
    scores(mod)$species %>% 
    as.data.frame() %>%
    rownames_to_column("Species")
  
  Sample_scores <-
    scores(mod)$sites %>% 
    as.data.frame() %>%
    rownames_to_column(colnames(veglist$dat)[1]) %>% 
    left_join(veglist$dat, by = colnames(veglist$dat)[1])
  
  veglist$mod_nmds <- mod
  veglist$species_scores <- Species_scores
  veglist$sample_scores <- Sample_scores
  
  return(veglist)
}


## Test
#tmp <- Indexed_UO_12S %>%
#  filter(Month == "Aug") %>% 
#  tidy_to_vegan(Sample_ID, scientificName, RA_Index,
#                Month, Location) %>% 
#  veglist_NMDS()


#' Run SIMPER Analysis and Update Veglist
#'
#' @param veglist A list containing the matrix of community data and species scores.
#' @param group (Optional) A grouping variable for comparing different groups.
#' @return The updated veglist with species scores and SIMPER results.
#' @examples
#' result <- model_simper(veglist, group = "Treatment")
#'
#' @export
veglist_SIMPER <- function(veglist, group = NULL) {
  
  # Run SIMPER analysis
  simper_result <- if (is.null(group)) {
    simper(veglist$mat)
  } else {
    simper(veglist$mat, group)
  }
  
  # Summarize the SIMPER result
  simper_summary <- summary(simper_result)
  
  # Error handling for multiple sample groups
  if (length(simper_summary) > 1) {
    stop("Error: More than two sample groups detected. Please run SIMPER without a grouping variable.")
  }
  
  # Convert the summary to a data frame and clean column names
  Simper_tab <- as.data.frame(simper_summary[[1]]) %>%
    rownames_to_column("Species") %>%
    rename_with(~ sub(".*\\.", "", .), -Species)  # Clean column names, excluding 'Species'
  
  # Add significance indicator if 'p' column exists
  if ("p" %in% colnames(Simper_tab)) {
    Simper_tab <- Simper_tab %>%
      mutate(Significant = ifelse(p < 0.05, "Yes", "No"))
  }
  
  # Merge the SIMPER results with species scores in the veglist
  veglist$species_scores <- veglist$species_scores %>%
    left_join(Simper_tab, by = "Species")  # Ensure correct merging by Species
  
  return(veglist)
}

# Test
#tmp <- Indexed_UO_12S %>%
#  filter(Month == "Aug") %>% 
#  tidy_to_vegan(Sample_ID, scientificName, RA_Index,
#                Month, Location, Environment) %>% 
#  veglist_NMDS() %>% 
#  veglist_SIMPER(group = .$dat$Environment)





#' The following function will generate a list of prevalent species by
#' filtering the output generated by the function above. The function will
#' remove species with inadequate species names, and remove species with
#' generally low occurrences throughout the data set.

getSpeciesList <- function(data, type = "DNA") {
  # This function require the output of "collapseRAindex" function.
  
  # Prefiltration for DNA data.
  # An initial filtering step will remov any low observation.
  if (type == "DNA") {data <- filter(data, Abundance > 50)}
  
  List <- data %>% 
    
    # Remove low occurrences.
    # Each species has to be observed in more than 2 samples.
    filter(RA_Index > 1) %>% 
    group_by(Taxa) %>%
    filter(n() > 1) %>% 
    
    # Remove bad species names:
    # The following name patterns indicate that species level identification failed. 
    filter(!grepl('sp.', Taxa),
           !grepl('_sp', Taxa),
           !grepl(' sp', Taxa),
           grepl(' ', Taxa),
           is.na(Taxa)==FALSE,
           Taxa != "no identification") %>%
    
    # Special case database name correction:
    mutate(Taxa = ifelse(Taxa== "Discoconchoecia aff. elegans CMARA05309_Os109.1.1",
                         "Discoconchoecia elegans", Taxa)) %>% 
    
    # Get Taxa names
    pull(Taxa) %>% unique()
  
  return(List)  
  # This function returns a name vector of species that passed filtering parameters.
  
}




#' The following function will generate a list of prevalent genera by
#' filtering the output generated by the function above. The function
#' will remove genera with inadequate genus names, and remove genera with
#' generally low occurrences throughout the data set.


getGenusList <- function(data, type="DNA") {
  # This function require the output of "collapseRAindex" function.
  
  # Prefiltration for DNA data.
  # An initial filtering step will remove any low observation.
  if (type == "DNA") {data <- filter(data, Abundance > 50)}
  
  List <- data %>%
    
    # Remove low occurrences
    # Each genus has to be observed in more than 5 samples.
    filter(RA_Index > 1) %>% 
    group_by(Taxa) %>%
    filter(n() > 1) %>%
    
    # Remove bad genus names
    # The following name patterns indicate that genus level identification failed. 
    filter(is.na(Taxa)==FALSE,
           !grepl('_', Taxa),
           Taxa != "no identification",
           Taxa != "unknown genus") %>% 
    # Get Taxa names
    pull(Taxa) %>% unique()
  
  return(List)  
  # This function returns a name vector of genera that passed filtering parameters.
}






