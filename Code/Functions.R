
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
      filter(x2 == x2_target) %>%
      arrange(x1)
    
    approx(data_filt$x1, data_filt$y, xout = x1_target)$y
  } # END interpolate_x1 function
  
  # Second level interpolation of x2 parameter
  interpolate_x2 <- function(x1_target, x2_target) {
    data_filt <- interp_x1 %>% 
      dplyr::filter(x1 == x1_target) %>%
      arrange(x2)
    
    approx(data_filt$x2, data_filt$y, xout = x2_target, na.rm = TRUE)$y
  } # END interpolate_x2 function
  
  # Generalize datasets
  data <- data %>% 
    transmute(x1 = {{x1}}, x2 = {{x2}}, y = {{y}})
  
  # Execute first interpolation along x1
  interp_x1 <-
    crossing(
      tibble(x1 = seq(min(data$x1), max(data$x1), by = x1res)),
      tibble(x2 = unique(data$x2))) %>% 
    group_by(x2) %>% 
    mutate(y = interpolate_x1(x1, x2[1])) %>% 
    filter(is.na(y) == FALSE) %>% 
    group_by(x1) %>% 
    filter(length(x1)>1) %>%
    #RollingMean starts here:
    group_by(x2) %>% 
    mutate(y = zoo::rollmean(y, k = k, fill = NA)) %>% 
    ungroup() %>% 
    filter(is.na(y) == FALSE)
  
  if (dim == 1) {
    
    out <- interp_x1 %>% 
      transmute("{{x1}}" := x1,
                "{{x2}}" := x2,
                "{{y}}" := y)
    return(out)
    
    }
  if (dim == 2) {
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
}





#####
# From ZoopMethodsComp

#' Calculate and index Relative Abundances (eDNA Index).
#'
#' @description
#' The following function will calculate relative abundance (RA) and the index
#' of relative abundance (RA_Index) for any data set. The RA_Index is identical
#' to eDNA Index, Double transformation, and Wisconsin transformation.
#' This function will also handle extreme values to falls outside a given
#' percentile. Extreme will otherwise have a big impact on the index,
#' that is based on the population max.
#' 
#' @param data (Required) Tidy data object.
#' @param sample (Required) Data variable with sample ID.
#' @param taxa (Required) Data variable with taxa ID.
#' @param abundance (Required) Data variable with sequence read counts (raw or rarefied).
#' @param ... (Optional) Other sample data variables to be preserved.
#' @param na.rep (Optional) Value to replace NA in abundance variable. Default: 0.
#' @param extreme.perc (Optional) Value defining upper percentile for extreme values. Default: 0.99. If no adjustment wanted: 1.
#' @value Same data class as input.
#' 
#' @example
#' df_18S %>% index_RA(
#'    sample = Library_ID,
#'    taxa = Genus,
#'    abundance = Abundance,
#'    Depth, Date)

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






