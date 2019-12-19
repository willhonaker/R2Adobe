#' @title Get JIL License Groups
#' 
#' @details 
#' 
#' @description Get a list of all license groups associated with a given product or user group from JIL API.
#' 
#' @param token 
#' @param x_api_key
#' @param top
#' @param verbose_output Toggle verbose output in request.
#' 
#' 
#' @return Data frame 
#' 
#' @export
#' 
#' @examples \dontrun{
#' license_groups <- JILLicenseGroups(item_id = myProduct, token = token, x_api_key = x_api_key)
#' }
#' 
#' 

JILLicenseGroups <- function(item_id,
                             token,
                             x_api_key,
                             get_by = "product",
                             top = 1000,
                             verbosity = FALSE){
  
  baseurl <- "https://bps-il.adobe.io/jil-api/v2/organizations/"
  if(get_by == "product"){
    url <- paste0(baseurl, AdobeAnalytics$JWT_Credentials$org_id, "/products/", item_id, "/license-groups?page_size=", as.character(top))
  } else if(get_by == "usergroup"){
    url <- paste0(baseurl, AdobeAnalytics$JWT_Credentials$org_id, "/user-groups/", item_id, "/license-groups?page_size=", as.character(top))
  } else {
    stop("Can't get license groups for indicated dimension.")
  }
  
  licensegroup_data <- JILGet(url = url,
                             token = token,
                             x_api_key = x_api_key,
                             verbose_output = verbosity)
  
  if(length(licensegroup_data) == 0){
    licensegroups <- data.frame(source_item_id = item_id) %>%
      mutate(id = NA,
             name = NA,
             type = NA,
             userCount = NA,
             adminCount = NA,
             administerable = NA,
             externallyManaged = NA,
             description = NA)
    
  } else {
    
    licensegroup_list <- list()
    for(i in 1:length(licensegroup_data)){
      license_info <- licensegroup_data[[i]]
      license_info["fulfilledItems"] <- NULL ## beware of this hardcode
      license_info <- as.data.frame(license_info, stringsAsFactors = FALSE)
      licensegroup_list[[i]] <- license_info
    }
    
    licensegroups <- bind_rows(licensegroup_list)
    licensegroups$source_item_id <- item_id
  }
  
  licensegroups
}