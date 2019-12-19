#' @title Get JIL Permissions
#' 
#' @details 
#' 
#' @description Get a dataframe summarizing all permission items in a given license group.
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
#' permissions_df <- JILPermissions(licensegroup_id = myLicenseGroup, product_id = myProduct, token = token, x_api_key = x_api_key)
#' }
#' 
#' 

JILPermissions <- function(licensegroup_id,
                           product_id,
                           token,
                           x_api_key,
                           verbosity = FALSE){
  
  baseurl <- "https://bps-il.adobe.io/jil-api/v2/organizations/"
  url <- paste0(baseurl, 
                AdobeAnalytics$JWT_Credentials$org_id, 
                "/products/",
                product_id,
                "/license-groups/", 
                license_group,
                "/permissions")
  
  permissions_data <- JILGet(url = url,
                             token = token,
                             x_api_key = x_api_key,
                             verbose_output = verbosity)

  permissions_df_list <- list()
  for(ii in 1:length(permissions_data$sections[[1]]$content)){
    permissions_type <- permissions_data$sections[[1]]$content[[ii]]$id
    permissions_payload <- permissions_data$sections[[1]]$content[[ii]]$content[[1]]$values
    permissions_item <- character()
    permissions_name <- character()
    included <- character()
    
    for(i in 1:length(permissions_payload)){
      permissions_item <- c(permissions_item, permissions_payload[[i]]$id)
      permissions_name <- c(permissions_name, permissions_payload[[i]]$name)
      included <- c(included, permissions_payload[[i]]$included)
    }
    
    permissions_df <- data.frame(permissions_item = permissions_item,
                                 permissions_name = permissions_name,
                                 included = included)
    permissions_df$permissions_type <- permissions_type
    permissions_df_list[[ii]] <- permissions_df
  }
  
  full_permissions_df <- do.call(rbind, permissions_df_list)
  full_permissions_df
}