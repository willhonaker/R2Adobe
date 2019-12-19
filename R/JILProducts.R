#' @title Get Products
#' 
#' @details 
#' 
#' @description Get a list of all products associated with your Organization's Adobe Instance.
#' 
#' @param token 
#' @param x_api_key
#' @param verbose_output Toggle verbose output in request.
#' 
#' 
#' @return Data frame 
#' 
#' @export
#' 
#' @examples \dontrun{
#' products <- JILProducts(token = token, x_api_key = x_api_key)
#' }
#' 
#' 

JILProducts <- function(token, 
                        x_api_key,
                        verbosity = FALSE){
  url <- paste0("https://bps-il.adobe.io/jil-api/v2/organizations/", AdobeAnalytics$JWT_Credentials$org_id, "/product")
  
  product_data <- JILGet(url, 
                         token = token, 
                         x_api_key = x_api_key,
                         verbose_output = verbosity)
  
  product_list <- list()
  for(i in 1:length(product_data)){
    productinfoflat <- lapply(product_data[[i]], function(x){
      x <- unlist(x)
      if(length(x) > 1){x <- paste(x, collapse = ";")}
      x
    })
    product_list[[i]] <- as.data.frame(productinfoflat[lengths(productinfoflat) != 0]) 
  }
  
  products <- bind_rows(product_list)
  
  products
}