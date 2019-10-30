#' Adobe Analytics JWT Authentication
#' for the R2Adobe package.
#' 
#' @title Generate JWT for 2.0 API Authentication
#'
#' @param filename Path to a config file containing parameters necessary for authentication.
#' @param expiry_time Expiration time of access token - defaults to one day.
#' @param ims_host Host for authentication call.
#' @param ims_endpoint_jwt Endpoint for authentication call.
#' @param verbose_output Flag for verbose output from httr GET request.
#'   
#' @importFrom httr POST content
#' @importFrom jose jwt_claim jwt_encode_sig
#' @importFrom jsonlite fromJSON
#'
#' @return Global credentials list in AdobeAnalytics (hidden environment.)    
#'            
#' @references 
#' Adobe documentation on how to set up a JWT integration can be found here:
#' 
#' https://www.adobe.io/authentication/auth-methods.html#!AdobeDocs/adobeio-auth/master/JWT/JWT.md
#' 
#' @examples 
#' \dontrun{
#' ## text
#' JWTAuth("api_key",
#'         "tech_acct",
#'         "org_id",
#'         "client_secret",
#'         "priv_key_filename",
#'          c("scope1", "scope2")
#' )
#' 
#' }
#' 
#' @export
#' 

JWTAuth <- function(filename,
                    expiry_time = as.integer(Sys.time()) + 60*60*24, ## hardcode later?
                    ims_host = 'ims-na1.adobelogin.com',
                    ims_endpoint_jwt = '/ims/exchange/jwt',
                    verbose_output = FALSE
                    ){
  
   if(class(try(read.table(filename, stringsAsFactors = FALSE), silent = FALSE)) != "try-error"){
     config_vars <- read.table(filename, 
                               stringsAsFactors = FALSE, 
                               col.names = c("key", "value"))
     api_key <- config_vars$value[config_vars$key == "api_key"]
     tech_acct = config_vars$value[config_vars$key == "tech_acct"]
     org_id = config_vars$value[config_vars$key == "org_id"]
     company_id = config_vars$value[config_vars$key == "company_id"]
     client_secret = config_vars$value[config_vars$key == "client_secret"]
     priv_key_filename = config_vars$value[config_vars$key == "priv_key_filename"]
     scopes = strsplit(config_vars$value[config_vars$key == "scopes"], split = ",")[[1]]
     message(paste0("Authenticating using file: ", filename))
   }  else {
     stop("Config file could not be read.")
   }

  if(is.null(api_key)){
    stop("You must specify an API key.")
  }
  
  if(is.null(tech_acct)){
    stop("You must specify a tech account ID.")
  }
  
  if(is.null(org_id)){
    stop("You must specify an organization ID.")
  }
  
  if(is.null(company_id)){
    stop("You must specify a company ID.")  ## make note about getting company ID here.
  }
  
  if(is.null(client_secret)){
    stop("You must specify a client secret.")
  }
  
  if(is.null(priv_key_filename)){
    stop("You must specify a path to a private key.")
  }
  
  
  
  ## Scopes should be defined by the user for now - possibly hardcode them later.
  ## Test integration used in writing this function user the following scopes:
  ## "ent_user_sdk" - User Management API 2.0
  ## "ent_analytics_bulk_ingest_sdk" Analytics API 2.0
  ##  The above scopes seem to be sufficient for most Adobe Analytics related Adobe usage.
  
  if(length(scopes) == 0){
    stop("You must define scopes in order to generate a token successfully.\nSee scopes for your integration in the JWT tab of your integration details page.")
  }
  
  if(length(scopes) == 0){
    stop("You must define scopes in order to generate a token successfully.\nSee scopes for your integration in the JWT tab of your integration details page.")
  }
  
  ## In the event that Adobe changes the authentication endpoints.
  if(ims_host != 'ims-na1.adobelogin.com' | ims_endpoint_jwt != '/ims/exchange/jwt'){
    warning("Using nonstandard authentication endpoint.")
  }
  
  ## Use jose package to generate jwt payload
  payload <- jwt_claim(iss = org_id,  
                       exp = expiry_time,
                       sub = tech_acct,
                       aud = paste0("https://", ims_host, "/c/",api_key))
  
  
  ## Add specified scopes to the payload
  for(scope in scopes){
    payload[paste0("https://", ims_host, "/s/", scope)] = TRUE
  }
  
  
  ## Read in private key file and generate RS256 token.
  priv_key <- readChar(priv_key_filename, file.info(priv_key_filename)$size)
  jwt_token <- jwt_encode_sig(payload, priv_key)
  
  
  if(verbose_output == TRUE){
    url = paste0("https://", ims_host, ims_endpoint_jwt)
    token_result <- POST(url, add_headers(`Content-Type` = "application/x-www-form-urlencoded",
                                          `Cache-Control` = "no-cache"),
                         body = list(client_id = api_key,
                                     client_secret = client_secret,
                                     jwt_token = jwt_token),
                         encode = "form",
                         verbose()
                         )
    token_result$status_code
  } else {
    url = paste0("https://", ims_host, ims_endpoint_jwt)
    token_result <- POST(url, add_headers(`Content-Type` = "application/x-www-form-urlencoded",
                                          `Cache-Control` = "no-cache"),
                         body = list(client_id = api_key,
                                     client_secret = client_secret,
                                     jwt_token = jwt_token),
                         encode = "form"
    )
    token_result$status_code
  }
  
  ## Get access token using httr package.
  if(token_result$status_code != 200){
    message(paste0("POST failed with error code: ",  ## Add more specific messages for different codes later?
                   as.character(token_result$status_code)))
  } else {
    message(paste0("POST successful! ",
                   as.character(token_result$status_code)))
  }
  
  #access_token
  access_token <- fromJSON(content(token_result, as = "text",type="application/json", encoding = "UTF-8"), flatten = TRUE)$access_token
  message("Access token generated:")
  message(access_token)
  assign("JWT_Credentials", list(access_token = access_token, 
                                 company_id = company_id,
                                 api_key =  api_key,
                                 org_id = org_id), 
         envir = AdobeAnalytics)
  print("Credentials saved in package namespace.")
}


#Create an environment to hold credentials - thank you Randy Zwitch.
AdobeAnalytics <- new.env(parent = emptyenv())

 