#' api_config Class
#'
#' A R6 class to manage config.
#'
api_config <- R6Class(
  "api_config",
  private = list(
    api_key = NULL,
    proxy=list(),
    api_endpoints = list(
      chat_completions = "https://api.openai.com/v1/chat/completions",
      completions = "https://api.openai.com/v1/completions",
      files = "https://api.openai.com/v1/files",
      models = "https://api.openai.com/v1/models",
      fine_tuning_jobs ="https://api.openai.com/v1/fine_tuning/jobs",
      embeddings = "https://api.openai.com/v1/embeddings",
      audio = "https://api.openai.com/v1/audio",
      images = "https://api.openai.com/v1/images",
      assistants = "https://api.openai.com/v1/assistants",
      threads = "https://api.openai.com/v1/threads"
    )
  ),
  public = list(
    #' @description Initialize the api_config object
    initialize = function() {
    },
    #' @description Configure the api_key settings.
    #' @param api_key your openai_key
    set_api_key=function(api_key){
      private$api_key=api_key
    },
    #' @description Configure the proxy settings.
    #' @param proxy_ip character Required. The IP address of the proxy.
    #' @param proxy_port character Required. The port number of the proxy.
    set_proxy = function(proxy_ip,proxy_port){
      if(!grepl("^([0-9]{1,3}\\.){3}[0-9]{1,3}$", proxy_ip)) {
        stop("Invalid proxy IP address.")
      }
      ip_components <- unlist(strsplit(proxy_ip, split = "\\."))
      if(any(as.numeric(ip_components) > 255) || any(as.numeric(ip_components) < 0)) {
        stop("Invalid proxy IP address.")
      }
      if(!is.numeric(proxy_port) || proxy_port < 1 || proxy_port > 65535) {
        stop("Invalid proxy port number.")
      }
      private$proxy$ip = proxy_ip
      private$proxy$port = proxy_port
    },
    #' @description Configure the proxy gettings.
    get_proxy=function(){
      private$proxy
    },
    #' @description Api key gettings.
    get_api_key=function(){
      private$api_key
    },
    #' @description Endpoints gettings.
    get_api_endpoints=function(){
      private$api_endpoints
    }
  )
)
