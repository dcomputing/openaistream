#' api_config Class
#'
#' A R6 class to manage config.
#'re
api_config <- R6Class(
  "api_config",
  private = list(
    api_key = NULL,
    targetAi = "local", # openai, deepseek, local
    proxy = list(),
    openai_api_endpoints = list(
      chat_completions = "https://api.openai.com/v1/chat/completions",
      completions = "https://api.openai.com/v1/completions",
      files = "https://api.openai.com/v1/files",
      models = "https://api.openai.com/v1/models",
      fine_tuning_jobs = "https://api.openai.com/v1/fine_tuning/jobs",
      embeddings = "https://api.openai.com/v1/embeddings",
      audio = "https://api.openai.com/v1/audio",
      images = "https://api.openai.com/v1/images",
      assistants = "https://api.openai.com/v1/assistants",
      threads = "https://api.openai.com/v1/threads",
      moderations = "https://api.openai.com/v1/moderations",
      batchs = "https://api.openai.com/v1/batches",
      vector_stores = "https://api.openai.com/v1/vector_stores"
    ),
    deepseek_api_endpoints = list(
      chat_completions = "https://api.deepseek.com/v1/chat/completions",
      completions = "https://api.deepseek.com/v1/completions",
      files = "https://api.deepseek.com/v1/files",
      models = "https://api.deepseek.com/v1/models",
      fine_tuning_jobs = "https://api.deepseek.com/v1/fine_tuning/jobs",
      embeddings = "https://api.deepseek.com/v1/embeddings",
      audio = "https://api.deepseek.com/v1/audio",
      images = "https://api.deepseek.com/v1/images",
      assistants = "https://api.deepseek.com/v1/assistants",
      threads = "https://api.deepseek.com/v1/threads",
      moderations = "https://api.deepseek.com/v1/moderations",
      batchs = "https://api.deepseek.com/v1/batches",
      vector_stores = "https://api.deepseek.com/v1/vector_stores"
    ),
    local_api_endpoints = list(
      chat_completions = "http://localhost:4891/v1/chat/completions",
      completions = "http://localhost:4891/v1/completions"
    )
  ),
  public = list(
    #' @description Initialize the api_config object
    initialize = function() {
    },
    #' @description Configure the api_key settings.
    #' @param api_key your openai_key
    set_api_key = function(api_key){
      private$api_key <- api_key
    },
    #' @description Configure the proxy settings.
    #' @param proxy_ip character Required. The IP address of the proxy.
    #' @param proxy_port character Required. The port number of the proxy.
    set_proxy = function(proxy_ip,proxy_port){
      private$proxy$ip <- proxy_ip
      private$proxy$port <- proxy_port
    },
    #' @description Configure the proxy gettings.
    get_proxy = function(){
      private$proxy
    },
    #' @description Api key gettings.
    get_api_key = function(){
      private$api_key
    },
    #' @description Endpoints gettings.
    get_api_endpoints = function(){
      switch(private$targetAi,
             "openai" = private$openai_api_endpoints,
             "deepseek" = private$deepseek_api_endpoints,
             "local" = private$local_api_endpoints)
    },
    set_targetAi = function(targetAi) {
      if (!targetAi %in% c("openai", "deepseek", "local")) {
        stop("Invalid targetAi. Choose from 'openai', 'deepseek', or 'local'.")
      }
      private$targetAi <- targetAi
    },
    get_targetAi = function() {
      private$targetAi
    }
  )
)
