#' An R6 Class Interface to OpenAI API
#'
#' @description Provides methods to interact with OpenAI API including
#'   fetching model details, generating completions, managing files, and more.
#'   Always ensure that the API key is kept private.
#' @examples
#' if(file.exists("openai-key")){
#'   key <- readLines(file("openai-key"))
#'   #NOTE To retrieve the correct data, it is necessary to provide an 'openaikey';
#'   # otherwise, an exception will be returned.
#' }else{
#'   key <- "you_api_key"
#' }
#' aaa <- openai$new(key)
#' #if need proxy
#' #aaa$set_proxy("127.0.0.1", 10890)
#' # List model
#' aaa$get_model_list()
#' \donttest{
#' #The following request will cause a timeout when the correct key is not present,
#' # so it is not necessary to test it during the check.
#' # Upload a file
#' train_file_path<-system.file("exdata","train.jsonl", package = "openaistream")
#' file_id <- aaa$files_upload(path = train_file_path,purpose = "fine-tune")
#' # Retrieve a file's details
#' aaa$files_retrieve(NULL, verbosity = 0)
#' # Delete a file
#' #aaa$files_delete(file_id$id, verbosity = 0)
#' # Get a list of files
#' aaa$files_get_list()
#' #aaa
#' job<-aaa$fine_tuning_jobs_create(model = "gpt-3.5-turbo",training_file = file_id$id)
#' aaa$files_delete(file_id$id, verbosity = 0)
#' # List fine-tuning jobs
#' aaa$fine_tuning_jobs_list(limit = 2, verbosity = 3)
#' # Retrieve details of a specific fine-tuning job
#' aaa$fine_tuning_jobs_retrieve(job$id, verbosity = 0)
#' # Get events related to a specific fine-tuning job
#' aaa$fine_tuning_jobs_events(job$id, verbosity = 3)
#' # Compute embeddings for input data
#' aaa$embeddings(model = "text-embedding-ada-002", input = "who are you?")
#' # chat test not stream
#' streamlg <- aaa$get_completions_query(
#'   prompt = "Please explain the World Wars?",
#'   model = "davinci-002",
#'   stream = FALSE,
#'   max_tokens = 20,
#'   num = 4,
#'   verbosity = 0
#' )
#'
#' # chat test stream
#' streamlg <- aaa$get_chat_completions_query(
#'   messages = data.frame(role = c("system", "user"),
#'                         content = c("You are a assistant.", "How's the weather today?")),
#'   model = "gpt-3.5-turbo",
#'   stream = TRUE,
#'   max_tokens = 10
#' )
#'
#' streamlg$get_state()
#' streamlg$next_value
#' streamlg$close()
#' streamlg$next_value
#' streamlg$get_state()
#' sss<-aaa$audio_speech(input = "Hi, this is a voice transmission test.",response_format="mp3")
#' #writeBin(sss,"test.mp3")
#'
#' streammp3<-aaa$audio_speech(input = "Hi, this is a voice transmission test",
#'               response_format="mp3",stream = TRUE,num=1000)
#' streammp3$get_state()
#' #fcon <- file("yourfile.mp3", "wb")
#' #for(i in 1:3){
#' #   writeBin(streammp3$next_value,fcon)
#' #}
#' streammp3$close()
#' streammp3$next_value
#' streammp3$get_state()
#'
#' #text_E1<-aaa$audio_transcriptions(path = "test.mp3")
#' #text_E2<-aaa$audio_translations(path = "test.mp3")
#' pic1<-aaa$images_generations(prompt = "A small bird flies over the ocean")
#' #req <- request(pic1$data$url) %>%req_perform(path = "pic1.png")
#' #Here is the image test.
#' #For simple test,you need install 'png' package.
#' #Note that when a mask is not provided, OpenAI defaults to using areas of the uploaded image
#' #  with a transparency of 0 as the modification area.
#' #The following test adds full image transparency of 0 (modifying the entire image) to pic1.
#' # pic1<-readPNG("pic1.png")
#' # if (dim(pic1)[3] == 3) {  # Checking if the image is RGB
#' #   # Adding an alpha channel (fully opaque)
#' #   alpha_channel <- matrix(0, nrow = dim(pic1)[1], ncol = dim(pic1)[2])
#' #   pic1_b<-array(c(pic1,alpha_channel), c(1024,1024,4))
#' # }
#' # writePNG(pic1_b,target = "pic1_b.png")
#' # pic3<-aaa$images_edits(image = "pic1_b.png",
#' # prompt = "Please modify the bird in the picture to a flock of birds."
#' #  ,verbosity = 3,n="2",size="256x256")
#' # request(pic3$data$url[1]) %>%req_perform(path = "pic3_1.png")
#' # request(pic3$data$url[2]) %>%req_perform(path = "pic3_2.png")
#' #
#' # pic4<-aaa$images_variations(image = "pic1.png",n="2",size="256x256")
#' # request(pic4$data$url[1]) %>%req_perform(path = "pic4_1.png")
#' # request(pic4$data$url[2]) %>%req_perform(path = "pic4_2.png")
#' }
#' @export
openai <- R6Class(
  "openai",
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
    ),
    #base_call
    base_call = function(endpoint, path="", body=NULL, method="GET", headers=list(), query=list()){
      url <- private$api_endpoints[[endpoint]]
      req <- request(url) %>%
        req_headers(Authorization = paste0("Bearer ", private$api_key),!!!headers) %>%
        req_method(method)
      if (path != "") {
        req <- req %>% req_url_path_append(strsplit(path, "/")[[1]])
      }
      if (length(query) > 0) {
        req <- req %>%req_url_query(!!!query)
      }

      if(length(private$proxy) == 2) {
        req <- req %>%
          req_proxy(private$proxy$ip, private$proxy$port)
      }
      req
    },
    #get and post and delete call
    api_call = function(endpoint, path="", body=NULL, method="GET", headers=list(), query=list(), verbosity=0,fpath=NULL) {
      req<-private$base_call(endpoint, path=path, method=method, headers=headers, query=query)
      #browser()
      if (!is.null(body)) {
        req <- req %>%req_body_json(body)
      }
      tryCatch({
        parsed <- req %>% req_perform(verbosity = verbosity,path=fpath)
        if(path=="/speech"){
          return(list(success=TRUE, data=parsed$body))
        }else{
          return(list(success=TRUE, data=fromJSON(rawToChar(parsed$body))))
        }
      }, error=function(e) {
        return(openai_error$new(as.character(e)))
      })
    },
    #stream call
    handle_call=function(endpoint, path="", body=NULL, headers=list(), query=list()){
      req<-private$base_call(endpoint, path=path, method="POST", headers=headers, query=query)
      if (!is.null(body)) {
        req <- req %>%req_body_json(body)
      }else{
        return(list(success=FALSE, data="not find body"))
      }
      return(curl::curl(req$url, handle = httr2:::req_handle(req)))
    },
    #file call
    file_call=function(endpoint, path="", body=NULL, headers=list(), query=list(), verbosity=0){
      req<-private$base_call(endpoint, path=path, method="POST", headers=headers, query=query)
      if (!is.null(body)) {
        req<-req %>% req_body_multipart(!!!body)
      }else{
        return(list(success=FALSE, data="not find body"))
      }
      tryCatch({
        parsed <- req %>% req_perform(verbosity = verbosity)
        return(list(success=TRUE, data=fromJSON(rawToChar(parsed$body))))
      }, error=function(e) {
        return(openai_error$new(as.character(e)))
      })
    }
  ),
  public = list(
    #' @description Initialize the OpenAI API interface with the provided API key.
    #' @param api_key The OpenAI API key.
    initialize = function(api_key) {
      private$api_key<-api_key
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
    #' @description Lists the currently available models, and provides basic information
    #'              about each one such as the owner and availability.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return A list of model objects.
    get_model_list=function(verbosity=0){
      result <- private$api_call("models", headers = list(`Content-Type` = "application/json"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data$data)
      }
    },
    #' @description Retrieves a model instance, providing basic information about
    #'              the model such as the owner and permissioning.
    #' @param model character Required. The ID of the model to use for this request
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return The model object matching the specified ID.
    get_model_retrieve=function(model,verbosity=0){
      result <- private$api_call("models", paste0("/", model), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data$data)
      }
    },
    #' @description Delete a fine-tuned model. You must have the Owner role in
    #'              your organization to delete a model.
    #' @param model character Required. The model to delete
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return Deletion status.
    fine_tuning_jobs_delete=function(model,verbosity=0){
      result <- private$api_call("models", paste0("/", model), method = "DELETE", verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Creates a completion for the provided prompt and parameters.
    #' @param prompt character Required. The prompt(s) to generate completions for, encoded as a string,
    #'               array of strings, array of tokens, or array of token arrays. Note that <|endoftext|> is
    #'               the document separator that the model sees during training, so if a prompt is not specified
    #'               the model will generate as if from the beginning of a new document.
    #' @param model character Required. The model to use for generating completions.
    #' @param stream logical. Whether to stream back partial progress. If set, tokens will be sent
    #'              as data-only server-sent events as they become available.
    #' @param n integer. How many chat completion choices to generate for each input message. Note that you will be
    #'           charged based on the number of generated tokens across all of the choices. Keep n as 1 to minimize costs.
    #' @param num The num parameter controls the number of text entries returned by a stream in one go.
    #'            Note that this is different from the n parameter, which specifies the number of results returned.
    #'            For detailed information on the n parameter, please refer to OpenAI's API documentation.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ... Additional parameters as required by the OpenAI API.For example:max_tokens;n;stop;temperature......
    #' @return Returns a completion object, or a sequence of completion objects if the request is streamed.
    get_completions_query=function(prompt,model,stream=F,n=1,num=2,verbosity = 0,...){
      option = list(...)
      option$prompt = prompt
      option$model = model
      option$stream = stream
      option$n = n
      if (option$stream) {
        handle <- private$handle_call("completions", body=option, headers=list(Accept="text/event-stream", `Content-Type` = "application/json"))
        return(DataStream$new(requery = handle, num = num))
      } else {
        result <- private$api_call("completions", body=option, headers=list(`Content-Type` = "application/json"),method = "POST", verbosity=verbosity)
        if (inherits(result, "openai_error")) {
          return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
        }else{
          return(list(all_resp=result$data, vres = result$data$choices))
        }
      }
    },
    #' @description Creates a model response for the given chat conversation.
    #' @param messages list Required. A list of messages comprising the conversation so far.
    #' @param model character Required. The model to use for generating chat completions.
    #' @param stream logical. Whether to stream back partial progress. If set, tokens will be sent
    #'              as data-only server-sent events as they become available.
    #' @param n integer. How many chat completion choices to generate for each input message. Note that you will be
    #'           charged based on the number of generated tokens across all of the choices. Keep n as 1 to minimize costs.
    #' @param num The num parameter controls the number of text entries returned by a stream in one go.
    #'            Note that this is different from the n parameter, which specifies the number of results returned.
    #'            For detailed information on the n parameter, please refer to OpenAI's API documentation.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ... Additional parameters as required by the OpenAI API.For example:max_tokens;n;stop;temperature......
    #' @return Returns a chat completion object, or a streamed sequence of chat completion chunk objects if the request is streamed.
    get_chat_completions_query=function(messages,model,stream=F,n=1,num=2,verbosity = 0,...){
      option = list(...)
      option$messages = messages
      option$model = model
      option$stream = stream
      option$n = n
      if (option$stream) {
        handle <- private$handle_call("chat_completions", body=option, headers=list(Accept="text/event-stream", `Content-Type` = "application/json"))
        return(DataStream$new(requery = handle , num = num))
      } else {
        result <- private$api_call("chat_completions", body=option, headers=list(`Content-Type` = "application/json"),method = "POST", verbosity=verbosity)
        if (inherits(result, "openai_error")) {
          return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
        }else{
          return(list(all_resp=result$data, vres = result$data$choices))
        }
      }
    },
    #' @description Upload a file that can be used across various endpoints. The size of all the
    #'              files uploaded by one organization can be up to 100 GB.The size of individual
    #'              files can be a maximum of 512 MB or 2 million tokens for Assistants.
    #'              See the Assistants Tools guide to learn more about the types of files supported.
    #'              The Fine-tuning API only supports .jsonl files.
    #' @param path character Required. Path to the file that needs to be uploaded.
    #' @param purpose The intended purpose of the uploaded file.
    #'                Use "fine-tune" for Fine-tuning and "assistants" for Assistants and Messages.
    #'                This allows us to validate the format of the uploaded file is correct for fine-tuning.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return The uploaded File object.
    files_upload=function(path,verbosity=0,purpose){
      result<-private$file_call(endpoint = "files",body = list(file = curl::form_file(path),purpose=purpose),verbosity=verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Returns a list of files that belong to the user's organization.
    #' @param ... Additional parameters as required by the OpenAI API.For example:purpose
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return A list of File objects.
    files_get_list=function(...,verbosity=0){
      option <- list(...)
      result <- private$api_call("files",query = option, verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Delete a file.
    #' @param file_id character Required. The ID of the file to use for this request.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return Deletion status.
    files_delete=function(file_id,verbosity=0){
      result <- private$api_call("files", paste0("/", file_id), method = "DELETE", verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Returns information about a specific file.
    #' @param file_id character Required. The ID of the file to retrieve details for.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return The File object matching the specified ID.
    files_retrieve=function(file_id,verbosity=0){
      result <- private$api_call("files", paste0("/", file_id), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Creates a job that fine-tunes a specified model from a given dataset.
    #'              Response includes details of the enqueued job including job status and
    #'              the name of the fine-tuned models once complete.
    #' @param model character Required. The model ID.
    #' @param training_file character Required. The file used for training.
    #' @param hyperparameters list. The hyperparameters used for the fine-tuning job. include batch_size;learning_rate_multiplier;n_epochs.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ... Additional parameters as required by the OpenAI API.For example:validation_file......
    #' @return Response indicating the success or failure of the fine-tuning job creation.
    fine_tuning_jobs_create=function(model,training_file,hyperparameters=list(n_epochs=1),...,verbosity=0){
      option <- list(...)
      option$model <- model
      option$training_file <- training_file
      option$hyperparameters <- hyperparameters
      result <- private$api_call("fine_tuning_jobs", body = option, method = "POST", headers = list(`Content-Type` = "application/json"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description List your organization's fine-tuning jobs
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ... Additional parameters as required by the OpenAI API. For example:after,limit.
    #' @return A list of paginated fine-tuning job objects.
    fine_tuning_jobs_list=function(verbosity=0,...){
      option = list(...)
      result <- private$api_call("fine_tuning_jobs", query = option, verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Get info about a fine-tuning job.
    #' @param job_id character Required. The ID of the fine-tuning job.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return The fine-tuning object with the given ID.
    fine_tuning_jobs_retrieve=function(job_id,verbosity=0){
      result <- private$api_call("fine_tuning_jobs", paste0("/", job_id), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Immediately cancel a fine-tune job.
    #' @param job_id character Required. The ID of the fine-tuning job to cancel.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return The cancelled fine-tuning object.
    fine_tuning_jobs_cancel=function(job_id,verbosity=0){
      result <- private$api_call("fine_tuning_jobs", paste0("/", job_id, "/", "cancel"), method = "POST", verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Get status updates for a fine-tuning job.
    #' @param job_id character Required. The ID of the fine-tuning job to get events for.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ... Additional parameters as required by the OpenAI API. For example:after,limit.
    #' @return A list of fine-tuning event objects.
    fine_tuning_jobs_events=function(job_id,...,verbosity=0){
      option = list(...)
      result <- private$api_call("fine_tuning_jobs", query = option, paste0("/", job_id, "/", "events"),method = "GET", verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Creates an embedding vector representing the input text.
    #' @param model character Required. ID of the model to use. You can use
    #'              the List models API to see all of your available models,
    #'              or see our Model overview for descriptions of them.
    #' @param input character Required. Input text to embed, encoded as a string or array of tokens.
    #'              To embed multiple inputs in a single request, pass an array of strings or array of token arrays.
    #'              The input must not exceed the max input tokens for
    #'              the model (8192 tokens for text-embedding-ada-002), cannot be an empty string,
    #'              and any array must be 2048 dimensions or less.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ... Additional parameters as required by the OpenAI API.For example:encoding_format;user....
    #' @return Embeddings for the input data.
    embeddings=function(model,input,...,verbosity=0){
      option <- list(...)
      option$model <- model
      option$input <- input
      result <- private$api_call("embeddings", body = option,method = "POST", headers = list(`Content-Type` = "application/json"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Generates audio from the input text.
    #' @param model character Required. One of the available TTS models: tts-1 or tts-1-hd
    #' @param input character Required. The text to generate audio for. The maximum length is 4096 characters.
    #' @param voice character Required. The voice to use when generating the audio. Supported voices are alloy, echo, fable, onyx, nova, and shimmer.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param stream logical. Using the stream call, it will return raw data of the specified length,
    #'               which can be saved in the set format such as mp3, etc. For details, please see the examples.
    #' @param num The num parameter controls the number of raw entries returned by a stream in one go.
    #'            Note that this is different from the n parameter, which specifies the number of results returned.
    #'            For detailed information on the n parameter, please refer to OpenAI's API documentation.
    #' @param ... Additional parameters as required by the OpenAI API.For example:response_format;speed....
    #' @return The audio file content.
    audio_speech=function(model="tts-1",input,voice="alloy",stream=F,num=100,...,verbosity=0){
      option <- list(...)
      option$model <- model
      option$input <- input
      option$voice <- voice
      if (stream) {
        handle <- private$handle_call("audio", body=option,path="/speech", headers=list(Accept="text/event-stream", `Content-Type` = "application/json"))
        return(DataStream$new(requery = handle , num = num))
      } else {
        result <- private$api_call("audio", body = option,"/speech",method = "POST", headers = list(`Content-Type` = "application/json"), verbosity = verbosity)
        if (inherits(result, "openai_error")) {
          return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
        }else{
          return(result$data)
        }
      }
    },
    #' @description Transcribes audio into the input language.
    #' @param path character Required. The audio file object (not file name) to transcribe,
    #'             in one of these formats: flac, mp3, mp4, mpeg, mpga, m4a, ogg, wav, or webm.
    #' @param model character Required. ID of the model to use. Only whisper-1 is currently available.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ... Additional parameters as required by the OpenAI API.For example:language;prompt;response_format;temperature....
    #' @return The transcribed text.
    audio_transcriptions=function(path,model="whisper-1",...,verbosity=0){
      option <- list(...)
      option$model <- model
      option$file <- curl::form_file(path)
      result<-private$file_call(endpoint = "audio",path = "/transcriptions",body = option,verbosity=verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Translates audio into English.
    #' @param path character Required. The audio file object (not file name) to transcribe,
    #'             in one of these formats: flac, mp3, mp4, mpeg, mpga, m4a, ogg, wav, or webm.
    #' @param model character Required. ID of the model to use. Only whisper-1 is currently available.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ... Additional parameters as required by the OpenAI API.For example:prompt;response_format;temperature....
    #' @return The transcribed text.
    audio_translations=function(path,model="whisper-1",...,verbosity=0){
      option <- list(...)
      option$model <- model
      option$file <- curl::form_file(path)
      result<-private$file_call(endpoint = "audio",path = "/translations",body = option,verbosity=verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Creates an image given a prompt.
    #' @param prompt character Required. A text description of the desired image(s).
    #'               The maximum length is 1000 characters for dall-e-2 and 4000 characters for dall-e-3
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ...  Additional parameters as required by the OpenAI API.For example:n;quality;response_format...
    #' @return Returns a list of image objects.
    images_generations=function(prompt,...,verbosity=0){
      option <- list(...)
      option$prompt <- prompt
      result<-private$api_call(endpoint = "images",path = "/generations", headers = list(`Content-Type` = "application/json"),method = "POST",body = option,verbosity=verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Creates an edited or extended image given an original image and a prompt.
    #' @param image character Required. The image to edit. Must be a valid PNG file, less than 4MB,
    #'              and square. If mask is not provided, image must have transparency, which will be used as the mask.
    #' @param prompt character Required. A text description of the desired image(s). The maximum length is 1000 characters.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ...  Additional parameters as required by the OpenAI API.For example:mask;model;n...
    #' @return Returns a list of image objects.
    images_edits=function(image,prompt,...,verbosity=0){
      option <- list(...)
      option$prompt <- prompt
      option$image<-curl::form_file(image)
      if(!is.null(option$mask)){
        option$mask<-curl::form_file(option$mask)
      }
      result<-private$file_call(endpoint = "images",path = "/edits",body = option,verbosity=verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Creates a variation of a given image.
    #' @param image character Required. The image to use as the basis for the variation(s). Must be a valid PNG file, less than 4MB, and square.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ...  Additional parameters as required by the OpenAI API.For example:model;n;response_format
    #' @return Returns a list of image objects.
    images_variations=function(image,...,verbosity=0){
      option <- list(...)
      option$image<-curl::form_file(image)
      result<-private$file_call(endpoint = "images",path = "/variations",body = option,verbosity=verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Create an assistant with a model and instructions.
    #' @param model character Required. ID of the model to use. You can use the List models API to see all of your available models,
    #'             or see our Model overview for descriptions of them.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ...  Additional parameters as required by the OpenAI API.For example:name;description;instructions;tools;file_ids;metadata
    #' @return An assistant object.
    assistants_create=function(model,...,verbosity=0){
      option <- list(...)
      option$model <- model
      result <- private$api_call("assistants", body = option,method = "POST", headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Retrieves an assistant.
    #' @param assistant_id character Required. The ID of the assistant to retrieve.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return The assistant object matching the specified ID.
    assistants_retrieve=function(assistant_id,verbosity=0){
      result <- private$api_call("assistants",path=paste0("/",assistant_id), body = option,method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Modifies an assistant.
    #' @param assistant_id character Required. The ID of the assistant to modify.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ...  Additional parameters as required by the OpenAI API.For example:model;name;description;instructions;tools;file_ids,metadata
    #' @return The assistant object matching the specified ID.
    assistants_modify=function(assistant_id,...,verbosity=0){
      option <- list(...)
      result <- private$api_call("assistants",path=paste0("/",assistant_id), body = option,method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Delete an assistant.
    #' @param assistant_id character Required. The ID of the assistant to delete.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return Deletion status
    assistants_delete=function(assistant_id,verbosity=0){
      result <- private$api_call("assistants", paste0("/", assistant_id), method = "DELETE",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Returns a list of assistants
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ...  Additional parameters as required by the OpenAI API.For example:limit;order;after;before;
    #' @return A list of assistant objects.
    assistants_list=function(...,verbosity=0){
      option <- list(...)
      result <- private$api_call("assistants",query = option,method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Create an assistant file by attaching a File to an assistant.
    #' @param assistant_id character Required. The ID of the assistant for which to create a File.
    #' @param file_id character Required. A File ID (with purpose="assistants") that the assistant should use.
    #'                Useful for tools like retrieval and code_interpreter that can access files.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return An assistant file object.
    assistants_file_create=function(assistant_id,file_id,verbosity=0){
      option <- list(file_id=file_id)
      result<-private$api_call("assistants", paste0("/", assistant_id,"/files"),body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Retrieves an AssistantFile.
    #' @param assistant_id character Required. The ID of the assistant who the file belongs to.
    #' @param file_id character Required. The ID of the file we're getting.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return The assistant file object matching the specified ID.
    assistants_file_retrieve=function(assistant_id,file_id,verbosity=0){
      result <- private$api_call("assistants", paste0("/", assistant_id,"/files/",file_id),headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Delete an assistant file.
    #' @param assistant_id character Required. The ID of the assistant who the file belongs to.
    #' @param file_id character Required. The ID of the file we're getting.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return Deletion status
    assistants_file_delete=function(assistant_id,file_id,verbosity=0){
      result <- private$api_call("assistants", paste0("/", assistant_id,"/files/",file_id), method = "DELETE",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Retrieve a list of files from OpenAI.
    #' @param assistant_id character Required. The ID of the assistant who the file belongs to.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ...  Additional parameters as required by the OpenAI API.For example:limit,order,after,before
    #' @return A list of files.
    assistants_file_list=function(assistant_id,...,verbosity=0){
      result <- private$api_call("assistants", paste0("/", assistant_id,"/files"), method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Create a thread.
    #' @param ... Additional parameters as required by the OpenAI API.For example:messages;name;metadata
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return A thread object.
    threads_create=function(...,verbosity=0){
      option <- list(...)
      result<-private$api_call("threads",body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Retrieves a thread.
    #' @param thread_id character Required. The ID of the thread to retrieve.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The thread object matching the specified ID.
    threads_retrieve=function(thread_id,verbosity=0){
      result <- private$api_call("threads", paste0("/", thread_id), method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Modifies a thread.
    #' @param thread_id The ID of the thread to modify. Only the metadata can be modified.
    #' @param ... Additional parameters as required by the OpenAI API.For example:metadata
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The modified thread object matching the specified ID.
    threads_modify=function(thread_id,...,verbosity=0){
      option <- list(...)
      result <- private$api_call("threads", paste0("/", thread_id),body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Delete a thread.
    #' @param thread_id character Required The ID of the thread to delete.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return Deletion status.
    threads_delete=function(thread_id,verbosity=0){
      result <- private$api_call("threads", paste0("/", thread_id), method = "DELETE",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Create a message.
    #' @param thread_id character Required. The ID of the thread to create a message for.
    #' @param role character Required. The role of the entity that is creating the message. Currently only user is supported.
    #' @param content character Required. The content of the message.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @param ...  Additional parameters as required by the OpenAI API. For example:file_ids,metadata
    #' @return A message object.
    messages_create=function(thread_id,role,content,...,verbosity=0){
      option <- list(...)
      option$role <- role
      option$content <- content
      result<-private$api_call("threads", paste0("/", thread_id,"/messages"),body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Retrieve a message.
    #' @param thread_id character Required. The ID of the thread the message belongs to.
    #' @param message_id character Required. The ID of the message to retrieve.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The message object matching the specified ID.
    messages_retrieve=function(thread_id,message_id,verbosity=0){
      result <- private$api_call("threads", paste0("/", thread_id,"/messages/",message_id), method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Modifies a message.
    #' @param thread_id character Required. The ID of the thread the message belongs to.
    #' @param message_id character Required. The ID of the message to retrieve.
    #' @param ... Additional parameters as required by the OpenAI API. For example:metadata
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The modified message object.
    messages_modify=function(thread_id,message_id,...,verbosity=0){
      option <- list(...)
      result <- private$api_call("threads", paste0("/", thread_id,"/messages/",message_id),body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Returns a list of messages for a given thread.
    #' @param thread_id character Required. The ID of the thread the messages belong to.
    #' @param ... Additional parameters as required by the OpenAI API.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return A list of message objects.
    messages_list=function(thread_id,...,verbosity=0){
      option <- list(...)
      result <- private$api_call("threads", paste0("/", thread_id,"/messages"),query = option, method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Retrieves a message file.
    #' @param thread_id character Required. The ID of the thread the message belongs to.
    #' @param message_id character Required. The ID of the message the file belongs to.
    #' @param file_id character Required The ID of the file being retrieved.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #' @return The message file object.
    messages_file_retrieve=function(thread_id,message_id,file_id,verbosity=0){
      result <- private$api_call("threads", paste0("/", thread_id,"/messages/",message_id,"/files/",file_id), method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Returns a list of message files.
    #' @param thread_id character Required. The ID of the thread the message belongs to.
    #' @param message_id character Required. The ID of the message the file belongs to.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #' @param ... Additional parameters as required by the OpenAI API.
    #' @return A list of message file objects.
    messages_file_list=function(thread_id,message_id,...,verbosity=0){
      option <- list(...)
      result <- private$api_call("threads", paste0("/", thread_id,"/messages/",message_id,"/files"),query = option, method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Create a run.
    #' @param thread_id character Required. The ID of the thread to run.
    #' @param assistant_id character Required. The ID of the assistant to use to execute this run.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @param ... Additional parameters as required by the OpenAI API.For example:model,instructions,tools,metadata
    #' @return A run object.
    runs_create=function(thread_id,assistant_id,...,verbosity=0){
      option <- list(...)
      option$assistant_id <- assistant_id
      result<-private$api_call("threads", paste0("/", thread_id,"/runs"),body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Retrieves a run.
    #' @param thread_id character Required The ID of the thread the run belongs to.
    #' @param run_id character Required The ID of the run to retrieve.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The run object matching the specified ID.
    runs_retrieve=function(thread_id,run_id,verbosity=0){
      result <- private$api_call("threads", paste0("/", thread_id,"/runs/",run_id), method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Modifies a run.
    #' @param thread_id character Required The ID of the thread the run belongs to.
    #' @param run_id character Required The ID of the run to retrieve.
    #' @param ... Additional parameters as required by the OpenAI API.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The modified run object matching the specified ID.
    runs_modify=function(thread_id,run_id,...,verbosity=0){
      option <- list(...)
      result <- private$api_call("threads", paste0("/", thread_id,"/runs/",run_id),body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Returns a list of runs for a given thread.
    #' @param thread_id character Required The ID of the thread the run belongs to.
    #' @param ... Additional parameters as required by the OpenAI API.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return A list of run objects.
    runs_list=function(thread_id,...,verbosity=0){
      option <- list(...)
      result <- private$api_call("threads", paste0("/", thread_id,"/runs"),query = option, method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description When a run has the status: "requires_action" and required_action.type is submit_tool_outputs,
    #'              this endpoint can be used to submit the outputs from the tool calls once they're all completed.
    #'              All outputs must be submitted in a single request.
    #' @param thread_id character Required The ID of the thread the run belongs to.
    #' @param run_id character Required The ID of the run to retrieve.
    #' @param tool_outputs character Required. A list of tools for which the outputs are being submitted.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The modified run object matching the specified ID.
    runs_submit_tool_outputs=function(thread_id,run_id,tool_outputs,verbosity=0){
      option <- list(tool_outputs=tool_outputs)
      result <- private$api_call("threads", paste0("/", thread_id,"/runs/",run_id,"/submit_tool_outputs"),body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Cancels a run that is in_progress.
    #' @param thread_id character Required The ID of the thread the run belongs to.
    #' @param run_id character Required The ID of the run to retrieve.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The modified run object matching the specified ID.
    runs_cancel=function(thread_id,run_id,verbosity=0){
      result <- private$api_call("threads", paste0("/", thread_id,"/runs/",run_id,"/cancel"), method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Create a thread and run it in one request.
    #' @param assistant_id character Required The ID of the assistant to use to execute this run.
    #' @param ... Additional parameters as required by the OpenAI API.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return A run object.
    runs_create_tread=function(assistant_id,...,verbosity=0){
      option <- list(...)
      option$assistant_id <- assistant_id
      result<-private$api_call("threads",path="/runs",body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Retrieves a run step.
    #' @param thread_id character Required. The ID of the thread to which the run and run step belongs.
    #' @param run_id character Required. The ID of the run the step belongs to.
    #' @param step_id character Required. The ID of the step to retrieve.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The run step object matching the specified ID.
    runs_steps_retrieve=function(thread_id,run_id,step_id,verbosity=0){
      result <- private$api_call("threads", paste0("/", thread_id,"/runs/",run_id,"/steps/",step_id), method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Returns a list of run steps belonging to a run.
    #' @param thread_id character Required The ID of the thread the run belongs to.
    #' @param run_id character Required The ID of the run the step belongs to.
    #' @param ... Additional parameters as required by the OpenAI API.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return A list of run step objects.
    runs_steps_list=function(thread_id,run_id,...,verbosity=0){
      option <- list(...)
      result <- private$api_call("threads", paste0("/", thread_id,"/runs/",run_id,"/steps"),query = option, method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v1"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    }
  )
)

