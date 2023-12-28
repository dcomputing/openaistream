test_that("initialize sets the api_key correctly", {
  api_key <- "my_api_key"
  openai <- initialize(api_key)
  expect_equal(openai$api_key, api_key)
})

test_that("set_proxy sets the proxy IP and port correctly", {
  openai <- initialize("my_api_key")
  proxy_ip <- "192.168.0.1"
  proxy_port <- 8080
  openai$set_proxy(proxy_ip, proxy_port)
  expect_equal(openai$private$proxy$ip, proxy_ip)
  expect_equal(openai$private$proxy$port, proxy_port)
})
#
# test_that("set_proxy throws an error for invalid proxy IP address", {
#   openai <- initialize("my_api_key")
#   proxy_ip <- "invalid_ip_address"
#   proxy_port <- 8080
#   expect_error(openai$set_proxy(proxy_ip, proxy_port), "Invalid proxy IP address.")
# })
#
# test_that("set_proxy throws an error for invalid proxy port number", {
#   openai <- initialize("my_api_key")
#   proxy_ip <- "192.168.0.1"
#   proxy_port <- "invalid_port_number"
#   expect_error(openai$set_proxy(proxy_ip, proxy_port), "Invalid proxy port number.")
# })test_that("get_model_list returns a list of model objects", {
#   openai <- initialize("my_api_key")
#   model_list <- openai$get_model_list()
#   expect_is(model_list, "list")
#   expect_true(all(sapply(model_list, is.list)))
# })
#
# test_that("get_model_retrieve returns the model object matching the specified ID", {
#   openai <- initialize("my_api_key")
#   model_id <- "gpt-3.5-turbo"
#   model <- openai$get_model_retrieve(model_id)
#   expect_is(model, "list")
#   expect_equal(model$id, model_id)
# })test_that("fine_tuning_jobs_delete deletes the specified model", {
#   model <- "my_model"
#   expected_url <- "/models/my_model"
#   expected_method <- "DELETE"
#   expected_verbosity <- 0
#
#   # Mock the api_call function
#   private$api_call <- function(url, method, verbosity) {
#     expect_equal(url, expected_url)
#     expect_equal(method, expected_method)
#     expect_equal(verbosity, expected_verbosity)
#     return(list(success=TRUE, message=NULL, type=NULL))
#   }
#
#   result <- fine_tuning_jobs_delete(model)
#
#   expect_true(result$success)
#   expect_is(result$message, "NULL")
#   expect_is(result$type, "NULL")
# })test_that("get_completions_query returns the correct completion object", {
#   prompt <- "This is a test prompt."
#   model <- "gpt-3.5-turbo"
#   completions <- get_completions_query(prompt, model)
#   expect_true(is.list(completions))
#   expect_true("all_resp" %in% names(completions))
#   expect_true("vres" %in% names(completions))
# })
#
# test_that("get_completions_query returns a stream when stream parameter is set to TRUE", {
#   prompt <- "This is a test prompt."
#   model <- "gpt-3.5-turbo"
#   completions <- get_completions_query(prompt, model, stream = TRUE)
#   expect_true(inherits(completions, "DataStream"))
# })
#
# test_that("get_completions_query returns an error message when API call fails", {
#   prompt <- "This is a test prompt."
#   model <- "gpt-3.5-turbo"
#   # Simulate a failed API call by passing an invalid model
#   completions <- get_completions_query(prompt, "invalid_model")
#   expect_true(is.list(completions))
#   expect_true("success" %in% names(completions))
#   expect_false(completions$success)
#   expect_true("message" %in% names(completions))
#   expect_true("type" %in% names(completions))
# })test_that("get_chat_completions_query returns the expected output", {
#   # Test case 1
#   messages <- list(
#     list(role = "system", content = "You are a helpful assistant."),
#     list(role = "user", content = "What's the weather like today?")
#   )
#   model <- "gpt-3.5-turbo"
#   completions <- get_chat_completions_query(messages, model)
#   expect_true(is.list(completions))
#   expect_true(length(completions) > 0)
#
#   # Test case 2
#   messages <- list(
#     list(role = "system", content = "You are a helpful assistant."),
#     list(role = "user", content = "Tell me a joke.")
#   )
#   model <- "gpt-3.5-turbo"
#   completions <- get_chat_completions_query(messages, model, n = 3)
#   expect_true(is.list(completions))
#   expect_true(length(completions) > 0)
#   expect_equal(length(completions), 3)
#
#   # Test case 3
#   messages <- list(
#     list(role = "system", content = "You are a helpful assistant."),
#     list(role = "user", content = "Translate 'hello' to French.")
#   )
#   model <- "gpt-3.5-turbo"
#   completions <- get_chat_completions_query(messages, model, stream = TRUE)
#   expect_true(is(DataStream, completions))
#   expect_true(completions$num > 0)
# })test_that("files_upload uploads the file correctly", {
#   # Set up
#   path <- "/path/to/file.txt"
#   purpose <- "fine-tune"
#
#   # Call the function
#   result <- files_upload(path, purpose = purpose)
#
#   # Check the result
#   expect_true(result$success)
#   expect_equal(result$message, NULL)
#   expect_equal(result$type, NULL)
# })test_that("files_get_list returns a list of files", {
#   openai <- initialize("my_api_key")
#   files <- openai$files_get_list()
#   expect_is(files, "list")
# })
#
# test_that("files_delete deletes a file", {
#   openai <- initialize("my_api_key")
#   file_id <- "file123"
#   deletion_status <- openai$files_delete(file_id)
#   expect_equal(deletion_status$success, TRUE)
# })test_that("files_retrieve returns the correct file details", {
#   file_id <- "my_file_id"
#   expected_result <- list(id = file_id, name = "example.txt", size = 1024)
#
#   result <- files_retrieve(file_id)
#
#   expect_equal(result$id, expected_result$id)
#   expect_equal(result$name, expected_result$name)
#   expect_equal(result$size, expected_result$size)
# })
#
# test_that("fine_tuning_jobs_create creates a fine-tuning job successfully", {
#   model <- "gpt-3.5-turbo"
#   training_file <- "training_data.txt"
#   hyperparameters <- list(n_epochs = 10, batch_size = 32, learning_rate_multiplier = 0.1)
#   expected_result <- list(job_id = "job_123", status = "queued")
#
#   result <- fine_tuning_jobs_create(model, training_file, hyperparameters)
#
#   expect_equal(result$job_id, expected_result$job_id)
#   expect_equal(result$status, expected_result$status)
# })test_that("fine_tuning_jobs_list returns a list of fine-tuning job objects", {
#   openai <- initialize("my_api_key")
#   result <- openai$fine_tuning_jobs_list()
#   expect_is(result, "list")
# })
#
# test_that("fine_tuning_jobs_retrieve returns the fine-tuning object with the given ID", {
#   openai <- initialize("my_api_key")
#   job_id <- "12345"
#   result <- openai$fine_tuning_jobs_retrieve(job_id)
#   expect_is(result, "list")
# })
#
# test_that("fine_tuning_jobs_cancel cancels the fine-tuning job", {
#   openai <- initialize("my_api_key")
#   job_id <- "12345"
#   result <- openai$fine_tuning_jobs_cancel(job_id)
#   expect_is(result, "list")
# })test_that("fine_tuning_jobs_events returns correct result", {
#   # Test case 1
#   job_id <- "job_id_1"
#   expected_result <- list(success=TRUE, data=list(event1="event1_data", event2="event2_data"))
#   mock_api_call <- function(endpoint, query, method, verbosity) {
#     expect_equal(endpoint, "fine_tuning_jobs/job_id_1/events")
#     expect_equal(query, list())
#     expect_equal(method, "GET")
#     expect_equal(verbosity, 0)
#     return(expected_result)
#   }
#   private$api_call <- mock_api_call
#   result <- fine_tuning_jobs_events(job_id)
#   expect_equal(result, expected_result$data)
#
#   # Test case 2
#   job_id <- "job_id_2"
#   expected_result <- list(success=TRUE, data=list(event1="event1_data", event2="event2_data"))
#   mock_api_call <- function(endpoint, query, method, verbosity) {
#     expect_equal(endpoint, "fine_tuning_jobs/job_id_2/events")
#     expect_equal(query, list(after="2022-01-01", limit=10))
#     expect_equal(method, "GET")
#     expect_equal(verbosity, 0)
#     return(expected_result)
#   }
#   private$api_call <- mock_api_call
#   result <- fine_tuning_jobs_events(job_id, after="2022-01-01", limit=10)
#   expect_equal(result, expected_result$data)
# })
#
# test_that("embeddings returns correct result", {
#   # Test case 1
#   model <- "model_id_1"
#   input <- "input_text"
#   expected_result <- list(success=TRUE, data=list(embedding1="embedding1_data", embedding2="embedding2_data"))
#   mock_api_call <- function(endpoint, body, method, headers, verbosity) {
#     expect_equal(endpoint, "embeddings")
#     expect_equal(body$model, "model_id_1")
#     expect_equal(body$input, "input_text")
#     expect_equal(method, "POST")
#     expect_equal(headers, list(`Content-Type` = "application/json"))
#     expect_equal(verbosity, 0)
#     return(expected_result)
#   }
#   private$api_call <- mock_api_call
#   result <- embeddings(model, input)
#   expect_equal(result, expected_result$data)
#
#   # Test case 2
#   model <- "model_id_2"
#   input <- c("input_text1", "input_text2")
#   expected_result <- list(success=TRUE, data=list(embedding1="embedding1_data", embedding2="embedding2_data"))
#   mock_api_call <- function(endpoint, body, method, headers, verbosity) {
#     expect_equal(endpoint, "embeddings")
#     expect_equal(body$model, "model_id_2")
#     expect_equal(body$input, c("input_text1", "input_text2"))
#     expect_equal(method, "POST")
#     expect_equal(headers, list(`Content-Type` = "application/json"))
#     expect_equal(verbosity, 0)
#     return(expected_result)
#   }
#   private$api_call <- mock_api_call
#   result <- embeddings(model, input)
#   expect_equal(result, expected_result$data)
# })test_that("audio_speech returns audio file content", {
#   api_key <- "my_api_key"
#   openai <- initialize(api_key)
#
#   # Test case 1: Verify that audio_speech returns audio file content
#   input <- "Hello, world!"
#   audio <- openai$audio_speech(input = input)
#   expect_true(!is.null(audio))
# })
#
# test_that("audio_speech handles stream parameter correctly", {
#   api_key <- "my_api_key"
#   openai <- initialize(api_key)
#
#   # Test case 2: Verify that audio_speech returns a DataStream object when stream parameter is set to TRUE
#   input <- "Hello, world!"
#   audio_stream <- openai$audio_speech(input = input, stream = TRUE)
#   expect_true(inherits(audio_stream, "DataStream"))
#
#   # Test case 3: Verify that audio_speech returns audio file content when stream parameter is set to FALSE
#   input <- "Hello, world!"
#   audio <- openai$audio_speech(input = input, stream = FALSE)
#   expect_true(!is.null(audio))
# })test_that("audio_transcriptions returns the transcribed text", {
#   # Set up test data
#   path <- "/path/to/audio/file.flac"
#   model <- "whisper-1"
#   expected_text <- "This is the transcribed text."
#
#   # Mock the file_call function
#   private$file_call <- function(endpoint, path, body, verbosity) {
#     expect_equal(endpoint, "audio")
#     expect_equal(path, "/transcriptions")
#     expect_equal(body$model, model)
#     expect_equal(body$file$name, "file")
#     expect_equal(body$file$value, path)
#     expect_equal(verbosity, 0)
#     return(list(data = expected_text))
#   }
#
#   # Call the audio_transcriptions function
#   result <- audio_transcriptions(path, model)
#
#   # Check the result
#   expect_equal(result, expected_text)
# })
#
# test_that("audio_transcriptions handles API errors", {
#   # Set up test data
#   path <- "/path/to/audio/file.flac"
#   model <- "whisper-1"
#   expected_message <- "API error message"
#   expected_type <- "API error type"
#
#   # Mock the file_call function to return an error
#   private$file_call <- function(endpoint, path, body, verbosity) {
#     return(list(success = FALSE, message = expected_message, type = expected_type))
#   }
#
#   # Call the audio_transcriptions function
#   result <- audio_transcriptions(path, model)
#
#   # Check the result
#   expect_equal(result$success, FALSE)
#   expect_equal(result$message, expected_message)
#   expect_equal(result$type, expected_type)
# })test_that("audio_translations returns the transcribed text", {
#   path <- "/path/to/audio/file.flac"
#   model <- "whisper-1"
#   expected_result <- "This is the transcribed text."
#
#   result <- audio_translations(path, model)
#
#   expect_equal(result, expected_result)
# })
#
# test_that("images_generations returns a list of image objects", {
#   prompt <- "Generate an image of a cat."
#   expected_result <- list(
#     list(
#       image_id = "1",
#       image_url = "https://example.com/image1.jpg"
#     ),
#     list(
#       image_id = "2",
#       image_url = "https://example.com/image2.jpg"
#     )
#   )
#
#   result <- images_generations(prompt)
#
#   expect_equal(result, expected_result)
# })test_that("images_edits function sets the correct options", {
#   image <- "path/to/image.png"
#   prompt <- "Edit the image"
#   option <- list(mask = "path/to/mask.png")
#
#   result <- images_edits(image, prompt, ..., verbosity = 2, mask = option$mask)
#
#   expect_equal(result$option$prompt, prompt)
#   expect_equal(result$option$image, curl::form_file(image))
#   expect_equal(result$option$mask, curl::form_file(option$mask))
#   expect_equal(result$verbosity, 2)
# })
#
# test_that("images_variations function sets the correct options", {
#   image <- "path/to/image.png"
#   option <- list(model = "model_name", n = 5, response_format = "json")
#
#   result <- images_variations(image, ..., verbosity = 1, model = option$model, n = option$n, response_format = option$response_format)
#
#   expect_equal(result$option$image, curl::form_file(image))
#   expect_equal(result$option$model, option$model)
#   expect_equal(result$option$n, option$n)
#   expect_equal(result$option$response_format, option$response_format)
#   expect_equal(result$verbosity, 1)
# })test_that("assistants_create creates a new assistant", {
#   model <- "gpt-3.5-turbo"
#   assistant <- assistants_create(model)
#   expect_equal(assistant$model, model)
# })
#
# test_that("assistants_retrieve retrieves an existing assistant", {
#   assistant_id <- "assistant_123"
#   assistant <- assistants_retrieve(assistant_id)
#   expect_equal(assistant$id, assistant_id)
# })
#
# test_that("assistants_modify modifies an existing assistant", {
#   assistant_id <- "assistant_123"
#   new_model <- "gpt-4.0-turbo"
#   assistant <- assistants_modify(assistant_id, model = new_model)
#   expect_equal(assistant$model, new_model)
# })test_that("assistants_delete deletes the assistant correctly", {
#   # Create a mock assistant ID
#   assistant_id <- "mock_assistant_id"
#
#   # Mock the API call
#   private$api_call <- function(endpoint, path, method, headers, verbosity) {
#     expect_equal(endpoint, "assistants")
#     expect_equal(path, "/mock_assistant_id")
#     expect_equal(method, "DELETE")
#     expect_equal(headers$`Content-Type`, "application/json")
#     expect_equal(headers$`OpenAI-Beta`, "assistants=v1")
#     expect_equal(verbosity, 0)
#
#     # Return a mock deletion status
#     return(list(success = TRUE))
#   }
#
#   # Call the function and expect the deletion status to be TRUE
#   result <- assistants_delete(assistant_id)
#   expect_equal(result$success, TRUE)
# })
#
# test_that("assistants_list returns a list of assistants", {
#   # Mock the API call
#   private$api_call <- function(endpoint, query, method, headers, verbosity) {
#     expect_equal(endpoint, "assistants")
#     expect_equal(query, list())
#     expect_equal(method, "GET")
#     expect_equal(headers$`Content-Type`, "application/json")
#     expect_equal(headers$`OpenAI-Beta`, "assistants=v1")
#     expect_equal(verbosity, 0)
#
#     # Return a mock list of assistants
#     return(list(data = list(
#       list(id = "assistant1", name = "Assistant 1"),
#       list(id = "assistant2", name = "Assistant 2")
#     )))
#   }
#
#   # Call the function and expect a list of assistants
#   result <- assistants_list()
#   expect_equal(length(result), 2)
#   expect_equal(result[[1]]$id, "assistant1")
#   expect_equal(result[[1]]$name, "Assistant 1")
#   expect_equal(result[[2]]$id, "assistant2")
#   expect_equal(result[[2]]$name, "Assistant 2")
# })test_that("assistants_file_create creates an assistant file", {
#   assistant_id <- "my_assistant_id"
#   file_id <- "my_file_id"
#   result <- assistants_file_create(assistant_id, file_id)
#   expect_true(result$success)
#   expect_equal(result$message, NULL)
#   expect_equal(result$type, NULL)
# })
#
# test_that("assistants_file_retrieve retrieves an assistant file", {
#   assistant_id <- "my_assistant_id"
#   file_id <- "my_file_id"
#   result <- assistants_file_retrieve(assistant_id, file_id)
#   expect_true(result$success)
#   expect_equal(result$message, NULL)
#   expect_equal(result$type, NULL)
# })test_that("assistants_file_delete deletes the file correctly", {
#   assistant_id <- "my_assistant_id"
#   file_id <- "my_file_id"
#   result <- assistants_file_delete(assistant_id, file_id)
#   expect_true(result$success)
# })
#
# test_that("assistants_file_list retrieves the list of files correctly", {
#   assistant_id <- "my_assistant_id"
#   result <- assistants_file_list(assistant_id)
#   expect_is(result, "list")
# })
#
# test_that("threads_create creates a thread correctly", {
#   result <- threads_create()
#   expect_is(result, "list")
# })test_that("threads_retrieve returns the correct thread object", {
#   # Create a mock thread ID
#   thread_id <- "mock_thread_id"
#
#   # Call the threads_retrieve function
#   result <- threads_retrieve(thread_id)
#
#   # Assert that the result is not NULL
#   expect_true(!is.null(result))
#
#   # Add more assertions here to validate the returned thread object
# })
#
# test_that("threads_modify returns the modified thread object", {
#   # Create a mock thread ID
#   thread_id <- "mock_thread_id"
#
#   # Create a mock metadata object
#   metadata <- list(key1 = "value1", key2 = "value2")
#
#   # Call the threads_modify function
#   result <- threads_modify(thread_id, metadata = metadata)
#
#   # Assert that the result is not NULL
#   expect_true(!is.null(result))
#
#   # Add more assertions here to validate the modified thread object
# })test_that("threads_delete deletes the thread correctly", {
#   thread_id <- "thread123"
#   expected_url <- "/threads/thread123"
#   expected_method <- "DELETE"
#   expected_headers <- list(`Content-Type` = "application/json", `OpenAI-Beta` = "assistants=v1")
#   expected_verbosity <- 0
#
#   mock_api_call <- function(url, method, headers, verbosity) {
#     expect_equal(url, expected_url)
#     expect_equal(method, expected_method)
#     expect_equal(headers, expected_headers)
#     expect_equal(verbosity, expected_verbosity)
#     # Return a dummy response
#     list(success = TRUE, data = "Thread deleted successfully")
#   }
#
#   private$api_call <- mock_api_call
#
#   result <- threads_delete(thread_id)
#
#   expect_equal(result$success, TRUE)
#   expect_equal(result$data, "Thread deleted successfully")
# })
#
# test_that("messages_create creates a message correctly", {
#   thread_id <- "thread123"
#   role <- "user"
#   content <- "Hello, how can I assist you?"
#   expected_url <- "/threads/thread123/messages"
#   expected_method <- "POST"
#   expected_headers <- list(`Content-Type` = "application/json", `OpenAI-Beta` = "assistants=v1")
#   expected_verbosity <- 0
#
#   mock_api_call <- function(url, method, headers, body, verbosity) {
#     expect_equal(url, expected_url)
#     expect_equal(method, expected_method)
#     expect_equal(headers, expected_headers)
#     expect_equal(body$role, role)
#     expect_equal(body$content, content)
#     expect_equal(verbosity, expected_verbosity)
#     # Return a dummy response
#     list(success = TRUE, data = "Message created successfully")
#   }
#
#   private$api_call <- mock_api_call
#
#   result <- messages_create(thread_id, role, content)
#
#   expect_equal(result$success, TRUE)
#   expect_equal(result$data, "Message created successfully")
# })test_that("messages_retrieve retrieves the correct message", {
#   thread_id <- "thread_123"
#   message_id <- "message_456"
#   expected_result <- list(id = message_id, text = "Hello, world!")
#
#   # BEGIN: Add test case
#   openai <- initialize("my_api_key")
#   result <- openai$messages_retrieve(thread_id, message_id)
#   expect_equal(result, expected_result)
#   # END: Add test case
# })
#
# test_that("messages_modify modifies the message correctly", {
#   thread_id <- "thread_123"
#   message_id <- "message_456"
#   metadata <- list(author = "John Doe", timestamp = "2022-01-01")
#   expected_result <- list(id = message_id, text = "Hello, world!", metadata = metadata)
#
#   # BEGIN: Add test case
#   openai <- initialize("my_api_key")
#   result <- openai$messages_modify(thread_id, message_id, metadata = metadata)
#   expect_equal(result, expected_result)
#   # END: Add test case
# })test_that("messages_list returns a list of messages", {
#   # Create a mock thread ID
#   thread_id <- "mock_thread_id"
#
#   # Call the messages_list function
#   result <- messages_list(thread_id)
#
#   # Assert that the result is a list
#   expect_is(result, "list")
# })
#
# test_that("messages_file_retrieve retrieves a message file", {
#   # Create mock thread, message, and file IDs
#   thread_id <- "mock_thread_id"
#   message_id <- "mock_message_id"
#   file_id <- "mock_file_id"
#
#   # Call the messages_file_retrieve function
#   result <- messages_file_retrieve(thread_id, message_id, file_id)
#
#   # Assert that the result is a list
#   expect_is(result, "list")
# })test_that("messages_file_list returns a list of message file objects", {
#   thread_id <- "my_thread_id"
#   message_id <- "my_message_id"
#   result <- messages_file_list(thread_id, message_id)
#   expect_is(result, "list")
#   expect_true(all(names(result) %in% c("success", "message", "type", "data")))
# })
#
# test_that("runs_create creates a run object", {
#   thread_id <- "my_thread_id"
#   assistant_id <- "my_assistant_id"
#   result <- runs_create(thread_id, assistant_id)
#   expect_is(result, "list")
#   expect_true(all(names(result) %in% c("success", "message", "type", "data")))
# })
#
# test_that("runs_retrieve retrieves a run object", {
#   thread_id <- "my_thread_id"
#   run_id <- "my_run_id"
#   result <- runs_retrieve(thread_id, run_id)
#   expect_is(result, "list")
#   expect_true(all(names(result) %in% c("success", "message", "type", "data")))
# })test_that("runs_modify modifies a run correctly", {
#   thread_id <- "my_thread_id"
#   run_id <- "my_run_id"
#   option <- list(param1 = "value1", param2 = "value2")
#   expected_body <- list(param1 = "value1", param2 = "value2")
#
#   result <- runs_modify(thread_id, run_id, option)
#
#   expect_equal(result$thread_id, thread_id)
#   expect_equal(result$run_id, run_id)
#   expect_equal(result$body, expected_body)
# })
#
# test_that("runs_list returns a list of run objects", {
#   thread_id <- "my_thread_id"
#   option <- list(param1 = "value1", param2 = "value2")
#
#   result <- runs_list(thread_id, option)
#
#   expect_is(result, "list")
#   expect_true(all(sapply(result, function(run) class(run) == "run_object")))
# })test_that("runs_submit_tool_outputs submits tool outputs correctly", {
#   thread_id <- "my_thread_id"
#   run_id <- "my_run_id"
#   tool_outputs <- list(
#     list(tool = "tool1", output = "output1"),
#     list(tool = "tool2", output = "output2")
#   )
#   expected_body <- list(tool_outputs = tool_outputs)
#
#   result <- runs_submit_tool_outputs(thread_id, run_id, tool_outputs)
#
#   expect_equal(result$success, TRUE)
#   expect_equal(result$message, NULL)
#   expect_equal(result$type, NULL)
#   expect_equal(result$data, expected_body)
# })
#
# test_that("runs_cancel cancels a run correctly", {
#   thread_id <- "my_thread_id"
#   run_id <- "my_run_id"
#
#   result <- runs_cancel(thread_id, run_id)
#
#   expect_equal(result$success, TRUE)
#   expect_equal(result$message, NULL)
#   expect_equal(result$type, NULL)
#   expect_equal(result$data, NULL)
# })test_that("runs_create_tread creates a thread and returns a run object", {
#   api_key <- "my_api_key"
#   openai <- initialize(api_key)
#   assistant_id <- "my_assistant_id"
#   option <- list(param1 = "value1", param2 = "value2")
#   result <- openai$runs_create_tread(assistant_id, ..., verbosity = 2)
#   expect_true(result$success)
#   expect_equal(result$message, NULL)
#   expect_equal(result$type, NULL)
#   expect_equal(result$data$assistant_id, assistant_id)
#   expect_equal(result$data$param1, option$param1)
#   expect_equal(result$data$param2, option$param2)
# })
#
# test_that("runs_steps_retrieve retrieves a run step", {
#   api_key <- "my_api_key"
#   openai <- initialize(api_key)
#   thread_id <- "my_thread_id"
#   run_id <- "my_run_id"
#   step_id <- "my_step_id"
#   result <- openai$runs_steps_retrieve(thread_id, run_id, step_id, verbosity = 1)
#   expect_true(result$success)
#   expect_equal(result$message, NULL)
#   expect_equal(result$type, NULL)
#   expect_equal(result$data$thread_id, thread_id)
#   expect_equal(result$data$run_id, run_id)
#   expect_equal(result$data$step_id, step_id)
# })
# test_that("runs_steps_list returns the correct run steps", {
#   thread_id <- "my_thread_id"
#   run_id <- "my_run_id"
#   expected_url <- paste0("/threads/", thread_id, "/runs/", run_id, "/steps")
#
#   # Mock the API call
#   private$api_call <- function(endpoint, query, method, headers, verbosity) {
#     expect_equal(endpoint, "threads")
#     expect_equal(query, expected_url)
#     expect_equal(method, "GET")
#     expect_equal(headers$`Content-Type`, "application/json")
#     expect_equal(headers$`OpenAI-Beta`, "assistants=v1")
#     expect_equal(verbosity, 0)
#
#     # Return sample data
#     return(list(data = list(
#       step1 = list(id = "step1", content = "Step 1"),
#       step2 = list(id = "step2", content = "Step 2")
#     )))
#   }
#
#   # Call the function
#   result <- runs_steps_list(thread_id, run_id)
#
#   # Check the result
#   expect_equal(result$step1$id, "step1")
#   expect_equal(result$step1$content, "Step 1")
#   expect_equal(result$step2$id, "step2")
#   expect_equal(result$step2$content, "Step 2")
# })
