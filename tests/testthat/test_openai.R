test_that("init the openai api", {
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  expect_type(handle_openai, "environment")
  fc<-c("runs_steps_list","runs_steps_retrieve","runs_create_tread","runs_cancel",
    "runs_submit_tool_outputs","runs_list","runs_modify","runs_retrieve","runs_create",
    "messages_file_list","messages_file_retrieve","messages_list","messages_modify",
    "messages_retrieve","messages_create","threads_delete","threads_modify",
    "threads_retrieve","threads_create","assistants_file_list",
    "assistants_file_delete","assistants_file_retrieve",
    "assistants_file_create","assistants_list","assistants_delete",
    "assistants_modify","assistants_retrieve","assistants_create","images_variations",
    "images_edits","images_generations","audio_translations","audio_transcriptions",
    "audio_speech","embeddings","fine_tuning_jobs_events","fine_tuning_jobs_cancel",
    "fine_tuning_jobs_retrieve","fine_tuning_jobs_list","fine_tuning_jobs_create",
    "files_retrieve","files_delete","files_get_list","files_upload","get_chat_completions_query",
    "get_completions_query","fine_tuning_jobs_delete","get_model_retrieve","get_model_list",
    "set_proxy")
    expect_in(fc,names(handle_openai))
    if(Sys.getenv("USE_PROXY")=="TRUE"){
      handle_openai$set_proxy("127.0.0.1", 10890)
    }
    res<-handle_openai$get_model_list(verbosity = 100)
    expect_true(!res$success)
    res<-handle_openai$get_model_list()
    expect_null(res$success)
})

test_that("test get_model_retrieve",{
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("127.0.0.1", 10890)
  }
  res<-handle_openai$get_model_retrieve(model = "gpt-3.5-turbo",verbosity = 0)
  expect_equal(res$id,"gpt-3.5-turbo")
  res<-handle_openai$get_model_retrieve(model = "gpt-3.5-turbo",verbosity = 200)
  expect_true(!res$success)
})

test_that("test set_proxy", {
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  handle_openai$set_proxy("127.0.0.1",10890)
  expect_silent(handle_openai$set_proxy("127.0.0.1", 10890))
  expect_error(handle_openai$set_proxy("127.0sdha", 10890))
  expect_error(handle_openai$set_proxy("127.0.0.1", "sdsd"))
  expect_error(handle_openai$set_proxy("127.0.0.1", 8217321))
  expect_error(handle_openai$set_proxy("999.0.888.1", 10890))
})

test_that("test_up_fileload",{
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("127.0.0.1",10890)
  }
  train_file_path<-system.file("exdata","train.jsonl", package = "openaistream")
  file_id <- handle_openai$files_upload(path = train_file_path,purpose = "fine-tune")
  expect_equal(file_id$filename,"train.jsonl")
  #error test
  res<-handle_openai$files_upload(path = train_file_path,purpose = "fine-tune",verbosity = 5)
  expect_true(!res$success)

  file_mes<-handle_openai$files_retrieve(file_id=file_id$id, verbosity = 0)
  #error test
  res<-handle_openai$files_retrieve(file_id=file_id$id, verbosity = 5)
  expect_true(!res$success)

  expect_equal(file_mes$id,file_id$id)
  del_res<-handle_openai$files_delete(file_id$id, verbosity = 0)
  #error test
  res<-handle_openai$files_delete(file_id$id, verbosity = 5)
  expect_true(!res$success)

  expect_true(del_res$deleted)
})





