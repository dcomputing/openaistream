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

test_that("test fileload and fine train",{
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

  file_list<-handle_openai$files_get_list(verbosity = 0)
  expect_null(file_list$success)
  #error test
  res<-handle_openai$files_get_list(verbosity = 5)
  expect_true(!res$success)

  file_mes<-handle_openai$files_retrieve(file_id=file_id$id, verbosity = 0)
  expect_equal(file_mes$id,file_id$id)
  #error test
  res<-handle_openai$files_retrieve(file_id=file_id$id, verbosity = 5)
  expect_true(!res$success)

  #job create for cancel job
  job<-handle_openai$fine_tuning_jobs_create(model = "gpt-3.5-turbo",training_file = file_id$id)
  expect_equal(job$training_file,file_id$id)

  #job create for error test
  res<-handle_openai$fine_tuning_jobs_create(model = "gpt-3.5-turbo",training_file = file_id$id,verbosity = 4)
  expect_true(!res$success)

  job_events<-handle_openai$fine_tuning_jobs_events(job$id, verbosity = 0)
  expect_contains(names(job_events$data),"data")

  #error test
  res<-handle_openai$fine_tuning_jobs_events(job$id, verbosity = 4)
  expect_true(!res$success)

  job_retrieve<-handle_openai$fine_tuning_jobs_retrieve(job$id, verbosity = 0)
  expect_equal(job_retrieve$id,job$id)

  #error test
  res<-handle_openai$fine_tuning_jobs_retrieve(job$id, verbosity = 4)
  expect_true(!res$success)

  job_list<-handle_openai$fine_tuning_jobs_list()
  expect_contains(names(job_list),"data")

  #error test
  res<-handle_openai$fine_tuning_jobs_list(verbosity = 4)
  expect_true(!res$success)

  job_status<-handle_openai$fine_tuning_jobs_cancel(job$id)
  expect_equal(job_status$id,job$id)

  #error test
  res<-handle_openai$fine_tuning_jobs_cancel("ftjob-VxJMKqY0gTmGPT")
  expect_true(!res$success)



  #error test
  del_res<-handle_openai$files_delete(file_id$id, verbosity = 0)
  expect_true(del_res$deleted)

  #error test
  res<-handle_openai$files_delete(file_id$id, verbosity = 5)
  expect_true(!res$success)
})
test_that("chat",{
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("127.0.0.1",10890)
  }
  embed<-handle_openai$embeddings(model = "text-embedding-ada-002", input = "who are you?")
  expect_type(embed$data$embedding[[1]],"double")
  # chat test not stream
  streamlg <- handle_openai$get_completions_query(
    prompt = "Please explain the World Wars?",
    model = "davinci-002",
    stream = FALSE,
    max_tokens = 20,
    verbosity = 0,n=3
  )
  expect_equal(nrow(streamlg$vres),3)
  streamlg <- handle_openai$get_chat_completions_query(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = F,
    max_tokens = 10,n=3
  )
  expect_equal(nrow(streamlg$vres),3)
  # chat test stream
  streamlg <- handle_openai$get_chat_completions_query(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = TRUE,
    max_tokens = 10,n = 3
  )
  expect_equal(class(streamlg)[1],"DataStream")
  streamlg$get_state()
  expect_equal(streamlg$get_state(),"initialized")
  text<-streamlg$next_value
  expect_equal(length(text$all_resp),2)
  ss<-streamlg$close()
  expect_equal(ss,"close success")
  text<-streamlg$next_value
  expect_equal(text,"close")
  stat<-streamlg$get_state()
  expect_equal(stat,"close")
})

test_that("long time test",{
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("127.0.0.1",10890)
  }
  train_file_path<-system.file("exdata","train.jsonl", package = "openaistream")
  file_id <- handle_openai$files_upload(path = train_file_path,purpose = "fine-tune")
  # the pretest
  #job create for delete job
  job2<-handle_openai$fine_tuning_jobs_create(model = "gpt-3.5-turbo",training_file = file_id$id)
  #test jobs_delete
  Sys.sleep(200)
  #error test
  for(i in 1:20){
    Sys.sleep(20)
    job_retrieve<-handle_openai$fine_tuning_jobs_retrieve(job2$id, verbosity = 0)
    print(paste0("\n",i," retry ",job_retrieve$status))
    if(job_retrieve$status=="succeeded"){
      res<-handle_openai$fine_tuning_jobs_delete(job_retrieve$fine_tuned_model)
      expect_true(res$deleted)
      break
    }
  }
  #error test
  res<-handle_openai$fine_tuning_jobs_delete("gpt-3.5-turbo-sadsada")
  expect_true(!res$success)
  del_res<-handle_openai$files_delete(file_id$id, verbosity = 0)
})
