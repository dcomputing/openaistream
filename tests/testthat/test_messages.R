test_that("messages",{
  Sys.setenv(TEST_EX_COND = "")
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("127.0.0.1",10890)
  }
  thc<-handle_openai$threads_create()
  expect_contains(names(thc),"id")
  train_file_path<-system.file("exdata","assfile.csv", package = "openaistream")
  file_id <- handle_openai$files_upload(path = train_file_path,purpose = "assistants",verbosity = 0)

  mesc<-handle_openai$messages_create(thc$id,role = "user",content = "hello",file_ids=list(file_id$id),verbosity = 3)
  expect_equal(mesc$object,"thread.message")
  mesl<-handle_openai$messages_list(thread_id = thc$id,verbosity = 3)
  expect_contains(mesl$data$id,mesc$id)
  meslf<-handle_openai$messages_file_list(thc$id,mesc$id)
  expect_contains(meslf$data$id,file_id$id)
  mesr<-handle_openai$messages_retrieve(thc$id,mesc$id)
  expect_equal(mesr$object,"thread.message")
  mesrf<-handle_openai$messages_file_retrieve(thc$id,mesc$id,file_id$id)
  expect_equal(mesr$object,"thread.message.file")

  del_res<-handle_openai$files_delete(file_id$id, verbosity = 0)
  expect_true(del_res$deleted)
  thd<-handle_openai$threads_delete(thc$id)
  expect_true(thd$deleted)
})
