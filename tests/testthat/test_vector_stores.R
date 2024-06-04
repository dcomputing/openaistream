#test5
test_that("run",{
  skip_on_cran()
  Sys.setenv(TEST_EX_COND = "")
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("10.0.108.36",7890)
  }
  goog_path<-system.file("exdata","goog-10k.pdf", package = "openaistream")
  brka_path<-system.file("exdata","brka-10k.txt", package = "openaistream")
  goog_file_id <- handle_openai$files$upload(path = goog_path,purpose = "assistants",verbosity = 0)
  brka_file_id <- handle_openai$files$upload(path = brka_path,purpose = "assistants",verbosity = 0)
  #以上为准备基础文件
  #开始向量数仓基础功能测试
  vector_store = handle_openai$vector_stores$create(name="Financial Statements")
  expect_equal(vector_store$status,"completed")
  e_vector_store = handle_openai$vector_stores$create(name="Financial Statements",verbosity = 4)
  expect_true(!e_vector_store$success)
  vsl <- handle_openai$vector_stores$list()
  expect_contains(vsl$data$name,"Financial Statements")
  e_vsl <- handle_openai$vector_stores$list(verbosity = 4)
  expect_true(!e_vsl$success)
  vsm <- handle_openai$vector_stores$modify(vector_store_id = vector_store$id,name="FS",expires_after=list(anchor="last_active_at",days=1))
  expect_equal(vsm$name,"FS")
  e_vsm <- handle_openai$vector_stores$modify(vector_store_id = vector_store$id,name="FS",expires_after=list(anchor="last_active_at",days=1),verbosity = 4)
  expect_true(!e_vsm$success)
  
  vsr <- handle_openai$vector_stores$retrieve(vector_store_id = vector_store$id)
  expect_equal(vsr$expires_after$days,1)
  Sys.sleep(3)
  e_vsr <- handle_openai$vector_stores$retrieve(vector_store_id = vector_store$id,verbosity = 4)
  expect_true(!e_vsr$success)
  #开始向量数仓文件功能测试
  
  vsd <- handle_openai$vector_stores$delete(vector_store_id = vector_store$id,verbosity = 3)
  expect_true(vsd$deleted)
  e_vsd <- handle_openai$vector_stores$delete(vector_store_id = vector_store$id)
  expect_true(!e_vsd$success)
  #以下为删除相关文件
  del_res1<-handle_openai$files$delete(file_id = goog_file_id$id, verbosity = 0)
  del_res2<-handle_openai$files$delete(file_id = brka_file_id$id, verbosity = 0)
  
})