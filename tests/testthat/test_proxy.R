test_that("test set_proxy", {
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  handle_openai$set_proxy("127.0.0.1",10890)
  expect_silent(handle_openai$set_proxy("127.0.0.1", 10890))
  expect_error(handle_openai$set_proxy("127.0sdha", 10890))
  expect_error(handle_openai$set_proxy("127.0.0.1", "sdsd"))
  expect_error(handle_openai$set_proxy("127.0.0.1", 8217321))
  expect_error(handle_openai$set_proxy("999.0.888.1", 10890))
})
