test_that("chat",{
  Sys.setenv(TEST_EX_COND = "")
  handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
  if(Sys.getenv("USE_PROXY")=="TRUE"){
    handle_openai$set_proxy("127.0.0.1",10890)
  }
  embed<-handle_openai$embeddings(model = "text-embedding-ada-002", input = "who are you?",verbosity = 3)
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
    max_tokens = 10,
    n =3
  )
  expect_equal(class(streamlg)[1],"DataStream")
  streamlg$get_state()
  expect_equal(streamlg$get_state(),"initialized")
  text<-streamlg$next_value
  expect_equal(length(text$all_resp),2)
  text<-streamlg$next_value
  expect_equal(length(text$all_resp),2)
  ss<-streamlg$close()
  expect_equal(ss,"close success")
  text<-streamlg$next_value
  expect_equal(text,"close")
  stat<-streamlg$get_state()
  expect_equal(stat,"close")

  #error test
  streamlg <- handle_openai$get_chat_completions_query(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = TRUE,
    max_tokens = 10,
    n = "sdasd"
  )
  text<-streamlg$next_value
  #expect_equal(text,"httr2_close open is fail")
  expect_equal(text$message,"parse error: trailing garbage\n                              \"message\": \"'sdasd' is not of type 'inte\n                     (right here) ------^\n")

  streamlg <- handle_openai$get_chat_completions_query(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = T,
    max_tokens = 10,
    n = 2,num="sdas"
  )
  text<-streamlg$next_value
  expect_equal(text$message,"non-numeric argument to binary operator")

  expect_error({
    streamlg <- handle_openai$get_chat_completions_query(
      messages = data.frame(role = c("system", "user"),
                            content = c("You are a assistant.", "How's the weather today?")),
      model = "gpt-3.5-turbo",
      stream = "sadas",
      max_tokens = 10,
      n = 2
    )
  })
  ####Testing extreme conditions
  Sys.setenv(TEST_EX_COND = "error chatstream active next value")
  streamlg <- handle_openai$get_chat_completions_query(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = TRUE,
    max_tokens = 10,
    n =3
  )
  text<-streamlg$next_value
  expect_equal(text$message,"error chatstream active next value")


  Sys.setenv(TEST_EX_COND = "error chatstream check is not curl")
  streamlg <- handle_openai$get_chat_completions_query(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = TRUE,
    max_tokens = 10,
    n =3
  )
  text<-streamlg$next_value
  expect_equal(text,"is not curl")

  Sys.setenv(TEST_EX_COND = "error chatstream active isOpen")
  streamlg <- handle_openai$get_chat_completions_query(
    messages = data.frame(role = c("system", "user"),
                          content = c("You are a assistant.", "How's the weather today?")),
    model = "gpt-3.5-turbo",
    stream = TRUE,
    max_tokens = 10,
    n =3
  )
  text<-streamlg$next_value
  expect_equal(text$message,"error chatstream active isOpen")
})
