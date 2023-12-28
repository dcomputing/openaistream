handle_openai<-openai$new(Sys.getenv("OPENAI_KEY"))
if(Sys.getenv("USE_PROXY")=="TRUE"){
  handle_openai$set_proxy("127.0.0.1", 10890)
}
res<-handle_openai$get_model_retrieve(model = "gpt-3.5-turbo",verbosity = 3)
