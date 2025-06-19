
# openaistreamES <img src="man/figures/lamb.jpg" align="right" width="120"/>


## Overview

`{openaistreamES}` is based on an unofficial SDK for OpenAI, which aimed to
implement all related features as comprehensively as possible. It was
updated every two months based on the actual changes in OpenAI’s
interfaces but was last updated June 2024. This fork extends the library to enable support for deepseek or local openai api capable streaming responses.

## 1. Installation

The method to install this package is to download and install from source:

``` r
  install.packages("path to openaistreamES", repos = NULL, type = "source")
```

## 2. Creating an OpenAI Connection

### 2.1 Create a Link Object

Before using the API, you need to create a link object and configure your secret key and the target ai server:

```r
handle_openai <- openai$new(Sys.getenv("OPENAI_KEY", "openai"))
```

### 2.2 Configure Proxy (Optional)

If you cannot access OpenAI due to network restrictions, you can configure a valid proxy address and port:

```r
handle_openai$set_proxy("PROXY_IP", PROXY_PORT)
```

### 2.3 Link Test: Query Available Models

Test the connection by querying available models:

```r
handle_openai$models$list()
```

## 3. Conversation

### 3.1 Initiate the First Chat

Start a chat session:

```r
streamlg <- handle_openai$chat$create(
  messages = data.frame(role = c("system", "user"),
                        content = c("You are an assistant.", "How's the weather today?")),
  model = "gpt-3.5-turbo",
  max_tokens = 10,
  n = 3
)
```

Note: The `messages` parameter is a `data.frame` with two columns: the first column is the role identifier, and the second column is the conversation content. The response will be stored in the `streamlg` variable.

### 3.2 Use Stream to Return Data

Add the `stream = TRUE` parameter to return a stream object, which includes three methods: `get_state()` to get the current stream state, `next_value` to get the next data value, and `close()` to close the current stream object.

```r
streamlg <- handle_openai$chat$create(
  messages = data.frame(role = c("system", "user"),
                        content = c("You are an assistant.", "How's the weather today?")),
  model = "gpt-3.5-turbo",
  stream = TRUE,
  max_tokens = 2,
  n = 3
)
streamlg$get_state()
text <- streamlg$next_value
streamlg$close()
```

### 3.3 Extension

All API parameters correspond to the OpenAI HTTP interface and support almost all OpenAI parameters. You can refer to the OpenAI official documentation for the relevant parameters. Apart from `messages` which is passed as a `data.frame`, other object parameters can be replaced with a `list`.

Note: In the following code, `stream_options` is an object passed as a `list` in R:

```r
streamlg <- handle_openai$chat$create(
  messages = data.frame(role = c("system", "user"),
                        content = c("You are an assistant.", "How's the weather today?")),
  model = "gpt-3.5-turbo",
  stream = TRUE,
  stream_options = list(include_usage = TRUE),
  max_tokens = 2,
  n = 3
)
```

(For full API support, usage reference: [chat.R](https://github.com/libingfei/openaistream/blob/main/tests/testthat/test_chat.R))

## 4. Images

(For full API support, usage reference: [test_images.R](https://github.com/libingfei/openaistream/blob/main/tests/testthat/test_images.R))

## 5. Files

(For full API support, usage reference: [test_files.R](https://github.com/libingfei/openaistream/blob/main/tests/testthat/test_files.R))

## 6. Fine-Tuning

(For full API support, usage reference: [test_fine_tuning.R](https://github.com/libingfei/openaistream/blob/main/tests/testthat/test_fine_tuning.R))

## 7. Vector Stores

(For full API support, usage reference: [test_vector_stores.R](https://github.com/libingfei/openaistream/blob/main/tests/testthat/test_vector_stores.R))

## 8. Assistants

(For full API support, usage reference: [test_assistants.R](https://github.com/libingfei/openaistream/blob/main/tests/testthat/test_assistants.R); [test_run.R](https://github.com/libingfei/openaistream/blob/main/tests/testthat/test_run.R))

## 9. Embedding

(For full API support, usage reference: [test_embedding.R](https://github.com/libingfei/openaistream/blob/main/tests/testthat/test_embedding.R))

## 10. Speech

(For full API support, usage reference: [test_speech.R](https://github.com/libingfei/openaistream/blob/main/tests/testthat/test_speech.R))

## 11. Batch

(For full API support, usage reference: [test_batch.R](https://github.com/libingfei/openaistream/blob/main/tests/testthat/test_batch.R))

## 12. Moderations

(For full API support, usage reference: [test_moderations.R](https://github.com/libingfei/openaistream/blob/main/tests/testthat/test_moderations.R))
