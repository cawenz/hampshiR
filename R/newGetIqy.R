parseIqyUrl <- function(path) {
  q <- readLines(path)
  qurl <- paste(q[3], q[4], sep = "?")
  return(qurl)
}

testUrl <- parseIqyUrl("CW_httr2Test.iqy")
# 
# res <- httr2::request(testUrl) %>% httr2::req_perform() %>% httr2::resp_body_string()
# table <- rvest::html_table(rvest::minimal_html(res), header=T)[[1]]


getIqyData <- function(url){
  res <- httr2::request(url) %>% 
          httr2::req_perform() %>% 
            httr2::resp_body_string()
  
  table <- rvest::html_table(rvest::minimal_html(res),
                             header=T, 
                             na.strings=""
                             )[[1]]
  
  table
}

##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Test this with a bunch of iqy files I have and see the results
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

result <- getIqyData(testUrl)

str(result)
