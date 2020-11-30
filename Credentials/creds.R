

db <- dbConnect(MySQL(), dbname = "odvjet12_gfi", host =  "91.234.46.219", 
                port = 3306L, user ="odvjet12_mislav", 
                password = "Theanswer0207")

nazivi <- fromJSON(toJSON(content(POST(url = "https://api.data-api.io/v1/subjekti",
                                       add_headers('x-dataapi-key' = "59dd75a6525e",
                                                   'content-type'= "application/json"),
                                       body = list(oib = firme$subjectid), encode = "json", verbose()),
                                  type = "application/json"), null = "null"), flatten = TRUE) 