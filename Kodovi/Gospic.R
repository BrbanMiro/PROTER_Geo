

# SIROVI PODATCI ZA GOSPIC

 
x <- function(x){x*1000000000}

gospic_full <-  data %>% 
  filter(municipalityid == 130) %>% # Gospic == 130
  mutate(djelatnosti = case_when(nacerev21 == "A" ~ "primarne",
                                 nacerev21 %in% c("B", "C", "D", "E", "F") ~ "sekundarne",
                                 nacerev21 %in% c("G", "H", "I", "J") ~ "tecijarne",
                                 nacerev21 %in% c("K", "L", "M", "N") ~ "kvartarne",
                                 nacerev21 %in% c("O", "P", "Q", "R", "S", "T") ~ "kvintarne")) %>%
  mutate( dv = placa_net + amortizacija + financijski_trosak + porez_dobit + nett)

gospic_full %>% write.csv2(., "D:/LUKA/Academic/PROTER_Geo/data/gospicFull.csv")


# POVUCI NAZIVE TVRTKI SA SREG API


## SREG-----

API_KEY <- "2d8807bbcc0c44ce96a9424945105af9"

# URLS

sjecajUrl <- "https://sudreg-api.pravosudje.hr/javni/postupak?offset=200&limit=800000&timestamp_id="
url <- "https://sudreg-api.pravosudje.hr/javni/subjekt?offset=200&limit=800000&timestamp_id=&only_active=false"
vrsta <- "https://sudreg-api.pravosudje.hr/javni/vrsta_postupka?timestamp_id="


# POVUCI 
#options(timeout = 4000000) 

API_zahtjev <- httr::GET(sjecajUrl, add_headers('Ocp-Apim-Subscription-Key' = API_KEY))
jS_tekst <- httr::content(API_zahtjev, as = "text", type = "aplication/json", encoding = "UTF-8")
DF_za_analizu <- jsonlite::fromJSON(jS_tekst, flatten = TRUE)



# KLEUTOV API


nazivi <- fromJSON(toJSON(content(POST(url = "https://api.data-api.io/v1/subjekti",
                                       add_headers('x-dataapi-key' = "59dd75a6525e",
                                                   'content-type'= "application/json"),
                                       body = list(oib = gospic_full$subjectid), encode = "json", verbose()),
                                  type = "application/json"), null = "null"), flatten = TRUE) %>%
  rename(subjectid = mb) 



naziviDF <- do.call(rbind.data.frame, nazivi)

naziviDF <- data.frame(Reduce(rbind, nazivi))

naziviDF <- enframe(nazivi)



req <- GET(paste0("https://sudreg-api.pravosudje.hr/javni/subjekt?offset=0&limit=800000&expand_relations=true&only_active=false"),
           add_headers('Ocp-Apim-Subscription-Key' = "0c7a9bbd34674a428e4218340fba732b"), timeout(9999))

req <- GET(paste0("https://sudreg-api.pravosudje.hr/javni/subjekt_detalji?tipIdentifikatora=oib"),
           add_headers('Ocp-Apim-Subscription-Key' = "0c7a9bbd34674a428e4218340fba732b"), timeout(9999))

https://sudreg-api.pravosudje.hr/javni/subjekt_detalji?tipIdentifikatora={tipIdentifikatora}&identifikator={identifikator}[&expand_relations][&timestamp_id]

firmeJson <- httr::content(req, as = "text", encoding = "UTF-8")
firmeJson <- jsonlite::fromJSON(firmeJson, simplifyDataFrame = TRUE)
firmeJson$oib <- str_pad(firmeJson$oib, 11L, 'left', "0")



str(nazivi)






gospic_alles <- full_join(gospic_full, nazivi, by = "subjectid")







gospic_full %>%
  group_by(reportyear) %>%
  summarise(n = n())






  group_by(reportyear) %>%
  summarise( n = sum(n, na.rm = TRUE),
             prihod = sum(prihod, na.rm = TRUE),
             izvoz = sum(izvoz, na.rm = TRUE),
             uvoz = sum(uvoz, na.rm = TRUE),
             dug = sum(dug, na.rm = TRUE),
             rad = sum(rad, na.rm = TRUE),
             mim = sum(mim, na.rm = TRUE),
             nim = sum(nim, na.rm = TRUE),
             placa = sum(plata, na.rm = TRUE)/4,
             kapital = sum(kapital , na.rm = TRUE),
             dv = sum(dv, na.rm = TRUE)) %>%
  mutate_at(c("prihod", "izvoz", "uvoz","dug","mim","nim", "kapital", "dv"),x ) %>% 
  mutate(GEO = "Gospic") %>% View()

write.csv2()            

dvor %>% write.csv2(., "D:/LUKA/Academic/PROTER_Geo/data/dvorMacros.csv")