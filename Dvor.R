












x <- function(x){x*1000000000}

dvor <- zup_sektori %>% 
  filter(municipalityid == 102) %>%
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
  mutate(GEO = "Dvor") %>% View()

 write.csv2()            
 
dvor %>% write.csv2(., "D:/LUKA/Academic/PROTER_Geo/data/dvorMacros.csv")
