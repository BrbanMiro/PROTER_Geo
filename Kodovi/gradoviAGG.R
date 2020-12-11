

str(data)
range(data$reportyear)



geo1 <- read.csv2("OPCINE.csv") %>%
  select(ZUPANIJA, NAZIV_ZUP) %>%
  distinct(.) %>%
  arrange(ZUPANIJA)


geo2 <- read.csv2("OPCINE.csv") %>%
  select(OPCINA, NAZIV_OPC) %>%
  distinct(.) %>%
  arrange(OPCINA)




zupanije <- data %>%
  mutate(djelatnosti = case_when(nacerev21 == "A" ~ "primarne",
                                 nacerev21 %in% c("B", "C", "D", "E", "F") ~ "sekundarne",
                                 nacerev21 %in% c("G", "H", "I", "J") ~ "tecijarne",
                                 nacerev21 %in% c("K", "L", "M", "N") ~ "kvartarne",
                                 nacerev21 %in% c("O", "P", "Q", "R", "S", "T") ~ "kvintarne")) %>%
  filter(reportyear == 2016) %>%
  group_by(djelatnosti, countyid, municipalityid) %>% 
  summarise(n = n_distinct(subjectid),
            prihod = sum(prihod, na.rm = TRUE),
            izvoz = sum(Export, na.rm = TRUE),
            uvoz = sum(Import, na.rm = TRUE),
            k_dug = sum(k_dug, na.rm = TRUE),
            d_dug = sum(d_dug, na.rm = TRUE),
            rad = sum(employeecounteop, na.rm = TRUE),
            trPlaca= sum(placa_brutto, na.rm = TRUE),
            mim = sum(matimov, na.rm = TRUE),
            nim = sum(nematimov, na.rm = TRUE),
            placa = sum(placa_brutto, na.rm = TRUE),
            amortizacija = sum(amortizacija , na.rm = TRUE),
            fin_rashod = sum(financijski_trosak , na.rm = TRUE),
            tax_nett = sum(porez_dobit , na.rm = TRUE),
            nett = sum(nett , na.rm = TRUE),
            kapital = sum(kapital_rezerve , na.rm = TRUE),
            placa = ((trPlaca)/(rad))*1000000000/12) %>%
  mutate(dv = trPlaca + amortizacija + fin_rashod + tax_nett + nett,
         imov = mim + nim,
         dug = k_dug + d_dug) %>%
  full_join(., geo1, by = c("countyid" = "ZUPANIJA")) %>%
  full_join(., geo2, by = c("municipalityid" = "OPCINA")) %>%
  select(djelatnosti,countyid,municipalityid,n,prihod,izvoz,uvoz, dv,NAZIV_ZUP,NAZIV_OPC ) %>% 
  mutate_at(vars(prihod:dv),  .funs = funs(. * 1000000000))
  
  


write.csv2(zupanije, "opcine2016.csv")

write.csv2(zupanije, "opcine2017.csv")




library(tidyverse)

gfi <- read.csv2("C:/Users/msagovac/Desktop/gfi2018/gfi_2018_wide.csv") %>%
  select(year, OIB, MBS, NKD2007, ZUPANIJA, OPCINA, AOP125, AOP137,AOP141,AOP165, AOP195, AOP202) %>%
  rename(godina = year,
         prihod = AOP125,
         place = AOP137,
         amortizacija = AOP141,
         finTrosak = AOP165,
         tax = AOP195,
         dobit = AOP202) 

nkd <-  read.csv2("C:/Users/msagovac/Desktop/gfi2018/SIFRANIKnkd.csv")


zupanija <- read.csv2("OPCINE.csv") %>%
  select(ZUPANIJA, NAZIV_ZUP) %>%
  distinct(.) %>%
  arrange(ZUPANIJA)


opcina <- read.csv2("OPCINE.csv") %>%
  select(OPCINA, NAZIV_OPC) %>%
  distinct(.) %>%
  arrange(OPCINA)



 dta <- gfi %>%
  filter(godina == 2018) %>%
  left_join(.,nkd,by=c("NKD2007" = "ActivityID")) %>%
  left_join(., zupanija, by = "ZUPANIJA")%>%
  left_join(., opcina, by =  "OPCINA") %>%
  mutate(prihod = as.numeric(prihod)) %>%
  mutate(dv = prihod + amortizacija + finTrosak + tax + dobit) %>%
  mutate(djelatnosti = case_when(ActivityArea == "A" ~ "primarne",
                                  ActivityArea %in% c("B", "C", "D", "E", "F") ~ "sekundarne",
                                  ActivityArea %in% c("G", "H", "I", "J") ~ "tecijarne",
                                  ActivityArea %in% c("K", "L", "M", "N") ~ "kvartarne",
                                  ActivityArea %in% c("O", "P", "Q", "R", "S", "T") ~ "kvintarne")) 



djelatnosti <- dta %>%
  group_by(djelatnosti, OPCINA, ZUPANIJA,NAZIV_ZUP, NAZIV_OPC) %>% 
  summarise(DV = sum(dv, na.rm = TRUE),
            prihod = sum(prihod, na.rm = TRUE),
            n = n_distinct(OIB))





