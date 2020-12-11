

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

# sanity check
djelatnosti %>% 
  ungroup() %>% 
  summarise(DV = sum(DV, na.rm = T))


write.csv2(djelatnosti, "GEOgfi2018.csv")

