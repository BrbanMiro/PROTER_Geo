


















data %>%
  mutate(djelatnosti = case_when(nacerev21 == "A" ~ "primarne",
                                 nacerev21 %in% c("B", "C", "D", "E", "F") ~ "sekundarne",
                                 nacerev21 %in% c("G", "H", "I", "J") ~ "tecijarne",
                                 nacerev21 %in% c("K", "L", "M", "N") ~ "kvartarne",
                                 nacerev21 %in% c("O", "P", "Q", "R", "S", "T") ~ "kvintarne")) %>%
  filter( countyid == "17") %>%
  filter(reportyear %in% c(2012,2018,2019)) %>%
  group_by(reportyear, djelatnosti) %>%
  summarise(n = n_distinct(subjectid),
            prihod = sum(prihod, na.rm = TRUE),
            izvoz = sum(Export, na.rm = TRUE),
            uvoz = sum(Import, na.rm = TRUE),
            k_dug = sum(k_dug, na.rm = TRUE),
            d_dug = sum(d_dug, na.rm = TRUE),
            rad = sum(employeecounteop, na.rm = TRUE),
            mim = sum(matimov, na.rm = TRUE),
            nim = sum(nematimov, na.rm = TRUE),
            placa = sum(placa_brutto, na.rm = TRUE),
            amortizacija = sum(amortizacija , na.rm = TRUE),
            fin_rashod = sum(financijski_trosak , na.rm = TRUE),
            tax_nett = sum(porez_dobit , na.rm = TRUE),
            nett = sum(nett , na.rm = TRUE),
            kapital = sum(kapital_rezerve , na.rm = TRUE),
            plata =  (sum(placa_brutto, na.rm = TRUE)/(sum(employeecounteop, na.rm = TRUE)))*1000000000/12) %>%
  mutate(dv = placa + amortizacija + fin_rashod + tax_nett + nett,
         imov = mim + nim,
         dug = k_dug + d_dug,
         invest = (imov + amortizacija) - dplyr::lag(imov)) %>%
  select(reportyear, djelatnosti, n, prihod, rad, dv,placa,amortizacija, fin_rashod, tax_nett, nett ) %>%
  ungroup() %>% 
  group_by(reportyear) %>%
  mutate(ukDV = sum(dv, na.rm = T),
         ukP = sum(prihod, na.rm = T),
         ukZ = sum(rad, na.rm = T),
         udio = (dv / ukDV)*100) %>%View()





placa + amortizacija + fin_rashod + tax_nett + nett


