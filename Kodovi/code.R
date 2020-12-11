
#PAKETI
#----
library(eurostat)
library(ggplot2)
library(dplyr)
library(knitr)
library(cowplot)
library(gridExtra)
library(grid)
library(gtable)
library(RMySQL)
library(RODBC)
library(kableExtra)
library(RMySQL)
library(RODBC)
library(stringi)
library(plm)
library(tidyr)
library(data.table)
library(scales)
library(prodest)
library(readxl)
library(httr)
library(jsonlite)
library(purrr)



#----
source("./Credentials/creds.R")


q <- sprintf("SELECT b372 AS Zaposleni,
                     b084/1000000000 AS d_dug,
                     b094/1000000000 AS k_dug,
                     b010/1000000000 AS matimov, 
                     b063/1000000000 AS kapital_rezerve,
                     b064/1000000000 AS osnovni_kapital,
                     b003/1000000000 AS nematimov,
                     b110/1000000000 AS prihod,
                     b120/1000000000 AS placa_net,
                     b119/1000000000 AS placa_brutto,
                     b123/1000000000 AS amortizacija,
                     b136/1000000000 AS financijski_trosak,
                     b159/1000000000 AS porez_dobit,
                     b151/1000000000 AS nett,
                     b115/1000000000 AS materijalni_trosak,
                     b084/1000000000 AS d_obveze,
                     b094/1000000000 AS k_obveze,
                     b335/1000000000 AS Export,
                     b370/1000000000 AS Import,
                     b337/1000000000 AS energy,
                     reportyear, subjectid, employeecounteop, foreigncontrol,
             nacerev21, nacerev22, nacerev23, nacerev24, countyid, municipalityid
             FROM gfi_all 
             WHERE reportyear >= 2000;")  

data <- dbGetQuery(db, q)
dbDisconnect(db)





#----

sifranik <- read_excel("C:/Users/msagovac/Dropbox/Baza/db_wiiw_sample.xlsx", sheet = "Counties")

gradovi <- toupper(c("Gospić",
                     "Otočac",
                     "Senj",
                     "Obrovac",
                     "Karlobag",
                     "Lovinac",
                     "Perušić",
                     "Gračac",
                     "Jasenice",
                     "Starigrad",
                     "Ervenik"
))



filter <- sifranik %>%
  filter(NAZIV_OPC %in% gradovi) %>%
  rename(  municipalityid = OPCINA ) %>%
  select(municipalityid, NAZIV_OPC:NAZIV_ZUP)


mtp <- function(x) x * 1000000000



data %>%
  filter(municipalityid %in% unique(filter$municipalityid)) %>%
  group_by(reportyear, municipalityid) %>%
  summarise(n = n_distinct(subjectid),
            prihod = sum(prihod, na.rm = TRUE),
            izvoz = sum(Export, na.rm = TRUE),
            uvoz = sum(Import, na.rm = TRUE),
            k_dug = sum(k_dug, na.rm = TRUE),
            d_dug = sum(d_dug, na.rm = TRUE),
            rad = sum(employeecounteop, na.rm = TRUE),
            mim = sum(matimov, na.rm = TRUE),
            nim = sum (nematimov, na.rm = TRUE),
            placa = sum(placa_brutto, na.rm = TRUE),
            amortizacija = sum(amortizacija , na.rm = TRUE),
            fin_rashod = sum(financijski_trosak , na.rm = TRUE),
            tax_nett = sum(porez_dobit , na.rm = TRUE),
            nett = sum(nett , na.rm = TRUE),
            kapital = sum(kapital_rezerve , na.rm = TRUE),
            plata =  (sum (placa_brutto, na.rm = TRUE)/(sum (employeecounteop, na.rm = TRUE)))*1000000000/12)%>%
  mutate(dv = placa + amortizacija + fin_rashod + tax_nett + nett,
         imov = mim + nim, 
         invest = (imov + amortizacija) - dplyr:: lag(imov)) %>%
  left_join(., filter, by = "municipalityid" ) -> gradovi

gradovi %>%
  select(reportyear, NAZIV_OPC, n, rad, imov, prihod) %>%
  mutate_at(., c("imov", "prihod"), mtp)




#----



data %>%
  filter(municipalityid %in% unique(filter$municipalityid)) %>%
  group_by(reportyear) %>% 
  left_join(., filter, by = "municipalityid" ) -> firme




nazivi %>% mutate(
  type = map_chr(temeljniKapital, typeof)
)

naziv <- dF$naziv
mb <- dF$mb

dF <- data.frame(naziv, mb)
names(dF) <- c("naziv", "mb")
dF$naziv <- as.character(dF$naziv)

ff <- cbind(firme, df$naziv)

fulfirme <- merge(firme, nazivi[,c("godinaOsnivanja","adresa","naziv", "mb")], by.x = "subjectid", by.y = "mb", 
                  all.x = TRUE, all.y = FALSE) %>%
  unnest(.,naziv) %>%
  unnest(.,adresa) %>%
  unnest(.,godinaOsnivanja)


saveRDS(fulfirme, "fulFirme.rds")
ff <- readRDS(file = "fulFirme.rds")

fulfirme %>%
  mutate_if(is.list, map) %>%
  unnest() %>%
  glimpse()

fulfirme %>%
  unnest(.,naziv) %>%
  unnest(.,adresa) %>%
  unnest(.,godinaOsnivanja) -> ful





write.csv2(fulfirme, "C:/Users/msagovac/Dropbox/Pilar/PROTER/Simunic/grad_11_gfi.csv")

fulfirme <- read.csv2("C:/Users/msagovac/Dropbox/Pilar/PROTER/Simunic/grad_11_gfi.csv")
