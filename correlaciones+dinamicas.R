# Script para correlaciones dinámicas de PIB con agregados monetarios
# Replicando ejercicio de MonetaryTheoryAndPolicy de Walsh
# Monetary Policy


# Importando librerias

rm(list = ls())
library(tidyverse)

# Importando bases ----
  library(readxl)
  agregados <- read_xlsx("Bases/AgregadosMonetarios.xlsx", skip=3)
    agregados[-1:-14,] %>% select(M1, M1A, M2, M2A, M3A) -> agregados
    drop_na(agregados) -> agregados
    agregados <- mutate_all(agregados, function(x) as.numeric(as.character(x)))
    
  basemonetaria <- read_xlsx("Bases/Base Monetaria.xlsx", skip=3)
  añoseliminados <- c(2002, 2003, 2004, 2005, 2006)
  basemonetaria <- subset(basemonetaria, `Año y mes` != 2002 & 
                                         `Año y mes` != 2003 &
                                         `Año y mes` != 2004 &
                                         `Año y mes` != 2005 &
                                         `Año y mes` != 2006)
    basemonetaria %>% select(...5, ...9, `Base monetaria`) -> basemonetaria
    basemonetaria[-1:-8,] %>% rename(Emision = ...5,
                             EncajeMN = ...9,
                             BaseMonetaria = 'Base monetaria') -> basemonetaria
    drop_na(basemonetaria) -> basemonetaria
    basemonetaria <- mutate_all(basemonetaria, function(x) as.numeric(as.character(x)))
    
    
    pibt <- read_xlsx("Bases/PIBT.xlsx")
    pibt[-1:-9,] %>% select(...3, ...7, ...10) -> pibt
    pibt %>% rename(pib_o = ...3,
                    pib_d = ...7,
                    pib_tc = ...10) -> pibt
    drop_na(pibt) -> pibt
    pibt <- mutate_all(pibt, function(x) as.numeric(as.character(x)))
    
  

# Creando variables
    library(stats)
    M1 <- ts(agregados$M1, start = c(2002,1), frequency = 12)
    M1 <- aggregate.ts(M1, nfrequency = 4, FUN = mean)
    
    M2 <- ts(agregados$M2, start = c(2002,1), frequency = 12)
    M2 <- aggregate.ts(M2, nfrequency = 4, FUN = mean)
    
    M0 <- ts(basemonetaria$Emision, start = c(2002, 1), frequency = 12)
    M0 <- aggregate.ts(M0, nfrequency = 4, FUN = mean)
    
    EncajeMN <- ts(basemonetaria$EncajeMN, start= c(2002, 1), frequency = 12)
    EncajeMN <- aggregate.ts(EncajeMN, nfrequency = 4, FUN = mean)
    
    BaseMonetaria <- ts(basemonetaria$BaseMonetaria, start= c(2002, 1), frequency = 12)
    BaseMonetaria <- aggregate.ts(BaseMonetaria, nfrequency = 4, FUN = mean)
    
    pib <- ts(pibt$pib_d, start=c(2006, 1), frequency = 4)
    
# Desestacionalizando y generando ciclos
    
    library(seasonal)
    library(mFilter)
    
    ciclo <- function(variable, añoinicio, frequencia){
      x <- log(variable)
      x <- final(seas(x,x11=""))
      xhp <- hpfilter(x, freq=1125, type="frequency")
      x <- xhp$cycle
      x <- ts(x, start=c(añoinicio, 1), frequency = frequencia)
      variable <- window(x, start=c(2006,1))
    }
    
    M0 <- ciclo(M0, 2002, 4)
    M1 <- ciclo(M1, 2002, 4)
    M2 <- ciclo(M2, 2002, 4)
    BaseMonetaria <- ciclo(BaseMonetaria, 2002, 4)
    EncajeMN <- ciclo(EncajeMN, 2002, 4)
    pib <- ciclo(pib, 2006, 4)

    
   # datos <- as.data.frame(NA)
    
    guardar <- function(x){
      var <- ccf(x, pib, type= "correlation", 12)
      var_ <- var$acf
      variable <- var_
    }
    
    
    M0q <- guardar(M0)
    M1q <- guardar(M1)
    M2q <- guardar(M2)
    BaseMonetariaq <- guardar(BaseMonetaria)
    EncajeMNq <- guardar(EncajeMN)    
    
    lags <- seq(from=-12, to=12)
    datos <- data.frame(M0q, M1q, M2q, BaseMonetariaq, EncajeMNq, lags)
    datos <- gather(datos, Variables, Correlacion, M0q:EncajeMNq)
    
    
    graph <- ggplot(data=datos, aes(x = lags, y = Correlacion, color = Variables)) +
      geom_line(size=1.5, linejoin="round")+
      geom_vline(xintercept = 0, color="darkgray")+
  #    theme(legend.text = element_text(size=30))+
      theme_minimal(base_size=35)
    plot(graph)
    
    library(Cairo)
    #/ Exportando
    png(filename="Salidas/correlaciondinamica.png",
        type="cairo",
        units="in", 
        width=16, 
        height=8, 
        pointsize=12, 
        res=500)
    print(graph)
    dev.off()
    