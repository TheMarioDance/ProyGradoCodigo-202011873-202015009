
pacman::p_load(tidyverse, magrittr, scales, ggtext, tidyr, dplyr, lubridate)


datos<- readxl::read_excel("Graficas Percentiles.xlsx", sheet = "PercentilesOTM", range = "C28:I42")


funcion_orden_datos <- function(pDatos){
  
  pDatos %<>% mutate(Mes = Mes %>% as.Date())
  
  pDatos %<>% pivot_longer(-Mes, names_to = "Escenario", values_to = "Ingreso")
  
  pDatos %<>% separate_wider_delim(Escenario, ";", names = c("Escenario", "Cobertura"))
  
  pDatos %<>% pivot_wider(names_from = Escenario, values_from = Ingreso)
  
  return(pDatos)
  
}


Ingresos<-funcion_orden_datos(datos)

vector_fechas <- Ingresos$Mes %>% unique() %>% update(day = 28)
vector_fechas <- c(vector_fechas, vector_fechas %>% min() %>% update(day = 1))

colores <- c('#53868B','#C1CDCD','#CD919E')
names(colores) <- c("Sin coberturas", "Con coberturas FWD", "Con coberturas Opciones")

Ingresos %<>% mutate(xmin = case_when(
  Cobertura == "Sin coberturas" ~ Mes %>% update(day = 5),
  TRUE ~ Mes %>% update(day = 17)
)) %>% 
  mutate(xmax = case_when(
    Cobertura == "Con coberturas FWD" ~ Mes %>% update(day = 24),
    Cobertura == "Con coberturas Opciones" ~ Mes %>% update(day = 12),
    TRUE ~ Mes %>% update(day = 12)
  ))


funcion_grafica <- function(pDatos, pVectorFechas){
  pDatos$xmin <- as.Date(pDatos$xmin)
  pDatos$xmax <- as.Date(pDatos$xmax)
  
  pDatos$medio_x <- rowMeans(cbind(pDatos$xmin, pDatos$xmax))
  
  orden_leyendas <-c("Sin coberturas", "Con coberturas Opciones", "Con coberturas FWD")
  
  pDatos$Cobertura<- factor(pDatos$Cobertura, levels = orden_leyendas)
  
  grafica <- ggplot(pDatos,
                    aes(xmin = xmin, xmax = xmax, ymin = `Peor Escenario`, ymax = `Mejor Escenario`,
                        fill = Cobertura))+
    theme_bw()+
    geom_rect(alpha = 0.9)+
    
    scale_x_date(breaks = pVectorFechas %>% update(day = 15) %>% unique(),
                 labels = scales::date_format("%b-%y"))+
    
    theme(axis.ticks = element_blank(), legend.title = element_blank(), 
          legend.position = "bottom",panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          axis.title.x = element_blank())+
    
    scale_y_continuous(labels = function(x) scales::comma_format()(round(x)))+
    
    ylab("Utilidad Bruta (Millones USD)")+
    
    scale_fill_manual(values = colores)+
    
    geom_vline(xintercept = as.numeric(as.Date(pVectorFechas)), 
               linetype = "dashed", color = "black", alpha = 0.5) +
    
    geom_label(data = subset(pDatos, !(Cobertura %in% c("Con coberturas FWD", "Con coberturas Opciones") & Mes <= as.Date('2025-05-01'))),
               aes(x = xmin + (xmax - xmin)/2, y = `Mejor Escenario`, 
                   label = scales::comma_format()(round(`Mejor Escenario`))), 
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2024-06-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Mejor Escenario` - 0.15 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Mejor Escenario`))), 
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2024-07-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Mejor Escenario` - 0.11 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Mejor Escenario`))), 
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2024-08-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Mejor Escenario` - 0.09 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Mejor Escenario`))), 
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2024-09-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Mejor Escenario` - 0.08 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Mejor Escenario`))), 
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2024-10-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Mejor Escenario` - 0.07 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Mejor Escenario`))), 
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2024-11-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Mejor Escenario` - 0.06 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Mejor Escenario`))), 
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2024-12-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Mejor Escenario` - 0.05 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Mejor Escenario`))), 
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2025-01-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Mejor Escenario` - 0.04 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Mejor Escenario`))), 
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2025-02-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Mejor Escenario` - 0.04 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Mejor Escenario`))), 
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2025-03-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Mejor Escenario` - 0.03 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Mejor Escenario`))), 
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2025-04-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Mejor Escenario` - 0.02 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Mejor Escenario`))), 
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2025-05-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Mejor Escenario` - 0.01 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Mejor Escenario`))), 
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas FWD" & Mes <= as.Date('2025-05-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Mejor Escenario` - 0.0000001 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Mejor Escenario`))), 
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, !(Cobertura %in% c("Con coberturas FWD", "Con coberturas Opciones") & Mes <= as.Date('2025-05-01'))),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario`, 
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2024-06-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` - 0.15 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2024-07-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` - 0.11 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2024-08-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` + 0.065 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2024-09-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` + 0.01 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2024-10-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` - 0.04 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2024-11-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` - 0.05 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2024-12-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` - 0.05 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2025-01-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` - 0.04 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2025-02-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` - 0.03 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2025-03-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` - 0.02 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2025-04-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` - 0.01 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas Opciones" & Mes == as.Date('2025-05-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` - 0.01 * (`Mejor Escenario` - `Peor Escenario`), 
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +

    geom_label(data = subset(pDatos, Cobertura == "Con coberturas FWD" & Mes == as.Date('2024-06-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` - 0.35 * (`Mejor Escenario` - `Peor Escenario`),
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +

    geom_label(data = subset(pDatos, Cobertura == "Con coberturas FWD" & Mes == as.Date('2024-07-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` - 0.49 * (`Mejor Escenario` - `Peor Escenario`),
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas FWD" & Mes == as.Date('2024-08-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` + 0.35 * (`Mejor Escenario` - `Peor Escenario`),
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas FWD" & Mes == as.Date('2024-09-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` + 0.19 * (`Mejor Escenario` - `Peor Escenario`),
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas FWD" & Mes == as.Date('2024-10-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` + 0.05 * (`Mejor Escenario` - `Peor Escenario`),
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas FWD" & Mes == as.Date('2024-11-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` + 0.000000001 * (`Mejor Escenario` - `Peor Escenario`),
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas FWD" & Mes == as.Date('2024-12-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` + 0.000000001 * (`Mejor Escenario` - `Peor Escenario`),
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas FWD" & Mes == as.Date('2025-01-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` + 0.000000001 * (`Mejor Escenario` - `Peor Escenario`),
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas FWD" & Mes == as.Date('2025-02-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` + 0.000000001 * (`Mejor Escenario` - `Peor Escenario`),
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas FWD" & Mes == as.Date('2025-03-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` + 0.000000001 * (`Mejor Escenario` - `Peor Escenario`),
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas FWD" & Mes == as.Date('2025-04-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` + 0.000000001 * (`Mejor Escenario` - `Peor Escenario`),
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +
    
    geom_label(data = subset(pDatos, Cobertura == "Con coberturas FWD" & Mes == as.Date('2025-05-01')),
               aes(x = xmin + (xmax - xmin)/2, y = `Peor Escenario` + 0.000000001 * (`Mejor Escenario` - `Peor Escenario`),
                   label = scales::comma_format()(round(`Peor Escenario`))),
               fill = "White", color = "black",
               vjust = 0.5, size = 3) +

    ggtitle(str_c("Utilidad Bruta")) +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(grafica)
  
  
  
}


graficaOTM <- funcion_grafica(Ingresos,vector_fechas)

ggsave(plot = graficaOTM,
       filename = "Utilidad Bruta OTM.png",
       width = 8, height = 5)

