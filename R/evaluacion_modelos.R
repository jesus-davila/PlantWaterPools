

library(tidyverse)
library(medfate)
library(broom)



# extraigo los datos simulados
setwd("C:/Users/MSI Modern/Documents/PlantWaterPools/data-raw/site_output")

files <- list.files(path="C:/Users/MSI Modern/Documents/PlantWaterPools/data-raw/site_output", pattern=".rds", all.files=FALSE,
                    full.names=FALSE, recursive = TRUE)%>% 
  lapply(readRDS)

setwd("C:/Users/MSI Modern/Documents/PlantWaterPools")

target <- list.files(path="C:/Users/MSI Modern/Documents/PlantWaterPools/data-raw/site_output", pattern=".rds", all.files=FALSE,
                     full.names=FALSE, recursive = TRUE)

names(files) <- target


# saco la transpiracion
data <- map(files, "Plants") |> 
  map("Transpiration")

data <- discard(data, ~ is.null(.x) || length(.x) == 0)

data <- purrr::map(data, ~as.data.frame(.x))

data <- purrr::imap(data, ~mutate(.x, plot = .y))

data <- purrr::map(data, ~rownames_to_column(.x, var="date"))

data <- purrr::map(data, ~gather(.x, key = cohorte, value = et, -plot, -date))

data <- bind_rows(data)

output_et <- data

# extraigo el LAI de los datos para corregir la ET

data <- map(files, "Plants") |> 
  map("LAI")

data <- discard(data, ~ is.null(.x) || length(.x) == 0)

data <- purrr::map(data, ~as.data.frame(.x))

data <- purrr::imap(data, ~mutate(.x, plot = .y))

data <- map(data, ~gather(.x, key = cohorte, value = lai, -plot)) 

data <- purrr::map(data, ~distinct(.x))

data <- bind_rows(data)

output_et <- merge(output_et, data)

# ¿valores 0 de LAI y ET?
output_et <- filter(output_et, lai > 0)
output_et <- filter(output_et, et > 0)

output_et <- mutate(output_et, et=et/lai)


# ahora saco los datos originales
setwd("C:/Users/MSI Modern/Documents/PlantWaterPools/data-raw/site_input")

files <- list.files(path="C:/Users/MSI Modern/Documents/PlantWaterPools/data-raw/site_input", pattern=".rds", all.files=FALSE,
                    full.names=FALSE, recursive = TRUE)%>% 
  lapply(readRDS)

setwd("C:/Users/MSI Modern/Documents/PlantWaterPools")

target <- list.files(path="C:/Users/MSI Modern/Documents/PlantWaterPools/data-raw/site_input", pattern=".rds", all.files=FALSE,
                     full.names=FALSE, recursive = TRUE)

names(files) <- target


# obtengo el SWC y la transpiracion
files <- map(files, function(item) {
  if (!is.null(item$measuredData)) {
    md <- as_tibble(item$measuredData)
    
    # si ya existe SWC, no hacemos nada
    if (!("SWC" %in% names(md))) {
      swc_matches <- grep("^SWC(?:[._-]?\\d+)?$", names(md), value = TRUE, ignore.case = FALSE)
      if (length(swc_matches) >= 1) {
        # renombrar la primera coincidencia a "SWC"
        names(md)[names(md) == swc_matches[1]] <- "SWC"
        
        # SI quieres eliminar otras variantes SWC.* (descomentar):
        # md <- md %>% select(-all_of(setdiff(swc_matches, swc_matches[1])))
      }
    }
    
    item$measuredData <- as.data.frame(md)
  }
  item
})

files <- imap(files, function(x, plot_name) {
  df <- as_tibble(x$measuredData) %>%
    rename(date = dates) %>%
    mutate(date = as.Date(date)) %>%
    mutate(plot = plot_name, .before = 1) %>%
    # eliminar columnas que contienen "_errr"
    select(-matches("_errr")) %>%
    select(-matches("_err")) %>%
    # mantener solo plot, Date, SWC y columnas que contienen "_T"
    select(plot, date, SWC, matches("_T"))
  
  return(df)
})

input <- map(files, ~gather(.x, key = cohorte, value = et, -1:-3)) 

input <- bind_rows(input)

output_et <- rename(output_et, et_predicted=et)

output <- output_et


input$plot <- gsub(".rds", "", input$plot)
output$plot <- gsub(".rds", "", output$plot)

input <- input %>% 
  separate(plot, sep="_", into=c("plot","b"), remove=FALSE)  %>%
  select(-b)

input <- input %>% 
  separate(cohorte, sep="_", into=c("specie","b", "c"), remove=FALSE)  %>%
  unite(col="cohorte", 
        c(b, c))

output <- output %>%
  mutate(
    model = str_extract(plot, "[^_]+$") ,
    plot = str_extract(plot, "^[^_]+"))

data <- merge(input, output) 


et <- select(data, plot, date, specie, cohorte, et, et_predicted, model)%>%
  na.omit() %>%
  distinct()


# evaluo los modelos con un GLM
library(glmmTMB)
library(performance)

# quito las fechas sin ET originales
et <- et %>%
  filter(et >= 0.001) %>%
  na.omit()

resultados <- et %>%
  group_by(plot, model) %>%
  do({
    modelo <- glmmTMB(et ~ et_predicted, data = ., family = gaussian())
    r2 <- r2_efron(modelo)
    tibble(R2 = r2)
  }) %>%
  ungroup()

# resultados en R2 de los distintos modelos y sitios
resultados %>%
  group_by(plot, model) %>%
  slice_max(R2, n = 1) %>%
  ungroup() 


# creacion del grafico entre observado y predicho

et <- et %>%
  mutate(doy = yday(date))  # doy = day of year

data_daily <- et %>%
  group_by(plot, model, doy) %>%
  summarise(
    et_mean = mean(et, na.rm = TRUE),
    et_predicted_mean = mean(et_predicted, na.rm = TRUE)
  ) %>%
  ungroup()

data_daily <- filter(data_daily, plot =="pu")

ggplot(data_daily, aes(x = doy)) +
  geom_line(aes(y = et_mean, color = "Observado"), size = 1) +
  geom_line(aes(y = et_predicted_mean, color = "Predicho"), size = 1, linetype = "dashed") +
  facet_wrap(~ model, scales = "free_y") +
  labs(
    x = "",
    y = "Transpiration",
    color = "Tipo"
  ) +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )



