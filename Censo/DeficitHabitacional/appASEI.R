# 0. Importing and calling libraries ----
library(tidyverse)
library(foreign)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinycssloaders)
library(mapsPERU)
library(sf)
library(scales)
library(plotly)
library(classInt)

# 1. Importing the data ----
## First we set the working directory
setwd("C:/Users/User/OneDrive - VIDENZA/1. Proyectos Videnza/1. Proyectos actuales/25. ASEI - Propuestas Vivienda/0. Insumos/CENSO")

## Now we import the dataset that is in an excel format
file_name <- "bd_censo_indicadores.xlsx"
data <- read_excel(file_name)

listDpto <-  deficit_cualitativo_departamento %>%
  select(ubigeo)  %>%  
  distinct()  %>%  
  arrange(ubigeo) %>% 
  drop_na()

listProv <-  deficit_cualitativo_provincial %>%
  select(ubigeo)  %>%  
  distinct()  %>%  
  arrange(ubigeo) %>% 
  drop_na()

listDist <-  deficit_cualitativo_distrital %>%
  select(ubigeo)  %>%  
  distinct()  %>%  
  arrange(ubigeo) %>% 
  drop_na()

listIndicadores <- c("Déficit Cualitativo", "Déficit Cuantitativo", "Déficit Habitacional")


#1.  Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("Dashboard ASEI", theme = shinytheme("lumen"),
             tabPanel("Mapa interactivo", fluid = TRUE, icon = icon("globe-americas"),
                      tags$head(
                        tags$style(HTML("
                        .container-fluid {
                          padding: 0;
                        }
                        .full-page {
                          height: calc(100vh - 50px); /* Adjusting for navbar height */
                          width: 50vw;
                        }
                      "))),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Seleccione el nivel de análisis"),
                          selectInput(inputId = "Nivel",
                                      label = "Elija el nivel de análisis",
                                      choices = c("Departamental", "Provincial", "Distrital"),
                                      selected = "Departamental",
                                      width = "220px"
                          ),
                          selectInput(inputId = "Dpto",
                                      label = "Elija el departamento",
                                      choices = listDpto,
                                      selected = "TODOS",
                                      width = "220px"
                          ),
                          selectInput(inputId = "Prov",
                                      label = "Elija la provincia",
                                      choices = c("Departamental", "Provincial", "Distrital"),
                                      selected = "TODOS",
                                      width = "220px"
                          ),
                          selectInput(inputId = "Dist",
                                      label = "Elija el distrito",
                                      choices = listDist,
                                      selected = "TODOS",
                                      width = "220px"
                          ),
                          selectInput(inputId = "Indicador",
                                      label = "Seleccione el indicador",
                                      choices = listIndicadores,
                                      selected = "Déficit Habitacional",
                                      width = "220px"
                          )),
                        mainPanel(
                          div(
                            class = "full-page",
                            plotlyOutput("map1", height = "100%", width = "100%")
                          )
                        ))),
             tabPanel("Ficha técnica", fluid = TRUE, icon = icon("chart-bar")
                      #
             )
             
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map1 <- renderPlotly({
      
      custom_palette <- c("#2dc937", "#e7b416", "#cc3232")
      
     
      if (input$Nivel == "Departamental"){
        map_peru <- map_DEP  %>%  #Cargamos la base de datos sobre los departamentos del Peru
          rename(ubigeo = COD_DEPARTAMENTO ) #renombramos la variable del DF para el merge por UBIGEO
        
        if (input$Indicador == "Déficit Habitacional"){
          
          map_shiny <- merge(x = map_peru, y = deficit_habitacional_departamento, by = "ubigeo", all.x = TRUE)  %>% 
            mutate(deficit_habitacional = as.numeric(deficit_habitacional))
            
          breaks <- classIntervals(map_shiny$deficit_habitacional, n = 3, style = "jenks")
          map_shiny$jenks_breaks <- cut(map_shiny$deficit_habitacional, breaks$brks, include.lowest = TRUE)
          
          
          mapa <- map_shiny %>% 
            ggplot() +
            aes(geometry = geometry) +
            geom_sf(aes(fill = jenks_breaks), linetype = 1,
                    lwd = 0.25) +
            theme_minimal()+
            theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
            scale_fill_manual(values = custom_palette, name = "Porcentaje de hogares (%)")
          ggplotly(mapa)
          
        } else if (input$Indicador == "Déficit Cualitativo"){
          
          map_shiny <- merge(x = map_peru, y = deficit_cualitativo_departamento, by = "ubigeo", all.x = TRUE)%>% 
            mutate(deficit_cuali = as.numeric(deficit_cuali_departamento))
          
          breaks <- classIntervals(map_shiny$deficit_cuali, n = 3, style = "jenks")
          map_shiny$jenks_breaks <- cut(map_shiny$deficit_cuali, breaks$brks, include.lowest = TRUE)
          
          
          mapa <- map_shiny %>% 
            ggplot() +
            aes(geometry = geometry) +
            geom_sf(aes(fill = jenks_breaks), linetype = 1,
                    lwd = 0.25) +
            theme_minimal()+
            theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
            scale_fill_manual(values = custom_palette, name = "Porcentaje de hogares (%)")
          ggplotly(mapa)
          
          
        } else {
          
          map_shiny <- merge(x = map_peru, y = deficit_cuantitativo_departamento, by = "ubigeo", all.x = TRUE)%>% 
            mutate(deficit_cuanti = as.numeric(deficit_cuanti_departamento))
          
          breaks <- classIntervals(map_shiny$deficit_cuanti, n = 3, style = "jenks")
          map_shiny$jenks_breaks <- cut(map_shiny$deficit_cuanti, breaks$brks, include.lowest = TRUE)
          
          
          mapa <- map_shiny %>% 
            ggplot() +
            aes(geometry = geometry) +
            geom_sf(aes(fill = jenks_breaks), linetype = 1,
                    lwd = 0.25) +
            theme_minimal()+
            theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
            scale_fill_manual(values = custom_palette, name = "Porcentaje de hogares (%)")
          ggplotly(mapa)
          
        }
        
      
        
      } else if  (input$Nivel == "Provincial"){
        
      } else {
        map_peru <- map_DIST  %>%  #Cargamos la base de datos sobre los departamentos del Peru
          rename(ubigeo = COD_DISTRITO ) #renombramos la variable del DF para el merge por UBIGEO
        
        if (input$Indicador == "Déficit Habitacional"){
          
          map_shiny <- merge(x = map_peru, y = deficit_habitacional_distrital, by = "ubigeo", all.x = TRUE)%>% 
            mutate(deficit_habitacional = as.numeric(deficit_habitacional))
          
          breaks <- classIntervals(map_shiny$deficit_habitacional, n = 3, style = "jenks")
          map_shiny$jenks_breaks <- cut(map_shiny$deficit_habitacional, breaks$brks, include.lowest = TRUE)
          
          mapa <- map_shiny %>% 
            ggplot() +
            aes(geometry = geometry) +
            geom_sf(color = "black", aes(fill = jenks_breaks), linewidth = 0) +
            theme_minimal() +
            theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
            scale_fill_manual(values = custom_palette, name = "Porcentaje de hogares (%)") +
            geom_sf(data = provinces, fill = NA, color = "black", lwd = 0.5) +
            geom_sf(data = departments, fill = NA, color = "black", lwd = 1)
          
          ggplotly(mapa)
          
          
        } else if (input$Indicador == "Déficit Cualitativo"){
          
          map_shiny <- merge(x = map_peru, y = deficit_cualitativo_distrital, by = "ubigeo", all.x = TRUE) %>% 
            mutate(deficit_cualitativo = as.numeric(deficit_cualitativo))
          
          breaks <- classIntervals(map_shiny$deficit_cualitativo, n = 3, style = "jenks")
          map_shiny$jenks_breaks <- cut(map_shiny$deficit_cualitativo, breaks$brks, include.lowest = TRUE)
          
          mapa <- map_shiny %>% 
            ggplot() +
            aes(geometry = geometry) +
            geom_sf(color = "black", aes(fill = jenks_breaks), linewidth = 0) +
            theme_minimal() +
            theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
            scale_fill_manual(values = custom_palette, name = "Porcentaje de hogares (%)") +
            geom_sf(data = provinces, fill = NA, color = "black", lwd = 0.5) +
            geom_sf(data = departments, fill = NA, color = "black", lwd = 1)
          
          ggplotly(mapa)
          
        } else {
          
          map_shiny <- merge(x = map_peru, y = deficit_cuantitativo_distrital, by = "ubigeo", all.x = TRUE) %>% 
            mutate(deficit_cuantitativo = as.numeric(deficit_cuantitativo))

          breaks <- classIntervals(map_shiny$deficit_cuantitativo, n = 3, style = "jenks")
          map_shiny$jenks_breaks <- cut(map_shiny$deficit_cuantitativo, breaks$brks, include.lowest = TRUE)
          
          mapa <- map_shiny %>% 
            ggplot() +
            aes(geometry = geometry) +
            geom_sf(color = "black", aes(fill = jenks_breaks), linewidth = 0) +
            theme_minimal() +
            theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
            scale_fill_manual(values = custom_palette, name = "Porcentaje de hogares (%)") +
            geom_sf(data = provinces, fill = NA, color = "black", lwd = 0.5) +
            geom_sf(data = departments, fill = NA, color = "black", lwd = 1)
          
          ggplotly(mapa)
          
        }
      }
      

      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
