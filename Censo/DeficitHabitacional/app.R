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

# 1. Importing the data ----
## First we set the working directory
setwd("C:/Users/User/OneDrive - MIGRACIÓN VIDENZA/1. Proyectos/1. Proyectos actuales/25. ASEI - Propuestas Vivienda/0. Insumos/CENSO/Mapas/Insumos")

## Now we import the dataset that it is in a CSV format
listTabs <- c('deficit_cualitativo_departamento', 'deficit_cualitativo_distrital', 'deficit_cuantitativo_departamento',
              'deficit_cuantitativo_distrital', 'deficit_habitacional_departamento', 'deficit_habitacional_distrital')

for (file in listTabs){
  
  # create file name using paste0 function
  file_name <- paste0(file, ".csv")
  
  alternative <- read.csv(file_name, sep = ";") %>% 
    mutate(ubigeo = str_pad(ubigeo, width = 6, side = "left", pad = "0"))
  
  assign(paste0(file), alternative)
}

listDpto <-  deficit_cualitativo_departamento %>%
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
          
          map_shiny <- merge(x = map_peru, y = deficit_habitacional_departamento, by = "ubigeo", all.x = TRUE)
          
          mapa <- map_shiny %>% 
            ggplot() +
            aes(geometry = geometry) +
            geom_sf(aes(fill = deficit_habitacional), linetype = 1,
                    lwd = 0.25) +
            theme_minimal()+
            theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank()) +
            scale_fill_gradientn(colors = custom_palette, name = "% de hogares") 
          ggplotly(mapa)
          
        } else if (input$Indicador == "Déficit Cualitativo"){
          
          map_shiny <- merge(x = map_peru, y = deficit_cualitativo_departamento, by = "ubigeo", all.x = TRUE)
          
          mapa <- map_shiny %>% 
            ggplot() +
            aes(geometry = geometry) +
            geom_sf(aes(fill = deficit_cuali_departamento), linetype = 1,
                    lwd = 0.25) +
            theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())+
            scale_fill_gradientn(colors = custom_palette, name = "% de hogares")
          ggplotly(mapa)
          
        } else {
          
          map_shiny <- merge(x = map_peru, y = deficit_cuantitativo_departamento, by = "ubigeo", all.x = TRUE)
          
          mapa <- map_shiny %>% 
            ggplot() +
            aes(geometry = geometry) +
            geom_sf(aes(fill = deficit_cuanti_departamento), linetype = 1,
                    lwd = 0.25) +
            theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())+
            scale_fill_gradientn(colors = custom_palette, name = "% de hogares")
          
          ggplotly(mapa)
        }
        
      
        
      } else if  (input$Nivel == "Provincial"){
        
      } else {
        map_peru <- map_DIST  %>%  #Cargamos la base de datos sobre los departamentos del Peru
          rename(ubigeo = COD_DISTRITO ) #renombramos la variable del DF para el merge por UBIGEO
        
        if (input$Indicador == "Déficit Habitacional"){
          
          map_shiny <- merge(x = map_peru, y = deficit_habitacional_distrital, by = "ubigeo", all.x = TRUE)
          
          mapa <- map_shiny %>% 
            ggplot() +
            aes(geometry = geometry) +
            geom_sf(color = "white", aes(fill = deficit_habitacional), linewidth = 0) +
            theme_minimal() +
            theme(
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank()
            ) +
            scale_fill_gradientn(colors = custom_palette, name = "% de hogares")
          
          ggplotly(mapa)
          
          
        } else if (input$Indicador == "Déficit Cualitativo"){
          
          map_shiny <- merge(x = map_peru, y = deficit_cualitativo_distrital, by = "ubigeo", all.x = TRUE) %>% 
            mutate(deficit_cualitativo = as.numeric(deficit_cualitativo))
          
          mapa <- map_shiny %>% 
            ggplot() +
            aes(geometry = geometry) +
            geom_sf(color = "white", aes(fill = deficit_cualitativo), linewidth = 0) +
            theme_minimal() +
            theme(
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank()
            ) +
            scale_fill_gradientn(colors = custom_palette, name = "% de hogares")
          
          ggplotly(mapa)
          
        } else {
          
          map_shiny <- merge(x = map_peru, y = deficit_cuantitativo_distrital, by = "ubigeo", all.x = TRUE) %>% 
            mutate(deficit_cuantitativo = as.numeric(deficit_cuantitativo))

          mapa <- map_shiny %>% 
            ggplot() +
            aes(geometry = geometry) +
            geom_sf(color = "white", aes(fill = deficit_cuantitativo), linewidth = 0) +
            theme_minimal() +
            theme(
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank()
            ) +
            scale_fill_gradientn(colors = custom_palette, name = "% de hogares")
          
          ggplotly(mapa)
          
        }
      }
      

      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
