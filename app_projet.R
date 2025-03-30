library(shiny)
library(bslib)
library(markdown)
library(shinyjs)
library(sf)
library(leaflet)
library(readxl)
library(ggmap)
library(ggplot2)
library(httr)
library(jsonlite)
library(tmaptools)
library(shinydashboard)
library(leaflet.extras)

data <- read_excel("~/Desktop/Projet turoré/RIPEC_202_PR_Anonymous.xlsx")

univ <- unique(data$`Libellé établissement`)
coord <- matrix(0, nrow = length(univ), ncol = 2)
occurrences <- integer(length(univ))
View(data)
coords_list <- lapply(univ, function(addr) geocode_OSM(addr))

for (i in 1:length(univ)) {
  if (!is.null(coords_list[[i]]$bbox)) {
    coord[i, 1] <- as.numeric(coords_list[[i]]$bbox["ymin"])
    coord[i, 2] <- as.numeric(coords_list[[i]]$bbox["xmin"])
  } else {
    coord[i, 1] <- NA
    coord[i, 2] <- NA
  }
  occurrences[i] <- sum(data$`Libellé établissement` == univ[i])
}

univ_df <- data.frame(
  Universite = univ,
  Latitude = coord[, 1],
  Longitude = coord[, 2],
  Occurrences = occurrences
)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  navbarPage(
    "App projet",
    tabPanel("Avis du rapporteur",
             uiOutput("avisPanel")
    ),
    tabPanel("Statistiques",
             leafletOutput("map"),
             column(6,
                    leafletOutput("map2", height = "400px")
    ),
    tabPanel("Connexion",
             card("Widget de connexion / déconnexion avec ID et mot de passe."),
             checkboxInput("checkConnecte", label = "Je suis connecté.e")
    )
  )
))

server <- function(input, output, session) {
  connected <- reactive({ input$checkConnecte })
  
  observeEvent(input$checkTermine, {
    if (length(input$checkGroupeModalites) == 0 || !(input$selectAvis %in% c("Très favorable", "Favorable", "Réservé"))) {
      alert("Il faut sélectionner des cases")
    } else {
      alert("Is ok. Save + marqué comme terminé.")
    }
  })
  
  output$avisPanel <- renderUI({
    if (connected()) {
      sidebarLayout(
        sidebarPanel(
          actionButton("buttonMenu", label = "Menu"),
          actionButton("buttonSave", label = "Sauvegarder"),
          actionButton("checkTermine", label = "J'ai terminé"),
          selectInput("selectAvis", label = "Avis", choices = list("", "Très favorable", "Favorable", "Réservé"), selected = NULL),
          checkboxGroupInput(
            "checkGroupeModalites",
            "Modalités d'attribution",
            choices = list("M1 (Ens)" = 1, "M2 (Rech)" = 2, "M3 (OIP)" = 3, "M4 (Diff)" = 4, "M5 (Eur)" = 5, "M6 (Coop)" = 6, "M7 (Invest)" = 7)
          ),
          actionButton("buttonUploadPreRapport", label = "Déposer un pré-rapport (PDF)")
        ),
        mainPanel(
          actionButton("buttonTelechargeDossier", label = "Télécharger le dossier du/de la candidat.e (PDF)"),
          card("PDF ou texte comprenant les informations de la candidature")
        )
      )
    } else {
      mainPanel("Vous n'êtes pas connecté. Connectez-vous pour accéder à cette page.")
    }
  })
  
  output$map <- renderLeaflet({
    leaflet(univ_df) %>%
      addTiles() %>%
      setView(lng = 2.5, lat = 46.5, zoom = 6) %>%
      setMaxBounds(
        lng1 = -5.142222, lat1 = 41.333740,
        lng2 = 9.561556, lat2 = 51.124199
      ) %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = ~sqrt(Occurrences) * 5,
        color = "red",
        fillOpacity = 0.5,
        popup = ~paste(Universite, "<br>", "Nombres de chercheurs ", Occurrences),
        label = ~paste(Occurrences)
      )
  })


output$map2 <- renderLeaflet({
  leaflet(univ_df) %>%
    addTiles() %>%
    setView(lng = 10, lat = 14.5, zoom = 7) %>%
    setMaxBounds(
      lng1 = 10, lat1 = 19,
      lng2 = -65, lat2 = -60
    ) %>%
    addCircleMarkers(
      lng = ~Longitude,
      lat = ~Latitude,
      radius = ~sqrt(Occurrences) * 5,
      color = "red",
      fillOpacity = 0.5,
      popup = ~paste(Universite, "<br>", "Nombres de chercheurs ", Occurrences),
      label = ~paste(Occurrences)
    )
})
}
shinyApp(ui = ui, server = server)
