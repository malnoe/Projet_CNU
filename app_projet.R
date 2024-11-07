library(shiny)
library(bslib)
library(markdown)
library(shinyjs)


ui <- fluidPage(
  shinyjs::useShinyjs(),
  navbarPage(
  "App projet",
                 # Page où le rapporteur peut étudier le dossier + émettre son avis.
                 tabPanel("Avis du rapporteur",
                          uiOutput("avisPanel")
                 ),
                 
                 # Page contenant les statistiques sur les avis déjà émis.
                 tabPanel("Statistiques",
                          sidebarLayout(
                            sidebarPanel("Bouttons pour la sélection des stats à afficher"),
                            mainPanel("Graphiques / Informations statistiques liées aux candidatures")
                          )
                 ),
                 
                 # Page de connexion
                 tabPanel("Connexion",
                          card("Widget de connexion / déconnexion avec ID et mot de passe."),
                          checkboxInput("checkConnecte", label = "Je suis connecté.e")
                 )
)
)
server <- function(input, output, session) {
  # Variable réactive pour l'état de connexion
  connected <- reactive({ input$checkConnecte })
  
  # Bouton "Terminé"
  observeEvent(input$checkTermine,{
    if(length(input$checkGroupeModalites)==0 || !(input$selectAvis %in% c("Très favorable", "Favorable", "Réservé"))){
      alert("Il faut sélectionner des cases")
    }
    else{
      alert("Is ok. Save + marqué comme terminé.")
    }
  })
  
  # UI Page Avis rapporteur
  output$avisPanel <- renderUI({
    if (connected()) {
      # Boutons et éléments pour l'avis
      sidebarLayout(
        sidebarPanel(
          actionButton("buttonMenu", label = "Menu"),
          actionButton("buttonSave", label = "Sauvegarder"),
          actionButton("checkTermine", label = "J'ai terminé"),
          selectInput("selectAvis", label = "Avis", choices = list("","Très favorable", "Favorable", "Réservé"), selected = NULL),
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
      # Message de non-connexion
      mainPanel("Vous n'êtes pas connecté. Connectez-vous pour accéder à cette page.")
    }
  })
}

shinyApp(ui = ui, server = server)