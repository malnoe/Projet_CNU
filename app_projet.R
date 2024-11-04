library(shiny)
library(shinyjs)
library(bslib)
library(markdown)

ui <- navbarPage("App projet",
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
                          checkboxInput("checkConnecte", label = "Je suis connecté.e",value=TRUE)
                 )
)

server <- function(input, output, session) {
  # Variable réactive pour l'état de connexion
  connected <- reactive({ input$checkConnecte }) # Temporaire avant de mettre un vrai widget d'authentification
  
  # UI Page Avis rapporteur
  output$avisPanel <- renderUI({
    if (connected()) {
      # Boutons et éléments pour l'avis
      sidebarLayout(
        sidebarPanel(
          # Boutton pour retourner au menu.
          actionButton("buttonMenu", label = "Menu"),
          
          # Boutton pour sauvegarder le travail -> push les données sur la base de données excel.
          actionButton("buttonSave", label = "Sauvegarder"),
          
          # Checkbox pour mettre l'avis comme validé (possibilité de le décocher) -> affichage dans le menu + sauvegarder automatiquement.
          checkboxInput("checkValidation", label = "Valider votre travail"),
            # Message si la validation est possible ou non
            textOutput("message"),
          
            tags$style(HTML("#error { color: red; }")),
            textOutput("error"),
          
          # Menu déroulant choix de l'avis.
          selectInput("selectAvis", label = "Avis", choices = list("Sélectionner", "Très favorable", "Favorable", "Réservé"), selected = NULL),
          
          # Choix multiple modalités d'attribution
          checkboxGroupInput(
            "checkGroupeModalites",
            "Modalités d'attribution",
            choices = list("M1 (Ens)" = 1, "M2 (Rech)" = 2, "M3 (OIP)" = 3, "M4 (Diff)" = 4, "M5 (Eur)" = 5, "M6 (Coop)" = 6, "M7 (Invest)" = 7)
          ),
          
          # Boutton pour uploader un fichier PDF
          actionButton("buttonUploadPreRapport", label = "Déposer un pré-rapport (PDF)")
        ),
        mainPanel(
          # Boutton pour télécharger le dossier de candidature
          actionButton("buttonTelechargeDossier", label = "Télécharger le dossier de candidature (PDF)"),
          
          # Affichage du PDF récap candidature.
          card("PDF ou texte comprenant les informations de la candidature")
        )
      )
    } else {
      # Message de non-connexion
      mainPanel("Vous n'êtes pas connecté. Connectez-vous pour accéder à cette page.")
    }
  })
  
  # Gestion permission de valider le travail
  valid_values <- c("Très favorable", "Favorable", "Réservé")
  observe({
    if (input$selectAvis %in% valid_values && length(input$checkGroupeModalites) > 0) {
      # Activer la checkbox si les conditions sont remplies
      enable("checkbox")
      output$error <- renderText("")  # Cacher le message d'erreur
    } else {
      # Désactiver la checkbox et décocher si les conditions ne sont pas remplies
      disable("checkbox")
      updateCheckboxInput(session, "checkValidation", value = FALSE)
      output$error <- renderText("Vous devez sélectionner un avis et au moins une modalité pour valider.")
    }
  })
    # Message pour informer l'utilisateur qu'il peut valider
  output$message <- renderText({
    if (input$selectAvis %in% valid_values && length(input$checkGroupeModalites) > 0) {
      "Vous pouvez valider votre travail. (Vous pourrez encore le modifier.)"
    } else {
      ""
    }
  })
  
}

shinyApp(ui = ui, server = server)