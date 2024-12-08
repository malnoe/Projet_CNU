library(shiny)
library(bslib)
library(markdown)
library(shinyjs)
library(digest)

users <- data.frame(
  username = c("user1", "user2"),
  password = sapply(c("password1", "password2"), digest), # Mots de passe hashés
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  navbarPage(
    "App projet",
    # Page de connexion
    tabPanel("Connexion",
             fluidRow(
               column(4, offset = 4,
                      uiOutput("authUI"), # Interface conditionnelle
                      textOutput("login_status") # Message d'erreur ou succès
               )
             )
    ),
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
    )
    
    
  )
)

server <- function(input, output, session) {
  # Page connexion - État de connexion
  user <- reactiveVal(NULL)  # NULL = pas connecté
  
  # Page connexion - Gestion de l'interface de connexion
  output$authUI <- renderUI({
    if (is.null(user())) {
      # Afficher les champs pour se connecter si non connecté
      tagList(
        textInput("username", "Nom d'utilisateur"),
        passwordInput("password", "Mot de passe"),
        actionButton("login", "Connexion")
      )
    } else {
      # Afficher le bouton de déconnexion si connecté
      actionButton("logout", "Déconnexion")
    }
  })
  
  # Page connexion - Gestion du message d'état de connexion
  login_status <- reactiveVal("") # Message réactif
  
  output$login_status <- renderText({
    login_status()
  })
  
  # Page conenxion - Gestion de la connexion
  observeEvent(input$login, {
    req(input$username, input$password)  # Les champs ne doivent pas être vides
    
    # Vérification des identifiants
    matched_user <- users[users$username == input$username, ]
    if (nrow(matched_user) == 1 && 
        digest(input$password) == matched_user$password) {
      user(input$username)  # Stocker l'utilisateur connecté
      shinyjs::alert("Connexion réussie !")
      
      # Effacer le message d'erreur et réinitialiser les champs
      login_status("") 
      updateTextInput(session, "username", value = "")
      updateTextInput(session, "password", value = "")
    } else {
      login_status("Identifiants incorrects")
      
      # Effacer le message d'erreur après 3 secondes
      shinyjs::delay(3000, login_status(""))
    }
  })
  
  # Page connexion - Gestion de la déconnexion
  observeEvent(input$logout, {
    user(NULL)  # Réinitialiser l'état utilisateur
    shinyjs::alert("Déconnexion réussie !")
    # Effacer les champs pour éviter les résidus après déconnexion
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
    login_status("") # Réinitialiser le message d'état
  })
  
  # Page Avis rapporteur - Bouton "Terminé"
  observeEvent(input$checkTermine,{
    if(!(input$selectAvis %in% c("Très favorable", "Favorable", "Réservé")) #Aucun avis sélectionné
      ){
      alert("Merci de sélectionner un avis.")
    }
    else if(input$selectAvis %in% c("Très favorable", "Favorable") && length(input$checkGroupeModalites)==0 #Pas de modalité sélectionnée
            ){
      alert("Merci de sélectionner des modalités.")
    }
    else{
      alert("Votre avis a bien été sauvegardé.")
    }
  })
  
  # Page Avis rapporteur - UI
  output$avisPanel <- renderUI({
    if (!is.null(user())) {
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
