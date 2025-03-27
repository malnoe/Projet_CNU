library(shiny)
library(bslib)
library(markdown)
library(shinyjs)
library(digest)
library(DT)      
library(readxl)
library(openxlsx)

users <- data.frame(
  username = c("user1", "user2"),
  password = sapply(c("password1", "password2"), digest), # Mots de passe hashés
  stringsAsFactors = FALSE
)

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
             fluidRow(
               column(4, offset = 4,
                      uiOutput("authUI"), # Interface conditionnelle
                      textOutput("login_status") # Message d'erreur ou succès
               )
             )
    )
  )
)

server <- function(input, output, session) {
  # État de connexion
  user <- reactiveVal(NULL)  # NULL = pas connecté
  
  # Gestion de l'interface de connexion
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
  
  # Gestion du message d'état
  login_status <- reactiveVal("") # Message réactif
  
  output$login_status <- renderText({
    login_status()
  })
  
  # Gestion de la connexion
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
  
  # Gestion de la déconnexion
  observeEvent(input$logout, {
    user(NULL)  # Réinitialiser l'état utilisateur
    shinyjs::alert("Déconnexion réussie !")
    # Effacer les champs pour éviter les résidus après déconnexion
    updateTextInput(session, "username", value = "")
    updateTextInput(session, "password", value = "")
    login_status("") # Réinitialiser le message d'état
  })
  
  # Bouton "Terminé"
  observeEvent(input$checkTermine,{
    if(length(input$checkGroupeModalites)==0 || !(input$selectAvis %in% c("Très favorable", "Favorable", "Réservé"))){
      alert("Il faut sélectionner des cases")
    }
    else{
      alert("Is ok. Save + marqué comme terminé.")
    }
  })
  
  
  candidateData <- reactive({
    req(input$excelFile)  # Vérifier que le fichier est téléchargé
    filePath <- input$excelFile$datapath
    data <- readxl::read_excel(filePath, sheet = 1)
    data[, 1:7]  # Sélectionner uniquement les 7 premières colonnes
  })
  
  observeEvent(input$loadData, {
    data <- candidateData()  # Lire les données avec les 7 colonnes
    updateSelectInput(session, "selectCandidate", choices = data$`Nom d´usage`)  
    
    output$candidateTable <- DT::renderDataTable({
      DT::datatable(data, selection = "single")  # Tableau 
    })
  })
  
  observe({
    selected <- input$candidateTable_rows_selected  # Ligne sélectionnée
    if (!is.null(selected)) {
      selectedCandidate <- candidateData()[selected, ]$'Nom d´usage' #  le nom du candidat sélectionné
      updateSelectInput(session, "selectCandidate", selected = selectedCandidate)
    }
  })
  
  # UI Page Avis rapporteur
  # UI
  output$avisPanel <- renderUI({
    if (!is.null(user())) {
      sidebarLayout(
        sidebarPanel(
          fileInput("excelFile", "Télécharger le fichier des candidatures", accept = c(".xlsx")),
          actionButton("loadData", "Charger les candidatures"),
          selectInput("selectCandidate", "Choisir un candidat", choices = NULL),
          selectInput("selectRapporteur", "Choisir un rapporteur :", 
                      choices = c("1" = 1, "2" = 2), selected = NULL),
          actionButton("buttonSave", label = "Sauvegarder"),
          actionButton("checkTermine", label = "J'ai terminé"),
          selectInput("selectAvis", label = "Avis", choices = list("", "Très favorable", "Favorable", "Réservé"), selected = NULL),
          checkboxGroupInput(
            "checkGroupeModalites",
            "Modalités d'attribution",
            choices = list("M1 (Ens)" = 1, "M2 (Rech)" = 2, "M3 (OIP)" = 3, "M4 (Diff)" = 4, "M5 (Eur)" = 5, "M6 (Coop)" = 6, "M7 (Invest)" = 7)
          ),
          actionButton("buttonUploadPreRapport", label = "Déposer un pré-rapport (PDF)"),
          uiOutput("downloadLink")
        ),
        mainPanel(
          DT::dataTableOutput("candidateTable")
        )
      )
    } else {
      mainPanel("Vous n'êtes pas connecté. Connectez-vous pour accéder à cette page.")
    }
  })
  
  
  observeEvent(input$buttonSave, {
    req(input$selectCandidate, input$selectRapporteur, input$selectAvis, input$checkGroupeModalites)
    
  
    filePath <- input$excelFile$datapath  
    data <- readxl::read_excel(filePath, sheet = 1)
    
  
    candidate_row <- which(data$`Nom d´usage` == input$selectCandidate)
    
    if (length(candidate_row) == 0) {
      shinyjs::alert("Candidat non trouvé dans le fichier.")
      return()
    }
    

    if (input$selectRapporteur == "1") {
      avis_col <- "Avis global R1"
      m_cols <- c("M1 (Ens) R1", "M2 (Rech) R1", "M3 (OIP) R1", "M4 (Diff) R1", "M5 (Eur) R1", "M6 (Coop) R1", "M7 (Invest) R1")
    } else if (input$selectRapporteur == "2") {
      avis_col <- "Avis global R2"
      m_cols <- c("M1 (Ens) R2", "M2 (Rech) R2", "M3 (OIP) R2", "M4 (Diff) R2", "M5 (Eur) R2", "M6 (Coop) R2", "M7 (Invest) R2")
    } else {
      shinyjs::alert("Rapporteur non valide.")
      return()
    }
    
  
    data[candidate_row, avis_col] <- input$selectAvis
    for (i in seq_along(m_cols)) {
      if (as.character(i) %in% input$checkGroupeModalites) {
        data[candidate_row, m_cols[i]] <- 1 
      } else {
        data[candidate_row, m_cols[i]] <- 0 
      }
    }
    
   
    outputFilePath <- tempfile(fileext = ".xlsx")  
    
    openxlsx::write.xlsx(data, outputFilePath, overwrite = TRUE)
    
    output$downloadLink <- renderUI({
      downloadButton("downloadFile", "Télécharger le fichier modifié")
    })
    
    output$downloadFile <- downloadHandler(
      filename = function() { "candidatures_modifiees.xlsx" },
      content = function(file) {
        file.copy(outputFilePath, file)
      }
    )
    
    shinyjs::alert("Les données ont été sauvegardées et un lien de téléchargement a été créé.")
  })
  
  
}

shinyApp(ui = ui, server = server)
