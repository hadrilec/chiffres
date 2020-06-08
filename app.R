##l'application chiffres de la Note a pour objectif de regrouper tous les chiffres de la Note en un seul endroit, garantissant en plus
##la fraicheur des donnees, toujours synchronisees avec les toutes dernieres sorties

library(shiny)
library(lubridate)##necessaire pour faire varier les dates des tableaux automatiquement

# EJO
#d
# coucou

ui <-
  fluidPage(theme="style.css",titlePanel("Chiffres de la Note"),tabsetPanel(
    ##la partie ui correspond a l'interface utilisateur
    tabPanel(
      "France",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            "tableau",
            "Tableau",
            c(
              "PIB France",
              "Production",
              "Investissement",
              "Revenu",
              "Echanges exterieurs",
              "Consommation et investissement des menages",
              "Emploi et salaires"
            ),
            multiple = F,
            selectize = T
          ),
          uiOutput("liste_gf"),
          ##bouton un peu plus complique que le precedent car il s'adapte aux Rdata presents dans le repertoire
          uiOutput("liste_gf_old"),
          checkboxInput("digi_fr", "Deuxieme decimale")#,##a enlever si le respo France ne veut pas devoiler sa tambouille
          #actionButton("export", "Exporter en CSV")#,##a faire en cas de demande, ou alors bouton impression
          
        ),
        
        mainPanel(
          "Attention le grisage n'est qu'indicatif et seules les donnees dans les sorties du garde-fou apparaissent. Si rien n'apparait vous n'avez pas acces a N:/GDESE/O-GDESE/Special/Special.GFou. \n Les calculs de contributions annuelles peuvent varier de 0,1 selon la methode utilisee",
          htmlOutput("table_fr")
        )
      )
    ),
    tabPanel(
      "International",
      
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            "tableau_int",
            "Tableau",
            c(
              "Petrole",
              "Taux de change",
              "Commerce mondial",
              "Zone euro",
              "Belgique Pays-Bas",
              "Espagne",
              "Italie",
              "Allemagne",
              "US",
              "Royaume-Uni",
              "Japon",
              "Chine",
              "Economies emergentes",
              "Taux souverains europeens",
              "Synthese"
            ),
            multiple = F,
            selectize = T
          ),uiOutput("liste_int"),uiOutput("liste_int_old"),
          checkboxInput("digi", "Deuxieme decimale")
        ),
        mainPanel(
          "Attention le grisage n'est qu'indicatif",
          htmlOutput("table_int")
        )
        
      )
    )
  ))




server <- function(input, output) {##partie serveur (code R)
  options(encoding="UTF-8")
  
  d_fr <<-"N:/GDESE/O-GDESE/Special/Special.GFou/Sorties/SyntheseGFOU"
 #repertoire de sortie pour les Rdata France
  liste_gf <<-
    rev(dir(d_fr)[grepl("sortieGFRdata", dir(d_fr))]) #listes des Rdata dans le repertoire ci-dessus
  output$liste_gf_old <-
    renderUI({
      ##creation du bouton des GF a comparer avec la liste_gf ci-dessus
      selectInput(
        "Gfou_old",
        "Garde-fou a comparer",
        liste_gf,
        multiple = F,
        selectize = T
      )
    })
  output$liste_gf <- renderUI({
    ##idem pour le GF de reference
    selectInput("Gfou",
                "Garde-fou",
                liste_gf,
                multiple = F,
                selectize = T)
  })
  d_int<<-"N:/GDCJ/N-GDCJ/Echanges.DCJ/DSC/Rdata internationaux" #chemin des Rdata internationaux

    
    output$liste_int <- renderUI({
      print("coucou")
      print(input$tableau_int)
        pays <- switch(
          input$tableau_int,
          "Petrole"="OIL",
          "Taux de change" = "FI.change",
          "Commerce mondial" = "DM",
          "Zone euro" = "ZE",
          "Belgique Pays-Bas"="BENL",
          "Espagne" = "ES",
          "US" = "US",
          "Italie" = "IT",
          "Allemagne" = "AL",
          "Royaume-Uni" = "UK",
          "Japon" = "JP",
          "Chine" = "CN",
          "Economies emergentes" = "EM",
          "Taux souverains europeens" = "FItaux",
          "Synthese"="SYN"
        )
        liste_int <<-
          rev(dir(d_int)[grepl(paste0(pays,"[0-9]"), dir(d_int))]) #listes des Rdata dans le repertoire ci-dessus
        #dont le nom correspond a la variable pays. Ainsi si on choisit Zone euro, seuls les Rdata avec ZE vont 
        #etre selectionnes
      
      ##idem pour le GF de reference
      selectInput("data_int",
                  "Sortie",
                  liste_int,
                  multiple = F,
                  selectize = T)
    })
  
    output$liste_int_old <- renderUI({
      pays <- switch(
        input$tableau_int,
        "Petrole"="OIL",
        "Taux de change" = "FI.change",
        "Commerce mondial" = "DM",
        "Zone euro" = "ZE",
        "Belgique Pays-Bas"="BENL",
        "Espagne" = "ES",
        "US" = "US",
        "Italie" = "IT",
        "Allemagne" = "AL",
        "Royaume-Uni" = "UK",
        "Japon" = "JP",
        "Chine" = "CN",
        "Economies emergentes" = "EM",
        "Taux souverains europeens" = "FItaux",
        "Synthese"="SYN"
      )
      liste_int_old <<-
        rev(dir(d_int)[grepl(paste0(pays,"[0-9]"), dir(d_int))]) #listes des Rdata dans le repertoire ci-dessus qui ont bien un chiffre juste apres le nom (pour faire la difference entre FI2019 et FI.change2019)  
      
      ##idem pour le GF de reference
      selectInput("data_int_old",
                  "Sortie a comparer",
                  liste_int_old,
                  multiple = F,
                  selectize = T)
    })
  
  options(stringsAsFactors=FALSE)##toujours utile quand on lit des dataframe de chiffres
  sapply(dir()[grepl("tabl_",dir())],FUN=source)###on source toutes les fonctions avec "tabl_" dans le nom, ce sont toutes les tables
  #pour en creer de nouvelles il faut donc bien les appeler tabl_qqchose
  ifelse(####choix automatique du dernier trimestre du tableau, si mois de novembre/decembre on change d'annee
    month(Sys.Date() ) %in% 11:12,
    trim <<-
      c(year(Sys.Date()) + 1, 2), trim <<-
      c(
        year(Sys.Date()), ifelse(month(Sys.Date()) %in% 1:3, 2, 4)
      ))
  annee<<-trim[1]  ##derniere annee a apparaitre dans le tableau
  source(file="fonctions_pour_fiche.R")  ##les fonctions utilisees sont toutes regroupees dans ce fichier R
     output$table_fr <- renderText({##creation du tableau France a afficher
       req(input$Gfou)#pour ne pas qu'il indique une erreur le temps qu'il cree les boutons
       req(input$Gfou_old)
     b_fr<<-paste(d_fr,input$Gfou,sep="//") ##le chemin plus le nom du Rdata contenant le Gfou selectionne
     b_fr_old<<-paste(d_fr,input$Gfou_old,sep="//")##le chemin plus le nom du Rdata contenant le Gfou a comparer selectionne
     
     switch(##selon le choix utilisateur, on lance une fonction donnee, avec le nombre de decimales souhaite
       input$tableau,
       "Production" = tabl_prod(dig = ifelse(input$digi_fr, 2, 1)),
       "Investissement" = tabl_inv(dig = ifelse(input$digi_fr, 2, 1)),
       "Revenu" = tabl_rev(dig = ifelse(input$digi_fr, 2, 1)),
       "Echanges exterieurs" = tabl_comext(dig = ifelse(input$digi_fr, 2, 1)),
       "Consommation et investissement des menages"= tabl_conso(dig = ifelse(input$digi_fr, 2, 1)),
       "PIB France"=tabl_pibfr(dig = ifelse(input$digi_fr, 2, 1)),
       "Emploi et salaires"=tabl_empl(dig = ifelse(input$digi_fr, 2, 1))
     )
     
   })
   
   

   output$table_int <- renderText({##creation du tableau international a afficher
     req(input$data_int)
     req(input$data_int_old)
     req(input$tableau_int)
     b_int<<-paste(d_int,input$data_int,sep="//") ##le chemin plus le nom du Rdata contenant le Gfou selectionne
     b_int_old<<-paste(d_int,input$data_int_old,sep="//")##le chemin plus le nom du Rdata contenant le Gfou a comparer selectionne
     pays <- switch(
       input$tableau_int,
       "Petrole"="OIL",
       "Taux de change" = "FI.change",
       "Commerce mondial" = "DM",
       "Zone euro" = "ZE",
       "Belgique Pays-Bas"="BENL",
       "Espagne" = "ES",
       "US" = "US",
       "Italie" = "IT",
       "Allemagne" = "AL",
       "Royaume-Uni" = "UK",
       "Japon" = "JP",
       "Chine" = "CN",
       "Economies emergentes" = "EM",
       "Taux souverains europeens" = "FItaux",
       "Synthese"="SYN"
     )
     if(grepl(pays,b_int)){##la condition est la uniquement pour empecher l'affichage temporaire d'une erreur
     switch(
       input$tableau_int,
       "Petrole"=tabl_oil(dig=ifelse(input$digi,2,1)),
       "Taux de change"=tabl_fi(dig=ifelse(input$digi,2,1)),
       "Commerce mondial" = tabl_dm(dig=ifelse(input$digi,2,1)),
       "Zone euro" = tabl_ze(dig=ifelse(input$digi,2,1)),
       "Belgique Pays-Bas" = tabl_benl(dig=ifelse(input$digi,2,1)),
       "Espagne" = tabl_es(dig=ifelse(input$digi,2,1)),
       "US" = tabl_us(dig=ifelse(input$digi,2,1)),
       "Italie" = tabl_it(dig=ifelse(input$digi,2,1)),
       "Allemagne" = tabl_al(dig=ifelse(input$digi,2,1)),
       "Royaume-Uni"=tabl_uk(dig=ifelse(input$digi,2,1)),
       "Japon"=tabl_jp(dig=ifelse(input$digi,2,1)),
       "Chine" = tabl_cn(dig=ifelse(input$digi,2,1)),
       "Economies emergentes"=tabl_em(dig=ifelse(input$digi,2,1)),
       "Taux souverains europeens" = tabl_fitaux(dig=ifelse(input$digi,2,1)),
       "Synthese" = tabl_syn(dig=ifelse(input$digi,2,1))
     )}
     
   })  
   

}

# Run the application 
shinyApp(ui = ui, server = server)
