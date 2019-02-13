##l'application chiffres de la Note a pour objectif de regrouper tous les chiffres de la Note en un seul endroit, garantissant en plus
##la fraicheur des donnees, toujours synchronisees avec les toutes dernieres sorties

library(shiny)
library(lubridate)##necessaire pour faire varier les dates des tableaux automatiquement


ui <-
  fluidPage(tabsetPanel(
    ##la parite ui correspond a l'interface utilisateur
    tabPanel(
      "Fiches France",
      
      # Application title
      titlePanel("Chiffres de la Note"),
      
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            "tableau",
            "Tableau",
            c(
              "Fiche PIB France",
              "Fiche Production",
              "Fiche Investissement",
              "Fiche Revenu",
              "Fiche Echanges exterieurs",
              "Fiche Consommation et investissement des menages",
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
          "Attention le grisage n'est qu'indicatif et seules les donnees dans les sorties du garde-fou apparaissent. Si rien n'apparait vous n'avez pas acces a O://Special/Special.GFou",
          htmlOutput("table_fr")
        )
      )
    ),
    tabPanel(
      "Fiches Internationales",
      titlePanel("Chiffres de la Note"),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            "tableau_int",
            "Tableau",
            c(
              "Petrole/Marches FI",
              "Fiche Zone euro",
              "Fiche Espagne",
              "Fiche Italie",
              "Fiche Allemagne",
              "Fiche US",
              "Fiche Royaume-Uni",
              "Fiche Japon",
              "Economies emergentes"
            ),
            multiple = F,
            selectize = T
          ),
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
  
  d_fr <<-
    "N:/GDESE/O-GDESE/Special/Special.GFou/Sorties/syntheseGFOU" #repertoire de sortie pour les Rdata France
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
     b_fr<<-paste(d_fr,input$Gfou,sep="//") ##le chemin plus le nom du Rdata contenant le Gfou selectionne
     b_fr_old<<-paste(d_fr,input$Gfou_old,sep="//")##le chemin plus le nom du Rdata contenant le Gfou a comparer selectionne
     
     switch(##selon le choix utilisateur, on lance une fonction donnee, avec le nombre de decimal souhaite
       input$tableau,
       "Fiche Production" = tabl_prod(dig = ifelse(input$digi_fr, 2, 1)),
       "Fiche Investissement" = tabl_inv(dig = ifelse(input$digi_fr, 2, 1)),
       "Fiche Revenu" = tabl_rev(dig = ifelse(input$digi_fr, 2, 1)),
       "Fiche Echanges exterieurs" = tabl_comext(dig = ifelse(input$digi_fr, 2, 1)),
       "Fiche Consommation et investissement des menages"= tabl_conso(dig = ifelse(input$digi_fr, 2, 1)),
       "Fiche PIB France"=tabl_pibfr(dig = ifelse(input$digi_fr, 2, 1)),
       "Emploi et salaires"=tabl_empl(dig = ifelse(input$digi_fr, 2, 1))
     )
     
   })
   
   d_int<<-"N://GDCJ//N-GDCJ//Echanges.DCJ//DSC//Rdata internationaux" #chemin des Rdata internationaux

   output$table_int <- renderText({##creation du tableau international a afficher
     switch(
       input$tableau_int,
       "Petrole/Marches FI"=tabl_fi(dig=ifelse(input$digi,2,1)),
       "Fiche Zone euro" = tabl_ze(dig=ifelse(input$digi,2,1)),
       "Fiche Espagne" = tabl_es(dig=ifelse(input$digi,2,1)),
       "Fiche US" = tabl_us(dig=ifelse(input$digi,2,1)),
       "Fiche Italie" = tabl_it(dig=ifelse(input$digi,2,1)),
       "Fiche Allemagne" = tabl_al(dig=ifelse(input$digi,2,1)),
       "Fiche Royaume-Uni"=tabl_uk(dig=ifelse(input$digi,2,1)),
       "Fiche Japon"=tabl_jp(dig=ifelse(input$digi,2,1)),
       "Economies emergentes"=tabl_em(dig=ifelse(input$digi,2,1))
     )
     
   })  
   

}

# Run the application 
shinyApp(ui = ui, server = server)
