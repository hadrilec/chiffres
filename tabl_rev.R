tabl_rev<-function(dig=1){
  charge<<-charge_fr
  row1 <-  ligne_date()
  row2 <-f_row("Revenu disponible brut (RDB)","b6_s14_3",VT,VA,b_fr,b_fr_old,dig=dig,bold=T)
  row3 <-f_row("Masse salariale brute","d11_s14r_3",VT,VA,b_fr,b_fr_old,dig=dig)
  row4 <-f_row("EBE des entrepreneurs individuels","b2_s14a_3",VT,VA,b_fr,b_fr_old,dig=dig)
  row5 <-f_row("Prestations sociales en espece","d62_s14r_3",VT,VA,b_fr,b_fr_old,dig=dig)
row6 <-f_row("EBE des menages purs","b2_s14b_3",VT,VA,b_fr,b_fr_old,dig=dig)
row7 <-f_row("Revenus de la propriete","d4_s14s_3",VT,VA,b_fr,b_fr_old,dig=dig)
row8 <-f_row("Cotisations des menages","d613z_s14e_3",VT,VA,b_fr,b_fr_old,dig=dig)
row9 <-f_row("Impots sur le revenu et le patrimoine","d5_s14e_3",VT,VA,b_fr,b_fr_old,dig=dig)
row10 <-f_row("Prix de la consommation des menages","p3m_d_9ch",VT,VA,b_fr,b_fr_old,dig=dig,bold=T)
row11 <-f_row("Pouvoir d'achat du RDB","pardb_men",VT,VA,b_fr,b_fr_old,dig=dig,bold=T)
tabl<-paste("<h1>Revenu des menages</h1><table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,"</table>",sep="")
return(tabl)}
