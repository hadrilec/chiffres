tabl_comext<-function(dig=1){
charge<<-charge_fr
return(paste(
  "<h1>Prevision de croissance des echanges exterieurs</h1>",
  "Pour le commerce mondiale et la demande adressee voir la page dediee dans les fiches internationales",
  "<table style=\"width:100%\" border=1>",
  ligne_date(),
  f_row("Exportations","p6_d_7ch",VT,VA,b_fr,b_fr_old,bold=T,dig=dig),
  f_row("dont Produits manufactures","p6_dim_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  f_row("Importations","p7_d_7ch",VT,VA,b_fr,b_fr_old,bold=T,dig=dig),
  f_row("dont Produits manufactures","p7_dim_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  f_row("Contribution du commerce exterieur","c.solde_d_7ch",NIV,MOY_PND,b_fr,b_fr_old,bold=T,dig=dig),
  "</table>",sep=""))}


