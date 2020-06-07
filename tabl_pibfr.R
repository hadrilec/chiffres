tabl_pibfr<-function(dig=1){
  charge<<-charge_fr
return(paste(
  "<h1>Fiche de PIB France</h1><table style=\"width:100%\" border=1>",
  ligne_date(),
  f_row("Produit interieur brut","pib_7ch",VT,VA,b_fr,b_fr_old,dig=dig,bold=T),
  f_row("Consommation privee","p3m_d_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  f_row("Investissement","p51_d_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  f_row("Consommation publique","p3pg_d_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  f_row("Exportations","p6_d_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  f_row("Importations","p7_d_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  "<tr><td><b>Contributions</b></td></tr>",
  f_row("Demande interieure hors stocks","c.dintfhs_d_7ch",NIV,MOY_PND,b_fr,b_fr_old,dig=dig),
  f_row("Variations de stocks","c.p54_d_7ch",NIV,MOY_PND,b_fr,b_fr_old,dig=dig),
  f_row("Commerce exterieur","c.solde_d_7ch",NIV,MOY_PND,b_fr,b_fr_old,dig=dig),
  "<tr><td><b>Detail investissement</b></td></tr>",
  f_row("Investissement des ENF","p51s_d_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  f_row("Investissement des menages","p51m_d_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  f_row("Investissement des APU","p51g_d_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  "</table>",sep=""))} 

