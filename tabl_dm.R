tabl_dm<-function(dig=1){
  charge<<-charge_int


return(paste("<h1>Commerce mondial et demandes mondiales adressees</h1><table style=\"width:100%\" border=1>",
ligne_date(),
f_row("Commerce mondial","dm_monde",VT,VA,b_int,b_int_old,dig=dig,bold=T),
f_row("Importations des economies avancees","m_av",VT,VA,b_int,b_int_old,dig=dig),
f_row("Importations des economies emergentes","m_em",VT,VA,b_int,b_int_old,dig=dig),
f_row("Demande mondiale adressee a la France","dm_france",VT,VA,b_int,b_int_old,dig=dig,bold=T),
f_row("Demande mondiale adressee a la zone euro","dm_zone_euro",VT,VA,b_int,b_int_old,dig=dig),
f_row("Demande mondiale adressee a l'Allemagne","dm_allemagne",VT,VA,b_int,b_int_old,dig=dig),
f_row("Demande mondiale adressee a l'Italie","dm_italie",VT,VA,b_int,b_int_old,dig=dig),
f_row("Demande mondiale adressee a l'Espagne","dm_espagne",VT,VA,b_int,b_int_old,dig=dig),
f_row("Demande mondiale adressee aux US","dm_etats_unis",VT,VA,b_int,b_int_old,dig=dig),
f_row("Demande mondiale adressee au Royaume-Uni","dm_royaume_uni",VT,VA,b_int,b_int_old,dig=dig),
f_row("Demande mondiale adressee au Japon","dm_japon",VT,VA,b_int,b_int_old,dig=dig),
"</table>",sep="")
)}

