tabl_empl<-function(dig=1){
  charge<<-charge_fr
return(paste(
  "<h1>Emploi et salaires</h1><table style=\"width:100%\" border=1>",
  ligne_date(),
f_row("Emploi salarie SMNA","gf.emps_smna_1",DIFF,DIFF_AN,b_fr,b_fr_old,dig=dig),
f_row("SMPT reel SMNA","gf.smptr_smna_3",VT,VA,b_fr,b_fr_old,dig=dig),
f_row("SMPT SMNA","gf.smpt_smna_3",VT,VA,b_fr,b_fr_old,dig=dig),
f_row("SMPT APU","gf.smpt_s13_3",VT,VA,b_fr,b_fr_old,dig=dig),
f_row("Productivite SMNA","gf.productiv_smna",VT,VA,b_fr,b_fr_old,dig=dig),
f_row("Masse salariale SMNA","gf.d11_smna_3",VT,VA,b_fr,b_fr_old,dig=dig),
"</table>",sep=""))}


