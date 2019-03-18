tabl_fi<-function(dig=1){
  charge<<-charge_int
  base<<-paste(d_int,"OIL.Rdata",sep="//")
  #base_an<<-paste(d_int,"AL_an.Rdata",sep="//")
  base_old<<-NULL
row1 <-  ligne_date()
row2 <-f_row("Brent en dollar","brent_m",NIV_depuis_mensuel,NIV_A,base,dig=dig,bold=T)


tabl<-paste("<h1>Petrole</h1>",version(base),
              "<table style=\"width:100%\" border=1>",row1,row2,"</table>",sep="")

base_cdm<-paste(d_int,"FI.change.Rdata",sep="//")

row1 <-  ligne_date()
row2 <-f_row("Dollar en euro","usdeur",NIV_depuis_mensuel,NIV_A,base_cdm,dig=2,bold=T)
row3 <-f_row("Livre sterling en euro","gbpeur",NIV_depuis_mensuel,NIV_A,base_cdm,dig=2)
row4 <-f_row("Yen en euro","jpyeur",NIV_depuis_mensuel,NIV_A,base_cdm,dig=2)

tabl_cdm<-paste("<h1>Taux de change</h1>",version(base_cdm),
                "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,"</table>",sep="")

base_syn<<-paste(d_int,"SYN.Rdata",sep="//")


row1 <-  ligne_date()
row2 <-f_row("PIB des economies avancees","syn.pib_ea",VT,VA,base_syn,dig=dig,bold=T)
tabl_syn<-paste("<h1>Synthese</h1>",version(base_syn),
                "<table style=\"width:100%\" border=1>",row1,row2,"</table>",sep="")

return(paste(tabl,tabl_cdm,tabl_syn,sep=""))}

