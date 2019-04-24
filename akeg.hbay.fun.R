require(tcltk) || stop("Package tcltk non disponible.")
require(tcltk2) || stop("Package tcltk non disponible.")
require(tkrplot) || stop("Package trplot non disponible.")
rm(list=ls())

akeg.hbay.fun<-function(){
  
  #-----------Fenetre principale--------------
  win2<-tktoplevel()
  tkwm.title(win2,"hbay.fun: Local Bayesian procedure for bandwidth selection")
  tkwm.resizable(win2,FALSE,FALSE)
  tkwm.geometry(win2,"1200x650+80+30")
  tkwm.minsize(win2,200,100)
  
  #fonction quitter
  quitter=function(){
    quit=tkmessageBox(title="Info...",message="VOULLEZ-VOUS FERMER CETTE FENETRE ?",
                      icon="question", type="yesno")
    if(tclvalue(quit)=="yes"){
      tkdestroy(win3)
      source("akeg.package.R")
      akeg.package()
    }
    
  }
  
  #-----------Mise en forme--------------
  police.bouton <- tkfont.create(family="arial", size=12,weight="bold")
  police.label <- tkfont.create(family="arial", size=10,weight="bold")
  police.label1 <- tkfont.create(family="arial", size=11)
  police.label2 <- tkfont.create(family="arial", size=12,weight="bold")
  police.label3 <- tkfont.create(family="arial", size=10,weight="bold",underline=T)
  police.menu <- tkfont.create(family="arial", size=11, weight="bold")
  police.titre  <- tkfont.create(family="arial", size=18, weight="bold",slant="italic")
  police.titre1  <- tkfont.create(family="arial", size=13, weight="bold",slant="italic")
  police.sous.titre <- tkfont.create(family="arial", size=12,underline=F)
  
  #-------------Les Frames------------------------------
  frm1 <- tk2frame(win2, borderwidth = 5,relief = "sunken",padding = 10)
  tkgrid(frm1,padx = 5, pady = c(5, 5), sticky = "w",columnspan=2,row=0,column=0)
  
  frm0 <- tk2frame(frm1,relief = "solid",padding =0)
  tkgrid(frm0,padx = 5, pady = c(5, 5),sticky = "n",columnspan=6,row=0,column=0)
  tkgrid(tk2label(frm0, text = "LOCAL BAYESIAN PROCEDURE FOR BANDWIDTH SELECTION", 
                  justify = "center", font=police.titre1),padx = 5,pady = c(5, 10), sticky = "n",columnspan=4,row=0,column=0)
  
  
  frm2 <- tk2frame(win2, borderwidth = 5,relief = "sunken",padding = 10)
  tkgrid(frm2,padx = 5, pady = c(5, 5), sticky = "w",columnspan=3,row=0,column=3)
  

  label_vec<-tk2label(frm1, text = "Data sample (Vec) : ", justify = "left",font=police.label)
  zone_saisie_vec1 <- tclVar("100")
  zone_saisie_vec2 <- tclVar("1.5")
  zone_saisie_vec3 <- tclVar("2.6")
  saisie_vec1 <-tk2entry(frm1, width = "5", textvariable =zone_saisie_vec1,font=police.label)
  saisie_vec2 <-tk2entry(frm1, width = "5", textvariable =zone_saisie_vec2,font=police.label)
  saisie_vec3 <-tk2entry(frm1, width = "5", textvariable =zone_saisie_vec3,font=police.label)
  
  
  #géneration du vecteur vec
  vec_fun <- function(){
      v1<- as.numeric(tclvalue(zone_saisie_vec1))
      v2<- as.numeric(tclvalue(zone_saisie_vec2))
      v3<- as.numeric(tclvalue(zone_saisie_vec3))
      v<-rgamma(v1,v2,v3)
  }
  
  #Fonction dke.fun
  hbay.fun <- function(){
   Vec<-vec_fun()
   if(is.null(Vec)){
     print("-----------Vec")
     print(Vec)
     tkmessageBox(title="Info...", message = "DONNEES ERRONEES")
   }else{
     x=NULL
     y1<-sort(Vec)	
     if(is.null(x)){x<-0:max(y1)}	
     vec1=0
     vec2=0
     alp=0.5 
     bet=15  
     for (i in 1: length(x)){
       if(x[i]<= y1+1){
         k=seq(0,x[i],by=1)
         vec1[i]=sum ((factorial(y1+1)*(y1^k)*beta(x[i]+alp-k+1,y1+bet-x[i]+1))/(factorial(y1+1-x[i])*factorial(k)*factorial (x[i]-k)*(y1+1)^(y1+1)))
         vec2[i]=sum ((factorial(y1+1)*(y1^k)*beta(x[i]+alp-k,y1+bet-x[i]+1))/(factorial(y1+1-x[i])*factorial(k)*factorial (x[i]-k)*(y1+1)^(y1+1)))
       }else{
         vec1[i]=0
         vec2[i]=0
       }
     }
     
     #The data - same as input Vec(data=Vec)
     tkgrid(tk2label(win2, text = "THE DATA SAMPLE", justify = "center",font=police.label3),
            padx = 5, pady = c(5, 5), sticky = "s", row=1,column=0)
     frm3 <- tk2frame(win2, borderwidth = 5, relief = "sunken",padding = 10)
     tkgrid(frm3,padx = 5, pady = c(5, 10), sticky = "n",rowspan=4,row=2,column=0)
     scrx1 <- tk2scrollbar(frm3, orient = "horizontal",command = function(...) tkxview(txt1, ...))
     scry1<- tk2scrollbar(frm3, orient = "vertical",command = function(...) tkyview(txt1, ...))
     txt1 <- tk2text(frm3, bg = "white",font = "courier", width = 20, height = 20,
                     xscrollcommand = function(...) tkset(scrx1, ...),
                     yscrollcommand = function(...) tkset(scry1, ...))
     tkgrid(txt1,scry1, sticky = "nsew")
     tkgrid.rowconfigure(frm3, txt1, weight = 1)
     tkgrid.columnconfigure(frm3,txt1, weight = 1)
     tkgrid(scrx1, sticky = "nsew")
     for (i in Vec)
       tkinsert(txt1, "end", paste0(i, "\n"))
     tkconfigure(txt1, state = "disabled")
     tkfocus(txt1)
     
     #y1
     tkgrid(tk2label(win2, text = "Y1", justify = "center",font=police.label3),
            padx = 5, pady = c(5, 5), sticky = "s", row=1,column=1)
     frm4 <- tk2frame(win2, borderwidth = 5, relief = "sunken",padding = 10)
     tkgrid(frm4,padx = 5, pady = c(5, 10), sticky = "n",rowspan=4,row=2,column=1)
     scrx2 <- tk2scrollbar(frm4, orient = "horizontal",command = function(...) tkxview(txt2, ...))
     scry2<- tk2scrollbar(frm4, orient = "vertical",command = function(...) tkyview(txt2, ...))
     txt2 <- tk2text(frm4, bg = "white",font = "courier", width = 20, height = 20,
                     xscrollcommand = function(...) tkset(scrx2, ...),
                     yscrollcommand = function(...) tkset(scry2, ...))
     tkgrid(txt2,scry2, sticky = "nsew")
     tkgrid.rowconfigure(frm4, txt2, weight = 1)
     tkgrid.columnconfigure(frm4,txt2, weight = 1)
     tkgrid(scrx2, sticky = "nsew")
     for (i in y1)
       tkinsert(txt2, "end", paste0(i, "\n"))
     tkconfigure(txt2, state = "disabled")
     tkfocus(txt2)
     
     #x
     tkgrid(tk2label(win2, text = "X", justify = "left",font=police.label3),
            padx = 5, pady = c(5, 5), sticky = "s", row=1,column=2)
     frm5 <- tk2frame(win2, borderwidth = 5, relief = "sunken",padding = 10)
     tkgrid(frm5,padx = 5, pady = c(5, 10), sticky = "n",rowspan=4,row=2,column=2)
     scrx3 <- tk2scrollbar(frm5, orient = "horizontal",command = function(...) tkxview(txt3, ...))
     scry3<- tk2scrollbar(frm5, orient = "vertical",command = function(...) tkyview(txt3, ...))
     txt3 <- tk2text(frm5, bg = "white",font = "courier", width = 5, height = 20,
                     xscrollcommand = function(...) tkset(scrx3, ...),
                     yscrollcommand = function(...) tkset(scry3, ...))
     tkgrid(txt3,scry3, sticky = "nsew")
     tkgrid.rowconfigure(frm5, txt3, weight = 1)
     tkgrid.columnconfigure(frm5,txt3, weight = 1)
     tkgrid(scrx3, sticky = "nsew")
     for (i in x)
       tkinsert(txt3, "end", paste0(i, "\n"))
     tkconfigure(txt3, state = "disabled")
     tkfocus(txt3)
     
     #vec1
     tkgrid(tk2label(win2, text = "Vec1", justify = "center",font=police.label3),
            padx = 5, pady = c(5, 5), sticky = "s", row=1,column=3)
     frm6 <- tk2frame(win2, borderwidth = 5, relief = "sunken",padding = 10)
     tkgrid(frm6,padx = 5, pady = c(5, 10), sticky = "n",rowspan=4,row=2,column=3)
     scrx4 <- tk2scrollbar(frm6, orient = "horizontal",command = function(...) tkxview(txt4, ...))
     scry4<- tk2scrollbar(frm6, orient = "vertical",command = function(...) tkyview(txt4, ...))
     txt4 <- tk2text(frm6, bg = "white",font = "courier", width = 20, height = 20,
                     xscrollcommand = function(...) tkset(scrx4, ...),
                     yscrollcommand = function(...) tkset(scry4, ...))
     tkgrid(txt4,scry4, sticky = "nsew")
     tkgrid.rowconfigure(frm6, txt4, weight = 1)
     tkgrid.columnconfigure(frm6,txt4, weight = 1)
     tkgrid(scrx4, sticky = "nsew")
     for (i in vec1)
       tkinsert(txt4, "end", paste0(i, "\n"))
     tkconfigure(txt4, state = "disabled")
     tkfocus(txt4)
     
     #vec2
     tkgrid(tk2label(win2, text = "Vec2", justify = "center",font=police.label3),
            padx = 5, pady = c(5, 5), sticky = "s", row=1,column=4)
     frm7 <- tk2frame(win2, borderwidth = 5, relief = "sunken",padding = 10)
     tkgrid(frm7,padx = 5, pady = c(5, 10), sticky = "n",rowspan=4,row=2,column=4)
     scrx5 <- tk2scrollbar(frm7, orient = "horizontal",command = function(...) tkxview(txt5, ...))
     scry5<- tk2scrollbar(frm7, orient = "vertical",command = function(...) tkyview(txt5, ...))
     txt5 <- tk2text(frm7, bg = "white",font = "courier", width = 20, height = 20,
                     xscrollcommand = function(...) tkset(scrx5, ...),
                     yscrollcommand = function(...) tkset(scry5, ...))
     tkgrid(txt5,scry5, sticky = "nsew")
     tkgrid.rowconfigure(frm7, txt5, weight = 1)
     tkgrid.columnconfigure(frm7,txt5, weight = 1)
     tkgrid(scrx5, sticky = "nsew")
     for (i in vec2)
       tkinsert(txt5, "end", paste0(i, "\n"))
     tkconfigure(txt5, state = "disabled")
     tkfocus(txt5)
     result=sum(vec1)/sum(vec2)
     
     #hby=result
     label_hby <- tclVar(" ")
     tclvalue(label_hby) <-""
     tclvalue(label_hby) <-result
     tkgrid(tk2label(frm2, text = "The bandwidth selected (hby) : ", justify = "left",font=police.label1),
            padx = 5,pady = c(5, 10), sticky = "w",row=0,column=0)
     tkgrid(tk2label(frm2, textvariable = label_hby, justify = "left",font=police.label),
            padx = 5,pady = c(5, 10), sticky = "w",row=0,column=1)
     
     msg<-structure(list(hby=result),class="hbay.fun")
     print(msg)
     
   }
    
  }
  
  bouton_hbayfun<-tkbutton(frm1,font=police.bouton,width = "50", text = "hbay.fun", command=hbay.fun)
  
  #Positionnement
  tkgrid(label_vec, padx = 5, pady = c(5, 10),sticky = "w", row=1,column=0)
  tkgrid(saisie_vec1, padx = 5, pady = c(5, 10), sticky = "w",row=1,column=1)
  tkgrid(saisie_vec2, padx = 5, pady = c(5, 10), sticky = "w",row=1,column=2)
  tkgrid(saisie_vec3, padx = 5, pady = c(5, 10), sticky = "w",row=1,column=3)
  
  tkgrid(bouton_hbayfun,padx = 5,pady = c(5, 10), sticky = "w",columnspan=6, row=5,column=0)
  
  
}

