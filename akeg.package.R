require(tcltk) || stop("Package tcltk non disponible.")
require(tcltk2) || stop("Package tcltk non disponible.")
require(tkrplot) || stop("Package trplot non disponible.")

rm(list=ls())

# creation de la fenetre d'acceuil
akeg.package<-function(){
  
  #-----------Fenetre principale--------------
  win0<-tktoplevel()
  tkwm.title(win0,"Ake-package: Associated kernel estimations")
  tkwm.resizable(win0,FALSE,FALSE)
  tkwm.geometry(win0,"850x650+200+50")
  tkwm.minsize(win0,200,100)
  tkconfigure(win0, bg="blue")
  tkwm.maxsize(win0,850,650)
  
  #-----------Mise en forme--------------
  police.bouton <- tkfont.create(family="arial", size=14)
  police.menu <- tkfont.create(family="arial", size=11, weight="bold")
  police.titre  <- tkfont.create(family="arial", size=20, weight="bold",slant="italic")
  police.sous.titre <- tkfont.create(family="arial", size=12,underline=F)
  
  #-------------Les Frames------------------------------
   frm <- tk2frame(win0, borderwidth = 5,relief = "raised",padding = 10)
   #tkgrid(frm,padx = 5, pady = c(5, 5), sticky = "w", columnspan=2,row=2,column=1)
   tkpack(frm,side="bottom",fill="both",expand=TRUE, padx=0, pady=10)
   #tkplace(frm,x=300,y=100,anchor="center")
   
   frm1 <- tk2frame(win0, borderwidth = 5,relief = "solid",borderwidth =10,padding = 10)
   #tkgrid(frm1,padx = 5, pady = c(5, 5), sticky = "w",row=0,column=1,columnspan=2)
   tkpack(frm1,side="top",fill="y",padx=10, pady=5)
   tkgrid(tk2label(frm1, text = "ASSOCIATED KERNEL ESTIMATIONS", justify = "center", font=police.titre),
          padx = 5,pady = c(5, 10), sticky = "e",row=1,column=1)
   
  
  #-----------Fonction quitter--------------
  quitter=function(){
    quit=tkmessageBox(title="info...",
                      message="ETES-VOUS SÛR DE QUITTER L'APPLICATION ?",
                      icon="question", 
                      type="yesno")
    if(tclvalue(quit)=="yes"){
      tkdestroy(win0)
      }
    
  }
   
  #-----------appel de la fenetre dke.fun------------
  dke.fun<-function(){
    #tkdestroy(win0)
    source("akeg.dke.fun.R")
    akeg.dke.fun()
  }
   #-----------appel de la fenetre hbay.fun------------
   hbay.fun<-function(){
     #tkdestroy(win0)
     source("akeg.hbay.fun.R")
     akeg.hbay.fun()
   }
   
   #-----------appel de la fenetre hcvc.fun------------
   hcvc.fun<-function(){
     #tkdestroy(win0)
     source("akeg.hcvc.fun.R")
     akeg.hcvc.fun()
   }
   
   #-----------appel de la fenetre hcvd.fun------------
   hcvd.fun<-function(){
     #tkdestroy(win0)
     source("akeg.hcvd.fun.R")
     akeg.hcvd.fun()
   }
   
   #-----------appel de la fenetre hcvreg.fun------------
   hcvreg.fun<-function(){
     #tkdestroy(win0)
     source("akeg.hcvreg.fun.R")
     akeg.hcvreg.fun()
   }
   
   #-----------appel de la fenetre kern.fun------------
   kern.fun<-function(){
     #tkdestroy(win0)
     source("akeg.kern.fun.R")
     akeg.kern.fun()
   }
   
   #-----------appel de la fenetre kpmfe.fun------------
   kpmfe.fun<-function(){
     #tkdestroy(win0)
     source("akeg.kpmfe.fun.R")
     akeg.kpmfe.fun()
   }
   
   #-----------appel de la fenetre reg.fun------------
   reg.fun<-function(){
     #tkdestroy(win0)
     source("akeg.reg.fun.R")
     akeg.reg.fun()
   }
   
   
   #-----------appel de la fenetre kef------------
   kef<-function(){
     #tkdestroy(win0)
     source("akeg.kef.R")
     akeg.kef()
   }
   
   #-----------appel de la fenetre plot.dke.fun------------
   plot.dke.fun<-function(){
     #tkdestroy(win0)
     source("akeg.plot.dke.fun.R")
     akeg.plot.dke.fun()
   }
   
   #-----------appel de la fenetre plot.hcvc.fun------------
   plot.hcvc.fun<-function(){
     #tkdestroy(win0)
     source("akeg.plot.hcvc.fun.R")
     akeg.plot.hcvc.fun()
   }
   
   #-----------appel de la fenetre plot.kern.fun------------
   plot.kern.fun<-function(){
     #tkdestroy(win0)
     source("akeg.plot.kern.fun.R")
     akeg.plot.kern.fun()
   }
   
   #-----------appel de la fenetre plot.kpmfe.fun------------
   plot.kpmfe.fun<-function(){
     #tkdestroy(win0)
     source("akeg.plot.kpmfe.fun.R")
     akeg.plot.kpmfe.fun()
   }
  
   #-----------appel de la fenetre plot.reg.fun------------
   plot.reg.fun<-function(){
     # tkdestroy(win0)
     source("akeg.plot.reg.fun.R")
     akeg.plot.reg.fun()
   }
  
  #---------------- Debut Menu-------------------------------------------
  
  win0$env$menu <- tk2menu(win0)           # Create a menu
  tkconfigure(win0, menu = win0$env$menu)  # Add it to the 'win0' window
  
  win0$env$menuFichier <- tk2menu(win0$env$menu, tearoff = FALSE,font=police.menu) #tearoff=false: evite que le menu soit detachable
  win0$env$menuFonctions <- tk2menu(win0$env$menu, tearoff = FALSE,font=police.menu)
  win0$env$menuGraphes <- tk2menu(win0$env$menu, tearoff = FALSE,font=police.menu)
  win0$env$menuAide <- tk2menu(win0$env$menu, tearoff = FALSE,font=police.menu)
  
  tkadd(win0$env$menuFichier, "command", label = "Quitter",command =quitter)
  tkadd(win0$env$menuFonctions, "command", label = "dke.fun",command = function() tkdestroy(win0))
  tkadd(win0$env$menuFonctions, "command", label = "hbay.fun",command = function() tkdestroy(win0))
  tkadd(win0$env$menuFonctions, "command", label = "hcvc.fun",command = function() tkdestroy(win0))
  tkadd(win0$env$menuFonctions, "command", label = "hcvd.fun",command = function() tkdestroy(win0))
  tkadd(win0$env$menuFonctions, "command", label = "hcvreg.fun",command = function() tkdestroy(win0))
  tkadd(win0$env$menuFonctions, "command", label = "kern.fun",command = function() tkdestroy(win0))
  tkadd(win0$env$menuFonctions, "command", label = "kpmfe.fun",command = function() tkdestroy(win0))
  tkadd(win0$env$menuFonctions, "command", label = "reg.fun",command = function() tkdestroy(win0))
  
  tkadd(win0$env$menuGraphes, "command", label = "plot.dke.fun",command = function() tkdestroy(win0))
  tkadd(win0$env$menuGraphes, "command", label = "plot.hcvc.fun",command = function() tkdestroy(win0))
  tkadd(win0$env$menuGraphes, "command", label = "plot.kern.fun",command = function() tkdestroy(win0))
  tkadd(win0$env$menuGraphes, "command", label = "plot.kpmfe.fun",command = function() tkdestroy(win0))
  tkadd(win0$env$menuGraphes, "command", label = "plot.reg.fun",command = function() tkdestroy(win0))
  
  tkadd(win0$env$menuAide, "command", label = "à propos",command = function() tkdestroy(win0))
  tkadd(win0$env$menuAide, "command", label = "Guide d'utilisation",command = function() tkdestroy(win0))
  
  tkadd(win0$env$menu, "cascade", label = "Fichier", menu = win0$env$menuFichier)
  tkadd(win0$env$menu, "cascade", label = "Fonctions", menu = win0$env$menuFonctions)
  tkadd(win0$env$menu, "cascade", label = "Graphes", menu = win0$env$menuGraphes)
  tkadd(win0$env$menu, "cascade", label = "Aide", menu = win0$env$menuAide)
  
  #---------------- Definition des boutons-------------------------------------------
   bouton.dke.fun<-tkbutton(frm,width = 35, text = "Function for density estimation(dke.fun)",
                            font=police.bouton,command=dke.fun)
   bouton.hbay.fun<-tkbutton(frm,width = 35, text = "Local Bayesian procedure for bandwidth
   selection (hbay.fun)",font=police.bouton, command=hbay.fun)
   bouton.hcvc.fun<-tkbutton(frm,width = 35, text = "Cross-validation function for bandwidth
   selection for continuous data (hcvc.fun)",font=police.bouton, command=hcvc.fun)
   bouton.hcvd.fun<-tkbutton(frm,width = 35, text = "Cross-validation function for bandwidth 
   selection in p.m.f.estimation (hcvd.fun)",font=police.bouton, command=hcvd.fun) 
   bouton.hcvreg.fun<-tkbutton(frm,width = 35, text = "Cross-validation function for bandwidth 
   selection in regresssion (hcvreg.fun)",font=police.bouton, command=hcvreg.fun) 
   bouton.kern.fun<-tkbutton(frm,width = 35, text = "The associated kernel function (kern.fun)",
                             font=police.bouton, command=kern.fun) 
   bouton.kpmfe.fun<-tkbutton(frm,width = 35, text = "Function for associated kernel estimation
   of p.m.f (kpmfe.fun)",font=police.bouton, command=kpmfe.fun) 
   bouton.reg.fun<-tkbutton(frm,width = 35, text = "Function for associated kernel estimation
   of regression (reg.fun)",font=police.bouton, command=reg.fun) 
   bouton.kef<-tkbutton(frm,width = 35, text = "Continuous and discrete associated kernel
   function (kef)",font=police.bouton, command=kef) 
   bouton.plot.hcvc.fun<-tkbutton(frm,width = 35, text = "Plot of cross-validation function for bandwidth 
          selection in density (plot.hcvc.fun)",font=police.bouton, command=plot.hcvc.fun) 
   bouton.plot.kern.fun<-tkbutton(frm,width = 35, text = "Plot of associated kernel function
          (plot.kern.fun)",font=police.bouton, command=plot.kern.fun) 
   bouton.plot.dke.fun<-tkbutton(frm,width = 35, text = "Plot of density function (plot.dke.fun)",
                                 font=police.bouton, command=plot.dke.fun) 
   bouton.plot.kpmfe.fun<-tkbutton(frm,width = 35, text = "Plot of the function for associated kernel 
   estimation of the p.m.f (plot.kpmfe.fun)",font=police.bouton, command=plot.kpmfe.fun) 
   bouton.plot.reg.fun<-tkbutton(frm,width = 35, text = "Plot for associated kernel regression 
          (plot.reg.fun)",font=police.bouton, command=plot.reg.fun) 
   
   #---------------- Positionnement des boutons-------------------------------------------
   tkgrid(bouton.dke.fun,padx = 5,pady = c(5, 10), sticky = "w",  row=0,column=0)
   tkgrid(bouton.hbay.fun,padx = 5,pady = c(5, 10), sticky = "w",  row=1,column=0)
   tkgrid(bouton.hcvc.fun,padx = 5,pady = c(5, 10), sticky = "w",  row=2,column=0)
   tkgrid(bouton.hcvd.fun,padx = 5,pady = c(5, 10), sticky = "w",  row=3,column=0)
   tkgrid(bouton.hcvreg.fun,padx = 5,pady = c(5, 10), sticky = "w",  row=4,column=0)
   tkgrid(bouton.kern.fun,padx = 5,pady = c(5, 10), sticky = "w",  row=5,column=0)
   tkgrid(bouton.kpmfe.fun,padx = 5,pady = c(5, 10), sticky = "w",  row=6,column=0)
   tkgrid(bouton.reg.fun,padx = 5,pady = c(5, 10), sticky = "w",  row=0,column=1)
   tkgrid(bouton.kef,padx = 5,pady = c(5, 10), sticky = "w",  row=1,column=1)
   tkgrid(bouton.plot.hcvc.fun,padx = 5,pady = c(5, 10), sticky = "w",  row=2,column=1)
   tkgrid(bouton.plot.kern.fun,padx = 5,pady = c(5, 10), sticky = "w",  row=3,column=1)
   tkgrid(bouton.plot.kpmfe.fun,padx = 5,pady = c(5, 10), sticky = "w",  row=4,column=1)
   tkgrid(bouton.plot.dke.fun,padx = 5,pady = c(5, 10), sticky = "w",  row=5,column=1)
   tkgrid(bouton.plot.reg.fun,padx = 5,pady = c(5, 10), sticky = "w",  row=6,column=1)
   
  
}

