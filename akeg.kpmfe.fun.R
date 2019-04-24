require(tcltk) || stop("Package tcltk non disponible.")
require(tcltk2) || stop("Package tcltk non disponible.")
require(tkrplot) || stop("Package trplot non disponible.")

# creation de la fenetre d'acceuil
rm(list=ls())

akeg.kpmfe.fun<-function(){
  
  #-----------Fenetre principale--------------
  win7<-tktoplevel()
  tkwm.title(win7,"kpmfe.fun: Function for associated kernel estimation of p.m.f.")
  tkwm.resizable(win7,FALSE,FALSE)
  tkwm.geometry(win7,"1300x700+20+5")
  #tkwm.minsize(win7,200,100)
  
  
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
  police.titre1  <- tkfont.create(family="arial", size=14, weight="bold",slant="italic")
  police.sous.titre <- tkfont.create(family="arial", size=12,underline=F)
  
  #-------------Les Frames------------------------------
  frm0 <- tk2frame(win7,relief = "solid",padding =0)
  tkgrid(frm0,padx = 5, pady = c(5, 5),sticky = "n",columnspan=6,row=0,column=0)
  tkgrid(tk2label(frm0, text = "FUNCTION FOR ASSOCIATED KERNEL ESTIMATION OF P.M.F", 
                  justify = "center", font=police.titre1),padx = 5,pady = c(5, 10), sticky = "s",columnspan=4,row=0,column=0)
  
  frm1 <- tk2frame(win7, borderwidth = 5,relief = "solid",padding = 10)
  tkgrid(frm1,padx = 5, pady = c(5, 5), sticky = "nsew",columnspan=3,row=1,column=0)
  

  #Les labels
  label_type_data<-tk2label(frm1, text = "Data sample type : ", justify = "left",font=police.label) #justify permet d'aligner le texte
  tab_type_data<- c("discrete", "continuous")
  select_type_data <- tclVar("discrete")
  combo_type_data <- ttkcombobox(frm1, values=tab_type_data, textvariable=select_type_data, state="readonly",font=police.label) 
  
  label_ker<-tk2label(frm1, text = "The associated kernel: ", justify = "left",font=police.label)
  tab_ker<- c("bino","triang","dirDU")
  select_ker<- tclVar("bino")
  combo_ker <- ttkcombobox(frm1, values=tab_ker, textvariable=select_ker, state="readonly",font=police.label) 
  
  label_h<-tk2label(frm1, text = "Bandwidth(h): ", justify = "left",font=police.label)
  zone_saisie_h <- tclVar("0.081")
  saisie_h <-tk2entry(frm1, width = "23", textvariable =zone_saisie_h,font=police.label) 
  
  #géneration du vecteur vec
  vec_fun <- function(){
    V<-c(10,0,1,0,4,0,6,0,0,0,1,1,1,2,4,4,5,6,6,6,6,7,1,7,0,
         7,7,7,8,0,8,12,8,8,9,9,0,9,9,10,10,10,10,0,10,10,11,
         12,12,10,12,12,13,14,15,16,16,17,0,12)
  }
  
  
  kpmfe.fun <- function(){
    rm(list=ls())
    Vec<-vec_fun()
    h<- as.numeric(tclvalue(zone_saisie_h))
    type_data<- tclvalue(select_type_data)
    ker<- tclvalue(select_ker)
    x=NULL
    a=1
    c=2
      
      ###########################################################################################################
      # INPUTS:
      #   "Vec" 	: Sample of data 
      #   "h" 		: Bandwidth.
      #   "ker" 	: The kernel function:  "dirDU" DiracDU,"bino" Binomial,"triang" discrete Triangular. 		   
      #   "a" 		: The arm is used only for the Discrete Triangular kernel. The default value is 1.
      #   "c" 		: The number of categories in the Aitchison and Aitken kernel  is used only for DiracaDU.The default value is 2.
      # OUTPUT: Returns a list containing:
      #    "n" 		: The number of observations.
      #    "support" 		: The support of fn.
      #    "C_n" 		: The normalizant constant.
      #    "ISE_0" 		: The integrated squared error when using the naive distribution instead of fn.
      #    "f_0" 		: The couples (x,f_0(x)).
      #    "f_n" 		: The couples (x,f_n(x)).
      #    "f0" 		: The empirical p.m.f.
      #    "fn" 		: The estimated p.m.f. containing estimated values after normalization.
      ###########################################################################################################
      V=data.frame(table(Vec),row.names=NULL)
      N=V$Freq 
      if(is.null(x)){
        if(ker=="dirDU"){x=0:(max(Vec))}
        else {x=0:(max(Vec)+2)}
      }
      t1=rep(0,length(x))
      t2=rep(0,length(x))
      n <- length(x)
      f0=c(N/sum(N),rep(0,length(x)-length(N)))
      m=matrix(0,n,length(Vec))
      source("kef.R")
      for(i in 1:n){
        m[i,]= kef(x[i],Vec,h,type_data,ker,a,c)
      }
      res<-apply(m,1,mean)
      result<-res/sum(res)
      E0=sum((result-f0)^2)
      for (i in 1:n){
        t1[i]=paste(x[i],";",f0[i])
        t2[i]=paste(x[i],";",result[i])
        
      }
      #The number of observations
      tkgrid(tk2label(win7, text = "DATA", justify = "center",font=police.label3),
             padx = 5, pady = c(5, 5), sticky = "s", row=2,column=0)
      frm3 <- tk2frame(win7, borderwidth = 5, relief = "sunken",padding = 10)
      tkgrid(frm3,padx = 5, pady = c(5, 10), sticky = "n",row=3,column=0)
      scr1 <- tk2scrollbar(frm3, orient = "vertical",command = function(...) tkyview(txt1, ...))
      txt1 <- tk2text(frm3, bg = "white",font = "courier", width =3, height = 20,yscrollcommand = function(...) tkset(scr1, ...))
      tkgrid(txt1,scr1, sticky = "nsew")
      tkgrid.rowconfigure(frm3, txt1, weight = 1)
      tkgrid.columnconfigure(frm3,txt1, weight = 1)
      for (i in Vec)
        tkinsert(txt1, "end", paste0(i, "\n"))
      tkconfigure(txt1, state = "disabled")
      tkfocus(txt1)
      
      #The support of the estimated p.m.f.
      tkgrid(tk2label(win7, text = "EVAL.POINTS", justify = "center",font=police.label3),
             padx = 5, pady = c(5, 5), sticky = "s", row=2,column=1)
      frm4 <- tk2frame(win7, borderwidth = 5, relief = "sunken",padding = 10)
      tkgrid(frm4,padx = 5, pady = c(5, 10), sticky = "n",row=3,column=1)
      scr2 <- tk2scrollbar(frm4, orient = "vertical",command = function(...) tkyview(txt2, ...))
      txt2 <- tk2text(frm4, bg = "white",font = "courier", width = 3, height = 20,yscrollcommand = function(...) tkset(scr2, ...))
      tkgrid(txt2,scr2, sticky = "nsew")
      tkgrid.rowconfigure(frm4, txt2, weight = 1)
      tkgrid.columnconfigure(frm4,txt2, weight = 1)
      for (i in x)
        tkinsert(txt2, "end", paste0(i, "\n"))
      tkconfigure(txt2, state = "disabled")
      tkfocus(txt2)
      
      #A vector of (x,f0(x)).
      tkgrid(tk2label(win7, text = "A VECTOR OF (x,f0(x))", justify = "center",font=police.label3),
             padx = 5, pady = c(5, 5), sticky = "s", row=2,column=2)
      frm5 <- tk2frame(win7, borderwidth = 5, relief = "sunken",padding = 10)
      tkgrid(frm5,padx = 5, pady = c(5, 10), sticky = "n",row=3,column=2)
      scr3 <- tk2scrollbar(frm5, orient = "vertical",command = function(...) tkyview(txt3, ...))
      txt3 <- tk2text(frm5, bg = "white",font = "courier", width = 24, height = 20,yscrollcommand = function(...) tkset(scr3, ...))
      tkgrid(txt3,scr3, sticky = "nsew")
      tkgrid.rowconfigure(frm5, txt3, weight = 1)
      tkgrid.columnconfigure(frm5,txt3, weight = 1)
      for (i in t1)
        tkinsert(txt3, "end", paste0(i, "\n"))
      tkconfigure(txt3, state = "disabled")
      tkfocus(txt3)
      
      #A vector of (x,fn(x)).
      tkgrid(tk2label(win7, text = "A VECTOR OF (x,fn(x))", justify = "center",font=police.label3),
             padx = 5, pady = c(5, 5), sticky = "s", row=2,column=3)
      frm6 <- tk2frame(win7, borderwidth = 5, relief = "sunken",padding = 10)
      tkgrid(frm6,padx = 5, pady = c(5, 10), sticky = "n",row=3,column=3)
      scr4 <- tk2scrollbar(frm6, orient = "vertical",command = function(...) tkyview(txt4, ...))
      txt4 <- tk2text(frm6, bg = "white",font = "courier", width = 24, height = 20,yscrollcommand = function(...) tkset(scr4, ...))
      tkgrid(txt4,scr4, sticky = "nsew")
      tkgrid.rowconfigure(frm6, txt4, weight = 1)
      tkgrid.columnconfigure(frm6,txt4, weight = 1)
      for (i in t2)
        tkinsert(txt4, "end", paste0(i, "\n"))
      tkconfigure(txt4, state = "disabled")
      tkfocus(txt4)
      
      #The empirical p.m.f.
      tkgrid(tk2label(win7, text = "EMPIRICAL P.M.F", justify = "center",font=police.label3),
             padx = 5, pady = c(5, 5), sticky = "s", row=2,column=4)
      frm7 <- tk2frame(win7, borderwidth = 5, relief = "sunken",padding = 10)
      tkgrid(frm7,padx = 5, pady = c(5, 10), sticky = "n",row=3,column=4)
      scr5 <- tk2scrollbar(frm7, orient = "vertical",command = function(...) tkyview(txt5, ...))
      txt5 <- tk2text(frm7, bg = "white",font = "courier", width = 18, height = 20 ,yscrollcommand = function(...) tkset(scr5, ...))
      tkgrid(txt5,scr5, sticky = "nsew")
      tkgrid.rowconfigure(frm7, txt5, weight = 1)
      tkgrid.columnconfigure(frm7,txt5, weight = 1)
      for (i in f0)
        tkinsert(txt5, "end", paste0(i, "\n"))
      tkconfigure(txt5, state = "disabled")
      tkfocus(txt5)
      
      #The estimated p.m.f. containing estimated values after normalization
      tkgrid(tk2label(win7, text = "THE ESTIMATED P.M.F", justify = "left",font=police.label3),
             padx = 5, pady = c(5, 5), sticky = "s", row=2,column=5)
      frm8 <- tk2frame(win7, borderwidth = 5, relief = "sunken",padding = 10)
      tkgrid(frm8,padx = 5, pady = c(5, 10), sticky = "n",row=3,column=5)
      scr6 <- tk2scrollbar(frm8, orient = "vertical",command = function(...) tkyview(txt6, ...))
      txt6 <- tk2text(frm8, bg = "white",font = "courier", width = 19, height = 20,yscrollcommand = function(...) tkset(scr6, ...))
      tkgrid(txt6,scr6, sticky = "nsew")
      tkgrid.rowconfigure(frm8, txt6, weight = 1)
      tkgrid.columnconfigure(frm8,txt6, weight = 1)
      for (i in result)
        tkinsert(txt6, "end", paste0(i, "\n"))
      tkconfigure(txt6, state = "disabled")
      tkfocus(txt6)
      
      msg<-structure(list(data=Vec,n=length(Vec),eval.points= x,h=h, kernel=ker,C_n=sum(res),ISE_0 = E0,f_0=t1,f_n=t2,f0=f0,est.fn=result),class="kpmfe_fun") 
      print(msg)
    
  }

  bouton_kepmfefun<-tkbutton(frm1,width = "35", text ="kepmfe.fun",font=police.bouton, command =kpmfe.fun)
  
  
  tkgrid(label_h,padx = 5, pady = c(5, 10), sticky = "w", row=2,column=0)
  tkgrid(saisie_h,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=2,column=1)
  
  tkgrid(label_type_data, padx = 5, pady = c(5, 10),sticky = "w", row=3,column=0)
  tkgrid(combo_type_data, padx = 5, pady = c(5, 10), sticky = "w",columnspan=4,row=3,column=1)
  
  tkgrid(label_ker,padx = 5, pady = c(5, 10), sticky = "w",row=4,column=0)
  tkgrid(combo_ker,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=4,column=1)
  
  tkgrid(bouton_kepmfefun,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=5,column=0)
  
}

