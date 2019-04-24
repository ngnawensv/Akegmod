require(tcltk) || stop("Package tcltk non disponible.")
require(tcltk2) || stop("Package tcltk non disponible.")
require(tkrplot) || stop("Package trplot non disponible.")
require(Ake) || stop("Package Ake non disponible.")
rm(list=ls())

akeg.reg.fun<-function(){
  #-----------Fenetre principale--------------
  win8<-tktoplevel()
  tkwm.title(win8,"reg.fun: Function for associated kernel estimation of regression")
  tkwm.resizable(win8,FALSE,FALSE)
  tkwm.geometry(win8,"1100x550+100+80")
  #tkwm.minsize(win8,200,100)
  
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
  frm0 <- tk2frame(win8,relief = "solid",padding =0)
  tkgrid(frm0,padx = 5, pady = c(5, 5),sticky = "s",columnspan=6,row=0,column=0)
  tkgrid(tk2label(frm0, text = "FUNCTION FOR ASSOCIATED KERNEL ESTIMATION OF REGRESSION", 
                  justify = "center", font=police.titre1),padx = 5,pady = c(5, 10), sticky = "s",columnspan=4,row=0,column=0)
  
  frm1 <- tk2frame(win8, borderwidth = 5,relief = "solid",padding = 10)
  tkgrid(frm1,padx = 5, pady = c(5, 5), sticky = "n",row=2,column=0)

  frm2 <- tk2frame(win8, borderwidth = 5,relief = "sunken",padding = 10)
  tkgrid(frm2,padx = 5, pady = c(5, 5), sticky = "n",row=3,column=0)
  
  label_type_data<-tk2label(frm1, text = "Type de données : ", justify = "left",font=police.label) #justify permet d'aligner le texte
  tab_type_data<- c("discrete", "continuous")
  select_type_data <- tclVar("discrete")
  combo_type_data <- ttkcombobox(frm1, values=tab_type_data, textvariable=select_type_data, state="readonly",font=police.label) 
  
  label_ker<-tk2label(frm1, text = "Noyau associé : ", justify = "left",font=police.label)
  tab_ker<- c("bino","triang","dirDU","BE","GA","LN","RIG")
  select_ker<- tclVar("bino")
  combo_ker <- ttkcombobox(frm1, values=tab_ker, textvariable=select_ker, state="readonly",font=police.label)

  label_h<-tk2label(frm1, text = "h : ", justify = "left",font=police.label)
  zone_saisie_h <- tclVar("0.10")
  saisie_h <-tk2entry(frm1, width = "23", textvariable =zone_saisie_h,font=police.label) 
  
  #La premiere colonne de la fentre
  label_frm1_donnees<-tk2label(win8, text = "Données d'entrée", justify = "center",font=police.label)
  label_seq_h<-tk2label(win8, text = "seq_h: ", justify = "left",font=police.label)
  label_cv<-tk2label(win8, text = "CV: ", justify = "left",font=police.label)

  
  #géneration du vecteur vec
  vec_fun <- function(){
    data(milk)
    list(xx=milk$week,yy=milk$yield)
  }
  
  #Fonction dke.fun
  reg.fun<-function(Vec,y,type_data,ker,h,x,a0,a1,a,c,...){
    rm(list=ls())
    x=NULL
    a0=0
    a1=1
    a=1
    c=2
    Vec<-vec_fun()
    type_data<- tclvalue(select_type_data)
    ker<- tclvalue(select_ker)
    h<-as.numeric(tclvalue(zone_saisie_h))
    y=Vec$yy
    
    if (missing(type_data))  stop("argument 'type_data' is omitted")
    if ((type_data=="discrete") & (ker=="GA"||ker=="LN"||ker=="BE" ||ker=="RIG")) 
      stop(" Not appropriate kernel for type_data")
    if ((type_data=="continuous") & (ker=="bino"||ker=="triang"||ker=="dirDU")) 
      stop(" Not appropriate kernel for 'type_data'")
    if ((type_data=="discrete") & missing(ker)) ker<-"bino"
    if ((type_data=="continuous") & missing(ker)) ker<-"GA"
    
    if(is.null(x)){x=Vec$xx}
    n <- length(x)
    m=matrix(0,n,length(Vec$xx))
    m2=matrix(0,n,length(Vec$xx))
    
    source("kef.R")
    for(i in 1:n){
      for(j in 1:length(Vec$xx)){
        m[i,]= kef(x[i],Vec$xx,h,type_data,ker,a0,a1,a,c)
        m2[i,]= m[i,]*y
      }
    }
    res<-apply(m,1,sum)
    res2<-apply(m2,1,sum)
    result<-res2/res
    moyY<-mean(y)
    R<-sum(((result-moyY)^2))/sum(((y-moyY)^2)) # Coefficient de détermination R^2
    
    #data=Vec$xx
    tkgrid(tk2label(win8, text = "Data", justify = "center",font=police.label3),
           padx = 5, pady = c(5, 5), sticky = "s", row=1,column=1)
    frm3 <- tk2frame(win8, borderwidth = 5, relief = "sunken",padding = 10)
    tkgrid(frm3,padx = 5, pady = c(5, 10), sticky = "n",rowspan=4,row=2,column=1)
    scr1 <- tk2scrollbar(frm3, orient = "vertical",command = function(...) tkyview(txt1, ...))
    txt1 <- tk2text(frm3, bg = "white",font = "courier", width = 5, height = 20,yscrollcommand = function(...) tkset(scr1, ...))
    tkgrid(txt1,scr1, sticky = "nsew")
    tkgrid.rowconfigure(frm3, txt1, weight = 1)
    tkgrid.columnconfigure(frm3,txt1, weight = 1)
    for (i in Vec$xx)
      tkinsert(txt1, "end", paste0(i, "\n"))
    tkconfigure(txt1, state = "disabled")
    tkfocus(txt1)
    
    #y=y
    tkgrid(tk2label(win8, text = "y", justify = "center",font=police.label3),
           padx = 5, pady = c(5, 5), sticky = "s", row=1,column=2)
    frm4 <- tk2frame(win8, borderwidth = 5, relief = "sunken",padding = 10)
    tkgrid(frm4,padx = 5, pady = c(5, 10), sticky = "n",rowspan=4,row=2,column=2)
    scr2 <- tk2scrollbar(frm4, orient = "vertical",command = function(...) tkyview(txt2, ...))
    txt2 <- tk2text(frm4, bg = "white",font = "courier", width = 10, height = 20,yscrollcommand = function(...) tkset(scr2, ...))
    tkgrid(txt2,scr2, sticky = "nsew")
    tkgrid.rowconfigure(frm4, txt2, weight = 1)
    tkgrid.columnconfigure(frm4,txt2, weight = 1)
    for (i in y)
      tkinsert(txt2, "end", paste0(i, "\n"))
    tkconfigure(txt2, state = "disabled")
    tkfocus(txt2)
    
    #eval.points=x
    tkgrid(tk2label(win8, text = "eval.points", justify = "center",font=police.label3),
           padx = 5, pady = c(5, 5), sticky = "s", row=1,column=3)
    frm5 <- tk2frame(win8, borderwidth = 5, relief = "sunken",padding = 10)
    tkgrid(frm5,padx = 5, pady = c(5, 10), sticky = "n",rowspan=4,row=2,column=3)
    scr3 <- tk2scrollbar(frm5, orient = "vertical",command = function(...) tkyview(txt3, ...))
    txt3 <- tk2text(frm5, bg = "white",font = "courier", width = 5, height = 20,yscrollcommand = function(...) tkset(scr3, ...))
    tkgrid(txt3,scr3, sticky = "nsew")
    tkgrid.rowconfigure(frm5, txt3, weight = 1)
    tkgrid.columnconfigure(frm5,txt3, weight = 1)
    for (i in x)
      tkinsert(txt3, "end", paste0(i, "\n"))
    tkconfigure(txt3, state = "disabled")
    tkfocus(txt3)
    
    #m_n = result
    tkgrid(tk2label(win8, text = "m_n", justify = "center",font=police.label3),
           padx = 5, pady = c(5, 5), sticky = "s", row=1,column=4)
    frm6 <- tk2frame(win8, borderwidth = 5, relief = "sunken",padding = 10)
    tkgrid(frm6,padx = 5, pady = c(5, 10), sticky = "n",rowspan=4,row=2,column=4)
    scr4 <- tk2scrollbar(frm6, orient = "vertical",command = function(...) tkyview(txt4, ...))
    txt4 <- tk2text(frm6, bg = "white",font = "courier", width = 20, height = 20,yscrollcommand = function(...) tkset(scr4, ...))
    tkgrid(txt4,scr4, sticky = "nsew")
    tkgrid.rowconfigure(frm6, txt4, weight = 1)
    tkgrid.columnconfigure(frm6,txt4, weight = 1)
    for (i in result)
      tkinsert(txt4, "end", paste0(i, "\n"))
    tkconfigure(txt4, state = "disabled",font=police.label2)
    tkfocus(txt4)
    
    #Coef_det=R
    label_coef_det <- tclVar(" ")
    tclvalue(label_coef_det) <-""
    tclvalue(label_coef_det) <-R
    tkgrid(tk2label(frm2, text = "Coef_det : ", justify = "center",font=police.label1),
           padx = 5,pady = c(5, 10), sticky = "s",row=0,column=0)
    tkgrid(tk2label(frm2, textvariable = label_coef_det, justify = "left",font=police.label),
           padx = 5,pady = c(5, 10), sticky = "w",row=0,column=1)
    msg<-structure(list(data=Vec$xx,y=y,n=length(Vec),kernel=ker,h=h,
                   eval.points=x, m_n = result,Coef_det=R),class="reg.fun")
    print(msg)
    
    
  }
  

  bouton_regfun<-tkbutton(frm1,width = "40", text = "reg.fun",font=police.bouton, command =function()reg.fun(x,y,type_data,ker,h))
  
  #Positionnement
  
  tkgrid(label_h,padx = 5, pady = c(5, 10), sticky = "w", row=3,column=0)
  tkgrid(saisie_h,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=3,column=1)
  
  tkgrid(label_type_data, padx = 5, pady = c(5, 10),sticky = "w", row=4,column=0)
  tkgrid(combo_type_data,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4,row=4,column=1)
  
  tkgrid(label_ker,padx = 5, pady = c(5, 10), sticky = "w",row=5,column=0)
  tkgrid(combo_ker,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=5,column=1)
  
  tkgrid(bouton_regfun,padx = 5,pady = c(5, 10), sticky = "w",columnspan=4, row=6,column=0)
  
  
}

