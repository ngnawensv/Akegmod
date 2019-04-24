require(tcltk) || stop("Package tcltk non disponible.")
require(tcltk2) || stop("Package tcltk non disponible.")
require(tkrplot) || stop("Package trplot non disponible.")

# creation de la fenetre d'acceuil
rm(list=ls())

akeg.kern.fun<-function(){
  
  #-----------Fenetre principale--------------
  win6<-tktoplevel()
  tkwm.title(win6,"kern.fun:  The associated kernel function")
  tkwm.resizable(win6,FALSE,FALSE)
  tkwm.geometry(win6,"980x530+180+80")
  tkwm.minsize(win6,200,100)
  
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
  frm1 <- tk2frame(win6, borderwidth = 5,relief = "solid",padding = 10)
  tkgrid(frm1,padx = 5, pady = c(5, 5), sticky = "n",row=2,column=0)
  
  frm0 <- tk2frame(win6,relief = "solid",padding =0)
  tkgrid(frm0,padx = 5, pady = c(5, 5),sticky = "s",columnspan=4,row=0,column=0)
  tkgrid(tk2label(frm0, text = "THE ASSOCIATED KERNEL FUNCTION", 
                  justify = "center", font=police.titre),padx = 5,pady = c(5, 10), sticky = "s",columnspan=4,row=0,column=0)
  
  frm2 <- tk2frame(win6, borderwidth = 5,relief = "sunken",padding = 10)
  tkgrid(frm2,padx = 5, pady = c(5, 5), sticky = "n",row=3,column=0)

  
  #Les labels
  label_t<-tk2label(frm1, text = "A single value or the grid(t): ", justify = "left",font=police.label)
  zone_saisie_t0 <- tclVar("0")
  zone_saisie_tN <- tclVar("10")
  saisie_t0 <-tk2entry(frm1, width = "10", textvariable =zone_saisie_t0,font=police.label)
  saisie_tN <-tk2entry(frm1, width = "10", textvariable =zone_saisie_tN,font=police.label)
  
  label_type_data<-tk2label(frm1, text = "The sample data type : ", justify = "left",font=police.label) #justify permet d'aligner le texte
  tab_type_data<- c("discrete", "continuous")
  select_type_data <- tclVar("discrete")
  combo_type_data <- ttkcombobox(frm1, values=tab_type_data, textvariable=select_type_data, state="readonly",font=police.label) 
  
  label_ker<-tk2label(frm1, text = "The associated kernel : ", justify = "left",font=police.label)
  tab_ker<- c("bino","triang","dirDU","BE","GA","LN","RIG")
  select_ker<- tclVar("bino")
  combo_ker <- ttkcombobox(frm1, values=tab_ker, textvariable=select_ker, state="readonly",font=police.label) 
  
  label_h<-tk2label(frm1, text = "Smoothing parameter(h) : ", justify = "left",font=police.label)
  zone_saisie_h <- tclVar("0.2")
  saisie_h <-tk2entry(frm1, width = "23", textvariable =zone_saisie_h,font=police.label) 
  
  label_x<-tk2label(frm1, text = "The target(x) : ", justify = "left",font=police.label)
  zone_saisie_x <- tclVar("5")
  saisie_x<-tk2entry(frm1, width = "23", textvariable =zone_saisie_x,font=police.label) 
  
  #géneration du vecteur vec
  vec_fun <- function(){
    t0<-as.integer(tclvalue(zone_saisie_t0))
    tN<-as.integer(tclvalue(zone_saisie_tN))
    tt<-t0:tN
  }
  
  #Fonction kef
  kern.fun <- function(){
    rm(list=ls())
    t<-vec_fun()
    x<-as.numeric(tclvalue(zone_saisie_x))
    h<- as.numeric(tclvalue(zone_saisie_h))
    type_data<- tclvalue(select_type_data)
    ker<- tclvalue(select_ker)
    a0=0
    a1=1
    a=1
    c=2
    if (missing(type_data)){ 
      tkmessageBox(title="Info...", message = "argument 'type_data' is omitted")
      stop("argument 'type_data' is omitted")
    }
    if ((type_data=="discrete") & (ker=="GA"||ker=="LN"||ker=="BE" ||ker=="RIG")){ 
      tkmessageBox(title="Info...", message = "Not appropriate kernel for type_data!!!")
      stop(" Not appropriate kernel for type_data")
    }
    if ((type_data=="continuous") & (ker=="bino"||ker=="triang"||ker=="dirDU")){ 
      tkmessageBox(title="Info...", message = "Not appropriate kernel for 'type_data'!!!")
      stop(" Not appropriate kernel for 'type_data'")
    }
    if ((type_data=="discrete") & missing(ker)) ker<-"bino"
    if ((type_data=="continuous") & missing(ker)) ker<-"GA"
    source("kef.R")
    kx <- kef(x,t,h,type_data,ker,a0,a1,a,c)
    
    #A single value or the grid where the discrete associated kernel function is computed
    tkgrid(tk2label(win6, text = "A SINGLE VALUE OR THE GRID(t): ", justify = "center",font=police.label3),
           padx = 5, pady = c(5, 5), sticky = "s", row=1,column=2)
    frm3 <- tk2frame(win6, borderwidth = 5, relief = "sunken",padding = 10)
    tkgrid(frm3,padx = 5, pady = c(5, 10), sticky = "n",row=2,column=2)
    scr1 <- tk2scrollbar(frm3, orient = "vertical",command = function(...) tkyview(txt1, ...))
    txt1 <- tk2text(frm3, bg = "white",font = "courier", width = 20, height = 15,yscrollcommand = function(...) tkset(scr1, ...))
    tkgrid(txt1,scr1, sticky = "nsew")
    tkgrid.rowconfigure(frm3, txt1, weight = 1)
    tkgrid.columnconfigure(frm3,txt1, weight = 1)
    for (i in t)
      tkinsert(txt1, "end", paste0(i, "\n"))
    tkconfigure(txt1, state = "disabled")
    tkfocus(txt1)
    
    #kx
    tkgrid(tk2label(win6, text = "KERNEL ESTIMATION(Kx): ", justify = "center",font=police.label3),
           padx = 5, pady = c(5, 5), sticky = "s", row=1,column=3)
    frm4 <- tk2frame(win6, borderwidth = 5, relief = "sunken",padding = 10)
    tkgrid(frm4,padx = 5, pady = c(5, 10), sticky = "n",row=2,column=3)
    scr2 <- tk2scrollbar(frm4, orient = "vertical",command = function(...) tkyview(txt2, ...))
    txt2 <- tk2text(frm4, bg = "white",font = "courier", width = 23, height = 15,yscrollcommand = function(...) tkset(scr2, ...))
    tkgrid(txt2,scr2, sticky = "nsew")
    tkgrid.rowconfigure(frm4, txt2, weight = 1)
    tkgrid.columnconfigure(frm4,txt2, weight = 1)
    for (i in kx)
      tkinsert(txt2, "end", paste0(i, "\n"))
    tkconfigure(txt2, state = "disabled")
    tkfocus(txt2)

    
    # #$kernel
    # label_kernel <- tclVar(" ")
    # tclvalue(label_kernel ) <-""
    # tclvalue(label_kernel ) <-ker
    # tkgrid(tk2label(frm1, text = "Ker : ", justify = "left"),padx = 5, pady = c(5, 5), sticky = "w", row=0,column=0)
    # tkgrid(tk2label(frm1, textvariable = label_kernel, justify = "left"),padx = 5,pady = c(5, 10), sticky = "w",row=0,column=1)
    # 
    # #$x
    # label_x <- tclVar(" ")
    # tclvalue(label_x ) <-""
    # tclvalue(label_x) <-x
    # tkgrid(tk2label(frm1, text = "x : ", justify = "left"),padx = 5, pady = c(5, 5), sticky = "w", row=1,column=0)
    # tkgrid(tk2label(frm1, textvariable = label_x, justify = "left"),padx = 5,pady = c(5, 10), sticky = "w",row=1,column=1)
  
    msg<-structure(list(kernel = ker,x=x,t=t,kx=kx),class="kern.fun")
    print(msg)
    
  }

  bouton_kernfun<-tkbutton(frm1,width = "35", text = "kern.fun",font=police.bouton, command = kern.fun)
  
  #Positionnement
  tkgrid(label_t, padx = 5, pady = c(5, 10),sticky = "w", row=0,column=0)
  tkgrid(saisie_t0, padx = 5, pady = c(5, 10), sticky = "w",row=0,column=1)
  tkgrid(saisie_tN, padx = 5, pady = c(5, 10), sticky = "w",row=0,column=2)
  
  tkgrid(label_x,padx = 5, pady = c(5, 10), sticky = "w", row=2,column=0)
  tkgrid(saisie_x,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=2,column=1)
  
  tkgrid(label_h,padx = 5, pady = c(5, 10), sticky = "w", row=3,column=0)
  tkgrid(saisie_h,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=3,column=1)
  
  tkgrid(label_type_data, padx = 5, pady = c(5, 10),sticky = "w", row=4,column=0)
  tkgrid(combo_type_data, padx = 5, pady = c(5, 10), sticky = "w",columnspan=4,row=4,column=1)
  
  tkgrid(label_ker,padx = 5, pady = c(5, 10), sticky = "w",row=5,column=0)
  tkgrid(combo_ker,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=5,column=1)
  
  tkgrid(bouton_kernfun,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=6,column=0)
  
}

