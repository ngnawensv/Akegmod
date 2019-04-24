require(tcltk) || stop("Package tcltk non disponible.")
require(tcltk2) || stop("Package tcltk non disponible.")
require(tkrplot) || stop("Package trplot non disponible.")
rm(list=ls())

akeg.hcvc.fun<-function(){
  
  #-----------Fenetre principale--------------
  win3<-tktoplevel()
  tkwm.title(win3,"hcvc.fun: Cross-validation function for bandwidth selection for continuous data")
  tkwm.resizable(win3,FALSE,FALSE)
  tkwm.geometry(win3,"980x530+180+80")
  tkwm.minsize(win3,200,100)
  
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
  frm1 <- tk2frame(win3, borderwidth = 5,relief = "solid",padding = 10)
  tkgrid(frm1,padx = 5, pady = c(5, 5), sticky = "n",row=2,column=0)
  
  frm0 <- tk2frame(win3,relief = "solid",padding =0)
  tkgrid(frm0,padx = 5, pady = c(5, 5),sticky = "s",columnspan=4,row=0,column=0)
  tkgrid(tk2label(frm0, text = "CROSS-VALIDATION FUNCTION FOR BANDWIDTH SELECTION FOR CONTINUOUS DATA", 
                  justify = "center", font=police.titre1),padx = 5,pady = c(5, 10), sticky = "S",columnspan=4,row=0,column=0)
  
  
  frm2 <- tk2frame(win3, borderwidth = 5,relief = "sunken",padding = 10)
  tkgrid(frm2,padx = 5, pady = c(5, 5), sticky = "n",row=3,column=0)

  
  label_vec<-tk2label(frm1, text = "Data sample (Vec) : ", justify = "left",font=police.label)
  zone_saisie_vec1 <- tclVar("100")
  zone_saisie_vec2 <- tclVar("1.5")
  zone_saisie_vec3 <- tclVar("2.6")
  saisie_vec1 <-tk2entry(frm1, width = "5", textvariable =zone_saisie_vec1,font=police.label)
  saisie_vec2 <-tk2entry(frm1, width = "5", textvariable =zone_saisie_vec2,font=police.label)
  saisie_vec3 <-tk2entry(frm1, width = "5", textvariable =zone_saisie_vec3,font=police.label)
  
  label_type_data<-tk2label(frm1, text = "Data sample type: ", justify = "left",font=police.label) #justify permet d'aligner le texte
  tab_type_data<- c("continuous","discrete")
  select_type_data <- tclVar("continuous")
  combo_type_data <- ttkcombobox(frm1, values=tab_type_data, textvariable=select_type_data, state="readonly",font=police.label) 
  
  label_ker<-tk2label(frm1, text = "Smoothing kernel : ", justify = "left",font=police.label)
  tab_ker<- c("GA","BE","LN", "RIG")
  select_ker<- tclVar("GA")
  combo_ker <- ttkcombobox(frm1, values=tab_ker, textvariable=select_ker, state="readonly",font=police.label) 
  
  label_h<-tk2label(frm1, text = "Bandwidth(h) : ", justify = "left",font=police.label)
  zone_saisie_h <- tclVar("0.052")
  saisie_h <-tk2entry(frm1, width = "23", textvariable =zone_saisie_h,font=police.label) 

  
  #géneration du vecteur vec
  vec_fun <- function(){
    # print("-------------------Vecteur des données---------------------")
    v1<- as.numeric(tclvalue(zone_saisie_vec1))
    v2<- as.numeric(tclvalue(zone_saisie_vec2))
    v3<- as.numeric(tclvalue(zone_saisie_vec3))
    v<-rgamma(v1,v2,v3)
    #print(v)
    
  }
  
  #Fonction dke.fun
  hcvc.fun<-function(){
    rm(list=ls())
    Vec<-vec_fun()
    bw=NULL
    type_data<- tclvalue(select_type_data)
    ker<- tclvalue(select_ker)
    a0=0
    a1=1
    if(is.null(bw)) {
      bw=seq((max(Vec)-min(Vec))/200,(max(Vec)-min(Vec))/2, length.out=100)
    }
    result1<-0
    result2<-0
    x=seq(min(Vec),max(Vec),length.out=100)
    n1 <- length(x)
    n2 <- length(Vec)
    m1=matrix(0,n1,length(Vec))
    m2=matrix(0,n2,n2)
    source("kef.R")
    source("simp_int1.R")
    Dak<-Vectorize(kef,vectorize.args=c('x','t'))
    if (type_data == "---Choisir---" || ker == "---Choisir---"){
      tkmessageBox(title="Info...", message = "Selectionner le type de donné et\n le noyau associé!!!")
    }else{
      if (type_data == "continuous"){
    for(k in 1:length(bw)){
      m1 <-outer(x,Vec,Dak,bw[k],type_data,ker,a0,a1)
      m2 <-outer(Vec,Vec,Dak,bw[k],type_data,ker,a0,a1) 
      res1<-apply(m1,1,mean)
      diag(m2)<-0
      res2<-apply(m2,1,sum)
      result1[k]=simp_int(x,res1^2)
      result2[k]=(2/((n2-1)*n2))*sum(res2)
    }
        CV=result1-result2  
        index<-which.min(CV)  #to compute the optimal bandwidth
        hcv<-bw[index]
        
        #the sequence of bandwidths where the cross validation is computed.
        tkgrid(tk2label(win3, text = "SEQUENCE OF BANDWIDTHS", justify = "center",font=police.label3),
               padx = 5, pady = c(5, 5), sticky = "s", row=1,column=1)
        frm3 <- tk2frame(win3, borderwidth = 5, relief = "sunken",padding = 10)
        tkgrid(frm3,padx = 5, pady = c(5, 10), sticky = "n",rowspan=2,row=2,column=1)
        scr1 <- tk2scrollbar(frm3, orient = "vertical",command = function(...) tkyview(txt1, ...))
        txt1 <- tk2text(frm3, bg = "white",font = "courier", width = 20, height = 20,yscrollcommand = function(...) tkset(scr1, ...))
        tkgrid(txt1,scr1, sticky = "nsew")
        tkgrid.rowconfigure(frm3, txt1, weight = 1)
        tkgrid.columnconfigure(frm3,txt1, weight = 1)
        #tkinsert(txt1, "end", paste0("seq_h \n"))
        for (i in bw)
          tkinsert(txt1, "end", paste0(i, "\n"))
        tkconfigure(txt1, state = "disabled")
        tkfocus(txt1)
        
        #the values of cross-validation function.
        tkgrid(tk2label(win3, text = "VALUES OF CROSS-VALIDATION FUNCTION", justify = "center",font=police.label3),
               padx = 5, pady = c(5, 5), sticky = "s", row=1,column=2)
        frm4 <- tk2frame(win3, borderwidth = 5, relief = "sunken",padding = 10)
        tkgrid(frm4,padx = 5, pady = c(5, 10), sticky = "n",rowspan=2,row=2,column=2)
        scr2 <- tk2scrollbar(frm4, orient = "vertical",command = function(...) tkyview(txt2, ...))
        txt2 <- tk2text(frm4, bg = "white",font = "courier", width = 20, height = 20,yscrollcommand = function(...) tkset(scr2, ...))
        tkgrid(txt2,scr2, sticky = "nsew")
        tkgrid.rowconfigure(frm4, txt2, weight = 1)
        tkgrid.columnconfigure(frm4,txt2, weight = 1)
       # tkinsert(txt2, "end", paste0("CV \n"))
        for (i in CV)
          tkinsert(txt2, "end", paste0(i, "\n"))
        tkconfigure(txt2, state = "disabled")
        tkfocus(txt2)
        
        #value of bandwidth parameter.
        label_hcv <- tclVar(" ")
        tclvalue(label_hcv) <-""
        tclvalue(label_hcv) <-hcv
        #tkgrid(tk2label(win3, text = "hcv: ", justify = "left"),padx = 5, pady = c(5, 5), sticky = "w", row=3,column=0)
        tkgrid(tk2label(frm2, text = "VALUE OF BANDWIDTH (hcv) : ", justify = "center",font=police.label1),
               padx = 5,pady = c(5, 10), sticky = "s",row=0,column=0)
        tkgrid(tk2label(frm2, textvariable = label_hcv, justify = "left",font=police.label),
               padx = 5,pady = c(5, 10), sticky = "w",row=0,column=1)
        
        msg<-structure(list(hcv=hcv,seq_h=bw,CV=CV),class="hcvc.fun")
        print(msg)
      }else{
        tkmessageBox(title="Info...", message = "Selectionner le type de données continues!!!")
      }
    }
  }
  
  bouton_hcvcfun<-tkbutton(frm1,width = "30", text = "hcvc.fun", font=police.bouton, command =hcvc.fun)
  
  #Positionnement
  tkgrid(label_vec, padx = 5, pady = c(5, 10),sticky = "w", row=1,column=0)
  tkgrid(saisie_vec1, padx = 5, pady = c(5, 10), sticky = "w",row=1,column=1)
  tkgrid(saisie_vec2, padx = 5, pady = c(5, 10), sticky = "w",row=1,column=2)
  tkgrid(saisie_vec3, padx = 5, pady = c(5, 10), sticky = "w",row=1,column=3)
  
  # tkgrid(label_borne, padx = 5, pady = c(5, 10),sticky = "w", row=2,column=0)
  # tkgrid(saisie_a0, padx = 5, pady = c(5, 10), sticky = "w",row=2,column=1)
  # tkgrid(saisie_a1, padx = 5, pady = c(5, 10), sticky = "w",row=2,column=3)
  
  tkgrid(label_h,padx = 5, pady = c(5, 10), sticky = "w", row=3,column=0)
  tkgrid(saisie_h,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=3,column=1)
  
  tkgrid(label_type_data, padx = 5, pady = c(5, 10),sticky = "w", row=4,column=0)
  tkgrid(combo_type_data,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4,row=4,column=1)
  
  tkgrid(label_ker,padx = 5, pady = c(5, 10), sticky = "w",row=5,column=0)
  tkgrid(combo_ker,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=5,column=1)
  
  tkgrid(bouton_hcvcfun,padx = 5,pady = c(5, 10), sticky = "w",columnspan=4, row=6,column=0)
  
  
}

