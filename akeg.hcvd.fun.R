require(tcltk) || stop("Package tcltk non disponible.")
require(tcltk2) || stop("Package tcltk non disponible.")
require(tkrplot) || stop("Package trplot non disponible.")

akeg.hcvd.fun<-function(){
  
  #-----------Fenetre principale--------------
  win4<-tktoplevel()
  tkwm.title(win4,"hcvd.fun: Cross-validation function for bandwidth selection in p.m.f. estimation for discrete data")
  tkwm.resizable(win4,FALSE,FALSE)
  tkwm.geometry(win4,"980x530+180+80")
  tkwm.minsize(win4,200,100)
  
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
  frm1 <- tk2frame(win4, borderwidth = 5,relief = "solid",padding = 10)
  tkgrid(frm1,padx = 5, pady = c(5, 5), sticky = "n",row=2,column=0)
  
  frm0 <- tk2frame(win4,relief = "solid",padding =0)
  tkgrid(frm0,padx = 5, pady = c(5, 5),sticky = "s",columnspan=4,row=0,column=0)
  tkgrid(tk2label(frm0, text = "CROSS-VALIDATION FUNCTION FOR BANDWIDTH SELECTION IN P.M.F ESTIMATION", 
                  justify = "center", font=police.titre1),padx = 5,pady = c(5, 10), sticky = "s",columnspan=4,row=0,column=0)
  
  
  frm2 <- tk2frame(win4, borderwidth = 5,relief = "sunken",padding = 10)
  tkgrid(frm2,padx = 5, pady = c(5, 5), sticky = "n",row=3,column=0)
  
  # label_vec<-tk2label(frm1, text = "Data sample (Vec) : ", justify = "left",font=police.label)
  # zone_saisie_vec1 <- tclVar("100")
  # zone_saisie_vec2 <- tclVar("1.5")
  # zone_saisie_vec3 <- tclVar("2.6")
  # saisie_vec1 <-tk2entry(frm1, width = "5", textvariable =zone_saisie_vec1,font=police.label)
  # saisie_vec2 <-tk2entry(frm1, width = "5", textvariable =zone_saisie_vec2,font=police.label)
  # saisie_vec3 <-tk2entry(frm1, width = "5", textvariable =zone_saisie_vec3,font=police.label)

  label_ker<-tk2label(frm1, text = "Associated kernel : ", justify = "left",font=police.label)
  tab_ker<- c("bino","triang","dirDU")
  select_ker<- tclVar("bino")
  combo_ker <- ttkcombobox(frm1, values=tab_ker, textvariable=select_ker, state="readonly",font=police.label) 

  #géneration du vecteur vec
  vec_fun <- function(){
    v<-c(10,0,1,0,4,0,6,0,0,0,1,1,1,2,4,4,5,6,6,6,6,7,1,7,0,7,7,7,8,0,8,12,8,8,9,9,0,9,9,10,10,
              10,10,0,10,10,11,12,12,10,12,12,13,14,15,16,16,17,0,12)
    
  }
  
  #Fonction dke.fun
  hcvd.fun<-function(){
    rm(list=ls())
    Vec<-vec_fun()
    seq_bws=NULL
    ker<- tclvalue(select_ker)
    a=1
    c=2
    if(is.null(seq_bws)) 
      if (ker=="bino")
      {
        seq_bws=seq((max(Vec)-min(Vec))/500,1, length.out=100) 
      }
    else
    {
      seq_bws=seq((max(Vec)-min(Vec))/200,(max(Vec)-min(Vec))/2, length.out=100)
    }
  
    result1<-0
    result2<-0
    if(ker=="dirDU"){x=0:(max(Vec))}# The values on the support must be up to max
    else {x=0:(max(Vec)+2)}# and up to two points after the max for other kernels.
    n1 <- length(x)
    n2 <- length(Vec)
    m1=matrix(0,n1,length(Vec))
    m2=matrix(0,n2,n2)
    source("kef.R")
    Dak<-Vectorize(kef,vectorize.args=c('x','t')) 
    for(k in 1:length(seq_bws)){
      m1 <-outer(x,Vec,Dak,seq_bws[k],"discrete",ker,a,c)
      m2 <-outer(Vec,Vec,Dak,seq_bws[k],"discrete",ker,a,c) 
      
      res1<-apply(m1,1,mean)
      diag(m2)<-0
      res2<-apply(m2,1,sum)
      result1[k]=sum(res1^2)
      result2[k]=(2/((n2-1)*n2))*sum(res2)
    }
    CV=result1-result2    
    index<-which.min(CV)  #to compute the optimal bandwidth
    hcv<-seq_bws[index]
    
    #The sequence of bandwidths where the cross-validation is computed.
    tkgrid(tk2label(win4, text = "SEQUENCE OF BANDWIDTHS ", justify = "center",font=police.label3),
           padx = 5, pady = c(5, 5), sticky = "s", row=1,column=1)
    frm3 <- tk2frame(win4, borderwidth = 5, relief = "sunken",padding = 10)
    tkgrid(frm3,padx = 5, pady = c(5, 10), sticky = "n",rowspan=2,row=2,column=1)
    scr1 <- tk2scrollbar(frm3, orient = "vertical",command = function(...) tkyview(txt1, ...))
    txt1 <- tk2text(frm3, bg = "white",font = "courier", width = 20, height = 20,yscrollcommand = function(...) tkset(scr1, ...))
    tkgrid(txt1,scr1, sticky = "nsew")
    tkgrid.rowconfigure(frm3, txt1, weight = 1)
    tkgrid.columnconfigure(frm3,txt1, weight = 1)
    #tkinsert(txt1, "end", paste0("seq_h \n"))
    for (i in seq_bws)
      tkinsert(txt1, "end", paste0(i, "\n"))
    tkconfigure(txt1, state = "disabled")
    tkfocus(txt1)
    
    #The cross-validation function values
    tkgrid(tk2label(win4, text = "CROSS-VALIDATION FUNCTION VALUES ", justify = "center",font=police.label3),
           padx = 5, pady = c(5, 5), sticky = "s", row=1,column=2)
    frm4 <- tk2frame(win4, borderwidth = 5, relief = "sunken",padding = 10)
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
    
    #The optimal bandwidth parameter
    label_hcv <- tclVar(" ")
    tclvalue(label_hcv) <-""
    tclvalue(label_hcv) <-hcv
    #tkgrid(tk2label(win4, text = "OPTIMAL BANDWIDTH PARAMETER (hcv) ", justify = "left"),padx = 5, pady = c(5, 5), sticky = "w", row=0,column=4)
    tkgrid(tk2label(frm2, text = "Optimal bandwidth parameter(hcv) ", justify = "center",font=police.label1),
           padx = 5,pady = c(5, 10), sticky = "s",row=0,column=0)
    tkgrid(tk2label(frm2, textvariable = label_hcv, justify = "center",font=police.label3),
           padx = 5,pady = c(5, 10), sticky = "s",row=1,column=0)
    
    #return(hcv=hcv)#,CV=CV,seq_bws=seq_bws))
    msg<- structure(list(hcv=hcv,seq_h=seq_bws,CV=CV),class="hcvd.fun")
    print(msg)
    
    }
  
  
  

  bouton_hcvdfun<-tkbutton(frm1,width = "30", text = "hcvd.fun", font=police.bouton,command =hcvd.fun)
  
  #Positionnement
  # tkgrid(label_vec, padx = 5, pady = c(5, 10),sticky = "w", row=1,column=0)
  # tkgrid(saisie_vec1, padx = 5, pady = c(5, 10), sticky = "w",row=1,column=1)
  # tkgrid(saisie_vec2, padx = 5, pady = c(5, 10), sticky = "w",row=1,column=2)
  # tkgrid(saisie_vec3, padx = 5, pady = c(5, 10), sticky = "w",row=1,column=3)
  
  tkgrid(label_ker,padx = 5, pady = c(5, 10), sticky = "w",row=5,column=0)
  tkgrid(combo_ker,padx = 5, pady = c(5, 10), sticky = "w",columnspan=4, row=5,column=1)
  
  tkgrid(bouton_hcvdfun,padx = 5,pady = c(5, 10), sticky = "w",columnspan=4, row=6,column=0)
  
  
}

