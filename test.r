require(tcltk)
tt  <- tktoplevel()
txt <- tktext(tt,bg="white",font="courier")
tkgrid(txt)
tkmark.set(txt,"insert","0.0")
tkfocus(txt)