library(soiltexture)
library(plotrix)
library(stringr)
data(soils)
# sand silt and clay must be capitalized
names(txt) <- toupper(names(soils)) 

# calculates the texture class
txt$TxtClass <- TT.points.in.classes(tri.data = txt,
                      class.sys ="USDA.TT",
                      PiC.type = 't')

# Texture class doesn't follow USDA (nor SWAT) notation,
# i.e. Sand should be S and not Sa, Loam is L and not Lo, and Clay is C
# not Cl.
txt$TxtClass <-str_replace(txt$TxtClass, "[a]", '')
txt$TxtClass <-str_replace(txt$TxtClass, "[o]", '')
txt$TxtClass <-str_replace(txt$TxtClass, "[l]", '')
# capitalized for SWAT
toupper(txt$TxtClass)
# paste each soil profile together, separating by hyphen
paste(txt$TxtClass[1:5], collapse= '-')

# for plotting
# TT.plot(class.sys='USDA.TT', tri.data = txt, text.tol = 3)