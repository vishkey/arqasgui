arqasgui
========

Web Interface in Shiny for the R package "arqas"


# Installation #
---

    git clone git://github.com/vishkey/arqasgui.git
    R CMD build arqasgui/

This will create a file named "arqasgui_VERSION.tar.gz". 

Then move the file into your working directory in R and type:

    > install.packages("arqasgui_VERSION.tar.gz",repos=NULL,type="source")
    > library(arqasgui)

A simpler solution is to use the 'devtools' package.

    > install.packages("devtools")
    > library(devtools)
    > install_github('arqasgui','vishkey')
