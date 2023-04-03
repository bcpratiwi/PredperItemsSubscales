* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.
GLM elnet_Items elnet_Subscales FAREG_Items Meta_method_both ols_Subscales PCovR_Items SPCA_Items BY nitems 
    ntrain r12 sig_items nscales meas
  /WSFACTOR=Rule 7 Polynomial 
  /METHOD=SSTYPE(3)
  /PRINT=ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Rule 
  /DESIGN=nitems ntrain r12 sig_items nscales meas nitems*ntrain nitems*r12 nitems*sig_items 
    nitems*nscales nitems*meas ntrain*r12 ntrain*sig_items ntrain*nscales ntrain*meas r12*sig_items 
    r12*nscales r12*meas sig_items*nscales sig_items*meas nscales*meas nitems*ntrain*r12 
    nitems*ntrain*sig_items nitems*ntrain*nscales nitems*ntrain*meas nitems*r12*sig_items 
    nitems*r12*nscales nitems*r12*meas nitems*sig_items*nscales nitems*sig_items*meas 
    nitems*nscales*meas ntrain*r12*sig_items ntrain*r12*nscales ntrain*r12*meas 
    ntrain*sig_items*nscales ntrain*sig_items*meas ntrain*nscales*meas r12*sig_items*nscales 
    r12*sig_items*meas r12*nscales*meas sig_items*nscales*meas nitems*ntrain*r12*sig_items 
    nitems*ntrain*r12*nscales nitems*ntrain*r12*meas nitems*ntrain*sig_items*nscales 
    nitems*ntrain*sig_items*meas nitems*ntrain*nscales*meas nitems*r12*sig_items*nscales 
    nitems*r12*sig_items*meas nitems*r12*nscales*meas nitems*sig_items*nscales*meas 
    ntrain*r12*sig_items*nscales ntrain*r12*sig_items*meas ntrain*r12*nscales*meas 
    ntrain*sig_items*nscales*meas r12*sig_items*nscales*meas nitems*ntrain*r12*sig_items*nscales 
    nitems*ntrain*r12*sig_items*meas nitems*ntrain*r12*nscales*meas 
    nitems*ntrain*sig_items*nscales*meas nitems*r12*sig_items*nscales*meas 
    ntrain*r12*sig_items*nscales*meas nitems*ntrain*r12*sig_items*nscales*meas.

DATASET ACTIVATE DataSet2.
GLM elnet_Items elnet_Subscales FAREG_Items Meta_method_both PCovR_Items SPCA_Items BY nitems ntrain r12 
    sig_items nscales meas
  /WSFACTOR=Rule 6 Polynomial 
  /METHOD=SSTYPE(3)
  /PRINT=ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Rule 
  /DESIGN=nitems ntrain r12 sig_items nscales meas nitems*ntrain nitems*r12 nitems*sig_items 
    nitems*nscales nitems*meas ntrain*r12 ntrain*sig_items ntrain*nscales ntrain*meas r12*sig_items 
    r12*nscales r12*meas sig_items*nscales sig_items*meas nscales*meas nitems*ntrain*r12 
    nitems*ntrain*sig_items nitems*ntrain*nscales nitems*ntrain*meas nitems*r12*sig_items 
    nitems*r12*nscales nitems*r12*meas nitems*sig_items*nscales nitems*sig_items*meas 
    nitems*nscales*meas ntrain*r12*sig_items ntrain*r12*nscales ntrain*r12*meas 
    ntrain*sig_items*nscales ntrain*sig_items*meas ntrain*nscales*meas r12*sig_items*nscales 
    r12*sig_items*meas r12*nscales*meas sig_items*nscales*meas nitems*ntrain*r12*sig_items 
    nitems*ntrain*r12*nscales nitems*ntrain*r12*meas nitems*ntrain*sig_items*nscales 
    nitems*ntrain*sig_items*meas nitems*ntrain*nscales*meas nitems*r12*sig_items*nscales 
    nitems*r12*sig_items*meas nitems*r12*nscales*meas nitems*sig_items*nscales*meas 
    ntrain*r12*sig_items*nscales ntrain*r12*sig_items*meas ntrain*r12*nscales*meas 
    ntrain*sig_items*nscales*meas r12*sig_items*nscales*meas nitems*ntrain*r12*sig_items*nscales 
    nitems*ntrain*r12*sig_items*meas nitems*ntrain*r12*nscales*meas 
    nitems*ntrain*sig_items*nscales*meas nitems*r12*sig_items*nscales*meas 
    ntrain*r12*sig_items*nscales*meas nitems*ntrain*r12*sig_items*nscales*meas.
