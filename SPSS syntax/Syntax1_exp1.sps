* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.
GLM elnet_Items elnet_Subscales Meta_method_both ols_Subscales PCovR_Items SPCA_Items BY nitems 
    ntrain r12 sig_comps nscales meas
  /WSFACTOR=Rule 6 Polynomial 
  /METHOD=SSTYPE(3)
  /PRINT=ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Rule 
  /DESIGN=nitems ntrain r12 sig_comps nscales meas nitems*ntrain nitems*r12 nitems*sig_comps 
    nitems*nscales nitems*meas ntrain*r12 ntrain*sig_comps ntrain*nscales ntrain*meas r12*sig_comps 
    r12*nscales r12*meas sig_comps*nscales sig_comps*meas nscales*meas nitems*ntrain*r12 
    nitems*ntrain*sig_comps nitems*ntrain*nscales nitems*ntrain*meas nitems*r12*sig_comps 
    nitems*r12*nscales nitems*r12*meas nitems*sig_comps*nscales nitems*sig_comps*meas 
    nitems*nscales*meas ntrain*r12*sig_comps ntrain*r12*nscales ntrain*r12*meas 
    ntrain*sig_comps*nscales ntrain*sig_comps*meas ntrain*nscales*meas r12*sig_comps*nscales 
    r12*sig_comps*meas r12*nscales*meas sig_comps*nscales*meas nitems*ntrain*r12*sig_comps 
    nitems*ntrain*r12*nscales nitems*ntrain*r12*meas nitems*ntrain*sig_comps*nscales 
    nitems*ntrain*sig_comps*meas nitems*ntrain*nscales*meas nitems*r12*sig_comps*nscales 
    nitems*r12*sig_comps*meas nitems*r12*nscales*meas nitems*sig_comps*nscales*meas 
    ntrain*r12*sig_comps*nscales ntrain*r12*sig_comps*meas ntrain*r12*nscales*meas 
    ntrain*sig_comps*nscales*meas r12*sig_comps*nscales*meas nitems*ntrain*r12*sig_comps*nscales 
    nitems*ntrain*r12*sig_comps*meas nitems*ntrain*r12*nscales*meas 
    nitems*ntrain*sig_comps*nscales*meas nitems*r12*sig_comps*nscales*meas 
    ntrain*r12*sig_comps*nscales*meas nitems*ntrain*r12*sig_comps*nscales*meas.

DATASET ACTIVATE DataSet2.
GLM elnet_Items elnet_Subscales Meta_method_both PCovR_Items SPCA_Items BY nitems ntrain r12 
    sig_comps nscales meas
  /WSFACTOR=Rule 5 Polynomial 
  /METHOD=SSTYPE(3)
  /PRINT=ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Rule 
  /DESIGN=nitems ntrain r12 sig_comps nscales meas nitems*ntrain nitems*r12 nitems*sig_comps 
    nitems*nscales nitems*meas ntrain*r12 ntrain*sig_comps ntrain*nscales ntrain*meas r12*sig_comps 
    r12*nscales r12*meas sig_comps*nscales sig_comps*meas nscales*meas nitems*ntrain*r12 
    nitems*ntrain*sig_comps nitems*ntrain*nscales nitems*ntrain*meas nitems*r12*sig_comps 
    nitems*r12*nscales nitems*r12*meas nitems*sig_comps*nscales nitems*sig_comps*meas 
    nitems*nscales*meas ntrain*r12*sig_comps ntrain*r12*nscales ntrain*r12*meas 
    ntrain*sig_comps*nscales ntrain*sig_comps*meas ntrain*nscales*meas r12*sig_comps*nscales 
    r12*sig_comps*meas r12*nscales*meas sig_comps*nscales*meas nitems*ntrain*r12*sig_comps 
    nitems*ntrain*r12*nscales nitems*ntrain*r12*meas nitems*ntrain*sig_comps*nscales 
    nitems*ntrain*sig_comps*meas nitems*ntrain*nscales*meas nitems*r12*sig_comps*nscales 
    nitems*r12*sig_comps*meas nitems*r12*nscales*meas nitems*sig_comps*nscales*meas 
    ntrain*r12*sig_comps*nscales ntrain*r12*sig_comps*meas ntrain*r12*nscales*meas 
    ntrain*sig_comps*nscales*meas r12*sig_comps*nscales*meas nitems*ntrain*r12*sig_comps*nscales 
    nitems*ntrain*r12*sig_comps*meas nitems*ntrain*r12*nscales*meas 
    nitems*ntrain*sig_comps*nscales*meas nitems*r12*sig_comps*nscales*meas 
    ntrain*r12*sig_comps*nscales*meas nitems*ntrain*r12*sig_comps*nscales*meas.


* without signal comps


DATASET ACTIVATE DataSet1.
GLM elnet_Items elnet_Subscales Meta_method_both ols_Subscales PCovR_Items SPCA_Items BY nitems 
    ntrain r12 nscales meas
  /WSFACTOR=Rule 6 Polynomial 
  /METHOD=SSTYPE(3)
  /PRINT=ETASQ HOMOGENEITY 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Rule 
  /DESIGN=nitems ntrain r12 nscales meas nitems*ntrain nitems*r12 nitems*nscales nitems*meas 
    ntrain*r12 ntrain*nscales ntrain*meas r12*nscales r12*meas nscales*meas nitems*ntrain*r12 
    nitems*ntrain*nscales nitems*ntrain*meas nitems*r12*nscales nitems*r12*meas nitems*nscales*meas 
    ntrain*r12*nscales ntrain*r12*meas ntrain*nscales*meas r12*nscales*meas nitems*ntrain*r12*nscales 
    nitems*ntrain*r12*meas nitems*ntrain*nscales*meas nitems*r12*nscales*meas ntrain*r12*nscales*meas 
    nitems*ntrain*r12*nscales*meas.
