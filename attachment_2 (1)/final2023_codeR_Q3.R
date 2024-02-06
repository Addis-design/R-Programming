library(sfcr)

rm(list=ls())

## Mod?le PC de base, pour nous donner un ?tat stationnaire 
## initial pour les variables r?elles

## Basic PC model to get initial steady state for
## all real variables

pc_eqs <- sfcr_set(
  Y ~ C + G,
  YD ~ Y - T + r[-1] * Bh[-1],
  T ~ theta * (Y + r[-1] * Bh[-1]),
  V ~  V[-1] + (YD - C), 
  C ~ alpha1 * YD + alpha2 * V[-1],
  Hh ~ V - Bh,
  Bh ~ V * (lambda0 + lambda1 * r - lambda2 * ( YD/V )),
  Bs ~ Bs[-1] + (G + r[-1] * Bs[-1]) - (T + r[-1] * Bcb[-1]),
  Hs ~ Hs[-1] + Bcb - Bcb[-1],
  Bcb ~ Bs - Bh,
  redondant ~ Hs - Hh 
)

pc_exog <- sfcr_set(
  r ~ 0.025,
  G ~ 20,
  alpha1 ~ 0.8, 
  alpha2 ~ 0.2, 
  theta ~ 0.2,  
  lambda0 ~ 0.635, 
  lambda1 ~ 5, 
  lambda2 ~ 0.01 
)

# Simulons ? partir de rien
# Simulate from scratch

nper <- 200

pcbase <- sfcr_baseline(
  equations = pc_eqs, 
  external = pc_exog, 
  periods = nper,
  method = "Broyden"
)

#######################################################
## Cr?ons le m?me mod?le avec variables r?elles
## et variables nominales. Nous voulons que le niveau
## des variables r?elles soit le m?me dans ce mod?le
## que dans le mod?le pcbase
##
## Create same model but with real and nominal var.
## We want the level of real variables be the same as 
## in model pcbase
#######################################################

pcnom_eqs <- sfcr_set(
  eq1 = Y ~ C + G, 
  eq2 = Yn ~ Y * P,
  eq3 = N ~ Y / prod,
  eq4 = W ~ xi_w*W[-1] + (1-xi_w)*(w0 + w1*(exp(100*(N/N_fe-1))-1)),
  eq5 = Wn ~ W*P,
  eq6 = WB ~ Wn * N,
  eq7 = Profits ~ Yn - WB,
  eq8 = UCn ~ WB / Y,
  eq9 = P ~ xi_p*P[-1]+(1-xi_p)*(1+markup)*UCn,
  eq10 = Pi ~ (P/P[-1]-1),
  eq11 = YDn ~ WB + Profits + r[-1] * Bh[-1] - TX,
  eq12 = YD ~ YDn / P,
  eq13 = TX ~ theta * (WB + Profits + r[-1] * Bh[-1]),
  eq14 = Vn ~ Vn[-1] + YDn - Cn,
  eq15 = V ~ Vn / P,
  eq16 = C ~ alpha1 * YD + alpha2 * V[-1],
  eq17 = Cn ~ C * P,
  eq18 = Hh ~ Vn - Bh,
  eq19 = Bh ~ Vn * (lambda0 + lambda1 * r - lambda2 * ( YDn/Vn )),
  eq20 = Bs ~ Bs[-1] + (Gn + r[-1] * Bs[-1]) - (TX + r[-1] * Bcb[-1]),
  eq21 = G ~ Gn / P,
  eq22 = Hs ~ Hs[-1] + Bcb - Bcb[-1],
  eq23 = Bcb ~ Bs - Bh,
  eq24 = r ~ rbar,
  eq25 = redondant ~ Hs - Hh 
)

nper <- 200 

pcnom_exog <- sfcr_set(
  prod ~ 1,
  N_fe ~ pcbase$Y[200],
  markup ~ 0.1,
  
  w0 ~ 1/1.1,
  w1 ~ 0.01,
  
  rbar ~ 0.025,
  Gn ~ 20,
  theta ~ 0.2,  

  alpha1 ~ 0.8, 
  alpha2 ~ 0.2, 

  lambda0 ~ 0.635, 
  lambda1 ~ 5, 
  lambda2 ~ 0.01,

  xi_p ~ 0.5,
  xi_w ~ 0.5
)


# On veut le m?me ?tat stationnaire pour les variable r?elles que celles obtenues dans le mod?le sans inflation
# We want the same steady state for real variables as the ones obtained in the model without inflation

pcnom_initval <- sfcr_set(
  P ~ 1,    
  Pi ~ 0,
  Y ~ pcbase$Y[nper], 
  Yn ~ pcbase$Y[nper],
  N ~ pcbase$Y[nper],
  W ~ 1/1.1,
  Wn ~ 1/1.1,
  WB ~ pcbase$Y[nper]/1.1,
  Profits ~ pcbase$Y[nper] - pcbase$Y[nper]/1.1,
  UCn ~ 1/1.1,
  YDn ~ pcbase$YD[nper],
  YD ~ pcbase$YD[nper],
  TX ~ pcbase$T[nper], # theta < 1
  Vn ~ pcbase$V[nper],
  V ~ pcbase$V[nper],
  C ~ pcbase$C[nper], # 0 < alpha2 < alpha1 < 1
  Cn ~ pcbase$C[nper],
  Hh ~ pcbase$Hh[nper],
  Bh ~ pcbase$Bh[nper],
  Bs ~ pcbase$Bs[nper],
  G ~ pcbase$G[nper],
  Hs ~ pcbase$Hs[nper],
  Bcb ~ pcbase$Bcb[nper],
  r ~ 0.025,
  redondant ~ 0 
)

pcnom <- sfcr_baseline(
  equations = pcnom_eqs, 
  external = pcnom_exog, 
  initial = pcnom_initval,
  periods = nper,
  method = "Broyden"
)

#######################################################
## M?me mod?le, avec r?gle de politique fiscale
##
## Same model, but with more complex fiscal policy
#######################################################

pcfisc_eqs <- sfcr_set(
  eq1 = Y ~ C + G, 
  eq2 = Yn ~ Y * P,
  eq3 = N ~ Y / prod,
  eq4 = W ~ xi_w*W[-1] + (1-xi_w)*(w0 + w1*(exp(100*(N/N_fe-1))-1)),
  eq5 = Wn ~ W*P,
  eq6 = WB ~ Wn * N,
  eq7 = Profits ~ Yn - WB,
  eq8 = UCn ~ WB / Y,
  eq9 = P ~ xi_p*P[-1]+(1-xi_p)*(1+markup)*UCn,
  eq10 = Pi ~ (P/P[-1]-1),
  eq11 = YDn ~ WB + Profits + r[-1] * Bh[-1] - TX,
  eq12 = YD ~ YDn / P,
  eq13 = TX ~ theta * (WB + Profits + r[-1] * Bh[-1]),
  eq14 = Vn ~ Vn[-1] + YDn - Cn,
  eq15 = V ~ Vn / P,
  eq16 = C ~ alpha1 * YD + alpha2 * V[-1],
  eq17 = Cn ~ C * P,
  eq18 = Hh ~ Vn - Bh,
  eq19 = Bh ~ Vn * (lambda0 + lambda1 * r - lambda2 * ( YDn/Vn )),
  eq20 = Bs ~ Bs[-1] + (Gn + r[-1] * Bs[-1]) - (TX + r[-1] * Bcb[-1]),
  eq21 = Gn ~ (1-fiscsw)*((1-rhog)*Gnbar + rhog*Gn[-1]) + fiscsw*(Gnbar+e_gn),
  eq22 = G ~ Gn / P,
  eq23 = Hs ~ Hs[-1] + Bcb - Bcb[-1],
  eq24 = Bcb ~ Bs - Bh,
  eq25 = r ~ rbar,
  eq26 = redondant ~ Hs - Hh,
)

nper <- 200 

pcfisc_exog <- sfcr_set(
  pcnom_exog,
  e_gn ~ 0,
  rhog ~ 0.5,
  Gnbar ~ pcnom$Gn[nper],
  fiscsw ~ 0,
  exclude = 7
)

pcfisc_initval <- sfcr_set(
  pcnom_initval,
  Gn ~ pcnom$Gn[nper]
)

pcfisc <- sfcr_baseline(
  equations = pcfisc_eqs, 
  external = pcfisc_exog, 
  initial = pcfisc_initval,
  periods = nper,
  method = "Broyden"
)

###################################
# Simulez jusqu'ici, puis commencez ? r?pondre
# ? la question 3
#
# Simulate up to here, then start answering question 3
###################################

