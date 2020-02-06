
#inerpolações de densidade de CO2

#densidade de CO2 entre temperaturas de, 300 e 305, e 320 e 325
attach(interpol)
names(interpol)
#[1] "Pinicial.Pa."         "Pfinal.Pa."           "Pinicial.MPa."        "Pfinal.MPa."         
#[5] "Temp1"                "Temp2"                "Press..o1.MPa."       "press..o2.MPa."      
#[9] "DensidadeTemp1.2MPa." "DesidadeTemp2.2MPa."  "DensidadeTemp1.3MPa." "DensidadeTemp2.3MPa."

#interpolação da densidade para pressão de 2 mepascal 

attach(ultim)
names(ultim)
plot(Pinicial.bar.~Densidade_na_T_2MPa)
attach(interp)
modrhot <-lm(rho2Mpa~temperatura,interp)
summary(modrhot)
plot(rho2Mpa~temperatura)
88.3302-0.1633*(303.15)
modrhot2 <-lm(rho3Mpa~temperatura)
summary(modrhot2)
