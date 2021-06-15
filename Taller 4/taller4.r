require(bnlearn)
library("Rgraphviz")
library(bnviewer)


data <-data(insurance)

modelstring = paste0("[Age][Mileage][SocioEcon|Age][GoodStudent|Age:SocioEcon]",
                     "[RiskAversion|Age:SocioEcon][OtherCar|SocioEcon][VehicleYear|SocioEcon:RiskAversion]",
                     "[MakeModel|SocioEcon:RiskAversion][SeniorTrain|Age:RiskAversion]",
                     "[HomeBase|SocioEcon:RiskAversion][AntiTheft|SocioEcon:RiskAversion]",
                     "[RuggedAuto|VehicleYear:MakeModel][Antilock|VehicleYear:MakeModel]",
                     "[DrivingSkill|Age:SeniorTrain][CarValue|VehicleYear:MakeModel:Mileage]",
                     "[Airbag|VehicleYear:MakeModel][DrivQuality|RiskAversion:DrivingSkill]",
                     "[Theft|CarValue:HomeBase:AntiTheft][Cushioning|RuggedAuto:Airbag]",
                     "[DrivHist|RiskAversion:DrivingSkill][Accident|DrivQuality:Mileage:Antilock]",
                     "[ThisCarDam|RuggedAuto:Accident][OtherCarCost|RuggedAuto:Accident]",
                     "[MedCost|Age:Accident:Cushioning][ILiCost|Accident]",
                     "[ThisCarCost|ThisCarDam:Theft:CarValue][PropCost|ThisCarCost:OtherCarCost]")

#Este es el model to network
dag = model2network(modelstring)

# HC
bn_df <- data.frame(insurance)
res <- hc(bn_df)
#plot(res)
print(score(res,bn_df)) #BIC
print(logLik(res,bn_df))

viewer(res,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_with_sugiyama",
       bayesianNetwork.title="Discrete Bayesian Network",
       bayesianNetwork.subtitle = "Insurance Dataset",
)   

#Aca se realiza la comparacion entre el res (que es el HC)con el dag (que es el model2network)
c1 <-compare(res,dag, arcs = TRUE)

# MAX MIN HC
resmmhc <- mmhc (bn_df)
plot(resmmhc)
print(score(resmmhc,bn_df))
print(logLik(resmmhc,bn_df))

#Aca se realiza la comparacion entre el resmmhc con el dag (que es el model2network)
c2 <- compare(resmmhc,dag, arcs = TRUE)

# MAX MIN PARENT CHILDREN
resmmpc <- mmpc(bn_df)
plot(resmmpc)
print(score(resmmpc,bn_df)) # no printea, error
print(logLik(resmmpc,bn_df))

#Aca se realiza la comparacion entre el resmmpc con el dag (que es el model2network)
c3<-compare(resmmpc,dag, arcs = TRUE)

################ Aca hacia abajo falta revisar

#se usa el modelo con mejor bic
fittedbn <- bn.fit(res, data = bn_df) # Se obtiene la tabla de probabilidades condicionales mediante EM. (Máxima Expectación, propagación de la evidencia)

bl <- matrix(c("Age","GoodStudent","Age","SeniorTrain","SocioEcon","OtherCar","ThisCarDam","Theft","MakeModel","VehicleYear","SeniorTrain","Age","Mileage","Mileage","HomeBase","OtherCar"),ncol=2,byrow = TRUE)

hc2 <- hc(bn_df,blacklist = bl)
plot(hc2)


viewer(hc2,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_with_sugiyama",
       bayesianNetwork.title="Discrete Bayesian Network",
       bayesianNetwork.subtitle = "Insurance Dataset",
)   


####Consultas

#dado un accidente, como afecta el drive quality y la edad?


cpquery(fittedbn, evidence = (DrivQuality=="Poor" & Age=="Adolescent"),event = (Accident=="Severe")) #0.2937788
cpquery(fittedbn, evidence = (DrivQuality=="Poor" & Age=="Adult"),event = (Accident=="Severe")) #0.3121983
cpquery(fittedbn, evidence = (DrivQuality=="Poor" & Age=="Senior"),event = (Accident=="Severe")) #0.3060109

cpquery(fittedbn, evidence = (DrivQuality=="Poor" & Age=="Adolescent"),event = (Accident=="Moderate")) #0.1822403
cpquery(fittedbn, evidence = (DrivQuality=="Poor" & Age=="Adult"),event = (Accident=="Moderate")) #0.191838
cpquery(fittedbn, evidence = (DrivQuality=="Poor" & Age=="Senior"),event = (Accident=="Moderate")) #0.1883853

cpquery(fittedbn, evidence = (DrivQuality=="Normal" & Age=="Adolescent"),event = (Accident=="Severe")) #0.00969988
cpquery(fittedbn, evidence = (DrivQuality=="Normal" & Age=="Adult"),event = (Accident=="Severe")) #0.00748469
cpquery(fittedbn, evidence = (DrivQuality=="Normal" & Age=="Senior"),event = (Accident=="Severe")) #0.0055555555


cpquery(fittedbn, evidence = (DrivQuality=="Poor" & Age=="Adolescent"),event = (Accident=="Moderate")) # 0.1915014
cpquery(fittedbn, evidence = (DrivQuality=="Poor" & Age=="Adult"),event = (Accident=="Moderate")) #0.1844343
cpquery(fittedbn, evidence = (DrivQuality=="Poor" & Age=="Senior"),event = (Accident=="Moderate")) #0.1565097


#como afectan abs y airbag al costo medico?
#todas dan 0, nada que ver
cpquery(fittedbn, evidence = (Antilock=="True" & Airbag=="False"),event = (Accident=="Thousand")) # 0
cpquery(fittedbn, evidence = (Antilock=="True" & Airbag=="True"),event = (Accident=="Thousand")) #0
cpquery(fittedbn, evidence = (Antilock=="False" & Airbag=="False"),event = (Accident=="Thousand")) #0


#La edad y el modelo del auto tienen que ver en el drive quality?
cpquery(fittedbn, evidence = (Age=="Adolescent" & MakeModel=="Economy"),event = (DrivQuality=="Normal")) #  0.2734584
cpquery(fittedbn, evidence = (Age=="Adult" & MakeModel=="Economy"),event = (DrivQuality=="Normal")) # 0.4751814
cpquery(fittedbn, evidence = (Age=="Senior" & MakeModel=="Economy"),event = (DrivQuality=="Normal")) # 0.5210697
cpquery(fittedbn, evidence = (Age=="Adolescent" & MakeModel=="Economy"),event = (DrivQuality=="Poor")) # 0.579929
cpquery(fittedbn, evidence = (Age=="Adult" & MakeModel=="Economy"),event = (DrivQuality=="Poor")) # 0.3467249
cpquery(fittedbn, evidence = (Age=="Senior" & MakeModel=="Economy"),event = (DrivQuality=="Poor")) # 0.2457002
cpquery(fittedbn, evidence = (Age=="Adolescent" & MakeModel=="Economy"),event = (DrivQuality=="Excelent")) # 0
cpquery(fittedbn, evidence = (Age=="Adult" & MakeModel=="Economy"),event = (DrivQuality=="Excelent")) # 0
cpquery(fittedbn, evidence = (Age=="Senior" & MakeModel=="Economy"),event = (DrivQuality=="Excelent")) # 0


cpquery(fittedbn, evidence = (Age=="Adolescent" & MakeModel=="FamilySedan"),event = (DrivQuality=="Normal")) #  0.2951907
cpquery(fittedbn, evidence = (Age=="Adult" & MakeModel=="FamilySedan"),event = (DrivQuality=="Normal")) # 0.4926576
cpquery(fittedbn, evidence = (Age=="Senior" & MakeModel=="FamilySedan"),event = (DrivQuality=="Normal")) # 0.4730159
cpquery(fittedbn, evidence = (Age=="Adolescent" & MakeModel=="FamilySedan"),event = (DrivQuality=="Poor")) # 0.6149279
cpquery(fittedbn, evidence = (Age=="Adult" & MakeModel=="FamilySedan"),event = (DrivQuality=="Poor")) # 0.3620835
cpquery(fittedbn, evidence = (Age=="Senior" & MakeModel=="FamilySedan"),event = (DrivQuality=="Poor")) # 0.2391073
cpquery(fittedbn, evidence = (Age=="Adolescent" & MakeModel=="FamilySedan"),event = (DrivQuality=="Excelent")) # 0
cpquery(fittedbn, evidence = (Age=="Adult" & MakeModel=="FamilySedan"),event = (DrivQuality=="Excelent")) # 0
cpquery(fittedbn, evidence = (Age=="Senior" & MakeModel=="FamilySedan"),event = (DrivQuality=="Excelent")) # 0



cpquery(fittedbn, evidence = (Age=="Adolescent" & MakeModel=="SportsCar"),event = (DrivQuality=="Normal")) #  0.3154206
cpquery(fittedbn, evidence = (Age=="Adult" & MakeModel=="SportsCar"),event = (DrivQuality=="Normal")) # 0.513289
cpquery(fittedbn, evidence = (Age=="Senior" & MakeModel=="SportsCar"),event = (DrivQuality=="Normal")) # 0.4739336
cpquery(fittedbn, evidence = (Age=="Adolescent" & MakeModel=="SportsCar"),event = (DrivQuality=="Poor")) # 0.5140187
cpquery(fittedbn, evidence = (Age=="Adult" & MakeModel=="SportsCar"),event = (DrivQuality=="Poor")) # 0.3263833
cpquery(fittedbn, evidence = (Age=="Senior" & MakeModel=="SportsCar"),event = (DrivQuality=="Poor")) # 0.2243902
cpquery(fittedbn, evidence = (Age=="Adolescent" & MakeModel=="SportsCar"),event = (DrivQuality=="Excelent")) # 0
cpquery(fittedbn, evidence = (Age=="Adult" & MakeModel=="SportsCar"),event = (DrivQuality=="Excelent")) # 0
cpquery(fittedbn, evidence = (Age=="Senior" & MakeModel=="SportsCar"),event = (DrivQuality=="Excelent")) # 0


cpquery(fittedbn, evidence = (Age=="Adolescent" & MakeModel=="Luxury"),event = (DrivQuality=="Normal")) #  0.3201439
cpquery(fittedbn, evidence = (Age=="Adult" & MakeModel=="Luxury"),event = (DrivQuality=="Normal")) # 0.4778068
cpquery(fittedbn, evidence = (Age=="Senior" & MakeModel=="Luxury"),event = (DrivQuality=="Normal")) # 0.4675926
cpquery(fittedbn, evidence = (Age=="Adolescent" & MakeModel=="Luxury"),event = (DrivQuality=="Poor")) # 0.5873606
cpquery(fittedbn, evidence = (Age=="Adult" & MakeModel=="Luxury"),event = (DrivQuality=="Poor")) # 0.3353884
cpquery(fittedbn, evidence = (Age=="Senior" & MakeModel=="Luxury"),event = (DrivQuality=="Poor")) # 0.24821
cpquery(fittedbn, evidence = (Age=="Adolescent" & MakeModel=="Luxury"),event = (DrivQuality=="Excelent")) # 0
cpquery(fittedbn, evidence = (Age=="Adult" & MakeModel=="Luxury"),event = (DrivQuality=="Excelent")) # 0
cpquery(fittedbn, evidence = (Age=="Senior" & MakeModel=="Luxury"),event = (DrivQuality=="Excelent")) # 0




