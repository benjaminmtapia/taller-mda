require(bnlearn)
library("Rgraphviz")

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
plot(res)
print(score(res,bn_df)) #BIC

#Aca se realiza la comparacion entre el res (que es el HC)con el dag (que es el model2network)
compare(res,dag, arcs = TRUE)

# MAX MIN HC
resmmhc <- mmhc (bn_df)
plot(resmmhc)
print(score(resmmhc,bn_df))

#Aca se realiza la comparacion entre el resmmhc con el dag (que es el model2network)
compare(resmmhc,dag, arcs = TRUE)

# MAX MIN PARENT CHILDREN
resmmpc <- mmpc(bn_df)
plot(resmmpc)
print(score(resmmpc,bn_df)) # no printea, error

#Aca se realiza la comparacion entre el resmmpc con el dag (que es el model2network)
compare(resmmpc,dag, arcs = TRUE)

################ Aca hacia abajo falta revisar

#se usa el modelo con mejor bic
fittedbn <- bn.fit(res, data = bn_df) # Se obtiene la tabla de probabilidades condicionales mediante EM. (Máxima Expectación, propagación de la evidencia)

bl <- matrix(c("Age","GoodStudent","Age","SeniorTrain","SocioEcon","OtherCar","ThisCarDam","Theft","MakeModel","VehicleYear","SeniorTrain","Age","Mileage","Mileage"),ncol=2,byrow = TRUE)

hc2 <- hc(bn_df,blacklist = bl)
plot(hc2)
