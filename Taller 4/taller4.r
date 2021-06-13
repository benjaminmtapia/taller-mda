require(bnlearn)
data <-data(insurance)

bn_df <- data.frame(insurance)
res <- hc(bn_df)
plot(res)
print(score(res,bn_df)) #BIC

#max min hc
resmmhc <- mmhc (bn_df)
plot(resmmhc)
print(score(resmmhc,bn_df))
#max min parent children
resmmpc <- mmpc(bn_df)
plot(resmmpc)
print(score(resmmpc,bn_df)) # no printea, error

#se usa el modelo con mejor bic
fittedbn <- bn.fit(res, data = bn_df) # Se obtiene la tabla de probabilidades condicionales mediante EM. (Máxima Expectación, propagación de la evidencia)

bl <- matrix(c("Age","GoodStudent","Age","SeniorTrain","SocioEcon","OtherCar","ThisCarDam","Theft","MakeModel","VehicleYear","SeniorTrain","Age","Mileage","Mileage"),ncol=2,byrow = TRUE)

hc2 <- hc(bn_df,blacklist = bl)
plot(hc2)