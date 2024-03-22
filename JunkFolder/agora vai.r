teste1 = set_model(
    data = bananas1,
    model_type = "ols",
    dependent_var = "weight",
    independent_var = c("bx"),
    fixed_effects = c("cohort", "grid"))


model = felm(teste1,data = bananas1)
testeboot = cluster.boot(model, R = 10, cluster = bananas1$grid)

birth_interval = c("birth_interval1","birth_interval2","birth_interval3")
teste2 = set_model(
    data = bananas2,
    model_type = "ols",
    dependent_var = "weight",
    independent_var = c("bx",birth_interval),
    fixed_effects = c("cohort", "mfeid*grid"))

model2 = felm(teste2,data = bananas2)
model2
bananas2$mfeid
testeboot = cluster.boot(model = model2, R = 10, cluster = cbind(bananas2$mfeid, bananas2$grid))
testeboot