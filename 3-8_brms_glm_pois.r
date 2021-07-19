# 分析の準備 ----

# パッケージの読み込み
library(rstan)
library(brms)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# データの読み込みと図示 -------------------------------------------------------------

# 分析対象のデータ
fish_num_climate <- read.csv("book-data/3-8-1-fish-num-1.csv")
head(fish_num_climate, 3)

# データの要約
summary(fish_num_climate)

# 図示
ggplot(data = fish_num_climate, 
       mapping = aes(x = temperature, y = fish_num)) +
  geom_point(aes(color = weather)) +
  labs(title = "釣獲尾数と気温・天気の関係")

# brmsによるポアソン回帰モデルの推定 ----
glm_pois_brms <- brm(
    formula = fish_num ~ temperature + weather, 
    data = fish_num_climate, 
    family = poisson(), 
    seed = 1, 
    prior = c(set_prior("", class = "Intercept"))
)

glm_pois_brms


# ポアソン回帰の回帰曲線 ----

# 95%ベイズ信用区間付きのグラフ
# marginal_effect非推奨やから代わりにconditional_effectsを使えと怒られた
eff <- conditional_effects(glm_pois_brms, effects = "temperature:weather")
plot(eff, points = TRUE)

# 99%ベイズ予測区間付きのグラフ
# probsじゃなくてprobを使えと怒られた
set.seed(1)
eff_pre <- marginal_effects(glm_pois_brms, 
                            method = "predict",
                            effects = "temperature:weather", 
                            probs = c(0.05, 0.995))
plot(eff_pre, points = TRUE)


# 補足：brmsを用いない実装の方法 ----

# デザイン行列の作成
formula_pois <- formula(fish_num ~ temperature + weather)
design_mat <- model.matrix(formula_pois, fish_num_climate)
design_mat

# データ作成
data_list_1 <- list(
    N = nrow(fish_num_climate),
    fish_num = fish_num_climate$fish_num, 
    temp = fish_num_climate$temperature, 
    sunny = as.numeric(design_mat[, "weathersunny"])
)
data_list_1

# stan関数で実行
glm_pois_stan_exp <- stan(
    file = "3-8_glm_pois.stan", 
    data = data_list_1, 
    seed = 1
)

print(glm_pois_stan_exp, probs = c(0.025, 0.5, 0.975))


# 補足：デザイン行列を使ったモデルの推定 ----
data_list_2 <- list(
    N = nrow(fish_num_climate), 
    K = ncol(design_mat), 
    Y = fish_num_climate$fish_num, 
    X = design_mat
)


glm_pois_stan_design_mat <- stan(
    file = "3-8_glm_pois_design.stan", 
    data = data_list_2, 
    seed =1
)

print(glm_pois_stan_design_mat,
      probs = c(0.025, 0.5, 0.975))
