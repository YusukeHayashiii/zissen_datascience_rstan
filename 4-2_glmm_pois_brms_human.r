# 分析の準備 ----

# パッケージの読み込み
library(rstan)
library(brms)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# データの読み込み ----

# 分析対象のデータ
fish_num_climate_3 <- read.csv("book-data/4-2-1-fish-num-3.csv")
head(fish_num_climate_3, n = 3)

summary(fish_num_climate_3)

# brmsによるGLMMの推定 ----

# brmsによるGLMMの推定
glmm_pois_brms_human <- brm(
    formula = fish_num ~ weather + temperature + (1|human),
    family = poisson(),
    data = fish_num_climate_3,
    seed = 1,
    prior = c(set_prior("", class = "Intercept"),
              set_prior("", class = "sd"))
)

# 結果の確認
plot(glmm_pois_brms_human)
stanplot(glmm_pois_brms_human, type = "rhat")
glmm_pois_brms_human

# 各々の調査者の影響の大きさ
ranef(glmm_pois_brms_human)

# ランダム切片モデルの回帰曲線の図示 ----

# 調査者ごとにグラフを分けて、回帰曲線を描く
conditions <- data.frame(
    human = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
)

eff_glmm_human <- marginal_effects(
    glmm_pois_brms_human, 
    effects = "temperature:weather", 
    re_formula = NULL,                  # ランダム効果を反映
    conditions = conditions             # グラフの分割の仕方を指定
)
plot(eff_glmm_human, points = TRUE)
