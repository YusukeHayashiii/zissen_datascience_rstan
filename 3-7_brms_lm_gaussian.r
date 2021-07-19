# 分析の準備 ----
# パッケージの読み込み
library(rstan)
library(brms)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# データの読み込みと図示 -------------------------------------------------------------

# 分析対象のデータ
sales_climate <- read.csv("book-data/3-7-1-beer-sales-4.csv")
head(sales_climate, 3)

# データの要約
summary(sales_climate)

# 図示
ggplot(data = sales_climate, 
       mapping = aes(x = temperature, y = sales)) +
  geom_point(aes(color = weather)) +
  labs(title = "ビールの売り上げと気温・天気の関係")

# brmsを用いた正規線形モデルの推定----

# 正規線形モデル
ln_brms <- brm(
    formula = sales ~ weather + temperature, 
    family = gaussian(), 
    data = sales_climate, 
    seed = 1, 
    prior = c(set_prior("", class = "Intercept"), 
              set_prior("", class = "sigma"))
)
ln_brms
prior_summary(ln_brms)

# 回帰直線
# effectを設定して天気ごとに線を引いている
eff <- marginal_effects(ln_brms, effects = "temperature:weather")
plot(eff, points = TRUE)


# 補足：正規線形モデルのデザイン行列 ----

# デザイン行列の作成
formula_lm <- formula(sales ~ weather + temperature)
design_mat <- model.matrix(formula_lm, sales_climate)

design_mat
