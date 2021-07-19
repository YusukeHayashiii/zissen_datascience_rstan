# 分析の準備：全体 ----
# パッケージの読み込み
library(rstan)
library(brms)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# カテゴリ×カテゴリ：モデル化 ----
# 分析対象のデータ
interaction_1 <- read.csv("book-data/3-10-1-interaction-1.csv")
head(interaction_1, n = 3)

# データ要約
summary(interaction_1)

# デザイン行列の作成
model.matrix(sales ~ publicity * bargen, interaction_1)

# モデル化
interaction_brms_1 <- brm(
    formula = sales ~ publicity * bargen, 
    family = gaussian(link = "identity"), 
    data = interaction_1, 
    seed = 1, 
    prior = c(set_prior("", class = "Intercept"), 
              set_prior("", class = "sigma"))
)

interaction_brms_1
plot(interaction_brms_1)


# カテゴリ×カテゴリ：係数の解釈 ----

# 交互作用の効果の確認
# 説明変数を作る
newdata_1 <- data.frame(
    publicity = rep(c("not", "to_implement"), 2), 
    bargen = rep(c("not", "to_implement"), each = 2)
)
newdata_1
# 予測
round(fitted(interaction_brms_1, newdata_1), 2)


# カテゴリ×カテゴリ：モデルの図示 ----

# モデルの図示
eff_1 <- conditional_effects(interaction_brms_1,
                          effects = "publicity:bargen")
plot(eff_1, points = T)


# カテゴリ×数量：モデル化 ----

# 分析対象のデータ
interaction_2 <- read.csv("book-data/3-10-2-interaction-2.csv")
head(interaction_2, n = 3)

# データの要約
summary(interaction_2)

# モデル化
interaction_brms_2 <- brm(
    formula = sales ~ publicity * temperature, 
    family = gaussian(link = "identity"), 
    data = interaction_2, 
    seed = 1, 
    prior = c(set_prior("", class = "Intercept"),
              set_prior("", class = "sigma"))
    )
interaction_brms_2
plot(interaction_brms_2)

# カテゴリ×数量：係数の解釈 ----
# 交互作用の効果の確認
# 説明変数を作る
newdata_2 <- data.frame(
  publicity   = rep(c("not", "to_implement"), each = 2),
  temperature = c(0,10,0,10)
)
newdata_2
# 予測
round(fitted(interaction_brms_2, newdata_2), 2)

# カテゴリ×数量：モデルの図示 ----
# 回帰直線の図示
eff_2 <- conditional_effects(interaction_brms_2,
                          effects = "temperature:publicity")
plot(eff_2, points = T)


# 数量×数量：モデル化 ----
# 分析対象のデータ
interaction_3 <- read.csv("book-data/3-10-3-interaction-3.csv")
head(interaction_3, n = 3)

# データの要約
summary(interaction_3)

# データの図示
ggplot(data = interaction_3,
       aes(x = product, y = sales, color = factor(clerk)))+
  geom_point()

# モデル化
interaction_brms_3 <- brm(
    formula = sales ~ product * clerk, 
    family = gaussian(link = "identity"), 
    data = interaction_3, 
    seed = 1, 
    prior = c(set_prior("", class = "Intercept"),
              set_prior("", class = "sigma"))
)

interaction_brms_3
plot(interaction_brms_3)

# 数量×数量：係数の解釈 ----
# 交互作用の効果の確認
# 説明変数を作る
newdata_3 <- data.frame(
  product = c(0,10,0,10),
  clerk   = c(0,0,10,10)
)
newdata_3
# 予測
round(fitted(interaction_brms_3, newdata_3), 2)


# 数量×数量：モデルの図示 ----
# 回帰直線の図示
# 1つのグラフに回帰直線をまとめて描画する
int_conditions <- list(
  clerk = setNames(1:9, paste("clerk=", 1:9, sep=""))
)
int_conditions

eff_3 <- conditional_effects(interaction_brms_3,
                          effects = "product:clerk",
                          int_conditions = int_conditions)
plot(eff_3, points = TRUE)

# 回帰直線の図示
# 働く人数ごとにグラフを分ける
conditions <- data.frame(clerk = 1:9)
conditions

eff_4 <- conditional_effects(interaction_brms_3,
                          effects = "product",
                          conditions = conditions)
plot(eff_4, points = FALSE)
