# 分析の準備 ----
# パッケージの読み込み
library(rstan)
library(brms)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# データの読み込みと図示 -------------------------------------------------------------

# 分析対象のデータ
germination_dat <- read.csv("book-data/3-9-1-germination.csv")
head(germination_dat, n = 3)

# データの要約
summary(germination_dat)

# 図示
ggplot(data = germination_dat, 
       mapping = aes(x = nutrition, y = germination, color = solar)) +
  geom_point() +
  labs(title = "種子の発芽数と、日照の有無・栄養素の量の関係")

# brmsによるロジスティック回帰モデルの推定 ----

# モデル作成
glm_binom_brms <- brm(
    germination | trials(size) ~ solar + nutrition, 
    family = binomial(), 
    data = germination_dat, 
    seed = 1, 
    prior = c(set_prior("", class = "Intercept"))
)

glm_binom_brms


# 推定されたモデルの解釈 ----

# 係数の解釈
# 説明変数を作る
newdata_1 <- data.frame(
    solar = c("shade", "sunshine", "sunshine"), 
    nutrition = c(2,2,3), 
    size = c(10,10,10)
)
newdata_1

# 発芽率を予測
# 線形予測子の予測値
linear_fit <- fitted(glm_binom_brms, newdata_1, scale = "linear")[,1]
# ロジスティック関数を適用
fit <- 1 / (1 + exp(-linear_fit))
fit

# オッズの計算
odds_1 <- fit[1] / (1 - fit[1])
odds_2 <- fit[2] / (1 - fit[2])
odds_3 <- fit[3] / (1 - fit[3])

# モデルの係数を取得
coef <- fixef(glm_binom_brms)[,1]
coef

# solarがshadeからsunshineに変わった時のオッズ比
odds_2 / odds_1
exp(coef["solarsunshine"])

# nutritionが1から2に変わった時のオッズ比
odds_3 / odds_2
exp(coef["nutrition"])

# 95%ベイズ信用区間付きの回帰曲線
eff <- conditional_effects(glm_binom_brms,
                        effects = "nutrition:solar", 
                        conditions = data.frame(size = 10)
                        )
plot(eff, points = TRUE)


# brmsを用いない実装の方法 -----

# ダミー変数の作成
solar_dummy <- as.numeric(germination_dat$solar == "sunshine")
solar_dummy

# データの作成
data_list_1 <- list(
    N = nrow(germination_dat),
    germination = germination_dat$germination,
    solar = solar_dummy, 
    nutrition = germination_dat$nutrition,
    binom_size = germination_dat$size
)
data_list_1

# stanコードの実装
glm_binom_stan <- stan(
    file = "book-data/3-9-1-glm-binom-1.stan",
    data = data_list_1, 
    seed = 1 
)

# 結果
print(glm_binom_stan, probs = c(0.025, 0.5, 0.975))
