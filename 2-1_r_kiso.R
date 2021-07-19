# 元となるデータフレーム
data_frame <- data.frame(
  data = 1:24
)

# 時系列データに変換
ts_1 <- ts(
  data_frame,
  start = c(2010, 1),
  frequency = 12 # 1年におけるデータ数
)
ts_1

birds <- read.csv('book-data/2-1-1-birds.csv')
head(birds, n=3)

# 乱数の生成 -------------------------------------------------------------------

# 平均0、標準偏差1の正規分布に従う乱数を1つ取得
# 実行するたびに結果が変わる
rnorm(n = 1, mean = 0, sd = 1)

# 乱数の固定
set.seed(1)  
rnorm(n = 1, mean = 0, sd = 1)

# 繰り返し構文とforループ ------------------------------------------------------------------
# 要素番号を変えながら実行
result_vec_1 <- c(0, 0, 0) # 結果を保存する入れ物
set.seed(1)                # 乱数の種
for (i in 1:3){
  result_vec_1[i] <- rnorm(n = 1, mean = 0, sd = 1)
}
result_vec_1

# 要素番号を変えながら実行
result_vec_2 <- c(0, 0, 0) # 結果を保存する入れ物
mean_vec <- c(0, 10, -5)   # 平均値を指定したベクトル
set.seed(1)                # 乱数の種
for (i in 1:3){
  result_vec_2[i] <- rnorm(n = 1, mean = mean_vec[i], sd = 1)
}
result_vec_2

# 外部パッケージの利用
library(tidyverse)
