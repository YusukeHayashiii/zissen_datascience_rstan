data {
    int T;
    int len_obs;
    vector[len_obs] y;
    int obs_no[len_obs];
}

parameters {
    vector[T] mu;
    real<lower=0> s_w;
    real<lower=0> s_v;
}

model {
    for(i in 2:T) {
        mu[i] ~ normal(mu[i-1], s_w);
    }

    // 「観測値が得られた時点」でのみ実行する
    for(i in 1:len_obs) {
        y[i] ~ normal(mu[obs_no[i]], s_v);
    }
}
