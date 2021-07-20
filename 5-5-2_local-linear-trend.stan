data {
    int T;
    vector[T] y;
}

parameters {
    vector[T] mu;
    vector[T] delta;
    real<lower=0> s_w;
    real<lower=0> s_z;
    real<lower=0> s_v;
}

model {
    // 弱情報事前分布
    s_w ~ normal(2, 2);
    s_z ~ normal(0.5, 0.5);
    s_v ~ normal(10, 5);

    for(i in 2:T) {
        delta[i] ~ normal(delta[i-1], s_z);
        mu[i] ~ normal(mu[i-1] + delta[i-1], s_w);
    }

    for(i in 1:T) {
        y[i] ~ normal(mu[i], s_v);
    }
}
