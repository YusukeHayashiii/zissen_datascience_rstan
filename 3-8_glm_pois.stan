data {
    int N;
    int fish_num[N];
    vector[N] temp;
    vector[N] sunny;
}

parameters {
    real Intercept;
    real b_temp;
    real b_sunny;
}

model {
    vector[N] lambda = Intercept + b_temp*temp + b_sunny*sunny;
    fish_num ~ poisson_log(lambda);
}
