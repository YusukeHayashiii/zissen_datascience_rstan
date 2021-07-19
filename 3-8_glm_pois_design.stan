data {
    int N;
    int K;
    int Y[N];
    matrix[N, K] X;
}

parameters {
    vector[K] b;
}

model {
    vector[N] lambda = X * b;
    Y ~ poisson_log(lambda);
}
