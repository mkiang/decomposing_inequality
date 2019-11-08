functions {
    /**
    * Return the log probability of an intrinsic autoregressive (IAR) prior 
    * with a sparse representation for the adjacency matrix
    *
    * Note: the `(n-1)` in the log probability indicates we have one connected
    *       component. In the future, may need to generalize this to k-connected
    *       components that can be passed in as an argument. 
    *
    * @param phi Vector containing the parameters with a IAR prior
    * @param tau Precision parameter for the IAR prior (real)
    * @param W_sparse Sparse representation of adjacency matrix (int array)
    * @param n Length of phi (int)
    * @param W_n Number of adjacent pairs (int)
    * @param D_sparse Number of neighbors for each location (vector)
    * @param lambda Eigenvalues of D^{-1/2} * W * D^{-1/2} (vector)
    *
    * @return Log probability density of IAR prior up to additive constant
    * 
    * See: https://github.com/mbjoseph/CARstan 
    *      (https://doi.org/10.5281/zenodo.210407)
    */
    real sparse_iar_lpdf(vector phi, real tau, int[,] W_sparse, 
                         vector D_sparse, vector lambda, int n, int W_n) {
        row_vector[n] phit_D; // phi' * D
        row_vector[n] phit_W; // phi' * W
        vector[n] ldet_terms;
        
        phit_D = (phi .* D_sparse)';
        phit_W = rep_row_vector(0, n);
        for (i in 1:W_n) {
            phit_W[W_sparse[i, 1]] = phit_W[W_sparse[i, 1]] + 
                                            phi[W_sparse[i, 2]];
            phit_W[W_sparse[i, 2]] = phit_W[W_sparse[i, 2]] + 
                                            phi[W_sparse[i, 1]];
        }
    return 0.5 * ((n - 1) * log(tau) - tau * (phit_D * phi - (phit_W * phi)));
    }
}


data {
    int<lower = 1> m;           // Number of obs (n_area * n_race)
    int<lower = 1> n;           // Number of counties
    int<lower = 1> s;           // Number of states
    int<lower = 0> y[m];        // Observed values
    real log_offset[m];         // log(Expected values)
    int<lower = 1> c_idx[m];    // County index: e.g., rep(1:n, 2)
    int<lower = 1> s_idx[m];    // State index
    
    int<lower = 0, upper = 1> d1_idx[m];    // race1 id: e.g., rep(0:1, each=n)
    int<lower = 0, upper = 1> d2_idx[m];    // race2 id: e.g., rep(1:0, each=n)
    
    real income[m];              // median household income for total pop
    
    // Use helper function `return_sparse_parts()` in R for these -- 
    // they are for sparse representation of CAR component. For large networks
    // we calculate everything in R and then pass to Stan as data so we aren't
    // transforming data and calculating eigenvalues for every compilation and
    // chain. 
    // TODO: Write a version of the model that will do transformations in Stan.
    int W_n;                    // Number of edges
    int W_sparse[W_n, 2];       // Edge list (adjacency pairs)
    vector[n] D_sparse;         // Number of neigbors for each node
    vector[n] lambda;           // Eigenvalues of D^(-.5) * A * D^(-.5)
}


parameters {
    vector[2] alpha;                // Coeffs on intercepts
    vector[2] beta;                 // Coeffs on principal component(s)
    vector[n] v_unstr[3];           // Unstructured spatial components
    vector[n] u_str_unscaled[3];    // Uncentered structured spatial part
    vector[s] nu;                   // Unstructured state effect
    real<lower = 0> sigma_u[3];     // SD of CAR part
    real<lower = 0> sigma_v[3];     // SD of iid spatial part
    real<lower = 0> sigma_s;        // SD of state random effect
    real<lower = 0> delta;          // Scaling of shared component
}


transformed parameters {
    vector[n] u_str[3];             // Centered version of u
    vector[n] psi[2];               // Race-specific spatial components (u + v)
    vector[n] phi;                  // Shared spatial component (u + v)
    real<lower = 0> tau_u[3];       // precision of CAR part
    real<lower = 0> tau_v[3];       // precision of iid spatial part

    // Convert SD to precision for comparison to old models
    tau_u = inv_square(sigma_u);
    tau_v = inv_square(sigma_v);
    
    // Brute-force centering -- as far as I can tell, there's no way to do this
    // outside of a for().
    for (i in 1:3) {
        u_str[i] = u_str_unscaled[i] - mean(u_str_unscaled[i]);
    }
    
    // Only sum of {u, v} identifiable. 
    // Calculate here to get more accurate ESS estimates. (I.e., (u_i + v_i)
    // may be more or less correlated than u_i and v_i individually. Also, 
    // cuts storage requirement since we only need to save one vector.
    for (i in 1:n) {
        psi[1][i] = (u_str[1][i] + v_unstr[1][i]);
        psi[2][i] = (u_str[2][i] + v_unstr[2][i]);
        phi[i]    = (u_str[3][i] + v_unstr[3][i]);
    }
}


model {
    // Container so we can vectorize the poisson_log() call. Never use `~` 
    // inside of a for(). *Super* slow in Stan.
    real log_mu[m];
    
    // No prior on alphas indicates improper flat. (Must use flat on BYM).
    beta ~ normal(0, 10);
    
    // Spatial priors -- u for CAR, v for iid.
    for (i in 1:3) {
        u_str_unscaled[i] ~ sparse_iar(tau_u[i], W_sparse, D_sparse, 
                                       lambda, n, W_n);
        // Normal distribution is parameterized as N(mean, sd) in Stan, unlike
        // WinBUGS which uses N(mean, precision). Thus, convert to sd.
        v_unstr[i] ~ normal(0, sigma_v[i]);  
    }

    // State random effects
    nu ~ normal(0, sigma_s);

    // Hyperpriors for spatial components
    // Note: tau_v ends up getting transformed into standard deviation -- 
    //       at some point, may be worth looking into drawing from invgamma.
    // tau_u ~ gamma(.1, .1);      // winbugs: g(.5, .0005) / carlin g(.1, .1)
    // tau_v ~ gamma(.001, .001);  // winbugs: g(.5, .0005) / carlin g(.001, .001)
    sigma_u ~ normal(0, 5);
    sigma_v ~ normal(0, 5);
    sigma_s ~ normal(0, 5);
    delta   ~ lognormal(0, .4117);

    // Likelihood
    for (i in 1:m) {
        log_mu[i] = log_offset[i] + 
                    alpha[1]  * d1_idx[i] +                 // white average
                    alpha[2]  * d2_idx[i] +                 // black average 
                    beta[1]   * income[i] +                    
                    beta[2]   * income[i] * d2_idx[i] + 
                    d1_idx[i] * (phi[c_idx[i]] * delta + psi[1][c_idx[i]]) +  
                    d2_idx[i] * (phi[c_idx[i]] / delta + psi[2][c_idx[i]]) + 
                    nu[s_idx[i]];
    }
    
    y ~ poisson_log(log_mu);
}
