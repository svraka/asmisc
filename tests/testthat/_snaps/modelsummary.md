# helper

    Code
      modelsummary::modelsummary(ms)
    Output
      
      +-------------+---------+
      |             | (1)     |
      +=============+=========+
      | (Intercept) | 37.285  |
      +-------------+---------+
      |             | (1.878) |
      +-------------+---------+
      | wt          | -5.344  |
      +-------------+---------+
      |             | (0.559) |
      +-------------+---------+
      | Num.Obs.    | 32      |
      +-------------+---------+
      | R2          | 0.753   |
      +-------------+---------+
      | R2 Adj.     | 0.745   |
      +-------------+---------+
      | R2 Within   |         |
      +-------------+---------+
      | R2 Pseudo   |         |
      +-------------+---------+
      | AIC         | 164.0   |
      +-------------+---------+
      | BIC         | 167.0   |
      +-------------+---------+
      | Log.Lik.    | -80.015 |
      +-------------+---------+ 

# default method

    Code
      modelsummary::modelsummary(ms)
    Output
      
      +-------------+---------+
      |             | (1)     |
      +=============+=========+
      | (Intercept) | 37.285  |
      +-------------+---------+
      |             | (1.878) |
      +-------------+---------+
      | wt          | -5.344  |
      +-------------+---------+
      |             | (0.559) |
      +-------------+---------+
      | Num.Obs.    | 32      |
      +-------------+---------+
      | R2          | 0.753   |
      +-------------+---------+
      | R2 Adj.     | 0.745   |
      +-------------+---------+
      | AIC         | 166.0   |
      +-------------+---------+
      | BIC         | 170.4   |
      +-------------+---------+
      | Log.Lik.    | -80.015 |
      +-------------+---------+ 

# fixest method

    Code
      modelsummary::modelsummary(ms)
    Output
      
      +-------------+---------+
      |             | (1)     |
      +=============+=========+
      | (Intercept) | 37.285  |
      +-------------+---------+
      |             | (1.878) |
      +-------------+---------+
      | wt          | -5.344  |
      +-------------+---------+
      |             | (0.559) |
      +-------------+---------+
      | Num.Obs.    | 32      |
      +-------------+---------+
      | R2          | 0.753   |
      +-------------+---------+
      | R2 Adj.     | 0.745   |
      +-------------+---------+
      | AIC         | 164.0   |
      +-------------+---------+
      | BIC         | 167.0   |
      +-------------+---------+
      | Log.Lik.    | -80.015 |
      +-------------+---------+
      | Std.Errors  | IID     |
      +-------------+---------+
      | Family      | OLS     |
      +-------------+---------+ 

