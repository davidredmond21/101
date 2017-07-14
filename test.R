ere X and X_g are a design matrices appled to the covariate and grid values respectively

# Create the design matrices
X = cbind(1, x)
X_g = cbind(1, x_g)
pred  = X_g %*% solve(t(X)%*%X, t(X)%*%y)
plot(x, y)
lines(x_g, pred) # not a very good fit
