##How do I decide what span to use in LOESS regression in R?
###1.Set up a toy data frame.

set.seed(4)
x <- rnorm(n = 500)
y <- (x)^3 + (x - 3)^2 + (x - 8) - 1 + rnorm(n = 500, sd = 0.5)
plot(x, y)
df <- data.frame(x, y)

###2.Set up useful variables to handle cross-validation loop.

span.seq <- seq(from = 0.15, to = 0.95, by = 0.05) #explores range of spans
k <- 10 #number of folds
set.seed(1) # replicate results
folds <- sample(x = 1:k, size = length(x), replace = TRUE)
cv.error.mtrx <- matrix(rep(x = NA, times = k * length(span.seq)), 
                        nrow = length(span.seq), ncol = k)

### 3.Run a nested for loop iterating over each span possibility in span.seq, 
###and each fold in folds.
for(i in 1:length(span.seq)) {
  for(j in 1:k) {
    loess.fit <- loess(formula = y ~ x, data = df[folds != j, ], span = span.seq[i])
    preds <- predict(object = loess.fit, newdata = df[folds == j, ])
    cv.error.mtrx[i, j] <- mean((df$y[folds == j] - preds)^2, na.rm = TRUE)
    # some predictions result in `NA` because of the `x` ranges in each fold
  }
}

####4.Calculate average cross-validation mean square error from each of the 10 folds:
###CV(10)=(1/10)∑_{1}^{10}MSEi
cv.errors <- rowMeans(cv.error.mtrx)

##5.Find which span resulted in the lowest MSE
best.span.i <- which.min(cv.errors)
best.span.i
span.seq[best.span.i]

###6. Plot your results.
plot(x = span.seq, y = cv.errors, type = "l", main = "CV Plot")
points(x = span.seq, y = cv.errors, 
       pch = 20, cex = 0.75, col = "blue")
points(x = span.seq[best.span.i], y = cv.errors[best.span.i], 
       pch = 20, cex = 1, col = "red")

best.loess.fit <- loess(formula = y ~ x, data = df, 
                        span = span.seq[best.span.i])

x.seq <- seq(from = min(x), to = max(x), length = 100)

plot(x = df$x, y = df$y, main = "Best Span Plot")
lines(x = x.seq, y = predict(object = best.loess.fit, 
                             newdata = data.frame(x = xp2.seq)), 
      col = "red", lwd = 2)