ea1 <-
function (data) 
{
    u <- list("", f1 = function(data) {
        names(data) = c("treatments", "response")
        data <- data.frame(treatments = factor(data$treatments), 
            response = data$response)
        m <- aov(response ~ treatments, data = data, contrasts = list(treatments = contr.sum))
        m1 <- aov(response ~ -1 + treatments, data = data, contrasts = list(treatments = contr.sum))
        a <- anova(m)
        r <- resid(m)
        s <- shapiro.test(r)
        b <- bartlett.test(response ~ treatments, na.action = na.omit, 
            data = data)
        cv <- function(x) {
            sd = (deviance(x)/df.residual(x))^0.5
            mm = mean(fitted(x))
            r = 100 * sd/mm
            return(round(r, 2))
        }
        cvf = cv(m)
        Adjust.Means <- coef(m1)
        Standart.Error <- sqrt(diag(vcov(m1)))
        Treatments <- levels(data$treatments)
        ma = data.frame(Treatments, Adjust.Means, Standart.Error)
        rownames(ma) = NULL
        test1 <- summary(glht(m, linfct = mcp(treatments = "Tukey")))
        cld(test1)
        test <- list(test1, cld(test1))
        l <- list(a, s, b, cvf, ma, test)
        names(l) = list("Analysis of variance", "Normality test ", 
            "Homogeneity of variances", "Coefficient of variation (%)", 
            "Means", "Multiple comparison test")
        return(l)
    }, f2 <- function(data) {
        names(data) = c("treatments", "blocks", "response")
        data <- data.frame(treatments = factor(data$treatments), 
            blocks = factor(data$blocks), response = data$response)
        m <- aov(response ~ treatments + blocks, data = data, 
            contrasts = list(treatments = contr.sum, blocks = contr.sum))
        m1 <- aov(response ~ -1 + treatments + blocks, data = data, 
            contrasts = list(treatments = contr.sum, blocks = contr.sum))
        a <- anova(m)
        residuals <- resid(m)
        s <- shapiro.test(residuals)
        data2 <- na.omit(data)
        b <- bartlett.test(residuals ~ treatments, data = data2)
        cv <- function(x) {
            sd = (deviance(x)/df.residual(x))^0.5
            mm = mean(fitted(x))
            r = 100 * sd/mm
            return(round(r, 2))
        }
        cvf = cv(m)
        a2 <- Anova(m, type = 3)
        a3 <- a2[-1, ]
        Adjust.Means <- coef(m1)[c(1:nlevels(data$treatments))]
        Standart.Error <- sqrt(diag(vcov(m1)))
        Standart.Error <- Standart.Error[1:nlevels(data$treatments)]
        Treatments <- levels(data$treatments)
        ma = data.frame(Treatments, Adjust.Means, Standart.Error)
        rownames(ma) = NULL
        test1 <- summary(glht(m, linfct = mcp(treatments = "Tukey")))
        cld(test1)
        test <- list(test1, cld(test1))
        l <- list(a3, s, b, cvf, ma, test)
        names(l) = list("Analysis of variance", "Normality test ", 
            "Homogeneity of variances", "Coefficient of variation (%)", 
            "Adjusted means", "Multiple comparison test")
        return(l)
    }, f3 <- function(data) {
        names(data) = c("treatments", "rows", "cols", "response")
        data <- data.frame(treatments = factor(data$treatments), 
            rows = factor(data$rows), cols = factor(data$cols), 
            response = data$response)
        m <- aov(response ~ treatments + rows + cols, data = data, 
            contrasts = list(treatments = contr.sum, rows = contr.sum, 
                cols = contr.sum))
  m1 <- aov(response ~ -1 + treatments + rows + cols, data = data, 
            contrasts = list(treatments = contr.sum, rows = contr.sum, 
                cols = contr.sum))
        a <- anova(m)
        residuals <- resid(m)
        s <- shapiro.test(residuals)
        data2 <- na.omit(data)
        b <- bartlett.test(residuals ~ treatments, data = data2)
        cv <- function(x) {
            sd = (deviance(x)/df.residual(x))^0.5
            mm = mean(fitted(x))
            r = 100 * sd/mm
            return(round(r, 2))
        }
        cvf = cv(m)
        a2 <- Anova(m, type = 3)
        a3 <- a2[-1, ]
        Adjust.Means <- coef(m1)[c(1:nlevels(data$treatments))]
        Standart.Error <- sqrt(diag(vcov(m1)))
        Standart.Error <- Standart.Error[1:nlevels(data$treatments)]
        Treatments <- levels(data$treatments)
        ma = data.frame(Treatments, Adjust.Means, Standart.Error)
        rownames(ma) = NULL
        test1 <- summary(glht(m, linfct = mcp(treatments = "Tukey")))
        cld(test1)
        test <- list(test1, cld(test1))
        l <- list(a3, s, b, cvf, ma, test)
        names(l) = list("Analysis of variance", "Normality test ", 
            "Homogeneity of variances", "Coefficient of variation (%)", 
            "Adjusted means", "Multiple comparison test")
        return(l)
    }, f4 = function(data) {
        names(data) = c("treatments", "squares", "rows", "cols", 
            "response")
        data <- data.frame(treatments = factor(data$treatments), 
            squares = factor(data$squares), rows = factor(data$rows), 
            cols = factor(data$cols), response = data$response)
        m <- aov(response ~ treatments + squares + rows + cols, 
            data = data, contrasts = list(treatments = contr.sum, 
                squares = contr.sum, rows = contr.sum, cols = contr.sum))
        m1 <- aov(response ~ -1 + treatments + squares + rows + 
            cols, data = data, contrasts = list(treatments = contr.sum, 
            squares = contr.sum, rows = contr.sum, cols = contr.sum))
        a <- anova(m)
        residuals <- resid(m)
        s <- shapiro.test(residuals)
        data2 <- na.omit(data)
        b <- bartlett.test(residuals ~ treatments, data = data2)
        cv <- function(x) {
            sd = (deviance(x)/df.residual(x))^0.5
            mm = mean(fitted(x))
            r = 100 * sd/mm
            return(round(r, 2))
        }
cvf = cv(m)
        Adjust.Means <- coef(m1)[1:nlevels(data$treatments)]
        Standart.Error <- sqrt(diag(vcov(m1))[1:nlevels(data$treatments)])
        Treatments <- levels(data$treatments)
        maf1 = data.frame(Treatments, Adjust.Means, Standart.Error)
        rownames(maf1) = NULL
        kp <- function(expr) {
            localWarnings <- list()
            value <- withCallingHandlers(expr, warning = function(w) {
                localWarnings[[length(localWarnings) + 1]] <<- w
                invokeRestart("muffleWarning")
            })
            value = value
        }
        test1 <- summary(kp(glht(m, linfct = mcp(treatments = "Tukey"))))
        cld(test1)
        test1 <- list(test1, cld(test1))
        l <- list(a, s, b, cvf, maf1, test1)
        names(l) = list("Analysis of variance", "Normality test ", 
            "Homogeneity of variances", "Coefficient of variation (%)", 
            "Adjusted means", "Multiple comparison test")
        return(l)
    })
    g = u[[length(data)]]
    g(data)
  }
