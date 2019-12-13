if (interactive()) {
options(device.ask.default = FALSE)

if (!require(shiny))
    install.packages("shiny", repos="https://cloud.r-project.org/")
if (!require(shinydashboard))
    install.packages("shinydashboard", repos="https://cloud.r-project.org/")
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Data Cloning Apps"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Distributions", tabName = "distributions"),
      menuItem("MLE", tabName = "mle"),
      menuItem("Beta prior", tabName = "betaprior"),
      menuItem("Normal prior", tabName = "normalprior"),
      menuItem("Bimodal prior", tabName = "bimodalprior"),
      menuItem("Data cloning", tabName = "datacloning")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "distributions",
        fluidRow(
          box(title="Histogram", width=8,
            plotOutput("distPlot")
          ),
          box(title="Inputs", width=4,
            selectInput("distr", "Distribution",
                  choices=c(Bernoulli = "Bernoulli",
                Binomial = "Binomial",
                Poisson = "Poisson",
                Normal = "Normal",
                Lognormal = "Lognormal",
                Uniform = "Uniform",
                Beta = "Beta",
                Gamma = "Gamma")),
            hr(),
              sliderInput("n", label = "Sample size",
                          min = 10, max = 1000, value = 100, step = 10),
              sliderInput("seed", label = "Random seed",
                          min = 0, max = 100, value = 0, step = 10),
              ## Bernoulli
              conditionalPanel(
                condition = "input.distr == 'Bernoulli'",
                  sliderInput("p", label = "Probability",
                          min = 0, max = 1, value = 0.3, step = 0.05)),
              ## Binomial
              conditionalPanel(
                condition = "input.distr == 'Binomial'",
                  sliderInput("pb", label = "Probability",
                          min = 0, max = 1, value = 0.3, step = 0.05),
                  sliderInput("size", label = "Size",
                          min = 1, max = 1000, value = 10, step = 50)),
              ## Poisson
              conditionalPanel(
                condition = "input.distr == 'Poisson'",
                  sliderInput("lambda", label = "Mean/Rate",
                          min = 0, max = 100, value = 5, step = 5)),
              ## Normal
              conditionalPanel(
                condition = "input.distr == 'Normal'",
                  sliderInput("mu", label = "Mean",
                          min = -10, max = 10, value = 0, step = 1),
                  sliderInput("var", label = "Variance",
                          min = 0.001, max = 10, value = 1, step = 0.5)),
              ## Logormal
              conditionalPanel(
                condition = "input.distr == 'Lognormal'",
                  sliderInput("mul", label = "Mean",
                          min = -10, max = 10, value = -1, step = 1),
                  sliderInput("varl", label = "Variance",
                          min = -10, max = 10, value = 1, step = 1)),
              ## Uniform
              conditionalPanel(
                condition = "input.distr == 'Uniform'",
                  sliderInput("a", label = "Minimum",
                          min = -10, max = 10, value = -1, step = 0.5),
                  sliderInput("b", label = "Maximum",
                          min = -10, max = 10, value = 1, step = 0.5)),
              ## Beta
              conditionalPanel(
                condition = "input.distr == 'Beta'",
                  sliderInput("shape1", label = "Shape 2",
                          min = 0, max = 10, value = 1, step = 0.5),
                  sliderInput("shape2", label = "Shape 1",
                          min = 0, max = 10, value = 1, step = 0.5)),
              ## Gamma
              conditionalPanel(
                condition = "input.distr == 'Gamma'",
                  sliderInput("shape", label = "Shape",
                          min = 0.001, max = 10, value = 1, step = 0.5),
                  sliderInput("rate", label = "Rate",
                          min = 0.001, max = 10, value = 1, step = 0.5))
            )
          )
        ),
      tabItem(tabName = "mle",
        fluidRow(
            box(title="Density", width=8,
                plotOutput("mlePlot")),
            box(title="Inputs", width=4,
                sliderInput("p_mle", label = "Probability (true)",
                    min = 0, max = 1, value = 0.3, step = 0.05),
                sliderInput("n_mle", label = "Sample size",
                    min = 10, max = 1000, value = 10, step = 10),
                sliderInput("seed_mle", label = "Random seed",
                    min = 0, max = 100, value = 0, step = 10)
                )
        )
      ),
      tabItem(tabName = "betaprior",
        fluidRow(
            box(title="Density", width=8,
                plotOutput("betaPlot")),
            box(title="Inputs", width=4,
              sliderInput("p_beta", label = "Probability (true)",
                          min = 0, max = 1, value = 0.3, step = 0.05),
              sliderInput("n_beta", label = "Sample size",
                          min = 1, max = 1000, value = 10, step = 10),
              sliderInput("a_beta", label = "Beta prior shape parameter a",
                          min = 0, max = 3, value = 1, step = 0.1),
              sliderInput("b_beta", label = "Beta prior shape parameter b",
                          min = 0, max = 3, value = 1, step = 0.1),
              radioButtons("scale_beta", label="Scale",
                         c("Probability (0, 1)" = "prob",
                           "Logit (-Inf, Inf)" = "logit")),
              sliderInput("seed_beta", label = "Random seed",
                          min = 0, max = 100, value = 0, step = 10)
                )
        )
      ),
      tabItem(tabName = "normalprior",
        fluidRow(
            box(title="Density", width=8,
                plotOutput("normPlot")),
            box(title="Inputs", width=4,
                sliderInput("p_norm", label = "Probability (true)",
                    min = 0, max = 1, value = 0.3, step = 0.05),
                sliderInput("n_norm", label = "Sample size",
                    min = 1, max = 1000, value = 10, step = 10),
                sliderInput("mu_norm", label = "Normal prior mean",
                    min = -10, max = 10, value = 0, step = 1),
                sliderInput("sig2_norm", label = "Normal prior variance",
                    min = 0.001, max = 100, value = 1, step = 10),
                radioButtons("scale_norm", label="Scale",
                    c("Probability (0, 1)" = "prob",
               "Logit (-Inf, Inf)" = "logit")),
                sliderInput("seed_norm", label = "Random seed",
                    min = 0, max = 100, value = 0, step = 10)
                )
        )
      ),
      tabItem(tabName = "bimodalprior",
        fluidRow(
            box(title="Density", width=8,
                plotOutput("bimodPlot")),
            box(title="Inputs", width=4,
                sliderInput("p_bimod", label = "Probability (true)",
                  min = 0, max = 1, value = 0.3, step = 0.05),
                sliderInput("n_bimod", label = "Sample size",
                  min = 0, max = 1000, value = 10, step = 10),
                sliderInput("mu_1_bimod", label = "Normal prior mean",
                  min = -10, max = 10, value = -2, step = 1),
                sliderInput("sig2_1_bimod", label = "Normal prior variance",
                  min = 0.001, max = 10, value = 1, step = 1),
                sliderInput("mu_2_bimod", label = "Normal prior mean",
                  min = -10, max = 10, value = 2, step = 1),
                sliderInput("sig2_2_bimod", label = "Normal prior variance",
                  min = 0.001, max = 10, value = 2, step = 1),
                radioButtons("scale_bimod", label="Scale",
                 c("Probability (0, 1)" = "prob",
                   "Logit (-Inf, Inf)" = "logit"),
                 selected = "logit"),
                sliderInput("seed_bimod", label = "Random seed",
                  min = 0, max = 100, value = 0, step = 10)
                )
        )
      ),
      tabItem(tabName = "datacloning",
        fluidRow(
            box(title="Density", width=8,
                plotOutput("dcPlot")),
            box(title="Inputs", width=4,
                sliderInput("p_dc", label = "Probability (true)",
                  min = 0, max = 1, value = 0.3, step = 0.05),
                sliderInput("n_dc", label = "Sample size",
                  min = 1, max = 50, value = 10, step = 5),
                sliderInput("a_dc", label = "Beta prior shape parameter a",
                  min = 0, max = 2, value = 1, step = 0.1),
                sliderInput("b_dc", label = "Beta prior shape parameter b",
                  min = 0, max = 2, value = 1, step = 0.1),
                sliderInput("K_dc", label = "Number of clones",
                  min = 1, max = 100, value = 1, step = 10),
                radioButtons("scale_dc", label="Scale",
                 c("Probability (0, 1)" = "prob",
                   "Logit (-Inf, Inf)" = "logit")),
                sliderInput("seed_dc", label = "Random seed",
                  min = 0, max = 100, value = 0, step = 10)
                )
        )
      )

    )
  )
)

server <- function(input, output) {
    output$distPlot <- renderPlot({
        par(las = 1)
        set.seed(input$seed)
        if (input$distr == "Uniform" && input$b < input$a)
            stop("Maximum must be greater than Minimum")
        y <- switch(input$distr,
            "Bernoulli" = rbinom(1000, 1, input$p),
            "Binomial" = rbinom(1000, input$size, input$pb),
            "Poisson" = rpois(1000, input$lambda),
            "Normal" = rnorm(1000, input$mu, sqrt(input$var)),
            "Lognormal" = rlnorm(1000, input$mul, sqrt(input$varl)),
            "Uniform" = runif(1000, input$a, input$b),
            "Beta" = rbeta(1000, input$shape1, input$shape2),
            "Gamma" = rgamma(1000, input$shape, input$rate))
        yy <- y[1:input$n]
        x <- switch(input$distr,
            "Bernoulli" = c(0,1),
            "Binomial" = seq(0, max(yy)+1, by = 1),
            "Poisson" = seq(0, max(yy)+1, by = 1),
            "Normal" = seq(min(yy)-1, max(yy)+1, length.out = 1000),
            "Lognormal" = seq(0.0001, max(yy)+1, length.out = 1000),
            "Uniform" = seq(input$a+0.0001, input$b-0.0001, length.out = 1000),
            "Beta" = seq(0.0001, 0.9999, length.out = 1000),
            "Gamma" = seq(0.0001, max(yy), length.out = 1000))
        d <- switch(input$distr,
            "Bernoulli" = dbinom(x, 1, input$p),
            "Binomial" = dbinom(x, input$size, input$pb),
            "Poisson" = dpois(x, input$lambda),
            "Normal" = dnorm(x, input$mu, sqrt(input$var)),
            "Lognormal" = dlnorm(x, input$mul, sqrt(input$varl)),
            "Uniform" = dunif(x, input$a, input$b),
            "Beta" = dbeta(x, input$shape1, input$shape2),
            "Gamma" = dgamma(x, input$shape, input$rate))
        xlab <- "x"
        ylab <- "Density"
        main <- paste0(input$distr, " distribution (n = ", input$n, ")")
        if (input$distr %in% c("Bernoulli", "Binomial", "Poisson")) {
            tmp <- table(yy) / input$n
            plot(tmp, ylim=c(0, max(tmp, d)),
                ylab = ylab, xlab = xlab, main = main,
                col = "#cccccc", lwd = 10)
            points(x, d, pch = 21, col = "#c7254e", type = "b",
                lty = 2, cex = 2)
        } else {
            tmp <- hist(yy, plot = FALSE)
            hist(yy, freq = FALSE, ylim=c(0, max(tmp$density, d)),
                ylab = ylab, xlab = xlab, main = main,
                col = "#ecf0f1", border = "#cccccc")
            lines(x, d, lwd = 2, col = "#c7254e")
        }
    })

    output$mlePlot <- renderPlot({
        par(las = 1)
        set.seed(input$seed_mle)
        y <- rbinom(n = 1000, size = 1, p = input$p_mle)
        pt <- seq(0, 1, by = 0.0005)
        L <- sapply(pt, function(z)
            prod(dbinom(y[1:input$n_mle], size = 1, prob = z)))
        plot(pt, L, type = "l", col="#3498db",
            ylab = "Likelihood", xlab="p",
            sub=paste0("Mean = ", round(mean(y[1:input$n_mle]), 2), " (",
                sum(1-y[1:input$n_mle]), " 0s & ", sum(y[1:input$n_mle]), " 1s)"),
            main = paste("Estimate =", round(pt[which.max(L)], 2)))
        abline(v = input$p_mle, lwd = 2, col = "#c7254e")
        abline(v = pt[which.max(L)], lwd = 2, col = "#18bc9c")
    })

    output$betaPlot <- renderPlot({
        par(las = 1)
        set.seed(input$seed_beta)
        y <- rbinom(n = 1000, size = 1, p = input$p_beta)
        BY <- 0.0005
        pval <- seq(0.001, 0.999, by = BY)
        fLik <- function(p, y)
            prod(dbinom(y, size = 1, prob = p))
        Lik <- sapply(pval, fLik, y=y[1:input$n_beta])
        fPri <- function(p, shape1=0.5, shape2=0.5)
            dbeta(p, shape1, shape2)
        Pri <- sapply(pval, fPri, input$a_beta, input$b_beta)
        if (input$scale_beta == "prob") {
            p <- input$p_beta
        } else {
            p <- qlogis(input$p_beta)
            br <- c(0.001, seq(0.001+BY/2, 0.999-BY/2, by = BY), 0.999)
            dx <- diff(pval)
            dx <- c(dx[1], dx)
            d <- Pri * dx / diff(qlogis(br))
            Pri <- smooth.spline(pval, d)$y
            pval <- qlogis(pval)
        }
        Pos <- Lik * Pri
        M <- cbind(Pri=Pri/max(Pri),
            Lik=Lik/max(Lik),
            Pos=Pos/max(Pos))
        Col <- c("#cccccc", "#3498db", "#f39c12")
        matplot(pval, M, type = "l",
            col=Col, lwd=2, lty=1,
            ylab = "Density",
            xlab=ifelse(input$scale_beta == "logit", "logit(p)","p"),
            sub=paste0("Mean = ", round(mean(y[1:input$n_beta]), 2), " (",
                sum(1-y[1:input$n_beta]), " 0s & ", sum(y[1:input$n_beta]), " 1s)"),
            main = paste0("True value = ", round(p, 2),
                ", Posterior mode = ", round(pval[which.max(Pos)], 2)))
        abline(v = p, lwd = 2, col = "#c7254e")
        abline(v = pval[which.max(Pos)], lwd = 2, col = "#18bc9c")
        legend("topleft",lty=1, lwd=2, col=Col, bty="n",
            legend=c("Prior","Likelihood","Posterior"))
    })

    output$normPlot <- renderPlot({
        par(las = 1)
        set.seed(input$seed_norm)
        y <- rbinom(n = 1000, size = 1, p = input$p_norm)
        pval <- seq(0.001, 0.999, by = 0.0005)
        fLik <- function(p, y)
            prod(dbinom(y, size = 1, prob = p))
        fPri <- function(p, mu, sig2, scale) {
            if (scale == "prob")
                out <- (1/(p*(1-p))) * dnorm(qlogis(p), mu, sqrt(sig2))
            if (scale == "logit")
                out <- dnorm(qlogis(p), mu, sqrt(sig2))
            out
        }
        Lik <- sapply(pval, fLik, y=y[1:input$n_norm])
        Pri <- sapply(pval, fPri, input$mu_norm, input$sig2_norm, input$scale_norm)
        Pos <- Lik * Pri
        M <- cbind(Pri=Pri/max(Pri),
            Lik=Lik/max(Lik),
            Pos=Pos/max(Pos))
        if (input$scale_norm == "logit") {
            p <- qlogis(input$p_norm)
            pval <- qlogis(pval)
        } else {
            p <- input$p_norm
        }
        Col <- c("#cccccc", "#3498db", "#f39c12")
        matplot(pval, M, type = "l",
            col=Col, lwd=2, lty=1,
            ylab = "Density",
            xlab=ifelse(input$scale_norm == "logit", "logit(p)","p"),
            sub=paste0("Mean = ", round(mean(y[1:input$n_norm]), 2), " (",
                sum(1-y[1:input$n_norm]), " 0s & ", sum(y[1:input$n_norm]), " 1s)"),
            main = paste0("True value = ", round(p, 2),
                ", Posterior mode = ", round(pval[which.max(Pos)], 2)))
        abline(v = p, lwd = 2, col = "#c7254e")
        abline(v = pval[which.max(Pos)], lwd = 2, col = "#18bc9c")
        legend("topleft",lty=1, lwd=2, col=Col, bty="n",
               legend=c("Prior","Likelihood","Posterior"))
    })

    output$bimodPlot <- renderPlot({
        par(las = 1)
        set.seed(input$seed_bimod)
        y <- rbinom(n = 1000, size = 1, p = input$p_bimod)
        BY <- 0.0005
        pval <- seq(0.001, 0.999, by = BY)
        fLik <- function(p, y)
            prod(dbinom(y, size = 1, prob = p))
        Lik <- sapply(pval, fLik, y=y[1:input$n_bimod])

        fPri <- function(p, mu_1, sig2_1, mu_2, sig2_2)
            0.5 * (dnorm(qlogis(p), mu_1, sqrt(sig2_1)) +
                dnorm(qlogis(p), mu_2, sqrt(sig2_2)))
        Pri <- sapply(pval, fPri, input$mu_1_bimod, input$sig2_1_bimod,
            input$mu_2_bimod, input$sig2_2_bimod)
        if (input$scale_bimod == "prob") {
            p <- input$p_bimod
            br <- qlogis(c(0.001, seq(0.001+BY/2, 0.999-BY/2, by = BY), 0.999))
            dx <- diff(qlogis(pval))
            dx <- c(dx[1], dx)
            d <- Pri * dx / diff(plogis(br))
            Pri <- smooth.spline(pval, d)$y
        } else {
            pval <- qlogis(pval)
            p <- qlogis(input$p_bimod)
        }

        Pos <- Lik * Pri
        M <- cbind(Pri=Pri/max(Pri),
            Lik=Lik/max(Lik),
            Pos=Pos/max(Pos))
        Col <- c("#cccccc", "#3498db", "#f39c12")
        matplot(pval, M, type = "l",
            col=Col, lwd=2, lty=1,
            ylab = "Density",
            xlab=ifelse(input$scale_bimod == "logit", "logit(p)","p"),
            sub=paste0("Mean = ", round(mean(y[1:input$n_bimod]), 2), " (",
                sum(1-y[1:input$n_bimod]), " 0s & ", sum(y[1:input$n_bimod]), " 1s)"),
            main = paste0("True value = ", round(p, 2),
                ", Posterior mode = ", round(pval[which.max(Pos)], 2)))
        abline(v = p, lwd = 2, col = "#c7254e")
        abline(v = pval[which.max(Pos)], lwd = 2, col = "#18bc9c")
        legend("topleft",lty=1, lwd=2, col=Col, bty="n",
               legend=c("Prior","Likelihood","Posterior"))
    })

    output$dcPlot <- renderPlot({
        par(las = 1)
        set.seed(input$seed_dc)
        y <- rbinom(n = input$n_dc, size = 1, p = input$p_dc)
        yk <- rep(y, input$K_dc)
        BY <- 0.0005
        pval <- seq(0.001, 0.999, by = BY)
        fLik <- function(p, y)
            sum(dbinom(y, size = 1, prob = p, log=TRUE))
        Lik <- exp(sapply(pval, fLik, y=yk))
        if (all(Lik <= 0)) {
            est <- optimize(fLik, c(0.001, 0.999), y=yk, maximum=TRUE)$maximum
            Lik[which.min(abs(pval - est))] <- 1
        }
        if (input$scale_dc == "prob") {
            p <- input$p_dc
            fPri <- function(p, shape1=0.5, shape2=0.5)
                dbeta(p, shape1, shape2)
            Pri <- sapply(pval, fPri, input$a_dc, input$b_dc)
        } else {
            p <- qlogis(input$p_dc)
            N <- 10^5
            x <- rbeta(N, input$a_dc, input$b_dc)
            br <- c(0.001, seq(0.001+BY/2, 0.999-BY/2, by = BY), 0.999)
            d <- as.numeric(table(cut(x, breaks=br))) / N
            pval <- qlogis(pval)
            g <- diff(qlogis(br))
            gy <-  d / g
            Pri <- smooth.spline(pval, gy)$y
        }
        Pos <- Lik * Pri
        M <- cbind(Pri=Pri/max(Pri),
            Lik=Lik/max(Lik),
            Pos=Pos/max(Pos))
        Col <- c("#cccccc", "#3498db", "#f39c12")
        matplot(pval, M, type = "l",
            col=Col, lwd=2, lty=1,
            ylab = "Density", xlab="p",
            sub=paste0("Mean = ", round(mean(y[1:input$n_dc]), 2), " (",
                sum(1-y[1:input$n_dc]), " 0s & ", sum(y[1:input$n_dc]), " 1s)"),
            main = paste0("True value = ", round(p, 2),
                ", Posterior mode = ", round(pval[which.max(Pos)], 2)))
        abline(v = p, lwd = 2, col = "#c7254e")
        abline(v = pval[which.max(Pos)], lwd = 2, col = "#18bc9c")
        legend("topleft",lty=1, lwd=2, col=Col, bty="n",
               legend=c("Prior","Likelihood","Posterior"))
    })

}

shinyApp(ui, server)
}
