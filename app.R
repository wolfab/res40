# Load packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(kableExtra)

# Load data
dat <- readRDS(file="~/R/res40/data/r.RDat")
dat <- arrange(dat, lob, type, ay, dev)

prm <- readRDS(file="~/R/res40/data/rprm.RDat")

# Add incremental diff and devf columns
dat <- dat %>% group_by(lob, type, ay) %>%
  mutate(diff = value - lag(value)) %>%
  mutate(diff = if_else(is.na(diff), value, diff)) %>%
  ungroup() %>%
  gather(cumul, value, -(lob:dev)) %>%
  mutate(cumul = if_else(cumul == "value", TRUE, FALSE))

dat <- left_join(dat, prm, by = c("lob","ay")) %>% mutate(lr = value / prm)

fdev <- function(dat){
  df <- group_by(dat, ay) %>%
    mutate(fac = value / lag(value)) %>%
    ungroup %>% na.omit %>% filter(is.finite(fac))

  res <- df %>%
    group_by(dev) %>%
    summarise(mean = mean(fac, trim = 0),
              meantrim = mean(fac, trim = 1/n()),
#              meantrim2 = mean(fac, trim = 0.2),
              q75 = quantile(fac, 0.75),
              q90 = quantile(fac, 0.9))

#  res <- bind_cols(res, loess = predict(loess(fac ~ dev, span = .7, data=df),
#                                       data.frame(dev = 2:max(df$dev))))

#  res <- bind_cols(res, qr75 = c(predict(rqss(fac ~ qss(dev, lambda = 0.5), tau = 0.75, data=df),
 #                                      data.frame(dev = 2:max(df$dev)))))
#  res <- bind_cols(res, qr90 = c(predict(rqss(fac ~ qss(dev, lambda = 0.5), tau = 0.90, data=df),
 #                                      data.frame(dev = 2:max(df$dev)))))
  list(df=df, est=res)
}

dtri <- function(dat, cal = FALSE, digits = 2, th = 0, q = c(0.2,2)){

  foo <- function(x, q) {
    cell_spec(x, color = if_else(!is.na(x), if_else(x < th, "red", "black"), "white"),
              font_size = case_when(
                abs(x - th) < q[1] ~ 12,
                abs(x - th) >=q[1] & abs(x) < q[2] ~ 14,
                abs(x - th) >=q[2] ~ 18) )
  }

  firstcol <- c(min(dat$ay):max(dat$ay))
  dt <- if(cal) {
    firstcol <- c(firstcol, "Sum-diag")
    latest <- filter(dat, dev == 1) %>% select(value) %>% pull
    dt <- dat %>% select(ay, cy, value) %>% spread(cy, value) %>% select(-ay)
    # pad with 0 at the end if young ays are missing from dat
    latest <- c(latest, rep(0, max(0,ncol(dt) - length(latest))))
    bind_rows(colSums(dt, na.rm = TRUE) - latest) %>% bind_rows(dt,.)
  } else {
    select(dat, ay, dev, value) %>% spread(dev, value) %>% select(-ay)
  }

  dt <- round(dt,digits)

  dt %>%
    mutate_all(list(~foo(.,q))) %>%
    add_column(AY=firstcol, .before = 1) %>%
    kable(escape=FALSE, caption = "", digits = 2) %>%
    column_spec(1, bold = T, include_thead = TRUE) %>%
    kable_styling(bootstrap_options = c("bordered", "striped", "condensed")) %>%
    column_spec(1, bold = T)
}

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("res40"),
                sidebarLayout(
                  sidebarPanel(
                    # Select type of trend to plot
                    sliderInput("year", "AY", min(dat$ay), max(dat$ay), step = 1,
                                value = c(2000, 2017),
                                sep = ""),
                    sliderInput("dev", "Dev", min(dat$dev), max(dat$dev), step = 1,
                                value = c(1, 10),
                                sep = ""),
                    selectInput(inputId = "lob",
                                label = "LOB",
                                choices = unique(dat$lob),
                                selected = unique(dat$lob)[1]),
                    selectInput(inputId = "type",
                                label = "Triangle Type",
                                choices = unique(dat$type),
                                selected = unique(dat$type)[1]),
                    selectInput(inputId = "digits",
                                label = "Digits",
                                choices = 0:4,
                                selected = 2),
                    checkboxInput(inputId = "cumul",
                                  label = "Cumulative",
                                  value = TRUE),
                    checkboxInput(inputId = "calen",
                                  label = "Calendar view",
                                  value = FALSE),
                    checkboxInput(inputId = "facet",
                                  label = "Facet loss curves",
                                  value = FALSE),
                    checkboxInput(inputId = "logloss",
                                  label = "Log loss",
                                  value = FALSE),
                    checkboxInput(inputId = "lossratio",
                                  label = "Loss Ratio",
                                  value = FALSE)
                  ),
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel("Triangle", tableOutput(outputId = "tritable")),
                                tabPanel("Loss Curves", plotOutput(outputId = "curves")),
                                tabPanel("Dev Factors", tableOutput(outputId = "factable")),
                                tabPanel("Factor Boxplots", plotOutput(outputId = "boxplots")),
                                tabPanel("Factor Curves", plotOutput(outputId = "faccurves")),
                                tabPanel("Results", tableOutput(outputId = "faccurvestable"))
                    )
                  )
                )
)

# Define server function
server <- function(input, output) {

  # Subset data
  datre <- reactive({
    dat %>%
      filter(lob == input$lob,
             type == input$type,
             ay >= input$year[1] & ay <= input$year[2],
             dev >= input$dev[1] & dev <= input$dev[2],
             cumul == input$cumul
      )
  })

  output$curves <- renderPlot({
    datp <- datre()
    if(input$logloss) {datp$value <- log(datp$value)}
    if(input$lossratio) {datp$value <- datp$lr}
#    datp <- datp %>% drop_na(value)
    p <- ggplot(datp, aes(x=dev, y=value, group=as.factor(ay), colour=as.factor(ay))) +
      geom_point(size=2) +
      geom_line(size=1) +
      scale_x_continuous(breaks = seq(min(datp$dev), max(datp$dev), by = 1)) +
      ggtitle("Losses") +
      scale_color_discrete(name = "AY") + labs(x="Development year", y=if_else(input$lossratio, "Loss Ratio", "Amount in € million")) +
      theme(legend.text=element_text(size=16))
    if(input$facet) {p + facet_wrap(~ as.factor(ay), ncol=6)}
    else { p }
  },
  height = 800)

  output$tritable <- function(){
    dtri(datre(), cal=input$calen, digits=as.numeric(input$digits))
  }

  output$factable <- function(){
    df <- group_by(datre(), ay) %>% mutate(value = value / lag(value)) %>% ungroup()
    dtri(df, cal=input$calen, digits=as.numeric(input$digits), th = 1, q = c(0.05,0.15))
  }

  output$boxplots <- renderPlot({
    df <- group_by(datre(), ay) %>%
      mutate(value = value / lag(value)) %>%
      ungroup() %>% na.omit
    ggplot(df, aes(x=as.factor(dev), y=value)) +
      geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4) +
      labs(x="Development year", y="Value")
  })

  output$faccurves <- renderPlot({
    res <- fdev(datre())
    df <- res$df
    df1 <- res$est %>% gather(method, fac, -dev)

    ggplot(df, aes(x=as.factor(dev), y=fac)) +
      geom_jitter(width=0.1) +
      geom_line(size=1, data=df1, aes(x=as.factor(dev), y=fac, group=as.factor(method), colour=as.factor(method))) +
      #       scale_x_continuous(breaks = seq(min(df$dev), max(df$dev), by = 1)) +
      scale_color_discrete(name = "Method") + labs(x="Development year", y="") +
      theme(legend.text=element_text(size=16))
  },
  height = 800)

  output$faccurvestable <- function(){
    res <- fdev(datre())
    df1 <- res$est %>%
      ungroup %>%
      gather(meth,val,-dev) %>%
      spread(dev, val) %>%
      rename(Method = meth) %>%
      kable("html", digits=as.numeric(input$digits)) %>%
      kable_styling(bootstrap_options = "striped", full_width = F, position = "left", font_size = 20)
  }

}

# Create Shiny object
shinyApp(ui = ui, server = server)


