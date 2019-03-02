# Load packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(kableExtra)

# Load data
dat <- readRDS(file="~/R/triplot/data/prv.RDat")
dat <- arrange(dat, lob, type, ay, dev)

# Add incremental diff and devf columns
dat <- dat %>% group_by(lob, type, ay) %>%
  mutate(diff = value - lag(value)) %>%
  mutate(diff = if_else(is.na(diff), value, diff)) %>%
  ungroup() %>%
  gather(cumul, value, -(lob:dev)) %>%
  mutate(cumul = if_else(cumul == "value", TRUE, FALSE))

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
                titlePanel("Visualize Loss Triangles"),
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
                                  label = "Calender View",
                                  value = FALSE),
                    checkboxInput(inputId = "facet",
                                  label = "Facet loss curves",
                                  value = FALSE)
                  ),
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel("Triangle", tableOutput(outputId = "tritable")),
                                tabPanel("Loss Curves", plotOutput(outputId = "curves")),
                                tabPanel("Dev Factors", tableOutput(outputId = "factable")),
                                tabPanel("Factor Boxplots", plotOutput(outputId = "boxplots")),
                                tabPanel("Factor Curves", plotOutput(outputId = "faccurves"),
                                         tableOutput(outputId = "faccurvestable"))
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
    datp <- datre() %>% na.omit()
    p <- ggplot(datp, aes(x=dev, y=value, group=as.factor(ay), colour=as.factor(ay))) +
      geom_point(size=2) +
      geom_line(size=1) +
      scale_x_continuous(breaks = seq(min(datp$dev), max(datp$dev), by = 1)) +
      ggtitle("Losses") +
      scale_color_discrete(name = "AY") + labs(x="Development year", y="Amount in € million")
    if(input$facet) {p + facet_wrap(~ as.factor(ay), ncol=6)}
    else { p }
  })

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
    df <- group_by(datre(), ay) %>%
      mutate(fac = value / lag(value)) %>%
      ungroup %>% na.omit %>% filter(is.finite(fac))

    df1 <- df %>% group_by(dev) %>%
      summarise(mean = mean(fac, trim = 0),
                trimmean = mean(fac, trim = 1/n())) %>%
      ungroup %>%
      gather(method, fac, -dev)

    ggplot(df, aes(x=as.factor(dev), y=fac)) +
      geom_jitter(width=0.1) +
      #         geom_boxplot(width=0.05, outlier.colour="red", outlier.shape=8, outlier.size=4) +
      geom_line(size=1, data=df1, aes(x=as.factor(dev), y=fac, group=as.factor(method), colour=as.factor(method))) +
      #       scale_x_continuous(breaks = seq(min(df$dev), max(df$dev), by = 1)) +
      scale_color_discrete(name = "Method") + labs(x="Development year", y="")
  })

  output$faccurvestable <- function(){
    df <- group_by(datre(), ay) %>%
      mutate(fac = value / lag(value)) %>%
      ungroup %>% na.omit %>% filter(is.finite(fac))

    df1 <- df %>% group_by(dev) %>%
      summarise(mean = mean(fac, trim = 0),
                trimmean = mean(fac, trim = 1/n())) %>%
      ungroup %>%
      gather(meth,val,-dev) %>%
      spread(dev, val) %>%
      rename(Method = meth) %>%
      kable("html", digits=as.numeric(input$digits)) %>%
      kable_styling(bootstrap_options = "striped", full_width = F, position = "center")
  }

}

# Create Shiny object
shinyApp(ui = ui, server = server)


