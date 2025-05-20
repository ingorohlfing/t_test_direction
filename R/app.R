library(shiny)
library(ggplot2)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "flatly"),
  titlePanel("Richtung des t-Tests"),
  sidebarLayout(
    sidebarPanel(
      numericInput("t_value", HTML("t-Wert der Stichprobe<br>(min: -2.5; max: 2.5)"),
                   value = 0, min = -2.5, max = 2.5, step = 0.1),
      numericInput("df", HTML("Freiheitsgrade <br> (min: 1; max: 100)"),
                   value = 30, min = 1, max = 100, step = 1),
      sliderInput("alpha", "Signifikanzniveau (α)",
                  min = 0.01, max = 0.1, value = 0.05, step = 0.01),
      radioButtons(
        "test_type", "Testart",
        choices = c("Zweiseitig" = "two.sided", "Linksseitig" = "left", "Rechtsseitig" = "right"),
        selected = "two.sided"
      ),
      tags$hr(),
      tags$p("Creator: Ingo Rohlfing",
             tags$a(href = "https://github.com/ingorohlfing", "(https://github.com/ingorohlfing)", target = "_blank")),
      width = 3
    ),
    mainPanel(
      plotOutput("tPlot"),
    div(style = "margin-top: 20px;",
        textOutput("infoText")),
    width = 7
    )
  )
)

server <- function(input, output) {
  output$tPlot <- renderPlot({
    df <- input$df
    x <- seq(-4, 4, length.out = 1000)
    y <- dt(x, df = df)
    data <- data.frame(x, y)

    # Calculate critical values
    alpha <- input$alpha
    if (input$test_type == "two.sided") {
      crit_low <- qt(alpha / 2, df)
      crit_high <- qt(1 - alpha / 2, df)
      data$region <- ifelse(data$x <= crit_low | data$x >= crit_high,
                            "Kritisch", "Nicht kritisch")
      pvalue <- 2 * pt(-abs(input$t_value), df)
    } else if (input$test_type == "left") {
      crit <- qt(alpha, df)
      data$region <- ifelse(data$x <= crit, "Kritisch", "Nicht kritisch")
      pvalue <- pt(input$t_value, df)
    } else { # right
      crit <- qt(1 - alpha, df)
      data$region <- ifelse(data$x >= crit, "Kritisch", "Nicht kritisch")
      pvalue <- 1 - pt(input$t_value, df)
    }

    if (input$test_type == "two.sided") {
      ggplot(data, aes(x, y)) +
        geom_line(color = "black") +
        geom_area(
          data = subset(data, x <= crit_low),
          aes(x, y),
          fill = "skyBlue", alpha = 1
        ) +
        geom_area(
          data = subset(data, x >= crit_high),
          aes(x, y),
          fill = "skyBlue", alpha = 1
        ) +
        geom_area(
          data = subset(data, x >= abs(input$t_value)),
          aes(x, y),
          fill = "orange", alpha = 0.5
        ) +
        geom_area(
          data = subset(data, x <= -abs(input$t_value)),
          aes(x, y),
          fill = "orange", alpha = 0.5
        ) +
        geom_vline(xintercept = input$t_value,
                   color = "orange", linetype = "dashed", linewidth = 1) +
        scale_x_continuous(breaks = seq(-4, 4, 1)) +
        labs(
          title = paste("t-Verteilung mit kritischem Bereich (Freiheitsgrade = ", df, ")", sep = ""),
          x = "t-Wert", y = "Dichte",
          caption = paste("p-Wert: ", round(pvalue, 3),
                          "\n Kritische Werte: ",
                          round(crit_low, 2), " und ",
                          round(crit_high, 2), sep = "")
        ) +
        theme_classic()
    } else if (input$test_type == "left") {
      ggplot(data, aes(x, y)) +
        geom_line(color = "black") +
        geom_area(
          data = subset(data, region == "Kritisch"),
          aes(x, y),
          fill = "skyBlue", alpha = 1
        ) +
        geom_area(
          data = subset(data, x <= input$t_value),
          aes(x, y),
          fill = "orange", alpha = 0.5
        ) +
        geom_vline(xintercept = input$t_value,
                   color = "orange", linetype = "dashed", linewidth = 1) +
        scale_x_continuous(breaks = seq(-4, 4, 1)) +
        labs(
          title = paste("t-Verteilung mit kritischem Bereich (Freiheitsgrade = ", df, ")", sep = ""),
          x = "t",
          y = "Dichte",
          caption = paste("p-Wert: ", round(pvalue, 3),
                          "\n Kritischer Wert: ",
                          round(crit, digits = 3), sep = "")
        ) +
        theme_classic()
    } else {
      ggplot(data, aes(x, y)) +
        geom_line(color = "black") +
        geom_area(
          data = subset(data, region == "Kritisch"),
          aes(x, y),
          fill = "skyBlue", alpha = 1
        ) +
        geom_area(
          data = subset(data, x >= input$t_value),
          aes(x, y),
          fill = "orange", alpha = 0.5
        ) +
        geom_vline(xintercept = input$t_value,
                   color = "orange", linetype = "dashed", linewidth = 1) +
        scale_x_continuous(breaks = seq(-4, 4, 1)) +
        labs(
          title = paste("t-Verteilung mit kritischem Bereich (Freiheitsgrade = ", df, ")", sep = ""),
          x = "t",
          y = "Dichte",
          caption = paste("p-Wert: ", round(pvalue, 3),
                          "\n Kritischer Wert: ",
                          round(crit, digits = 3), sep = "")
        ) +
        theme_classic()
    }
  }, res = 96)
  output$infoText <- renderText({
    "In dem Plot sind zwei Bereiche farbig dargestellt. Der kritische Bereich
    ist in hellblau dargestellt (nicht als hellblau erkennbar, wenn er vom
    orangen Bereich überlagert wird. Der orange Bereich ist die grafische Darstellung
    des p-Werts für die gewählte Richtung des Tests.
    (Die Farben sind bei einer Farbenblindheit anders, sollten aber immer noch
    unterscheidbar sein.)"
  })
}

shinyApp(ui = ui, server = server)
