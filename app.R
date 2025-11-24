library(shiny)
library(bslib)
library(fpp3)
library(gt)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(purrr)
library(rsconnect)

# -------------------------------------------------------------------
# Data prep
# -------------------------------------------------------------------

aus_wine <- readr::read_csv(
  "AustralianWines.csv",
  na = "*",
  col_types = readr::cols(
    Rose = readr::col_number()
  ),
  show_col_types = FALSE
) |>
  tidyr::fill(Rose, .direction = "down") |>
  dplyr::mutate(
    Month = lubridate::mdy(
      stringr::str_replace(Month, "-", "-01-")
    ) |> yearmonth()
  ) |>
  tidyr::pivot_longer(
    cols      = -Month,
    names_to  = "Varietal",
    values_to = "Sales"
  ) |>
  as_tsibble(index = Month, key = Varietal)

varietals <- aus_wine |>
  dplyr::distinct(Varietal) |>
  dplyr::pull(Varietal)

min_month_ym <- min(aus_wine$Month)
max_month_ym <- max(aus_wine$Month)

min_month <- as.Date(min_month_ym)
max_month <- as.Date(max_month_ym)

# -------------------------------------------------------------------
# UI
# -------------------------------------------------------------------

ui <- page_navbar(
  title = "Australian Wine Forecast",

  # TAB 1 - Visualization
  nav_panel(
    title = "Visualization",
    page_sidebar(
      sidebar = sidebar(
        checkboxGroupInput(
          "selected_varietals",
          "Select varietals:",
          choices  = setNames(varietals, varietals),
          selected = varietals
        ),
        sliderInput(
          "date_range",
          "Training / forecast range:",
          min   = min_month,
          max   = max_month,
          # default: last year of data
          value = c(max_month - 365, max_month),
          timeFormat = "%Y-%m"
        ),
        p("First date: Training cutoff (default 1 year before end)."),
        p("Second date: End of forecast horizon."),
        checkboxInput(
          "free_y_scale",
          "Free Y scale",
          value = TRUE
        )
      ),
      card(
        card_header("Wine Sales Overview"),
        plotOutput("overview_plot", height = "700px")
      ),
      card(
        card_header("Seasonal and Trend Decomposition (STL)"),
        plotOutput("decomp_plot", height = "700px")
      )
    )
  ),

  # TAB 2 - Model Building
  nav_panel(
    title = "Model Building",
    page_sidebar(
      sidebar = sidebar(
        checkboxInput(
          "show_model_spec",
          "Show model specifications",
          value = TRUE
        ),
        checkboxInput(
          "show_training_accuracy",
          "Show training accuracy",
          value = TRUE
        ),
        # Download button for model accuracy tables
        downloadButton("download_accuracy", "Download Accuracy CSV")
      ),
      # Model specs
      conditionalPanel(
        condition = "input.show_model_spec",
        card(
          card_header("Model Specifications"),
          tableOutput("model_specs")
        )
      ),

      # Training / validation accuracy
      layout_columns(
        conditionalPanel(
          condition = "input.show_training_accuracy",
          card(
            card_header("Training Accuracy"),
            tableOutput("training_accuracy")
          )
        ),
        card(
          card_header("Validation Accuracy"),
          tableOutput("validation_accuracy")
        )
      )
    )
  ),

  # TAB 3 - Forecasts
  nav_panel(
    title = "Forecasts",
    page_sidebar(
      sidebar = sidebar(
        checkboxGroupInput(
          "selected_models",
          "Select models for forecast:",
          choices = c(
            "TSLM"  = "TSLM",
            "ETS"   = "ETS",
            "ARIMA" = "ARIMA"
          ),
          selected = c("TSLM", "ETS", "ARIMA")
        ),
        # Download button for forecasts
        downloadButton("download_forecast", "Download Forecast CSV")
      ),
      layout_columns(
        card(
          card_header("Forecast Visualization"),
          plotOutput("forecast_plot", height = "600px")
        ),
        card(
          card_header("Forecast Tables"),
          htmlOutput("forecast_table_out")
        )
      )
    )
  )
)

# -------------------------------------------------------------------
# SERVER
# -------------------------------------------------------------------

server <- function(input, output, session) {

  # Filtered data based on varietal selection
  filtered_data <- reactive({
    req(input$selected_varietals)

    aus_wine |>
      filter(Varietal %in% input$selected_varietals)
  })

  # Training cutoff and forecast end as yearmonth
  train_cutoff_ym <- reactive({
    yearmonth(input$date_range[1])   # first slider date
  })

  forecast_end_ym <- reactive({
    yearmonth(input$date_range[2])   # second slider date
  })

  training_data <- reactive({
    df  <- filtered_data()
    cut <- train_cutoff_ym()
    df |>
      filter(Month < cut)
  })

  validation_data <- reactive({
    df  <- filtered_data()
    cut <- train_cutoff_ym()
    df |>
      filter(Month >= cut, Month <= forecast_end_ym())
  })

  # Number of forecast months between cutoff and end
  h_periods <- reactive({
    cut <- train_cutoff_ym()
    end <- forecast_end_ym()
    dates_seq <- seq(as.Date(cut), as.Date(end), by = "month")
    length(dates_seq)
  })

  # 3.1 Fitted models
  fitted_models <- reactive({
    train_ts <- training_data()

    # Min = at least 36 rows total
    validate(
      need(nrow(train_ts) >= 36,
           "Not enough training data for the selected varietals.")
    )

    train_ts |>
      model(
        TSLM  = TSLM(Sales ~ trend() + season()),
        ETS   = ETS(Sales),
        ARIMA = ARIMA(Sales)
      )
  })

  # Forecasts for horizon defined by the slider / date range
  forecasts <- reactive({
    mdl <- fitted_models()
    h   <- h_periods()
    req(mdl, h > 0)
    forecast(mdl, h = h)
  })

  # ---------------------------------------------------------------
  # Tab 1 - Visualization
  # ---------------------------------------------------------------

  output$overview_plot <- renderPlot({
    df  <- filtered_data()
    cut <- train_cutoff_ym()
    end <- forecast_end_ym()
    req(nrow(df) > 0)

    scales_option <- if (isTRUE(input$free_y_scale)) "free_y" else "fixed"

    df |>
      autoplot(Sales) +
      annotate(
        "rect",
        xmin = as.Date(cut),
        xmax = as.Date(end),
        ymin = -Inf,
        ymax = Inf,
        alpha = 0.15
      ) +
      geom_vline(
        xintercept = as.Date(cut),
        colour     = "red",
        linetype   = "solid",
        linewidth  = 0.7
      ) +
      labs(
        y       = "Sales (thousand litres)",
        title   = "Australian wine sales by varietal",
        caption = "Red line: training cutoff. Gray area: forecast horizon."
      ) +
      facet_wrap(~ Varietal, scales = scales_option) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title   = element_text(hjust = 0.5, size = 16, face = "bold"),
        strip.text   = element_text(face = "bold", size = 12),
        panel.grid.minor = element_blank()
      )
  })

  output$decomp_plot <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 36)

    scales_option <- if (isTRUE(input$free_y_scale)) "free_y" else "fixed"

    df |>
      model(STL = STL(Sales)) |>
      components() |>
      autoplot() +
      facet_wrap(~ Varietal, scales = scales_option) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        strip.text = element_text(face = "bold")
      )
  })

  # ---------------------------------------------------------------
  # Tab 2 - Model specs & accuracy
  # ---------------------------------------------------------------

  output$model_specs <- renderTable({
    mdl <- fitted_models()
    req(NROW(mdl) > 0)

    mdl |>
      as_tibble() |>
      pivot_longer(
        cols = -Varietal,
        names_to = "Model",
        values_to = "fit"
      ) |>
      mutate(Spec = format(fit)) |>
      select(Varietal, Model, Spec)
  })

  output$training_accuracy <- renderTable({
    mdl <- fitted_models()

    mdl |>
      accuracy() |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(Varietal, RMSE)
  })

  output$validation_accuracy <- renderTable({
    mdl <- fitted_models()
    fc  <- forecasts()
    valid_ts <- validation_data()
    req(nrow(valid_ts) > 0)

    fc |>
      accuracy(valid_ts) |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(Varietal, RMSE)
  })

  # Download accuracy CSV
  output$download_accuracy <- downloadHandler(
    filename = function() {
      paste0("model_accuracy_", Sys.Date(), ".csv")
    },
    content = function(file) {
      mdl <- fitted_models()
      train_acc <- mdl |>
        accuracy() |>
        mutate(type = "training")
      fc  <- forecasts()
      valid_ts <- validation_data()
      valid_acc <- fc |>
        accuracy(valid_ts) |>
        mutate(type = "validation")
      acc_all <- bind_rows(train_acc, valid_acc)
      write.csv(acc_all, file, row.names = FALSE)
    }
  )

  # ---------------------------------------------------------------
  # Tab 3 - Forecasts
  # ---------------------------------------------------------------

  output$forecast_plot <- renderPlot({
    fc <- forecasts()
    req(fc, input$selected_models)

    fc_filt <- fc |>
      dplyr::filter(.model %in% input$selected_models)

    cut <- train_cutoff_ym()
    history_start <- cut - 36

    hist_data <- filtered_data() |>
      dplyr::filter(Month >= history_start)

    scales_option <- if (isTRUE(input$free_y_scale)) "free_y" else "fixed"
    n_models <- length(input$selected_models)

    fc_filt |>
      autoplot(hist_data) +
      labs(
        y       = "Sales (thousand litres)",
        title   = "Model forecasts by varietal and model",
        caption = "History: last 3 years; solid lines: forecasts."
      ) +
      facet_wrap(Varietal ~ .model, ncol = n_models, scales = scales_option) +
      theme_minimal(base_size = 12)
  })

  output$forecast_table_out <- renderUI({
    fc <- forecasts()
    req(fc, input$selected_models)

    end <- forecast_end_ym()

    table_list <- map(input$selected_models, function(model_name) {

      tbl <- fc |>
        filter(.model == model_name) |>
        as_tibble() |>
        filter(Month <= end) |>
        select(Varietal, Month, .mean) |>
        mutate(Month = as.character(Month)) |>
        pivot_wider(
          names_from  = Month,
          values_from = .mean
        )

      gt_tbl <- tbl |>
        gt() |>
        fmt_number(everything(), decimals = 1) |>
        tab_header(title = paste("Forecast Table -", model_name))

      div(
        style = "margin-bottom: 30px;",
        HTML(as_raw_html(gt_tbl))
      )
    })

    do.call(tagList, table_list)
  })

  # Download forecast CSV
  output$download_forecast <- downloadHandler(
    filename = function() {
      paste0("forecast_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      fc <- forecasts() |>
        as_tibble()
      write.csv(fc, file, row.names = FALSE)
    }
  )
}

# -------------------------------------------------------------------
# Run the app
# -------------------------------------------------------------------

shinyApp(ui, server)
