

# Define server function
server <- function(input, output, session) {

  # Authentication
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  output$res_auth <- renderPrint({
    cat("Welcome ", reactiveValuesToList(result_auth)[[1]], "!")
  })

# TOP LEVEL FILTERS ----
  db_fin_top <- reactive(
    dplyr::filter(db_fin, currency == input$in_currency)
    #dplyr::between(end_date, input$in_end[1], input$in_end[2])
  )
  db_fin_time_top <- reactive(
    dplyr::filter(db_fin_time, currency == input$in_currency)
  )


# HOME PAGE ----

  output$empty_row <- renderPrint(cat(" "))

  ## Calculate main nums for later use ----
  main_nums <- reactive({
    # Get summary for active 
    main_num <- dplyr::filter(db_fin_top(), active == "active") |>
          dplyr::group_by(category) |>
          dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

    income <- dplyr::pull(main_num[main_num$category == "income", "value"])
    expense <- dplyr::pull(main_num[main_num$category == "expense", "value"])
    loan <- dplyr::pull(main_num[main_num$category == "loan", "value"])
    balance <- income - (expense + loan)

    main_nums <- list(income = income, expense = expense,
    loan = loan, balance = balance)
  })
  ## Expiring expenses in 6 months
expires <- reactive({
    db_fin_top() |>
      dplyr::filter(active == "active", expire_months <= 6) |>
      dplyr::group_by(expire_months, item, category) |>
      dplyr::tally(value, name = "value") |>
      dplyr::select(category, item, expire_months, value)
  })

  ## Highlights ----

  ### Highlight main numbers - render on top of home page ----
  output$income <- renderPrint({
    cat(fmat.num(main_nums()[["income"]]), " ", input$in_currency)
  })
  output$expense <- renderPrint({
    cat(fmat.num(main_nums()[["expense"]]), " ", input$in_currency)
  })
  output$loan <- renderPrint({
    cat(fmat.num(main_nums()[["loan"]]), " ", input$in_currency)
  })
  output$balance <- renderPrint({
    cat(fmat.num(main_nums()[["balance"]]), " ", input$in_currency)
  })

  ### Highlight expense/loans expires
  output$exp_expire3 <- renderPrint({
    df <- expires()
    x <- df[
      df$expire_months <= 3 & df$category %in% c("expense", "loan"), "value"
    ]
    HTML(paste(fmat.num(sum(x)), "in 3 months", sep = "<br/>"))
  })
  output$exp_expire6 <- renderPrint({
    df <- expires()
    x <- df[
      df$expire_months > 3 & df$category %in% c("expense", "loan"), "value"
    ]
    HTML(paste(fmat.num(sum(x)), "in 6 months", sep = "<br/>"))
  })
  ### Highlight income expires
  output$inc_expire3 <- renderUI({
    df <- expires()
    x <- df[df$expire_months <= 3 & df$category == "income", "value"]
    HTML(paste(fmat.num(sum(x)), "in 3 months", sep = "<br/>"))
  })
  output$inc_expire6 <- renderPrint({
    df <- expires()
    x <- df[df$expire_months > 3 & df$category == "income", "value"]
    HTML(paste(fmat.num(sum(x)), "in 6 months", sep = "<br/>"))
  })


  ## Expense asigments table by bank ----
  output$asigns <- renderTable({
    # Summarise finance by banks
    asigns <- dplyr::filter(db_fin_top(), active == "active") |>
      dplyr::group_by(bank_asign, category) |>
      dplyr::summarise(expense = sum(value, na.rm = TRUE), .groups = "drop") |>
      tidyr::pivot_wider(
        names_from = category, values_from = expense, values_fill = 0
      ) |>
      dplyr::mutate(total_expenses = expense + loan)
    # Asign life balance as reminder of income-expenses
    balance <- sum(asigns$income) - sum(asigns$total_expenses)
    asigns$life_balance <- 0
    asigns$life_balance[asigns$bank_asign == "bpi_current"] <- balance * 0.8
    asigns$life_balance[asigns$bank_asign == "santander"] <- balance * 0.2
    # Get column totals
    asigns <- dplyr::mutate(
      asigns, input = total_expenses + life_balance, .before = 2
    )
    asigns_total <- dplyr::summarise(asigns, dplyr::across(-bank_asign, sum)) |>
      dplyr::mutate(bank_asign = "all_banks", .before = 1)
    # Bind totals and format out 
    asigns <- dplyr::bind_rows(asigns, asigns_total) |>
      dplyr::select(bank_asign, input, total_expenses, life_balance) |>
      dplyr::mutate(dplyr::across(
        c(input, total_expenses, life_balance), ~ fmat.num(.)
      ))
    new_names <- c(
      "Asigned Bank" = "bank_asign",
      "Input transfer" = "input",
      "Expenses" = "total_expenses",
      "Life Balance" = "life_balance"
    )
    asigns <- dplyr::rename(asigns, dplyr::all_of(new_names))
  },
  striped = TRUE,
  # caption = as.character(shiny::tags$h4(style = "color: black", "Bank assignments")),
  # caption.placement = "top",
  digits = 0, align = "c"
  )

  ## Expenses trhough time
  output$fin_time <- renderPlotly({
    df <- db_fin_time_top() |>
      dplyr::select(category, date, value)
    # recat expense & loan into expenses
    if (input$agg_expenses) {
      df <- dplyr::mutate(df, category = dplyr::if_else(
        category %in% c("expense", "loan"), "expenses", category))
    }
    # Summarise by data and cumsum
    df <- dplyr::group_by(df, category, date) |>
      dplyr::tally(value, name = "value") |>
      dplyr::mutate(value_cum = round(cumsum(value), 0))

    if (!input$agg_expenses) {
      # Get last expense and set same value for today. (only when not agg_expenses) # nolint: line_length_linter.
      # Loan/income run for years/months ahead, but last expense added usually is months ago. To at least read the current expenses until today,use this. This piece makes no sense when expense andloanare aggregated. # nolint: line_length_linter.
      max_date <- df[df$category == "expense", "date"] |> dplyr::pull() |> max()
      max_val <- df[df$date == max_date & df$category == "expense", "value_cum"] |>
        dplyr::pull()
      expense_today <- data.frame(
        category = "expense", date = Sys.Date(), value = 0, value_cum = max_val
      )
      df <- dplyr::bind_rows(df, expense_today)
    }
    # Draw plot
    cols <- c(income = "green3", expense = "goldenrod3", 
      loan = "red3", expenses = "red3")
    linep <- ggplot(df, aes(x = date, y = value_cum, color = category)) +
      geom_step() +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      scale_colour_manual(values = cols) +
      labs(y = "Amount") +
      theme_minimal() +
      theme(axis.title.x = element_blank())

    lineply <- ggplotly(linep) |> layout(legend = list(orientation = 'h'))
})

  ## Staked bar by group
  output$stackp <- renderPlotly({
    stackp <- db_fin_top() |>
      dplyr::filter(active == "active", category != "income") |>
      dplyr::group_by(category, group) |> dplyr::tally(value, name = "value") |>
      ggplot(aes(x = group, y = value, fill = category, text = fmat.num(value))) +
      geom_bar(stat = "identity") +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = c("goldenrod3", "red3")) +
      #labs(x = "Expense group", y = "Amount") +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        axis.text.x = ggplot2::element_text(angle = -45, hjust = 0, vjust = 1))
    stackply <- ggplotly(stackp, tooltip = c("x", "y")) |>
      layout(showlegend = FALSE)
  })
  ## Waterfall chart by group
  output$waterp <- renderPlotly({
    # Format data for waterfall
    balance <- dplyr::filter(db_fin_top(), active == "active") |>
      dplyr::group_by(category, group) |>
      dplyr::summarise(value = sum(value), .groups = "drop") |>
      dplyr::mutate(
        value = dplyr::if_else(category %in% c("expense", "loan"), -value, value)
      )
    # Get reminder as life balance
    income <- balance[1,]
    expenses <- dplyr::arrange(balance[-1, ], group)
    life_balance <- data.frame(
      category = "net", group = "life_balance", value = sum(balance$value)
    )
    # Bind reminder to all expensies/income
    balance <- dplyr::bind_rows(income, expenses, life_balance)
    ## get category prefix for group
    pre_cat <- gsub("(.{4}).+", "\\1", balance$category)
    # Convert to factor and unique identify groups (groups+category)
    balance <- balance |>
      dplyr::mutate(
          category = factor(category, levels = c("income", "expense", "loan", "net")),
          group = factor(paste0(pre_cat, "_", group), levels = unique(paste0(pre_cat, "_", group)))
      )
    # Calculate ends for waterfall
    balance$id <- seq_along(balance$value)
    balance$end <- cumsum(balance$value)
    balance$end <- c(head(balance$end, -1), 0)
    balance$start <- c(0, head(balance$end, -1))
    balance <- dplyr::select(balance, id, category, group, start, end, value)
    # plot waterfall
    p_breaks <- pretty(min(balance$end):max(balance$start), n = 10)
    waterp <- ggplot(balance, aes(
      x = group, xmin = id - 0.5, xmax = id + 0.5, ymin = end, ymax = start,
      fill = category, text = paste(group, fmat.num(value))
    )) +
      geom_rect() +
      scale_y_continuous(
        labels = scales::comma,
        breaks = c(min(balance$end), p_breaks[-1], max(balance$start))
      ) +
      scale_fill_manual(values = c("green3", "goldenrod3", "red3", "steelblue3")) +
      scale_x_discrete(labels = gsub("^.+_(.+)$", "\\1", balance$group)) +
      labs(x = "Expenses group", y = "Amount") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = -45, hjust = 0, vjust = 1)
      )
    waterply <- ggplotly(waterp) |> 
      layout(legend = list(orientation = 'h', x = 0.3, y = 1.1))
  })

  # DETAILED
  output$waterp_detail <- renderPlotly({
    # Format data for waterfall
    balance <- dplyr::filter(db_fin_top(), active == "active") |>
      dplyr::mutate(
        value = dplyr::if_else(category %in% c("expense", "loan"), -value, value)
      )
    # Get reminder as life balance
    income <- balance[1,]
    expenses <- dplyr::arrange(balance[-1, ], group, item)
    life_balance <- data.frame(
      category = "net", item = "life_balance", value = sum(balance$value)
    )
    # Bind reminder to all expensies/income
    balance <- dplyr::bind_rows(income, expenses, life_balance)
    ## get category prefix for group
    pre_grp <- gsub("(.{4}).+", "\\1", balance$group)
    # Convert to factor and unique identify groups (groups+category)
    balance <- balance |>
      dplyr::mutate(
          category = factor(category, 
            levels = c("income", "expense", "loan", "net")),
          #group = factor(paste0(pre_cat, "_", item), levels = unique(paste0(pre_cat, "_", item))),
          item = factor(paste0(pre_grp, "_", item), 
            levels = unique(paste0(pre_grp, "_", item))),
      )
    # Calculate ends for waterfall
    balance$id <- seq_along(balance$value)
    balance$end <- cumsum(balance$value)
    balance$end <- c(head(balance$end, -1), 0)
    balance$start <- c(0, head(balance$end, -1))
    balance <- dplyr::select(balance, id, category, item, start, end, value)
    # plot waterfall
    p_breaks <- pretty(min(balance$end):max(balance$start), n = 10)
    waterp_detail <- ggplot(balance, aes(
      x = item, xmin = id - 0.5, xmax = id + 0.5, ymin = end, ymax = start,
      fill = category, text = paste(item, fmat.num(value))
    )) +
      geom_rect() +
      scale_y_continuous(
        labels = scales::comma,
        breaks = c(min(balance$end), p_breaks[-1], max(balance$start))
      ) +
      scale_fill_manual(values = c("forestgreen", "goldenrod3", "red4", "steelblue4")) +
      #scale_x_discrete(labels = gsub("^.+_(.+)$", "\\1", balance$item)) +
      labs(x = "Expenses group", y = "Amount") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = -45, hjust = 0, vjust = 1)
      )
    waterply_detail <- ggplotly(waterp_detail)
  })

  output$expire_detail <- renderTable({
    expires() |>
      dplyr::filter(category %in% c("expense", "loan")) |>
      dplyr::select(item, months = expire_months, value)
  },
  striped = TRUE,
  caption = as.character(shiny::tags$h4(
    style = "color: black", "Expenses expiring"
  )),
  caption.placement = "top",
  digits = 0, align = "l"
  )
  output$income_detail <- renderTable({
    expires() |>
      dplyr::filter(category == "income") |>
      dplyr::select(item, months = expire_months, value)
  },
  striped = TRUE,
  caption = as.character(shiny::tags$h4(
    style = "color: black", "Income expiring"
  )),
  caption.placement = "top",
  digits = 0, align = "l"
  )

  # DATASETS ----

  ## Reactive filters ----
  db_fin_select <- reactive({
    # ilter data
    db_fin |> dplyr::filter(
      category %in% input$in_category,
      currency %in% input$in_currency_db,
      group %in% input$in_group,
      bank_asign %in% input$in_bank,
      active %in% input$in_active,
      #dplyr::between(end_date, input$daterange[1], input$daterange[2])
    )
  })
  ## Data table ----
  output$data_fin <- DT::renderDataTable({
    # Reoder columns
    db_fin_data <- dplyr::select(
      db_fin_select(), category, group, item, bank_asign, value, currency, 
      active, end_date, expire_months,
    ) |>
    dplyr::mutate(value = fmat.num(value))
    # Render datatable
    DT::datatable(db_fin_data, editable = TRUE)
  })

}
