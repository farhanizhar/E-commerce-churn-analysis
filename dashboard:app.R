# ================================
# E-Commerce Churn Analysis
# Shiny Dashboard — app.R
# ================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)

# ================================
# LOAD DATA
# ================================
customers  <- read_csv("archive/olist_customers_dataset.csv")
orders     <- read_csv("archive/olist_orders_dataset.csv")
items      <- read_csv("archive/olist_order_items_dataset.csv")
payments   <- read_csv("archive/olist_order_payments_dataset.csv")
reviews    <- read_csv("archive/olist_order_reviews_dataset.csv")
products   <- read_csv("archive/olist_products_dataset.csv")
sellers    <- read_csv("archive/olist_sellers_dataset.csv")
cat_names  <- read_csv("archive/product_category_name_translation.csv")

# ================================
# PREPARE DATA
# ================================
orders_clean <- orders %>%
  filter(order_status == "delivered")

master <- orders_clean %>%
  left_join(customers, by = "customer_id") %>%
  left_join(items %>%
              group_by(order_id) %>%
              summarise(
                order_value = sum(price + freight_value),
                total_items = n()
              ), by = "order_id") %>%
  left_join(payments %>%
              group_by(order_id) %>%
              slice(1) %>%
              select(order_id, payment_type),
            by = "order_id") %>%
  left_join(reviews %>%
              group_by(order_id) %>%
              slice(1) %>%
              select(order_id, review_score),
            by = "order_id") %>%
  select(
    order_id, customer_unique_id,
    customer_state, customer_city,
    order_purchase_timestamp,
    order_delivered_customer_date,
    order_estimated_delivery_date,
    order_value, total_items,
    payment_type, review_score
  ) %>%
  mutate(
    order_month    = floor_date(order_purchase_timestamp, "month"),
    order_year     = year(order_purchase_timestamp),
    delivery_delay = as.numeric(difftime(
      order_delivered_customer_date,
      order_estimated_delivery_date,
      units = "days")),
    is_late        = delivery_delay > 0
  )

# Customer level
reference_date <- max(master$order_purchase_timestamp,
                      na.rm = TRUE)

customer_orders <- master %>%
  group_by(customer_unique_id) %>%
  summarise(
    total_orders    = n(),
    total_spent     = sum(order_value, na.rm = TRUE),
    avg_order_value = mean(order_value, na.rm = TRUE),
    first_order     = min(order_purchase_timestamp),
    last_order      = max(order_purchase_timestamp),
    avg_review      = mean(review_score, na.rm = TRUE),
    .groups         = "drop"
  ) %>%
  mutate(
    customer_type         = ifelse(total_orders == 1,
                                   "One-Time", "Repeat"),
    days_since_last_order = as.numeric(difftime(
      reference_date, last_order, units = "days")),
    churned               = ifelse(
      days_since_last_order > 180, "Churned", "Retained")
  )

# RFM
rfm_scored <- customer_orders %>%
  mutate(
    r_score   = ntile(desc(days_since_last_order), 5),
    f_score   = ntile(total_orders, 5),
    m_score   = ntile(total_spent, 5),
    rfm_score = r_score + f_score + m_score,
    segment   = case_when(
      r_score >= 4 & f_score >= 4 & m_score >= 4 ~ "Champions",
      r_score >= 3 & f_score >= 3                ~ "Loyal Customers",
      r_score >= 4 & f_score <= 2                ~ "Potential Loyalists",
      r_score >= 3 & f_score <= 2 & m_score >= 3 ~ "Promising",
      r_score <= 2 & f_score >= 3                ~ "At Risk",
      r_score <= 2 & f_score <= 2 & m_score >= 3 ~ "Cant Lose Them",
      r_score == 1 & f_score == 1                ~ "Lost",
      TRUE                                        ~ "Need Attention"
    )
  )

# Segment colors
seg_colors <- c(
  "Champions"           = "#1B5E20",
  "Loyal Customers"     = "#388E3C",
  "Potential Loyalists" = "#81C784",
  "Promising"           = "#FFF176",
  "Need Attention"      = "#FFB74D",
  "At Risk"             = "#EF6C00",
  "Cant Lose Them"      = "#F44336",
  "Lost"                = "#B71C1C"
)

# ================================
# UI
# ================================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "Churn Analysis"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",   tabName = "overview",
               icon = icon("chart-line")),
      menuItem("Churn",      tabName = "churn",
               icon = icon("user-minus")),
      menuItem("Cohort",     tabName = "cohort",
               icon = icon("table")),
      menuItem("RFM & CLV",  tabName = "rfm",
               icon = icon("users"))
    ),
    
    # Global year filter
    selectInput(
      inputId  = "year_filter",
      label    = "Select Year",
      choices  = c("All", "2016", "2017", "2018"),
      selected = "All"
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ── TAB 1: OVERVIEW ──────────────────────────
      tabItem(
        tabName = "overview",
        
        # KPI Cards
        fluidRow(
          valueBoxOutput("box_orders",    width = 3),
          valueBoxOutput("box_customers", width = 3),
          valueBoxOutput("box_revenue",   width = 3),
          valueBoxOutput("box_avg_order", width = 3)
        ),
        
        fluidRow(
          box(
            title  = "Orders Per Month",
            width  = 8,
            status = "primary",
            plotOutput("plot_orders_month", height = 300)
          ),
          box(
            title  = "Payment Types",
            width  = 4,
            status = "primary",
            plotOutput("plot_payment", height = 300)
          )
        ),
        
        fluidRow(
          box(
            title  = "Top 10 States by Orders",
            width  = 6,
            status = "info",
            plotOutput("plot_states", height = 300)
          ),
          box(
            title  = "Review Score Distribution",
            width  = 6,
            status = "info",
            plotOutput("plot_reviews", height = 300)
          )
        )
      ),
      
      # ── TAB 2: CHURN ─────────────────────────────
      tabItem(
        tabName = "churn",
        
        fluidRow(
          valueBoxOutput("box_churn_rate",    width = 4),
          valueBoxOutput("box_onetime",       width = 4),
          valueBoxOutput("box_repeat_rate",   width = 4)
        ),
        
        fluidRow(
          box(
            title  = "Churned vs Retained",
            width  = 6,
            status = "danger",
            plotOutput("plot_churn", height = 300)
          ),
          box(
            title  = "One-Time vs Repeat Buyers",
            width  = 6,
            status = "danger",
            plotOutput("plot_onetime", height = 300)
          )
        ),
        
        fluidRow(
          box(
            title  = "Churn Rate by Review Score",
            width  = 6,
            status = "warning",
            plotOutput("plot_churn_review", height = 300)
          ),
          box(
            title  = "Delivery Performance",
            width  = 6,
            status = "warning",
            plotOutput("plot_delivery", height = 300)
          )
        )
      ),
      
      # ── TAB 3: COHORT ────────────────────────────
      tabItem(
        tabName = "cohort",
        
        fluidRow(
          box(
            title  = "Retention Heatmap",
            width  = 12,
            status = "primary",
            plotOutput("plot_cohort", height = 500)
          )
        ),
        
        fluidRow(
          box(
            title  = "Average Retention Curve",
            width  = 12,
            status = "info",
            plotOutput("plot_retention_curve", height = 300)
          )
        )
      ),
      
      # ── TAB 4: RFM & CLV ─────────────────────────
      tabItem(
        tabName = "rfm",
        
        fluidRow(
          valueBoxOutput("box_champions", width = 4),
          valueBoxOutput("box_at_risk",   width = 4),
          valueBoxOutput("box_clv_uplift",width = 4)
        ),
        
        fluidRow(
          box(
            title  = "RFM Segment Distribution",
            width  = 6,
            status = "success",
            plotOutput("plot_rfm_seg", height = 350)
          ),
          box(
            title  = "Avg CLV by Segment",
            width  = 6,
            status = "success",
            plotOutput("plot_clv", height = 350)
          )
        ),
        
        fluidRow(
          box(
            title  = "Segment Revenue Summary",
            width  = 12,
            status = "info",
            tableOutput("table_segment")
          )
        )
      )
    )
  )
)

# ================================
# SERVER
# ================================
server <- function(input, output) {
  
  # Reactive filtered data
  master_f <- reactive({
    if (input$year_filter == "All") {
      master
    } else {
      master %>%
        filter(order_year == as.integer(input$year_filter))
    }
  })
  
  customer_f <- reactive({
    ids <- master_f() %>%
      pull(customer_unique_id) %>%
      unique()
    customer_orders %>%
      filter(customer_unique_id %in% ids)
  })
  
  # ── KPI BOXES — OVERVIEW ──────────────────────
  output$box_orders <- renderValueBox({
    valueBox(
      value    = format(nrow(master_f()), big.mark = ","),
      subtitle = "Total Orders",
      icon     = icon("shopping-cart"),
      color    = "blue"
    )
  })
  
  output$box_customers <- renderValueBox({
    valueBox(
      value    = format(
        n_distinct(master_f()$customer_unique_id),
        big.mark = ","),
      subtitle = "Unique Customers",
      icon     = icon("users"),
      color    = "green"
    )
  })
  
  output$box_revenue <- renderValueBox({
    valueBox(
      value    = paste0("R$ ", format(
        round(sum(master_f()$order_value, na.rm = TRUE), 0),
        big.mark = ",")),
      subtitle = "Total Revenue",
      icon     = icon("dollar-sign"),
      color    = "yellow"
    )
  })
  
  output$box_avg_order <- renderValueBox({
    valueBox(
      value    = paste0("R$ ", round(
        mean(master_f()$order_value, na.rm = TRUE), 2)),
      subtitle = "Avg Order Value",
      icon     = icon("receipt"),
      color    = "purple"
    )
  })
  
  # ── OVERVIEW PLOTS ────────────────────────────
  output$plot_orders_month <- renderPlot({
    master_f() %>%
      count(order_month) %>%
      ggplot(aes(x = order_month, y = n)) +
      geom_line(color = "#2196F3", linewidth = 1) +
      geom_point(color = "#2196F3", size = 2) +
      labs(x = "Month", y = "Orders") +
      theme_minimal()
  })
  
  output$plot_payment <- renderPlot({
    master_f() %>%
      filter(!is.na(payment_type)) %>%
      count(payment_type, sort = TRUE) %>%
      mutate(pct = round(n / sum(n) * 100, 1)) %>%
      ggplot(aes(x = reorder(payment_type, n),
                 y = n, fill = payment_type)) +
      geom_col() +
      geom_text(aes(label = paste0(pct, "%")),
                hjust = -0.1, size = 3) +
      coord_flip() +
      labs(x = "", y = "Orders") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$plot_states <- renderPlot({
    master_f() %>%
      count(customer_state, sort = TRUE) %>%
      slice(1:10) %>%
      ggplot(aes(x = reorder(customer_state, n),
                 y = n)) +
      geom_col(fill = "#4CAF50") +
      coord_flip() +
      labs(x = "State", y = "Orders") +
      theme_minimal()
  })
  
  output$plot_reviews <- renderPlot({
    master_f() %>%
      filter(!is.na(review_score)) %>%
      count(review_score) %>%
      mutate(pct = round(n / sum(n) * 100, 1)) %>%
      ggplot(aes(x = factor(review_score), y = n,
                 fill = factor(review_score))) +
      geom_col() +
      geom_text(aes(label = paste0(pct, "%")),
                vjust = -0.5, size = 3) +
      scale_fill_manual(values = c(
        "1" = "#F44336", "2" = "#FF9800",
        "3" = "#FFC107", "4" = "#8BC34A",
        "5" = "#4CAF50")) +
      labs(x = "Score", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # ── KPI BOXES — CHURN ────────────────────────
  output$box_churn_rate <- renderValueBox({
    rate <- customer_f() %>%
      summarise(r = round(
        mean(churned == "Churned") * 100, 1)) %>%
      pull(r)
    valueBox(
      value    = paste0(rate, "%"),
      subtitle = "Churn Rate (180 days)",
      icon     = icon("user-minus"),
      color    = "red"
    )
  })
  
  output$box_onetime <- renderValueBox({
    rate <- customer_f() %>%
      summarise(r = round(
        mean(customer_type == "One-Time") * 100, 1)) %>%
      pull(r)
    valueBox(
      value    = paste0(rate, "%"),
      subtitle = "One-Time Buyers",
      icon     = icon("user"),
      color    = "orange"
    )
  })
  
  output$box_repeat_rate <- renderValueBox({
    rate <- customer_f() %>%
      summarise(r = round(
        mean(customer_type == "Repeat") * 100, 1)) %>%
      pull(r)
    valueBox(
      value    = paste0(rate, "%"),
      subtitle = "Repeat Rate",
      icon     = icon("redo"),
      color    = "green"
    )
  })
  
  # ── CHURN PLOTS ───────────────────────────────
  output$plot_churn <- renderPlot({
    customer_f() %>%
      count(churned) %>%
      mutate(pct = round(n / sum(n) * 100, 1)) %>%
      ggplot(aes(x = churned, y = n, fill = churned)) +
      geom_col(width = 0.5) +
      geom_text(aes(label = paste0(pct, "%")),
                vjust = -0.5, size = 4) +
      scale_fill_manual(values = c(
        "Churned"  = "#F44336",
        "Retained" = "#4CAF50")) +
      labs(x = "", y = "Customers") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$plot_onetime <- renderPlot({
    customer_f() %>%
      count(customer_type) %>%
      mutate(pct = round(n / sum(n) * 100, 1)) %>%
      ggplot(aes(x = customer_type, y = n,
                 fill = customer_type)) +
      geom_col(width = 0.5) +
      geom_text(aes(label = paste0(pct, "%")),
                vjust = -0.5, size = 4) +
      scale_fill_manual(values = c(
        "One-Time" = "#F44336",
        "Repeat"   = "#4CAF50")) +
      labs(x = "", y = "Customers") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$plot_churn_review <- renderPlot({
    master_f() %>%
      filter(!is.na(review_score)) %>%
      group_by(customer_unique_id) %>%
      summarise(avg_review = mean(review_score),
                .groups = "drop") %>%
      left_join(customer_f() %>%
                  select(customer_unique_id, churned),
                by = "customer_unique_id") %>%
      filter(!is.na(churned)) %>%
      ggplot(aes(x = factor(round(avg_review)),
                 fill = churned)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = c(
        "Churned"  = "#F44336",
        "Retained" = "#4CAF50")) +
      labs(x = "Review Score", y = "Proportion",
           fill = "") +
      theme_minimal()
  })
  
  output$plot_delivery <- renderPlot({
    master_f() %>%
      filter(!is.na(is_late)) %>%
      mutate(label = ifelse(is_late,
                            "Late", "On Time")) %>%
      count(label) %>%
      mutate(pct = round(n / sum(n) * 100, 1)) %>%
      ggplot(aes(x = label, y = n, fill = label)) +
      geom_col(width = 0.5) +
      geom_text(aes(label = paste0(pct, "%")),
                vjust = -0.5, size = 4) +
      scale_fill_manual(values = c(
        "On Time" = "#4CAF50",
        "Late"    = "#F44336")) +
      labs(x = "", y = "Orders") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # ── COHORT PLOTS ──────────────────────────────
  output$plot_cohort <- renderPlot({
    cohort_data <- master_f() %>%
      group_by(customer_unique_id) %>%
      mutate(cohort_month = floor_date(
        min(order_purchase_timestamp), "month")) %>%
      ungroup() %>%
      mutate(
        order_month_date = floor_date(
          order_purchase_timestamp, "month"),
        month_number = as.integer(
          interval(cohort_month,
                   order_month_date) %/% months(1))
      )
    
    cohort_table <- cohort_data %>%
      group_by(cohort_month, month_number) %>%
      summarise(customers = n_distinct(customer_unique_id),
                .groups = "drop")
    
    cohort_size <- cohort_table %>%
      filter(month_number == 0) %>%
      select(cohort_month, cohort_size = customers)
    
    cohort_pct <- cohort_table %>%
      left_join(cohort_size, by = "cohort_month") %>%
      mutate(retention_pct = round(
        customers / cohort_size * 100, 1)) %>%
      filter(
        cohort_month >= as.Date("2017-01-01"),
        cohort_month <= as.Date("2018-04-01"),
        month_number <= 12
      ) %>%
      mutate(cohort_label = format(cohort_month, "%b %Y"))
    
    cohort_pct %>%
      ggplot(aes(
        x    = factor(month_number),
        y    = reorder(cohort_label, desc(cohort_month)),
        fill = retention_pct
      )) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = paste0(retention_pct, "%")),
                size = 3) +
      scale_fill_gradient2(
        low = "#FFEBEE", mid = "#EF9A9A",
        high = "#B71C1C", midpoint = 10) +
      labs(x = "Months After First Purchase",
           y = "Cohort", fill = "Retention %") +
      theme_minimal()
  })
  
  output$plot_retention_curve <- renderPlot({
    cohort_data <- master_f() %>%
      group_by(customer_unique_id) %>%
      mutate(cohort_month = floor_date(
        min(order_purchase_timestamp), "month")) %>%
      ungroup() %>%
      mutate(
        order_month_date = floor_date(
          order_purchase_timestamp, "month"),
        month_number = as.integer(
          interval(cohort_month,
                   order_month_date) %/% months(1))
      )
    
    cohort_table <- cohort_data %>%
      group_by(cohort_month, month_number) %>%
      summarise(customers = n_distinct(customer_unique_id),
                .groups = "drop")
    
    cohort_size <- cohort_table %>%
      filter(month_number == 0) %>%
      select(cohort_month, cohort_size = customers)
    
    cohort_table %>%
      left_join(cohort_size, by = "cohort_month") %>%
      mutate(retention_pct = customers / cohort_size * 100) %>%
      filter(month_number <= 12) %>%
      group_by(month_number) %>%
      summarise(avg_ret = round(mean(retention_pct), 2),
                .groups = "drop") %>%
      ggplot(aes(x = month_number, y = avg_ret)) +
      geom_line(color = "#C62828", linewidth = 1.2) +
      geom_point(color = "#C62828", size = 3) +
      geom_text(aes(label = paste0(avg_ret, "%")),
                vjust = -0.8, size = 3.5) +
      scale_x_continuous(breaks = 0:12) +
      labs(x = "Months After First Purchase",
           y = "Avg Retention %") +
      theme_minimal()
  })
  
  # ── KPI BOXES — RFM ──────────────────────────
  output$box_champions <- renderValueBox({
    n <- rfm_scored %>%
      filter(segment == "Champions") %>%
      nrow()
    valueBox(
      value    = format(n, big.mark = ","),
      subtitle = "Champions",
      icon     = icon("trophy"),
      color    = "green"
    )
  })
  
  output$box_at_risk <- renderValueBox({
    n <- rfm_scored %>%
      filter(segment == "At Risk") %>%
      nrow()
    valueBox(
      value    = format(n, big.mark = ","),
      subtitle = "At Risk Customers",
      icon     = icon("exclamation-triangle"),
      color    = "orange"
    )
  })
  
  output$box_clv_uplift <- renderValueBox({
    valueBox(
      value    = "R$ 2,077,785",
      subtitle = "CLV Uplift Potential",
      icon     = icon("arrow-up"),
      color    = "blue"
    )
  })
  
  # ── RFM PLOTS ─────────────────────────────────
  output$plot_rfm_seg <- renderPlot({
    rfm_scored %>%
      count(segment) %>%
      mutate(pct = round(n / sum(n) * 100, 1)) %>%
      ggplot(aes(x = reorder(segment, n),
                 y = n, fill = segment)) +
      geom_col() +
      geom_text(aes(label = paste0(pct, "%")),
                hjust = -0.1, size = 3) +
      coord_flip() +
      scale_fill_manual(values = seg_colors) +
      labs(x = "", y = "Customers") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$plot_clv <- renderPlot({
    dataset_years <- 1.95
    
    rfm_scored %>%
      mutate(
        avg_order_value    = total_spent / total_orders,
        frequency_per_year = total_orders / dataset_years,
        lifespan_years     = ifelse(total_orders == 1,
                                    0.5, dataset_years),
        clv = avg_order_value * frequency_per_year *
          lifespan_years
      ) %>%
      group_by(segment) %>%
      summarise(avg_clv = round(mean(clv), 2),
                .groups = "drop") %>%
      ggplot(aes(x = reorder(segment, avg_clv),
                 y = avg_clv, fill = segment)) +
      geom_col() +
      geom_text(aes(label = paste0("R$", avg_clv)),
                hjust = -0.1, size = 3) +
      coord_flip() +
      scale_fill_manual(values = seg_colors) +
      labs(x = "", y = "Avg CLV (R$)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$table_segment <- renderTable({
    rfm_scored %>%
      group_by(segment) %>%
      summarise(
        Customers     = n(),
        `Total Revenue (R$)` = round(sum(total_spent), 0),
        `Avg Revenue (R$)`   = round(mean(total_spent), 2),
        `Avg Orders`         = round(mean(total_orders), 2),
        .groups = "drop"
      ) %>%
      arrange(desc(`Total Revenue (R$)`))
  })
}

# ================================
# RUN APP
# ================================
shinyApp(ui = ui, server = server)
