# Tests for stock.R -----

# 0. synthetic data helpers -----

# Random posting intervals for two sources over a short window.
sim_intervals <- function(seed = 1L, n = 400L, max_dur = 40L) {
  set.seed(seed)
  start <- data.table::as.IDate("2024-01-01") +
    sample(0:89, n, replace = TRUE)
  data.table::data.table(
    source = sample(c("s1", "s2"), n, replace = TRUE),
    start = start,
    end = start + sample(0:max_dur, n, replace = TRUE),
    n = sample(1:3, n, replace = TRUE)
  )
}

# Brute-force stock: for each source and day, sum of n over intervals with
# start <= d <= end.
brute_stock <- function(iv, dates) {
  out <- data.table::CJ(source = sort(unique(iv$source)), date = dates)
  out[,
    stock := vapply(
      seq_len(.N),
      function(i) {
        sel <- iv$source == source[i] & iv$start <= date[i] & iv$end >= date[i]
        sum(iv$n[sel])
      },
      integer(1)
    )
  ]
  out[]
}

# Constant daily inflow with every posting lasting `dur` days after start.
sim_constant_duration <- function(seed = 2L, n_days = 400L, dur = 120L) {
  set.seed(seed)
  d0 <- data.table::as.IDate("2023-01-02")
  data.table::data.table(
    start = d0 + 0:(n_days - 1L),
    n = stats::rpois(n_days, 50)
  )[, end := start + dur][]
}

# 1. events_from_intervals -----

test_that("events_from_intervals puts inflow at start and outflow at end + 1", {
  iv <- data.table::data.table(
    start = data.table::as.IDate(c("2024-01-01", "2024-01-01", "2024-01-03")),
    end = data.table::as.IDate(c("2024-01-02", "2024-01-03", "2024-01-03"))
  )
  ev <- events_from_intervals(iv, start_col = "start", end_col = "end")
  expect_identical(
    ev$date,
    data.table::as.IDate(c("2024-01-01", "2024-01-03", "2024-01-04"))
  )
  expect_identical(ev$inflow, c(2L, 1L, 0L))
  expect_identical(ev$outflow, c(0L, 1L, 2L))
  expect_identical(sum(ev$inflow), sum(ev$outflow))
})

test_that("events_from_intervals uses weights and groups", {
  iv <- sim_intervals()
  ev <- events_from_intervals(
    iv,
    by = "source",
    start_col = "start",
    end_col = "end",
    n_col = "n"
  )
  expect_identical(names(ev), c("source", "date", "inflow", "outflow"))
  expect_identical(
    ev[, sum(inflow), keyby = source]$V1,
    iv[, sum(n), keyby = source]$V1
  )
  expect_identical(anyDuplicated(ev, by = c("source", "date")), 0L)
})

# 2. stock_from_events -----

test_that("stock_from_events equals the brute-force expansion of intervals", {
  for (seed in 1:3) {
    iv <- sim_intervals(seed = seed)
    ev <- events_from_intervals(
      iv,
      by = "source",
      start_col = "start",
      end_col = "end",
      n_col = "n"
    )
    st <- stock_from_events(ev, by = "source")
    dates <- seq(min(ev$date), max(ev$date), by = 1L)
    bf <- brute_stock(iv, dates)
    expect_identical(st$source, bf$source)
    expect_identical(st$date, bf$date)
    expect_identical(st$stock, bf$stock)
    # last day: every posting has left
    expect_identical(st[, data.table::last(stock), by = source]$V1, c(0L, 0L))
  }
})

test_that("stock_from_events fills missing days and honours from/to", {
  ev <- data.table::data.table(
    date = data.table::as.IDate(c("2024-01-01", "2024-01-05")),
    inflow = c(3L, 0L),
    outflow = c(0L, 2L)
  )
  st <- stock_from_events(ev)
  expect_identical(nrow(st), 5L)
  expect_identical(st$stock, c(3L, 3L, 3L, 3L, 1L))
  expect_identical(st$inflow, c(3L, 0L, 0L, 0L, 0L))

  # events before `from` are carried into the stock; `to` extends the grid
  st2 <- stock_from_events(ev, from = "2024-01-03", to = "2024-01-07")
  expect_identical(
    st2$date,
    data.table::as.IDate("2024-01-03") + 0:4
  )
  expect_identical(st2$stock, c(3L, 3L, 1L, 1L, 1L))

  # Date input is accepted and returned as IDate
  ev_date <- data.table::copy(ev)[, date := as.Date(date)]
  expect_identical(stock_from_events(ev_date)$stock, st$stock)
  expect_s3_class(stock_from_events(ev_date)$date, "IDate")
})

test_that("stock_from_events with 120-day durations is a 121-day rolling sum", {
  iv <- sim_constant_duration(dur = 120L)
  ev <- events_from_intervals(
    iv,
    start_col = "start",
    end_col = "end",
    n_col = "n"
  )
  st <- stock_from_events(ev, to = max(iv$start))
  roll <- data.table::frollsum(st$inflow, 121L)
  warm <- 121:nrow(st)
  expect_identical(st$stock[warm], as.integer(roll[warm]))
  # during the warm-up the stock is the cumulative inflow
  expect_identical(st$stock[1:120], cumsum(st$inflow)[1:120])
})

# 3. duration_survival -----

test_that("duration_survival returns P(duration >= j)", {
  d <- data.table::data.table(
    source = c("a", "a", "a", "b"),
    duration = c(0L, 2L, 5L, 3L),
    n = c(10L, 30L, 60L, 4L)
  )
  sv <- duration_survival(d, by = "source")
  expect_identical(names(sv), c("source", "j", "surv"))
  expect_identical(sv[source == "a", j], 0:5)
  expect_identical(sv[source == "b", j], 0:5)
  expect_equal(
    sv[source == "a", surv],
    c(1, 0.9, 0.9, 0.6, 0.6, 0.6),
    tolerance = 1e-12
  )
  expect_equal(
    sv[source == "b", surv],
    c(1, 1, 1, 1, 0, 0),
    tolerance = 1e-12
  )
  expect_identical(sv[j == 0L, surv], c(1, 1))
  expect_true(all(sv[, all(diff(surv) <= 0), by = source]$V1))
})

test_that("duration_survival matches the empirical tail on random data", {
  set.seed(5)
  dur <- sample(0:30, 500, replace = TRUE)
  sv <- duration_survival(
    data.table::data.table(duration = dur),
    n_col = NULL,
    max_duration = 20
  )
  expect_identical(sv$j, 0:20)
  expect_equal(
    sv$surv,
    vapply(0:20, function(k) mean(dur >= k), numeric(1)),
    tolerance = 1e-12
  )
})

# 4. standardized_stock -----

test_that("standardized_stock equals the stock under constant durations", {
  iv <- sim_constant_duration(dur = 120L)
  iv[, duration := as.integer(end - start)]
  ev <- events_from_intervals(
    iv,
    start_col = "start",
    end_col = "end",
    n_col = "n"
  )
  st <- stock_from_events(ev, to = max(iv$start))
  sv <- duration_survival(iv[, list(duration, n)])
  ss <- standardized_stock(st, sv)
  expect_identical(ss$date, st$date)
  expect_equal(ss$stock_std, as.numeric(st$stock), tolerance = 1e-10)
  expect_identical(ss$complete, seq_len(nrow(ss)) >= 121L)
})

test_that("standardized_stock does not jump with a duration regime change", {
  d0 <- data.table::as.IDate("2023-01-02")
  n_days <- 500L
  break_day <- 250L
  iv <- data.table::data.table(start = d0 + 0:(n_days - 1L), n = 20L)
  iv[,
    duration := data.table::fifelse(
      seq_len(.N) <= break_day,
      30L,
      120L
    )
  ]
  iv[, end := start + duration]
  ev <- events_from_intervals(
    iv,
    start_col = "start",
    end_col = "end",
    n_col = "n"
  )
  st <- stock_from_events(ev, to = max(iv$start))

  # fixed G from the base period (first regime)
  sv <- duration_survival(iv[seq_len(break_day), list(duration, n)])
  ss <- standardized_stock(st, sv)
  ok <- ss$complete

  # S* = 20 * sum(G) on every complete day: flat across the break
  expect_equal(
    ss$stock_std[ok],
    rep(20 * sum(sv$surv), sum(ok)),
    tolerance = 1e-10
  )
  # proportional to the G-weighted rolling sum of inflow
  roll_w <- stats::filter(st$inflow, rev(sv$surv), sides = 1L)
  expect_equal(ss$stock_std[ok], as.numeric(roll_w)[ok], tolerance = 1e-10)

  # the raw stock moves from 31 * 20 to 121 * 20
  before <- st$stock[100:break_day]
  after <- st$stock[(n_days - 50L):n_days]
  expect_identical(unique(before), 31L * 20L)
  expect_identical(unique(after), 121L * 20L)
})

test_that("standardized_stock applies curves per group and partial by", {
  flow <- data.table::CJ(
    source = c("a", "b"),
    skill = c("k1", "k2"),
    date = data.table::as.IDate("2024-01-01") + 0:9
  )
  flow[, inflow := rep(1:4, each = 10)]
  sv <- data.table::data.table(
    source = c("a", "a", "b", "b", "b"),
    j = c(0L, 1L, 0L, 1L, 2L),
    surv = c(1, 0.5, 1, 1, 1)
  )
  ss <- standardized_stock(flow, sv, by = c("source", "skill"))
  expect_identical(
    names(ss),
    c("source", "skill", "date", "stock_std", "complete")
  )
  expect_equal(ss[source == "a" & skill == "k1", stock_std], c(1, rep(1.5, 9)))
  expect_equal(
    ss[source == "b" & skill == "k2", stock_std],
    c(4, 8, rep(12, 8))
  )
  expect_identical(
    ss[source == "a" & skill == "k1", complete],
    c(FALSE, rep(TRUE, 9))
  )
  expect_identical(
    ss[source == "b" & skill == "k1", complete],
    c(FALSE, FALSE, rep(TRUE, 8))
  )

  # missing rows count as zero, NA propagates over the window
  fl <- data.table::data.table(
    date = data.table::as.IDate("2024-01-01") + c(0:3, 5:6),
    inflow = c(2, 2, NA, 2, 2, 2)
  )
  g <- data.table::data.table(j = 0:1, surv = c(1, 1))
  out <- standardized_stock(fl, g)
  expect_identical(nrow(out), 7L)
  expect_equal(out$stock_std, c(2, 4, NA, NA, 2, 2, 4))
})

# 5. to_weekly -----

test_that("to_weekly aggregates on Monday weeks", {
  # 2024-01-01 is a Monday
  d <- data.table::data.table(
    source = rep(c("a", "b"), each = 17),
    date = rep(data.table::as.IDate("2024-01-03") + 0:16, 2),
    stock = c(1:17, 101:117),
    inflow = rep(1L, 34)
  )
  w <- to_weekly(d, by = "source", mean_cols = "stock", sum_cols = "inflow")
  expect_identical(names(w), c("source", "week", "n_days", "stock", "inflow"))
  expect_identical(
    w$week,
    rep(data.table::as.IDate(c("2024-01-01", "2024-01-08", "2024-01-15")), 2)
  )
  expect_true(all(data.table::wday(w$week) == 2L))
  expect_identical(w$n_days, rep(c(5L, 7L, 5L), 2))
  expect_identical(w$inflow, rep(c(5L, 7L, 5L), 2))
  expect_equal(
    w$stock,
    c(
      mean(1:5),
      mean(6:12),
      mean(13:17),
      mean(101:105),
      mean(106:112),
      mean(113:117)
    ),
    tolerance = 1e-12
  )

  # Sunday weeks
  w7 <- to_weekly(d[source == "a"], sum_cols = "inflow", week_start = 7L)
  expect_true(all(data.table::wday(w7$week) == 1L))
  expect_identical(sum(w7$inflow), 17L)
  expect_identical(w7$week[1], data.table::as.IDate("2023-12-31"))
})

# 6. error handling -----

test_that("events_from_intervals validates its input", {
  iv <- data.table::data.table(
    start = data.table::as.IDate("2024-01-05"),
    end = data.table::as.IDate("2024-01-01")
  )
  expect_error(
    events_from_intervals(iv, start_col = "start", end_col = "end"),
    "end before they start"
  )
  expect_error(
    events_from_intervals(iv, start_col = "start", end_col = "fine"),
    "missing required columns: fine"
  )
  expect_error(
    events_from_intervals(
      data.table::data.table(start = "2024-01-01", end = "2024-01-02"),
      start_col = "start",
      end_col = "end"
    ),
    "class Date or IDate"
  )
})

test_that("stock_from_events validates its input", {
  ev <- data.table::data.table(
    date = data.table::as.IDate(c("2024-01-01", "2024-01-02")),
    inflow = c(1L, 0L),
    outflow = c(0L, 2L)
  )
  expect_error(stock_from_events(ev), "negative stock on 2024-01-02")
  ev_g <- data.table::copy(ev)[, source := "s9"]
  expect_error(stock_from_events(ev_g, by = "source"), "source = s9")
  expect_error(
    stock_from_events(ev, in_col = "ingressi"),
    "missing required columns"
  )
  expect_error(stock_from_events(ev[0]), "no rows")
  expect_error(
    stock_from_events(data.table::copy(ev)[, inflow := -1L]),
    "non-negative"
  )
  expect_error(
    stock_from_events(ev, from = "2024-02-01", to = "2024-01-01"),
    "output window is empty"
  )
  expect_error(stock_from_events(ev, from = "not a date"), "single Date")
  expect_error(stock_from_events(list(a = 1)), "data.frame")
})

test_that("duration_survival validates its input", {
  d <- data.table::data.table(duration = c(1, 2.5), n = c(1L, 1L))
  expect_error(duration_survival(d), "whole numbers")
  expect_error(
    duration_survival(data.table::data.table(duration = -1L, n = 1L)),
    "non-negative"
  )
  expect_error(
    duration_survival(data.table::data.table(duration = 1L, n = 0L)),
    "no postings"
  )
  expect_error(
    duration_survival(data.table::data.table(duration = 1L)),
    "missing required columns: n"
  )
  expect_error(
    duration_survival(
      data.table::data.table(duration = 1L, n = 1L),
      max_duration = -2
    ),
    "max_duration"
  )
})

test_that("standardized_stock validates its input", {
  flow <- data.table::data.table(
    source = "a",
    date = data.table::as.IDate("2024-01-01") + 0:2,
    inflow = 1L
  )
  sv <- data.table::data.table(source = "b", j = 0:1, surv = c(1, 0.5))
  expect_error(
    standardized_stock(flow, sv, by = "source"),
    "no survival curve for group \\(source = a\\)"
  )
  expect_error(
    standardized_stock(flow, sv[, list(j)], by = "source"),
    "missing required columns: surv"
  )
  expect_error(
    standardized_stock(flow, data.table::data.table(j = c(0L, 2L), surv = 1)),
    "every j"
  )
  expect_error(
    standardized_stock(rbind(flow, flow), sv[, list(j, surv)]),
    "one row per"
  )
  expect_error(
    standardized_stock(flow, sv[, list(j, surv)], flow_col = "x"),
    "missing required columns: x"
  )
})

test_that("to_weekly validates its input", {
  d <- data.table::data.table(
    date = data.table::as.IDate("2024-01-01") + 0:2,
    v = 1:3,
    lab = c("a", "b", "c")
  )
  expect_error(to_weekly(d, sum_cols = "lab"), "must be numeric")
  expect_error(to_weekly(d, mean_cols = "v", sum_cols = "v"), "both")
  expect_error(to_weekly(d, sum_cols = "w"), "missing required columns: w")
  expect_error(to_weekly(d, week_start = 8L), "week_start")
  expect_error(to_weekly(rbind(d, d), sum_cols = "v"), "one row per")
})
