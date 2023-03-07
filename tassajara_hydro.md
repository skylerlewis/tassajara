Tassajara Creek H&H Calculations
================

# Hydrologics

``` r
import_streamflow_data <- function(csv) {
  return(read_csv(csv) %>%
    janitor::clean_names() %>%
    mutate(result_datetime = lubridate::mdy(result_date) + lubridate::hms(result_time),
           stage_ft = pmax(gh_probe_ft, 0),
           streamflow_cfs = case_when(streamflow_cfs >=0 ~ streamflow_cfs),
           rain_intensity_in = rain_interval_in * 4
           )
    )
}

new_years_storm <- import_streamflow_data("data/tc-bi580_2022-12-31.csv") 
```

    ## Rows: 3480 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): Result Date
    ## dbl  (8): GHBubbler(ft), GHProbe(ft), WaterTemp(C), RainInterval(in), RainDa...
    ## time (1): Result Time
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
new_years_storm %>% 
  ggplot(aes(y = streamflow_cfs, x = stage_ft)) + 
    geom_line(color = "black") + 
    scale_y_continuous(trans = 'log10', limits = c(10, 1e4)) + 
    scale_x_continuous(trans = 'log10', limits = c(1, 5), breaks = 1:4) +
    ggtitle("Rating Curve for TC_BI580 (Tassajara Creek Below I-580") 
```

    ## Warning: Transformation introduced infinite values in continuous y-axis

    ## Warning: Transformation introduced infinite values in continuous x-axis

    ## Warning: Removed 1937 rows containing missing values (`geom_line()`).

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
power_function_fit <- function(data, y, x) {
  df <- data %>% 
    select({{y}}, {{x}}) %>% 
    filter({{y}} > 0 & {{x}} > 0)
  model <- lm(log(df[[deparse(substitute(y))]]) ~ log(df[[deparse(substitute(x))]]))
  coeff <- summary(model)$coeff
  output <- c(alpha = exp(coeff[1,1]), beta = coeff[2,1])
  power_function <- function(x) {
    return(output["alpha"] * x^output["beta"])
  }
  #print(paste0(deparse(substitute(y)), " = ", output["alpha"], " * ", deparse(substitute(x)), "^", output["beta"]))
  #output <- output %>% append(c("function" = power_function))
  #return(output)
  return(power_function)
}

# version 1 of the rating curve using just the stream gauge 
rating_curve <- new_years_storm %>% power_function_fit(y = streamflow_cfs, x = stage_ft)

new_years_storm_pred <- new_years_storm %>%
  #mutate(streamflow_cfs_pred = rating_curve["alpha"] * stage_ft^rating_curve["beta"])
  mutate(streamflow_cfs_pred = rating_curve(stage_ft))

# version 2 of the rating curve using stepwise interpolation based on the I-580 HWM
peak_flow_jan_2023 <- 3657

rating_curve <- new_years_storm %>% 
  select(c(streamflow_cfs, stage_ft)) %>% 
  filter(!is.na(streamflow_cfs)) %>% 
  slice_max(stage_ft) %>%
  bind_rows(tribble(
    ~streamflow_cfs, ~stage_ft,
    peak_flow_jan_2023, max(new_years_storm$stage_ft)
  )) %>%
  power_function_fit(y = streamflow_cfs, x = stage_ft)

new_years_storm_pred <- new_years_storm %>%
  mutate(streamflow_cfs_pred = case_when(is.na(streamflow_cfs) ~ rating_curve(stage_ft), TRUE ~ streamflow_cfs),
         is_predicted = is.na(streamflow_cfs))

new_years_storm_pred %>% ggplot(aes(x = stage_ft)) + 
  geom_line(aes(y = streamflow_cfs_pred), color = "red") + 
  geom_line(aes(y = streamflow_cfs), color="black") +
  scale_y_continuous(trans = 'log10', limits = c(10, 1e4)) + 
  scale_x_continuous(trans = 'log10', limits = c(1, 8), breaks = 1:7) +
  ggtitle("Rating curve extrapolated based on January 2023 HWM")
```

    ## Warning: Transformation introduced infinite values in continuous y-axis

    ## Warning: Transformation introduced infinite values in continuous x-axis

    ## Warning: Transformation introduced infinite values in continuous y-axis

    ## Warning: Transformation introduced infinite values in continuous x-axis

    ## Warning: Removed 1868 rows containing missing values (`geom_line()`).

    ## Warning: Removed 1937 rows containing missing values (`geom_line()`).

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#new_years_storm_pred %>% ggplot(aes(x = result_datetime)) + 
#  geom_line(aes(y = streamflow_cfs_pred), color = "red") + 
#  geom_line(aes(y = streamflow_cfs), color="black")

maxRange <- 5000
coeff <- 0.0005

new_years_storm_pred %>% 
  filter(result_datetime < ymd("2023-01-23")) %>%
  ggplot(aes(x = result_datetime)) +
  geom_area(aes(y = streamflow_cfs_pred, 
                fill = "q_pred")) + 
  geom_area(aes(y = streamflow_cfs, 
                fill = "q")) +
  geom_tile(aes(y = maxRange - rain_intensity_in/coeff/2, 
                height = rain_intensity_in/coeff, 
                fill = "precip")) +
  scale_y_continuous(name = "streamflow (cfs)",
                     limit = c(0, maxRange),
                     expand = c(0, 0),
                     sec.axis = sec_axis(trans = ~(.-maxRange)*coeff, 
                                         name = "precipitation (in/hr)")) +
  theme(legend.position = 'top') + 
  ggtitle("Dec 2022 - Jan 2023 rainfall and streamflow") +
  geom_hline(yintercept = 650 , linetype = "dashed") + annotate("text", x=as.POSIXct(ymd("2023-01-21")), y=650 , label="Q2 = 650 cfs") +
  geom_hline(yintercept = 1200, linetype = "dashed") + annotate("text", x=as.POSIXct(ymd("2023-01-21")), y=1200, label="Q5 = 1200 cfs") +
  scale_fill_manual(values = c("precip" = "blue", "q" = "black", "q_pred" = "red"),
                    labels = c("precip" = "Precipitation", "q" = "Streamflow (gauged)", "q_pred" = "Streamflow (predicted)"),
                    name = NULL) 
```

    ## Warning: Removed 69 rows containing non-finite values (`stat_align()`).

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# figure out the peak flow from the December storm
peak_flow_dec_2022 <- new_years_storm_pred %>% 
  filter(date(result_datetime) == ymd("2022-12-31")) %>% 
  pull(streamflow_cfs_pred) %>% 
  max()
print(peak_flow_dec_2022)
```

    ## [1] 3464.893

``` r
# export the hydrograph for RAS 2D
min_flow <- 100

hydro_dec <- new_years_storm_pred %>% 
  filter(between(result_datetime, ymd("2022-12-31"), ymd("2023-01-02"))) 
ras_hydro_dec <- hydro_dec %>%
  mutate(cfs = case_when(streamflow_cfs_pred > min_flow ~ streamflow_cfs_pred, TRUE ~ min_flow)) %>%
  select(result_datetime, cfs) 

ras_hydro_dec %>% write_csv("data/ras_hydro_2022-12-31.csv")
ggplot() + geom_line(data = ras_hydro_dec, aes(x = result_datetime, y = cfs))
```

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
hydro_jan <- new_years_storm_pred %>% 
  filter(between(result_datetime, ymd("2023-01-14"), ymd("2023-01-18")))
ras_hydro_jan <- hydro_jan %>%
  mutate(cfs = case_when(streamflow_cfs_pred > min_flow ~ streamflow_cfs_pred, TRUE ~ min_flow)) %>%
  select(result_datetime, cfs) 

ras_hydro_jan %>% write_csv("data/ras_hydro_2023-01-14.csv")
ggplot() + geom_line(data = ras_hydro_jan, aes(x = result_datetime, y = cfs)) 
```

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
# scale December hydrograph to 5200 cfs 100-year flow, 
# as a simple approximation of the 100-year storm

multiplier <- 5200 / max(ras_hydro_dec$cfs)

ras_hydro_q100 <- hydro_dec %>% 
  mutate(cfs = (cfs = case_when(streamflow_cfs_pred * multiplier > min_flow ~ streamflow_cfs_pred * multiplier, TRUE ~ min_flow))) %>%
  select(result_datetime, cfs)

ras_hydro_q100 %>% write_csv("data/ras_hydro_q100.csv")
ggplot() + geom_line(data = ras_hydro_q100, aes(x = result_datetime, y = cfs))
```

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggplot() + 
  geom_line(data = ras_hydro_q100, aes(x = result_datetime, y = cfs, color = "4. Scaled for Q100(AC)")) +
  geom_line(data = ras_hydro_dec, aes(x = result_datetime, y = cfs, color = "3. Modified for US BC")) + 
  geom_line(data = new_years_storm_pred %>% filter(result_datetime >= ymd("2022-12-31") & result_datetime < ymd("2023-01-02")), 
            aes(x = result_datetime, y = streamflow_cfs_pred, color = "2. Extrapolated")) +
  geom_line(data = new_years_storm_pred %>% filter(result_datetime >= ymd("2022-12-31") & result_datetime < ymd("2023-01-02")), 
            aes(x = result_datetime, y = streamflow_cfs, color = "1. Raw")) + 
  scale_color_manual(values = c("black", "red", "purple", "orange")) + 
  scale_size_manual(values = c(2, 1, 2, 1)) + 
  scale_linetype_manual(values = c("solid", "dashed", "dashed", "dashed")) + 
  theme(legend.position = "top")
```

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

# Flow Frequency

``` r
peak_flows <- tribble(
  ~peak_flow_date, ~discharge_cfs,
  ymd("2019-02-13"), 560,
  ymd("2022-11-11"), 105,
  ymd("2022-12-31"), peak_flow_dec_2022,
  ymd("2023-01-14"), peak_flow_jan_2023
)

flow_freq <- tribble(
  ~use, ~name, ~return_interval_y, ~discharge_cfs,
  T, "Q2 BKF",     2,  650, # 18.4 m3/s per MacWilliams et al 2008
  T, "Q25 BKF",   25, 2048, #  58 m3/s per MacWilliams et al 2008
  T, "Q100 BKF", 100, 4300, # 121.8 m3/s per MacWilliams et al 2008
  F, "Q5 Channel Capacity BKF",     5, 1200, #  34 m3/s per MacWilliams et al 2008
  F, "Q100 Alameda County", 100, 5200, # AlCo estimate from Chan and Heard
)

flow_freq_curve <- flow_freq %>% filter(use) %>% 
  power_function_fit(y = discharge_cfs, x = return_interval_y)

flow_freq_curve_inv <- flow_freq %>% filter(use) %>% 
  power_function_fit(y = return_interval_y, x = discharge_cfs)

flow_freq_pred <- flow_freq %>%
  mutate(return_interval_y_pred = flow_freq_curve_inv(discharge_cfs))

peak_flows_pred <- peak_flows %>%
  mutate(return_interval_y_pred = flow_freq_curve_inv(discharge_cfs))

ggplot() + 
  geom_point(data = flow_freq, aes(y = discharge_cfs, x = return_interval_y)) + 
  geom_point(data = peak_flows_pred, aes(y = discharge_cfs, x = return_interval_y_pred), color = "red") + 
  geom_line(data = flow_freq_pred, aes(y = discharge_cfs, x = return_interval_y_pred)) + 
  scale_y_continuous(trans='log10') + 
  scale_x_continuous(trans='log10') + 
  ggtitle("Flow frequency for model profiles") +
  ggrepel::geom_text_repel(data = flow_freq, aes(y = discharge_cfs, x = return_interval_y, label = name)) + 
  ggrepel::geom_text_repel(data = peak_flows_pred, aes(y = discharge_cfs, x = return_interval_y_pred, label = as.character(peak_flow_date)), color = "red") 
```

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
#peak_flows %>% fasstr::compute_frequency_analysis()
```

# Hydraulics

``` r
stations <- tribble(
  ~cross_section, ~station_1d, ~station_2d,
   NA,    NA,    0, # US boundary
   NA, 11300,   26, # gleason apron toe
  "B", 10800,  490,    
   NA, 10400,   NA, # pedestrian path
  "D", 10200, 1210,     
  "E",  9300, 2061,     
  "F",  8800, 2568,    
  "G",  8200, 3157,    
  "H",  7600, 3828,  
   NA,  6900,   NA, # 580 frontage apron head
   NA,  6800,   NA, # 580 underpass
   NA,    NA, 4507  # DS boundary
)

cross_sections <- stations %>% drop_na(cross_section)
```

Constants

``` r
# gravitational constant, cm/s2
g_cgs <- 981
# grain density and water density, g/cm3
rho_s_cgs <- 2.65
rho_cgs <- 1.00
# kinematic viscosity of water, cm2/s
nu_cgs <- 0.01
```

Helper functions for cross sectional geometry calculations

``` r
prep_xs <- function(data, sta, elev, delta_x) {
  data %>%
    arrange({{sta}}) %>%
    mutate(sta = {{sta}}) %>%
    complete(sta = seq(from = min({{sta}}), to = max({{sta}}), by = delta_x)) %>%
    mutate(gse = zoo::na.approx({{elev}}, x = sta)) %>% 
    select(c(sta, gse))
}

calc_xs <- function(data, water_elev) {
  data %>% 
    arrange(sta) %>%
    mutate(wse = case_when(water_elev > gse ~ water_elev),
           delta_x = abs(lag(sta, 1) - sta),
           delta_z = abs(lag(gse, 1) - gse),
           depth = wse - gse, 
           hyp_length = sqrt(delta_x^2 + delta_z^2)
           ) %>%
    filter(!is.na(wse)) %>% 
    summarize(thalweg_elevation = min(gse),
              water_surface_elevation = water_elev,
              max_depth = water_surface_elevation - thalweg_elevation,
              cross_sectional_area = sum(delta_x * depth),
              wetted_perimeter = sum(hyp_length),
              ) %>% 
    as.list() %>% 
    list_flatten()
}

plot_xs <- function(data, water_elev) {
  data %>% 
    arrange(sta) %>%
    mutate(wse = case_when(water_elev > gse ~ water_elev)) %>%
    ggplot(aes(x = sta)) + 
      geom_line(aes(y = gse)) + 
      geom_line(aes(y = wse))
}
```

Import surveyed cross section geometries

``` r
xs_b <- read_csv("data/xs_geom/xs_b.csv") %>%
  prep_xs(sta = station_left_bank_ft, elev = elevation_ngvd29_ft, delta_x = 0.1) 

xs_d <- read_csv("data/xs_geom/xs_d.csv") %>%
  prep_xs(sta = station_left_bank_ft, elev = elevation_ngvd29_ft, delta_x = 0.1) 

xs_e <- read_csv("data/xs_geom/xs_e.csv") %>%
  prep_xs(sta = station_left_bank_ft, elev = elevation_ngvd29_ft, delta_x = 0.1) 

xs_f <- read_csv("data/xs_geom/xs_f.csv") %>%
  prep_xs(sta = station_left_bank_ft, elev = elevation_ngvd29_ft, delta_x = 0.1) 

xs_g <- read_csv("data/xs_geom/xs_g.csv") %>%
  prep_xs(sta = station_left_bank_ft, elev = elevation_ngvd29_ft, delta_x = 0.1) 

xs_h <- read_csv("data/xs_geom/xs_h.csv") %>%
  prep_xs(sta = station_left_bank_ft, elev = elevation_ngvd29_ft, delta_x = 0.1) 

xs_dfs <- tribble(
  ~xs_id, ~xs_df,
  "B", xs_b,
  "D", xs_d,
  "E", xs_e,
  "F", xs_f,
  "G", xs_g,
  "H", xs_h, 
)

xs_dfs %>% 
  unnest() %>% 
  ggplot(aes(y = gse, x = sta)) + 
    facet_wrap(~xs_id) + 
    geom_line() + 
    ggtitle("Cross section geometries")
```

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Example plot with a water surface intersected

``` r
plot_xs(xs_b, 359)
```

    ## Warning: Removed 1421 rows containing missing values (`geom_line()`).

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Calculate hydraulic geometries under HWMs (hydraulic radius, etc)

``` r
hwm_geometries <- read_csv("data/high_water_marks_v2.csv") %>%
  janitor::clean_names() %>%
  left_join(xs_dfs, by = join_by(cross_section == xs_id)) %>%
  mutate(result = map2(xs_df, hwm_elevation, calc_xs)) %>% 
  unnest_wider(result)
```

    ## Rows: 16 Columns: 5
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): series, cross section
    ## dbl  (2): hwm_elevation, slope
    ## date (1): peak flow date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
hwm_geometries
```

    ## # A tibble: 16 × 11
    ##    series     cross…¹ peak_flo…² hwm_e…³  slope xs_df    thalw…⁴ water…⁵ max_d…⁶
    ##    <chr>      <chr>   <date>       <dbl>  <dbl> <list>     <dbl>   <dbl>   <dbl>
    ##  1 channel    B       2019-02-13    356. 0.012  <tibble>    352.    356.    4.14
    ##  2 channel    D       2019-02-13    353. 0.005  <tibble>    348.    353.    5.51
    ##  3 channel    E       2019-02-13    352. 0.003  <tibble>    344.    352.    7.44
    ##  4 channel    F       2022-11-11    346. 0.003  <tibble>    343.    346.    3.18
    ##  5 channel    G       2022-11-11    344. 0.003  <tibble>    342.    344.    2.70
    ##  6 channel    H       2019-02-13    343. 0.003  <tibble>    339.    343.    3.81
    ##  7 floodplain B       2022-12-31    365. 0.0076 <tibble>    352.    365.   13.3 
    ##  8 floodplain D       2022-12-31    361. 0.0019 <tibble>    348.    361.   13.3 
    ##  9 floodplain E       2022-12-31    356. 0.0056 <tibble>    344.    356.   11.8 
    ## 10 floodplain E       2023-01-14    357. 0.0063 <tibble>    344.    357.   12.6 
    ## 11 floodplain F       2022-12-31    353. 0.0073 <tibble>    343.    353.   10.3 
    ## 12 floodplain F       2023-01-14    354. 0.0075 <tibble>    343.    354.   10.7 
    ## 13 floodplain G       2022-12-31    350. 0.008  <tibble>    342.    350.    8.45
    ## 14 floodplain G       2023-01-14    351. 0.0077 <tibble>    342.    351.    8.96
    ## 15 floodplain H       2022-12-31    347. 0.0023 <tibble>    339.    347.    7.63
    ## 16 floodplain H       2023-01-14    347. 0.0019 <tibble>    339.    347.    8.00
    ## # … with 2 more variables: cross_sectional_area <dbl>, wetted_perimeter <dbl>,
    ## #   and abbreviated variable names ¹​cross_section, ²​peak_flow_date,
    ## #   ³​hwm_elevation, ⁴​thalweg_elevation, ⁵​water_surface_elevation, ⁶​max_depth

``` r
calc_thalweg_elevation <- function(data) {
  min(data$gse)
}
thalweg_elevations <- xs_dfs %>% 
  mutate(thalweg_elevation = map(xs_df, calc_thalweg_elevation)) %>% 
  unnest(thalweg_elevation) %>%
  select(xs_id, thalweg_elevation)
```

Calculate hydraulics based on hwm geometries and peak flows

``` r
hwm_hydraulics <- hwm_geometries %>% 
  rename(cross_sectional_area_ft2 = cross_sectional_area,
         wetted_perimeter_ft = wetted_perimeter) %>%
  left_join(peak_flows) %>%
  mutate(velocity_ft_s = discharge_cfs / cross_sectional_area_ft2,
         hydraulic_radius_ft = cross_sectional_area_ft2 / wetted_perimeter_ft,
         mannings_n = 1.486 * cross_sectional_area_ft2 * hydraulic_radius_ft^(2/3) 
                      * slope^(1/2) * discharge_cfs^(-1),
         # metric conversions
         velocity_m_s = velocity_ft_s / 0.3048,
         hydraulic_radius_m = hydraulic_radius_ft / 0.3048,
         cross_sectional_area_m2 = cross_sectional_area_ft2 / 0.3048,
         # bed mobilization
         critical_shields_number = 0.15 * slope^(1/4),
         grain_size_mobilized_mm = 10 * rho_cgs * hydraulic_radius_m * slope / 
                         (critical_shields_number * (rho_s_cgs - rho_cgs)),
         grain_size_mobilized_phi = -log2(grain_size_mobilized_mm),
         # suspended transport
         shear_velocity_cm_s = sqrt(g_cgs * (hydraulic_radius_m*100) * slope),
         settling_velocity_ndim = rho_cgs * shear_velocity_cm_s^3 / 
                         ((rho_s_cgs - rho_cgs) * g_cgs * nu_cgs),
         grain_size_suspended_ndim = sqrt(5832 * settling_velocity_ndim),
         grain_size_suspended_mm = 10 * grain_size_suspended_ndim * rho_cgs * nu_cgs^2 /
                         ((rho_s_cgs - rho_cgs) * g_cgs)^(1/3),
         grain_size_suspended_phi = -log2(grain_size_suspended_mm)
  )
```

    ## Joining with `by = join_by(peak_flow_date)`

``` r
hwm_hydraulics
```

    ## # A tibble: 16 × 26
    ##    series     cross…¹ peak_flo…² hwm_e…³  slope xs_df    thalw…⁴ water…⁵ max_d…⁶
    ##    <chr>      <chr>   <date>       <dbl>  <dbl> <list>     <dbl>   <dbl>   <dbl>
    ##  1 channel    B       2019-02-13    356. 0.012  <tibble>    352.    356.    4.14
    ##  2 channel    D       2019-02-13    353. 0.005  <tibble>    348.    353.    5.51
    ##  3 channel    E       2019-02-13    352. 0.003  <tibble>    344.    352.    7.44
    ##  4 channel    F       2022-11-11    346. 0.003  <tibble>    343.    346.    3.18
    ##  5 channel    G       2022-11-11    344. 0.003  <tibble>    342.    344.    2.70
    ##  6 channel    H       2019-02-13    343. 0.003  <tibble>    339.    343.    3.81
    ##  7 floodplain B       2022-12-31    365. 0.0076 <tibble>    352.    365.   13.3 
    ##  8 floodplain D       2022-12-31    361. 0.0019 <tibble>    348.    361.   13.3 
    ##  9 floodplain E       2022-12-31    356. 0.0056 <tibble>    344.    356.   11.8 
    ## 10 floodplain E       2023-01-14    357. 0.0063 <tibble>    344.    357.   12.6 
    ## 11 floodplain F       2022-12-31    353. 0.0073 <tibble>    343.    353.   10.3 
    ## 12 floodplain F       2023-01-14    354. 0.0075 <tibble>    343.    354.   10.7 
    ## 13 floodplain G       2022-12-31    350. 0.008  <tibble>    342.    350.    8.45
    ## 14 floodplain G       2023-01-14    351. 0.0077 <tibble>    342.    351.    8.96
    ## 15 floodplain H       2022-12-31    347. 0.0023 <tibble>    339.    347.    7.63
    ## 16 floodplain H       2023-01-14    347. 0.0019 <tibble>    339.    347.    8.00
    ## # … with 17 more variables: cross_sectional_area_ft2 <dbl>,
    ## #   wetted_perimeter_ft <dbl>, discharge_cfs <dbl>, velocity_ft_s <dbl>,
    ## #   hydraulic_radius_ft <dbl>, mannings_n <dbl>, velocity_m_s <dbl>,
    ## #   hydraulic_radius_m <dbl>, cross_sectional_area_m2 <dbl>,
    ## #   critical_shields_number <dbl>, grain_size_mobilized_mm <dbl>,
    ## #   grain_size_mobilized_phi <dbl>, shear_velocity_cm_s <dbl>,
    ## #   settling_velocity_ndim <dbl>, grain_size_suspended_ndim <dbl>, …

Bankfull estimates (need to fix flow freq calc though)

``` r
#bankfull <- tribble(
#    ~cross_section, ~bankfull_wse, 
#    "B", 359.08,
#    "D", 357.44,
#    "E", 352.73,
#    "F", 350.28,
#    "G", 346.27,
#    "H", 343.16  ) %>%
#  left_join(xs_dfs, by = join_by(cross_section == xs_id)) %>%
#  mutate(result = map2(xs_df, bankfull_wse, calc_xs)) %>% 
#  unnest_wider(result)
#
#bankfull %>% 
#  rename(cross_sectional_area_ft2 = cross_sectional_area,
#         wetted_perimeter_ft = wetted_perimeter) %>%
#  left_join(cross_sections) %>%
#  left_join(hwm_hydraulics %>% 
#              filter(series== "channel") %>% 
#              select(cross_section, slope, mannings_n)) %>%
#  mutate(hydraulic_radius_ft = cross_sectional_area_ft2 / wetted_perimeter_ft,
#         bankfull_discharge = 1.486 * cross_sectional_area_ft2 * hydraulic_radius_ft^(2/3) * slope^(1/2) * mannings_n^(-1),
#         bankfull_return_interval = flow_freq_curve_inv(bankfull_discharge))
```

View RAS 1D results

``` r
ras_1d <- read_csv("data/hec_ras_1d_out_v2.csv") %>%
  janitor::clean_names() %>% 
  left_join(cross_sections, by = join_by(river_sta == station_1d)) %>%
  mutate(peak_flow_date = dmy(profile))
```

    ## Rows: 80 Columns: 18
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): Reach, Profile
    ## dbl (16): River Sta, Q Total (cfs), Q Channel (cfs), Q Left (cfs), Q Right (...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `peak_flow_date = dmy(profile)`.
    ## Caused by warning:
    ## !  32 failed to parse.

``` r
ras_1d_xs <- ras_1d %>% 
  drop_na(cross_section) #%>% 
  #left_join(capacity) %>%
  #mutate(freeboard = capacity_wse - w_s_elev_ft)

ras_1d_pivot <- bind_rows(
  ras_1d_xs %>% pivot_longer(starts_with("q_")) %>% mutate(measure = "discharge_cfs", 
                                                        loc = case_when(name == "q_total_cfs" ~ "total",
                                                                        name == "q_channel_cfs" ~ "channel",
                                                                        name == "q_left_cfs" ~ "left",
                                                                        name == "q_right_cfs" ~ "right"
                                                                        )),
  ras_1d_xs %>% pivot_longer(starts_with("area_")) %>% mutate(measure = "cross_sectional_area_ft2",
                                                           loc = case_when(name == "area_sq_ft" ~ "total",
                                                                           name == "area_channel_sq_ft" ~ "channel",
                                                                           name == "area_left_sq_ft" ~ "left",
                                                                           name == "area_right_sq_ft" ~ "right"
                                                                        )),
  ras_1d_xs %>% pivot_longer(starts_with("hydr_")) %>% mutate(measure = "hydraulic_radius_ft", 
                                                           loc = case_when(name == "hydr_radius_ft" ~ "total",
                                                                           name == "hydr_radius_c_ft" ~ "channel",
                                                                           name == "hydr_radius_l_ft" ~ "left",
                                                                           name == "hydr_radius_r_ft" ~ "right"
                                                                        ))) %>% 
  select(reach, river_sta, profile, measure, loc, value) %>% 
  pivot_wider(names_from = measure, values_from = value) %>%
  left_join(ras_1d %>% select(cross_section, river_sta, profile, min_ch_el_ft, w_s_elev_ft, e_g_slope_ft_ft, q_total_cfs), by = c("river_sta", "profile"))   

ras_1d_sed <- ras_1d_pivot %>% 
  drop_na(discharge_cfs) %>%
  mutate(series = case_when((loc == "left" | loc == "right") ~ "floodplain", TRUE ~ loc),
                        velocity_ft_s = discharge_cfs / cross_sectional_area_ft2,
                        slope = e_g_slope_ft_ft, # use this for now
                       # metric conversions
                       velocity_m_s = velocity_ft_s / 0.3048,
                       hydraulic_radius_m = hydraulic_radius_ft / 0.3048,
                       cross_sectional_area_m2 = cross_sectional_area_ft2 / 0.3048,
                       # bed mobilization
                       critical_shields_number = 0.15 * slope^(1/4),
                       grain_size_mobilized_mm = 10 * rho_cgs * hydraulic_radius_m * slope / 
                                       (critical_shields_number * (rho_s_cgs - rho_cgs)),
                       grain_size_mobilized_phi = -log2(grain_size_mobilized_mm),
                       # suspended transport
                       shear_velocity_cm_s = sqrt(g_cgs * (hydraulic_radius_m*100) * slope),
                       settling_velocity_ndim = rho_cgs * shear_velocity_cm_s^3 / 
                                       ((rho_s_cgs - rho_cgs) * g_cgs * nu_cgs),
                       grain_size_suspended_ndim = sqrt(5832 * settling_velocity_ndim),
                       grain_size_suspended_mm = 10 * grain_size_suspended_ndim * rho_cgs * nu_cgs^2 /
                                       ((rho_s_cgs - rho_cgs) * g_cgs)^(1/3),
                       grain_size_suspended_phi = -log2(grain_size_suspended_mm)
              )

ras_1d_sed
```

    ## # A tibble: 181 × 26
    ##    reach   river…¹ profile loc   disch…² cross…³ hydra…⁴ cross…⁵ min_c…⁶ w_s_e…⁷
    ##    <chr>     <dbl> <chr>   <chr>   <dbl>   <dbl>   <dbl> <chr>     <dbl>   <dbl>
    ##  1 Gleaso…   10800 Q2      total  650     107.      2.82 B          352.    357.
    ##  2 Gleaso…   10800 Q2      chan…  650     107.      2.82 B          352.    357.
    ##  3 Gleaso…   10800 Q5      total 1200     179.      3.73 B          352.    359.
    ##  4 Gleaso…   10800 Q5      chan… 1200.    179.      3.9  B          352.    359.
    ##  5 Gleaso…   10800 Q5      right    0.03    0.16    0.08 B          352.    359.
    ##  6 Gleaso…   10800 Q100 F… total 4300     404.      3.83 B          352.    362.
    ##  7 Gleaso…   10800 Q100 F… chan… 4111.    314.      5.64 B          352.    362.
    ##  8 Gleaso…   10800 Q100 F… left     0.43    1.06    0.17 B          352.    362.
    ##  9 Gleaso…   10800 Q100 F… right  188.     89.4     2.04 B          352.    362.
    ## 10 Gleaso…   10800 Q100 AC total 5200     495.      4    B          352.    363.
    ## # … with 171 more rows, 16 more variables: e_g_slope_ft_ft <dbl>,
    ## #   q_total_cfs <dbl>, series <chr>, velocity_ft_s <dbl>, slope <dbl>,
    ## #   velocity_m_s <dbl>, hydraulic_radius_m <dbl>,
    ## #   cross_sectional_area_m2 <dbl>, critical_shields_number <dbl>,
    ## #   grain_size_mobilized_mm <dbl>, grain_size_mobilized_phi <dbl>,
    ## #   shear_velocity_cm_s <dbl>, settling_velocity_ndim <dbl>,
    ## #   grain_size_suspended_ndim <dbl>, grain_size_suspended_mm <dbl>, …

Manning’s N esitmates

``` r
chan_heard_2006_n = tribble(
  ~cross_section, ~mannings_n,
  "B", 0.0865,
  "D", 0.0368,
  "E", 0.0687,
  "F", 0.0705,
  "G", 0.0520,
  "H", 0.0380
) %>% mutate(series = "floodplain (2006)")

mannings_n <- hwm_hydraulics %>% 
  group_by(series, cross_section) %>%
  summarize(mannings_n = n() / sum(1 / mannings_n)) %>% # harmonic mean
  bind_rows(chan_heard_2006_n)
```

    ## `summarise()` has grouped output by 'series'. You can override using the
    ## `.groups` argument.

``` r
mannings_n %>% 
  ggplot(aes(x = cross_section, y = mannings_n, color = series, label = round(mannings_n,3))) + 
  #geom_point(aes(group = series)) +
  geom_point() +
  ggrepel::geom_text_repel() +
  scale_y_continuous(breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.09, 0.12), limits = c(0, 0.13), expand=c(0,0)) +
  scale_color_manual(values = c("channel" = "black", "floodplain" = "red", "floodplain (2006)" = "pink")) + 
  ggtitle("Manning's roughness estimates based on high water marks") + 
  geom_hline(yintercept = 0.04, color = "black", linetype = "dashed") +
  geom_hline(yintercept = 0.06, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 0.12, color = "red", linetype = "dashed") 
```

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
# add chan and heard estimates
# add comparison with design estimates
```

Bankfull discharge estimates

``` r
bankfull <- tribble(
    ~cross_section, ~bankfull_wse, #~w_s_elev_ft, 
    "B", 359.08,
    "D", 357.44,
    "E", 352.73,
    "F", 350.28,
    "G", 346.27,
    "H", 343.16  ) #%>% mutate(profile = "bankfull")

xs_depth_vs_discharge <- ras_1d_xs %>%
  left_join(thalweg_elevations, by = join_by(cross_section == xs_id)) %>%
  mutate(depth = w_s_elev_ft - thalweg_elevation) %>%
  select(cross_section, profile, q_total_cfs, depth) 

bf_model <- xs_depth_vs_discharge %>%
  group_by(cross_section) %>%
  do(model = lm(data = ., formula = log(q_total_cfs) ~ log(depth))) %>%
  mutate(model_coeff = list(model$coefficients)) %>%
  unnest_wider(col = model_coeff) %>%
  janitor::clean_names()  %>%
  mutate(bf_alpha = exp(intercept), bf_beta = log_depth) %>%
  select(cross_section, bf_alpha, bf_beta)

bankfull_q <- bankfull %>% 
  left_join(thalweg_elevations, by = join_by(cross_section == xs_id)) %>% 
  mutate(bankfull_depth = bankfull_wse - thalweg_elevation) %>%
  left_join(bf_model) %>%
  mutate(bankfull_discharge = bf_alpha * bankfull_depth^bf_beta) %>%
  mutate(bankfull_discharge_ri = flow_freq_curve_inv(bankfull_discharge))
```

    ## Joining with `by = join_by(cross_section)`

``` r
ggplot() + 
  geom_point(data = bankfull_q, aes(x = bankfull_depth, y = bankfull_discharge, color = cross_section, shape = "bankfull")) +
  ggrepel::geom_text_repel(data = bankfull_q, aes(x = bankfull_depth, y = bankfull_discharge, color = cross_section, label = round(bankfull_discharge))) +
  geom_point(data = xs_depth_vs_discharge, aes(x = depth, y = q_total_cfs, color = cross_section, shape = "modeled")) + 
  #geom_line(data = xs_depth_vs_discharge, aes(x = depth, y = q_total_cfs, color = cross_section, shape = "modeled")) +
  geom_smooth(data = xs_depth_vs_discharge, aes(x = depth, y = q_total_cfs, color = cross_section), method = "lm", se = F, linewidth=0.5) +
  scale_x_log10() + scale_y_log10()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
print(bankfull_q)
```

    ## # A tibble: 6 × 8
    ##   cross_section bankfull_wse thalweg_e…¹ bankf…² bf_al…³ bf_beta bankf…⁴ bankf…⁵
    ##   <chr>                <dbl>       <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 B                     359.        352.    7.5    23.6     2.15   1796.   17.2 
    ## 2 D                     357.        348.    9.59    6.43    2.51   1867.   18.7 
    ## 3 E                     353.        344.    8.41    1.88    3.04   1221.    7.72
    ## 4 F                     350.        343.    7.09    2.40    3.09   1016.    5.27
    ## 5 G                     346.        342.    4.66    5.92    3.14    742.    2.74
    ## 6 H                     343.        339.    3.77   26.6     2.30    562.    1.53
    ## # … with abbreviated variable names ¹​thalweg_elevation, ²​bankfull_depth,
    ## #   ³​bf_alpha, ⁴​bankfull_discharge, ⁵​bankfull_discharge_ri

Freeboard

``` r
capacity <- tribble(
    ~cross_section, ~capacity_wse, 
    "B", 370.05,                    
    "D", 364.88,
    "E", 359.95,
    "F", 356.01,
    "G", 353.96,
    "H", 352.44) 

ras_1d_xs %>%
  left_join(capacity) %>%
  filter(stringr::str_detect(profile, "Q100")) %>% 
  mutate(freeboard = capacity_wse - w_s_elev_ft) %>%
  select(cross_section, profile, freeboard) %>%
  pivot_wider(names_from = profile, values_from = freeboard)
```

    ## Joining with `by = join_by(cross_section)`

    ## # A tibble: 6 × 3
    ##   cross_section `Q100 FEMA` `Q100 AC`
    ##   <chr>               <dbl>     <dbl>
    ## 1 B                    8.33      7.53
    ## 2 D                    4.38      3.66
    ## 3 E                    3.41      2.73
    ## 4 F                    1.87      1.39
    ## 5 G                    4.19      3.29
    ## 6 H                    3.64      2.63

Sediment transport plots

``` r
hwm_hydraulics %>% 
  pivot_longer(cols = c(grain_size_suspended_mm, grain_size_mobilized_mm), names_to = "measure", values_to = "grain_size") %>%
  ggplot(aes(x = cross_section, color = series)) + 
  geom_point(aes(y = grain_size, group = series)) +
  scale_color_manual(values = c("channel" = "black", "floodplain" = "red")) + 
  facet_wrap(~measure, ncol = 1) +
  scale_y_continuous(trans = "log2") + 
  ggtitle("Sediment transport estimates based on high water marks")
```

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
breaks <- ras_1d_sed %>% group_by(profile, q_total_cfs) %>% summarize() %>% as.list()
```

    ## `summarise()` has grouped output by 'profile'. You can override using the
    ## `.groups` argument.

``` r
ras_1d_sed %>% 
  filter(loc != "total") %>%
  #group_by(series, cross_section, profile, q_total_cfs) %>% 
  #summarize(velocity_ft_s = mean(velocity_ft_s)) %>%
  ggplot(aes(x = q_total_cfs, y = velocity_ft_s, color = cross_section, label = cross_section, shape = loc)) + 
  geom_point() + scale_shape_manual(values=c(19, 24, 25)) +
  facet_grid(cols = vars(series)) + ggtitle("Velocity (via 1D model)") +
  geom_hline(yintercept=10, linetype='dotted', col = 'red') + 
  scale_x_continuous(breaks = breaks$q_total_cfs, labels = breaks$profile, position = "top", name = "", 
                     sec.axis = sec_axis(trans = ~ ., name = "discharge (cfs)")) + 
  theme(axis.text.x.top = element_text(angle = 45, vjust = 0, hjust=0))
```

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
ras_1d_sed %>% 
  filter(loc != "total") %>%
  #group_by(series, cross_section, profile, q_total_cfs) %>% 
  #summarize(grain_size_mobilized_mm = mean(grain_size_mobilized_mm)) %>%
  ggplot(aes(x = q_total_cfs, y = grain_size_mobilized_mm, color = cross_section, label = cross_section, shape = loc)) + 
  geom_point() + scale_shape_manual(values=c(19, 24, 25)) +
  facet_grid(cols = vars(series)) + ggtitle("Bed Mobilization (via 1D model)") +
  scale_y_continuous(limits = c(0,20), breaks = c(2^0, 2^1, 2^2, 2^3, 2^4)) + 
    scale_x_continuous(breaks = breaks$q_total_cfs, labels = breaks$profile, position = "top", name = "", 
                     sec.axis = sec_axis(trans = ~ ., name = "discharge (cfs)")) + 
  geom_hline(yintercept=2^0, col = "gray", linetype = "dotted") +
  geom_hline(yintercept=2^1,   col = "gray", linetype = "dotted") +
  geom_hline(yintercept=2^2,   col = "gray", linetype = "dotted") +
  geom_hline(yintercept=2^3,   col = "gray", linetype = "dotted") +
  geom_hline(yintercept=2^4 ,  col = "gray", linetype = "dotted") +
  theme(axis.text.x.top = element_text(angle = 45, vjust = 0, hjust=0))
```

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-25-2.png)<!-- -->

``` r
ras_1d_sed %>% 
  filter(loc != "total") %>%
  #group_by(series, cross_section, profile, q_total_cfs) %>% 
  #summarize(grain_size_suspended_mm = mean(grain_size_suspended_mm)) %>%
  ggplot(aes(x = q_total_cfs, y = grain_size_suspended_mm, color = cross_section, label = cross_section, shape = loc)) + 
  geom_point() + scale_shape_manual(values=c(19, 24, 25)) +
  facet_grid(cols = vars(series)) + ggtitle("Suspended Transport (via 1D model)") +
  scale_y_continuous(limits = c(0,2.25), breaks = c(2^-4, 2^-3, 2^-2, 2^-1, 2^0, 2^1)) +
    scale_x_continuous(breaks = breaks$q_total_cfs, labels = breaks$profile, position = "top", name = "", 
                     sec.axis = sec_axis(trans = ~ ., name = "discharge (cfs)")) + 
  geom_hline(yintercept=2^-4, col = "gray", linetype = "dotted") +
  geom_hline(yintercept=2^-3,   col = "gray", linetype = "dotted") +
  geom_hline(yintercept=2^-2,   col = "gray", linetype = "dotted") +
  geom_hline(yintercept=2^-1,   col = "gray", linetype = "dotted") +
  geom_hline(yintercept=2^0 ,  col = "gray", linetype = "dotted") +
  geom_hline(yintercept=2^1 ,  col = "gray", linetype = "dotted") +
  theme(axis.text.x.top = element_text(angle = 45, vjust = 0, hjust=0)) 
```

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-25-3.png)<!-- -->

``` r
ras_1d_sed %>% 
  filter(loc != "total") %>%
  #group_by(series, cross_section, profile, q_total_cfs) %>% 
  #summarize(velocity_ft_s = mean(velocity_ft_s)) %>%
  ggplot(aes(x = q_total_cfs, y = velocity_ft_s)) + 
  geom_point(aes(color = cross_section, shape = series)) + scale_shape_manual(values=c(19, 21)) +
  #geom_smooth(aes(color = cross_section, shape = series)) + scale_shape_manual(values=c(19, 21)) +
  #facet_grid(cols = vars(series)) + 
  ggtitle("Velocity (via 1D model)") +
  scale_x_continuous(breaks = breaks$q_total_cfs, labels = breaks$profile, position = "top", name = "", 
                     sec.axis = sec_axis(trans = ~ ., name = "discharge (cfs)")) + 
  theme(axis.text.x.top = element_text(angle = 45, vjust = 0, hjust=0), panel.grid.major.y = element_blank()) + 
  geom_hline(yintercept=1.5, col = "gray", linetype = "dotted") +
  geom_hline(yintercept=3,   col = "gray", linetype = "dotted") +
  geom_hline(yintercept=4,   col = "gray", linetype = "dotted") +
  geom_hline(yintercept=6,   col = "gray", linetype = "dotted") +
  geom_hline(yintercept=10,  col = "gray", linetype = "dotted") +
  scale_y_continuous(breaks = c(0, 1.5, 3, 4, 6, 10)) 
```

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

# View RAS 2D results

``` r
# RAS 2D results
ras_2d_gse <- read_csv("data/hec_ras_2d_profile_terrain.csv") %>%
  janitor::clean_names()
```

    ## Rows: 21282 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (3): ID, Station (ft), Terrain Profile (ft)
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ras_2d_wse <- read_csv("data/hec_ras_2d_profile_wse_max.csv") %>%
  janitor::clean_names()
```

    ## Rows: 7000 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (3): ID, Station (ft), WSE 'Max' (feet)
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ras_2d_wse_q100 <- read_csv("data/hec_ras_2d_profile_wse_max_q100.csv") %>%
  janitor::clean_names()
```

    ## Rows: 7000 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (3): ID, Station (ft), WSE 'Max' (feet)
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ras_2d_gse_bda <- read_csv("data/hec_ras_2d_profile_terrain_bda.csv") %>%
  janitor::clean_names()
```

    ## Rows: 27343 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (3): ID, Station (ft), Terrain Profile (ft)
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ras_2d_wse_bda <- read_csv("data/hec_ras_2d_profile_wse_max_bda.csv") %>%
  janitor::clean_names()
```

    ## Rows: 7022 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (3): ID, Station (ft), WSE 'Max' (feet)
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# RAS 1D results
hwm_dec_2022 <- hwm_geometries %>% 
  filter(peak_flow_date == "2022-12-31") %>% 
  left_join(cross_sections)
```

    ## Joining with `by = join_by(cross_section)`

``` r
# Upper bank elevations calculated in ArcGIS
bank_elev <- inner_join(
    read_csv("data/bank_elev_lb.csv") %>%
      janitor::clean_names() %>% 
      rename(station_2d = objectid, bank_elev_left = elev) %>%
      select(station_2d, bank_elev_left),
    read_csv("data/bank_elev_rb.csv") %>%
      janitor::clean_names() %>% 
      rename(station_2d = objectid, bank_elev_right = elev) %>%
      select(station_2d, bank_elev_right)
  ) %>%
  mutate(bank_elev_min = case_when(
    bank_elev_left < bank_elev_right ~ bank_elev_left, 
    TRUE ~bank_elev_right)) %>%
  mutate(bank_elev_rolling_med = zoo::rollapply(bank_elev_min, 100, median, partial=T))
```

    ## Rows: 4509 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Shape *
    ## dbl (6): OBJECTID *, ORIG_FID, FID_, FID_lidar2, Shape_Leng, elev
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 4509 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Shape *
    ## dbl (6): OBJECTID *, ORIG_FID, FID_, FID_lidar2, Shape_Leng, elev
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Joining with `by = join_by(station_2d)`

``` r
# # Floodplain elevations (25ft from thalweg) calculated in ArcGIS
# floodplain_elev <- inner_join(
#     read_csv("data/bank_elev_lb_floodplain.csv") %>%
#       janitor::clean_names() %>% 
#       rename(station_2d = objectid, floodplain_elev_left = elev) %>%
#       select(station_2d, floodplain_elev_left),
#     read_csv("data/bank_elev_rb_floodplain.csv") %>%
#       janitor::clean_names() %>% 
#       rename(station_2d = objectid, floodplain_elev_right = elev) %>%
#       select(station_2d, floodplain_elev_right)
#   ) %>%
#   mutate(floodplain_elev_min = case_when(
#     floodplain_elev_left < floodplain_elev_right ~ floodplain_elev_left, 
#     TRUE ~floodplain_elev_right)) %>%
#   mutate(floodplain_elev_rolling_med = zoo::rollapply(floodplain_elev_min, 100, median, partial=T))


ggplot() + 
  geom_line(data = ras_2d_gse, aes(x = station_ft, y = terrain_profile_ft, color = "Terrain Profile")) + 
  geom_line(data = ras_2d_wse, aes(x = station_ft, y = wse_max_feet, color = "Water Surface (RAS)")) +
  geom_line(data = ras_2d_wse_q100, aes(x = station_ft, y = wse_max_feet, color = "Water Surface (RAS)"), linetype = "dotted") +
  geom_line(data = ras_2d_gse_bda, aes(x = station_ft, y = terrain_profile_ft, color = "Terrain Profile"), linetype = "dashed") + 
  geom_line(data = ras_2d_wse_bda, aes(x = station_ft, y = wse_max_feet, color = "Water Surface (RAS)"), linetype = "dashed") +
  geom_point(data = hwm_dec_2022, aes(x = station_2d, y = thalweg_elevation, color = "Terrain Profile")) +
  geom_point(data = hwm_dec_2022, aes(x = station_2d, y = hwm_elevation, color = "Surveyed HWM"), shape = 9, size = 4) + 
  geom_point(data = filter(ras_1d, profile=="31 Dec 2022"), aes(x = station_2d, y = w_s_elev_ft, color = "Water Surface (RAS)")) +
  #geom_smooth(data = bank_elev, aes(x = station_2d, y = bank_elev_rolling_med, color = "Appx. Ground Surface"), 
  #            method = "gam", linetype = "dashed", size = 0.75) +
  #geom_smooth(data = floodplain_elev, aes(x = station_2d, y = floodplain_elev_rolling_med, color = "Appx. Ground Surface"), 
  #            method = "gam", linetype = "dashed", size = 0.75) +
  tidyquant::geom_ma(data = bank_elev, aes(x = station_2d, y = bank_elev_min, color = "Appx. Ground Surface"), ma_fun = EMA, n = 100) +
  #tidyquant::geom_ma(data = floodplain_elev, aes(x = station_2d, y = floodplain_elev_min, color = "Appx. Ground Surface"), 
  #                   ma_fun = SMA, n = 100) +
  scale_color_manual(values = c("Terrain Profile" = "black", 
                                "Appx. Ground Surface" = "black", 
                                "Water Surface (RAS)" = "blue",
                                "Surveyed HWM" = "red")) + 
  theme(legend.position = "none")
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ## Warning: Using the `size` aesthetic in this geom was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` in the `default_aes` field and elsewhere instead.

    ## Warning: Removed 23 rows containing missing values (`geom_line()`).
    ## Removed 23 rows containing missing values (`geom_line()`).
    ## Removed 23 rows containing missing values (`geom_line()`).

    ## Warning: Removed 2 rows containing missing values (`geom_point()`).

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

``` r
ggplot() + 
  geom_line(data = ras_2d_gse, aes(x = station_ft, y = terrain_profile_ft, color = "Terrain Profile")) + 
  geom_line(data = ras_2d_wse, aes(x = station_ft, y = wse_max_feet, color = "Water Surface (RAS)")) +
  geom_line(data = ras_2d_wse_q100, aes(x = station_ft, y = wse_max_feet, color = "Water Surface (RAS)"), linetype = "dotted") +
  geom_line(data = ras_2d_gse_bda, aes(x = station_ft, y = terrain_profile_ft, color = "Terrain Profile"), linetype = "dashed") + 
  geom_line(data = ras_2d_wse_bda, aes(x = station_ft, y = wse_max_feet, color = "Water Surface (RAS)"), linetype = "dashed") +
  geom_point(data = hwm_dec_2022, aes(x = station_2d, y = hwm_elevation, color = "Surveyed HWM"), shape = 9, size = 4) + 
  geom_point(data = hwm_dec_2022, aes(x = station_2d, y = hwm_elevation, color = "Surveyed HWM"), shape = 9, size = 4) + 
  tidyquant::geom_ma(data = bank_elev, aes(x = station_2d, y = bank_elev_min, color = "Appx. Ground Surface"), ma_fun = EMA, n = 100) +
  scale_color_manual(values = c("Terrain Profile" = "black", 
                                "Appx. Ground Surface" = "black", 
                                "Water Surface (RAS)" = "blue",
                                "Surveyed HWM" = "red")) + 
  theme(legend.position = "none") +ggtitle("2D model result")
```

    ## Warning: Removed 23 rows containing missing values (`geom_line()`).
    ## Removed 23 rows containing missing values (`geom_line()`).
    ## Removed 23 rows containing missing values (`geom_line()`).

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
ras_1d %>% 
  left_join(hwm_geometries) %>% 
  filter(profile != "5 Jan 1982") %>%
  arrange(water_surface_elevation) %>%
  ggplot(aes(x = river_sta, color = forcats::fct_reorder(profile, -w_s_elev_ft))) + 
    geom_point(aes(y = water_surface_elevation), shape = 9, size = 4) + 
    geom_line(aes(y = w_s_elev_ft, color = profile)) + 
    geom_line(aes(y = min_ch_el_ft), color = "black") +
    scale_x_reverse() + theme(legend.title=element_blank())
```

    ## Joining with `by = join_by(cross_section, peak_flow_date)`

    ## Warning: Removed 48 rows containing missing values (`geom_point()`).

![](tassajara_hydro_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
# summarize at-a-station hydraulics for 2D results

ras_2d_cleaned <- ras_2d_wse %>% 
  # join to the cross section table
  mutate(station_2d = as.integer(station_ft)) %>%
  group_by(station_2d) %>% 
  summarize(wse_2d = mean(wse_max_feet)) %>% 
  mutate(slope = abs((lag(wse_2d, n = 1) - wse_2d) / 
                    (lag(station_2d, n = 1) - station_2d))) 

ras_2d_hydraulics <- ras_2d_cleaned %>%
  inner_join(cross_sections) %>%
  left_join(xs_dfs, by = join_by(cross_section == xs_id)) %>%
  # calculate hydraulic geometries
  mutate(result = map2(xs_df, wse_2d, calc_xs)) %>% 
  unnest_wider(result) %>% 
  # compare against surveyed HWMs
  left_join(hwm_dec_2022 %>% select(cross_section, hwm_elevation)) %>%
  mutate(hwm_difference = wse_2d - hwm_elevation)
```

    ## Joining with `by = join_by(station_2d)`
    ## Joining with `by = join_by(cross_section)`

``` r
ras_2d_hydraulics %>%
    rename(cross_sectional_area_ft2 = cross_sectional_area,
         wetted_perimeter_ft = wetted_perimeter) %>%
  mutate(discharge_cfs = peak_flow_dec_2022,
         velocity_ft_s = discharge_cfs / cross_sectional_area_ft2,
         hydraulic_radius_ft = cross_sectional_area_ft2 / wetted_perimeter_ft,
         mannings_n = 1.486 * cross_sectional_area_ft2 * hydraulic_radius_ft^(2/3) 
                      * slope^(1/2) * discharge_cfs^(-1),
         # metric conversions
         velocity_m_s = velocity_ft_s / 0.3048,
         hydraulic_radius_m = hydraulic_radius_ft / 0.3048,
         cross_sectional_area_m2 = cross_sectional_area_ft2 / 0.3048,
         # bed mobilization
         critical_shields_number = 0.15 * slope^(1/4),
         grain_size_mobilized_mm = 10 * rho_cgs * hydraulic_radius_m * slope / 
                         (critical_shields_number * (rho_s_cgs - rho_cgs)),
         grain_size_mobilized_phi = -log2(grain_size_mobilized_mm),
         # suspended transport
         shear_velocity_cm_s = sqrt(g_cgs * (hydraulic_radius_m*100) * slope),
         settling_velocity_ndim = rho_cgs * shear_velocity_cm_s^3 / 
                         ((rho_s_cgs - rho_cgs) * g_cgs * nu_cgs),
         grain_size_suspended_ndim = sqrt(5832 * settling_velocity_ndim),
         grain_size_suspended_mm = 10 * grain_size_suspended_ndim * rho_cgs * nu_cgs^2 /
                         ((rho_s_cgs - rho_cgs) * g_cgs)^(1/3),
         grain_size_suspended_phi = -log2(grain_size_suspended_mm)
  )
```

    ## # A tibble: 6 × 28
    ##   station_2d wse_2d   slope cross_sec…¹ stati…² xs_df    thalw…³ water…⁴ max_d…⁵
    ##        <dbl>  <dbl>   <dbl> <chr>         <dbl> <list>     <dbl>   <dbl>   <dbl>
    ## 1        490   364. 0.00600 B             10800 <tibble>    352.    364.   12.3 
    ## 2       1210   361. 0.00467 D             10200 <tibble>    348.    361.   12.8 
    ## 3       2061   357. 0.00967 E              9300 <tibble>    344.    357.   12.4 
    ## 4       2568   354. 0.00133 F              8800 <tibble>    343.    354.   10.7 
    ## 5       3157   351. 0.00733 G              8200 <tibble>    342.    351.    9.42
    ## 6       3828   348. 0.00533 H              7600 <tibble>    339.    348.    8.21
    ## # … with 19 more variables: cross_sectional_area_ft2 <dbl>,
    ## #   wetted_perimeter_ft <dbl>, hwm_elevation <dbl>, hwm_difference <dbl>,
    ## #   discharge_cfs <dbl>, velocity_ft_s <dbl>, hydraulic_radius_ft <dbl>,
    ## #   mannings_n <dbl>, velocity_m_s <dbl>, hydraulic_radius_m <dbl>,
    ## #   cross_sectional_area_m2 <dbl>, critical_shields_number <dbl>,
    ## #   grain_size_mobilized_mm <dbl>, grain_size_mobilized_phi <dbl>,
    ## #   shear_velocity_cm_s <dbl>, settling_velocity_ndim <dbl>, …
