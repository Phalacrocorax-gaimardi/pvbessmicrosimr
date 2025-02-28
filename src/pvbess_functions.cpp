#include <Rcpp.h>
using namespace Rcpp;

//' demand_cpp
//'
//' calculate daily expected demand
//'
//' @param day day of year
//' @param D_max max demand in winter
//' @param D_min min demand in winter
//' @param lag_D lag in days default 30

// [[Rcpp::export]]
NumericVector demand_cpp(NumericVector day, double D_max, double D_min, double lag_D = 30.0) {
  // Calculate the phase shift in radians
  double phase_D = lag_D / 360 * 2 * M_PI;

  // Calculate the demand using the cosine function
  NumericVector demand = (D_max + D_min) / 2 + (D_max - D_min) / 2 * cos(2 * M_PI * day / 365 - phase_D);

  // Return the result
  return demand;
}

//' day_length_cpp
//'
//' the length of a day
//'
//' @param day day of year
//' @param latitude latitude


// [[Rcpp::export]]
NumericVector day_length_cpp(NumericVector day, double latitude) {
  // Calculate declination
  NumericVector declination = 23.44 * sin(2 * M_PI / 365 * (day - 81));

  // Calculate day length
  NumericVector day_length = 24 * acos(-tan(2 * M_PI * latitude / 360) * tan(declination * 2 * M_PI / 360)) / M_PI;

  // Return the result
  return day_length;
}

// [[Rcpp::export]]
NumericVector daylight_usage_cpp(NumericVector day, double rho_solstice) {
  double latitude = 53.0; // Fixed latitude value

  // Calculate day length using day_length_fun
  NumericVector day_length = day_length_cpp(day, latitude);

  // Calculate daylight usage
  NumericVector daylight_usage = pmin(1.0, rho_solstice * day_length / 12);

  // Return the result
  return daylight_usage;
}


// [[Rcpp::export]]
double clearness_index_cpp(double day, double K_max, double K_min, double phase_K = 0, double exponent = 4) {
  // Define the sigma value based on K_max and K_min
  double sigma = (K_min < K_max) ? -1.0 / std::log(K_min / K_max) : std::numeric_limits<double>::infinity();

  // Calculate the cos_term based on the day and phase_K
  double cos_term = std::cos(M_PI * (day / 365.0) - phase_K);

  // Return the clearness index for this particular day
  return K_max * std::exp(-std::pow(std::abs(cos_term), exponent) / sigma);
}

#include <cmath>
#include <unordered_map>

// [[Rcpp::export]]
NumericVector solar_potential_cpp(NumericVector day, double latitude = 53, double K_max = 0.42, double K_min = 0.3, std::string azimuth_angle = "South", double phase_K = 0) {
  int n = day.size();
  NumericVector result(n);

  // Define solar potentials for different azimuth angles
  std::unordered_map<std::string, double> solar_potentials = {
    {"South", 2.2},
    {"SW", 2.07},
    {"West", 1.77},
    {"NW", 1.47},
    {"North", 1.34},
    {"NE", 1.46},
    {"East", 1.76},
    {"SE", 2.06}
  };

  // Calculate aspect factor based on azimuth angle
  double aspect_factor = 0.0;
  auto it = solar_potentials.find(azimuth_angle);
  if (it != solar_potentials.end()) {
    aspect_factor = it->second / 2.5;
  } else {
    stop("Error: Azimuth angle not found in predefined solar_potentials.");
  }

  // Calculate solar potential for each day
  for (int i = 0; i < n; i++) {
    double declination = 23.44 * std::sin(2 * M_PI / 365 * (day[i] - 81));
    double cos_psi = -std::tan(2 * M_PI * latitude / 360) * std::tan(declination * 2 * M_PI / 360);
    cos_psi = (std::abs(cos_psi) > 1) ? std::copysign(1.0, cos_psi) : cos_psi;
    double daily_irradiance = (1 / M_PI) * std::cos(2 * M_PI * latitude / 360) * std::cos(2 * M_PI * declination / 360) * std::sqrt(1 - cos_psi * cos_psi) +
      (1 / M_PI) * std::acos(cos_psi) * std::sin(2 * M_PI * latitude / 360) * std::sin(2 * M_PI * declination / 360);

    result[i] = aspect_factor * 24 * 1367 * daily_irradiance / 1000 * clearness_index_cpp(day[i], K_max, K_min, phase_K, 4);
  }

  return result;
}


// [[Rcpp::export]]
DataFrame energy_flows_cpp(double S_1, double S_2, std::string aspect,
                            double shading_factor_1, double shading_factor_2,
                            double B, double D_max, double D_min, NumericVector params) {

  // Create day vector manually (1 to 365)
  NumericVector day(365);
  for (int i = 0; i < 365; i++) {
    day[i] = i + 1;  // Fill with values from 1 to 365
  }

  // Validate aspect
  if (!(aspect == "South-North" || aspect == "East-West" || aspect == "SW-NE" || aspect == "SE-NW")) {
    stop("bad house orientation label");
  }

  // Calculate demand, solar potential, and rho
  NumericVector demand = demand_cpp(day, D_max, D_min, params["lag_D"]);
  NumericVector solar_potential_1 = shading_factor_1 * solar_potential_cpp(day, params["latitude"], params["K_max"], params["K_min"], aspect.substr(0, aspect.find("-")));
  NumericVector solar_potential_2 = shading_factor_2 * solar_potential_cpp(day, params["latitude"], params["K_max"], params["K_min"], aspect.substr(aspect.find("-") + 1));
  NumericVector rho = daylight_usage_cpp(day, params["rho_solstice"]);

  // Initialize variables to store results
  NumericVector d_tilde(day.size());
  NumericVector sh_tilde(day.size());
  NumericVector E_s(day.size());
  NumericVector E_n1(day.size());
  NumericVector E_n2(day.size());
  IntegerVector f1(day.size());
  IntegerVector f2(day.size());
  IntegerVector f3(day.size());
  NumericVector evening_imports(day.size());
  NumericVector day_imports(day.size());
  NumericVector day_exports(day.size());

  // Loop over days to calculate each value
  for (int i = 0; i < day.size(); i++) {
    d_tilde[i] = (1 - rho[i]) * demand[i];
    sh_tilde[i] = S_1 * solar_potential_1[i] + S_2 * solar_potential_2[i] - rho[i] * demand[i];

    E_s[i] = std::max(0.0, std::min(B, std::min(sh_tilde[i], d_tilde[i])));
    E_n1[i] = std::max(0.0, std::min(B, -sh_tilde[i]));

    if (sh_tilde[i] < 0) {
      E_n2[i] = std::min(B - E_n1[i], d_tilde[i]);
    } else {
      E_n2[i] = std::min(B, d_tilde[i]) - E_s[i];
    }

    f1[i] = (d_tilde[i] - E_s[i] - E_n2[i] > 0) ? 1 : 0;
    f2[i] = (-sh_tilde[i] - E_n1[i] > 0) ? 1 : 0;
    f3[i] = (sh_tilde[i] - E_s[i] > 0) ? 1 : 0;

    evening_imports[i] = (d_tilde[i] - E_s[i] - E_n2[i]) * f1[i];
    day_imports[i] = (-sh_tilde[i] - E_n1[i]) * f2[i];
    day_exports[i] = (sh_tilde[i] - E_s[i]) * f3[i];
  }

  // Return as DataFrame
  return DataFrame::create(
    Named("day") = day,
    Named("demand") = demand,
    Named("solar_potential_1") = solar_potential_1,
    Named("solar_potential_2") = solar_potential_2,
    Named("rho") = rho,
    Named("d_tilde") = d_tilde,
    Named("sh_tilde") = sh_tilde,
    Named("E_s") = E_s,
    Named("E_n1") = E_n1,
    Named("E_n2") = E_n2,
    Named("f1") = f1,
    Named("f2") = f2,
    Named("f3") = f3,
    Named("evening_imports") = evening_imports,
    Named("day_imports") = day_imports,
    Named("day_exports") = day_exports
  );
}

//' @name seai_grant_cpp
//' @title seai_grant_cpp
//' @description The fast version of seai_grant
//'
//' @param params fast scenario parameters
//' @param s solar capacity in kW
//' @param b battery capacity in kWh
//' @return grant amount in euros
//' @export
//'
//' @examples

// [[Rcpp::export]]
double seai_grant_cpp(NumericVector params, double s, double b) {
  // Extract parameters
  double sol_lower_threshold = params["sol_lower_threshold"];
  double sol_upper_threshold = params["sol_upper_threshold"];
  double sol_lower_grant = params["sol_lower_grant"];
  double sol_upper_grant = params["sol_upper_grant"];
  double bat_threshold = params["bat_threshold"];
  double bat_grant = params["bat_grant"];
  double yeartime = params["yeartime"];
  double grant_introduction_date = params["grant_introduction_date"];
  double grant_removal_date = params["grant_removal_date"];

  // Calculate max_sol_grant
  double max_sol_grant = sol_lower_threshold * sol_lower_grant + (sol_upper_threshold - sol_lower_threshold) * sol_upper_grant;

  // Calculate grant based on conditions
  double grant;
  if (s <= sol_lower_threshold) {
    grant = sol_lower_grant * s;
  } else if (s >= sol_upper_threshold && b < bat_threshold) {
    grant = max_sol_grant;
  } else if (s >= sol_upper_threshold && b >= bat_threshold) {
    grant = max_sol_grant + bat_grant;
  } else if (s > sol_lower_threshold && s < sol_upper_threshold) {
    grant = sol_lower_threshold * sol_lower_grant + (s - sol_lower_threshold) * sol_upper_grant;
  } else {
    grant = NA_REAL; // NA in Rcpp
  }

  // Apply time constraints
  if (yeartime >= grant_introduction_date && yeartime <= grant_removal_date) {
    return grant;
  } else {
    return 0.0;
  }
}



