/*-------------------------------------------------------------------------------
  This file is part of generalized random forest (grf).

  grf is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  grf is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with grf. If not, see <http://www.gnu.org/licenses/>.
 #-------------------------------------------------------------------------------*/

#include <stdio.h>

#include "commons/utility.h"
#include "relabeling/FERelabelingStrategy.h"

namespace grf {

FERelabelingStrategy::FERelabelingStrategy():
  reduced_form_weight(0) {}

FERelabelingStrategy::FERelabelingStrategy(double reduced_form_weight):
  reduced_form_weight(reduced_form_weight) {}

bool FERelabelingStrategy::relabel(
    const std::vector<size_t>& samples,
    const Data& data,
    Eigen::ArrayXXd& responses_by_sample) const {

  const auto[average_outcome_i,
	           average_outcome_t,
			   average_outcome_tau,
			   average_treatment_i, 
			   average_treatment_t,
			   average_treatment_tau] = FERelabelingStrategy::calculate_means(samples, data);

  // Calculate the treatment effect.
  double numerator = 0.0;
  double denominator = 0.0;

  for (size_t sample : samples) {
    double weight = data.get_weight(sample);
    double outcome = data.get_outcome(sample);
    double treatment = data.get_treatment(sample);
	size_t group = data.get_individual_fe(sample);
	size_t t = data.get_time_fe(sample);
	size_t tau = data.get_event_time_fe(sample);
	
	double treat_demeaned = treatment - average_treatment_i[group] - average_treatment_t[t] - average_treatment_tau[tau];
	double outcome_demeaned = outcome - average_outcome_i[group] - average_outcome_t[t] - average_outcome_tau[tau];
	
    numerator += weight * treat_demeaned * outcome_demeaned;
    denominator += weight * treat_demeaned * treat_demeaned;
  }

  if (equal_doubles(denominator, 0.0, 1.0e-10)) {
    return true;
  }

  double local_average_treatment_effect = numerator / denominator;

  // Create the new outcomes.
  for (size_t sample : samples) {
    double response = data.get_outcome(sample);
    double treatment = data.get_treatment(sample);
	size_t group = data.get_individual_fe(sample);
	size_t t = data.get_time_fe(sample);
	size_t tau = data.get_event_time_fe(sample);

    double residual = (response - average_outcome_i[group] - average_outcome_t[t] - average_outcome_tau[tau]) - local_average_treatment_effect * (treatment - average_treatment_i[group] - average_treatment_t[t] - average_treatment_tau[t]);
    responses_by_sample(sample, 0) = (treatment - average_treatment_i[group] - average_treatment_t[t] - average_treatment_tau[tau]) * residual;
  }
  
  return false;
}

std::tuple<std::vector<double>, std::vector<double>, std::vector<double>, std::vector<double>, std::vector<double>, std::vector<double>> FERelabelingStrategy::calculate_means(
      const std::vector<size_t>& samples,
	  const Data& data) {
  // Calculate per-individual average
  size_t num_individuals = data.get_num_individual_groups();
  size_t num_periods = data.get_num_time_groups();
  size_t num_event_periods = data.get_num_event_time_groups();
  std::vector<double> total_outcome_i (num_individuals);
  std::vector<double> total_treatment_i (num_individuals);
  std::vector<double> total_weight_i (num_individuals);
   
  for (size_t sample : samples) {
	size_t group = data.get_individual_fe(sample);
	double weight = data.get_weight(sample);
	
	total_outcome_i[group] += weight * data.get_outcome(sample);
    total_treatment_i[group] += weight * data.get_treatment(sample);
	total_weight_i[group] += weight;
  }

  std::vector<double> average_outcome_i (num_individuals);	
  std::vector<double> average_treatment_i (num_individuals);
  for (size_t group = 0; group < num_individuals; group++) {
	average_outcome_i[group] = total_outcome_i[group] / total_weight_i[group];
    average_treatment_i[group] = total_treatment_i[group] / total_weight_i[group];
  }
  
  // Calculate per-period average
  std::vector<double> sum_weight_t (num_periods);
  std::vector<double> total_outcome_t (num_periods);
  std::vector<double> total_treatment_t (num_periods);
  for (size_t sample : samples) {
	size_t group = data.get_individual_fe(sample);
	size_t period = data.get_time_fe(sample);
	double weight = data.get_weight(sample);
	
	total_outcome_t[period] += weight * (data.get_outcome(sample) - average_outcome_i[group]);
    total_treatment_t[period] += weight * (data.get_treatment(sample) - average_treatment_i[group]);
    sum_weight_t[period] += weight;
  }
  
  std::vector<double> average_outcome_t (num_periods);
  std::vector<double> average_treatment_t (num_periods);
  for (size_t t = 0; t < num_periods; t++) {
	average_outcome_t[t] = total_outcome_t[t] / sum_weight_t[t];
    average_treatment_t[t] = total_treatment_t[t] / sum_weight_t[t];
  }
  
  // Calculate per-event-period average
  std::vector<double> sum_weight_tau (num_event_periods);
  std::vector<double> total_outcome_tau (num_event_periods);
  std::vector<double> total_treatment_tau (num_event_periods);
  for (size_t sample : samples) {
	size_t group = data.get_individual_fe(sample);
	size_t period = data.get_time_fe(sample);
	size_t event_period = data.get_event_time_fe(sample);
	double weight = data.get_weight(sample);
	
	total_outcome_tau[event_period] += weight * (data.get_outcome(sample) - average_outcome_i[group] - average_outcome_t[period]);
    total_treatment_tau[event_period] += weight * (data.get_treatment(sample) - average_treatment_i[group] - average_treatment_t[period]);
    sum_weight_tau[event_period] += weight;
  }
  
  std::vector<double> average_outcome_tau (num_event_periods);
  std::vector<double> average_treatment_tau (num_event_periods);
  for (size_t tau = 0; tau < num_event_periods; tau++) {
	average_outcome_t[tau] = total_outcome_t[tau] / sum_weight_t[tau];
    average_treatment_t[tau] = total_treatment_t[tau] / sum_weight_t[tau];
  }
  
  return std::make_tuple(average_outcome_i, average_outcome_t, average_outcome_tau, average_treatment_i, average_treatment_t, average_treatment_tau);
}

} // namespace grf

