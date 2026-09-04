#include "relabeling/FERelabelingStrategy.h"
#include "prediction/FEPredictionStrategy.h"

namespace grf {

const std::size_t FEPredictionStrategy::OUTCOME_TREATMENT = 0;
const std::size_t FEPredictionStrategy::TREATMENT_TREATMENT = 1;
const std::size_t FEPredictionStrategy::NUM_TYPES = 2;

size_t FEPredictionStrategy::prediction_length() const {
    return 1;
}

std::vector<double> FEPredictionStrategy::predict(const std::vector<double>& average) const {
  return { average.at(OUTCOME_TREATMENT) / average.at(TREATMENT_TREATMENT) };
}


size_t FEPredictionStrategy::prediction_value_length() const {
  return NUM_TYPES;
}

PredictionValues FEPredictionStrategy::precompute_prediction_values(
    const std::vector<std::vector<size_t>>& leaf_samples,
    const Data& data) const {
  size_t num_leaves = leaf_samples.size();

  std::vector<std::vector<double>> values(num_leaves);

  for (size_t i = 0; i < leaf_samples.size(); ++i) {
    size_t leaf_size = leaf_samples[i].size();
    if (leaf_size == 0) {
      continue;
    }
	
	const auto[average_outcome_i,
	           average_outcome_t,
			   average_outcome_tau,
			   average_treatment_i, 
			   average_treatment_t,
			   average_treatment_tau] = FERelabelingStrategy::calculate_means(leaf_samples[i], data);

    double sum_YW = 0;
    double sum_WW = 0;

    double sum_weight = 0.0;
    for (auto& sample : leaf_samples[i]) {
      auto weight = data.get_weight(sample);
      size_t group = data.get_individual_fe(sample);
	  size_t t = data.get_time_fe(sample);
	  size_t tau = data.get_event_time_fe(sample);

	  double outcome_demeaned = data.get_outcome(sample) - average_outcome_i[group] - average_outcome_t[t] - average_outcome_tau[tau];
	  double treat_demeaned = data.get_treatment(sample) - average_treatment_i[group] - average_treatment_t[t] - average_treatment_tau[tau];
	  
      sum_YW += weight * outcome_demeaned * treat_demeaned;
      sum_WW += weight * treat_demeaned * treat_demeaned;
      sum_weight += weight;
    }

    // if total weight is very small, treat the leaf as empty
    if (std::abs(sum_weight) <= 1e-16) {
      continue;
    }
    std::vector<double>& value = values[i];
    value.resize(NUM_TYPES);

    value[OUTCOME_TREATMENT] = sum_YW / leaf_size;
    value[TREATMENT_TREATMENT] = sum_WW / leaf_size;
  }

  return PredictionValues(values, NUM_TYPES);
}

std::vector<double> FEPredictionStrategy::compute_variance(
        const std::vector<double>& average,
        const PredictionValues& leaf_values,
        size_t ci_group_size) const {
  double treatment_effect_estimate = average.at(OUTCOME_TREATMENT) / average.at(TREATMENT_TREATMENT);
  double v_estimate = average.at(TREATMENT_TREATMENT);
  
  double num_good_groups = 0;
  double rho_squared = 0;
  double rho_grouped_squared = 0;

  for (size_t group = 0; group < leaf_values.get_num_nodes() / ci_group_size; ++group) {
    bool good_group = true;
    for (size_t j = 0; j < ci_group_size; ++j) {
      if (leaf_values.empty(group * ci_group_size + j)) {
        good_group = false;
      }
    }
    if (!good_group) continue;

    num_good_groups++;

    double group_rho = 0;

    for (size_t j = 0; j < ci_group_size; ++j) {

      size_t i = group * ci_group_size + j;
      const std::vector<double>& leaf_value = leaf_values.get_values(i);

      double rho = (leaf_value.at(OUTCOME_TREATMENT) - leaf_value.at(TREATMENT_TREATMENT) * treatment_effect_estimate) / v_estimate;
      rho_squared += rho * rho;
      group_rho += rho;
    }

    group_rho /= ci_group_size;
    rho_grouped_squared += group_rho * group_rho;
  }

  double var_between = rho_grouped_squared / num_good_groups;
  double var_total = rho_squared / (num_good_groups * ci_group_size);

  // This is the amount by which var_between is inflated due to using small groups
  double group_noise = (var_total - var_between) / (ci_group_size - 1);

  // A simple variance correction, would be to use:
  // var_debiased = var_between - group_noise.
  // However, this may be biased in small samples; we do an objective
  // Bayes analysis of variance instead to avoid negative values.
  double var_debiased = bayes_debiaser.debias(var_between, group_noise, num_good_groups);

  return { var_debiased };
}

std::vector<std::pair<double, double>> FEPredictionStrategy::compute_error(
    size_t sample,
    const std::vector<double>& average,
    const PredictionValues& leaf_values,
    const Data& data) const {
  return { std::make_pair(0., 0.) };
}

}