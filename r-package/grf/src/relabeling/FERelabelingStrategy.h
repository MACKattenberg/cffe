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

#ifndef GRF_FERELABELINGSTRATEGY_H
#define GRF_FERELABELINGSTRATEGY_H

#include <tuple>
#include <vector>

#include "commons/Data.h"
#include "relabeling/RelabelingStrategy.h"
#include "tree/Tree.h"

namespace grf {

class FERelabelingStrategy final: public RelabelingStrategy {
public:
  FERelabelingStrategy();

  FERelabelingStrategy(double reduced_form_weight);

  bool relabel(
      const std::vector<size_t>& samples,
      const Data& data,
      Eigen::ArrayXXd& responses_by_sample) const;
	  
  static std::tuple<std::vector<double>, std::vector<double>, std::vector<double>, std::vector<double>, std::vector<double>, std::vector<double>> calculate_means(
      const std::vector<size_t>& samples,
	  const Data& data);

  DISALLOW_COPY_AND_ASSIGN(FERelabelingStrategy);

private:
  double reduced_form_weight;
};

} // namespace grf

#endif //GRF_FERELABELINGSTRATEGY_H
