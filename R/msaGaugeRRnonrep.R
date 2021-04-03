#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

msaGaugeRRnonrep <- function(jaspResults, dataset, options, ...){

  measurements <- unlist(options$measurements)
  parts <- unlist(options$parts)
  operators <- unlist(options$operators)

  numeric.vars <- measurements

  factor.vars <- c(parts, operators)
  factor.vars <- factor.vars[factor.vars != ""]

  ready <- (length(measurements) != 0 && operators != "" && parts != "")
  readyRangeMethod <- length(measurements) == 2

  if (is.null(dataset)) {
    dataset         <- .readDataSetToEnd(columns.as.numeric  = numeric.vars, columns.as.factor = factor.vars,
                                         exclude.na.listwise = c(numeric.vars, factor.vars))
  }

}
