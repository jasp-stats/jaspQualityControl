// Copyright (C) 2013-2018 University of Amsterdam
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Layouts
import JASP.Controls

Form
{
	columns:									1

	info:										qsTr("A Type 4 Linearity Study (Gauge Linearity and Bias) investigates the linearity (accuracy across the expected range of measurements) and bias (agreement between measurements and reference values) of a measurement system.")

	infoBottom: 								"## " + qsTr("Output") + "\n"
		+ "- " + qsTr("Gauge bias table: per part, the reference values, mean per reference value, bias per reference value, and a t-test of the bias against zero.") + "\n"
		+ "- " + qsTr("Regression model table: coefficients (intercept and slope), t-statistics, standard errors, and p-values.") + "\n"
		+ "- " + qsTr("Gauge linearity table: regression sigma (S), absolute coefficient values multiplied by the process variation (linearity), R-squared, and linearity as a percentage of the process variation.") + "\n"
		+ "- " + qsTr("Bias and linearity plot: linear relationship between bias and reference values.") + "\n"
		+ "- " + qsTr("Percentage process variation graph: percentage of linearity and bias relative to the process variation.") + "\n"
		+ "\n---\n## " + qsTr("References") + "\n"
		+ "- " + qsTr("Duncan, A. J. (1986). Quality control and industrial statistics. Richard D. Irwin, Inc.; Automotive Industry Action Group (2005). Statistical process control (SPC) – Reference manual. AIAG.") + "\n"
		+ "- " + qsTr("Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2009). Statistical process control handbook. SKF group.") + "\n"
		+ "\n---\n## " + qsTr("R Packages") + "\n"
		+ "- jaspGraphs\n- ggplot2\n- ggpubr\n"

	VariablesForm
	{
		id:										variablesForm

		AvailableVariablesList
		{
			name:								"variablesForm"
		}

		AssignedVariablesList
		{
			name:								"part"
			title:								qsTr("Parts")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("The parts of the measurement system.")
		}

		AssignedVariablesList
		{
			name:								"measurement"
			title:								qsTr("Measurements")
			singleVariable:						true
			allowedColumns:						["scale"]
			info:								qsTr("The observations/data collected from the process.")
		}

		AssignedVariablesList
		{
			name:								"standard"
			title:								qsTr("Standard")
			singleVariable:						true
			allowedColumns:						["scale"]
			info:								qsTr("The reference (standard) values.")
		}
	}

	Group
	{
		title: 									qsTr("Analysis Options")

		CheckBox
		{
			name: 								"linearityTable"
			label: 								qsTr("Linearity table")
			checked: 							true
			info:								qsTr("Output the regression model and gauge linearity tables.")
		}

		CheckBox
		{
			name: 								"biasTable"
			label: 								qsTr("Bias table")
			checked:							true
			info:								qsTr("Output the gauge bias table.")
		}
	}

	Group
	{
		title:									qsTr("Plots")

		CheckBox
		{
			name:								"linearityAndBiasPlot"
			label:								qsTr("Linearity and bias graph")
			checked:							true
			info:								qsTr("Plot the linear relationship between the bias and the reference values.")
		}
	}

	Group
	{
		title: qsTr("Optional")

		CheckBox
		{
			name: 								"manualProcessVariation"
			label: 								qsTr("Process variation")
			childrenOnSameRow: 					true
			id:									manualProcessVariation
			info:								qsTr("Manually specify the process variation (number of standard deviations, by default 6) used to compute the bias.")

			DoubleField
			{
				name: 							"manualProcessVariationValue"
				defaultValue:					1
				negativeValues:				false
				decimals: 						7
				fieldWidth: 					30
			}
		}

		CheckBox
		{
			name: 								"percentProcessVariationPlot"
			label: 								qsTr("Percent process variation graph")
			checked: 							false
			enabled:							manualProcessVariation.checked
			info:								qsTr("Plot the percentage of gauge bias and linearity relative to the process variation.")
		}
	}
}
