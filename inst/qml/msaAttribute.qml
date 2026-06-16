
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

	info:										qsTr("Attribute Agreement Analysis assesses the agreement between the operators' ratings and a known standard's ratings. One aim is to determine the accuracy of the investigated operators.")

	infoBottom: 								"## " + qsTr("Output") + "\n"
		+ "- " + qsTr("Study effectiveness summary: effectiveness, miss rate, and false alarm rate per operator with an acceptance evaluation.") + "\n"
		+ "- " + qsTr("Within appraisers: agreement between the inspected items and operators, with a 95%% confidence interval.") + "\n"
		+ "- " + qsTr("Each appraiser vs standard: agreement between the known standard and inspected items per operator, with a 95%% confidence interval.") + "\n"
		+ "- " + qsTr("Between appraisers: agreement between the different operators, with a 95%% confidence interval.") + "\n"
		+ "- " + qsTr("All appraisers vs standard: agreement between all operators and the known standard, with a 95%% confidence interval.") + "\n"
		+ "- " + qsTr("Plots of the agreement percentages and their confidence intervals (within appraisers and each appraiser vs standard).") + "\n"
		+ "\n---\n## " + qsTr("References") + "\n"
		+ "- " + qsTr("Duncan, A. J. (1986). Quality control and industrial statistics. Richard D. Irwin, Inc.; Automotive Industry Action Group (2005). Statistical process control (SPC) – Reference manual. AIAG.") + "\n"
		+ "- " + qsTr("Dodson, B., Lynch, D., Weidenbacher, M., & Klerx, R. (2009). Statistical process control handbook. SKF group.") + "\n"
		+ "\n---\n## " + qsTr("R Packages") + "\n"
		+ "- jaspGraphs\n- ggplot2\n- tidyr\n- psych\n- irr\n"

	DropDown
	{
		name: 									"dataFormat"
		label: 									qsTr("Data format")
		id: 									dataFormat
		indexDefaultValue: 						0
		info:									qsTr("Layout of the data: all observations in one column (\"Single column\") or spread across rows with a subgroup index (\"Across rows\").")
		values:
		[
			{ label: qsTr("Single column"), value: "longFormat"},
			{ label: qsTr("Across rows"), value: "wideFormat"},
		]
	}

	VariablesForm
	{
		id:										variablesFormLongFormat
		visible:								dataFormat.currentValue == "longFormat"

		AvailableVariablesList
		{
			name:								"variablesFormLongFormat"
		}

		AssignedVariablesList
		{
			name:								"measurementLongFormat"
			title:								qsTr("Results")
			id:									measurementLongFormat
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("The ratings made by the operators.")
		}

		AssignedVariablesList
		{
			name:								"operatorLongFormat"
			title:								qsTr("Operator")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("The operators in the measurement system.")
		}

		AssignedVariablesList
		{
			name:								"partLongFormat"
			title:								qsTr("Part")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("The parts of the measurement system.")
		}

		AssignedVariablesList
		{
			name:								"standardLongFormat"
			title:								qsTr("Standard")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("The ratings by the known standard (optional).")
		}
	}

	VariablesForm
	{
		id:										variablesFormWideFormat
		visible:								dataFormat.currentValue == "wideFormat"

		AvailableVariablesList
		{
			name:								"variablesFormwideFormat"
		}

		AssignedVariablesList
		{
			name:								"measurementsWideFormat"
			title:								qsTr("Results")
			id:									measurementsWideFormat
			allowedColumns:						["nominal"]
			info:								qsTr("The ratings made by the operators (one column per repeated rating).")
		}

		AssignedVariablesList
		{
			name:								"operatorWideFormat"
			title:								qsTr("Operator")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("The operators in the measurement system.")
		}

		AssignedVariablesList
		{
			name:								"partWideFormat"
			title:								qsTr("Part")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("The parts of the measurement system.")
		}

		AssignedVariablesList
		{
			name:								"standardWideFormat"
			title:								qsTr("Standard")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("The ratings by the known standard (optional).")
		}

	}

	Section
	{
		title:									qsTr("Kappa study")
		info:									qsTr("Kappa studies for binary data, quantifying rater agreement.")

		Group
		{
			title: 								qsTr("Tables")

			TextField
			{
				name: 							"positiveReference"
				label: 							qsTr("Positive reference:")
				id:								positiveReference
				enabled:						!kendallsTau.checked
				info:							qsTr("The positive reference category used for the rating classifications (for example \"Yes\", \"fit\", \"Good\").")
			}

			CheckBox
			{
				name:							"cohensKappa"
				label: 							qsTr("Cohen's kappa (interrater kappa)")
				id:								cohenskappa
				enabled:						positiveReference.value != ""
				checked:						!positiveReference.value == ""
				info:							qsTr("Compute Cohen's kappa (interrater kappa) per operator.")
			}

			CheckBox
			{
				name: 							"fleissKappa"
				label: 							qsTr("Fleiss' kappa (multirater kappa)")
				id:								fleisskappa
				checked:						!positiveReference.value == ""
				enabled:						!kendallsTau.checked
				info:							qsTr("Compute Fleiss' kappa (multirater kappa) per operator.")

			}
		}
	}

	Section
	{
		title: 									qsTr("Tau study")
		info:									qsTr("Tau study for ordinal data.")

		CheckBox
		{
			name: 								"kendallsTau"
			label: 								qsTr("Kendall's tau")
			id:									kendallsTau
			info:								qsTr("Compute Kendall's tau correlations between the operators and their ratings.")
		}
	}
}
