
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

import QtQuick 									2.8
import QtQuick.Layouts 							1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form
{
	usesJaspResults:							true
	columns:									1

	DropDown
	{
		name: "gaugeRRdataFormat"
		label: qsTr("Data format")
		indexDefaultValue: 0
		values:
			[
			{label: qsTr("Column"),					value: "gaugeRRlongFormat"},
			{label: qsTr("Row"),				value: "gaugeRRwideFormat"},
		]
		id: gaugeRRdataFormat
	}

	VariablesForm
	{
		id:										variablesForm

		AvailableVariablesList
		{
			name:								"variablesForm"
		}

		AssignedVariablesList
		{
			id:									variable1
			name:								"operators"
			title:								qsTr("Operators")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal", "scale"]
		}

		AssignedVariablesList
		{
			id:									variable2
			name:								"parts"
			title:								qsTr("Parts")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal", "scale"]
		}

		AssignedVariablesList
		{
			id:									variable4
			name:								"measurementsLong"
			title:								qsTr("Measurements")
			singleVariable:						true
			visible:							gaugeRRdataFormat.currentValue == "gaugeRRlongFormat"
			allowedColumns:						["scale"]
		}

		AssignedVariablesList
		{
			id:									variable3
			name:								"measurements"
			title:								qsTr("Measurements")
			singleVariable:						false
			visible:							gaugeRRdataFormat.currentValue == "gaugeRRwideFormat"
			allowedColumns:						["scale"]
		}



	}
	DropDown
	{
		name: "gaugeRRmethod"
		label: qsTr("Gauge r&R Method")
		indexDefaultValue: 0
		values:
			[
			{label: qsTr("ANOVA method"),					value: "anovaMethod"},
			{label: qsTr("Range method"),				value: "rangeMethod"},
		]
		id: gaugeRRmethod
	}

	Section
	{
		title: qsTr("ANOVA Method Options")
		visible: gaugeRRmethod.currentValue == "anovaMethod"

		Group
		{
			title: qsTr("Analysis Options")

			DropDown
			{
				name: "standardDeviationReference"
				label: qsTr("Std. Deviation reference")
				indexDefaultValue: 0
				values:
					[
					{label: qsTr("Study Std. Deviation"),					value: "studyStandardDeviation"},
					{label: qsTr("Historical process Std. Deviation"),				value: "historicalStandardDeviation"}
				]
				id: variationReference
			}

			DoubleField
			{
				name:			"historicalStandardDeviationValue"
				label:			qsTr("Hist. Std. Deviation value:")
				defaultValue:	0
				enabled:		variationReference.currentValue == "historicalStandardDeviation"
			}


			CheckBox
			{
				name: "gaugeToleranceEnabled"
				label: qsTr("Tolerance:")
				checked: false
				childrenOnSameRow: true

				DoubleField
				{
					name: "tolerance"
					defaultValue: 1
					min: 0.001
					decimals: 3
				}
			}

			CheckBox
			{
				name: "gaugeANOVA"
				label: qsTr("r&R table ANOVA method")
				checked: true

				DoubleField
				{
					name: "alphaForANOVA"
					label: qsTr("Alpha interaction removal:")
					fieldWidth: 60
					defaultValue: 0.05
					max: 1
					decimals: 3
				}

				DropDown
				{
					name: "studyVarMultiplierType"
					label: qsTr("Study Var. multiplier type")
					indexDefaultValue: 0
					values:
						[
						{label: qsTr("Std. Deviation"),		value: "svmSD"},
						{label: qsTr("Percent"),				value: "svmPercent"}
					]
					id: studyVarMultiplierType
				}

				DoubleField
				{
					name: "studyVarMultiplier"
					label: qsTr("Study Var. multiplier value:")
					fieldWidth: 60
					defaultValue: 6
					min:			0.001
					max:			99.999
					decimals: 3
				}

				CheckBox
				{
					name: "gaugeVarCompGraph"
					label: qsTr("Components of variation")
					checked: true
				}
			}

			CheckBox
			{
				name: "gaugeDescriptives";		label: qsTr("Descriptives table");		checked: true
			}
		}


		Group
		{
			title: qsTr("Plots")

			CheckBox
			{
				name: "gaugeRchart"
				label: qsTr("R charts by operator")
			}

			CheckBox
			{
				name: "gaugeXbarChart"
				label: qsTr("X-bar charts by operator")
			}

			CheckBox
			{
				name: "gaugeScatterPlotOperators"
				label: qsTr("Scatter plots operators")

				CheckBox
				{
					name: "gaugeScatterPlotFitLine"
					label: qsTr("Fit line")
				}

				CheckBox
				{
					name: "gaugeScatterPlotOriginLine"
					label: qsTr("Show origin line")
				}
			}

			CheckBox
			{
				name: "gaugeByPart"
				label: qsTr("Measurement by part plot")

				CheckBox
				{
					name: "gaugeByPartAll"
					label: qsTr("Display all measurements")
				}
			}

			CheckBox
			{
				name: "gaugeByOperator"
				label: qsTr("Measurement by operators plot")
			}

			CheckBox
			{
				name: "gaugeByInteraction";		label: qsTr("Part x operator interaction plot")
			}

			CheckBox
			{
				name: "trafficPlot";		label: qsTr("Traffic light graph")
			}

		}
	}


	Section
	{
		title: qsTr("ANOVA Method Report")
		visible: gaugeRRmethod.currentValue == "anovaMethod"


		TextField
		{
			id:						anovaGaugeTitle
			label: 					qsTr("Title:")
			name: 					"anovaGaugeTitle"
			placeholderText:		qsTr("Measurement")
			fieldWidth:				100
		}

		TextField
		{
			id:						anovaGaugeName
			label: 					qsTr("Gauge Name:")
			name: 					"anovaGaugeName"
			placeholderText:		qsTr("Name")
			fieldWidth:				100
		}

		TextField
		{
			id:						anovaGaugeDate
			label: 					qsTr("Date:")
			name: 					"anovaGaugeDate"
			placeholderText:		qsTr("Date")
			fieldWidth:				100
		}

		TextField
		{
			id:						anovaGaugeReportedBy
			label: 					qsTr("Reported by:")
			name: 					"anovaGaugeReportedBy"
			placeholderText:		qsTr("Name")
			fieldWidth:				100
		}

		TextField
		{
			id:						anovaGaugeMisc
			label: 					qsTr("Misc:")
			name: 					"anovaGaugeMisc"
			placeholderText:		qsTr("Miscellaneous")
			fieldWidth:				100
		}

		CheckBox
		{
			name: "anovaGaugeReport";		label: qsTr("Show Report")
		}


	}

	Section
	{
		title: qsTr("Range Method Options")
		visible: gaugeRRmethod.currentValue == "rangeMethod"

		Group
		{
			title: qsTr("Analysis Options")

			DoubleField
			{
				name:			"rangePSD"
				label:			qsTr("Process Std. Deviation:")
				defaultValue:	1
				enabled:		TRUE
			}

			CheckBox
			{
				name: "rangeRr"
				label: qsTr("r&R table")
				checked: true
			}
		}

		Group
		{
			title: qsTr("Plots")

			CheckBox
			{
				name: "rangeScatterPlotOperatorParts"
				label: qsTr("Scatter plot operators vs. parts")
			}

			CheckBox
			{
				name: "rangeScatterPlotOperators"
				label: qsTr("Scatter plot operators")
				checked: true

				CheckBox
				{
					name: "rangeScatterPlotFitLine"
					label: qsTr("Fit line")
					checked: true
				}

				CheckBox
				{
					name: "rangeScatterPlotOriginLine"
					label: qsTr("Show origin line")
					checked: true
				}

			}

			CheckBox
			{
				name: "rangeRchart"
				label: qsTr("R chart")
			}
		}
	}
}
