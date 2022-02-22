
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
	columns:									2

	VariablesForm
	{
		id:										variablesForm

		AvailableVariablesList
		{
			name:								"variablesForm"
		}

		AssignedVariablesList
		{
			name:								"measurements"
			title:								qsTr("Measurement")
			singleVariable:						true
			allowedColumns:						["scale"]
		}
	}

	Group
	{
	title: 									qsTr("Analysis options")

		DoubleField
		{
			name: 								"biasReferenceValue"
			label: 								qsTr("Reference value")
			defaultValue: 						0
			negativeValues: 					true
			decimals: 							5
			fieldWidth: 						60
		}

		DoubleField
		{
			name: 								"biasTolerance"
			label: 								qsTr("Tolerance range")
			defaultValue: 						1
			negativeValues: 					true
			decimals: 							5
			fieldWidth: 						60
		}

		DoubleField
		{
			name: 								"biasPercentCG"
			label: 								qsTr("Percent of tolerance for Cg")
			defaultValue: 						20
			negativeValues: 					false
			min:								0.001
			max:								99.999
		}

		DropDown
		{
			id: 								studyVarMultiplier
			name: 								"BiasStudyVarMultiplier"
			label: 								qsTr("Study var. (number of std. deviations)")
			indexDefaultValue: 					0
			values: [
				{ label: qsTr("6"), value: 6},
				{ label: qsTr("4"), value: 4}
			]
		}

		CheckBox
		{
			name: 								"biasTable"
			label: 								qsTr("Bias and instrument capability table")
			checked: 							true
		}

		CheckBox
		{
			name: 								"biasTtest"
			label: 								qsTr("One sample T-test")
			checked:							true

			CIField
			{
				name: 							"biasTtestConfidenceIntervalPercent"
				label: 							qsTr("Confidence interval for bias")
			}
		}
	}

	Group
	{
		title: 									qsTr("Plots")

		CheckBox
		{
			name: 								"biasRun"
			label: 								qsTr("Run chart")
			checked: 							true

			CheckBox
			{
				name: 							"biasRunDots"
				label: 							qsTr("Display individual measurements")
				checked: 						true
			}

			CheckBox
			{
				name: 							"biasRunTolLims"
				label: 							qsTr("Display tolerance limits")
				checked: 						true
			}
		}

		CheckBox
		{
			name: 								"biasHistogram"
			label: 								qsTr("Histogram")

			DropDown
			{
				id: 							binWidthType
				name:							"biasBinWidthType"
				label:							qsTr("Bin width type")
				indexDefaultValue:				0
				values: [
					{ label: qsTr("Sturges"), value: "sturges"},
					{ label: qsTr("Scott"), value: "scott"},
					{ label: qsTr("Doane"), value: "doane"},
					{ label: qsTr("Freedman-Diaconis"), value: "fd"},
					{ label: qsTr("Manual"), value: "manual"}
				]
			}

			DoubleField
			{
				name:							"biasNumberOfBins"
				label:							qsTr("Number of bins")
				defaultValue:					30
				min:							3
				max:							10000
				enabled:						binWidthType.currentValue === "manual"
			}

			CheckBox
			{
				name: 							"biasHistMean"
				label: 							qsTr("Display mean")
				checked: 						true

				CheckBox
				{
					name: 						"biasHistMeanConfidenceInterval"
					label: 						qsTr("Confidence interval for mean")
					checked:					true
					childrenOnSameRow:			true

					CIField
					{
						name: 					"biasHistMeanConfidenceIntervalPercent"
					}
				}
			}

			CheckBox
			{
				name: 							"biasHistRef"
				label: 							qsTr("Display reference value")
				checked: 						true
			}
		}
	}
}
