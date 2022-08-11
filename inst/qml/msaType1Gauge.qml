
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
			name:								"measurement"
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
			name: 								"referenceValue"
			label: 								qsTr("Reference value")
			defaultValue: 						0
			negativeValues: 					true
			decimals: 							9
			fieldWidth: 						60
		}

		DoubleField
		{
			name: 								"toleranceRange"
			label: 								qsTr("Tolerance range")
			defaultValue: 						1
			negativeValues: 					false
			decimals: 							9
			fieldWidth: 						60
		}

		DoubleField
		{
			name: 								"percentToleranceForCg"
			label: 								qsTr("Percent of tolerance for Cg")
			defaultValue: 						20
			negativeValues: 					false
			min:								0.001
			max:								99.999
		}

		DropDown
		{
			name: 								"studyVarianceMultiplier"
			label: 								qsTr("Study var. (number of std. deviations)")
			id: 								studyVarMultiplier
			indexDefaultValue: 					0
			values: 
			[
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
			name: 								"tTest"
			label: 								qsTr("One sample T-test")
			checked:							true

			CIField
			{
				name: 							"tTestCiLevel"
				label: 							qsTr("Confidence interval for bias")
			}
		}
	}

	Group
	{
		title: 									qsTr("Plots")

		CheckBox
		{
			name: 								"runChart"
			label: 								qsTr("Run chart")
			checked: 							true

			CheckBox
			{
				name: 							"runChartIndividualMeasurementDots"
				label: 							qsTr("Display individual measurements")
				checked: 						true
			}

			CheckBox
			{
				name: 							"runChartToleranceLimitLines"
				label: 							qsTr("Display tolerance limits")
				checked: 						true
			}
		}

		CheckBox
		{
			name: 								"histogram"
			label: 								qsTr("Histogram")

			DropDown
			{
				name:							"histogramBinWidthType"
				label:							qsTr("Bin width type")
				id: 							binWidthType
				indexDefaultValue:				0
				values: [
					{ label: qsTr("Sturges"), value: "sturges"},
					{ label: qsTr("Scott"), value: "scott"},
					{ label: qsTr("Doane"), value: "doane"},
					{ label: qsTr("Freedman-Diaconis"), value: "freedmanDiaconis"},
					{ label: qsTr("Manual"), value: "manual"}
				]
			}

			DoubleField
			{
				name:							"histogramManualNumberOfBins"
				label:							qsTr("Number of bins")
				defaultValue:					30
				min:							3
				max:							10000
				enabled:						binWidthType.currentValue === "manual"
			}

			CheckBox
			{
				name: 							"histogramMeanLine"
				label: 							qsTr("Display mean")
				checked: 						true

				CheckBox
				{
					name: 						"histogramMeanCi"
					label: 						qsTr("Confidence interval for mean")
					checked:					true
					childrenOnSameRow:			true

					CIField
					{
						name: 					"histogramMeanCiLevel"
					}
				}
			}

			CheckBox
			{
				name: 							"histogramReferenceValueLine"
				label: 							qsTr("Display reference value")
				checked: 						true
			}
		}
	}
}
