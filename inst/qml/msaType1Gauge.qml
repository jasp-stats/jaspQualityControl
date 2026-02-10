
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
			label: 								qsTr("Reference/master value")
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
			max:								100
		}

		DropDown
		{
			name: 								"studyVarianceMultiplier"
			label: 								qsTr("Number of std. dev. for instrument variation")
			id: 								studyVarMultiplier
			indexDefaultValue: 					0
			values: 
			[
				{ label: qsTr("6"), value: 6},
				{ label: qsTr("4"), value: 4}
			]
		}
	}
	
	Group
	{
		title: 									qsTr("Bias study options")


		CheckBox
		{
			name: 								"biasTable"
			label: 								qsTr("Bias and instrument capability table")
			checked: 							true
		}

		CheckBox
		{
			name: 								"tTest"
			label: 								qsTr("One sample t-test")
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
				label: 							qsTr("Display boundaries of the reference interval")
				checked: 						true
			}
		}

		CheckBox
		{
			name: 								"histogram"
			label: 								qsTr("Histogram")

			DropDown
			{
				name: 					"histogramBinBoundaryDirection"
				id: 					histogramBinBoundaryDirection
				label: 					qsTr("Histogram bin boundaries")
				values: 
				[
					{ label: qsTr("Left open"),		value: "left"},
					{ label: qsTr("Right open"),	value: "right"}
					
				]
			}

			DropDown
			{
				name:							"histogramBinWidthType"
				label:							qsTr("Bin width type")
				id: 							binWidthType
				indexDefaultValue:				0
				values: [
					{ label: qsTr("Sturges"), value: "sturges"},
					{ label: qsTr("Scott"), value: "scott"},
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
