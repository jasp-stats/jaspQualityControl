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
	columns:									1

	DropDown
	{
		id: 									gaugeRRdataFormat
		name: 									"gaugeRRdataFormat"
		label: 									qsTr("Data format")
		indexDefaultValue: 						0
		values: [
			{ label: qsTr("Single column"), value: "gaugeRRlongFormat"},
			{ label: qsTr("Across rows"), value: "gaugeRRwideFormat"},
		]
		onValueChanged:
		{
			measurements.itemDoubleClicked(0)
			measurementsLong.itemDoubleClicked(0)
		}
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
			id:									operators
			name:								"operators"
			title:								qsTr("Operators")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal"]
			visible: 							!type3.checked
		}

		AssignedVariablesList
		{
			name:								"parts"
			title:								qsTr("Parts")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal", "scale"]
		}

		AssignedVariablesList
		{
			id:									measurementsLong
			name:								"measurementsLong"
			title:								qsTr("Measurements")
			singleVariable:						true
			visible:							gaugeRRdataFormat.currentValue == "gaugeRRlongFormat"
			allowedColumns:						["scale"]
		}

		AssignedVariablesList
		{
			id:									measurements
			name:								"measurements"
			title:								qsTr("Measurements")
			singleVariable:						false
			visible:							gaugeRRdataFormat.currentValue == "gaugeRRwideFormat"
			allowedColumns:						["scale"]
		}

		CheckBox
		{
			id:									type3
			name:								"Type3"
			label:								qsTr("Type 3 study (automatic equipment)")
			onCheckedChanged:
			{
				operators.itemDoubleClicked(0)
			}
		}
	}

	Group
	{
		title: 									qsTr("ANOVA Method Options")
		columns: 								2

		Group
		{

			DropDown
			{
				id: 							variationReference
				name: 							"standardDeviationReference"
				label: 							qsTr("Process variation based on")
				indexDefaultValue: 				0
				values: [
					{ label: qsTr("Study variation"), value: "studyStandardDeviation" },
					{ label: qsTr("Historical standard deviation"), value: "historicalStandardDeviation" }
				]
			}

			DoubleField
			{
				name:							"historicalStandardDeviationValue"
				label:							qsTr("Historical standard deviation:")
				defaultValue:					3
				min: 						0.000001
				decimals: 					6
				enabled:						variationReference.currentValue == "historicalStandardDeviation"
			}

			CheckBox
			{
				name: 							"gaugeToleranceEnabled"
				label: 							qsTr("Tolerance width")
				childrenOnSameRow: 				true

				DoubleField
				{
					name: 						"tolerance"
					defaultValue: 				10
					min: 						0.000001
					decimals: 					6
				}
			}

			CheckBox
			{
				name: 							"gaugeANOVA"
				label: 							qsTr("r&R table ANOVA method")
				checked: 						true

				DropDown
				{
					name: 						"TypeForFstat"
					label: 						qsTr("Type of model for F-statistic")
					values: [
        					{ label: qsTr("Fixed effects"), value: "FixedEffects"},
            				{ label: qsTr("Random effects"), value: "RandomEffects"}
        					]
					indexDefaultValue: (type3.checked) ? 1 : 2
					enabled:      !type3.checked
				}

				DoubleField
				{
					name: 						"alphaForANOVA"
					label: 						qsTr("Alpha interaction removal")
					fieldWidth: 				60
					defaultValue: 				0.05
					max: 						1
					decimals: 					3
				}

				DropDown
				{
					id: 						studyVarMultiplierType
					name: 						"studyVarMultiplierType"
					label: 						qsTr("Study Var. multiplier type")
					indexDefaultValue: 			0
					values: [
						{ label: qsTr("Std. Deviation"), value: "svmSD" },
						{ label: qsTr("Percent"), value: "svmPercent" }
					]
				}

				DoubleField
				{
					name: 						"studyVarMultiplier"
					label: 						qsTr("Study variation multiplier")
					fieldWidth: 				60
					defaultValue: 				6
					min:						0.001
					max:						99.999
					decimals: 					3
				}

				CheckBox
				{
					name: 						"gaugeVarCompGraph"
					label: 						qsTr("Components of variation")
					checked: 					true
				}
			}
		}

		Group
		{
			title: 								qsTr("Plots")

			CheckBox
			{
				name: 							"gaugeRchart"
				label: 							qsTr("R charts by operator")
			}

			CheckBox
			{
				name:							"gaugeXbarChart"
				label:							qsTr("Average charts by operator")
			}

			CheckBox
			{
				name: 							"gaugeScatterPlotOperators"
				label:							qsTr("Scatter plots operators")
				enabled:						!type3.checked

				CheckBox
				{
					name:						"gaugeScatterPlotFitLine"
					label:						qsTr("Fit line")
				}

				CheckBox
				{
					name:						"gaugeScatterPlotOriginLine"
					label:						qsTr("Show origin line")
				}
			}

			CheckBox
			{
				name:							"gaugeByPart"
				label:							qsTr("Measurements by part plot")

				CheckBox
				{
					name:						"gaugeByPartAll"
					label:						qsTr("Display all measurements")
				}
			}

			CheckBox
			{
				name:							"gaugeByOperator"
				label:							qsTr("Measurements by operator plot")
			}

			CheckBox
			{
				name: 							"gaugeByInteraction"
				label: 							qsTr("Part × operator interaction plot")
			}

			CheckBox
			{
				name: 							"trafficPlot"
				label: 							qsTr("Traffic light chart")
			}
		}
	}

	Section
	{
		title:	qsTr("ANOVA Method Report")
		
		CheckBox
		{
			name: "anovaGaugeReport"
			label: qsTr("Show report")
			id:		anovaGaugeReport
			columns: 2
				
				
			CheckBox
			{
				name:		"reportMetaData"
				label:		qsTr("Show report metadata")
				checked:	true
				columns: 1

				TextField
				{
					id:									anovaGaugeTitle
					label: 								qsTr("Title")
					name: 								"anovaGaugeTitle"
					placeholderText:					qsTr("Measurement")
					fieldWidth:							100
				}

				TextField
				{
					id:									anovaGaugeName
					label: 								qsTr("Gauge name")
					name: 								"anovaGaugeName"
					placeholderText:					qsTr("Name")
					fieldWidth:							100
				}

				TextField
				{
					id:									anovaGaugeDate
					label: 								qsTr("Date")
					name: 								"anovaGaugeDate"
					placeholderText:					qsTr("Date")
					fieldWidth:							100
				}

				TextField
				{
					id:									anovaGaugeReportedBy
					label: 								qsTr("Reported by")
					name: 								"anovaGaugeReportedBy"
					placeholderText:					qsTr("Name")
					fieldWidth:							100
				}

				TextField
				{
					id:									anovaGaugeMisc
					label: 								qsTr("Misc")
					name: 								"anovaGaugeMisc"
					placeholderText:					qsTr("Miscellaneous")
					fieldWidth:							100
				}
			}
		
			Group
			{
				title:			qsTr("Select Report Components")
			
				CheckBox
				{
					name:		"reportVariationComponents"
					label:		qsTr("Show components of variation")
					checked:	true
				}
				
				CheckBox
				{
					name:		"reportMeasurementsByPartPlot"
					label:		qsTr("Show measurements by part")
					checked:	true
				}
			
				CheckBox
				{
					name:		"reportRChartByOperator"
					label:		qsTr("Show R chart by operator")
					checked:	true
				}
				
				CheckBox
				{
					name:		"reportMeasurementsByOperatorPlot"
					label:		qsTr("Show measurements by operator")
					checked:	true
				}
				
				CheckBox
				{
					name:		"reportAverageChartByOperator"
					label:		qsTr("Show average chart by operator")
					checked:	true
				}
				
				CheckBox
				{
					name:		"reportPartByOperatorPlot"
					label:		qsTr("Show part × operator interaction")
					checked:	true
				}
				
				CheckBox
				{
					name:		"reportTrafficLightCHart"
					label:		qsTr("Show traffic light chart")
					checked:	true
				}
			}
		}
	}
}
