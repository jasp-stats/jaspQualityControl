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

import "./common" as Common

Form
{
	function sortIntervalValues() {

		var values = [
			interval1.displayValue,
			interval2.displayValue,
			interval3.displayValue,
			interval4.displayValue
		]
		values.sort(function(a, b) { return a - b })
		interval1.value = values[0]
		interval2.value = values[1]
		interval3.value = values[2]
		interval4.value = values[3]
		interval1b.value = values[0]
		interval2b.value = values[1]
		interval3b.value = values[2]
		interval4b.value = values[3]
	}
	function sortIntervalValuesb() {

		var values = [
			interval1b.displayValue,
			interval2b.displayValue,
			interval3b.displayValue,
			interval4b.displayValue
		]
		values.sort(function(a, b) { return a - b })
		interval1.value = values[0]
		interval2.value = values[1]
		interval3.value = values[2]
		interval4.value = values[3]
		interval1b.value = values[0]
		interval2b.value = values[1]
		interval3b.value = values[2]
		interval4b.value = values[3]
	}
	columns:	 2

	DropDown
	{
		name: "dataFormat"
		label: qsTr("Data format")
		id: dataFormat
		indexDefaultValue: 0
		values:
		[
			{label: qsTr("Single column"),			value: "longFormat"},
			{label: qsTr("Across rows"),			value: "wideFormat"},
		]
		// onValueChanged:
		// {
		// 	measurementLongFormat.itemDoubleClicked(0)
		// 	measurementsWideFormat.itemDoubleClicked(0)
		// }
	}

	VariablesForm
	{
		id:								variablesFormLongFormat
		visible:							dataFormat.currentValue === "longFormat"

		AvailableVariablesList
		{
			name:							"variablesFormLongFormat"
		}

		AssignedVariablesList
		{
			name:							"measurementLongFormat"
			title:							qsTr("Measurement")
			id:								measurementLongFormat
			allowedColumns:					["scale"]
			singleVariable:					true
		}

		AssignedVariablesList
		{
			name:							"subgroup"
			title:						 	qsTr("Subgroup")
			id:					 			subgroup
			singleVariable:		 			true
			allowedColumns:					["nominal"]
			enabled: 						subgroupSizeType.value === "groupingVariable"
		}

		AssignedVariablesList
		{
			id:									stagesLongFormat
			name:								"stagesLongFormat"
			title:								qsTr("Stages")
			singleVariable:						true
			allowedColumns:						["nominal"]
		}
	}

	VariablesForm
	{
		id:								variablesFormWideFormat
		visible:							dataFormat.currentValue === "wideFormat"

		AvailableVariablesList
		{
			name:						"variablesFormWideFormat"
		}

		AssignedVariablesList
		{
			name:							"measurementsWideFormat"
			title:							qsTr("Measurements")
			id:								measurementsWideFormat
			allowedColumns:					["scale"]
		}

		AssignedVariablesList
		{
			id:									axisLabels
			name:								"axisLabels"
			title:								qsTr("Timestamp (optional)")
			singleVariable:						true
			allowedColumns:						["nominal"]
		}

		AssignedVariablesList
		{
			id:									stagesWideFormat
			name:								"stagesWideFormat"
			title:								qsTr("Stages")
			singleVariable:						true
			allowedColumns:						["nominal"]
		}
	}

	Group
	{
		columns:							2

		RadioButtonGroup
		{
			name:					"subgroupSizeType"
			title:					qsTr("Specify subgroups")
			id:						subgroupSizeType
			visible:					dataFormat.currentValue === "longFormat"

			RadioButton
			{
				value: 				"manual"
				label: 				qsTr("Subgroup size")
				checked:		 		true
				childrenOnSameRow:	true

				DoubleField
				{
					name: 									"manualSubgroupSizeValue"
					id:										manualSubgroupSizeValue
					min: 									1
					max:									dataSetModel.rowCount()
					negativeValues:							false
					defaultValue:							5

				}
			}

			RadioButton
			{
				value: 							"groupingVariable"
				label: 							qsTr("Through grouping variable")

				DropDown
				{
					name: 				"groupingVariableMethod"
					id: 					groupingVariable
					label: 				"Grouping method"
					values:
					[
						{ label: qsTr("Subgroup value change"),			value: "newLabel"},
						{ label: qsTr("Same subgroup value"),			value: "sameLabel"}
					]
					indexDefaultValue: 0
				}
			}
		}

		/*
		RadioButtonGroup
		{
			name:								"subgroupSizeUnequal"
			title: 								qsTr("Unequal subgroup sizes")
			id:									subgroupSizeUnequal

			RadioButton
			{
				value: 								"actualSizes"
				label: 								qsTr("Use actual sizes")
				checked: 							true
			}

			RadioButton
			{
				value: 								"fixedSubgroupSize"
				label: 								qsTr("Use fixed subgroup size")
				childrenOnSameRow:		 			true

				IntegerField
				{
					name: 								"fixedSubgroupSizeValue"
					fieldWidth: 						30
					defaultValue: 						5
					min:								2
				}
			}
		}
		*/
	}

	Section
	{
		title: qsTr("Process capability options")

		Group
		{
			title:					qsTr("Type of data distribution")


			RadioButtonGroup
			{
				name: 					"capabilityStudyType"
				id: 					capabilityStudyType

				RadioButton
				{
					name: 				"normalCapabilityAnalysis"
					id : 				normalCapabilityAnalysis
					label: 				qsTr("Normal distribution")
					checked: 			true
				}

				RadioButton
				{
					name: 				"tCapabilityAnalysis"
					id : 				tCapabilityAnalysis
					label: 				qsTr("Student's t-distribution")
					// checked: 			true
				}

			}
		}

		Group
		{
			title: qsTr("Metrics")
			info: qsTr("Select the process capability metrics to report.")
			CheckBox { name: "Cp";   label: qsTr("Cp");  checked: true }
			CheckBox { name: "Cpu";	 label: qsTr("Cpu"); checked: true }
			CheckBox { name: "Cpl";	 label: qsTr("Cpl"); checked: true }
			CheckBox { name: "Cpk";	 label: qsTr("Cpk"); checked: true }
			CheckBox { name: "Cpc";	 label: qsTr("Cpc"); checked: true }
			CheckBox { name: "Cpm";	 label: qsTr("Cpm"); checked: true }
		}

		Group
		{
			title: 							qsTr("Capability study")

			CheckBox
			{
				name: 						"lowerSpecificationLimit"
				label: 						qsTr("Lower specification limit")
				id:							lowerSpecificationLimit
				childrenOnSameRow:			true

				DoubleField
				{
					name: 					"lowerSpecificationLimitValue"
					id:						lowerSpecificationLimitValue
					negativeValues:			true
					defaultValue:			-1
					decimals:				9
				}

			}

			CheckBox
			{
				name: 						"target"
				label: 						qsTr("Target value")
				id:							target
				childrenOnSameRow:			true

				DoubleField
				{
					name: 					"targetValue"
					id:						targetValue
					negativeValues:			true
					defaultValue:			0
					decimals:				9
				}
			}

			CheckBox
			{
				name: 						"upperSpecificationLimit"
				label: 						qsTr("Upper specification limit")
				id:							upperSpecificationLimit
				childrenOnSameRow:			true

				DoubleField
				{
					name: 					"upperSpecificationLimitValue"
					id:						upperSpecificationLimitValue
					negativeValues:			true
					defaultValue:			1
					decimals:				9
				}

			}

		}

		Group
		{

			title: qsTr("Process Criteria")
			GridLayout
			{
				// title: qsTr("Process Criteria")
				columns: 5
				columnSpacing: 2
				rowSpacing: jaspTheme.rowGridSpacing / 3
				id: intervalRow
				property int dbWidth: 50
				property int txtWidth: 100

				// Row 0: Headers
				Label {text: qsTr("Left bound")}
				Item{}
				Label {text: qsTr("Classification")}
				Item{}
				Label {text: qsTr("Right bound")}

				// Row 1: Incapable
				Item{}
				Item{}
				TextField { name: "intervalLabel1"; defaultValue: qsTr("Incapable"); fieldWidth: intervalRow.txtWidth}
				Label { text: "<"; }
				DoubleField { name: "interval1"; id: interval1; fieldWidth: intervalRow.dbWidth; defaultValue: 1.00; onEditingFinished: sortIntervalValues() }

				// Row 2: Capable
				DoubleField { name: "interval1b";id: interval1b; fieldWidth: intervalRow.dbWidth; editable: true; value: interval1.value; onEditingFinished: {sortIntervalValuesb()}  }
				Label { text: "<"; }
				TextField { name: "intervalLabel2"; defaultValue: qsTr("Capable"); fieldWidth: intervalRow.txtWidth}
				Label { text: "≤"; }
				DoubleField { name: "interval2"; id: interval2; fieldWidth: intervalRow.dbWidth; defaultValue: 1.33; onEditingFinished: sortIntervalValues() }

				// Row 3: Satisfactory
				DoubleField { name: "interval2b"; id: interval2b; fieldWidth: intervalRow.dbWidth; editable: true; value: interval2.value; onEditingFinished: {sortIntervalValuesb()}  }
				Label { text: "<"; }
				TextField { name: "intervalLabel3"; defaultValue: qsTr("Satisfactory"); fieldWidth: intervalRow.txtWidth}
				Label { text: "≤"; }
				DoubleField { name: "interval3"; id: interval3; fieldWidth: intervalRow.dbWidth; defaultValue: 1.50; onEditingFinished: sortIntervalValues() }

				// Row 4: Excellent
				DoubleField { name: "interval3b"; id: interval3b; fieldWidth: intervalRow.dbWidth; editable: true; value: interval3.value; onEditingFinished: {sortIntervalValuesb()}  }
				Label { text: "<"; }
				TextField { name: "intervalLabel4"; defaultValue: qsTr("Excellent"); fieldWidth: intervalRow.txtWidth}
				Label { text: "≤"; }
				DoubleField { name: "interval4"; id: interval4; fieldWidth: intervalRow.dbWidth; defaultValue: 2.00; onEditingFinished: sortIntervalValues() }

				// Row 5: Super
				DoubleField { name: "interval4b"; id: interval4b; fieldWidth: intervalRow.dbWidth; editable: true; value: interval4.value; onEditingFinished: {sortIntervalValuesb()}  }
				Label { text: ">"; }
				TextField { name: "intervalLabel5"; defaultValue: qsTr("Super"); fieldWidth: intervalRow.txtWidth}
				Item{}
				Item{}
			}
		}


	}

	// Section
	// {
	// 	title: qsTr("Prior distributions")

	// }

	Section
	{
		title: qsTr("Tables")
		CheckBox
		{
			name: "intervalTable"
			label: qsTr("Interval table")
			info: qsTr("Show the posterior probabilities of the interval specified with the input on the right. Note that the input is automatically sorted and that the first and last fields are always negative and positive infinity.")
		}
		CIField
		{
			name: "credibleIntervalWidth"
			label: qsTr("Credible interval")
			info: qsTr("Width of the credible interval used for the posterior distribution in the Capability table.")
		}
	}

	Section
	{

		title: qsTr("Prior and Posterior Inference")

		Common.PlotLayout {}

		Common.PlotLayout
		{
			baseName: "priorDistributionPlot"
			baseLabel: qsTr("Prior distribution")
			hasPrior: false
		}

	}

	Section
	{
		title: qsTr("Sequential Analysis")

		Common.PlotLayout
		{
			baseName: "sequentialAnalysisPointEstimatePlot"
			baseLabel: qsTr("Point estimate plot")
			hasPrior: false
		}

		Common.PlotLayout
		{
			baseName: "sequentialAnalysisPointIntervalPlot"
			baseLabel: qsTr("Interval estimate plot")
			hasPrior: false
			hasEstimate: false
			hasCi: false
			hasType: true
		}

		Group
		{
			CheckBox
			{
				name:		"sequentialAnalysisPlotAdditionalInfo"
				label:		qsTr("Show process criteria")
				checked:	true
				info:		qsTr("Add a secondary right axis with condition bounds for the process")
			}

			CheckBox
			{
				enabled:	sequentialAnalysisPointEstimatePlot.checked || sequentialAnalysisIntervalEstimatePlot.checked
				name:		"sequentialAnalysisUpdatingTable"
				label:		qsTr("Posterior updating table")
				checked:	false
				info:		qsTr("Show the data from the sequential analysis in a table. Will show both the information for the point estimate and interval estimate plots, if both are selected.")
			}
		}
	}

	Section
	{

		title: qsTr("Prior and Posterior Predictive Plots")

		Common.PlotLayout
		{
			baseName: "posteriorPredictiveDistributionPlot"
			baseLabel: qsTr("Posterior predictive distribution")
			hasPrior: false
		}

		Common.PlotLayout
		{
			baseName: "priorPredictiveDistributionPlot"
			baseLabel: qsTr("Prior predictive distribution")
			hasPrior: false
		}

	}

	Section
	{
		title: qsTr("Prior distributions")

		Common.Priors
		{
			baseName: "populationMeanPrior"
			baseLabel: qsTr("Population mean")
			fullRealLLine: true
		}

		Common.Priors
		{
			baseName: "populationSigmaPrior"
			baseLabel: qsTr("Population standard deviation")
			fullRealLLine: false
		}

		Common.Priors
		{
			baseName: "populationDfPrior"
			baseLabel: qsTr("Population degrees of freedom")
			fullRealLLine: false
			enabled: capabilityStudyType.value === "tCapabilityAnalysis"
			hasJeffreys: false
		}

	}

		Section
	{
		title: qsTr("New Prior distributions")

		Common.PriorsNew
		{
			priorType: capabilityStudyType.value === "normalCapabilityAnalysis" ? "normalModel" : "tModel"
		}

	}

	Section
	{
		title: qsTr("Advanced options")
	}
}
