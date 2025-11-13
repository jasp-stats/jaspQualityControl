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

		// CheckBox
		// {
		// 	name: 						"posteriorDistributionPlot"
		// 	label: 						qsTr("Posterior distribution")

		// 	// Group so the options are shown in a 2-column layout
		// 	Group
		// 	{

		// 		columns: 					2
		// 		columnSpacing: 				10 * jaspTheme.columnGroupSpacing

		// 		// Group so point estimate and CI options are shown in a single column
		// 		Group
		// 		{
		// 			CheckBox
		// 			{
		// 				label:				qsTr("Point estimate")
		// 				name:				"posteriorDistributionPlotIndividualPointEstimate"
		// 				childrenOnSameRow:	true

		// 				DropDown
		// 				{
		// 					name:	"posteriorDistributionPlotIndividualPointEstimateType"
		// 					label:	""
		// 					values:	[
		// 						{label: qsTr("mean"),					value: "mean"},
		// 						{label: qsTr("median"),					value: "median"},
		// 						{label: qsTr("mode"),					value: "mode"}
		// 					]
		// 				}
		// 			}

		// 			// Group so CI checkbox and options are shown in a single column (with subgroup so CI options are indented)
		// 			Group
		// 			{
		// 				columns: 1
		// 				CheckBox
		// 				{
		// 					name:				"posteriorDistributionPlotIndividualCi"
		// 					label:				qsTr("CI")
		// 					id:					posteriorPlotIndividualCI
		// 					childrenOnSameRow:	true

		// 					DropDown
		// 					{
		// 						name:		"posteriorDistributionPlotIndividualCiType"
		// 						label:		""
		// 						id:			posteriorPlotIndividualType
		// 						values:		[
		// 							{label: qsTr("central"),				value: "central"},
		// 							{label: qsTr("HPD"),					value: "HPD"},
		// 							{label: qsTr("custom"),					value: "custom"}//,
		// 							// {label: qsTr("support"),				value: "support"}
		// 						]
		// 					}
		// 				}

		// 				Group
		// 				{
		// 					columns: 2
		// 					indent: true
		// 					enabled:		posteriorPlotIndividualCI.checked

		// 					CIField
		// 					{
		// 						visible:		posteriorPlotIndividualType.currentValue === "central" || posteriorPlotIndividualType.currentValue === "HPD"
		// 						name:			"posteriorDistributionPlotIndividualCiMass"
		// 						label:			qsTr("Mass")
		// 						fieldWidth:		50
		// 						defaultValue: 	95
		// 						min:			1
		// 						max:			100
		// 						inclusive:		JASP.MinMax
		// 					}

		// 					DoubleField
		// 					{
		// 						visible:		posteriorPlotIndividualType.currentValue === "custom"
		// 						name:			"posteriorDistributionPlotIndividualCiLower"
		// 						label:			qsTr("Lower")
		// 						id:				plotsPosteriorLower
		// 						fieldWidth:		50
		// 						defaultValue:	0
		// 						negativeValues: true
		// 						inclusive:		JASP.MinMax
		// 					}

		// 					DoubleField
		// 					{
		// 						visible:		posteriorPlotIndividualType.currentValue === "custom"
		// 						name:			"posteriorDistributionPlotIndividualCiUpper"
		// 						label:			qsTr("Upper")
		// 						id:				plotsPosteriorUpper
		// 						fieldWidth:		50
		// 						defaultValue:	1
		// 						negativeValues: true
		// 						inclusive:		JASP.MinMax
		// 					}

		// 					FormulaField
		// 					{
		// 						visible:		posteriorPlotIndividualType.currentValue === "support"
		// 						name:			"posteriorDistributionPlotIndividualCiBf"
		// 						label:			qsTr("BF")
		// 						fieldWidth:		50
		// 						defaultValue:	"1"
		// 						min:			0
		// 						inclusive:		JASP.None
		// 					}
		// 				}
		// 			}
		// 		}

		// 		// Common.PlotLayout {} // <- would be better but does not work...
		// 		RadioButtonGroup
		// 		{
		// 			name:		"posteriorDistributionPlotPanelLayout"
		// 			title:		qsTr("Layout")
		// 			id:			posteriorDistributionPlotPanelLayout

		// 			RadioButton { value: "multiplePanels"; 	label: qsTr("One plot per metric"); 	checked: true	}
		// 			RadioButton { value: "singlePanel";		label: qsTr("All metrics in one plot") 					}

		// 		}

		// 		RadioButtonGroup
		// 		{
		// 			name:		"posteriorDistributionPlotAxes"
		// 			title:		qsTr("Axes")
		// 			id:			posteriorDistributionPlotAxes

		// 			RadioButton { value: "identical"; 	label: qsTr("Automatic"); 					checked: true	}
		// 			RadioButton { value: "automatic";	label: qsTr("Identical across panels"); 	enabled: posteriorDistributionPlotPanelLayout.value === "multiplePanels"	}
		// 			RadioButton { value: "custom";		label: qsTr("Custom axes");	}
		// 		}

		// 		Group
		// 		{

		// 		title: qsTr("Custom axes")
		// 		enabled: posteriorDistributionPlotAxes.value === "custom"
		// 		visible: posteriorDistributionPlotAxes.value === "custom"

		// 			GridLayout
		// 			{
		// 				columns: 5
		// 				columnSpacing: 2
		// 				rowSpacing: jaspTheme.rowGridSpacing / 3
		// 				id: customAxesLayout
		// 				property int dbWidth: 50
		// 				property int txtWidth: 100

		// 				// Row 0: Headers
		// 				Label {text: qsTr("Axis")}
		// 				Item{}
		// 				Label {text: qsTr("Min")}
		// 				Item{}
		// 				Label {text: qsTr("Max")}

		// 				// Row 1: x axis
		// 				Label { text: qsTr("x axis"); }
		// 				Item{}
		// 				DoubleField { name: "custom_x_min"; id: custom_x_min; fieldWidth: customAxesLayout.dbWidth; defaultValue: 0.00; negativeValues: true; max: custom_x_max.value}
		// 				Item{}
		// 				DoubleField { name: "custom_x_max"; id: custom_x_max; fieldWidth: customAxesLayout.dbWidth; defaultValue: 1.00; negativeValues: true; min: custom_x_min.value}

		// 				// Row 2: y axis
		// 				Label { text: qsTr("y axis"); }
		// 				Item{}
		// 				DoubleField { name: "custom_y_min"; id: custom_y_min; fieldWidth: customAxesLayout.dbWidth; defaultValue: 0.00; negativeValues: false; max: custom_y_max.value}
		// 				Item{}
		// 				DoubleField { name: "custom_y_max"; id: custom_y_max; fieldWidth: customAxesLayout.dbWidth; defaultValue: 1.00; negativeValues: false; min: custom_y_min.value}
		// 			}
		// 		}

		// 		CheckBox
		// 		{
		// 			name:		"posteriorDistributionPlotPriorDistribution"
		// 			label:		qsTr("Show prior distribution")
		// 			checked:	false
		// 		}
		// 	}
		// }

		// CheckBox
		// {
		// 	name: 						"priorDistributionPlot"
		// 	label: 						qsTr("Prior distribution")

		// 	// Group so the options are shown in a 2-column layout
		// 	Group
		// 	{

		// 		columns: 					2
		// 		columnSpacing: 				10 * jaspTheme.columnGroupSpacing

		// 		// Group so point estimate and CI options are shown in a single column
		// 		Group
		// 		{
		// 			CheckBox
		// 			{
		// 				label:				qsTr("Point estimate")
		// 				name:				"priorDistributionPlotIndividualPointEstimate"
		// 				childrenOnSameRow:	true

		// 				DropDown
		// 				{
		// 					name:	"priorDistributionPlotIndividualPointEstimateType"
		// 					label:	""
		// 					values:	[
		// 						{label: qsTr("mean"),					value: "mean"},
		// 						{label: qsTr("median"),					value: "median"},
		// 						{label: qsTr("mode"),					value: "mode"}
		// 					]
		// 				}
		// 			}

		// 			// Group so CI checkbox and options are shown in a single column (with subgroup so CI options are indented)
		// 			Group
		// 			{
		// 				columns: 1
		// 				CheckBox
		// 				{
		// 					name:				"priorDistributionPlotIndividualCi"
		// 					label:				qsTr("CI")
		// 					id:					priorPlotIndividualCI
		// 					childrenOnSameRow:	true

		// 					DropDown
		// 					{
		// 						name:		"priorDistributionPlotIndividualCiType"
		// 						label:		""
		// 						id:			priorPlotIndividualType
		// 						values:		[
		// 							{label: qsTr("central"),				value: "central"},
		// 							{label: qsTr("HPD"),					value: "HPD"},
		// 							{label: qsTr("custom"),					value: "custom"}//,
		// 							// {label: qsTr("support"),				value: "support"}
		// 						]
		// 					}
		// 				}

		// 				Group
		// 				{
		// 					columns: 2
		// 					indent: true
		// 					enabled:		priorPlotIndividualCI.checked

		// 					CIField
		// 					{
		// 						visible:		priorPlotIndividualType.currentValue === "central" || priorPlotIndividualType.currentValue === "HPD"
		// 						name:			"priorDistributionPlotIndividualCiMass"
		// 						label:			qsTr("Mass")
		// 						fieldWidth:		50
		// 						defaultValue: 	95
		// 						min:			1
		// 						max:			100
		// 						inclusive:		JASP.MinMax
		// 					}

		// 					DoubleField
		// 					{
		// 						visible:		priorPlotIndividualType.currentValue === "custom"
		// 						name:			"priorDistributionPlotIndividualCiLower"
		// 						label:			qsTr("Lower")
		// 						id:				plotspriorLower
		// 						fieldWidth:		50
		// 						defaultValue:	0
		// 						negativeValues: true
		// 						inclusive:		JASP.MinMax
		// 					}

		// 					DoubleField
		// 					{
		// 						visible:		priorPlotIndividualType.currentValue === "custom"
		// 						name:			"priorDistributionPlotIndividualCiUpper"
		// 						label:			qsTr("Upper")
		// 						id:				plotspriorUpper
		// 						fieldWidth:		50
		// 						defaultValue:	1
		// 						negativeValues: true
		// 						inclusive:		JASP.MinMax
		// 					}

		// 					FormulaField
		// 					{
		// 						visible:		priorPlotIndividualType.currentValue === "support"
		// 						name:			"priorDistributionPlotIndividualCiBf"
		// 						label:			qsTr("BF")
		// 						fieldWidth:		50
		// 						defaultValue:	"1"
		// 						min:			0
		// 						inclusive:		JASP.None
		// 					}
		// 				}
		// 			}
		// 		}

		// 		RadioButtonGroup
		// 		{
		// 			name:		"priorDistributionPlotPanelLayout"
		// 			title:		qsTr("Layout")
		// 			id:			priorDistributionPlotPanelLayout

		// 			RadioButton { value: "multiplePanels"; 	label: qsTr("One plot per metric"); 	checked: true	}
		// 			RadioButton { value: "singlePanel";		label: qsTr("All metrics in one plot") 					}

		// 		}

		// 		RadioButtonGroup
		// 		{
		// 			name:		"priorDistributionPlotAxes"
		// 			title:		qsTr("Axes")
		// 			id:			priorDistributionPlotAxes

		// 			RadioButton { value: "identical"; 	label: qsTr("Automatic"); 					checked: true	}
		// 			RadioButton { value: "automatic";	label: qsTr("Identical across panels"); 	enabled: priorDistributionPlotPanelLayout.value === "multiplePanels"	}
		// 			RadioButton { value: "custom";		label: qsTr("Custom axes");	}
		// 		}

		// 		Group
		// 		{

		// 		title: qsTr("Custom axes")
		// 		enabled: priorDistributionPlotAxes.value === "custom"
		// 		visible: priorDistributionPlotAxes.value === "custom"

		// 			GridLayout
		// 			{
		// 				columns: 5
		// 				columnSpacing: 2
		// 				rowSpacing: jaspTheme.rowGridSpacing / 3
		// 				id: priorCustomAxesLayout
		// 				property int dbWidth: 50
		// 				property int txtWidth: 100

		// 				// Row 0: Headers
		// 				Label {text: qsTr("Axis")}
		// 				Item{}
		// 				Label {text: qsTr("Min")}
		// 				Item{}
		// 				Label {text: qsTr("Max")}

		// 				// Row 1: x axis
		// 				Label { text: qsTr("x axis"); }
		// 				Item{}
		// 				DoubleField { name: "prior_custom_x_min"; id: prior_custom_x_min; fieldWidth: priorCustomAxesLayout.dbWidth; defaultValue: 0.00; negativeValues: true; max: prior_custom_x_max.value}
		// 				Item{}
		// 				DoubleField { name: "prior_custom_x_max"; id: prior_custom_x_max; fieldWidth: priorCustomAxesLayout.dbWidth; defaultValue: 1.00; negativeValues: true; min: prior_custom_x_min.value}

		// 				// Row 2: y axis
		// 				Label { text: qsTr("y axis"); }
		// 				Item{}
		// 				DoubleField { name: "prior_custom_y_min"; id: prior_custom_y_min; fieldWidth: priorCustomAxesLayout.dbWidth; defaultValue: 0.00; negativeValues: false; max: prior_custom_y_max.value}
		// 				Item{}
		// 				DoubleField { name: "prior_custom_y_max"; id: prior_custom_y_max; fieldWidth: priorCustomAxesLayout.dbWidth; defaultValue: 1.00; negativeValues: false; min: prior_custom_y_min.value}
		// 			}
		// 		}
		// 	}
		// }

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

		// CheckBox
		// {
		// 	name:		"sequentialAnalysisPointEstimatePlot"
		// 	label:		qsTr("Point estimate plot")
		// 	id:			sequentialAnalysisPointEstimatePlot

		// 	Group
		// 	{
		// 		columns: 					2
		// 		columnSpacing: 				10 * jaspTheme.columnGroupSpacing

		// 		DropDown
		// 		{
		// 			label:		qsTr("Type")
		// 			name:		"sequentialAnalysisPlotPointEstimateType"
		// 			values:		[
		// 				{label: qsTr("mean"),					value: "mean"},
		// 				{label: qsTr("median"),					value: "median"}
		// 				// mode?
		// 			]
		// 		}

		// 		CheckBox
		// 		{
		// 			name:				"sequentialAnalysisPlotPointEstimateCi"
		// 			label:				qsTr("CI")
		// 			childrenOnSameRow:	true

		// 			CIField
		// 			{
		// 				name:				"sequentialAnalysisPlotPointEstimateCiMass"
		// 				label:				qsTr("Mass")
		// 				fieldWidth:			50
		// 				defaultValue:		95
		// 				min:				1
		// 				max:				100
		// 				inclusive:			JASP.MaxOnly
		// 			}
		// 		}

		// 	}
		// }

		// CheckBox
		// {
		// 	name:		"sequentialAnalysisIntervalEstimatePlot"
		// 	label:		qsTr("Interval estimate plot")
		// 	id:			sequentialAnalysisIntervalEstimatePlot

		// 	DropDown
		// 	{
		// 		label:		qsTr("Type")
		// 		name:		"sequentialAnalysisPlotPointEstimateType"
		// 		values:		[
		// 			{label: qsTr("mean"),					value: "mean"},
		// 			{label: qsTr("median"),					value: "median"}
		// 			// mode?
		// 		]
		// 	}

		// 	CheckBox
		// 	{
		// 		name:				"sequentialAnalysisPlotCi"
		// 		label:				qsTr("CI")
		// 		childrenOnSameRow:	true

		// 		CIField
		// 		{
		// 			name:				"sequentialAnalysisPlotCiMass"
		// 			label:				qsTr("Mass")
		// 			fieldWidth:			50
		// 			defaultValue:		95
		// 			min:				1
		// 			max:				100
		// 			inclusive:			JASP.MaxOnly
		// 		}
		// 	}

		// 	CheckBox
		// 	{
		// 		name:		"sequentialAnalysisPlotAdditionalInfo"
		// 		label:		qsTr("Show process criteria")
		// 		checked:	true
		// 		info:		qsTr("Add a secondary right axis with condition bounds for the process")
		// 	}
		// }

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
		title: qsTr("Prior distribution")
	}

	Section
	{
		title: qsTr("Advanced options")
	}
}
