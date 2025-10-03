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

// import "./common" as Common

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
			{label: qsTr("Across rows"),				value: "wideFormat"},
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

		ColumnLayout
		{
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

					/*
					RadioButton
					{
						name:				"nonNormalCapabilityAnalysis"
						id :				nonNormalCapabilityAnalysis
						label:				qsTr("Non-normal distribution")

						DropDown
						{
							name: 					"nonNormalDistribution"
							id: 					nonNormalDistribution
							label:					qsTr("Specify a distribution")
							values:
							[
								{label: qsTr("Weibull"),					value: "weibull"},
								{label: qsTr("Lognormal"),					value: "lognormal"},
								{label: qsTr("3-parameter Weibull"),		value: "3ParameterWeibull"},
								{label: qsTr("3-parameter lognormal"),		value: "3ParameterLognormal"}
							]
							indexDefaultValue: 0
						}

						DropDown
						{
							name:					"nonNormalMethod"
							label:					qsTr("Non-normal capability statistics")
							indexDefaultValue:		0
							values:
								[
								{label: qsTr("Percentile"),				value: "percentile"},
								{label: qsTr("Non-conformance"),		value: "nonConformance"  }
							]
						}
					}
					*/
				}
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

					// CheckBox
					// {
					// 	name: 						"lowerSpecificationLimitBoundary"
					// 	label: 						qsTr("Boundary")
					// 	id:							lowerSpecificationLimitBoundary
					// }
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

					// CheckBox
					// {
					// name: 						"upperSpecificationLimitBoundary"
					// label: 						qsTr("Boundary")
					// id:							upperSpecificationLimitBoundary
					// }
				}

				CIField 	{ name: "credibleIntervalWidth"; label: qsTr("Credible interval") }


				/*
				CheckBox
				{
					name: 						"processCapabilityPlot"
					label: 						qsTr("Process capability plot")
					checked: 					true
					enabled:					upperSpecificationLimit.checked || lowerSpecificationLimit.checked

					DoubleField
					{
						name:						"processCapabilityPlotBinNumber"
						label:						qsTr("Number of bins")
						defaultValue:				10
						min:						3;
						max:						10000;
						enabled:					csBinWidthType.currentValue === "manual"
					}

					CheckBox
					{
						name: 								"processCapabilityPlotDistributions"
						label: 								qsTr("Overlay distribution")
						checked: 							true
					}

					CheckBox
					{
						name: 								"processCapabilityPlotSpecificationLimits"
						label: 								qsTr("Display specification limits")
						checked: 							true
					}
				}

				CheckBox
				{
					name: 							"processCapabilityTable"
					label: 							qsTr("Process capability tables")
					checked: 						true
					enabled:						upperSpecificationLimit.checked || lowerSpecificationLimit.checked

					CheckBox
					{
						name: "processCapabilityTableCi";
						label: qsTr("Confidence intervals")
						checked: capabilityStudyType.value === "normalCapabilityAnalysis"
						enabled: capabilityStudyType.value === "normalCapabilityAnalysis"
						childrenOnSameRow: true

						CIField
						{
							name: "processCapabilityTableCiLevel"
							defaultValue: 90}
						}

					}
				}
				*/
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

			Group
			{

				title: 							qsTr("Plots")

				CheckBox
				{
					name: 						"posteriorDistributionPlot"
					label: 						qsTr("Posterior distribution")

					CheckBox
					{
						label:				qsTr("Point estimate")
						name:				"posteriorDistributionPlotIndividualPointEstimate"
						childrenOnSameRow:	true

						DropDown
						{
							name:	"posteriorDistributionPlotIndividualPointEstimateType"
							label:	""
							values:	[
								{label: qsTr("Mean"),					value: "mean"},
								{label: qsTr("Median"),					value: "median"},
								{label: qsTr("Mode"),					value: "mode"}
							]
						}
					}

					CheckBox
					{
						name:				"posteriorDistributionPlotIndividualCi"
						label:				qsTr("CI")
						id:					plotsPosteriorIndividualCI
						childrenOnSameRow:	true

						DropDown
						{
							name:		"posteriorDistributionPlotIndividualCiType"
							label:		""
							id:			plotsPosteriorIndividualType
							values:		[
								{label: qsTr("Central"),				value: "central"},
								{label: qsTr("HPD"),					value: "HPD"},
								{label: qsTr("Custom"),					value: "custom"},
								{label: qsTr("Support"),				value: "support"}
							]
						}
					}

					Group
					{
						columns: 2

						CIField
						{
							visible:		plotsPosteriorIndividualType.currentValue === "central" || plotsPosteriorIndividualType.currentValue === "HPD"
							enabled:		plotsPosteriorIndividualCI.checked
							name:			"posteriorDistributionPlotIndividualCiMass"
							label:			qsTr("Mass")
							fieldWidth:		50
							defaultValue: 	95
							min:			1
							max:			100
							inclusive:		JASP.MinMax
						}

						DoubleField
						{
							visible:		plotsPosteriorIndividualType.currentValue === "custom"
							enabled:		plotsPosteriorIndividualCI.checked
							name:			"posteriorDistributionPlotIndividualCiLower"
							label:			qsTr("Lower")
							id:				plotsPosteriorLower
							fieldWidth:		50
							defaultValue:	0
							negativeValues: true
							inclusive:		JASP.MinMax
						}

						DoubleField
						{
							visible:		plotsPosteriorIndividualType.currentValue === "custom"
							enabled:		plotsPosteriorIndividualCI.checked
							name:			"posteriorDistributionPlotIndividualCiUpper"
							label:			qsTr("Upper")
							id:				plotsPosteriorUpper
							fieldWidth:		50
							defaultValue:	1
							negativeValues: true
							inclusive:		JASP.MinMax
						}

						FormulaField
						{
							visible:		plotsPosteriorIndividualType.currentValue === "support"
							enabled:		plotsPosteriorIndividualCI.checked
							name:			"posteriorDistributionPlotIndividualCiBf"
							label:			qsTr("BF")
							fieldWidth:		50
							defaultValue:	"1"
							min:			0
							inclusive:		JASP.None
						}
					}

					CheckBox
					{
						name:		"posteriorDistributionPlotPriorDistribution"
						label:		qsTr("Prior distribution")
						checked:	false
					}

					// CheckBox
					// {
					// 	name:		"posteriorDistributionPlotObservedProportion"
					// 	label:		qsTr("Observed proportion")
					// 	id:			plotsPosteriorIndividualProportion
					// 	checked:	false
					// }

				}

				CheckBox
				{
					name:		"sequentialAnalysisPlot"
					label:		qsTr("Sequential analysis")

					DropDown
					{
						label:		qsTr("Type")
						name:		"sequentialAnalysisPlotPointEstimateType"
						values:		["mean", "median"]//, "mode"]
					}

					CheckBox
					{
						name:				"sequentialAnalysisPlotCi"
						label:				qsTr("CI")
						childrenOnSameRow:	true

						CIField
						{
							name:				"sequentialAnalysisPlotCiMass"
							label:				qsTr("Mass")
							fieldWidth:			50
							defaultValue:		95
							min:				1
							max:				100
							inclusive:			JASP.MaxOnly
						}
					}

					CheckBox
					{
						name:		"sequentialAnalysisPlotAdditionalInfo"
						label:		qsTr("Show process criteria")
						checked:	true
						info:		qsTr("Add a secondary right axis with condition bounds for the process")
					}
				}
			}

			Group
			{

				title: 							qsTr("Tables")

				CheckBox
				{
					name: "intervalTable"; label: qsTr("Interval"); childrenOnSameRow: false

					info: qsTr("Show the posterior probabilities of the interval specifed with the input on the right. Note that the input is automatically sorted and that the first and last fields are always negative and positive infinity.")

					// ColumnLayout
					// {
					// 	// A poor man's table
					// 	Row
					// 	{
					// 		id: labelRow
					// 		spacing: jaspTheme.rowSpacing
					// 		property int fw: 60 // there are 6 elements of width 50 below, so this one is 6 * 50 / 5 = 60
					// 		Label { width: labelRow.fw; text: qsTr("Incapable") }
					// 		Label { width: labelRow.fw; text: qsTr("Capable") }
					// 		Label { width: labelRow.fw; text: qsTr("Satisfactory") }
					// 		Label { width: labelRow.fw; text: qsTr("Excellent") }
					// 		Label { width: labelRow.fw; text: qsTr("Super") }
					// 	}
					// 	Row
					// 	{
					// 		id: intervalRow
					// 		spacing: jaspTheme.rowSpacing
					// 		property int fw: 50
					// 		DoubleField { id: interval0; name: "interval0"; fieldWidth: intervalRow.fw; negativeValues: true; defaultValue: -Infinity;	editable: false							}
					// 		DoubleField { id: interval1; name: "interval1"; fieldWidth: intervalRow.fw; negativeValues: true; defaultValue: 1.00;		onEditingFinished: sortIntervalValues()	}
					// 		DoubleField { id: interval2; name: "interval2"; fieldWidth: intervalRow.fw; negativeValues: true; defaultValue: 1.33;		onEditingFinished: sortIntervalValues()	}
					// 		DoubleField { id: interval3; name: "interval3"; fieldWidth: intervalRow.fw; negativeValues: true; defaultValue: 1.50;		onEditingFinished: sortIntervalValues()	}
					// 		DoubleField { id: interval5; name: "interval5"; fieldWidth: intervalRow.fw; negativeValues: true; defaultValue: 2.00;		onEditingFinished: sortIntervalValues()	}
					// 		DoubleField { id: interval6; name: "interval6"; fieldWidth: intervalRow.fw; negativeValues: true; defaultValue:  Infinity;	editable: false							}
					// 	}
					// }



				}

			}

		}

		ColumnLayout
		{

			Group
			{
				title:			qsTr("Stability of the process")
			}
		}
	}

	Section
	{
		title: qsTr("Process capability report")

	}

	Section
	{
		title: qsTr("Advanced options")

	}
}
