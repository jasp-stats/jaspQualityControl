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

import QtQuick								2.8
import QtQuick.Layouts						1.3
import JASP.Controls						1.0
import JASP.Widgets							1.0

Form
{
	columns:								2


	DropDown
	{
		name: "dataFormat"
		label: qsTr("Data format")
		id: dataFormat
		indexDefaultValue: 0
		values:
		[
			{label: qsTr("Single column"),				value: "longFormat"},
			{label: qsTr("Across rows"),				value: "wideFormat"},
		]
		onValueChanged:
		{
			measurementLongFormat.itemDoubleClicked(0)
			measurementsWideFormat.itemDoubleClicked(0)
		}
	}

	VariablesForm
	{
		id:									variablesFormLongFormat
		visible:							dataFormat.currentValue == "longFormat"

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
			allowedColumns:					["nominal", "nominalText", "ordinal"]
			enabled: 						subgroupSizeType.value == "groupingVariable"
		}

		AssignedVariablesList
		{
			id:									stagesLongFormat
			name:								"stagesLongFormat"
			title:								qsTr("Stages")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal"]
		}
	}

	VariablesForm
	{
		id:									variablesFormWideFormat
		visible:							dataFormat.currentValue == "wideFormat"

		AvailableVariablesList
		{
			name:							"variablesFormWideFormat"
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
			title:								qsTr("Axis labels")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal"]
		}

		AssignedVariablesList
		{
			id:									stagesWideFormat
			name:								"stagesWideFormat"
			title:								qsTr("Stages")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal"]
		}
	}

	Group
	{
		columns:							2

		RadioButtonGroup
		{
			name:								"subgroupSizeType"
			title: 								qsTr("Specify subgroups")
			id:									subgroupSizeType
			visible:							dataFormat.currentValue == "longFormat"							

			RadioButton
				{
					value: 							"manual"
					label: 							qsTr("Subgroup size")
					checked:		 				true
					childrenOnSameRow:				true
					
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
				}

				

		}

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
	}

	Section
	{
		title: qsTr("Process Capability Options")

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
								{label: qsTr("3-parameter lognormal"),		value: "3ParameterLognormal"},
								{label: qsTr("3-parameter Weibull"),		value: "3ParameterWeibull"}
							]
							indexDefaultValue: (nullDistribution.currentValue == "weibull") ? 0 : 1
						}

						DropDown
						{
							name:					"nonNormalMethod"
							label:					qsTr("Non-normal capability statistics:")
							indexDefaultValue:		0
							values:
								[
								{label: qsTr("Percentile"),				value: "percentile"},
								{label: qsTr("Non-conformance"),		value: "nonConformance"  }
							]
						}
					}
				}


			Group
			{
				title: 							qsTr("Capability studies")

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

					CheckBox
					{
					name: 						"lowerSpecificationLimitBoundary"
					label: 						qsTr("Boundary")
					id:							lowerSpecificationLimitBoundary
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

					CheckBox
					{
					name: 						"upperSpecificationLimitBoundary"
					label: 						qsTr("Boundary")
					id:							upperSpecificationLimitBoundary
					}
				}

				CheckBox
				{
					name: 						"processCapabilityPlot"
					label: 						qsTr("Process capability plot")
					checked: 					true
					enabled:					upperSpecificationLimit.checked || target.checked || lowerSpecificationLimit.checked

					DoubleField
					{
						name:						"processCapabilityPlotBinNumber"
						label:						qsTr("Number of bins")
						defaultValue:				10
						min:						3;
						max:						10000;
						enabled:					csBinWidthType.currentValue === "manual"
					}
				}

				CheckBox
				{
					name: 							"processCapabilityTable"
					label: 							qsTr("Process capability tables")
					checked: 						true
					enabled:						upperSpecificationLimit.checked | target.checked | lowerSpecificationLimit.checked

					CheckBox
					{
						name: "processCapabilityTableCi";	
						label: qsTr("Confidence intervals")
						checked: true
						childrenOnSameRow: true

						CIField 
						{
							name: "processCapabilityTableCiLevel"
							defaultValue: 90}
						}

					}
				}
			}
		}

		ColumnLayout
		{

			RadioButtonGroup
			{
				title: 					qsTr("Stability of the process")
				name:					"controlChartType"

				RadioButton
				{
					name: 				"xBarR"
					id : 				xbarR
					label: 				qsTr("X-bar & R chart")
					enabled:			(dataFormat.currentValue == "longFormat" & manualSubgroupSizeValue.value > 1) | dataFormat.currentValue == "wideFormat"
					checked:			dataFormat.currentValue == "wideFormat" | (dataFormat.currentValue == "longFormat" & manualSubgroupSizeValue.value > 1)
				}

				RadioButton
				{
					name: 				"xBarS"
					id : 				xbarS
					label: 				qsTr("X-bar & s chart")
					enabled:			(dataFormat.currentValue == "longFormat" & manualSubgroupSizeValue.value > 1) | dataFormat.currentValue == "wideFormat"
				} 

				RadioButton
				{
					name: 				"xBarMR"
					id : 				xbarMR
					label: 				qsTr("X-bar & mR chart")
					visible:			dataFormat.currentValue == "wideFormat"
					checked:			false

					DoubleField
					{
					name:							"xBarMovingRangeLength"
					label:							qsTr("Moving range length")
					defaultValue:					2
					min: 							2
					max: 							dataSetInfo.rowCount
					}
				}

				RadioButton
				{
					name: 				"xmr"
					id : 				xmr
					label: 				qsTr("X-mR chart")
					enabled:			dataFormat.currentValue == "longFormat"
					visible:			dataFormat.currentValue == "longFormat"
					checked:			(dataFormat.currentValue == "longFormat" & manualSubgroupSizeValue.value == 1)

					DoubleField
					{
					name:							"xmrChartMovingRangeLength"
					label:							qsTr("Moving range length")
					defaultValue:					2
					min: 							2
					max: 							dataSetInfo.rowCount
					}
				}
			}

			Group
			{
				title: 							qsTr("Distribution of the process")

				CheckBox
				{
					name: 						"histogram"
					label: 						qsTr("Histogram")
					checked: 					true

					CheckBox
					{
						name:					"histogramDensityLine"
						label:					qsTr("Fit distribution")
						checked:				true
					}

					DoubleField
					{
						name:					"histogramBinNumber"
						label:					qsTr("Number of bins")
						defaultValue:			10
						min:					3;
						max:					10000;
					}
				}

				CheckBox
				{
					name:						"probabilityPlot"
					label:						qsTr("Probability table and plot")
					checked: 					true

					CheckBox
					{
						name:					"probabilityPlotGridLines"
						label:					qsTr("Display grid lines")
					}
				}
			}
		}
	}



	Section
	{
		title: qsTr("Process Capability Report")
		
		CheckBox
		{
			name: "report"
			label: qsTr("Show Report")
			columns: 2
			
			
						CheckBox
			{
				name:		"reportMetaData"
				label:		qsTr("Show report metadata")
				checked:	true
				columns: 1

		TextField
		{
			name: 					"reportTitle"
			label:					qsTr("Title")
			id:						reportTitle
			placeholderText:		qsTr("Measurement")
			fieldWidth:				100
		}

		TextField
		{
			name:					"reportProcessName"
			label:					qsTr("Process Name")
			id:						reportProcessName
			placeholderText:		qsTr("Name")
			fieldWidth:				100
		}

		TextField
		{
			name:					"reportDate"
			label:					qsTr("Date")
			id:						reportDate
			placeholderText:		qsTr("Date")
			fieldWidth:				100
		}

		TextField
		{
			name:					"reportReportedBy"
			label:					qsTr("Reported by")
			id:						reportReportedBy
			placeholderText:		qsTr("Name")
			fieldWidth:				100
		}

		TextField
		{
			name:					"reportMiscellaneous"
			label:					qsTr("Misc")
			id:						reportMiscellaneous
			placeholderText:		qsTr("Miscellaneous")
			fieldWidth:				100
		}
		
		
		}
		
		
		
					Group
			{
				title:			qsTr("Select Report Components")
			
				CheckBox
				{
					name:		"reportProcessStability"
					label:		qsTr("Show stability of process charts")
					checked:	true
				}
				
								CheckBox
				{
					name:		"reportProcessCapabilityPlot"
					label:		qsTr("Show process capability plot")
					checked:	true
				}
				
												CheckBox
				{
					name:		"reportProbabilityPlot"
					label:		qsTr("Show probability plot")
					checked:	true
				}
				
																CheckBox
				{
					name:		"reportProcessCapabilityTables"
					label:		qsTr("Show process capability tables")
					checked:	true
				}

				
				
				
				}

		}
	}

	Section
	{
		title: qsTr("Advanced Options")
		
		Group
		{
			DropDown
			{
				name:					"probabilityPlotRankMethod"
				label:					qsTr("Rank method for probability plot")
				indexDefaultValue:		0
				values:
				[
					{ value: "bernard",			label: qsTr("Median Rank (Benard)")			},
					{ value: "herdJohnson",		label: qsTr("Mean Rank (Herd-Johnson)")		},
					{ value: "kaplanMeier",		label: qsTr("Kaplan-Meier")					},
					{ value: "hazen",			label: qsTr("Modified Kaplan-Meier (Hazen)")}
				]
			}
					
			CheckBox
			{
				name:								"manualTicksXAxis"
				label: 								qsTr("Number of ticks on x-axis for X-bar & R or X-mR chart:")
				childrenOnSameRow: true

				DoubleField
				{
					name: 							"manualTicksXAxisValue"
					defaultValue:					5
				}
			}

			CheckBox
			{
				name:								"manualTicksProbabilityPlot"
				label: 								qsTr("Number of ticks on x-axis for probability plot:")
				childrenOnSameRow: true

				DoubleField
				{
					name: 							"manualTicksProbabilityPlotValue"
					defaultValue:					5
				}
			}

			DropDown
			{
				name: 					"nullDistribution"
				id: 					nullDistribution
				label: 					qsTr("Null distribution for probability plot")
				values: 
				[
					{ label: qsTr("Normal"),		value: "normal"		},
					{ label: qsTr("Lognormal"),		value: "lognormal"	},
					{ label: qsTr("Weibull"),		value: "weibull"	}
				]
				indexDefaultValue: (capabilityStudyType.value == "nonNormalCapabilityAnalysis") ? 
				(nonNormalDistribution.currentValue == "lognormal" || nonNormalDistribution.currentValue == "3ParameterLognormal") ? 1 : 
				(nonNormalDistribution.currentValue == "3ParameterWeibull" || nonNormalDistribution.currentValue == "weibull") ? 2 : 0 
				: 0
			}
		}
	}
}
