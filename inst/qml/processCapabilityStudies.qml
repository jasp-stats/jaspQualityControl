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
			allowedColumns:					["nominal"]
			enabled: 						subgroupSizeType.value == "groupingVariable"
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

				DropDown
				{
					name: 					"groupingVariableMethod"
					id: 					groupingVariable
					label: 					"Grouping method"
					values: 
					[
						{ label: qsTr("Subgroup value change"),			value: "newLabel"},
						{ label: qsTr("Same subgroup value"),			value: "sameLabel"}
					]
					indexDefaultValue: 0
				}
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
						checked: capabilityStudyType.value == "normalCapabilityAnalysis"
						enabled: capabilityStudyType.value == "normalCapabilityAnalysis"
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

			Group
			{
				title:			qsTr("Stability of the process")

				CheckBox
				{
					name: 						"controlChart"
					label: 						""
					checked: 					true
					childrenOnSameRow:			true
					columns:					1
					

					DropDown
					{
						name: 					"controlChartType"
						id: 					controlChartType
						label: 					""
						values: dataFormat.currentValue == "longFormat" ?
						[
							{ label: qsTr("X-bar & R control chart"),			value: "xBarR"},
							{ label: qsTr("X-bar & s control chart"),			value: "xBarS"},
							{ label: qsTr("X-mR control chart"),				value: "xmr"}
						] :
						[
							{ label: qsTr("X-bar & R control chart"),			value: "xBarR"},
							{ label: qsTr("X-bar & s control chart"),			value: "xBarS"},
							{ label: qsTr("X-bar & mR control chart"),			value: "xBarMR"}
						]
						indexDefaultValue: 
						(dataFormat.currentValue == "wideFormat" || (dataFormat.currentValue == "longFormat" && manualSubgroupSizeValue.value > 1)) ? 1 :
						(dataFormat.currentValue == "longFormat" && manualSubgroupSizeValue.value == 1) ? 2 : 1
					}
				

					DoubleField
					{
						name:							"xBarMovingRangeLength"
						label:							qsTr("Moving range length")
						visible:						controlChartType.currentIndex == 2 && dataFormat.currentValue == "wideFormat"
						defaultValue:					2
						min: 							2
						max: 							dataSetInfo.rowCount
					}

					Group 
					{
						visible:						controlChartType.currentIndex == 2 && dataFormat.currentValue == "longFormat"

						DoubleField
						{
							name:							"xmrChartMovingRangeLength"
							label:							qsTr("Moving range length")
							defaultValue:					2
							min: 							2
							max: 							dataSetInfo.rowCount
						}

						CheckBox
						{
							name: 							"xmrChartSpecificationLimits"
							label: 							qsTr("Display specification limits")
							checked: 						false
						}
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
		title: qsTr("Process capability report")
		
		CheckBox
		{
			name: "report"
			label: qsTr("Show Report")
			columns: 1
			
			
			CheckBox
			{
				name:		"reportMetaData"
				label:		qsTr("Show report metadata")
				checked:	true
				columns: 2

				CheckBox
				{
					name:					"reportTitle"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 					"reportTitleText"
						label:					qsTr("Title")
						id:						reportTitleText
						placeholderText:		qsTr("Process Capability Report")
						fieldWidth:				100
					}
				}

				CheckBox
				{
					name:					"reportLocation"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name:					"reportLocationText"
						label:					qsTr("Location")
						id:						reportLocationText
						placeholderText:		qsTr("Location")
						fieldWidth:				100
					}
				}

				CheckBox
				{
					name:					"reportLine"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name:					"reportLineText"
						label:					qsTr("Line")
						id:						reportLineText
						placeholderText:		qsTr("Line")
						fieldWidth:				100
					}
				}

				CheckBox
				{
					name:					"reportMachine"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name:					"reportMachineText"
						label:					qsTr("Machine")
						id:						reportMachineText
						placeholderText:		qsTr("Machine")
						fieldWidth:				100
					}
				}

				CheckBox
				{
					name:					"reportVariable"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name:					"reportVariableText"
						label:					qsTr("Variable")
						id:						reportVariableText
						placeholderText:		qsTr("Variable")
						fieldWidth:				100
					}
				}

				CheckBox
				{
					name:					"reportProcess"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name:					"reportProcessText"
						label:					qsTr("Process")
						id:						reportProcessText
						placeholderText:		qsTr("Name")
						fieldWidth:				100
					}
				}

				CheckBox
				{
					name:					"reportDate"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name:					"reportDateText"
						label:					qsTr("Date")
						id:						reportDateText
						placeholderText:		qsTr("Date")
						fieldWidth:				100
					}
				}

				CheckBox
				{
					name:					"reportReportedBy"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name:					"reportReportedByText"
						label:					qsTr("Reported by")
						id:						reportReportedByText
						placeholderText:		qsTr("Name")
						fieldWidth:				100
					}
				}

				CheckBox
				{
					name:					"reportConclusion"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name:					"reportConclusionText"
						label:					qsTr("Conclusion")
						id:						reportConclusionText
						placeholderText:		qsTr("Conclusion")
						fieldWidth:				100
					}
				}
			}
		
			Group
			{
				title:			qsTr("Select report components")
			
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
		title: qsTr("Advanced options")
		
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

			Group
			{
				title:			""
				columns:		2

				DropDown
				{
					name: 					"controlChartSdEstimationMethodGroupSize"
					id: 					controlChartSdEstimationMethodGroupSize
					label: 					qsTr("Method for estimating within subgroup std. dev. for subgroup size")
					values: 
					[
						{ label: qsTr(" > 1"),		value: "largerThanOne"},
						{ label: qsTr("= 1"),		value: "equalOne"}
					]
						indexDefaultValue:
						(dataFormat.currentValue == "longFormat" && manualSubgroupSizeValue.value == 1) ? 1 : 0
				}

				DropDown
				{
					name: 					"controlChartSdEstimationMethodGroupSizeLargerThanOne"
					id: 					controlChartSdEstimationMethodGroupSizeLargerThanOne
					visible:				controlChartSdEstimationMethodGroupSize.currentValue == "largerThanOne"
					label: 					""
					values: 
					[
						{ label: qsTr("R-bar"),			value: "rBar"},
						{ label: qsTr("S-bar"),			value: "sBar"}
					]
					indexDefaultValue:
					(controlChartType.currentValue == "xBarR" || controlChartType.currentValue == "xBarMR") ? 0 : 
					(controlChartType.currentValue == "xBarS") ? 1 : 0
				}

				DropDown
				{
					name: 					"controlChartSdEstimationMethodGroupSizeEqualOne"
					id: 					controlChartSdEstimationMethodGroupSizeEqualOne
					visible:				controlChartSdEstimationMethodGroupSize.currentValue == "equalOne"
					label: 					""
					values: 
					[
						{ label: qsTr("Mean moving range"),			value: "meanMovingRange"}
					]
				}

				DoubleField
				{
					name:							"controlChartSdEstimationMethodMeanMovingRangeLength"
					visible:						controlChartSdEstimationMethodGroupSize.currentIndex == 1
					label:							qsTr("Moving range length")
					defaultValue:					2
					min: 							2
					max: 							dataSetModel.rowCount()
				}

				CheckBox
				{
					name:								"controlChartSdUnbiasingConstant"
					label: 								qsTr("Use unbiasing constant")
					visible:							controlChartSdEstimationMethodGroupSize.currentIndex == 1 ? false : true
					checked:							true
				}
			}

			DoubleField
			{
				name: 								"controlLimitsNumberOfSigmas"
				label: 								qsTr("Number of std. dev. for calculation of control limits")
				fieldWidth: 						30
				defaultValue: 						3
				min:								.000001
			}
		}
	}
}
