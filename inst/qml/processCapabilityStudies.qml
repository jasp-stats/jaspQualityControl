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
		id:									variablesForm

		AvailableVariablesList
		{
			name:							"variablesForm"
		}

		AssignedVariablesList
		{
			name:							"measurementLongFormat"
			title:							qsTr("Measurement")
			id:								measurementLongFormat
			allowedColumns:					["scale"]
			singleVariable:					true
			visible:						dataFormat.currentValue == "longFormat"
		}

		AssignedVariablesList
		{
			name:							"measurementsWideFormat"
			title:							qsTr("Measurements")
			id:								measurementsWideFormat
			allowedColumns:					["scale"]
			visible:						dataFormat.currentValue == "wideFormat"
		}

		AssignedVariablesList
		{
			name:							"subgroup"
			title:						 	qsTr("Subgroup")
			id:					 			subgroup
			singleVariable:		 			true
			allowedColumns:					["nominal", "nominalText", "ordinal"]
		}
	}

	CheckBox
	{
		name: 						"manualSubgroupSize"
		label: 						qsTr("Specifiy subgroup size manually:")
		id: 						manualSubgroupSize
		checked: 					true
		childrenOnSameRow:			true
		visible:					dataFormat.currentValue == "longFormat"

		DoubleField
		{
			name: 					"manualSubgroupSizeValue"
			id:						manualSubgroupSizeValue
			negativeValues:			false
			min: 					1
			max: 					dataSetModel.rowCount()
			defaultValue:			5
			visible:				dataFormat.currentValue == "longFormat"
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
			Group
			{
				title:							qsTr("Stability of the process")

				CheckBox
				{
					name:						"xBarAndRChart"
					label: 						qsTr("X-bar & R chart")
					enabled:					manualSubgroupSizeValue.value > 1
					checked: 					manualSubgroupSizeValue.value > 1
				}

				CheckBox
				{
					name: 						"xmrChart"
					label: 						qsTr("X-mR chart")
					enabled: 					manualSubgroupSizeValue.value == 1 || dataFormat.currentValue == "wideFormat"
					checked: 					manualSubgroupSizeValue.value == 1

					DoubleField
					{
						name:							"movingRangeLength"
						label:							qsTr("Moving range length")
						defaultValue:					2
						min: 							2
						max: 							dataSetModel.rowCount()
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
