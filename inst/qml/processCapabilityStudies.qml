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

import QtQuick                  			  2.8
import QtQuick.Layouts              		1.3
import JASP.Controls              			1.0
import JASP.Widgets               			1.0

Form
{
	columns:								2


	DropDown
	{
		name: "pcDataFormat"
		label: qsTr("Data format")
		indexDefaultValue: 0
		values:
			[
			{label: qsTr("Single column"),			value: "PClongFormat"},
			{label: qsTr("Across rows"),				value: "PCwideFormat"},
		]
		id: pcDataFormat
		onValueChanged:
		{
			variablesLong.itemDoubleClicked(0)
			variables.itemDoubleClicked(0)
		}
	}

	VariablesForm
	{
		id:                   				variablesForm

		AvailableVariablesList
		{
			name:               			"variablesForm"
		}

		AssignedVariablesList
		{
			id:                 			variablesLong
			name:               			"variablesLong"
			title:              			qsTr("Measurements")
			allowedColumns:     			["scale"]
			singleVariable:					true
			visible:						pcDataFormat.currentValue == "PClongFormat"
		}

		AssignedVariablesList
		{
			id:                 			variables
			name:               			"variables"
			title:              			qsTr("Measurements")
			allowedColumns:     			["scale"]
			visible:						pcDataFormat.currentValue == "PCwideFormat"
		}

		AssignedVariablesList
		{
			name:               			"subgroup"
			title:             			 	qsTr("Subgroup")
			id:                 			subgroup
			singleVariable:    	 			true
			allowedColumns:     			["nominal", "nominalText", "ordinal"]
		}
	}
	CheckBox
	{
		name: 						"manualSubgroupSize"
		id: 						manualSubgroupSize
		label: 						qsTr("Specifiy subgroup size manually:")
		checked: 					true
		childrenOnSameRow:			true
		visible:						pcDataFormat.currentValue == "PClongFormat"

		DoubleField
		{
			name: 					"pcSubgroupSize"
			id:						pcSubgroupSize
			negativeValues:			false
			min: 					1
			max: 					dataSetModel.rowCount()
			defaultValue:			5
			visible:				pcDataFormat.currentValue == "PClongFormat"
		}
	}
	Section
	{
		title: qsTr("Process Capability Options")

		ColumnLayout
		{
			Group
			{
				title:							qsTr("Type of data distribution")


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
						name: 				"nonnormalCapabilityAnalysis"
						id : 				nonnormalCapabilityAnalysis
						label: 				qsTr("Non-normal distribution")
						DropDown
						{
							name: 					"nonNormalDist"
							id: 					nonNormalDist
							label: 					qsTr("Specify a distribution")
							values:
								[
								{label: qsTr("Weibull"),		value: "Weibull"  },
								{label: qsTr("Lognormal"),		value: "Lognormal"},
								{label: qsTr("3-parameter lognormal"),		value: "3lognormal"},
								{label: qsTr("3-parameter Weibull"),		value: "3weibull"}
							]
							indexDefaultValue: (nullDistribution.currentValue == "Weibull") ? 0 : 1
						}

						DropDown
						{
							name: 					"nonNormalMethod"
							label: 					qsTr("Non-normal capability statistics:")
							indexDefaultValue: 		0
							values:
								[
								{label: qsTr("Percentile"),		value: "percentile"},
								{label: qsTr("Non-conformance"),		value: "nonconformance"  }
							]
						}
					}
				}


			Group
			{
				title: 							qsTr("Capability studies")

				CheckBox
				{
				  id: checkedlower
					name: 						"lowerSpecificationField"
					label: 						qsTr("Lower specification limit")
					childrenOnSameRow:			true

					DoubleField
					{
						id:						lower
						name: 					"lowerSpecification"
						negativeValues:			true
						defaultValue:			-1
						decimals:				9
					}
				}

				CheckBox
				{
				  id: checkedtarget
					name: 						"targetValueField"
					label: 						qsTr("Target value")
					childrenOnSameRow:			true

					DoubleField
					{
						id:						target
						name: 					"targetValue"
						negativeValues:			true
						defaultValue:			0
						decimals:				9
					}
				}

				CheckBox
				{
				  id: checkedupper
					name: 						"upperSpecificationField"
					childrenOnSameRow:			true
					label: 						qsTr("Upper specification limit")

					DoubleField
					{
						id:						upper
						name: 					"upperSpecification"
						negativeValues:			true
						defaultValue:			1
						decimals:				9
					}
				}

				CheckBox
				{
					name: 						"CapabilityStudyPlot"
					label: 						qsTr("Process capability plot")
					checked: 					true
					enabled:          checkedupper.checked || checkedtarget.checked || checkedlower.checked

					DoubleField
					{
						name:			"csNumberOfBins"
						label:			qsTr("Number of bins")
						defaultValue:	10
						min:			3;
						max:			10000;
						enabled:		csBinWidthType.currentValue === "manual"
					}
				}

				CheckBox
				{
					name: 						"CapabilityStudyTables"
					label: 						qsTr("Process capability tables")
					checked: 					true
					enabled:          checkedupper.checked | checkedtarget.checked | checkedlower.checked

					CheckBox
					{
						name: "csConfidenceInterval";	label: qsTr("Confidence intervals")
						checked: true
						childrenOnSameRow: true
						CIField {
							name: "csConfidenceIntervalPercent"
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
				title: 							qsTr("Stability of the process")

				CheckBox
				{
				  	name:                  		"xbarR"
					label: 				   		qsTr("X-bar & R chart")
					enabled:                    pcSubgroupSize.value > 1
					checked: 					pcSubgroupSize.value > 1
				}

			    CheckBox
				{
					name: 						"IMR"
					label: 						qsTr("X-mR chart")
					enabled: 					pcSubgroupSize.value == 1 || pcDataFormat.currentValue == "PCwideFormat"
					checked: 					pcSubgroupSize.value == 1

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
						name:					"displayDensity"
						label:					qsTr("Fit distribution")
						checked:				true
					}

					DoubleField
					{
						name:			"pcNumberOfBins"
						label:			qsTr("Number of bins")
						defaultValue:	10
						min:			3;
						max:			10000;
						enabled:		binWidthType.currentValue === "manual"
					}
				}

				CheckBox
				{
					name: 						"probabilityPlot"
					label: 						qsTr("Probability table and plot")
					checked: 					true

					CheckBox
					{
						name:					"addGridlines"
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
			name: "pcReportDisplay"
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
			id:						pcReportTitle
			label: 					qsTr("Title:")
			name: 					"pcReportTitle"
			placeholderText:		qsTr("Measurement")
			fieldWidth:				100
		}

		TextField
		{
			id:						pcReportName
			label: 					qsTr("Process Name:")
			name: 					"pcReportName"
			placeholderText:		qsTr("Name")
			fieldWidth:				100
		}

		TextField
		{
			id:						pcReportDate
			label: 					qsTr("Date:")
			name: 					"pcReportDate"
			placeholderText:		qsTr("Date")
			fieldWidth:				100
		}

		TextField
		{
			id:						pcReportReportedBy
			label: 					qsTr("Reported by:")
			name: 					"pcReportReportedBy"
			placeholderText:		qsTr("Name")
			fieldWidth:				100
		}

		TextField
		{
			id:						pcReportMisc
			label: 					qsTr("Misc:")
			name: 					"pcReportMisc"
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

	Section{
  			title: qsTr("Advanced Options")
			  Group{
				  	DropDown
					{
						name: 					"rank"
						label: 					qsTr("Rank method for probability plot")
						indexDefaultValue: 		0
						values:
							[
							{ value: "Bernard",    		label: qsTr("Median Rank (Benard)")         },
							{ value: "Herd-Johnson",    label: qsTr("Mean Rank (Herd-Johnson)")     },
							{ value: "Kaplan-Meier",    label: qsTr("Kaplan-Meier")                 },
							{ value: "Hazen",   		label: qsTr("Modified Kaplan-Meier (Hazen)")}
						]
					}
					
					CheckBox
					{
						name:                   			"manualTicksXAxis"
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
						name:					"manualTicksProbabilityPlot"
						label: 								qsTr("Number of ticks on x-axis for probability plot:")
						childrenOnSameRow: true

						DoubleField
						{
							name: 							"nTicksProbabilityPlot"
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
							{ label: qsTr("Normal"),		value: "Normal"	   },
							{ label: qsTr("Lognormal"),		value: "Lognormal" },
							{ label: qsTr("Weibull"),		value: "Weibull"   }
						]
						indexDefaultValue: (capabilityStudyType.value == "nonnormalCapabilityAnalysis") ? 
							(nonNormalDist.currentValue == "Lognormal" || nonNormalDist.currentValue == "3lognormal") ? 1 : 
								(nonNormalDist.currentValue == "3weibull" || nonNormalDist.currentValue == "Weibull") ? 2 : 0 
						: 0
					}
			  }
	}
}
