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

import QtQuick                  			2.8
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
			{label: qsTr("Long format"),					value: "PClongFormat"},
			{label: qsTr("Wide format"),				value: "PCwideFormat"},
		]
		id: pcDataFormat
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
			id:                 			subgroups
			name:               			"subgroups"
			title:             			 	qsTr("Subgroups")
			singleVariable:    	 			true
			allowedColumns:     			["nominal", "nominalText", "ordinal"]
		}
	}
	
	DoubleField
	{
		id:						pcSubgroupSize
		name: 					"pcSubgroupSize"
		label: 					qsTr("Subgroup size:")
		negativeValues:			false
		min: 					5
		defaultValue:			5
		visible:				pcDataFormat.currentValue == "PClongFormat"
	}
	
	
	Section
	{
		title: qsTr("Process Capability Options")

		ColumnLayout
		{
			Group
			{
				title:							qsTr("Study Type")


				RadioButtonGroup
				{
					name: 					"capabilityStudyType"

					RadioButton
					{
						name: 				"normalCapabilityAnalysis"
						label: 				qsTr("Normal capability study")
						checked: 			true
					}

					RadioButton
					{
						name: 				"nonnormalCapabilityAnalysis"
						label: 				qsTr("Non-normal capability study")
						
						DropDown
						{
							name: 					"nonNormalDist"
							label: 					qsTr("Specify a distribution")
							indexDefaultValue: 		0
							values:
								[
								{label: qsTr("Weibull"),		value: "Weibull"  },
								{label: qsTr("Lognormal"),		value: "Lognormal"}
							]
						}
					}
				}

				CheckBox
				{
					name: 						"CapabilityStudyPlot"
					label: 						qsTr("Process capability plot")
					checked: 					true

					DropDown
					{
						name: "csBinWidthType"
						label: qsTr("Bin width type")
						indexDefaultValue: 0
						values:
							[
							{label: qsTr("Sturges"),				value: "sturges"},
							{label: qsTr("Scott"),					value: "scott"},
							{label: qsTr("Doane"),					value: "doane"},
							{label: qsTr("Freedman-Diaconis"),		value: "fd"	},
							{label: qsTr("Manual"),					value: "manual"	}
						]
						id: csBinWidthType
					}

					DoubleField
					{
						name:			"csNumberOfBins"
						label:			qsTr("Number of bins")
						defaultValue:	30
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

					CheckBox
					{
						name: "csConfidenceInterval";	label: qsTr("Confidence intervals")
						checked: true
						childrenOnSameRow: true
						CIField { name: "csConfidenceIntervalPercent"}
					}

				}

			}

			Group
			{
				title: 							qsTr("Specification Limits")

				CheckBox
				{
					name: 						"lowerSpecificationField"
					label: 						qsTr("Lower specification limit")
					childrenOnSameRow:			true

					DoubleField
					{
						id:						lower
						name: 					"lowerSpecification"
						negativeValues:			true
						defaultValue:			-1
						max:					target.value
					}
				}

				CheckBox
				{
					name: 						"targetValueField"
					label: 						qsTr("Target value")
					childrenOnSameRow:			true

					DoubleField
					{
						id:						target
						name: 					"targetValue"
						negativeValues:			true
						defaultValue:			0
						max:					upper.value
						min:					lower.value
					}
				}

				CheckBox
				{
					name: 						"upperSpecificationField"
					childrenOnSameRow:			true
					label: 						qsTr("Upper specification limit")

					DoubleField
					{
						id:						upper
						name: 					"upperSpecification"
						negativeValues:			true
						defaultValue:			1
						min:					target.value
					}
				}
			}
		}

		ColumnLayout
		{
			Group
			{
				title: 							qsTr("Stability of the Process")
				

				RadioButtonGroup
				{
					name: 					"controlChartsType"

					RadioButton
					{
						name: 				"xbarR"
						label: 				qsTr("X-bar & R chart")
						checked: 			true
					}

					RadioButton
					{
						name: 				"IMR"
						label: 				qsTr("I-MR chart")
					}
				}
			}

			Group
			{
				title: 							qsTr("Distribution of the Process")

				CheckBox
				{
					name: 						"histogram"
					label: 						qsTr("Histogram")
					checked: 					true

					CheckBox
					{
						name:					"displayDensity"
						label:					qsTr("Fit normal distribution")
						checked:				true
					}

					DropDown
					{
						name: "pcBinWidthType"
						label: qsTr("Bin width type")
						indexDefaultValue: 0
						values:
							[
							{label: qsTr("Sturges"),				value: "sturges"},
							{label: qsTr("Scott"),					value: "scott"},
							{label: qsTr("Doane"),					value: "doane"},
							{label: qsTr("Freedman-Diaconis"),		value: "fd"	},
							{label: qsTr("Manual"),					value: "manual"	}
						]
						id: binWidthType
					}

					DoubleField
					{
						name:			"pcNumberOfBins"
						label:			qsTr("Number of bins")
						defaultValue:	30
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

					DropDown
					{
						name: 					"rank"
						label: 					qsTr("Rank method")
						indexDefaultValue: 		0
						values:
							[
							{ value: "Bernard",    		label: qsTr("Median Rank (Benard)")         },
							{ value: "Herd-Johnson",    label: qsTr("Mean Rank (Herd-Johnson)")     },
							{ value: "Kaplan-Meier",    label: qsTr("Kaplan-Meier")                 },
							{ value: "Hazen",   		label: qsTr("Modified Kaplan-Meier (Hazen)")}
						]
					}

					DropDown
					{
						name: 					"nullDistribution"
						label: 					qsTr("Null distribution")
						indexDefaultValue: 		0
						values:
							[
							{ label: qsTr("Normal"),		value: "Normal"	   },
							{ label: qsTr("Lognormal"),		value: "Lognormal" },
							{ label: qsTr("Weibull"),		value: "Weibull"   }
						]
					}

					CheckBox
					{
						name:					"addGridlines"
						label:					qsTr("Display grid lines in plots")
					}
				}
			}
		}
	}
	
	
	
	Section
	{
		title: qsTr("Process Capability Report")
		
		
		TextField
		{
			id:						anovaGaugeTitle
			label: 					qsTr("Title:")
			name: 					"anovaGaugeTitle"
			placeholderText:		qsTr("Measurement")
			fieldWidth:				100
		}
		
		TextField
		{
			id:						anovaGaugeName
			label: 					qsTr("Gauge Name:")
			name: 					"anovaGaugeName"
			placeholderText:		qsTr("Name")
			fieldWidth:				100
		}
		
		TextField
		{
			id:						anovaGaugeDate
			label: 					qsTr("Date:")
			name: 					"anovaGaugeDate"
			placeholderText:		qsTr("Date")
			fieldWidth:				100
		}
		
		TextField
		{
			id:						anovaGaugeReportedBy
			label: 					qsTr("Reported by:")
			name: 					"anovaGaugeReportedBy"
			placeholderText:		qsTr("Name")
			fieldWidth:				100
		}
		
		TextField
		{
			id:						anovaGaugeMisc
			label: 					qsTr("Misc:")
			name: 					"anovaGaugeMisc"
			placeholderText:		qsTr("Miscellaneous")
			fieldWidth:				100
		}
		
		CheckBox
		{
			name: "anovaGaugeReport";		label: qsTr("Show Report")
		}
		

	}
}
