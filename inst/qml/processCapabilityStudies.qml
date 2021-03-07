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

	VariablesForm
	{
		id:                   				variablesForm

		AvailableVariablesList
		{
			name:               			"variablesForm"
		}

		AssignedVariablesList
		{
			id:                 			variables
			name:               			"variables"
			title:              			qsTr("Measurements")
			allowedColumns:     			["scale"]
		}

		AssignedVariablesList
		{
			id:                 			subgroups
			name:               			"subgroups"
			title:             			 	qsTr("Subgroups")
			singleVariable:    	 			true
			allowedColumns:     			["nominal", "nominalText", "ordinal"]
			debug:							true // Not sure how this is supposed to be used yet
		}
	}

	ColumnLayout
	{
		Group
		{
			title:							qsTr("Study Type")

			CheckBox 
			{ 									
				name: 						"normalCapabilityStudy"
				label: 						qsTr("Normal capability study")
				checked: 					true
												
				RadioButtonGroup
				{
					name: 					"capabilityStudy"

					RadioButton 
					{ 
						name: 				"initialCapabilityAnalysis"
						label: 				qsTr("Initial capability study")
						checked: 			true	
					}

					RadioButton 
					{ 
						name: 				"followupCapabilityAnalysis"
						label: 				qsTr("Follow-up capability study")
					}
				}
			}

			CheckBox 
			{ 
				name: 						"nonNormalCapabilityStudy"
				label: 						qsTr("Non-normal capability study")

				DropDown
				{
					name: 					"nonNormalDist"
					label: 					qsTr("Specify a distribution")
					indexDefaultValue: 		0
					values:
					[
						{ label: qsTr("Weibull"),		value: "Weibull"  },
						{ label: qsTr("Lognormal"),		value: "Lognormal"}
					]
				}
			}
		}

		Group
		{
			title: 							qsTr("Study Limits")

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
			
			CheckBox 
			{ 
				name: 						"controlCharts"
				label: 						qsTr("X-bar & R chart")
				checked: 					true
			}
		}

		Group
		{
			title: 							qsTr("Distribution of the Process")

			CheckBox 
			{ 
				name: 						"histogram"
				label: 						qsTr("Distribution plot")
				checked: 					true

				CheckBox
				{
					name:					"displayDensity"
					label:					qsTr("Display density")
					checked:				true
				}
			}

			CheckBox 
			{ 
				name: 						"probabilityPlot"
				label: 						qsTr("Probability tables and plots")
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
					label:					qsTr("Display grid lines in plot")
				}
			}
		}
	}
}
