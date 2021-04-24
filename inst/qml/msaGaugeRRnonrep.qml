
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
	usesJaspResults:							true
	columns:									1

	VariablesForm
	{
		id:										variablesForm

		AvailableVariablesList
		{
			name:								"variablesForm"
		}

		AssignedVariablesList
		{
			id:									variable1
			name:								"operators"
			title:								qsTr("Operators")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal"]
		}
		
		AssignedVariablesList
		{
			id:									variable2
			name:								"parts"
			title:								qsTr("Parts")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal"]
		}

		AssignedVariablesList
		{
			id:									variable3
			name:								"measurements"
			title:								qsTr("Measurements")
			singleVariable:						true
			allowedColumns:						["scale"]
		}

	}
	

		Group
		{
			title: qsTr("Analysis Options")
			
									DropDown {
                name: "standardDeviationReferenceNR"
                label: qsTr("Std. Deviation reference")
                indexDefaultValue: 0
                values:
                [	
					{label: qsTr("Study Std. Deviation"),					value: "studyStandardDeviation"},
					{label: qsTr("Historical process Std. Deviation"),				value: "historicalStandardDeviation"},
				]
				
				id: variationReference
				}
				DoubleField
				{
					name:			"NRhistoricalStandardDeviationValue"
					label:			qsTr("Std. Deviation value:")
					defaultValue:	0
					enabled:		variationReference.currentValue == "historicalStandardDeviation"
				}
			
			

				DoubleField
				{
					name:			"NRtolerance"
					label:			qsTr("Tolerance:")
					defaultValue:	10
					enabled:		TRUE
				}
			
			CheckBox
			{
				name: "NRgaugeRR";		label: qsTr("r&R tables"); checked: true
				
				
				DropDown{
						name: "NRstudyVarMultiplierType"
						label: qsTr("Study Var. multiplier type")
						indexDefaultValue: 0
						values:
							[
							{label: qsTr("Std. Deviation"),		value: "svmSD"},
							{label: qsTr("Percent"),				value: "svmPercent"},
							]
						id: studyVarMultiplierType
						}
										
				DoubleField { 
						name: "NRstudyVarMultiplier"
						label: qsTr("Study Var. multiplier value:")
						fieldWidth: 60 
						defaultValue: 6
						min:			0.001;
						max:			99.999;						
						decimals: 3
						}
					
				CheckBox{name: "NRgaugeVarCompGraph";		label: qsTr("Graph variation components"); checked: true}
				
			}
		
		}
		
		Group
		{
			title: qsTr("Plots")
			
			CheckBox{name: "NRrCharts";		label: qsTr("R charts by operator")}
			CheckBox{name: "NRxbarCharts";		label: qsTr("X-bar charts by operator")}
			CheckBox{name: "NRpartOperatorGraph";		label: qsTr("Measurement by part x operator plot")}
			CheckBox{name: "NRoperatorGraph";		label: qsTr("Measurement by operator plot")}

		}
	
}
