
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
			singleVariable:						false
			allowedColumns:						["scale"]
		}

	}
	
			RadioButtonGroup
		{
			name: "gaugeRRmethod"
			RadioButton { name: "anovaMethod";	label: qsTr("ANOVA Method"); checked: true}
			RadioButton { name: "rangeMethod";  label: qsTr("Range Method")	}
		}
	
	
		Section
	{
		title: qsTr("ANOVA Method Options")
		
		Group
		{
			title: qsTr("Analysis Options")
			
									DropDown {
                name: "standardDeviationReference"
                label: qsTr("Std. Deviation Reference")
                indexDefaultValue: 0
                values:
                [	
					{label: qsTr("Study Std. Deviation"),					value: "studyStandardDeviation"},
					{label: qsTr("Historical Process Std. Deviation"),				value: "historicalStandardDeviation"},
				]
				
				id: variationReference
				}
				DoubleField
				{
					name:			"historicalStandardDeviationValue"
					label:			qsTr("Std. Deviation Value:")
					defaultValue:	0
					enabled:		variationReference.currentValue == "historicalStandardDeviation"
				}
			
			

				DoubleField
				{
					name:			"tolerance"
					label:			qsTr("Tolerance:")
					defaultValue:	10
					enabled:		TRUE
				}
			
			CheckBox
			{
				name: "gaugeANOVA";		label: qsTr("R&R Table ANOVA Method"); checked: true
			
						DoubleField { name: "alphaForANOVA";		label: qsTr("Alpha Interaction Removal:");	fieldWidth: 60; defaultValue: 0.05; max: 1; decimals: 3 }
					
				
				DropDown{
						name: "studyVarMultiplierType"
						label: qsTr("Study Var. Multiplier Type")
						indexDefaultValue: 0
						values:
							[
							{label: qsTr("Std. Deviation"),		value: "svmSD"},
							{label: qsTr("Percent"),				value: "svmPercent"},
							]
						id: studyVarMultiplierType
						}
										
				DoubleField { 
						name: "studyVarMultiplier"
						label: qsTr("Study Var. Multiplier Value:")
						fieldWidth: 60 
						defaultValue: 6
						min:			0.001;
						max:			99.999;						
						decimals: 3
						}
					
				CheckBox{
						name: "gaugeVarCompGraph";		label: qsTr("Graph Variation Components"); checked: true
						}
				
			}
		
		}
		
		Group
		{
			title: qsTr("Plots")

			CheckBox
			{
				name: "gaugeRchart";		label: qsTr("R Chart by Operator")
			}

			CheckBox
			{
				name: "gaugeXbarChart";		label: qsTr("X-bar Chart by Operator")
			}
		
			CheckBox
			{
				name: "gaugeScatterPlotOperators";		label: qsTr("Scatter Plot Operators")
				
				CheckBox
				{
					name: "gaugeScatterPlotFitLine";		label: qsTr("Fit Line")
				}

				CheckBox
				{
					name: "gaugeScatterPlotOriginLine";		label: qsTr("Show Origin Line")
				}
			}

			CheckBox
			{
			name: "gaugeByPart";		label: qsTr("Measurement by Part Graph")
			
				CheckBox
				{
					name: "gaugeByPartAll";		label: qsTr("Display all Measurements")
				}
			}	

			CheckBox
			{
			name: "gaugeByOperator";		label: qsTr("Measurement by Operator Graph")
			}
		
			CheckBox
			{
			name: "gaugeByInteraction";		label: qsTr("Measurement Interaction Graph")
			}
		}
	}
	
	Section
	{
		title: qsTr("Range Method Options")
		
			Group
			{
				title: qsTr("Analysis Options")
				
								DoubleField
				{
					name:			"rangePSD"
					label:			qsTr("Process Std. Deviation:")
					defaultValue:	1
					enabled:		TRUE
				}
				
				CheckBox
				{
					name: "rangeRr";		label: qsTr("r&R Table"); checked: true
				}
			}
		
			Group
			{
				title: qsTr("Plots")
				
				CheckBox
				{
					name: "rangeScatterPlotOperatorParts";		label: qsTr("Scatter Plot Operators vs. Parts")
				}
			
				CheckBox
				{
					name: "rangeScatterPlotOperators";		label: qsTr("Scatter Plot Operators"); checked: true
				
					CheckBox
					{
						name: "rangeScatterPlotFitLine";		label: qsTr("Fit Line"); checked: true
					}
				
					CheckBox
					{
						name: "rangeScatterPlotOriginLine";		label: qsTr("Show Origin Line"); checked: true
					}
		
				}
				CheckBox
				{
					name: "rangeRchart";		label: qsTr("R Chart")
				}
			}
	}
}
