
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
			allowedColumns:						["nominal", "nominalText", "ordinal", "scale"]
		}
		
		AssignedVariablesList
		{
			id:									variable2
			name:								"parts"
			title:								qsTr("Parts")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal", "scale"]
		}

		AssignedVariablesList
		{
			id:									variable3
			name:								"measurements"
			title:								qsTr("Measurements")
			singleVariable:						false
			allowedColumns:						["nominal", "nominalText", "ordinal", "scale"]
		}
		
		AssignedVariablesList
		{
			id:									variable4
			name:								"standard"
			title:								qsTr("Standard")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal", "scale"]
		}
		

	}
	
		Group{
						DropDown {
                name: "variationReference"
                label: qsTr("Variation Reference")
                indexDefaultValue: 0
                values:
                [	
					{label: qsTr("Known Process Variation"),				value: "processVariation"},
                    {label: qsTr("Study Variation"),					value: "studyVariation"},
                    {label: qsTr("Tolerance"),					value: "tolerance"},
                ]
				id: variationReference
				}
				DoubleField
				{
					name:			"processVariationOrTolerance"
					label:			qsTr("Value:")
					defaultValue:	0
					enabled:		variationReference.currentValue != "studyVariation"
				}
			}
	
		Section
	{
		title: qsTr("Gauge r&R")
		CheckBox
				{
			name: "gaugeRrTable";		label: qsTr("r&R Table");		checked: true
			
		}
		CheckBox
		{
			name: "gaugeANOVA";		label: qsTr("ANOVA")
		}
		CheckBox
		{
			name: "gaugeComponentsGraph";		label: qsTr("Graph Variation Components")
		}
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
	
	Section
	{
		title: qsTr("Range Method")
		
		CheckBox
		{
                name: "rangeRr";		label: qsTr("r&R Table")
		}
		
		
		CheckBox
		{
                name: "rangeScatterPlotOperatorParts";		label: qsTr("Scatter Plot Operators vs. Parts")
		}
			
		CheckBox
		{
                name: "rangeScatterPlotOperators";		label: qsTr("Scatter Plot Operators")
				

				
			CheckBox
			{
                name: "rangeScatterPlotFitLine";		label: qsTr("Fit Line")
			}
				
			CheckBox
			{
                name: "rangeScatterPlotOriginLine";		label: qsTr("Show Origin Line")
			}
		
		}

		CheckBox
		{
			name: "rangeRchart";		label: qsTr("R Chart")
		}
		
	}
		


	
	Section
	{
		title: qsTr("Attribute Agreement Analysis")
		
		CheckBox
		{
			name: "AAAfleissKappa";		label: qsTr("Fleiss Kappa")
		}
		CheckBox
		{
			name: "AAAcohensKappa";		label: qsTr("Cohen's Kappa")
		}
		CheckBox
		{
			name: "AAAkendallTau";		label: qsTr("Kendall's Tau")
		}
		CheckBox
		{
			name: "AAAtable";		label: qsTr("Attribute Agreement Analysis Table")
		}
		CheckBox
		{
			name: "AAAgraphs";		label: qsTr("Attribute Agreement Analysis Graphs")
		}

	}
		Section
	{
		title: qsTr("Determine Bias")
					DoubleField { name: "biasReferenceValue";	label: qsTr("Reference Value:");		defaultValue: 0;	negativeValues: true	}
					DoubleField { name: "biasTolerance";	label: qsTr("Tolerance Value:");		defaultValue: 0;	negativeValues: true	}
		CheckBox
		{
			name: "biasTable";		label: qsTr("Bias Table")
		}
			
		CheckBox
		{
			name: "biasTtest";		label: qsTr("One Sample T-Test")

		}
		CheckBox
		{
			name: "biasHistogram";		label: qsTr("Histogram")
		}
						CheckBox
		{
			name: "biasRun";		label: qsTr("Run Chart")
		}
	}
}
