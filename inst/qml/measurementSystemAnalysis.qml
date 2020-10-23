
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
			id:									variables
			name:								"measurements"
			title:								qsTr("Measurements")
			allowedColumns:						["nominal", "nominalText", "ordinal", "scale"]
		}
	}
	
		Section
	{
		title: qsTr("Gauge r&R")
		
		CheckBox
			{
                name: "gaugeANOVA";		label: qsTr("ANOVA")
			}
		CheckBox
			{
                name: "gaugeTtest";		label: qsTr("One Sample T-Test")
				DoubleField { name: "gaugeTtestValue";	label: qsTr("Test value:");		defaultValue: 0;	negativeValues: true	}
			}
		CheckBox
			{
                name: "gaugeComponentsGraph";		label: qsTr("Graph Variation Components")
			}
		CheckBox
			{
                name: "gaugeRchart";		label: qsTr("R Chart")
			}
		CheckBox
			{
                name: "gaugeXbarChart";		label: qsTr("X-bar Chart")
			}
		CheckBox
			{
                name: "gaugeByPart";		label: qsTr("Measurement by Part Graph")
			}
		CheckBox
			{
                name: "gaugeByOperator";		label: qsTr("Measurement by Operator Graph")
			}
		CheckBox
			{
                name: "gaugeByInteraction";		label: qsTr("Measurement Interaction Graph")
			}
		CheckBox
			{
                name: "gaugeHistogram";		label: qsTr("Histogram")
			}
	}
	
			Section
	{
		title: qsTr("Range Method")
		
		CheckBox
			{
                name: "rangeRr";		label: qsTr("r&R Values")
			}
		
		
		CheckBox
			{
                name: "rangeScatterPlotOperatorParts";		label: qsTr("Scatter Plot Operators vs. Measurements")
			}
		
		
		CheckBox
			{
                name: "rangeScatterPlotOperators";		label: qsTr("Scatter Plot Operators")
				
				CheckBox
				{
                name: "rangeScatterPlotFitLine";		label: qsTr("Fit Line")
				}
			}
			
		CheckBox
			{
                name: "rangeRchart";		label: qsTr("R Chart")
			}
		
		CheckBox
			{
                name: "rangeIsoPlot";		label: qsTr("Iso Plot")
			}
	
	
	
	}
	
			Section
	{
		title: qsTr("Attribute Agreement Analysis")
		
		CheckBox
		{
            name: "AAAkappa";		label: qsTr("Fleiss Kappa")
		}
		CheckBox
		{
            name: "AAAchiSquare";		label: qsTr("Chi Square")
		}
		CheckBox
		{
            name: "AAAkendallTau";		label: qsTr("Kendall's Tau")
		}
		CheckBox
		{
            name: "AAAgraphs";		label: qsTr("Graphs")
		}
	
	}
}
