
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
	}
	
		Group
	{
		DoubleField { name: "processSD";	label: qsTr("Process Standard Deviation");		defaultValue: 0;	negativeValues: true	}
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
			name: "gaugeRchart";		label: qsTr("R Chart by Operator")
		}
		CheckBox
		{
			name: "gaugeXbarChart";		label: qsTr("X-bar Chart by Operator")
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
	
	Section
	{
		title: qsTr("Iso Plot")
		
		CheckBox
			{
                name: "IsoPlot";		label: qsTr("Iso Plot")
			}
			
			CheckBox
			{
                name: "IsoPlotTable";		label: qsTr("Iso Plot Table")
			}
	}
}
