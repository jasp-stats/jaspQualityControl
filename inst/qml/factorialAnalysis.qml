
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

import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0


Form
{
	columns:                                    1

	VariablesForm
	{
		AvailableVariablesList
		{
			name:                               "FAallVariables"
			label:                              qsTr("Available factors")
		}

		AssignedVariablesList
		{
			name:                               "FAresponse"
            allowedColumns:                     ["scale"]
			singleVariable:                     true
			label:                              qsTr("Response variable")
		}

		AssignedVariablesList
		{
			name:                               "FAassignedFactors"
            allowedColumns:                     ["ordinal", "nominal", "scale"]
			label:                              qsTr("Assigned factors")
		}

		AssignedVariablesList
		{
			debug:                              true
			name:                               "FAblocks"
			singleVariable:                     true
			label:                              qsTr("Blocks")
		}

		AssignedVariablesList
		{
            id:                                 runOrder
            name:                               "FArunOrder"
            allowedColumns:                     ["scale", "ordinal"]
			singleVariable:                     true
			label:                              qsTr("Run order")
		}
	}

	Section
	{
		title: qsTr("Model")
		
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList { name: "components"; title: qsTr("Components"); source: ["FAassignedFactors"]}
			AssignedVariablesList {  name: "modelTerms"; id: modelTerms; title: qsTr("Model Terms"); listViewType: JASP.Interaction }
		}

	}

Group
{
	columns: 2
	title: qsTr("Plots")

	Group 
	{
		title: qsTr("Design plots")

		CheckBox
		{
			name:                               "showAliasStructure"
			label:                              "Show alias structure"
		}

		CheckBox
		{
			name:                                   "NormalPlot"
			label:                                  qsTr("Normal Plot of the Standardized Effect")

			CheckBox
			{
			name:                                   "addGridlines"
			label:                                  qsTr("Display grid lines")
			}
		}

		CheckBox
		{
			name:                                   "paretoPlot"
			label:                                  qsTr("Pareto Plot of Standardized Effects")
		}
	}

	Group
	{
		name:                                   "resPlots"
		title:                                  qsTr("Residuals plots")

		CheckBox
		{
			name:                               "resNorm"
			label:                              qsTr("Normal probability plot of residuals")
		}

		CheckBox
		{
			name:                               "resHist"
			label:                              qsTr("Histogram of residuals")
		}

		CheckBox
		{
			name:                               "resFitted"
			label:                              qsTr("Residuals vs fitted value")
		}

		CheckBox
		{
			name:                               "resOrder"
			label:                              qsTr("Residuals vs run order")
		}
	}
}
	
}
