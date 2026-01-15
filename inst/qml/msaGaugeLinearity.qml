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

import QtQuick
import QtQuick.Layouts
import JASP.Controls

Form
{
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
			name:								"part"
			title:								qsTr("Parts")
			singleVariable:						true
			allowedColumns:						["nominal"]
		}

		AssignedVariablesList
		{
			name:								"measurement"
			title:								qsTr("Measurements")
			singleVariable:						true
			allowedColumns:						["scale"]
		}

		AssignedVariablesList
		{
			name:								"standard"
			title:								qsTr("Standard")
			singleVariable:						true
			allowedColumns:						["scale"]
		}
	}

	Group
	{
		title: 									qsTr("Analysis Options")

		CheckBox
		{
			name: 								"linearityTable"
			label: 								qsTr("Linearity table")
			checked: 							true
		}

		CheckBox
		{
			name: 								"biasTable"
			label: 								qsTr("Bias table")
			checked:							true
		}
	}

	Group
	{
		title:									qsTr("Plots")

		CheckBox
		{
			name:								"linearityAndBiasPlot"
			label:								qsTr("Linearity and bias graph")
			checked:							true
		}
	}

	Group
	{
		title: qsTr("Optional")

		CheckBox
		{
			name: 								"manualProcessVariation"
			label: 								qsTr("Process variation")
			childrenOnSameRow: 					true
			id:									manualProcessVariation

			DoubleField
			{
				name: 							"manualProcessVariationValue"
				defaultValue:					1
				negativeValues:				false
				decimals: 						7
				fieldWidth: 					30
			}
		}

		CheckBox
		{
			name: 								"percentProcessVariationPlot"
			label: 								qsTr("Percent process variation graph")
			checked: 							false
			enabled:							manualProcessVariation.checked
		}
	}
}
