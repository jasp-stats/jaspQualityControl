
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

	DropDown
	{
		name: "AAAdataFormat"
		label: qsTr("Data format")
		indexDefaultValue: 0
		values:
			[
			{label: qsTr("Single column"),					value: "AAAlongFormat"},
			{label: qsTr("Across rows"),				value: "AAAwideFormat"},
		]
		id: aaadataFormat
		onValueChanged:
		{
			variable4.itemDoubleClicked(0)
			variable3.itemDoubleClicked(0)
		}
	}

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
			allowedColumns:						["nominal", "nominalText"]
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
			title:								qsTr("Result")
			singleVariable:						false
			visible:							aaadataFormat.currentValue == "AAAwideFormat"
			allowedColumns:						["nominal", "nominalText", "ordinal"]
		}

		AssignedVariablesList
		{
			id:									variable4
			name:								"measurementsLong"
			title:								qsTr("Result")
			singleVariable:						true
			visible:							aaadataFormat.currentValue == "AAAlongFormat"
			allowedColumns:						["nominal", "nominalText", "ordinal"]
		}

		AssignedVariablesList
		{
			id:									variable5
			name:								"standard"
			title:								qsTr("Standard")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal"]
		}
	}

	Section
	{
	  title: qsTr("Kappa Stuides")
	  Group
	  {
		  title: qsTr("Tables")
    		CheckBox
    		{
    			name: "AAAcohensKappa";		label: qsTr("Cohen's kappa (interrater kappa)")
    		}
    		CheckBox
    		{
    			name: "AAAfleissKappa";		label: qsTr("Fleiss' kappa (multirater kappa)")
    		}
    		TextField
      	{
      		name: 					"PositiveRef"
      		label: 					qsTr("Positive refernce:")
      	}
		}
	}

  Section
  {
    title: qsTr("Tau Stuides")
      CheckBox
  		{
  			name: "AAAkendallTau";		label: qsTr("Kendall's tau")
  		}
    }
}
