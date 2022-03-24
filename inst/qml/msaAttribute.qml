
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
	columns:									1

	DropDown
	{
		id: 									aaadataFormat
		name: 									"AAAdataFormat"
		label: 									qsTr("Data format")
		indexDefaultValue: 						0
		values: [
			{ label: qsTr("Single column"), value: "AAAlongFormat"},
			{ label: qsTr("Across rows"), value: "AAAwideFormat"},
		]
		onValueChanged:
		{
			measurements.itemDoubleClicked(0)
			measurementsLong.itemDoubleClicked(0)
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
			name:								"operators"
			title:								qsTr("Operators")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText"]
		}

		AssignedVariablesList
		{
			name:								"parts"
			title:								qsTr("Parts")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal", "scale"]
		}

		AssignedVariablesList
		{
			id:									measurements
			name:								"measurements"
			title:								qsTr("Results")
			visible:							aaadataFormat.currentValue == "AAAwideFormat"
			allowedColumns:						["nominal", "nominalText", "ordinal","scale"]
		}

		AssignedVariablesList
		{
			id:									measurementsLong
			name:								"measurementsLong"
			title:								qsTr("Results")
			singleVariable:						true
			visible:							aaadataFormat.currentValue == "AAAlongFormat"
			allowedColumns:						["nominal", "nominalText", "ordinal","scale"]
		}

		AssignedVariablesList
		{
			name:								"standard"
			title:								qsTr("Standard")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal","scale"]
		}
	}

	Section
	{
		title:									qsTr("Kappa Study")

		Group
		{
			title: 								qsTr("Tables")

			TextField
			{
				name: 							"PositiveRef"
				id:                 positiveRef
				label: 							qsTr("Positive reference:")
				enabled:            !kendallTau.checked
			}

			CheckBox
			{
				name: 							"AAAcohensKappa"
				id:                 cohenskappa
				label: 							qsTr("Cohen's kappa (interrater kappa)")
				enabled:            positiveRef.value != ""
				checked:            !positiveRef.value == ""
			}

			CheckBox
			{
				name: 							"AAAfleissKappa"
				id:                 fleisskappa
				label: 							qsTr("Fleiss' kappa (multirater kappa)")
				checked:            !positiveRef.value == ""
				enabled:            !kendallTau.checked

			}
		}
	}

	Section
	{
		title: 									qsTr("Tau Study")

		CheckBox
		{
			name: 								"AAAkendallTau"
			id:                   kendallTau
			label: 								qsTr("Kendall's tau")
		}
	}
}
