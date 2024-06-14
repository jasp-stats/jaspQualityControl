
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
		name: 									"dataFormat"
		label: 									qsTr("Data format")
		id: 									dataFormat
		indexDefaultValue: 						0
		values: 
		[
			{ label: qsTr("Single column"), value: "longFormat"},
			{ label: qsTr("Across rows"), value: "wideFormat"},
		]
	}

	VariablesForm
	{
		id:										variablesFormLongFormat
		visible:								dataFormat.currentValue == "longFormat"

		AvailableVariablesList
		{
			name:								"variablesFormLongFormat"
		}

		AssignedVariablesList
		{
			name:								"measurementLongFormat"
			title:								qsTr("Results")
			id:									measurementLongFormat
			singleVariable:						true
			allowedColumns:						["nominal"]
		}

		AssignedVariablesList
		{
			name:								"operatorLongFormat"
			title:								qsTr("Operator")
			singleVariable:						true
			allowedColumns:						["nominal"]
		}

		AssignedVariablesList
		{
			name:								"partLongFormat"
			title:								qsTr("Part")
			singleVariable:						true
			allowedColumns:						["nominal"]
		}

		AssignedVariablesList
		{
			name:								"standardLongFormat"
			title:								qsTr("Standard")
			singleVariable:						true
			allowedColumns:						["nominal"]
		}
	}

	VariablesForm
	{
		id:										variablesFormWideFormat
		visible:								dataFormat.currentValue == "wideFormat"

		AvailableVariablesList
		{
			name:								"variablesFormwideFormat"
		}

		AssignedVariablesList
		{
			name:								"measurementsWideFormat"
			title:								qsTr("Results")
			id:									measurementsWideFormat
			allowedColumns:						["nominal"]
		}

		AssignedVariablesList
		{
			name:								"operatorWideFormat"
			title:								qsTr("Operator")
			singleVariable:						true
			allowedColumns:						["nominal"]
		}

		AssignedVariablesList
		{
			name:								"partWideFormat"
			title:								qsTr("Part")
			singleVariable:						true
			allowedColumns:						["nominal"]
		}

		AssignedVariablesList
		{
			name:								"standardWideFormat"
			title:								qsTr("Standard")
			singleVariable:						true
			allowedColumns:						["nominal"]
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
				name: 							"positiveReference"
				label: 							qsTr("Positive reference:")
				id:								positiveReference
				enabled:						!kendallsTau.checked
			}

			CheckBox
			{
				name:							"cohensKappa"
				label: 							qsTr("Cohen's kappa (interrater kappa)")
				id:								cohenskappa
				enabled:						positiveReference.value != ""
				checked:						!positiveReference.value == ""
			}

			CheckBox
			{
				name: 							"fleissKappa"
				label: 							qsTr("Fleiss' kappa (multirater kappa)")
				id:								fleisskappa
				checked:						!positiveReference.value == ""
				enabled:						!kendallsTau.checked

			}
		}
	}

	Section
	{
		title: 									qsTr("Tau Study")

		CheckBox
		{
			name: 								"kendallsTau"
			label: 								qsTr("Kendall's tau")
			id:									kendallsTau
		}
	}
}
