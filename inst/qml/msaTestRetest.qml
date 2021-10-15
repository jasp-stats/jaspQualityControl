
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
		name: "testRetestDataFormat"
		label: qsTr("Data format")
		indexDefaultValue: 0
		values:
			[
			{label: qsTr("Single column"),					value: "testRetestLongFormat"},
			{label: qsTr("Across rows"),				value: "testRetestWideFormat"},
		]
		id: gaugeRRdataFormat
		onValueChanged:
		{
			variable3.itemDoubleClicked(0)
			variable4.itemDoubleClicked(0)
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
			title:								qsTr("Operator / Repetition")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal"]
			visible:							gaugeRRdataFormat.currentValue == "testRetestLongFormat"
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
			id:									variable4
			name:								"measurementsLong"
			title:								qsTr("Measurements")
			singleVariable:						true
			visible:							gaugeRRdataFormat.currentValue == "testRetestLongFormat"
			allowedColumns:						["scale"]
		}

		AssignedVariablesList
		{
			id:									variable3
			name:								"measurements"
			title:								qsTr("Measurements")
			singleVariable:						false
			visible:							gaugeRRdataFormat.currentValue == "testRetestWideFormat"
			allowedColumns:						["scale"]
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
				name: "rangeRr"
				label: qsTr("r&R table")
				checked: true
			}
		}

		Group
		{
			title: qsTr("Plots")

			CheckBox
			{
				name: "rangeScatterPlotOperatorParts"
				label: qsTr("Run chart of parts")
			}

			CheckBox
			{
			name: "rangeScatterPlotOperators"
			label: qsTr("Scatter plot")
			checked: true

				CheckBox
				{
				name: "rangeScatterPlotFitLine"
				label: qsTr("Regression line")
				checked: true
				}

			}


			CheckBox
			{
				name: "rangeRchart"
				label: qsTr("Range chart")

			    CheckBox
				  {
				  name: "jitter"
				  label: qsTr("Add jitter")
				  }
			}
						CheckBox
			{
				name: "trafficPlot"
				label: qsTr("Traffic light graph")
			}
		}
	}
}
