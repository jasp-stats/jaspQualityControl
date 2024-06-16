
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
		indexDefaultValue:						0
		values: [
			{ label: qsTr("Single column"), value: "longFormat"},
			{ label: qsTr("Across rows"), value: "wideFormat"}
		]
		onValueChanged:
		{
			measurementsWideFormat.itemDoubleClicked(0)
			measurementLongFormat.itemDoubleClicked(0)
		}
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
			title:								qsTr("Measurement")
			id:									measurementLongFormat
			singleVariable:						true
			allowedColumns:						["scale"]
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
			name:								"operator"
			title:								qsTr("Operator/Repetition")
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
			name:								"variablesFormWideFormat"
		}

		
		AssignedVariablesList
		{
			name:								"measurementsWideFormat"
			title:								qsTr("Measurements")
			id:									measurementsWideFormat
			visible:							dataFormat.currentValue == "wideFormat"
			allowedColumns:						["scale"]
		}

		AssignedVariablesList
		{
			name:								"partWideFormat"
			title:								qsTr("Part")
			singleVariable:						true
			allowedColumns:						["nominal", "ordinal", "scale"]
		}
	}
		

	Group
	{
		title: 								qsTr("Analysis options")

		CheckBox
		{
			name:							"manualProcessSd"
			label:							qsTr("Process std. dev.")
			childrenOnSameRow:				true

			DoubleField
			{
				name:						"manualProcessSdValue"
				defaultValue:				1
			}
		}

		CheckBox
		{
			name:							"tolerance"
			label:							qsTr("Tolerance")
			childrenOnSameRow: 				true

			DoubleField
			{
				name:						"toleranceValue"
				defaultValue:				1
			}
		}

		CheckBox
		{
			name: 							"repeatabilityAndReproducibilityTable"
			label: 							qsTr("r&R table")
			checked: 						true
		}
	}

	Group
	{
		title: 								qsTr("Plots")

		CheckBox
		{
			name: 							"runChartPart"
			label:							qsTr("Run chart of parts")
		}

		CheckBox
		{
			name:							"scatterPlotMeasurement"
			label:							qsTr("Scatter plot measurement")
			checked:						true

			CheckBox
			{
				name:						"scatterPlotMeasurementFitLine"
				label:						qsTr("Fit line")
				checked:					true
			}

			CheckBox
			{
				name:						"scatterPlotMeasurementAllValues"
				label:						qsTr("Display all measurements")
			}
		}

		CheckBox
		{
			name: 							"rChart"
			label: 							qsTr("Range chart")
		}

		CheckBox
		{
			name: 							"trafficLightChart"
			label: 							qsTr("Traffic light chart")
		}
	}
}
