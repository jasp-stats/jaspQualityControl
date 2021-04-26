import QtQuick 								    2.8
import QtQuick.Layouts 							1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form
{
	columns:									1

	VariablesForm
	{
		preferredHeight: 						jaspTheme.smallDefaultVariablesFormHeight
		id:										variablesForm

		AvailableVariablesList
		{
			name:								"variablesForm"
		}

		AssignedVariablesList
		{
			id:									variables
			name:								"variables"
			title:								qsTr("Variables")
			allowedColumns:						["scale"]
		}
		AssignedVariablesList
		{
			id:                 			time
			name:               			"time"
			title:             			 	qsTr("Time Stamp (optional)")
			singleVariable:    	 			true
			allowedColumns:     			["nominal", "nominalText", "ordinal", "scale"]
		}
	}

	Group
	{
		title: 									qsTr("Control Charts")
		columns: 								1

		CheckBox
		{
			name: 								"Xbarchart"
			label: 								qsTr("X-bar & R")

			CheckBox
			{
				name: 								"Wlimits"
				label: 								qsTr("Warning limits")
			}

		}

		CheckBox
		{
			name: 								"Schart"
			label: 								qsTr("X-bar & s")

			CheckBox
			{
				name: 								"Wlimits2"
				label: 								qsTr("Warning limits")
			}

		}
	}
}
