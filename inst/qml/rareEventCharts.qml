import QtQuick 									2.8
import QtQuick.Layouts 							1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form
{
	columns:									2

	VariablesForm
	{
		id:										variablesForm
		AvailableVariablesList
		{
			name:								"variablesForm"
		}

		AssignedVariablesList
		{
			name:								"variables"
			title:								qsTr("Variables")
			id:									variable
			allowedColumns:						["scale"]
			singleVariable:						false
		}
	}

	Group
	{
		CheckBox
		{
			name: 								"gChart"
			label: 								qsTr("g chart")
			checked:							true
		}

		CheckBox
		{
			name: 								"tChart"
			label: 								qsTr("t chart")
			checked:							true
		}
	}
}
