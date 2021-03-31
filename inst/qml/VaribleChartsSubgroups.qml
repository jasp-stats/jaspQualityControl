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
		title: 									qsTr("Charts for Subgroups")
		columns: 								1

		CheckBox
		{
			name: 								"Xbarchart"
			label: 								qsTr("X-bar & R chart")
		}

		CheckBox
		{
			name: 								"Schart"
			label: 								qsTr("X-bar & S chart")
		}
    }
}
