import QtQuick 								    2.8
import QtQuick.Layouts 						1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form
{
    usesJaspResults:							true
    columns:									1

        VariablesForm
        {
            preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
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
          }

        Group
        {
            title: qsTr("Charts for Individuals")

       Group {
        columns: 									1
        CheckBox {	name: "ImRchart";		label: qsTr("I-MR chart"); checked: true				}
            }
            }
  }
