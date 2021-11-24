import QtQuick									2.8
import QtQuick.Layouts							1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form
{
  columns:									1

  VariablesForm
  {
    preferredHeight: 						jaspTheme.smallDefaultVariablesFormHeight

    AvailableVariablesList
    {
      name:								"variablesForm"
    }

    AssignedVariablesList
    {
      name:								"D"
      title:								qsTr("Defectives/Defects")
      allowedColumns:						["scale"]
      singleVariable:						true
    }

    AssignedVariablesList
    {
      name:								"total"
      title:								qsTr("Sample")
      allowedColumns:						["scale"]
      singleVariable:						true
    }
  }

  Group
  {
    RadioButtonGroup
    {
      name:								"Attributes"
      title: 								qsTr("Charts for Attributes")
      columns: 							3

      RadioButton
      {
        name: 							"Defectives"
        label: 							qsTr("Defectives")
        checked: 						true

        RadioButtonGroup
        {
          name:						"TypeDefectives"

          RadioButton
          {
            name: 					"npchart"
            label: 					qsTr("np chart")
            checked:		 		true
          }

          RadioButton
          {
            name: 					"pchart"
            label: 					qsTr("p chart")
          }

          RadioButton
          {
            name: 					"Laneyprimechart"
            label: 					qsTr("Laney p' (p-prime) chart")
          }
        }
      }

      RadioButton
      {
        value: 							"Defects"
        label: 							qsTr("Defects")

        RadioButtonGroup
        {
          name:						"TypeDefects"

          RadioButton
          {
            value: 					"cchart"
            label: 					qsTr("c chart")
            checked: 				true
          }

          RadioButton
          {
            value:					"uchart"
            label:					qsTr("u chart")
          }

          RadioButton
          {
            value:					"Laneychart"
            label:					qsTr("Laney u' (u-prime) chart")
          }
        }
      }

      RadioButton
      {
        value:					"ImR"
        label:					qsTr("X-mR chart")
      }
    }
  }

  Section
  {
    title: 									qsTr("Control Charts for Attributes Report")

    TextField
    {
      label: 								qsTr("Title")
      name: 								"ATitle"
      placeholderText:					qsTr("Measurement")
      fieldWidth:							100
    }

    TextField
    {
      label: 								qsTr("Name")
      name: 								"AName"
      placeholderText:					qsTr("Name")
      fieldWidth:							100
    }

    TextField
    {
      label: 								qsTr("Operator")
      name: 								"AOperator"
      placeholderText:					qsTr("Operator")
      fieldWidth:							100
    }

    TextField
    {
      label: 								qsTr("ID")
      name: 								"AID"
      placeholderText:					qsTr("ID")
      fieldWidth:							100
    }

    TextField
    {
      label: 								qsTr("Misc")
      name: 								"AMisc"
      placeholderText:					qsTr("Miscellaneous")
      fieldWidth:							100
    }

    TextField
    {
      label: 								qsTr("Appraiser")
      name: 								"AAppraiser"
      placeholderText:					qsTr("Appraiser")
      fieldWidth:							100
    }

    TextField
    {
      label: 								qsTr("Measurement system")
      name: 								"AMeasurement"
      placeholderText:					qsTr("Measurement")
      fieldWidth:							100
    }

    TextField
    {
      label: 								qsTr("Subgroups size")
      name: 								"ASize"
      placeholderText:			qsTr("Size")
      fieldWidth:							100
    }
    TextField
    {
      label: 								qsTr("Time")
      name: 								"ATime"
      placeholderText:					qsTr("Time")
      fieldWidth:							100
    }
    TextField
    {
      label: 								qsTr("Frequency")
      name: 								"AFrequency"
      placeholderText:					qsTr("Frequency")
      fieldWidth:							100
    }

    CheckBox
    {
      name: 								"AReport"
      label: 								qsTr("Show Report")
    }
  }
}
