import QtQuick 								    2.8
import QtQuick.Layouts 						    1.3
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
            AssignedVariablesList
            {
                id:									variables3
                name:								"D"
                title:								qsTr("Defectives/Defects (attributes)")
                allowedColumns:						["scale"]
                singleVariable:						true
            }
            AssignedVariablesList
            {
                id:									variables4
                name:								"total"
                title:								qsTr("Sample (attributes)")
                allowedColumns:						["scale"]
                singleVariable:						true
            }
        }
        Group
        {
            title: qsTr("Charts for variables")

       Group {
           columns: 									1
        CheckBox {	name: "Xbarchart";		label: qsTr("X bar chart")              }
        CheckBox {	name: "Rchart";		label: qsTr("R chart")                      }
        CheckBox {	name: "ImRchart";		label: qsTr("I-mR chart for variables")				}
        CheckBox {	name: "Schart";		label: qsTr("S chart")                      }
            }

    }

    Group
    {
        title: qsTr("Charts for attributes")
        Group{
            columns: 									3
            CheckBox {

                name: "Defectivescharts";		label: qsTr("Defectives charts")
                RadioButtonGroup
                {
                    name:	"TypeDefectives";
                    title:	qsTr("Type")
                    RadioButton { value: "npchart";				label: qsTr("np chart");		checked: true					}
                    RadioButton { value: "pchart";				label: qsTr("p chart")											}
                    RadioButton { value: "Laneyprimechart";		label: qsTr("Laney p’ (p-prime) chart")							}
                }

            }
            CheckBox {	name: "Defectscharts";		label: qsTr("Defects charts")
                RadioButtonGroup
                {
                    name:	"TypeDefects";
                    title:	qsTr("Type")
                    RadioButton { value: "cchart";			label: qsTr("c chart");						checked: true	}
                    RadioButton { value: "uchart";			label: qsTr("u chart")										}
                    RadioButton { value: "Laneychart";		label: qsTr("Laney u’ (u-prime) chart")						}
                }
            }
            CheckBox {	name: "ImRchart2";		label: qsTr("I-mR chart for attributes")												}
        }

    }

    Section
    {
        title: qsTr("Special control charts")
        Group
        {
            VariablesForm
            {
                preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
                id:										variablesForm3

                AvailableVariablesList
                {
                    name:								"variablesForm3"
                }

                AssignedVariablesList
                {
                    id:									variables5
                    name:								"measurements"
                    title:								qsTr("Measurements")
                    allowedColumns:						["scale", "nominal", "ordinal"]
                }
            }
            Group{
                      columns: 									3
            CheckBox {	name: "Cumulativechart";				label: qsTr("Cumulative sum chart")									}
            CheckBox {	name: "Exponentialchart";				label: qsTr("Exponentially weighted moving average chart")			}
            CheckBox {	name: "gchart";							label: qsTr("g chart")												}
            CheckBox {	name: "tchart";							label: qsTr("t chart")												}
            }

        }

    }
}
