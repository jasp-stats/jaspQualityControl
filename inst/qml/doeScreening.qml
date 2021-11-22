
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

import QtQuick                                  2.8
import QtQuick.Layouts                          1.3
import JASP.Controls                            1.0
import JASP.Widgets                             1.0

Form
{
    usesJaspResults:                            true
    columns:                                    1

    DropDown
    {
        id:                                     screeningType
        name:                                   "screeningType"
        label:                                  qsTr("Type of Screening Design")
        indexDefaultValue:                      0
        values:
        [
            { value: "PBdes", 	label: qsTr("Plackett-Burman")       },
            { value: "DSdes", 	label: qsTr("Definitive screening") 	}
        ]

    }

    GroupBox
    {
        title: 									qsTr("Design Information")
        name:									"designInfo"

        IntegerField
        {
            id:									numberOfFactorsScreen3
            name:								"numberOfFactorsScreen3"
            label:								qsTr("Number of 3-level factors")
            visible:                            screeningType.currentIndex == 1
            defaultValue:						screeningType.currentIndex == 1 ? 4 : 0
            min:								screeningType.currentIndex == 1 ? 4 : 0
            max:								12
        }

        IntegerField
        {
            visible:                            false
            id:                                 numberOfFactorsForTableScreen3
            name:                               "numberOfFactorsForTableScreen3"
            defaultValue:                       numberOfFactorsScreen3.value
        }

        IntegerField
        {
            id:									numberOfFactorsScreen2
            name:								"numberOfFactorsScreen2"
            label:								qsTr("Number of 2-level factors")
            defaultValue:						screeningType.currentIndex == 0 ? 4 : 0
            min:								screeningType.currentIndex == 0 ? 4 : 0
            max:								screeningType.currentIndex == 0 ? 256 : 4
        }

        IntegerField
        {
            visible:                            false
            id:                                 numberOfFactorsForTableScreen2
            name:                               "numberOfFactorsForTableScreen2"
            defaultValue:                       numberOfFactorsScreen2.value
        }

        DropDown
        {
            name:                               "PBruns"
            label:                              qsTr("Number of runs")
            visible:                            screeningType.currentIndex == 0
            indexDefaultValue:                  0
            values:
            [
                { value: (Math.floor(numberOfFactorsForTableScreen2.value/4)+1)*4, label: Number((Math.floor(numberOfFactorsForTableScreen2.value/4)+1)*4)},
                { value: (Math.floor(numberOfFactorsForTableScreen2.value/4)+2)*4, label: Number((Math.floor(numberOfFactorsForTableScreen2.value/4)+2)*4)},
                { value: (Math.floor(numberOfFactorsForTableScreen2.value/4)+3)*4, label: Number((Math.floor(numberOfFactorsForTableScreen2.value/4)+3)*4)},
                { value: (Math.floor(numberOfFactorsForTableScreen2.value/4)+4)*4, label: Number((Math.floor(numberOfFactorsForTableScreen2.value/4)+4)*4)},
                { value: (Math.floor(numberOfFactorsForTableScreen2.value/4)+5)*4, label: Number((Math.floor(numberOfFactorsForTableScreen2.value/4)+5)*4)}
            ]
        }
    }

    RadioButtonGroup
    {
        title:                                  qsTr("Unit Display")
        name:                                   "dataCodingScreen"

        RadioButton
        {
            name:                               "dataCoded"
            label:                              qsTr("Coded")
            checked:                            true

        }

        RadioButton
        {
            name:                               "dataUncoded"
            label:                              qsTr("Uncoded")

        }
    }

    RadioButtonGroup
    {
        name:                                   "runOrderScreen"
        title:                                  qsTr("Run Order")

        RadioButton
        {
            name:                              "runOrderStandard"
            label:                              qsTr("Standard")
            checked:                            true
        }

        RadioButton
        {
            name:                               "runOrderRandom"
            label:                              qsTr("Random")
        }
    }

    ColumnLayout
    {
        visible:                                screeningType.currentIndex == 1
        spacing:                                0
        Layout.preferredWidth:					parent.width
        Layout.columnSpan:						1

        RowLayout
        {
            Label { text: qsTr("Factor");		Layout.leftMargin: 5 * preferencesModel.uiScale; Layout.preferredWidth: 42 * preferencesModel.uiScale}
            Label { text: qsTr("Name");			Layout.preferredWidth: 150 * preferencesModel.uiScale}
            Label { text: qsTr("Level 1");		Layout.preferredWidth: 100 * preferencesModel.uiScale}
            Label { text: qsTr("Level 2");		Layout.preferredWidth: 100 * preferencesModel.uiScale}
            Label { text: qsTr("Level 3");		Layout.preferredWidth: 100 * preferencesModel.uiScale}
        }

        ComponentsList
        {
            name:								"factors3"
            addItemManually:                    false
            values:                             numberOfFactorsForTableScreen3.value // update only when numberOfFactors.value gets "entered"

            rowComponent: 						RowLayout
            {
                Row
                {
                    spacing:					5 * preferencesModel.uiScale
                    Layout.preferredWidth:		40 * preferencesModel.uiScale
                    Label
                    {
                        text: 					rowValue
                    }
                }
                Row
                {
                    spacing:					5 * preferencesModel.uiScale
                    Layout.preferredWidth:		100 * preferencesModel.uiScale

                    TextField
                    {
                        label: 					""
                        name: 					"factorName3"
                        placeholderText:		qsTr("Factor ") + (rowIndex + 1)
                        fieldWidth:				100 * preferencesModel.uiScale
                        useExternalBorder:		false
                        showBorder:				true
                    }
                }
                Row
                {
                    spacing:					5 * preferencesModel.uiScale
                    Layout.preferredWidth:		100 * preferencesModel.uiScale
                    TextField
                    {
                        label: 					""
                        name: 					"low3"
                        placeholderText:		qsTr("Factor ") + (rowIndex + 1) + qsTr(" Level 1")
                        fieldWidth:				100 * preferencesModel.uiScale
                        useExternalBorder:		false
                        showBorder:				true
                    }
                }
                Row
                {
                    spacing:					5 * preferencesModel.uiScale
                    Layout.preferredWidth:		100 * preferencesModel.uiScale
                    TextField
                    {
                        label: 					""
                        name: 					"center3"
                        placeholderText:		qsTr("Factor ") + (rowIndex + 1) + qsTr(" Level 2")
                        fieldWidth:				100 * preferencesModel.uiScale
                        useExternalBorder:		false
                        showBorder:				true
                    }
                }
                Row
                {
                    spacing:					5 * preferencesModel.uiScale
                    Layout.preferredWidth:		100 * preferencesModel.uiScale
                    TextField
                    {
                        label: 					""
                        name: 					"high3"
                        placeholderText:		qsTr("Factor ") + (rowIndex + 1) + qsTr(" Level 3")
                        fieldWidth:				100 * preferencesModel.uiScale
                        useExternalBorder:		false
                        showBorder:				true
                    }
                }
            }
        }
    }

    ColumnLayout
    {
        spacing:                                0
        Layout.preferredWidth:					parent.width
        Layout.columnSpan:						1
        visible:                                numberOfFactorsForTableScreen2.value > 0

        RowLayout
        {
            Label { text: qsTr("Factor");		Layout.leftMargin: 5 * preferencesModel.uiScale; Layout.preferredWidth: 42 * preferencesModel.uiScale}
            Label { text: qsTr("Name");			Layout.preferredWidth: 150 * preferencesModel.uiScale}
            Label { text: qsTr("Level 1");		Layout.preferredWidth: 100 * preferencesModel.uiScale}
            Label { text: qsTr("Level 2");		Layout.preferredWidth: 100 * preferencesModel.uiScale}
        }

        ComponentsList
        {
            name:								"factors2"
            addItemManually:                    false
            values:                             numberOfFactorsForTableScreen2.value // update only when numberOfFactors.value gets "entered"

            rowComponent: 						RowLayout
            {
                Row
                {
                    spacing:					5 * preferencesModel.uiScale
                    Layout.preferredWidth:		40 * preferencesModel.uiScale
                    Label
                    {
                        text: 					Number(rowValue)+Number(numberOfFactorsForTableScreen3.value)
                    }
                }
                Row
                {
                    spacing:					5 * preferencesModel.uiScale
                    Layout.preferredWidth:		100 * preferencesModel.uiScale

                    TextField
                    {
                        label: 					""
                        name: 					"factorName2"
                        placeholderText:		qsTr("Factor ") + (Number(rowValue)+Number(numberOfFactorsForTableScreen3.value))
                        fieldWidth:				100 * preferencesModel.uiScale
                        useExternalBorder:		false
                        showBorder:				true
                    }
                }
                Row
                {
                    spacing:					5 * preferencesModel.uiScale
                    Layout.preferredWidth:		100 * preferencesModel.uiScale
                    TextField
                    {
                        label: 					""
                        name: 					"low2"
                        placeholderText:		qsTr("Factor ") + (Number(rowValue)+Number(numberOfFactorsForTableScreen3.value)) + qsTr(" Level 1")
                        fieldWidth:				100 * preferencesModel.uiScale
                        useExternalBorder:		false
                        showBorder:				true
                    }
                }
                Row
                {
                    spacing:					5 * preferencesModel.uiScale
                    Layout.preferredWidth:		100 * preferencesModel.uiScale
                    TextField
                    {
                        label: 					""
                        name: 					"high2"
                        placeholderText:		qsTr("Factor ") + (Number(rowValue)+Number(numberOfFactorsForTableScreen3.value)) + qsTr(" Level 2")
                        fieldWidth:				100 * preferencesModel.uiScale
                        useExternalBorder:		false
                        showBorder:				true
                    }
                }
            }
        }
    }

    GroupBox
    {
        title:                                  qsTr("Additional Options")
        enabled:                                numberOfFactorsForTableScreen2.value == 0 | screeningType.currentIndex == 0

        IntegerField
        {
            name:                               "screeningCenterPoints"
            label:                              qsTr("Number of centre points")
            defaultValue:                       0
            min:                            	0
            max:                            	256
        }

        IntegerField
        {
            enabled:                            screeningType.currentIndex == 0
            id:                                 screeningCornerReplicates
            name:                               "screeningCornerReplicates"
            label:                              qsTr("Number of replicates for corner points")
            defaultValue:                       1
            min:                                1
            max:                                10

        }

        CheckBox
        {
            visible:                            screeningType.currentIndex == 0 & screeningCornerReplicates.value > 1
            name:                               "screeningRepeats"
            label:                              "Repeats only"
        }
    }

    CheckBox
    {
        name:                                   "displayScreeningDesign"
        label:                                  "Display selected design"
    }

    GroupBox
    {
        FileSelector
        {
            id:                                 fileScreening
            name:                               "fileScreening"
            label:                              qsTr("Save as:")
            filter:                             "*.csv"
            save:                               true
        }

        Button
        {
            id:                                 exportScreeningDesign
            anchors.right:                      parent.right
            anchors.bottom:                     parent.bottom
            text:                               actualScreeningExporter.checked ? qsTr("<b>Sync Design: On</b>") : qsTr("<b>Sync Design: Off</b>")
            onClicked:                          actualScreeningExporter.click()
        }

        CheckBox
        {
            id:                                 actualScreeningExporter
            name:                               "actualExporter"
            visible:                            false
        }
    }
}
