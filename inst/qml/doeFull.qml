
// Copyright (C) 2013-2021 University of Amsterdam
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

    GroupBox
    {
        title: 									qsTr("Design Space")
        name:									"designInfo"

        IntegerField
        {
            id:									numberOfFactors
            name:								"numberOfFactors"
            label:								qsTr("Number of factors")
            defaultValue:						3
            min:								2
            max:								256
        }

        IntegerField
        {
            visible:                            false
            id:                                 numberOfFactorsForTable
            name:                               "numberOfFactorsForTable"
            defaultValue:                       numberOfFactors.value
        }
    }

//    RadioButtonGroup
//    {
//        title:                                  qsTr("Unit Display")
//        name:                                   "dataCoding"

//        RadioButton
//        {
//            name:                               "dataCoded"
//            label:                              qsTr("Coded")
//            checked:                            true

//        }

//        RadioButton
//        {
//            name:                               "dataUncoded"
//            label:                              qsTr("Uncoded")

//        }
//    }

    RadioButtonGroup
    {
        name:                                   "runOrderFull"
        title:                                  qsTr("Run Order")

        RadioButton
        {
            name:                               "runOrderRandom"
            label:                              qsTr("Random")
            checked:                            true
        }

        RadioButton
        {
            name:                              "runOrderStandard"
            label:                              qsTr("Standard")
        }
    }

    ColumnLayout
    {
        spacing:                                0
        Layout.preferredWidth:					230 * preferencesModel.uiScale
        Layout.columnSpan:						1

        RowLayout
        {
            Label { text: qsTr("Factor");       Layout.leftMargin: 5 * preferencesModel.uiScale; Layout.preferredWidth: 42 * preferencesModel.uiScale}
            Label { text: qsTr("Name");         Layout.preferredWidth: 100 * preferencesModel.uiScale}
            Label { text: qsTr("Levels");       Layout.preferredWidth: 50 * preferencesModel.uiScale}
        }

        ComponentsList
        {
            name:                               "factors"
            addItemManually:                    false
            values:                             numberOfFactorsForTable.value

            rowComponent:                       RowLayout
            {
                Row
                {
                    spacing:                    5 * preferencesModel.uiScale
                    Layout.leftMargin:          2 * preferencesModel.uiScale
                    Layout.preferredWidth:      40 * preferencesModel.uiScale

                    Label
                    {
                        text:                   rowIndex + 1
                    }
                }
                Row
                {
                    spacing:                    5 * preferencesModel.uiScale
                    Layout.preferredWidth:      100 * preferencesModel.uiScale

                    TextField
                    {
                        id:                     factorName
                        label:                  ""
                        name:                   "factorName"
                        placeholderText:        qsTr("Factor ") + (rowIndex + 1)
                        fieldWidth:             100 * preferencesModel.uiScale
                        useExternalBorder:      false
                        showBorder:             true
                    }
                }
                Row
                {
                    spacing:                    5 * preferencesModel.uiScale
                    Layout.preferredWidth:      50 * preferencesModel.uiScale
                    IntegerField
                    {
                        label:                  ""
                        name:                   "numberOfLevels"
                        fieldWidth:             50 * preferencesModel.uiScale
                        useExternalBorder:      false
                        showBorder:             true
                        defaultValue:           2
                        min:                    2
                        max:                    16
                    }
                }
            }
        }
    }

    GroupBox
    {
        title:                                  qsTr("Additional Options")

        IntegerField
        {
            id:                                 fullCornerReplicates
            name:                               "fullCornerReplicates"
            label:                              qsTr("Number of replicates for corner points")
            defaultValue:                       1
            min:                                1
            max:                                8
        }

        CheckBox
        {
            visible:                            fullCornerReplicates.value > 1
            name:                               "fullRepeats"
            label:                              "Repeats only"
        }

        IntegerField
        {
            name:                               "fullRepeatRuns"
            label:                              qsTr("Number of random runs to repeat")
            defaultValue:                       0
            min:                                0
            max:                                10
        }
    }

    CheckBox
    {
        name:                                   "displayFullDesign"
        label:                                  "Display selected design"
    }

    GroupBox
    {
        FileSelector
        {
            id:                                 fileFull
            name:                               "fileFull"
            label:                              qsTr("Save as:")
            filter:                             "*.csv"
            save:                               true
        }

        Button
        {
            id:                                 exportScreeningDesign
            anchors.right:                      parent.right
            anchors.bottom:                     parent.bottom
            text:                               actualFullExporter.checked ? qsTr("<b>Sync Design: On</b>") : qsTr("<b>Sync Design: Off</b>")
            onClicked:                          actualFullExporter.click()
        }

        CheckBox
        {
            id:                                 actualFullExporter
            name:                               "actualExporter"
            visible:                            false
        }
    }
}
