
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
    usesJaspResults:                            true
    columns:                                    2

    VariablesForm
    {
        AvailableVariablesList
        {
            name:                               "MDallVariables"
            label:                              qsTr("Available factors")
        }

        AssignedVariablesList
        {
            name:                               "MDresponse"
            singleVariable:                     true
            label:                              qsTr("Response variable")
        }

        AssignedVariablesList
        {
            name:                               "MDassignedFactors"
            label:                              qsTr("Assigned factors")
        }

//		AssignedVariablesList
//		{
//			debug:                              true
//			name:                               "FAblocks"
//			singleVariable:                     true
//			label:                              qsTr("Blocks")
//		}

        AssignedVariablesList
        {
            id:                                 runOrder
            name:                               "MDrunOrder"
            singleVariable:                     true
            label:                              qsTr("Run order")
        }
    }

    RadioButtonGroup
    {
        title:                                  qsTr("Data Coding")
        name:                                   "dataCoding"

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
        name:                                   "runOrder"
        title:                                  qsTr("Run Order")
        enabled:                                !factorialTypeSplit.checked

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

    Section
    {
        title: 									qsTr("Desired Two-level Factorial Design Options")
        columns:								2

        GroupBox
        {
            title:                              qsTr("Design by")

            RadioButtonGroup
            {
                name:                           "designBy"

                RadioButton
                {
                    name:                       "byRuns"
                    label:                      qsTr("Number of runs")
                    childrenOnSameRow:          true
                    checked:                    true

                    DropDown
                    {
                        name:                   "MDruns"
                        indexDefaultValue:      0
                        values:
                        [
                            { value: "8", 	label: qsTr("8") 	},
                            { value: "16", 	label: qsTr("16") 	},
                            { value: "32", 	label: qsTr("32") 	},
                            { value: "64", 	label: qsTr("64") 	},
                            { value: "128", label: qsTr("128")	},
                            { value: "256", label: qsTr("256")	},
                            { value: "512", label: qsTr("512")	},
                            { value: "1024", label: qsTr("1024")},
                            { value: "2048", label: qsTr("2048")},
                            { value: "4096", label: qsTr("4096")}
                        ]
                    }
                }

                RadioButton
                {
                    id:                         byResolution
                    name:                       "byResolution"
                    label:                      qsTr("Resolution")
                    childrenOnSameRow:          true

                    DropDown
                    {
                        name:                   "MDresolution"
                        indexDefaultValue:      1
                        values:
                        [
                            { value: "Full", label: qsTr("Full")},
                            { value: "III", label: qsTr("III") 	},
                            { value: "IV", 	label: qsTr("IV") 	},
                            { value: "V", 	label: qsTr("V") 	},
                            { value: "VI", 	label: qsTr("VI")	},
                            { value: "VII", label: qsTr("VII")	},
                            { value: "VIII", label: qsTr("VIII")}
                        ]
                    }
                }

                RadioButton
                {
                    name:                       "byFraction"
                    label:                      qsTr("Fraction")
                    childrenOnSameRow:          true

                    DropDown
                    {
                        name:                   "MDfraction"
                        indexDefaultValue:      0
                        values:
                        [
                            { value: "1/2", label: qsTr("1/2")  },
                            { value: "1/4", label: qsTr("1/4")  },
                            { value: "1/8", label: qsTr("1/8")  }
                        ]
                    }
                }
            }
        }


        GroupBox
        {
            title:                              qsTr("Additional Options")

            IntegerField
            {
                name:                           "MDcenterPoints"
                label:                          qsTr("Number of center points per block")
                defaultValue:                   0
                min:                            0
                max:                            2**(numberOfFactorsForTable.value - 1)
            }

            IntegerField
            {
                id:                             mdCornerReplicates
                name:                           "MDcornerReplicates"
                label:                          qsTr("Number of replicates for corner points")
                defaultValue:                   1
                min:                            1
                max:                            8

            }

            CheckBox
            {
                visible:                        mdCornerReplicates.value > 1
                name:                           "MDrepeats"
                label:                          "Repeats only"
            }


            IntegerField
            {
                name:                           "repeatRuns"
                label:                          qsTr("Number of random runs to repeat")
                defaultValue:                   0
                min:                            0
                max:                            10
            }
        }
    }

    CheckBox
    {
        name:                                   "showDesiredDesign"
        label:                                  qsTr("Show desired design")
    }
}
