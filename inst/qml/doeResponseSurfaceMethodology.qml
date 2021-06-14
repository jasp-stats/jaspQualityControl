
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
	usesJaspResults:							true
	columns:									2

        VariablesForm
        {
            AvailableVariablesList { name: "rsmVariablesList" }
            AssignedVariablesList
            {
                name: "rsmVariables"
                title: qsTr("Predictors [Location in coded format]")
                suggestedColumns:   ["scale", "ordinal", "nominal"]

                rowComponent: Row
                            {
                                DoubleField {name: "Point_P"; negativeValues: true}
                            }


            }
            AssignedVariablesList  { name: "rsmResponseVariables";	title: qsTr("Response");  suggestedColumns:   ["scale", "ordinal", "nominal"]; singleVariable: true}
            AssignedVariablesList  { name: "rsmBlocks";	            title: qsTr("Blocks");    suggestedColumns:   ["scale", "ordinal", "nominal"]; singleVariable: true}
        }



		
        Group
        {
            title: qsTr("Contour Plot Options")
            VariablesForm
            {
                    preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
                    AvailableVariablesList
                    {
                        name:  "rsmVariables2";	    source:"rsmVariables"

                    }




                    AssignedPairsVariablesList
                    {	name:  "pairs";				suggestedColumns: ["scale", "ordinal", "nominal"] }


            }

            TextArea
            {
                title: qsTr("RSM formula")
                name: "Formula"
                text: "Enter the formula used in RSM"
                width: 200
            }


            CheckBox
            {
                    name:                      "contour";label:   qsTr("Contour Surface")
                    CheckBox
                    {
                        name:                       "cplot"
                        label:                      qsTr("Only show 2D plot")
                    }

                    CheckBox
                    {
                        name:                       "coded"
                        label:                      qsTr("Show analysis and graphs in coded form")
                    }

                    CheckBox
                    {
                        name:                       "legend"
                        label:                      qsTr("Show legend next to graph")
                    }
                    DropDown
                    {
                        name:                       "divide"
                        label:                      qsTr("Divide response surface into N parts")
                        values:                     [2,3,4,5,6,7]
                    }

                    Slider
                    {
                        name:                       "phi"
                        label:                      qsTr("Rotating angle (vertical plane)")
                        value:                      0


                    }

                    Slider
                    {
                        name:                       "theta"
                        label:                      qsTr("Rotating angle (horizontal plane)")
                        value:                      0.5
                        vertical:                   false
                    }

            }
        }



        Section
		{
            title: 							qsTr("Box designs")

            CheckBox
            {
                name:                       "showDesign";label:            qsTr("Central composite design")
                IntegerField
                {
                    name:                       "factorResponse"
                    label:                      "Number of factors"
                    defaultValue:               2
                    min:                        2
                    max:                        50
                }

                IntegerField
                {
                    name:						"responseSurfaceCentre"
                    label:						qsTr("Number of centre points")
                    defaultValue:				3
                    min:						1
                    max:						50
                }


                IntegerField
                {
                    name:						"responseSurfaceReplicationStar"
                    label:						qsTr("Number of replicates")
                    defaultValue:				3
                    min:						1
                    max:						50
                }



            }



        }

	Item 
	{
        Layout.preferredHeight: 				generateDesign.height
        Layout.fillWidth: 						true
        Layout.columnSpan:						2

        Button
        {
            id: 								generateDesign
            anchors.right:						parent.right
            anchors.bottom:						parent.bottom
            text: 								qsTr("<b>Create Design</b>")
            // onClicked: 							form.exportResults()
        }
    }
}
