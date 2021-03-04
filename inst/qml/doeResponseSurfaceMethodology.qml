
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
            AssignedVariablesList  { name: "rsmVariables";	        title: qsTr("Predictors");	suggestedColumns: ["scale"]   }
            AssignedVariablesList  { name: "rsmResponseVariables";	title: qsTr("Response"  );  suggestedColumns: ["scale"]   }
            AssignedVariablesList  { name: "rsmBlocks";	            title: qsTr("Blocks"  );    suggestedColumns: ["nominal"]   }
        }
		
		GroupBox
		{
			title: 							qsTr("Additional options")

            CheckBox
            {
                name:                       "showDesign";label:     qsTr("Suggest designs"); checked: true
                IntegerField
                {
                    name:                       "factorResponse"
                    label:                      "Number of factors"
                    defaultValue:               2

                }


                IntegerField
                {
                    name:						"responseSurfaceCenterReplicates"
                    label:						qsTr("Number of replicates for center points")
                    defaultValue:				3
                    min:						1
                    max:						50
                }


                IntegerField
                {
                    name:						"responseSurfaceStarReplicates"
                    label:						qsTr("Number of replicates for star points")
                    defaultValue:				3
                    min:						1
                    max:						50
                }

            }

            CheckBox
            {
                    name:                      "contour";label:   qsTr("Contour plots")
                    IntegerField
                    {
                        name:						"firstVar"
                        label:						qsTr("Divide the first variable by step")
                        defaultValue:				5
                        min:						1
                        max:						50
                    }


                    IntegerField
                    {
                        name:						"secondVar"
                        label:						qsTr("Divide the second variable by step")
                        defaultValue:				5
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
