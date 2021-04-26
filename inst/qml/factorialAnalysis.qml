
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

    VariablesForm
    {
        AvailableVariablesList
        {
            name:                               "FAallVariables"
            label:                              qsTr("Available factors")
        }

        AssignedVariablesList
        {
            name:                               "FAresponse"
            singleVariable:                     true
            label:                              qsTr("Response variable")
        }

        AssignedVariablesList
        {
            name:                               "FAassignedFactors"
            label:                              qsTr("Assigned factors")
        }

        AssignedVariablesList
        {
            name:                               "FArunOrder"
            singleVariable:                     true
            label:                              qsTr("Run order")
        }
    }

    IntegerField
    {
        name:                                   "intOrder"
        label:                                  qsTr("Highest order interaction term:")
        defaultValue:                           1
        min:                                    1
        max:                                    5 // change this to number of items in assignedFactors
    }

    Group
    {
        name:                                   "resPlots"
        title:                                  qsTr("Residuals plots")

        CheckBox
        {
            name:                               "resNorm"
            label:                              qsTr("Normal probability plot of residuals")
        }

        CheckBox
        {
            name:                               "resHist"
            label:                              qsTr("Histogram of residuals")
        }

        CheckBox
        {
            name:                               "resFitted"
            label:                              qsTr("Residuals vs fitted value")
        }

        CheckBox
        {
            name:                               "resOrder"
            label:                              qsTr("Residuals vs run order")
        }
    }
}
