
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
            name:                               "DSAallVariables"
            label:                              qsTr("Available variables")
        }

        AssignedVariablesList
        {
            name:                               "DSAresponse"
            singleVariable:                     true
            label:                              qsTr("Response variable")
        }

        AssignedVariablesList
        {
            name:                               "DSAassignedFactors"
            label:                              qsTr("Assigned factors")
        }

        AssignedVariablesList
        {
            debug:                              true
            name:                               "DSArunOrder"
            singleVariable:                     true
            label:                              qsTr("Run order")
        }
    }

    DoubleField
    {
        name:                                   "DSAalpha"
        label:                                  qsTr("Alpha level:")
        defaultValue:                           0.05
        min:                                    0.01
        max:                                    0.20
    }

    CheckBox
    {
        debug:                                  true
        name:                                   "DSparetoPlot"
        label:                                  qsTr("Pareto plot of standardized effects")
    }

    Group
    {
        name:                                   "DSresPlots"
        title:                                  qsTr("Residuals plots")

        CheckBox
        {
            name:                               "DSresNorm"
            label:                              qsTr("Normal probability plot of residuals")
        }

        CheckBox
        {
            name:                               "DSresHist"
            label:                              qsTr("Histogram of residuals")
        }

        CheckBox
        {
            debug:                              true
            name:                               "DSresFitted"
            label:                              qsTr("Residuals vs fitted value")
        }

        CheckBox
        {
            debug:                              true
            name:                               "DSresOrder"
            label:                              qsTr("Residuals vs run order")
        }
    }
}
