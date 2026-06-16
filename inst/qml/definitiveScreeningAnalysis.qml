
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

import QtQuick
import QtQuick.Layouts
import JASP.Controls

Form
{
    columns:                                    1

    info:										qsTr("Analyse a definitive screening design to identify the influential factors. A regression model is fitted to the response and the standardised effects are assessed against the chosen alpha level.")

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
            info:                               qsTr("The measured response variable of the design.")
        }

        AssignedVariablesList
        {
            name:                               "DSAassignedFactors"
            label:                              qsTr("Assigned factors")
            info:                               qsTr("The factor columns of the design.")
        }

        AssignedVariablesList
        {
            debug:                              true
            name:                               "DSArunOrder"
            singleVariable:                     true
            label:                              qsTr("Run order")
            info:                               qsTr("The column specifying the run order of the design.")
        }
    }

    DoubleField
    {
        name:                                   "DSAalpha"
        label:                                  qsTr("Alpha level:")
        defaultValue:                           0.05
        min:                                    0.01
        max:                                    0.20
        info:                                   qsTr("Alpha (significance) level used to assess the effects.")
    }

    CheckBox
    {
        debug:                                  true
        name:                                   "DSparetoPlot"
        label:                                  qsTr("Pareto plot of standardized effects")
        info:                                   qsTr("Show a Pareto plot of the standardised effects.")
    }

    Group
    {
        name:                                   "DSresPlots"
        title:                                  qsTr("Residuals plots")

        CheckBox
        {
            name:                               "DSresNorm"
            label:                              qsTr("Normal probability plot of residuals")
            info:                               qsTr("Show a normal probability plot of the residuals.")
        }

        CheckBox
        {
            name:                               "DSresHist"
            label:                              qsTr("Histogram of residuals")
            info:                               qsTr("Show a histogram of the residuals.")
        }

        CheckBox
        {
            debug:                              true
            name:                               "DSresFitted"
            label:                              qsTr("Residuals vs fitted value")
            info:                               qsTr("Show a plot of the residuals against the fitted values.")
        }

        CheckBox
        {
            debug:                              true
            name:                               "DSresOrder"
            label:                              qsTr("Residuals vs run order")
            info:                               qsTr("Show a plot of the residuals against the run order.")
        }
    }
}
