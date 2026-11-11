// Copyright (C) 2013-2023 University of Amsterdam
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
import JASP

Group
{

	columns : 1
	CheckBox { name: "displayDesign"; label: qsTr("Display design"); checked: true
		info: qsTr("Display the design table in the output. If a design table is shown in the options, one of its designs needs to be selected first.")
		CheckBox{ name: "codedOutput";	label: qsTr("Coded units"); info: qsTr("Display the factor levels in coded units instead of the given units.")}
		RadioButtonGroup
		{
			name:								"runOrder"
			infoLabel:							qsTr("Sort order")
			info:								qsTr("Whether to sort the runs in the design by run order or standard order.")

			RadioButton
			{
				name:							"runOrderRandom"
				label:							qsTr("Sort by run order")
				info:							qsTr("Sort the runs in the randomized order in which they should be carried out.")
				checked:						true
			}

			RadioButton
			{
				name:							"runOrderStandard"
				label:							qsTr("Sort by standard order")
				info:							qsTr("Sort the runs in standard (non-randomized) order.")
			}
		}
	}
	// Export the design to a csv
	Group
	{
		FileSelector
		{
			id:			exportDesignFile
			name:		"exportDesignFile"
			label:		qsTr("Save as:")
			info:		qsTr("Option to save the design as a .csv file. Specify a name and path for the file or use the browse option.")
			placeholderText: qsTr("e.g. design.csv")
			filter:		"*.csv"
			save:		true
		}

		CheckBox
		{
			id:					actualExporter
			name:				"actualExporter"
			label:				qsTr("Export design")
			info:				qsTr("Check to save the selected design to the specified file.")
			Layout.leftMargin:  25 * preferencesModel.uiScale
			enabled: 			exportDesignFile.value != ""
		}
	}

}
