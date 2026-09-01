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
		info: qsTr("Display the design table in the output. One of the preset designs must be selected in the design table.")
		CheckBox{ name: "codedOutput";	label: qsTr("Coded units");	info: qsTr("Display the factor levels in coded units rather than the given units.")}
		RadioButtonGroup
		{
			name:								"runOrder"
			info:								qsTr("Whether the runs in the design are sorted by run order or by standard order.")

			RadioButton
			{
				name:							"runOrderRandom"
				label:							qsTr("Sort by run order")
				checked:						true
			}

			RadioButton
			{
				name:							"runOrderStandard"
				label:							qsTr("Sort by standard order")
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
			placeholderText: qsTr("e.g. design.csv")
			filter:		"*.csv"
			save:		true
			info:		qsTr("Name and path of the .csv file to save the design to.")
		}

		CheckBox
		{
			id:					actualExporter
			name:				"actualExporter"
			label:				qsTr("Export design")
			Layout.leftMargin:  25 * preferencesModel.uiScale
			enabled: 			exportDesignFile.value != ""
			info:				qsTr("Save the selected design to the specified .csv file.")
		}
	}

}
