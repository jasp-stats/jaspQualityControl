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

import QtQuick 2.15
import QtQuick.Layouts							1.3
import JASP										1.0
import JASP.Controls 							1.0
import JASP.Widgets 							1.0


Group
{

	columns : 1
	CheckBox { name: "displayDesign"; label: qsTr("Display design"); checked: false }
	// Export the design to a csv
	Group
	{
		FileSelector
		{
			name:		"exportDesignFile"
			label:		qsTr("Export design:")
			filter:		"*.csv"
			save:		true
		}

		Button
		{
			// this would be better but jasp doesn't support it
//			Layout.alignment:	Qt.AlignRight
			// let's ignore the warning
			anchors.right:		parent.right
			anchors.bottom:		parent.bottom
			text:				actualExporter.checked ? qsTr("<b>Sync Design: On</b>") : qsTr("<b>Sync Design: Off</b>")
			onClicked: 			actualExporter.click()
		}

		CheckBox
		{
			id:					actualExporter
			name:				"actualExporter"
			visible:			false
		}
	}

}
