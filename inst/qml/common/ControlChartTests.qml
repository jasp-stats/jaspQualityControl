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
	title:		qsTr("Tests for control charts")

	DropDown
	{
		name:									"testSet"
		label:									qsTr("Test set")
		id: 									testSet
		indexDefaultValue:						0
		values: [
			{ label: qsTr("JASP"), value: "jaspDefault"},
			{ label: qsTr("Nelson laws"), value: "nelsonLaws"},
			{ label: qsTr("Western Electric rules"), value: "westernElectric"},
			{ label: qsTr("Custom selection"), value: "custom"}
		]
	}


	CheckBox
	{
		name: 								"rule1"
		label: 								qsTr("One point outside of control limits - Test 1: Beyond limit")
		checked:							true
		visible:							testSet.currentValue == "custom"
	}

	CheckBox
	{
		name: 								"rule2"
		label: 								""
		checked:							true
		visible:							testSet.currentValue == "custom"
		childrenOnSameRow:					true

		IntegerField
		{
			name: 								"rule2Value"
			afterLabel:							qsTr("points in a row, on the same side of center line - Test 2: Shift")
			fieldWidth: 						25
			defaultValue: 						testSet.currentValue == "nelsonLaws" ? 9 : testSet.currentValue == "westernElectric" ? 8 : 7
			min:								2
		}
	}

	CheckBox
	{
		name: 								"rule3"
		label: 								""
		checked:							testSet.currentValue != "westernElectric"
		visible:							testSet.currentValue == "custom"
		childrenOnSameRow:					true

		IntegerField
		{
			name: 								"rule3Value"
			afterLabel:							qsTr("points in a row, all increasing or decreasing - Test 3: Trend")
			fieldWidth: 						25
			defaultValue: 						testSet.currentValue == "nelsonLaws" ? 6 : 7
			min:								2
		}
	}

	CheckBox
	{
		name: 								"rule4"
		label: 								""
		checked:							true
		visible:							testSet.currentValue == "custom"
		childrenOnSameRow:					true

		IntegerField
		{
			name: 								"rule4Value"
			afterLabel:							qsTr("out of k+1 points > 2 std. dev. from center line (same side) - Test 4: Increasing variation")
			fieldWidth: 						25
			defaultValue: 						2
			min:								2
		}
	}

	CheckBox
	{
		name: 								"rule5"
		label: 								""
		checked:							testSet.currentValue == "nelsonLaws" | testSet.currentValue == "custom" | testSet.currentValue == "jaspDefault"
		visible:							testSet.currentValue == "custom"
		childrenOnSameRow:					true

		IntegerField
		{
			name: 								"rule5Value"
			afterLabel:							qsTr("points in a row < 1 std. dev from center line (either side) - Test 5: Reducing variation")
			fieldWidth: 						25
			defaultValue: 						15
			min:								2
		}
	}

	CheckBox
	{
		name: 								"rule6"
		label: 								""
		checked:							testSet.currentValue == "nelsonLaws" | testSet.currentValue == "custom" | testSet.currentValue == "jaspDefault"
		visible:							testSet.currentValue == "custom"
		childrenOnSameRow:					true

		IntegerField
		{
			name: 								"rule6Value"
			afterLabel:							qsTr("points in a row > 1 std. dev from center line (either side) - Test 6: Bimodal distribution")
			fieldWidth: 						25
			defaultValue: 						8
			min:								2
		}
	}

	CheckBox
	{
		name: 								"rule7"
		label: 								""
		checked:							testSet.currentValue != "jaspDefault"
		visible:							testSet.currentValue == "custom"
		childrenOnSameRow:					true

		IntegerField
		{
			name: 								"rule7Value"
			afterLabel:							qsTr("out of k+1 points > 1 std. dev. from center line (same side) - Test 7: Slightly increasing variation")
			fieldWidth: 						25
			defaultValue: 						4
			min:								2
		}
	}

	CheckBox
	{
		name: 								"rule8"
		label: 								""
		checked:							testSet.currentValue == "nelsonLaws" | testSet.currentValue == "custom"
		visible:							testSet.currentValue == "custom"
		childrenOnSameRow:					true

		IntegerField
		{
			name: 								"rule8Value"
			afterLabel:							qsTr("points in a row, alternating increase and decrease - Test 8: Oscillation")
			fieldWidth: 						25
			defaultValue: 						14
			min:								2
		}
	}
}
