//// Copyright (C) 2013-2018 University of Amsterdam
//// This program is free software: you can redistribute it and/or modify
//// it under the terms of the GNU Affero General Public License as
//// published by the Free Software Foundation, either version 3 of the
//// License, or (at your option) any later version.
//// This program is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//// GNU Affero General Public License for more details.
//// You should have received a copy of the GNU Affero General Public
//// License along with this program.  If not, see
//// <http://www.gnu.org/licenses/>.
////

//import QtQuick 									2.15
//import QtQuick.Layouts 							1.3
//import QtQuick.Controls							2.12
//import JASP										1.0
//import JASP.Controls 							1.0
//import JASP.Widgets 							1.0

//Group
//{

//	property int noContinuous

//	// TODO: should probably come after specifying the variables
//	RowLayout
//	{
//		id: textAbove
//		Label { text: qsTr("Design");	Layout.preferredWidth: 40 * preferencesModel.uiScale; Layout.leftMargin: 45 * preferencesModel.uiScale}
//		Label { text: qsTr("Runs");		Layout.preferredWidth: 40 * preferencesModel.uiScale }
//		Label { text: qsTr("Blocks");	Layout.preferredWidth: 40 * preferencesModel.uiScale }
////			Label { text: qsTr("Center Points") }
//		Label { text: qsTr("Total");	Layout.preferredWidth: 40 * preferencesModel.uiScale }
//		Label { text: qsTr("Cube");		Layout.preferredWidth: 40 * preferencesModel.uiScale }
//		Label { text: qsTr("Axial");	Layout.preferredWidth: 40 * preferencesModel.uiScale }
//		Label { text: qsTr("Alpha");	Layout.preferredWidth: 40 * preferencesModel.uiScale }
//	}

//	Component.onCompleted: {
//		selectedDesign.defaultValues =  Qt.binding(function ()
//		{
////				switch (numberOfContinuous.value)
////				{
////					case 2:
//				if (numberOfContinuous.value)
//				{
//					return [
//						{"design": "full", "runs": 13, "blocks": 1, "total" : 5, "cube": 0, "axial": 0, "alpha": Math.round(Math.sqrt(2), 3)},
//						{"design": "full", "runs": 14, "blocks": 2, "total" : 6, "cube": 3, "axial": 3, "alpha": Math.round(Math.sqrt(2), 3)}
//					];
//				}
//				else
//				{
////					default:
////					case 3:
//					return [
//						{"design": "full", "runs": 20, "blocks": 1, "total" : 6, "cube": 0, "axial": 0, "alpha": Math.round(2**(3/4), 3)},
//						{"design": "full", "runs": 20, "blocks": 2, "total" : 6, "cube": 4, "axial": 2, "alpha": Math.round(2**(3/4), 3)},
//						{"design": "full", "runs": 20, "blocks": 3, "total" : 6, "cube": 4, "axial": 2, "alpha": Math.round(2**(3/4), 3)}
//					];
//				}
////				}
//		})
//	}

//	ComponentsList
//	{

//		width:					form.implicitWidth//textAbove.width
//		id:						selectedDesign
//		name:					"selectedDesign"
//		optionKey:				"name"
//		addItemManually:		false
////			showAddIcon:			false
////			sgo

////			values: {
//////				switch (numberOfContinuous.value)
//////				{
//////					case 2:
////					if (numberOfContinuous.value)
////					{
////						return [
////							{"design": "full", "runs": 13, "blocks": 1, "total" : 5, "cube": 0, "axial": 0, "alpha": Math.round(Math.sqrt(2), 3)},
////							{"design": "full", "runs": 14, "blocks": 2, "total" : 6, "cube": 3, "axial": 3, "alpha": Math.round(Math.sqrt(2), 3)}
////						];
////					}
////					else
////					{
//////					default:
//////					case 3:
////						return [
////							{"design": "full", "runs": 20, "blocks": 1, "total" : 6, "cube": 0, "axial": 0, "alpha": Math.round(2**(3/4), 3)},
////							{"design": "full", "runs": 20, "blocks": 2, "total" : 6, "cube": 4, "axial": 2, "alpha": Math.round(2**(3/4), 3)},
////							{"design": "full", "runs": 20, "blocks": 3, "total" : 6, "cube": 4, "axial": 2, "alpha": Math.round(2**(3/4), 3)}
////						];
////					}
////				}
//////			})




////			ButtonGroup { id: radioGroup }
////			RadioButtonGroup
//		RadioButtonGroup{ id: radioGroup; name: "selected" }

//		rowComponent: RowLayout
//		{

////				RadioButtonGroup{ id: selected; name: "selected"; RadioButton { name: "selectedValue"; label: ""; checked: false; ButtonGroup.group: radioGroup } }
//			RadioButton { name: 1 + rowIndex; label: ""; checked: false; buttonGroup: radioGroup.buttonGroup }

//			TextField		{ id: design;	name: "design";		editable: false;	fieldWidth: 40				} // could also be a dropdown?
//			IntegerField	{ id: runs;		name: "runs";		editable: false									}
//			IntegerField	{ id: blocks;	name: "blocks";		editable: false									}
//			IntegerField	{ id: total;	name: "total";		editable: false									}
//			IntegerField	{ id: cube;		name: "cube";		editable: false;	negativeValues: true		}
//			IntegerField	{ id: axial;	name: "axial";		editable: false;	negativeValues: true		}
//			DoubleField		{ id: alpha;	name: "alpha";		editable: false									}

//		}
//	}
//}
