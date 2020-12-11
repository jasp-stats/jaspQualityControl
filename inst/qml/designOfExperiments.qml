
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

	DropDown
	{
		id:										design
		name: 									"design"
		label: 									qsTr("Design")
		indexDefaultValue: 						0
		values:
		[
			{ value: "factorial", label: qsTr("Factorial") 			},
			{ value: "screening", label: qsTr("Screening") 			},
			{ value: "response",  label: qsTr("Response surface") 	},
			{ value: "mixture",   label: qsTr("Mixture") 			},
			{ value: "taguchi",   label: qsTr("Taguchi")			}
		]
	}

	CheckBox
	{
		name: 									"displayDesign"
		label:									qsTr("Preview design")
	}

	ColumnLayout
	{
		spacing:								0
		Layout.preferredWidth:					parent.width
		Layout.columnSpan:						2

		RowLayout
		{
			Label { text: qsTr("Factor");		Layout.leftMargin: 5 * preferencesModel.uiScale; Layout.preferredWidth: 42 * preferencesModel.uiScale}
			Label { text: qsTr("Name");			Layout.preferredWidth: 200 * preferencesModel.uiScale	}
			Label { text: qsTr("Level 1");		Layout.preferredWidth: 100 * preferencesModel.uiScale	}
			Label { text: qsTr("Level 2");		Layout.preferredWidth: 100 * preferencesModel.uiScale	}
		}

		ComponentsList
		{
			name:								"factors"
			defaultValues: 			[
				{ name: qsTr("Factor 1"), low: qsTr("Factor 1 Level 1"), high: qsTr("Factor 1 Level 2") },
				{ name: qsTr("Factor 2"), low: qsTr("Factor 2 Level 1"), high: qsTr("Factor 2 Level 2") }
			]
			rowComponent: 						RowLayout
			{
				Row
				{
					spacing:					5 * preferencesModel.uiScale
					Layout.preferredWidth:		40 * preferencesModel.uiScale
					Label 
					{ 
						text: 					rowIndex + 1 
					}
				}
				Row
				{
					spacing:					5 * preferencesModel.uiScale
					Layout.preferredWidth:		200 * preferencesModel.uiScale
					TextField
					{
						label: 					""
						name: 					"name"
						startValue:				qsTr("Factor ") + (rowIndex + 1)
						fieldWidth:				200 * preferencesModel.uiScale
						useExternalBorder:		false
						showBorder:				true
					}
				}
				Row
				{
					spacing:					5 * preferencesModel.uiScale
					Layout.preferredWidth:		100 * preferencesModel.uiScale
					TextField
					{
						label: 					""
						name: 					"low"
						startValue:				qsTr("Factor ") + (rowIndex + 1) + qsTr(" Level 1")
						fieldWidth:				100 * preferencesModel.uiScale
						useExternalBorder:		false
						showBorder:				true
					}
				}
				Row
				{
					spacing:					5 * preferencesModel.uiScale
					Layout.preferredWidth:		100 * preferencesModel.uiScale
					TextField
					{
						label: 					""
						name: 					"high"
						startValue:				qsTr("Factor ") + (rowIndex + 1) + qsTr(" Level 2")
						fieldWidth:				100 * preferencesModel.uiScale
						useExternalBorder:		false
						showBorder:				true
					}
				}
			}
		}
	}

	Section
	{
		visible: 								[0].includes(design.currentIndex)
		title: 									qsTr("Factorial Design Options")
		columns:								2

		RadioButtonGroup
		{
			name: 								"factorialType"
			title:								qsTr("Type of Factorial Design")

			RadioButton
			{
				name:							"factorialTypeDefault"
				label:							qsTr("2-level factorial (default generators)")
				checked:						true
			}

			RadioButton
			{
				name:							"factorialTypeSpecify"
				label:							qsTr("2-level factorial (specify generators)")
			}

			RadioButton
			{
				name:							"factorialTypeSplit"
				label:							qsTr("2-level split-plot (hard-to-change factors)")
			}

			RadioButton
			{
				name:							"factorialTypePlackettBurman"
				label:							qsTr("Plackett-Burman design")
			}

			RadioButton
			{
				name:							"factorialTypeFull"
				label:							qsTr("General full factorial design")
			}
		}

		ColumnLayout
		{

			GroupBox
			{
				title: 							qsTr("Design by")

				RadioButtonGroup
				{
					name:						"designBy"

					RadioButton
					{
						name:					"designByRuns"
						label: 					qsTr("Number of runs")
						childrenOnSameRow:		true
						checked:				true

						DropDown
						{
							name: 				"factorialRuns"
							indexDefaultValue: 	0
							values:
							[
								{ value: "4", 	label: qsTr("4") 	},
								{ value: "8", 	label: qsTr("8") 	},
								{ value: "16", 	label: qsTr("16") 	},
								{ value: "32", 	label: qsTr("32") 	},
								{ value: "64", 	label: qsTr("64") 	},
								{ value: "128", label: qsTr("128")	}
							]
						}
					}

					RadioButton
					{
						name:					"designByResolution"
						label: 					qsTr("Resolution")
						childrenOnSameRow:		true

						DropDown
						{
							name: 				"factorialResolution"
							indexDefaultValue: 	7
							values:
							[
								{ value: "I", 	label: qsTr("I") 	},
								{ value: "II", 	label: qsTr("II") 	},
								{ value: "III", label: qsTr("III") 	},
								{ value: "IV", 	label: qsTr("IV") 	},
								{ value: "V", 	label: qsTr("V") 	},
								{ value: "VI", 	label: qsTr("VI")	},
								{ value: "VIII", label: qsTr("VIII")},
								{ value: "Full", label: qsTr("Full")}
							]
						}
					}

				}

			}	

			GroupBox
			{
				title: 							qsTr("Additional options")

				IntegerField
				{
					name:						"factorialCenterPoints"
					label:						qsTr("Number of center points per block")
					defaultValue:				1
					min:						1
					max:						50
				}

				IntegerField
				{
					name:						"factorialCornerReplicates"
					label:						qsTr("Number of replicates for corner points")
					defaultValue:				3
					min:						1
					max:						50
				}

				IntegerField
				{
					name:						"factorialBlocks"
					label:						qsTr("Number of blocks")
					defaultValue:				1
					min:						1
					max:						50
				}
			}

		}
	}

	Section
	{
		visible: 								[1].includes(design.currentIndex)
		title: 									qsTr("Screening Design Options")

	}


	Section
	{
		visible: 								[2].includes(design.currentIndex)
		title: 									qsTr("Response Surface Design Options")
		
	}

	Section
	{
		visible: 								[3].includes(design.currentIndex)
		title: 									qsTr("Mixture Design Options")
		
	}

	Section
	{
		visible: 								[4].includes(design.currentIndex)
		title: 									qsTr("Taguchi Design Options")
		
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
