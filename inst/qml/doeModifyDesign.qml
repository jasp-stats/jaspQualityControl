
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
	columns:									1

	VariablesForm
	{
		AvailableVariablesList
		{
			name:								"MDallVariables"
			label:								qsTr("Available factors")
		}

		AssignedVariablesList
		{
			name:								"responseVariable"
			singleVariable:						true
			label:								qsTr("Response variable")
		}

		AssignedVariablesList
		{
			name:								"assignedFactors"
			label:								qsTr("Assigned factors")
			id:									assignedFactors
		}

//		AssignedVariablesList
//		{
//			debug:								true
//			name:								"FAblocks"
//			singleVariable:						true
//			label:							  qsTr("Blocks")
//		}

		AssignedVariablesList
		{
			name:								"runOrder"
			label:								qsTr("Run order")
			id:									runOrder
			singleVariable:						true
		}
	}

	RadioButtonGroup
	{
		name:									"unitDisplay"
		title:									qsTr("Unit Display")

		RadioButton
		{
			name:								"coded"
			label:								qsTr("Coded")
			checked:							true

		}

		RadioButton
		{
			name:								"uncoded"
			label:								qsTr("Uncoded")

		}
	}

	RadioButtonGroup
	{
		name:									"displayedRunOrder"
		title:									qsTr("Run Order")
		enabled:								!factorialTypeSplit.checked

		RadioButton
		{
			name:								"random"
			label:								qsTr("Random")
			checked:							true
		}

		RadioButton
		{
			name:								"standard"
			label:								qsTr("Standard")
		}
	}

	Section
	{
		title: 									qsTr("Desired Two-level Factorial Design Options")
		columns:								2

		GroupBox
		{
			title:								qsTr("Design Options")

			RadioButtonGroup
			{
				name:							"designOptionsType"

				RadioButton
				{
					name:						"numberOfRuns"
					label:						qsTr("Number of runs")
					childrenOnSameRow:			true
					checked:					true

					DropDown
					{
						name:					"designOptionsTypeNumberOfRunsValue"
						indexDefaultValue:		0
						values:
						[
							{ value: 2**(1+Math.floor(Math.log2(nAssignedFactors.value))), label: Number(2**(1+Math.floor(Math.log2(nAssignedFactors.value))))},
							{ value: 2**(2+Math.floor(Math.log2(nAssignedFactors.value))), label: Number(2**(2+Math.floor(Math.log2(nAssignedFactors.value))))},
							{ value: 2**(3+Math.floor(Math.log2(nAssignedFactors.value))), label: Number(2**(3+Math.floor(Math.log2(nAssignedFactors.value))))},
							{ value: 2**(4+Math.floor(Math.log2(nAssignedFactors.value))), label: Number(2**(4+Math.floor(Math.log2(nAssignedFactors.value))))},
							{ value: 2**(5+Math.floor(Math.log2(nAssignedFactors.value))), label: Number(2**(5+Math.floor(Math.log2(nAssignedFactors.value))))}
						]
					}
				}

				RadioButton
				{
					name:						"resolution"
					label:						qsTr("Resolution")
					id:							resolution
					childrenOnSameRow:			true

					DropDown
					{
						name:					"designOptionsTypeResolutionValue"
						indexDefaultValue:		1
						values:
						[
							{ value: "Full", label: qsTr("Full")},
							{ value: "III", label: qsTr("III") 	},
							{ value: "IV", 	label: qsTr("IV") 	},
							{ value: "V", 	label: qsTr("V") 	},
							{ value: "VI", 	label: qsTr("VI")	},
							{ value: "VII", label: qsTr("VII")	},
							{ value: "VIII", label: qsTr("VIII")}
						]
					}
				}

				RadioButton
				{
					name:						"fraction"
					label:						qsTr("Fraction")
					childrenOnSameRow:			true

					DropDown
					{
						name:					"designOptionsTypeFractionValue"
						indexDefaultValue:		0
						values:
							[
								{
									value: "0.5",
									label: qsTr("1/2")
								},
								{
									value: nAssignedFactors.value > 5
										   ? "0.25"
										   : "0.5",
									label: nAssignedFactors.value > 5
										   ? qsTr("1/4")
										   : qsTr("1/2")
								},
								{
									value: nAssignedFactors.value > 6
										   ? "0.125"
										   : nAssignedFactors.value > 5
											 ? "0.25"
											 : "0.5",
									label: nAssignedFactors.value > 6
										   ? qsTr("1/8")
										   : nAssignedFactors.value > 5
											 ? qsTr("1/4")
											 : qsTr("1/2")
								}
							]
					}
				}
			}
		}

		GroupBox
		{
			title:								qsTr("Additional Options")

			IntegerField
			{
				name:							"numberCenterPoints"
				label:							qsTr("Number of center points")
				defaultValue:					0
				min:							0
				max:							2**(numberOfFactorsForTable.value - 1)
			}

			IntegerField
			{
				name:							"randomRunsNumberRepetitions"
				label:							qsTr("Number of random runs to repeat")
				defaultValue:					0
				min:							0
				max:							10
			}
		}
	}

	CheckBox
	{
		name:									"desiredDesignTable"
		label:									qsTr("Show desired design")
	}

	IntegerField
	{
		name:									"nAssignedFactors"
		visible:								false
		id:										nAssignedFactors
		defaultValue:							assignedFactors.count
	}
}
