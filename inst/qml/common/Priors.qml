//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import JASP.Controls

Group
{
	columns: 2

	property	string	baseName: "populationMeanPrior"
	property	string	baseLabel: qsTr("Population mean")

	property 	bool	fullRealLLine: true
	property 	bool	hasJeffreys: true

// TODO: should be dropdown, see RBMAP
	RadioButtonGroup
	{
		id: 	priorChoice
		title: 	baseLabel
		name: 	baseName

		RadioButton
		{
			visible: 			hasJeffreys
			enabled: 			hasJeffreys
			label: 				qsTr("Jeffreys")
			name: 				baseName + "jeffreys"
			checked: 			hasJeffreys
		}

		RadioButton
		{
			id: 				uniformInformative
			label: 				qsTr("Uniform")
			name: 				baseName + "uniform"
			checked: 			!hasJeffreys
			childrenOnSameRow: 	true

			DoubleField
			{
				id: 			uniformLower
				label: 			qsTr("Lower:")
				name: 			baseName + "uniformLower"
				visible: 		uniformInformative.checked
				defaultValue: 	fullRealLLine ? -3 : 0
				negativeValues: fullRealLLine ? true : false
				max:			uniformUpper.value
			}

			DoubleField
			{
				id: 			uniformUpper
				label: 			qsTr("Upper:");
				name: 			baseName + "uniformUpper"
				visible: 		uniformInformative.checked
				defaultValue: 	3
				fieldWidth: 	50
				negativeValues: fullRealLLine ? true : false
				min:			uniformLower.value
			}
		}

		RadioButton
		{
			id: 				cauchyInformative
			label: 				fullRealLLine ? qsTr("Cauchy") : qsTr("Truncated Cauchy")
			name: 				baseName + "cauchy"
			childrenOnSameRow: 	true

			DoubleField
			{
				label: 			qsTr("location:")
				name: 			baseName + "cauchyLocation"
				visible: 		cauchyInformative.checked
				defaultValue: 	0
				negativeValues: true
			}

			DoubleField
			{
				label: 			qsTr("scale:");
				name: 			baseName + "cauchyScale"
				visible: 		cauchyInformative.checked
				defaultValue: 	0.707
				fieldWidth: 	50
			}
		}

		RadioButton
		{
			id: 				normalInformative
			label: 				fullRealLLine ? qsTr("Normal") : qsTr("Truncated Normal")
			name: 				baseName + "normal"
			childrenOnSameRow:	true

			DoubleField
			{
				label: 			qsTr("mean:")
				name: 			baseName + "normalMean"
				visible: 		normalInformative.checked
				defaultValue: 	0
				negativeValues: true
			}

			DoubleField
			{
				label: 			qsTr("std:")
				name: 			baseName + "normalSd"
				visible: 		normalInformative.checked
				defaultValue: 	0.707
				fieldWidth: 	50
			}


		}

		RadioButton
		{
			id: 				tInformative
			label: 				fullRealLLine ? qsTr("t") : qsTr("Truncated t")
			name: 				baseName + "t"
			childrenOnSameRow: 	true

			DoubleField
			{
				label: 			qsTr("location:")
				name: 			baseName + "tLocation"
				visible: 		tInformative.checked
				defaultValue: 	0
				negativeValues: true
			}

			DoubleField
			{
				label: 			qsTr("scale:")
				name: 			baseName + "tScale"
				visible: 		tInformative.checked
				defaultValue: 	0.707
				fieldWidth: 	50
			}

			IntegerField
			{
				label: 			qsTr("df:");
				name: 			baseName + "tDf";
				visible: 		tInformative.checked;
				min:			1
				defaultValue: 	1
			}
		}
	}

	Group
	{
		title: qsTr("Truncation")
		enabled: priorChoice.value !== baseName + "jeffreys"
		visible: priorChoice.value !== baseName + "jeffreys"

		CheckBox
		{
			name: 				baseName + "truncationLowerBound"
			childrenOnSameRow: 	true

			FormulaField
			{
				id: 			lowerTT
				name: 			baseName + "truncationLowerBoundValue"
				label: 			qsTr("Lower bound:")
				fieldWidth: 	50
				defaultValue: 	fullRealLLine ? -Inf : 0
				max: 			upperTT.value
				min: 			fullRealLLine ? -Inf : 0
			}
		}

		CheckBox
		{
			name: 				baseName + "truncationUpperBound"
			childrenOnSameRow: 	true

			FormulaField
			{
				id: upperTT
				name: 			baseName + "truncationUpperBoundValue"
				label: 			qsTr("Upper bound:")
				fieldWidth: 	50
				defaultValue: 	Inf
				min: 			Math.max(fullRealLLine ? -Inf : 0, lowerTT.value)

			}
		}
	}

}
