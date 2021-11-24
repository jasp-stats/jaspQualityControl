
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


	DropDown
	{
		id: 									gaugeRRNonRepDataFormat
		name:									"gaugeRRNonRepDataFormat"
		label:									qsTr("Data format")
		indexDefaultValue:						0
		values: [
			{ label: qsTr("Single column"), value: "gaugeRRNonRepLongFormat" },
			{ label: qsTr("Across rows"), value: "gaugeRRNonRepWideFormat" }
		]
	}

	VariablesForm
	{
		id:										variablesForm

		AvailableVariablesList
		{
			name:								"variablesForm"
		}

		AssignedVariablesList
		{
			name:								"operators"
			title:								qsTr("Operators")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal"]
		}

		AssignedVariablesList
		{
			name:								"parts"
			title:								qsTr("Parts")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal", "scale"]
		}

		AssignedVariablesList
		{
			name:								"measurements"
			title:								qsTr("Measurements")
			singleVariable:						true
			allowedColumns:						["scale"]
			visible:							gaugeRRNonRepDataFormat.currentValue == "gaugeRRNonRepLongFormat"
		}

		AssignedVariablesList
		{
			name:								"measurementsWide"
			title:								qsTr("Measurements")
			singleVariable:						false
			allowedColumns:						["scale"]
			visible:							gaugeRRNonRepDataFormat.currentValue == "gaugeRRNonRepWideFormat"
		}
	}

	Section
	{
		title: 									qsTr("Gauge r&R Options")

		Group
		{
			title: 								qsTr("Analysis Options")

			DropDown
			{
				id: 							variationReference
				name: 							"NRstandardDeviationReference"
				label: 							qsTr("Std. Deviation reference")
				indexDefaultValue: 				0
				values: [
					{ label: qsTr("Study Std. Deviation"), value: "studyStandardDeviation"},
					{ label: qsTr("Historical process Std. Deviation"), value: "historicalStandardDeviation"}
				]
			}

			DoubleField
			{
				name:							"NRhistoricalStandardDeviationValue"
				label:							qsTr("Std. Deviation value")
				defaultValue:					0
				enabled:						variationReference.currentValue == "historicalStandardDeviation"
			}

			CheckBox
			{
				name:							"gaugeNRToleranceEnabled"
				label:							qsTr("Tolerance")
				childrenOnSameRow:				true

				DoubleField
				{
					name: 						"NRtolerance"
					defaultValue:				1
					min:						0.001
					decimals:					3
				}
			}

			CheckBox
			{
				name: 							"NRgaugeRR"
				label: 							qsTr("r&R tables")
				checked:						true

				DropDown
				{
					id: 						studyVarMultiplierType
					name:						"NRstudyVarMultiplierType"
					label:						qsTr("Study Var. multiplier type")
					indexDefaultValue:			0
					values: [
						{ label: qsTr("Std. Deviation"), value: "svmSD"},
						{ label: qsTr("Percent"), value: "svmPercent"}
					]
				}

				DoubleField
				{
					name: 						"NRstudyVarMultiplier"
					label: 						qsTr("Study Var. multiplier value")
					fieldWidth: 				60
					defaultValue:				6
					min:						0.001
					max:						99.999
					decimals:					3
				}

				CheckBox
				{
					name:						"NRgaugeVarCompGraph"
					label:						qsTr("Graph variation components")
					checked:					true
				}
			}
		}

		Group
		{
			title:								qsTr("Plots")

			CheckBox
			{
				name: 							"NRrCharts"
				label: 							qsTr("R charts by operator")
			}

			CheckBox
			{
				name: 							"NRxbarCharts"
				label: 							qsTr("X-bar charts by operator")
			}

			CheckBox
			{
				name: 							"NRpartOperatorGraph"
				label: 							qsTr("Measurement by part x operator plot")

				CheckBox
				{
					name: 						"NRpartOperatorGraphAll"
					label: 						qsTr("Display all measurements")
				}
			}

			CheckBox
			{
				name: 							"NRoperatorGraph"
				label: 							qsTr("Measurement by operator plot")
			}
		}
	}

	Section
	{
		title: 									qsTr("Gauge r&R Report")

		TextField
		{
			id:									anovaGaugeNestedTitle
			label: 								qsTr("Title")
			name: 								"anovaGaugeNestedTitle"
			placeholderText:					qsTr("Measurement")
			fieldWidth:							100
		}

		TextField
		{
			id:									anovaGaugeNestedName
			label: 								qsTr("Gauge name")
			name: 								"anovaGaugeNestedName"
			placeholderText:					qsTr("Name")
			fieldWidth:							100
		}

		TextField
		{
			id:									anovaGaugeNestedDate
			label: 								qsTr("Date")
			name: 								"anovaGaugeNestedDate"
			placeholderText:					qsTr("Date")
			fieldWidth:							100
		}

		TextField
		{
			id:									anovaGaugeNestedReportedBy
			label: 								qsTr("Reported by")
			name: 								"anovaGaugeNestedReportedBy"
			placeholderText:					qsTr("Name")
			fieldWidth:							100
		}

		TextField
		{
			id:									anovaGaugeNestedMisc
			label: 								qsTr("Misc")
			name: 								"anovaGaugeNestedMisc"
			placeholderText:					qsTr("Miscellaneous")
			fieldWidth:							100
		}

		CheckBox
		{
			name: 								"anovaGaugeNestedReport"
			label: 								qsTr("Show Report")
		}
	}
}
