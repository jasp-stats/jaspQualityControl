import QtQuick									2.8
import QtQuick.Layouts							1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form
{
	columns:									1

	VariablesForm
	{
		preferredHeight: 						jaspTheme.smallDefaultVariablesFormHeight

		AvailableVariablesList
		{
			name:								"variablesForm"
		}

		AssignedVariablesList
		{
			id:									defects
			name:								"defects"
			title:								qsTr("Nonconformities")
			allowedColumns:						["scale", "ordinal", "nominal"]
			singleVariable:						true
		}

		AssignedVariablesList
		{
			id:									samplesize
			name:								"sampleSize"
			title:								qsTr("Sample Size")
			allowedColumns:						["scale"]
			singleVariable:						true
		}

		AssignedVariablesList
		{
			id:									subgroups
			name:								"timeStamp"
			title:								qsTr("Labels (Optional)")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal", "scale"]
		}
	}

	DropDown
	{
		name: 									"palette"
		indexDefaultValue: 						0
		label:									qsTr("Color Palette")
		values: 
		[
			{ value: "iso", label: qsTr("ISO 7870-1") },
			{ value: "jasp", label: qsTr("JASP") },
			{ value: "colorblind", label: qsTr("Colorblind") }
		]
	}

	Group
	{
		title:									qsTr("Plots")

	CheckBox
	{
		name:									"xmrchart"
		label:									qsTr("X-mR - Proportion and moving range")

	}

		CheckBox
		{
			name: 								"npchart"
			label: 								qsTr("NP - Number of nonconforming units")
		}

		CheckBox
		{
			name: 								"pchart"
			label: 								qsTr("P - Proportion of nonconforming units")
		}

		CheckBox
		{
			name: 								"lpchart"
			label: 								qsTr("Laney p' (p-prime) - Proportion of nonconforming units")
		}

		CheckBox
		{
			name: 								"cchart"
			label:								qsTr("C - Number of nonconformities per unit")
		}

		CheckBox
		{
			name:								"uchart"
			label:								qsTr("U - Average nonconformities per unit")
		}

		CheckBox
		{
			name:								"luchart"
			label:								qsTr("Laney u' (u-prime) - Average nonconformities per unit")
		}
	}

	CheckBox
	{
		id:										report
		name: 									"report"
		label: 									qsTr("Generate report")
		enabled:								defects.count > 0

		Group
		{
			columns:							2
			visible:							report.checked

		TextField
		{
			label: 								qsTr("Title")
			name: 								"ATitle"
			placeholderText:					qsTr("Measurement")
			fieldWidth:							100
		}

		TextField
		{
			label: 								qsTr("Name")
			name: 								"AName"
			placeholderText:					qsTr("Name")
			fieldWidth:							100
		}

		TextField
		{
			label: 								qsTr("Operator")
			name: 								"AOperator"
			placeholderText:					qsTr("Operator")
			fieldWidth:							100
		}

		TextField
		{
			label: 								qsTr("ID")
			name: 								"AID"
			placeholderText:					qsTr("ID")
			fieldWidth:							100
		}

		TextField
		{
			label: 								qsTr("Misc")
			name: 								"AMisc"
			placeholderText:					qsTr("Miscellaneous")
			fieldWidth:							100
		}

		TextField
		{
			label: 								qsTr("Appraiser")
			name: 								"AAppraiser"
			placeholderText:					qsTr("Appraiser")
			fieldWidth:							100
		}

		TextField
		{
			label: 								qsTr("Measurement system")
			name: 								"AMeasurement"
			placeholderText:					qsTr("Measurement")
			fieldWidth:							100
		}

		TextField
		{
			label: 								qsTr("Subgroups size")
			name: 								"ASize"
			placeholderText:					qsTr("Size")
			fieldWidth:							100
		}
		TextField
		{
			label: 								qsTr("Time")
			name: 								"ATime"
			placeholderText:					qsTr("Time")
			fieldWidth:							100
		}
		TextField
		{
			label: 								qsTr("Frequency")
			name: 								"AFrequency"
			placeholderText:					qsTr("Frequency")
			fieldWidth:							100
		}
		}
	}
}
