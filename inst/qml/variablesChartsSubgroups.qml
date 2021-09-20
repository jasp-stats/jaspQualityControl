import QtQuick 								    2.8
import QtQuick.Layouts 							1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form
{
	columns:								2


	DropDown
	{
		name: "CCDataFormat"
		label: qsTr("Data format")
		indexDefaultValue: 0
		values:
			[
			{label: qsTr("Column"),					value: "CClongFormat"},
			{label: qsTr("Row"),				value: "CCwideFormat"},
		]
		id: pcDataFormat
	}

	VariablesForm
	{
		id:                   				variablesForm

		AvailableVariablesList
		{
			name:               			"variablesForm"
		}

		AssignedVariablesList
		{
			id:                 			variablesLong
			name:               			"variablesLong"
			title:              			qsTr("Measurements")
			allowedColumns:     			["scale"]
			singleVariable:					true
			visible:						pcDataFormat.currentValue == "CClongFormat"
		}

		AssignedVariablesList
		{
			id:                 			variables
			name:               			"variables"
			title:              			qsTr("Measurements")
			allowedColumns:     			["scale"]
			visible:						pcDataFormat.currentValue == "CCwideFormat"
		}

		AssignedVariablesList
		{
			id:                 			subgroups
			name:               			"subgroups"
			title:             			 	qsTr("Subgroups")
			singleVariable:    	 			true
			allowedColumns:     			["nominal", "nominalText", "ordinal"]
			visible:						pcDataFormat.currentValue == "CCwideFormat"
		}
	}

	DoubleField
	{
		id:						pcSubgroupSize
		name: 					"CCSubgroupSize"
		label: 					qsTr("Subgroup size:")
		negativeValues:			false
		min: 					2
		defaultValue:			5
		visible:				pcDataFormat.currentValue == "CClongFormat"
	}

	Group
	{
		title: 									qsTr("Control Charts")
		columns: 								1

		CheckBox
		{
			name: 								"Xbarchart"
			label: 								qsTr("X-bar & R")

			CheckBox
			{
				name: 								"Wlimits"
				label: 								qsTr("Warning limits")
			}
      	CheckBox
			{	name: 								"Phase2_XR"
				label: 								qsTr("Known parameters:")

			  DoubleField
			  {
				  name:			"mean_XR"
				  label:			qsTr("Mean:")
				  defaultValue:	0
          negativeValues: true
			  }

			  DoubleField
			  {
			  	 name:			"SD_XR"
			  	 label:			qsTr("Standard deviation:")
				  defaultValue:	0
			  }
		  }

		}

		CheckBox
		{
			name: 								"Schart"
			label: 								qsTr("X-bar & s")

			CheckBox
			{
				name: 								"Wlimits2"
				label: 								qsTr("Warning limits")
			}
      	CheckBox
			{	name: 								"Phase2_S"
				label: 								qsTr("Known parameters:")

			  DoubleField
			  {
				  name:			"mean_S"
				  label:			qsTr("Mean:")
          negativeValues: true
			  }

			  DoubleField
			  {
			  	 name:			"SD_S"
			  	 label:			qsTr("Standard deviation:")
			  }
		  }

		}
	}

	Section
	{
		title: qsTr("Variable Charts for Subgroups Report")


		TextField
		{
			id:						ccTitle
			label: 					qsTr("Title:")
			name: 					"ccTitle"
			placeholderText:		qsTr("Measurement")
			fieldWidth:				100
		}

		TextField
		{
			id:						ccName
			label: 					qsTr("Name:")
			name: 					"ccName"
			placeholderText:		qsTr("Name")
			fieldWidth:				100
		}

		TextField
		{
			id:						ccDate
			label: 					qsTr("Date:")
			name: 					"ccDate"
			placeholderText:		qsTr("Date")
			fieldWidth:				100
		}

		TextField
		{
			id:						ccReportedBy
			label: 					qsTr("Reported by:")
			name: 					"ccReportedBy"
			placeholderText:		qsTr("Name")
			fieldWidth:				100
		}

		TextField
		{
			id:						ccMisc
			label: 					qsTr("Misc:")
			name: 					"ccMisc"
			placeholderText:		qsTr("Miscellaneous")
			fieldWidth:				100
		}

		CheckBox
		{
			name: "CCReport";		label: qsTr("Show Report")
		}
	}
}
