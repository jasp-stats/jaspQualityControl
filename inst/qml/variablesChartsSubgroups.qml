import QtQuick 								    2.8
import QtQuick.Layouts 							1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form
{
	columns:									2

		DropDown
	{
		name: "pcDataFormat"
		label: qsTr("Data format")
		indexDefaultValue: 0
		values:
			[
			{label: qsTr("Long format"),					value: "PClongFormat"},
			{label: qsTr("Wide format"),				value: "PCwideFormat"},
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
    visible:						pcDataFormat.currentValue == "PClongFormat"
  }

  AssignedVariablesList
  {
    id:                 			variables
    name:               			"variables"
    title:              			qsTr("Measurements")
    allowedColumns:     			["scale"]
    visible:						pcDataFormat.currentValue == "PCwideFormat"
  }

  AssignedVariablesList
  {
    id:                 			subgroups
    name:               			"subgroups"
    title:             			 	qsTr("Subgroups")
    singleVariable:    	 			true
    allowedColumns:     			["nominal", "nominalText", "ordinal"]
    visible:						pcDataFormat.currentValue == "PClongFormat"
  }
  AssignedVariablesList
  {
    id:                 			time
    name:               			"time"
    title:             			 	qsTr("Time Stamp (optional)")
    singleVariable:    	 			true
    allowedColumns:     			["nominal", "nominalText", "ordinal", "scale"]
  }
}
	DoubleField
	{
		id:						pcSubgroupSize
		name: 					"pcSubgroupSize"
		label: 					qsTr("Subgroup size:")
		negativeValues:			false
		min: 					5
		defaultValue:			5
		visible:				pcDataFormat.currentValue == "PClongFormat"
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
}
