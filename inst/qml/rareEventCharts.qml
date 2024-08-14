import QtQuick 									2.8
import QtQuick.Layouts 							1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form
{
	columns:									2

	VariablesForm
	{
		id:										variablesForm
		AvailableVariablesList
		{
			name:								"variablesForm"
		}

		AssignedVariablesList
		{
			name:								"variable"
			title:								qsTr("Variable")
			id:									variable
			allowedColumns:						["nominal"]
			singleVariable:						true
		}

		AssignedVariablesList
		{
			name:								"stage"
			title:								qsTr("Stage")
			id:									stages
			allowedColumns:						["nominal"]
			singleVariable:						true
		}
	}

	RadioButtonGroup
	{
		name:								"dataType"
		title: 								qsTr("Data type")
		id:									dataType

		RadioButton
		{
			value: 							"dataTypeDates"
			label: 							qsTr("Date/time")
			checked:		 				true

			TextField
			{
				name:						"dataTypeDatesFormat"
				label: 						qsTr("Format")
				defaultValue:				"dd/mm/yy hh:mm"
				fieldWidth:					100
			}
		}

		RadioButton
		{
			value: 							"dataTypeInterval"
			label: 							qsTr("Interval between events")
			childrenOnSameRow:				false

			DropDown
			{
				name: 					"dataTypeIntervalType"
				id: 					dataTypeIntervalType
				label: 					qsTr("Interval type")
				values: 
				[
					{ label: qsTr("Opportunities"),								value: "dataTypeIntervalTypeOpportunities"},
					{ label: qsTr("Time"),										value: "dataTypeIntervalTypeTime"},
					{ label: qsTr("Hours (decimal)"),							value: "dataTypeIntervalTypeHours"},
					{ label: qsTr("Days (decimal)"),							value: "dataTypeIntervalTypeDays"}				
				]
				indexDefaultValue: 0
			}

			TextField
			{
				name:						"dataTypeIntervalTimeFormat"
				label:						qsTr("Format")
				defaultValue:				"hh:mm"
				visible:					dataTypeIntervalType.value == "dataTypeIntervalTypeTime"
				fieldWidth:					50
			}
		}

	}

	Group
	{
		CheckBox
		{
			name: 								"gChart"
			label: 								qsTr("G chart")
			checked:							true

			DropDown
			{
				name: 					"gChartProportionSource"
				id: 					gChartProportionSource
				label:					qsTr("Proportion")
				values: 
				[
					{ label: qsTr("Estimated from data"),			value: "gChartProportionSourceData"},
					{ label: qsTr("Historical"),					value: "gChartProportionSourceHistorical"}
				]
				indexDefaultValue: 0
			}

			DoubleField
			{
				name: 									"gChartHistoricalProportion"
				id:										gChartHistoricalProportion
				label:									qsTr("Proportion value")
				min: 									0
				max:									1
				visible:								gChartProportionSource.value == "gChartProportionSourceHistorical"
				defaultValue:							0.5
			}
		}

		CheckBox
		{
			name: 								"tChart"
			label: 								qsTr("T chart")
			checked:							true
		
			DropDown
			{
				name: 					"tChartDistribution"
				id: 					tChartDistribution
				label:					qsTr("Based on")
				values: 
				[
					{ label: qsTr("Weibull distribution"),			value: "tChartDistributionWeibull"},
					{ label: qsTr("Exponential distribution"),		value: "tChartDistributionExponential"}
				]
				indexDefaultValue: 0
			}

			DropDown
			{
				name: 					"tChartDistributionParameterSource"
				id: 					tChartDistributionParameterSource
				label:					qsTr("Distribution parameters")
				values: 
				[
					{ label: qsTr("Estimated from data"),			value: "tChartDistributionParameterSourceData"},
					{ label: qsTr("Historical"),					value: "tChartDistributionParameterSourceHistorical"}
				]
				indexDefaultValue: 0
			}

			DoubleField
			{
				name: 									"tChartHistoricalParametersWeibullShape"
				id:										tChartHistoricalParametersWeibullShape
				label:									qsTr("Shape")
				min: 									0
				inclusive:								JASP.None
				visible:								tChartDistributionParameterSource.value == "tChartDistributionParameterSourceHistorical" & tChartDistribution.value == "tChartDistributionWeibull"
				defaultValue:							2
			}

			
			DoubleField
			{
				name: 									"tChartHistoricalParametersScale"
				id:										tChartHistoricalParametersScale
				label:									qsTr("Scale")
				min: 									0
				inclusive:								JASP.None
				visible:								tChartDistributionParameterSource.value == "tChartDistributionParameterSourceHistorical"
				defaultValue:							2
			}
		}
	}
}
