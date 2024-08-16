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

			DropDown
			{
				name: 					"dataTypeDatesStructure"
				id: 					dataTypeDatesStructure
				label: 					qsTr("Structure")
				values: 
				[
					{ label: qsTr("Date + Time"),						value: "dateTime"},
					{ label: qsTr("Time + Date"),						value: "timeDate"},
					{ label: qsTr("Date only"),							value: "dateOnly"},
					{ label: qsTr("Time only"),							value: "timeOnly"}				
				]
				indexDefaultValue: 0
			}

			DropDown
			{
				name: 					"dataTypeDatesFormatDate"
				id: 					dataTypeDatesFormatDate
				label: 					qsTr("Date format")
				visible:				dataTypeDatesStructure.value != "timeOnly"
				values: 
				[
					{ label: qsTr("DMY"),							value: "dmy"},
					{ label: qsTr("MDY"),							value: "mdy"},
					{ label: qsTr("YMD"),							value: "ymd"},
					{ label: qsTr("DM"),							value: "dm"},
					{ label: qsTr("MD"),							value: "md"}
				]
				indexDefaultValue: 0
			}

			DropDown
			{
				name: 					"dataTypeDatesFormatTime"
				id: 					dataTypeDatesFormatTime
				label: 					qsTr("Time format")
				visible:				dataTypeDatesStructure.value != "dateOnly"
				values: 
				[
					{ label: qsTr("H"),								value: "H"},
					{ label: qsTr("HM"),							value: "HM"},
					{ label: qsTr("HMS"),							value: "HMS"},
					{ label: qsTr("Ip"),							value: "Ip"},
					{ label: qsTr("IMp"),							value: "IMp"}				
				]
				indexDefaultValue: 1
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

			DropDown
			{
				name: 					"dataTypeIntervalTimeFormat"
				id: 					dataTypeIntervalTimeFormat
				label: 					qsTr("Time format")
				visible:				dataTypeIntervalType.value == "dataTypeIntervalTypeTime"
				values: 
				[
					{ label: qsTr("H"),								value: "H"},
					{ label: qsTr("HM"),							value: "HM"},
					{ label: qsTr("HMS"),							value: "HMS"},
					{ label: qsTr("Ip"),							value: "Ip"},
					{ label: qsTr("IMp"),							value: "IMp"}				
				]
				indexDefaultValue: 1
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
