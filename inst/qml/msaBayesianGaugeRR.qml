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

import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

Form
{
    columns:									1

	DropDown
	{
		name: 									"dataFormat"
		label: 									qsTr("Data format")
		id: 									dataFormat
		indexDefaultValue: 						0
		values: 
		[
			{ label: qsTr("Single column"), value: "longFormat"},
			{ label: qsTr("Across rows"), value: "wideFormat"},
		]
		onValueChanged:
		{
			measurementsWideFormat.itemDoubleClicked(0)
			measurementLongFormat.itemDoubleClicked(0)
		}
	}

	VariablesForm
	{
		id:										variablesFormLongFormat
		visible:								dataFormat.currentValue === "longFormat"

		AvailableVariablesList
		{
			name:								"variablesFormLongFormat"
		}

		AssignedVariablesList
		{
			name:								"measurementLongFormat"
			title:								qsTr("Measurement")
			id:									measurementLongFormat
			singleVariable:						true
			allowedColumns:						["scale"]
		}

		AssignedVariablesList
		{
			name:								"operatorLongFormat"
			title:								qsTr("Operator")
			id:									operatorLongFormat
			singleVariable:						true
			allowedColumns:						["nominal"]
			enabled: 							!type3.checked
		}

		AssignedVariablesList
		{
			name:								"partLongFormat"
			title:								qsTr("Part")
			singleVariable:						true
			allowedColumns:						["nominal"]
		}
	}

	VariablesForm
	{
		id:										variablesFormWideFormat
		visible:								dataFormat.currentValue === "wideFormat"

		AvailableVariablesList
		{
			name:								"variablesFormWideFormat"
		}

		AssignedVariablesList
		{
			name:								"measurementsWideFormat"
			title:								qsTr("Measurements")
			id:									measurementsWideFormat
			singleVariable:						false
			allowedColumns:						["scale"]
		}

		AssignedVariablesList
		{
			name:								"operatorWideFormat"
			title:								qsTr("Operator")
			id:									operatorWideFormat
			singleVariable:						true
			allowedColumns:						["nominal"]
			enabled: 							!type3.checked
		}

		AssignedVariablesList
		{
			name:								"partWideFormat"
			title:								qsTr("Part")
			singleVariable:						true
			allowedColumns:						["nominal"]
		}
	}

	CheckBox
	{
		name:								"type3"
		id:									type3
		label:								qsTr("Type 3 study (automatic equipment)")
		onCheckedChanged:
		{
			operatorLongFormat.itemDoubleClicked(0)
			operatorWideFormat.itemDoubleClicked(0)
		}
	}

	Group
	{
		title: 									qsTr("Analysis options")
		
		DropDown
		{
			name: 							"estimationType"
			label: 							qsTr("Estimation")
			id: 							estimationType
			indexDefaultValue: 				0
			visible: 						!type3.checked
			values: 
			[
				{ label: qsTr("Automatic"), value: "automatic" },
				{ label: qsTr("Manual"), value: "manual" },
			]
		}

		DoubleField
		{
			name: 					"bfFavorFull"
			label: 					qsTr("Cut-off BF in favor of full model")
			id: 					bfFavorFull
			defaultValue: 			1
			min: 					0
			inclusive: 			    JASP.None
			decimals: 				3
			visible: 				!type3.checked && estimationType.currentValue === "automatic"
		}
		
		RadioButtonGroup
		{
			name: 					"modelType"
			visible: 				!type3.checked

			RadioButton
			{
				name: 					"fullModel"
				label: 					qsTr("Full model")
				id: 					fullModel
				checked: 				true
				visible: 				estimationType.currentValue === "manual"
			}

			RadioButton
			{
				name: 							"mainEffectsOnly"
				label: 							qsTr("Main effects only")
				id: 							mainEffectsOnly
				checked: 						false
				visible: 						estimationType.currentValue === "manual"
			}	
		}
				


		DropDown
		{
			name: 							"processVariationReference"
			label: 							qsTr("Process variation based on")
			id: 							variationReference
			indexDefaultValue: 				0
			values: 
			[
				{ label: qsTr("Study variation"), value: "studySd" },
				{ label: qsTr("Historical standard deviation"), value: "historicalSd" }
			]
		}

		DoubleField
		{
			name:							"historicalSdValue"
			label:							qsTr("Historical standard deviation:")
			defaultValue:					3
			min: 							0
			inclusive: 					    JASP.None
			decimals: 						9
			enabled:						variationReference.currentValue === "historicalSd"
		}

		CheckBox
		{
			name: 							"tolerance"
			label: 							qsTr("Tolerance width")
			id:								tolerance
			childrenOnSameRow: 				true

			DoubleField
			{
				name: 						"toleranceValue"
				id:							toleranceValue
				defaultValue: 				10
				min: 						0
				inclusive: 					JASP.None
				decimals: 					9
			}
		}

		CheckBox
		{
			name: 							"RRTable"
			label: 							qsTr("r&R table")
			checked: 						true

			DropDown
			{
				name: 						"studyVarianceMultiplierType"
				label: 						qsTr("Study var. multiplier type")
				id: 						studyVarianceMultiplierType
				indexDefaultValue: 			0
				values: 
				[
					{ label: qsTr("Std. Deviation"), value: "sd" },
					{ label: qsTr("Percent"), value: "percent" }
				]
			}

			DoubleField
			{
				name: 						"studyVarianceMultiplierValue"
				label: 						qsTr("Study var. multiplier value")
				fieldWidth: 				60
				defaultValue: 				6
				min:						0
				max:						100
				inclusive: 					JASP.None
				decimals: 					3
			}
		}

	}

	Section 
	{
		title: qsTr("Plots")
		columns: 2
		
		Group
		{
			CheckBox
			{
				name: 						"priorPlot"
				label: 						qsTr("Prior")
				checked: 					false
			}

			CheckBox
			{
				name: 						"posteriorPlot"
				label: 						qsTr("Posterior")
				checked: 					false

				DropDown
				{
					name:   "posteriorPlotType"
					label:  ""
					id:     posteriorPlotType
					values: tolerance.checked ? [
						{ label: qsTr("Variances"),        value: "var" },
						{ label: qsTr("%Contribution"),    value: "percContrib"},
						{ label: qsTr("%Study variation"), value: "percStudyVar"},
						{ label: qsTr("%Tolerance"),       value: "percTol"}
					] : [
						{ label: qsTr("Variances"),        value: "var" },
						{ label: qsTr("%Contribution"),    value: "percContrib"},
						{ label: qsTr("%Study variation"), value: "percStudyVar"}
					]
				}

				CheckBox
				{
					label:				qsTr("Display histogram")
					name:				"posteriorHistogram"
				}

				CheckBox
				{
					label:				qsTr("Point estimate")
					name:				"posteriorPointEstimate"
					childrenOnSameRow:	true
					checked:			true

					DropDown
					{
						name:	"posteriorPointEstimateType"
						label:	""
						values:	
						[
							{ label: qsTr("mean"),   value: "mean" }, 
							{ label: qsTr("median"), value: "median" },
							{ label: qsTr("mode"),   value: "mode" }
						]
					}
				}

				CheckBox
				{
					name:				"posteriorCi"
					label:				qsTr("CI")
					id:					posteriorCi
					childrenOnSameRow:	true
					checked:			false

					DropDown
					{
						name:		"posteriorCiType"
						label:		""
						values:		
						[
							{ label: qsTr("central"), value: "central" }, 
							{ label: qsTr("HPD"),     value: "HPD" },
							{ label: qsTr("custom"),  value: "custom" }
						]
						id:			posteriorCiType
					}
				}

				Group
				{
					columns:	2

					CIField
					{
						visible:		posteriorCiType.currentText === "central" || posteriorCiType.currentText === "HPD"
						enabled:		posteriorCi.checked
						name:			"posteriorCiMass"
						label:			qsTr("Mass")
						fieldWidth:		50
						defaultValue:	95
						min:			1
						max:			100
						inclusive:		JASP.MinOnly
					}

					DoubleField
					{
						visible:		posteriorCiType.currentText == "custom"
						enabled:		posteriorCi.checked
						name:			"posteriorCiLower"
						label:			qsTr("Lower")
						id:				posteriorCiLower
						fieldWidth:		50
						defaultValue:	0.25
						min:			0
						max:			posteriorCiUpper.value
						inclusive:		JASP.None
					}

					DoubleField
					{
						visible:		posteriorCiType.currentText === "custom"
						enabled:		posteriorCi.checked
						name:			"posteriorCiUpper"
						label:			qsTr("Upper")
						id:				posteriorCiUpper
						fieldWidth:		50
						defaultValue:	0.75
						min:			posteriorCiLower.value
						max:			1
						inclusive:		JASP.None
					}
				}
			}

			CheckBox
				{
					name: "contourPlot"
					label: qsTr("Contour plot")

					DoubleField
					{
						name: "contourLSL"
						label: qsTr("Lower specification limit")
						id: contourLSL
						fieldWidth: 60
						negativeValues: true
						defaultValue: -1
						max: contourUSL.value
						inclusive: JASP.None
					}

					DoubleField
					{
						name: "contourUSL"
						label: qsTr("Upper specification limit")
						id: contourUSL
						fieldWidth: 60
						defaultValue: 1
						negativeValues: true
						min: contourLSL.value
						inclusive: JASP.None
					}
				}
		}

		Group
		{
			CheckBox
			{
				name: 						"varianceComponentsGraph"
				label: 						qsTr("Components of variation")
				checked: 					true
			}

			CheckBox
			{
				name: 							"rChart"
				label: 							qsTr("Range charts by operator")
				enabled:						!type3.checked
			}

			CheckBox
			{
				name:							"xBarChart"
				label:							qsTr("Average chart by operator")
				enabled:						!type3.checked
			}

			CheckBox
			{
				name: 							"scatterPlot"
				label:							qsTr("Scatter plots operators")
				enabled:						!type3.checked

				CheckBox
				{
					name:						"scatterPlotFitLine"
					label:						qsTr("Fit line")
				}

				CheckBox
				{
					name:						"scatterPlotOriginLine"
					label:						qsTr("Show origin line")
				}
			}

			CheckBox
			{
				name:							"partMeasurementPlot"
				label:							qsTr("Measurements by part plot")

				CheckBox
				{
					name:						"partMeasurementPlotAllValues"
					label:						qsTr("Display all measurements")
				}
			}

			CheckBox
			{
				name:							"operatorMeasurementPlot"
				label:							qsTr("Measurements by operator plot")
				enabled:						!type3.checked
			}

			CheckBox
			{
				name: 							"partByOperatorMeasurementPlot"
				label: 							qsTr("Part × operator interaction plot")
				enabled:						!type3.checked
			}

			CheckBox
			{
				name: 							"trafficLightChart"
				label: 							qsTr("Traffic light chart")
			}
		}
	}
	
	Section
	{
		title: 		qsTr("MCMC diagnostics")
		columns: 	1

		CheckBox
		{
			label: 	  	qsTr("Diagnostics table")
			name: 	  	"diagnosticsTable"
			checked:	true
		}

		CheckBox
		{
			label: 	  			qsTr("Plots")
			name: 	  			"diagnosticsPlots"
			checked:			false
			childrenOnSameRow: 	true

			DropDown
			{
				name: "diagnosticsPlotType"
				values:
				[
					{ label: qsTr("Traceplot"), 		value: "trace" },
					{ label: qsTr("Autocorrelation"), 	value: "autocor" },
					{ label: qsTr("Density"),			value: "density"}
				]
				indexDefaultValue: 0
			}
		}
		
	}

	Section
	{
		title: qsTr("Advanced options")

		Group 
		{
			title: qsTr("Priors")

			DoubleField
			{
				name: "rscalePrior"
				label: qsTr("r scale prior")
				defaultValue: 1
				min: 0
				max: 10
				inclusive: JASP.MaxOnly
				decimals: 3
			}
		}

		Group
		{
			SetSeed{}
		}

		Group
		{
			title: qsTr("MCMC options")

			IntegerField
			{
				name: "mcmcChains"
				label: qsTr("Chains")
				defaultValue: 4
				min: 1
				max: 10
			}

			IntegerField
			{
				name: "mcmcIterations"
				label: qsTr("Iterations per chain")
				id: mcmcIterations
				defaultValue: 10000
				min: Math.max(mcmcBurnin.value * 2, 100)
				max: 100000
				fieldWidth: 60
			}

			IntegerField
			{
				name: "mcmcBurnin"
				label: qsTr("Burn-in per chain")
				defaultValue: 2000
				id: mcmcBurnin
				min: 1
				max: mcmcIterations.value / 2
				fieldWidth: 60
			}			
		}

		Group
		{
			title: qsTr("Distribution fit to MCMC samples")

			DropDown
			{
				name: "distType"
				label: qsTr("Distribution")
				values: posteriorPlotType.currentValue === "var" ? [
					{ label: qsTr("Generalized inverse Gaussian"), value: "gig" },
					{ label: qsTr("Metalog"), value: "metalog" }
				] : [	
					{ label: qsTr("Metalog"), value: "metalog" }
				]
				indexDefaultValue: 0
			}	
		}
	}

	Section
	{
		title:	qsTr("Report options")
		
		CheckBox
		{
			name: "report"
			label: qsTr("Show Report")
			id:		anovaGaugeReport
			columns: 1
				
			CheckBox
			{
				name:		"reportMetaData"
				label:		qsTr("Show report metadata")
				checked:	true
				columns: 2

				CheckBox
				{
					name:					"reportTitle"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportTitleText"
						label: 								qsTr("Title")
						id:									reportTitleText
						placeholderText:					qsTr("Gauge r&R Report")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:					"reportPartName"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportPartNameText"
						label: 								qsTr("Part name")
						id:									reportPartNameText
						placeholderText:					qsTr("Name")
						fieldWidth:							100
					}
				}


				CheckBox
				{
					name:					"reportGaugeName"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportGaugeNameText"
						label: 								qsTr("Gauge name")
						id:									reportGaugeNameText
						placeholderText:					qsTr("Name")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:					"reportCharacteristic"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportCharacteristicText"
						label: 								qsTr("Characteristic")
						id:									reportCharacteristicText
						placeholderText:					qsTr("Characteristic")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:					"reportGaugeNumber"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportGaugeNumberText"
						label: 								qsTr("Gauge number")
						id:									reportGaugeNumberText
						placeholderText:					qsTr("Number")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:					"reportTolerance"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportToleranceText"
						label: 								qsTr("Tolerance")
						id:									reportToleranceText
						placeholderText:					qsTr("Tolerance")
						fieldWidth:							100
					}
				}
				
				CheckBox
				{
					name:					"reportLocation"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportLocationText"
						label: 								qsTr("Location")
						id:									reportLocationText
						placeholderText:					qsTr("Location")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:					"reportPerformedBy"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportPerformedByText"
						label: 								qsTr("Performed by")
						id:									reportPerformedByText
						placeholderText:					qsTr("Analyst")
						fieldWidth:							100
					}
				}

				CheckBox
				{
					name:					"reportDate"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 								"reportDateText"
						label: 								qsTr("Date")
						id:									reportDate
						placeholderText:					qsTr("Date")
						fieldWidth:							100
					}
				}
			}
		
			Group
			{
				title:			qsTr("Select Report Components")

				CheckBox
				{
					name:		"reportGaugeTable"
					label:		qsTr("Show gauge evaluation table")
					checked:	true
				}
			
				CheckBox
				{
					name:		"reportVariationComponents"
					label:		qsTr("Show components of variation")
					checked:	true
				}
				
				CheckBox
				{
					name:		"reportMeasurementsByPartPlot"
					label:		qsTr("Show measurements by part")
					checked:	true
				}
			
				CheckBox
				{
					name:		"reportRChartByOperator"
					label:		qsTr("Show range charts by operator")
					checked:	!type3.checked
					enabled: 	!type3.checked
				}
				
				CheckBox
				{
					name:		"reportMeasurementsByOperatorPlot"
					label:		qsTr("Show measurements by operator")
					checked:	!type3.checked
					enabled: 	!type3.checked
				}
				
				CheckBox
				{
					name:		"reportAverageChartByOperator"
					label:		qsTr("Show average charts by operator")
					checked:	!type3.checked
					enabled: 	!type3.checked
				}
				
				CheckBox
				{
					name:		"reportPartByOperatorPlot"
					label:		qsTr("Show part × operator interaction")
					checked:	!type3.checked
					enabled: 	!type3.checked
				}
				
				CheckBox
				{
					name:		"reportTrafficLightChart"
					label:		qsTr("Show traffic light chart")
					checked:	true
				}
			}
		}
	}
}