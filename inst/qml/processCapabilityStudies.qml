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

import "./common" as Common

Form
{
	columns:								2

	info:									qsTr("A process capability study analyses a process to determine whether it is capable of producing good quality products, using data from an initial run of parts to predict whether the manufacturing process can repeatably produce parts that meet specifications. A subgroup is a group of parts produced under the same set of conditions.")

	infoBottom: 							"## " + qsTr("Output") + "\n"
		+ "- " + qsTr("Control charts (X-bar & R, X-bar & s, X-mR, X-bar & mR) showing process average and dispersion over time, used to assess stability.") + "\n"
		+ "- " + qsTr("Histogram of the values with an optional fitted distribution.") + "\n"
		+ "- " + qsTr("Probability table and plot: number of observations, mean, standard deviation, Anderson-Darling statistic and p-value, plotting the data against a theoretical distribution.") + "\n"
		+ "- " + qsTr("Capability tables: process summary, capability of the process plot, process capability (within: Cp, CpL, CpU, Cpk), process performance (overall: Pp, PpL, PpU, Ppk, Cpm), and non-conformance (observed and expected ppm).") + "\n"
		+ "\n" + qsTr("Note: capability reports are only meaningful when the process is in a state of statistical control.") + "\n"
		+ "\n---\n## " + qsTr("References") + "\n"
		+ "- " + qsTr("Automotive Industry Action Group (2005). Statistical process control (SPC) – Reference manual (2nd ed.). AIAG.") + "\n"
		+ "- " + qsTr("Yeo, I. K., & Johnson, R. A. (2000). A new family of power transformations to improve normality or symmetry. Biometrika, 87(4), 954-959.") + "\n"
		+ "- " + qsTr("Chou, Y. M., Polansky, A. M., & Mason, R. L. (1998). Transforming non-normal data to normality in statistical process control. Journal of Quality Technology, 30(2), 133-141.") + "\n"
		+ "\n---\n## " + qsTr("R Packages") + "\n"
		+ "- ggplot2\n- qcc\n- jaspGraphs\n- ggrepel\n- FAdist\n- goftest\n- fitdistrplus\n- tidyr\n- tibble\n- EnvStats\n- weibullness\n"

	DropDown
	{
		name: "dataFormat"
		label: qsTr("Data format")
		id: dataFormat
		indexDefaultValue: 0
		info:	qsTr("Layout of the data: all observations in one column (\"Single column\") or one subgroup per row (\"Across rows\").")
		values:
		[
			{label: qsTr("Single column"),				value: "longFormat"},
			{label: qsTr("Across rows"),				value: "wideFormat"},
		]
		onValueChanged:
		{
			measurementLongFormat.itemDoubleClicked(0)
			measurementsWideFormat.itemDoubleClicked(0)
		}
	}

	VariablesForm
	{
		id:									variablesFormLongFormat
		visible:							dataFormat.currentValue == "longFormat"

		AvailableVariablesList
		{
			name:							"variablesFormLongFormat"
		}

		AssignedVariablesList
		{
			name:							"measurementLongFormat"
			title:							qsTr("Measurement")
			id:								measurementLongFormat
			allowedColumns:					["scale"]
			singleVariable:					true
			info:							qsTr("The observations/data collected from a process parameter.")
		}

		AssignedVariablesList
		{
			name:							"subgroup"
			title:						 	qsTr("Subgroup")
			id:					 			subgroup
			singleVariable:		 			true
			allowedColumns:					["nominal"]
			enabled: 						subgroupSizeType.value == "groupingVariable"
			info:							qsTr("The subgroup each observation is assigned to, when all observations are in a single column.")
		}

		AssignedVariablesList
		{
			id:									stagesLongFormat
			name:								"stagesLongFormat"
			title:								qsTr("Stages")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("A column that splits the analysis into multiple stages by assigning a stage to each subgroup.")
		}
	}

	VariablesForm
	{
		id:									variablesFormWideFormat
		visible:							dataFormat.currentValue == "wideFormat"

		AvailableVariablesList
		{
			name:							"variablesFormWideFormat"
		}

		AssignedVariablesList
		{
			name:							"measurementsWideFormat"
			title:							qsTr("Measurements")
			id:								measurementsWideFormat
			allowedColumns:					["scale"]
			info:							qsTr("The measurement columns, with one subgroup per row.")
		}

		AssignedVariablesList
		{
			id:									axisLabels
			name:								"axisLabels"
			title:								qsTr("Timestamp (optional)")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("Optional subgroup names for each row, used as x-axis labels.")
		}

		AssignedVariablesList
		{
			id:									stagesWideFormat
			name:								"stagesWideFormat"
			title:								qsTr("Stages")
			singleVariable:						true
			allowedColumns:						["nominal"]
			info:								qsTr("A column that splits the analysis into multiple stages by assigning a stage to each subgroup row.")
		}
	}

	Group
	{
		columns:							2

		RadioButtonGroup
		{
			name:								"subgroupSizeType"
			title: 								qsTr("Specify subgroups")
			id:									subgroupSizeType
			visible:							dataFormat.currentValue == "longFormat"
			info:								qsTr("How subgroups are formed from a single column of observations.")

			RadioButton
			{
				value: 							"manual"
				label: 							qsTr("Subgroup size")
				checked:		 				true
				childrenOnSameRow:				true
				info:							qsTr("Assign observations in order of appearance to subgroups of the specified size. If the count is not divisible, the last subgroup holds the remaining observations.")

				DoubleField
				{
					name: 									"manualSubgroupSizeValue"
					id:										manualSubgroupSizeValue
					min: 									1
					max:									dataSetInfo.rowCount
					negativeValues:							false
					defaultValue:							(dataSetInfo.rowCount < 5)? dataSetInfo.rowCount : 5
				}
			}

			RadioButton
			{
				value: 							"groupingVariable"
				label: 							qsTr("Through grouping variable")
				info:							qsTr("Use a single-column subgroup variable that assigns each observation to a subgroup.")

				DropDown
				{
					name: 					"groupingVariableMethod"
					id: 					groupingVariable
					label: 					"Grouping method"
					info:					qsTr("How to group when identical subgroup values are not adjacent. \"Subgroup value change\" groups only adjacent identical values; \"Same subgroup value\" groups all identical values regardless of adjacency.")
					values:
					[
						{ label: qsTr("Subgroup value change"),			value: "newLabel"},
						{ label: qsTr("Same subgroup value"),			value: "sameLabel"}
					]
					indexDefaultValue: 0
				}
			}
		}

		RadioButtonGroup
		{
			name:								"subgroupSizeUnequal"
			title: 								qsTr("Unequal subgroup sizes")
			id:									subgroupSizeUnequal
			info:								qsTr("How to handle subgroups of differing sizes when computing the process variance and control limits.")

			RadioButton
			{
				value: 								"actualSizes"
				label: 								qsTr("Use actual sizes")
				checked: 							true
				info:								qsTr("Compute control limits per subgroup using the actual subgroup sizes.")
			}

			RadioButton
			{
				value: 								"fixedSubgroupSize"
				label: 								qsTr("Use fixed subgroup size")
				childrenOnSameRow:		 			true
				info:								qsTr("Assume a single fixed subgroup size, producing the same control limits for all groups.")

				IntegerField
				{
					name: 								"fixedSubgroupSizeValue"
					fieldWidth: 						30
					defaultValue: 						5
					min:								2
				}
			}
		}
	}

	Section
	{
		title: qsTr("Process capability options")

		ColumnLayout
		{
			Group
			{
				title: qsTr("Transform data")
				info:	qsTr("Transform the data before analysis to improve normality.")
				DropDown
				{
					name:		"dataTransformation"
					id:			dataTransformation
					label:		qsTr("Type")
					info:		qsTr("Transformation applied to the data. Box-Cox and Yeo-Johnson can estimate lambda automatically; Yeo-Johnson (auto) and Johnson allow only process performance results (no process capability). Johnson is fully automatic.")
					values:
					[
						{label: qsTr("None"),					value: "none"},
						{label: qsTr("Box-Cox"),				value: "boxCox"},
						{label: qsTr("Box-Cox (auto)"),			value: "boxCoxAuto"},
						{label: qsTr("Yeo-Johnson"),			value: "yeoJohnson"},
						{label: qsTr("Yeo-Johnson (auto)"),		value: "yeoJohnsonAuto"},
						{label: qsTr("Johnson"),				value: "johnson"},
					]
				}

				DoubleField
				{
					label: qsTr("Shift")
					name: "dataTransformationShift"
					negativeValues:	true
					defaultValue: 0
					enabled: ["boxCox", "boxCoxAuto"].includes(dataTransformation.value)
					info:	qsTr("Value of the shift parameter for transforms accepting bounded data. Disabled for unbounded transforms (Yeo-Johnson, Johnson).")
				}
				DoubleField
				{
					label: qsTr("Lambda")
					name: "dataTransformationLambda"
					negativeValues:	true
					defaultValue: 0
					enabled: ["boxCox", "yeoJohnson"].includes(dataTransformation.value)
					info:	qsTr("Value of the lambda parameter of the transform. Disabled for transforms that estimate their parameter automatically.")
				}
				DropDown
				{
					name:		"dataTransformationMethod"
					label:		qsTr("Type")
					info:		qsTr("Method for selecting the best lambda. Log-Lik maximises the normal likelihood; SD minimises the sum of squares; Average moving range minimises variability based on the moving range (appropriate for individuals data).")
					values:
					[
						{label: qsTr("Log. Lik"),					value: "loglik"},
						{label: qsTr("SD"),							value: "sd"},
						{label: qsTr("Average moving range"),		value: "movingRange"},
					]
					enabled: ["boxCoxAuto"].includes(dataTransformation.value)
				}
				CheckBox
				{
					label: qsTr("Continuity Adjustment")
					name: "dataTransformationContinuityAdjustment"
					checked: false
					enabled: ["boxCox", "boxCoxAuto"].includes(dataTransformation.value)
					info:	qsTr("Include the continuity adjustment term in the Box-Cox transform.")
				}
			}
			Group
			{
				title:					qsTr("Type of data distribution")
				info:					qsTr("Whether the data approximate a normal distribution or another distribution.")


				RadioButtonGroup
				{
					name: 					"capabilityStudyType"
					id: 					capabilityStudyType

					RadioButton
					{
						name: 				"normalCapabilityAnalysis"
						id : 				normalCapabilityAnalysis
						label: 				qsTr("Normal distribution")
						checked: 			true
						info:				qsTr("Assume the data are approximately normally distributed.")


						CheckBox
						{
							name: 						"historicalMean"
							label: 						qsTr("Historical mean")
							id:							historicalMean
							childrenOnSameRow:			true
							info:						qsTr("Use a fixed historical mean instead of estimating it from the data.")

							DoubleField
							{
								name: 					"historicalMeanValue"
								id:						historicalMeanValue
								negativeValues:			true
								defaultValue:			0
								decimals:				9
							}
						}

						CheckBox
						{
							name: 						"historicalStdDev"
							label: 						qsTr("Historical std. dev.")
							id:							historicalStdDev
							childrenOnSameRow:			true
							info:						qsTr("Use a fixed historical standard deviation instead of estimating it from the data.")

							DoubleField
							{
								name: 					"historicalStdDevValue"
								id:						historicalStdDevValue
								negativeValues:			false
								min:					0.000001
								defaultValue:			1
								decimals:				9
							}
						}
					}

					RadioButton
					{
						name:				"nonNormalCapabilityAnalysis"
						id :				nonNormalCapabilityAnalysis
						label:				qsTr("Non-normal distribution")
						info:				qsTr("Assume the data follow a specified non-normal distribution.")

						DropDown
						{
							name: 					"nonNormalDistribution"
							id: 					nonNormalDistribution
							label:					qsTr("Specify a distribution")
							info:					qsTr("The non-normal distribution used to model the data.")
							values:
							[
								{label: qsTr("Weibull"),					value: "weibull"},
								{label: qsTr("Log-normal"),					value: "lognormal"},
								{label: qsTr("Gamma"),						value: "gamma"},
								{label: qsTr("Exponential"),				value: "exponential"},
								{label: qsTr("Logistic"),					value: "logistic"},
								{label: qsTr("Log-logistic"),				value: "loglogistic"},
								{label: qsTr("3-parameter Weibull"),		value: "3ParameterWeibull"},
								{label: qsTr("3-parameter log-normal"),		value: "3ParameterLognormal"}
							]
							indexDefaultValue: 0
						}

						CheckBox
						{
							name: 						"historicalShape"
							label: 						qsTr("Historical shape")
							id:							historicalShape
							childrenOnSameRow:			true
							visible:					nonNormalDistribution.currentValue == "weibull" || nonNormalDistribution.currentValue == "3ParameterWeibull" || nonNormalDistribution.currentValue == "gamma"

							DoubleField
							{
								name: 					"historicalShapeValue"
								id:						historicalShapeValue
								negativeValues:			false
								min:					0.000001
								defaultValue:			1
								decimals:				9
							}
						}

						CheckBox
						{
							name: 						"historicalLocation"
							label: 						qsTr("Historical location")
							id:							historicalLocation
							childrenOnSameRow:			true
							visible:					nonNormalDistribution.currentValue == "logistic" || nonNormalDistribution.currentValue == "loglogistic"

							DoubleField
							{
								name: 					"historicalLocationValue"
								id:						historicalLocationValue
								negativeValues:			true
								defaultValue:			1
								decimals:				9
							}
						}

						CheckBox
						{
							name: 						"historicalScale"
							label: 						qsTr("Historical scale")
							id:							historicalScale
							childrenOnSameRow:			true
							visible:					nonNormalDistribution.currentValue == "weibull" || nonNormalDistribution.currentValue == "3ParameterWeibull" || nonNormalDistribution.currentValue == "gamma" ||  nonNormalDistribution.currentValue == "exponential" || nonNormalDistribution.currentValue == "logistic" || nonNormalDistribution.currentValue == "loglogistic"

							DoubleField
							{
								name: 					"historicalScaleValue"
								id:						historicalScaleValue
								negativeValues:			false
								min:					0.000001
								defaultValue:			1
								decimals:				9
							}
						}

						CheckBox
						{
							name: 						"historicalLogMean"
							label: 						qsTr("Historical log mean")
							id:							historicalLogMean
							childrenOnSameRow:			true
							visible:					nonNormalDistribution.currentValue == "lognormal" || nonNormalDistribution.currentValue == "3ParameterLognormal"

							DoubleField
							{
								name: 					"historicalLogMeanValue"
								id:						historicalLogMeanValue
								negativeValues:			true
								defaultValue:			1
								decimals:				9
							}
						}

						CheckBox
						{
							name: 						"historicalLogStdDev"
							label: 						qsTr("Historical log std. dev.")
							id:							historicalLogStdDev
							childrenOnSameRow:			true
							visible:					nonNormalDistribution.currentValue == "lognormal" || nonNormalDistribution.currentValue == "3ParameterLognormal"

							DoubleField
							{
								name: 					"historicalLogStdDevValue"
								id:						historicalLogStdDevValue
								negativeValues:			false
								min:					0.000001
								defaultValue:			1
								decimals:				9
							}
						}

						CheckBox
						{
							name: 						"historicalThreshold"
							label: 						qsTr("Historical threshold")
							id:							historicalThreshold
							childrenOnSameRow:			true
							visible:					nonNormalDistribution.currentValue == "3ParameterLognormal" || nonNormalDistribution.currentValue == "3ParameterWeibull"

							DoubleField
							{
								name: 					"historicalThresholdValue"
								id:						historicalThresholdValue
								negativeValues:			true
								defaultValue:			1
								decimals:				9
							}
						}

						DropDown
						{
							name:					"nonNormalMethod"
							label:					qsTr("Non-normal capability statistics")
							indexDefaultValue:		0
							info:					qsTr("Method used to calculate the capability statistics for non-normally distributed data. Historical parameter fields below let you fix selected distribution parameters instead of estimating them.")
							values:
								[
								{label: qsTr("Percentile"),				value: "percentile"},
								{label: qsTr("Non-conformance"),		value: "nonConformance"  }
							]
						}
					}
				}


			Group
			{
				title: 							qsTr("Capability study")

				CheckBox
				{
					name: 						"lowerSpecificationLimit"
					label: 						qsTr("Lower specification limit")
					id:							lowerSpecificationLimit
					childrenOnSameRow:			true
					info:						qsTr("The value used as the lower tolerance limit.")

					DoubleField
					{
						name: 					"lowerSpecificationLimitValue"
						id:						lowerSpecificationLimitValue
						negativeValues:			true
						defaultValue:			-1
						decimals:				9
					}

					CheckBox
					{
					name: 						"lowerSpecificationLimitBoundary"
					label: 						qsTr("Boundary")
					id:							lowerSpecificationLimitBoundary
					info:						qsTr("Whether the lower specification limit is a physical boundary that cannot be exceeded.")
					}
				}

				CheckBox
				{
					name: 						"target"
					label: 						qsTr("Target value")
					id:							target
					childrenOnSameRow:			true
					info:						qsTr("The value used as the target.")

					DoubleField
					{
						name: 					"targetValue"
						id:						targetValue
						negativeValues:			true
						defaultValue:			0
						decimals:				9
					}
				}

				CheckBox
				{
					name: 						"upperSpecificationLimit"
					label: 						qsTr("Upper specification limit")
					id:							upperSpecificationLimit
					childrenOnSameRow:			true
					info:						qsTr("The value used as the upper tolerance limit.")

					DoubleField
					{
						name: 					"upperSpecificationLimitValue"
						id:						upperSpecificationLimitValue
						negativeValues:			true
						defaultValue:			1
						decimals:				9
					}

					CheckBox
					{
					name: 						"upperSpecificationLimitBoundary"
					label: 						qsTr("Boundary")
					id:							upperSpecificationLimitBoundary
					info:						qsTr("Whether the upper specification limit is a physical boundary that cannot be exceeded.")
					}
				}

				CheckBox
				{
					name: 						"processCapabilityPlot"
					label: 						qsTr("Process capability plot")
					checked: 					true
					enabled:					upperSpecificationLimit.checked || lowerSpecificationLimit.checked
					info:						qsTr("Plot the frequency distribution with the fitted distribution based on the overall and within process variation, compared to the specification limits.")

					DoubleField
					{
						name:						"processCapabilityPlotBinNumber"
						label:						qsTr("Number of bins")
						defaultValue:				10
						min:						3;
						max:						10000;
						enabled:					csBinWidthType.currentValue === "manual"
						info:						qsTr("Number of classes (bins) plotted in the process capability plot.")
					}

					CheckBox
					{
						name: 								"processCapabilityPlotDistributions"
						label: 								qsTr("Overlay distribution")
						checked: 							true
						info:								qsTr("Overlay the fitted distribution curves (within and overall) on the plot.")
					}

					CheckBox
					{
						name: 								"processCapabilityPlotSpecificationLimits"
						label: 								qsTr("Display specification limits")
						checked: 							true
						info:								qsTr("Display the specification limits as vertical lines on the plot.")
					}

					RadioButtonGroup
					{
						name:	"processCapabilityPlotXAxisModification"
						title:	qsTr("Axis modifications")
						info:	qsTr("Optional modifications to the x-axis labelling of the plot.")

						RadioButton { value: "none";     label: qsTr("None");              checked: true }
						RadioButton { value: "thin";     label: qsTr("Thin x-axis labels") }
						RadioButton { value: "compress"; label: qsTr("Compress x-axis")    }
					}
				}

				CheckBox
				{
					name: 							"processCapabilityTable"
					label: 							qsTr("Process capability tables")
					checked: 						true
					enabled:						upperSpecificationLimit.checked || lowerSpecificationLimit.checked
					info:							qsTr("Display the process capability and performance tables (Cp, Cpk, Pp, Ppk, Cpm, and non-conformance statistics).")

					CheckBox
					{
						name: "processCapabilityTableCi";
						label: qsTr("Confidence intervals")
						checked: capabilityStudyType.value == "normalCapabilityAnalysis"
						enabled: capabilityStudyType.value == "normalCapabilityAnalysis"
						childrenOnSameRow: true
						info:	qsTr("Display confidence intervals for the capability statistics (normal distribution only); set the confidence level alongside.")

						CIField
						{
							name: "processCapabilityTableCiLevel"
							defaultValue: 90}
						}

					CheckBox
					{
						name: "processCapabilityTableZ"
						label: qsTr("Z (ST) / Z (LT)")
						info:	qsTr("Display the short-term Z (ST) and long-term Z (LT) sigma-level statistics.")
					}

				}

				}
			}
		}

		ColumnLayout
		{

			Group
			{
				title:			qsTr("Stability of the process")
				info:			qsTr("Control charts used to assess whether the process is in a state of statistical control.")

				CheckBox
				{
					name: 						"controlChart"
					label: 						""
					checked: 					true
					childrenOnSameRow:			true
					columns:					1
					info:						qsTr("Display a control chart of the process to assess its stability; select the chart type below.")


					DropDown
					{
						name: 					"controlChartType"
						id: 					controlChartType
						label: 					""
						values: dataFormat.currentValue == "longFormat" && subgroupSizeType.value == "manual" &&manualSubgroupSizeValue.value == 1 ?
						[
							{ label: qsTr("X-bar & R control chart"),			value: "xBarR"},
							{ label: qsTr("X-bar & s control chart"),			value: "xBarS"},
							{ label: qsTr("X-mR control chart"),				value: "xmr"}
						] : dataFormat.currentValue == "longFormat" ?
						[
							{ label: qsTr("X-bar & R control chart"),			value: "xBarR"},
							{ label: qsTr("X-bar & s control chart"),			value: "xBarS"},
							{ label: qsTr("X-bar & mR control chart"),			value: "xBarMR"}
						] :
						[
							{ label: qsTr("X-bar & R control chart"),			value: "xBarR"},
							{ label: qsTr("X-bar & s control chart"),			value: "xBarS"},
							{ label: qsTr("X-bar & mR control chart"),			value: "xBarMR"}
						]
						indexDefaultValue: 
						(dataFormat.currentValue == "wideFormat" || (dataFormat.currentValue == "longFormat" && manualSubgroupSizeValue.value > 1)) ? 1 :
						(dataFormat.currentValue == "longFormat" && subgroupSizeType.value == "manual" &&  manualSubgroupSizeValue.value == 1) ? 2 : 1
					}
				

					DoubleField
					{
						name:							"xBarMovingRangeLength"
						label:							qsTr("Moving range length")
						visible:						controlChartType.currentIndex == 2 && dataFormat.currentValue == "wideFormat"
						defaultValue:					2
						min: 							2
						max: 							dataSetInfo.rowCount
					}

					Group 
					{
						visible:						controlChartType.currentIndex == 2 && dataFormat.currentValue == "longFormat"

						DoubleField
						{
							name:							"xmrChartMovingRangeLength"
							label:							qsTr("Moving range length")
							defaultValue:					2
							min: 							2
							max: 							dataSetInfo.rowCount
						}

						CheckBox
						{
							name: 							"xmrChartSpecificationLimits"
							label: 							qsTr("Display specification limits")
							checked: 						false
						}
					}
				}			
			}

			Group
			{
				title: 							qsTr("Distribution of the process")
				info:							qsTr("Visualisations of the distribution of the process data.")

				CheckBox
				{
					name: 						"histogram"
					label: 						qsTr("Histogram")
					checked: 					true
					info:						qsTr("Display a histogram of the process values.")

					CheckBox
					{
						name:					"histogramDensityLine"
						label:					qsTr("Fit distribution")
						checked:				true
						info:					qsTr("Add a line representing the fitted distribution to the histogram.")
					}

					DoubleField
					{
						name:					"histogramBinNumber"
						label:					qsTr("Number of bins")
						defaultValue:			10
						min:					3;
						max:					10000;
						info:					qsTr("Number of bins used for the histogram.")
					}
				}

				CheckBox
				{
					name:						"probabilityPlot"
					label:						qsTr("Probability table and plot")
					checked: 					true
					info:						qsTr("Display the probability table and plot, examining whether the data follow the chosen distribution.")

					CheckBox
					{
						name:					"probabilityPlotGridLines"
						label:					qsTr("Display grid lines")
						info:					qsTr("Add grid lines to the probability plot.")
					}
				}
			}
		}
	}



	Section
	{
		title: qsTr("Process capability report")
		
		CheckBox
		{
			name: "report"
			label: qsTr("Show Report")
			columns: 1
			info:	qsTr("Display a formatted process capability report combining the selected metadata and components.")


			CheckBox
			{
				name:		"reportMetaData"
				label:		qsTr("Show report metadata")
				checked:	true
				columns: 2
				info:		qsTr("Include a metadata header (title, location, machine, variable, date, etc.) in the report.")

				CheckBox
				{
					name:					"reportTitle"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name: 					"reportTitleText"
						label:					qsTr("Title")
						id:						reportTitleText
						placeholderText:		qsTr("Process Capability Report")
						fieldWidth:				100
					}
				}

				CheckBox
				{
					name:					"reportLocation"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name:					"reportLocationText"
						label:					qsTr("Location")
						id:						reportLocationText
						placeholderText:		qsTr("Location")
						fieldWidth:				100
					}
				}

				CheckBox
				{
					name:					"reportLine"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name:					"reportLineText"
						label:					qsTr("Line")
						id:						reportLineText
						placeholderText:		qsTr("Line")
						fieldWidth:				100
					}
				}

				CheckBox
				{
					name:					"reportMachine"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name:					"reportMachineText"
						label:					qsTr("Machine")
						id:						reportMachineText
						placeholderText:		qsTr("Machine")
						fieldWidth:				100
					}
				}

				CheckBox
				{
					name:					"reportVariable"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name:					"reportVariableText"
						label:					qsTr("Variable")
						id:						reportVariableText
						placeholderText:		qsTr("Variable")
						fieldWidth:				100
					}
				}

				CheckBox
				{
					name:					"reportProcess"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name:					"reportProcessText"
						label:					qsTr("Process")
						id:						reportProcessText
						placeholderText:		qsTr("Name")
						fieldWidth:				100
					}
				}

				CheckBox
				{
					name:					"reportDate"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name:					"reportDateText"
						label:					qsTr("Date")
						id:						reportDateText
						placeholderText:		qsTr("Date")
						fieldWidth:				100
					}
				}

				CheckBox
				{
					name:					"reportReportedBy"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name:					"reportReportedByText"
						label:					qsTr("Reported by")
						id:						reportReportedByText
						placeholderText:		qsTr("Name")
						fieldWidth:				100
					}
				}

				CheckBox
				{
					name:					"reportConclusion"
					checked:				true
					childrenOnSameRow:		true

					TextField
					{
						name:					"reportConclusionText"
						label:					qsTr("Conclusion")
						id:						reportConclusionText
						placeholderText:		qsTr("Conclusion")
						fieldWidth:				100
					}
				}
			}
		
			Group
			{
				title:			qsTr("Select report components")
				info:			qsTr("Choose which charts and tables are included in the report.")

				CheckBox
				{
					name:		"reportProcessStability"
					label:		qsTr("Show stability of process charts")
					checked:	true
				}
				
								CheckBox
				{
					name:		"reportProcessCapabilityPlot"
					label:		qsTr("Show process capability plot")
					checked:	true
				}
				
												CheckBox
				{
					name:		"reportProbabilityPlot"
					label:		qsTr("Show probability plot")
					checked:	true
				}
				
																CheckBox
				{
					name:		"reportProcessCapabilityTables"
					label:		qsTr("Show process capability tables")
					checked:	true
				}

				
				
				
				}

		}
	}

	Section
	{
		title: qsTr("Advanced options")
		
		Group
		{
			DropDown
			{
				name:					"probabilityPlotRankMethod"
				label:					qsTr("Rank method for probability plot")
				indexDefaultValue:		0
				info:					qsTr("Method used to calculate the rank of the data in the probability plot. Benard's median rank is the most common and very close to the exact method.")
				values:
				[
					{ value: "bernard",			label: qsTr("Median Rank (Benard)")			},
					{ value: "herdJohnson",		label: qsTr("Mean Rank (Herd-Johnson)")		},
					{ value: "kaplanMeier",		label: qsTr("Kaplan-Meier")					},
					{ value: "hazen",			label: qsTr("Modified Kaplan-Meier (Hazen)")}
				]
			}

			DropDown
				{
					name: 					"histogramBinBoundaryDirection"
					id: 					histogramBinBoundaryDirection
					label: 					qsTr("Histogram bin boundaries")
					info:					qsTr("Whether the histogram bin intervals are left-open or right-open.")
					values:
					[
						{ label: qsTr("Left open"),		value: "left"},
						{ label: qsTr("Right open"),	value: "right"}

					]
				}

			DropDown
			{
				name: 					"nullDistribution"
				id: 					nullDistribution
				label: 					qsTr("Null distribution for probability plot")
				info:					qsTr("The distribution used to examine the data in the probability plot.")
				values:
				[
					{ label: qsTr("Normal"),		         value: "normal"		},
					{ label: qsTr("Log-normal"),	         value: "lognormal"	},
					{ label: qsTr("Weibull"),		         value: "weibull"	},
					{ label: qsTr("Gamma"),			         value: "gamma"		},
					{ label: qsTr("Exponential"),	         value: "exponential"},
					{ label: qsTr("Logistic"),		         value: "logistic"	},
					{ label: qsTr("Log-logistic"),	         value: "loglogistic"},
					{ label: qsTr("3-parameter Weibull"),    value: "3ParameterWeibull"},
					{ label: qsTr("3-parameter log-normal"), value: "3ParameterLognormal"}


								
								
				]
				indexDefaultValue: (capabilityStudyType.value == "nonNormalCapabilityAnalysis") ? 
				(nonNormalDistribution.currentValue == "lognormal") ? 1 : 
				(nonNormalDistribution.currentValue == "weibull") ? 2 :
				(nonNormalDistribution.currentValue == "gamma") ? 3 :
				(nonNormalDistribution.currentValue == "exponential") ? 4 :
				(nonNormalDistribution.currentValue == "logistic") ? 5 :
				(nonNormalDistribution.currentValue == "loglogistic") ? 6 :
				(nonNormalDistribution.currentValue == "3ParameterWeibull") ? 7 :
				(nonNormalDistribution.currentValue == "3ParameterLognormal") ? 8 : 0
				: 0
			}

			Group
			{
				title:			""
				columns:		2

				DropDown
				{
					name: 					"controlChartSdEstimationMethodGroupSize"
					id: 					controlChartSdEstimationMethodGroupSize
					label: 					qsTr("Method for estimating within subgroup std. dev. for subgroup size")
					info:					qsTr("Whether the within-subgroup standard deviation is estimated for subgroups larger than one or for individual observations (size of one).")
					values:
					[
						{ label: qsTr(" > 1"),		value: "largerThanOne"},
						{ label: qsTr("= 1"),		value: "equalOne"}
					]
						indexDefaultValue:
						(dataFormat.currentValue == "longFormat" && manualSubgroupSizeValue.value == 1) ? 1 : 0
				}

				DropDown
				{
					name: 					"controlChartSdEstimationMethodGroupSizeLargerThanOne"
					id: 					controlChartSdEstimationMethodGroupSizeLargerThanOne
					visible:				controlChartSdEstimationMethodGroupSize.currentValue == "largerThanOne"
					label: 					""
					values: 
					[
						{ label: qsTr("R-bar"),			value: "rBar"},
						{ label: qsTr("S-bar"),			value: "sBar"}
					]
					indexDefaultValue:
					(controlChartType.currentValue == "xBarR" || controlChartType.currentValue == "xBarMR") ? 0 : 
					(controlChartType.currentValue == "xBarS") ? 1 : 0
				}

				DropDown
				{
					name: 					"controlChartSdEstimationMethodGroupSizeEqualOne"
					id: 					controlChartSdEstimationMethodGroupSizeEqualOne
					visible:				controlChartSdEstimationMethodGroupSize.currentValue == "equalOne"
					label: 					""
					values: 
					[
						{ label: qsTr("Mean moving range"),			value: "meanMovingRange"}
					]
				}

				DoubleField
				{
					name:							"controlChartSdEstimationMethodMeanMovingRangeLength"
					visible:						controlChartSdEstimationMethodGroupSize.currentIndex == 1
					label:							qsTr("Moving range length")
					defaultValue:					2
					min: 							2
					max: 							dataSetModel.rowCount()
					info:							qsTr("Number of consecutive observations spanned by each moving range when estimating the standard deviation for individuals data.")
				}

				CheckBox
				{
					name:								"controlChartSdUnbiasingConstant"
					label: 								qsTr("Use unbiasing constant")
					visible:							controlChartSdEstimationMethodGroupSize.currentIndex == 1 ? false : true
					checked:							true
					info:								qsTr("Apply the unbiasing constant when estimating the standard deviation.")
				}
			}

			DoubleField
			{
				name: 								"controlLimitsNumberOfSigmas"
				label: 								qsTr("Number of std. dev. for calculation of control limits")
				fieldWidth: 						30
				defaultValue: 						3
				min:								.000001
				info:								qsTr("Number of standard deviations from the central line used to compute the control limits.")
			}
		}

		Common.ControlChartTests {}
	}
}
