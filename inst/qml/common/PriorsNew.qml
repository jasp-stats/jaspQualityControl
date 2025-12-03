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
import JASP

ColumnLayout
{
	spacing: 						0
	property string priorType:	"normalModel"

 	Component.onCompleted: {
		console.log("Component completed, priorType: " + priorType);
		console.log("Current component values: " + JSON.stringify(currentComponentValues));
	}

	onPriorTypeChanged: {
		// this is not shown?
		console.log("Prior type changed to: " + priorType);
	}

	// TODO: these should not be fixed, no?
	property var meanValues: 	{ "name": "mean",  "type": "normal",   "mu":    "0", "sigma": "1" }
	property var sigmaValues: 	{ "name": "sigma", "type": "invgamma", "alpha": "1", "beta": "0.15", "truncationLower": 0 }
	property var dfValues: 	  	{ "name": "t",     "type": "invgamma", "alpha": "1", "beta": "0.15", "truncationLower": 0, "hasJeffreys": false }

	property var currentComponentValues: {
		switch (priorType) {
			case "normalModel":
					return [ meanValues, sigmaValues ];
			case "tModel":
				return [ meanValues, sigmaValues, dfValues ];
		}
	}


	// TODO: this could also be a gridLayout, no?
	property double width1: 70 * preferencesModel.uiScale;
	property double width2: 140 * preferencesModel.uiScale;
	property double width3: 155 * preferencesModel.uiScale;
	property double width4: 130 * preferencesModel.uiScale;

	RowLayout
	{
		Label { text: qsTr("Parameter"); 	Layout.preferredWidth: width1; Layout.leftMargin: 5 * preferencesModel.uiScale}
		Label { text: qsTr("Distribution"); Layout.preferredWidth: width2; Layout.leftMargin: 5 * preferencesModel.uiScale}
		Label { text: qsTr("Parameters");	Layout.preferredWidth: width3 }
		Label { text: qsTr("Truncation"); 	Layout.preferredWidth: width4 }
	}

	ComponentsList
	{
		name:					priorType + "ComponentsList"
		optionKey:				"name"

		addItemManually:		false

		defaultValues:			currentComponentValues

		rowComponent: 			RowLayout
		{
			Row
			{
				spacing:				4 * preferencesModel.uiScale
				Layout.preferredWidth:	width1

				Label
				{
					text: rowValue// + "|" + "|" + rowValue.type;
				}
			}

			Row
			{
				spacing:				4 * preferencesModel.uiScale
				Layout.preferredWidth:	width2

				DropDown
				{
					id: typeItem
					name: "type"
					useExternalBorder: true
					values:
					[
						{ label: qsTr("Normal(μ,σ)"),			value: "normal"},
						{ label: qsTr("Student-t(μ,σ,v)"),		value: "t"},
						{ label: qsTr("Cauchy(x₀,θ)"),			value: "cauchy"},
						{ label: qsTr("Jeffreys"),				value: "jeffreys"},
						{ label: qsTr("Gamma(α,β)"),			value: "gammaAB"},
						{ label: qsTr("Gamma(k,θ)"),			value: "gammaK0"},
						{ label: qsTr("Inverse-Gamma(α,β)"),	value: "invgamma"},
						{ label: qsTr("Log-Normal(μ,σ)"),		value: "lognormal"},
						{ label: qsTr("Beta(α,β)"),				value: "beta"},
						{ label: qsTr("Uniform(a,b)"),			value: "uniform"}//,
						//{ label: qsTr("Spike(x₀)"),				value: "spike"},
						// { label: qsTr("None"),					value: "none"}
					]

					onValueChanged: {
						console.log("Selected prior type: " + typeItem.currentValue);
						console.log("rowValue equals: " + rowValue[1]);
					}
				}
			}

			Row
			{
				spacing:				4 * preferencesModel.uiScale
				Layout.preferredWidth:	width3

				FormulaField
				{
					label:				"μ"
					name:				"mu"
					visible:			typeItem.currentValue === "normal"		||
										typeItem.currentValue === "lognormal"	||
										typeItem.currentValue === "t"
					value:				"0"
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
					onValueChanged : {
						console.log("Selected prior type: " + typeItem.currentValue);
						console.log("rowValue equals: " + rowValue[1]);
					}
				}
				FormulaField
				{
					label:				"x₀"
					name:				"x0"
					visible:			typeItem.currentValue === "cauchy"	||
										typeItem.currentValue === "spike"
					value:				"0"
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"σ"
					name:				"sigma"
					id:					sigma
					visible:			typeItem.currentValue === "normal"		||
										typeItem.currentValue === "lognormal"	||
										typeItem.currentValue === "t"
					value:				"1"
					min:				0
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"k "
					name:				"k"
					visible:			typeItem.currentValue === "gammaK0"
					value:				"1"
					min:				0
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
				}
				FormulaField
				{
					label:				"θ"
					name:				"theta"
					visible:			typeItem.currentValue === "cauchy"	||
										typeItem.currentValue === "gammaK0"
					value:				"1"
					min:				0
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"ν"
					name:				"nu"
					visible:			typeItem.currentValue === "t"
					value:				"2"
					min:				1
					inclusive:			JASP.MinOnly
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"α "
					name:				"alpha"
					visible:			typeItem.currentValue === "gammaAB"	 ||
										typeItem.currentValue === "invgamma" ||
										typeItem.currentValue === "beta"
					value:				"1"
					min:				0
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"β"
					name:				"beta"
					visible:			typeItem.currentValue === "gammaAB"	 ||
										typeItem.currentValue === "invgamma" ||
										typeItem.currentValue === "beta"
					value:				"0.15"
					min:				0
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"a "
					name:				"a"
					id:					a
					visible:			typeItem.currentValue === "uniform"
					value:				"0"
					max:				b.value
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
				FormulaField
				{
					label:				"b"
					name:				"b"
					id:					b
					visible:			typeItem.currentValue === "uniform"
					value:				"1"
					min:				a.value
					inclusive:			JASP.None
					fieldWidth: 		40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder: 		true
				}
			}

			Row
			{
				spacing:				4 * preferencesModel.uiScale
				Layout.preferredWidth:	width4

				FormulaField
				{
					id:					truncationLower
					label: 				qsTr("lower")
					name: 				"truncationLower"
					visible:			typeItem.currentValue !== "spike" && typeItem.currentValue !== "uniform"
					value:
					{
						console.log("rowValue:" + rowValue)
						"-Inf"
					}
					min:
					{
						console.log("rowValue:" + rowValue)
						"-Inf"
					}
					max: 				truncationUpper.value
					inclusive: 			JASP.MinOnly
					fieldWidth:			40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder:			true
				}
				FormulaField
				{
					id:					truncationUpper
					label: 				qsTr("upper")
					name: 				"truncationUpper"
					visible:			typeItem.currentValue !== "spike" && typeItem.currentValue !== "uniform"
					value:
					{
						if (typeItem.currentValue === "beta")
							1
						else
							"Inf"
					}
					max:
					{
						if (typeItem.currentValue === "beta")
							1
						else
							"Inf"
					}
					min: 				truncationLower ? truncationLower.value : 0
					inclusive: 			JASP.MaxOnly
					fieldWidth:			40 * preferencesModel.uiScale
					useExternalBorder:	false
					showBorder:			true
				}
			}
		}
	}
}
