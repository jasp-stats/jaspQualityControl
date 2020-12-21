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

import QtQuick                  2.8
import QtQuick.Layouts              1.3
import JASP.Controls              1.0
import JASP.Widgets               1.0

Form
{
	usesJaspResults:              true
	columns:						1

	VariablesForm
	{
		id:                   variablesForm

		AvailableVariablesList
		{
			name:               "variablesForm"
		}

		AssignedVariablesList
		{
			id:                 variable1
			name:               "diameter"
			title:              qsTr("Measurements")
			allowedColumns:     ["scale"]
		}

		AssignedVariablesList
		{
			id:                 variable2
			name:               "subgroups"
			title:              qsTr("Subgroups")
			singleVariable:     true
			allowedColumns:     ["nominal", "nominalText", "ordinal", "scale"]
		}
	}

	Group
	{
		title: qsTr("Stability of the Process")
		CheckBox { name: "controlCharts";			label: qsTr("X-bar & Range Control Chart") }
	}

	Group
	{
		title: qsTr("Distribution of the Process")
		CheckBox { name: "histogram";			label: qsTr("Histogram")}
		CheckBox { name: "probabilityPlot";    label: qsTr("Probability Plot")
			DropDown
			{
				name: "rank"
				label: qsTr("Rank method")
				indexDefaultValue: 0
				values:
					[
					{ value: "median",    label: qsTr("Median Rank (Benard)")                        },
					{ value: "mean",      label: qsTr("Mean Rank (Herd-Johnson)")                    },
					{ value: "KM",        label: qsTr("Kaplan-Meier")                                },
					{ value: "KMmodif",   label: qsTr("Modified Kaplan-Meier (Hazen)")               }
				]
			}
			DropDown
			{
				name: "Nulldis"
				label: qsTr("Null distribution")
				indexDefaultValue: 0
				values:
					[
					{ label: qsTr("Normal"),		value: "Normal"			},
					{ label: qsTr("Lognormal"),	value: "Lognormal"      },
					{ label: qsTr("Weibull"),		value: "Weibull"        }
				]
			}
		}
	}

	Section
	{
		title:	qsTr("Process Capability Study")
		RadioButtonGroup
		{
			name: "capabilityStudy"
			RadioButton { name: "initialCapabilityAnalysis";	label: qsTr("Initial Capability Analysis"); checked: true	}
			RadioButton { name: "followupCapabilityAnalysis";  label: qsTr("Follow-up Capability Analysis")	}
		}
		Group
		{
			DoubleField { name: "upperSpecification";  label: qsTr("Upper Specification Limit") ; negativeValues: true   }
			DoubleField { name: "targetValue";         label: qsTr("Target value") ; negativeValues: true                }
			DoubleField { name: "lowerSpecification";  label: qsTr("Lower Specification Limit") ; negativeValues: true   }
		}
	}
}
