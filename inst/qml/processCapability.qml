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
  usesJaspResults:							true
  columns:									1

  VariablesForm
  {
    id:										variablesForm

    AvailableVariablesList
    {
      name:								"variablesForm"
    }

    AssignedVariablesList
    {
      id:									variable1
      name:								"diameter"
      title:								qsTr("Diameter")
      allowedColumns:						["scale"]
    }

    AssignedVariablesList
    {
      id:									variable2
      name:								"subgroups"
      title:								qsTr("Subgroups")
      singleVariable:						true
      allowedColumns:						["nominal", "nominalText"]
    }
  }

  Group
  {
    IntegerField { name: "lowerSpecification";  label: qsTr("Lower Specification Limit")                    }
    IntegerField { name: "upperSpecification";  label: qsTr("Upper Specification Limit")                    }
    IntegerField { name: "targetValue";         label: qsTr("Target value")                                 }
  }

  Section
  {
    title: qsTr("Initial Process Capability Study")

    CheckBox { name: "initialXbarchart";			label: qsTr("X-bar Chart")						}
    CheckBox { name: "initialHistogram";			label: qsTr("Histogram")						}
    CheckBox { name: "initialProbabilityPlot";		label: qsTr("Normal Probability Plot")
        DropDown{
            name: "Nulldis"
            label: qsTr("Null distribution (normality test)")
            indexDefaultValue: 0
            values:
            [
                { label: qsTr("Normal"),	                           	value: "normal"		                },
                { label: qsTr("Lognormal"),	                            value: "lognormal"                 	},
                { label: qsTr("3-parameter lognormal"),		          	value: "3parLog"	            	},
                { label: qsTr("Gamma"),			                        value: "gamma"	                 	},
                { label: qsTr("3-parameter gamma"),		          		value: "3parGam"           			},
                { label: qsTr("Exponential"),			              	value: "expo"		             	},
                { label: qsTr("2-parameter exponential"),				value: "2parExpo"			        },
                { label: qsTr("Smallest extreme value"),				value: "small"		                },
                { label: qsTr("Weibull"),				                value: "weibull"			        },
                { label: qsTr("3-parameter Weibull"),				    value: "3parWeibull"			    },
                { label: qsTr("Largest extreme value"),			     	value: "large"		             	},
                { label: qsTr("Logistic"),				                value: "logistic"		         	},
                { label: qsTr("Loglogistic"),				            value: "logLogistic"	    		},
                { label: qsTr("3-parameter loglogistic	"),				value: "3parlogLogistic"			}

            ]

        }
        RadioButtonGroup
                           {
                               name:	"formula";
                               title:	qsTr("Formula for rank calculation")
                               RadioButton { value: "median";		label: qsTr("Median Rank (Benard)");		               checked: true}
                               RadioButton { value: "mean";	        label: qsTr("Mean Rank (Herd-Johnson)")					                }
                               RadioButton { value: "KM";		    label: qsTr("Kaplan-Meier")							                    }
                               RadioButton { value: "KMmodif";		label: qsTr("Modified Kaplan-Meier (Hazen)")							}
                           }


    }
    CheckBox { name: "initialCapabilityAnalysis";	label: qsTr("Process Capability of Diameter")	}
  }

  Section
  {
    title: qsTr("Follow-up Process Capability Study")

    CheckBox { name: "followupControlchart";		label: qsTr("X-bar & Range Control Chart")		}
    CheckBox { name: "followupHistogram";			label: qsTr("Histogram")						}
    CheckBox { name: "followupProbabilityPlot";		label: qsTr("Normal Probability Plot")			}
    CheckBox { name: "followupCapabilityAnalysis";	label: qsTr("Process Capability of Diameter")	}
  }
}
