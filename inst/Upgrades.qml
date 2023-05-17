import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{
	Upgrade
	{
		functionName:	"doeFull"
		newFunctionName: "doeResponseSurfaceMethodology"
		fromVersion:	"0.17.0"
		toVersion:		"0.17.2.1"

	}

	Upgrade
	{
		functionName:	"doeAnalysis"
		fromVersion:	"0.17.0"
		toVersion:		"0.17.2.1"

		ChangeRename { from: "FAallVariables";										to: "allVariables"					}
		ChangeRename { from: "FAresponse";											to: "dependent"						}
		ChangeRename { from: "FAassignedFactors";									to: "fixedFactors"					}
		ChangeRename { from: "FAblocks";											to: "blocks"						}
		ChangeRename { from: "enabledIntOrder ";									to: "highestOrder"					}
		ChangeRename { from: "intOrder";											to: "order"							}
	}

	Upgrade
	{
		functionName:	"variablesChartsIndividuals"
		fromVersion:	"0.17.0"
		toVersion:		"0.17.2.1"

		ChangeRename { from: "ncol";												to: "movingRangeLength"				}
	}
}