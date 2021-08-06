*Excel
*execute 'gdxxrw.exe Results_ReferenceFOFA_2050_VeganOrganic_1.gdx o=Results_ReferenceFOFA_2050_VeganOrganic_1.xls par=Results'
*works





*CSV
*$call gdxdump GeneralModelVariables_Various_MR.gdx output=GeneralModelVariables_Various_MR.csv symb=Results format=csv
*works

*$call gdxdump Results_ReferenceFOFA_2050_VeganOrganic_1.gdx output=Results_ReferenceFOFA_2050_VeganOrganic_1_Results.csv symb=Results format=csv
$call gdxdump Results_VeganScenarios_210731_mainRuns.gdx output=world_allProd.csv symb=Organic_Vegan_Results format=csv
$call gdxdump Results_VeganScenarios_210731_mainRuns.gdx output=VActCropsGrass_QuantityActUnits_MR.csv symb=VActCropsGrass_QuantityActUnits_MR format=csv
$call gdxdump Results_VeganScenarios_210731_mainRuns.gdx output=VCommod_Food_Contents_MR.csv symb=VCommod_Food_Contents_MR format=csv
$call gdxdump Results_VeganScenarios_210731_mainRuns.gdx output=VCommod_Food_Contents_Prod_MR.csv symb=VCommod_Food_Contents_Prod_MR format=csv



*$call gdxdump GeneralModelVariables_Auxiliary_MR.gdx output=GeneralModelVariables_Auxiliary_MR.csv symb=VPopulationNumbers_MR format=csv

*$call gdxdump Results_ReferenceFOFA_2050_allVeganScenarios.gdx output=Results_ReferenceFOFA_2050_allVeganScenarios.csv symb=Results format=csv
*$call gdxdump Results_ReferenceFOFA_2050_allVeganScenarios.gdx output=Results_ReferenceFOFA_2050_allVeganScenarios2.csv symb=Results2 format=csv

*$call gdxdump GeneralModelVariables_ActivityQuantities_MR.gdx output=VActCropsGrass_QuantityActUnits_MR.csv symb=VActCropsGrass_QuantityActUnits_MR format=csv


*$call gdxdump commodity_contentsTest.gdx output=commodity_contentsTest.csv symb=Commod_Contents_MR format=csv
*$call gdxdump nut_ha_agg_V.gdx output=nut_ha_agg_V.csv symb=nut_ha_agg_V format=csv
*$call gdxdump nut_ha_V.gdx output=nut_ha_V.csv symb=nut_ha_V format=csv

*$call gdxdump test_cropRotationShares_FOFA_2050_VeganOrgBarb_2.gdx output=test_cropRotationShares_FOFA_2050_VeganOrgBarb_2.csv symb=CropRotationShare format=csv
*$call gdxdump test_cropRotationShares_FOFA_2050_VeganOrgSchmidt_3.gdx output=test_cropRotationShares_FOFA_2050_VeganOrgSchmidt_3.csv symb=CropRotationShare format=csv

