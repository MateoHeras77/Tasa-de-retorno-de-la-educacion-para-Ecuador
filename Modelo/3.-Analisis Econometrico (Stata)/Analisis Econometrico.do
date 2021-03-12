clear all
cls
import delimited "D:\Mateo Heras\Escritorio\Econometria\Trabajo Final\Modelo\3.-Analisis Econometrico (Stata)\Data_2.csv"

*Tabla general
*asdoc summ años_de_educacion ingresos experiencia_laboral_2 edad,replace stat( N sd mean min max kurtosis skewness)
 
 *==============================================================================*

*Generacion de variables 
gen ln_ingr =ln(ingresos)
gen expe_2 = experiencia_laboral_2* experiencia_laboral_2
gen vidlab = 50
gen ln_edu = ln(años_de_educacion)

*Cambio de nombre de variables
rename experiencia_laboral_2 expe
rename ingresos ingr
rename años_de_educacion años_edu
label variable expe "Experiencial Total"
label variable experiencia_laboral "Experiencia Ultima"
rename experiencia_laboral exp_rel
gen exp_rel_2 = exp_rel * exp_rel
*==================================================================================================================================*
*********************************************Modelo de Thomas Johnson*************************************************************
*==================================================================================================================================*
cls
*ereturn list

***********Modelo de Thomas Jhonson**********************************
*Los valores semillas se escogieron del estudio de Silvana Escando y Mayra Ortiz
nl (ln_ingr = {b=-1.7650}+ln(1-{alpha=0.5736}+({alpha}/vidlab)* expe)+({beta=0.0923}-{sigma=0.0279})* años_edu +({beta}*{alpha}-{sigma})* expe-({beta}*{alpha}/(2*vidlab))* expe_2),vce(robust) nolog  variables(vidlab expe años_edu expe_2)
testnl -_b[/b]=-_b[ /alpha]=-_b[  /beta]=-_b[ /sigma ]=0
estimates store TJ_1_g
eststo empJhonson_g: margins, dydx(*) post

***********Modelo de Thomas Jhonson (Hombre)**********************************
nl (ln_ingr = {b=-1.7650}+ln(1-{alpha=0.5736}+({alpha}/vidlab)* expe)+({beta=0.0923}-{sigma=0.0279})* años_edu +({beta}*{alpha}-{sigma})* expe-({beta}*{alpha}/(2*vidlab))* expe_2)if sexo=="Hombre",vce(robust) nolog  variables(vidlab expe años_edu expe_2)
testnl -_b[/b]=-_b[ /alpha]=-_b[  /beta]=-_b[ /sigma ]=0
estimates store TJ_1_h
eststo empJhonson_h: margins, dydx(*) post

***********Modelo de Thomas Jhonson (Mujer)**********************************
nl (ln_ingr = {b=-1.7650}+ln(1-{alpha=0.5736}+({alpha}/vidlab)* expe)+({beta=0.0923}-{sigma=0.0279})* años_edu +({beta}*{alpha}-{sigma})* expe-({beta}*{alpha}/(2*vidlab))* expe_2) if sexo=="Mujer",vce(robust) nolog  variables(vidlab expe años_edu expe_2)
testnl -_b[/b]=-_b[ /alpha]=-_b[  /beta]=-_b[ /sigma ]=0
estimates store TJ_1_m
eststo empJhonson_m: margins, dydx(*) post

asdoc esttab TJ_1_g TJ_1_h TJ_1_m

*******Efectos Marginales***************
asdoc esttab empJhonson_g empJhonson_h empJhonson_m

*asdoc margins, eyex(años_edu) at(años_edu = 7)
*asdoc margins, eyex(años_edu) at(años_edu = 6)
*asdoc margins, eyex(años_edu) at(años_edu = 5)


*==================================================================================================================================================*
*************************************************Estudio de la transformacion BOX COX**************************************************************
*==================================================================================================================================================*

*reg ln_ingr años_edu expe expe_2

*========================= SIN logaritmos=========================

reg ingr años_edu  expe expe_2 
estimates store Sln
predict ing_hat 
predict ing_error, residuals
kdensity ing_error

*=========================BOX COX=========================
boxcox ingr años_edu  , notrans(expe expe_2) nolog model(theta) lrtest 
boxcox ingr años_edu  expe expe_2, model(lambda)  nolog /*both sides Box-Cox model with same parameter */

boxcox ingr años_edu  expe expe_2, model(lhsonly)  nolog  /* left-hand-side Box-Cox model*/
boxcox ingr años_edu  expe expe_2, model(rhsonly)  nolog /*right-hand-side Box-Cox model */


*=========================CON logaritmos=========================
reg ln_ingr ln_edu  expe expe_2

estimates store Cln
predict lning_hat 
predict lning_error, residuals
kdensity lning_error
gen ln_edu_1 = ln_edu if ln_edu >0

*========================= BOX COX=========================

boxcox ln_ingr ln_edu_1  , notrans(expe expe_2) nolog model(theta) lrtest
boxcox ln_ingr ln_edu_1 expe expe_2, model(lambda)  nolog /*both sides Box-Cox model with same parameter */

boxcox ln_ingr ln_edu_1 expe expe_2, model(lhsonly)  nolog  /* left-hand-side Box-Cox model*/
boxcox ln_ingr ln_edu_1 expe expe_2, model(rhsonly)  nolog /*right-hand-side Box-Cox model */

* Vemos la normalidad de los errores para verificar el modelo C-D a escoger
swilk ing_error lning_error
quietly kdensity ing_error, normal title("Sin Logaritmos", size(medium)) ytitle("Densidad") legend(size(small))
quietly graph save ing_error.gph, replace
quietly kdensity lning_error, normal title("Con Logaritmos", size(medium)) ytitle("Densidad") legend(size(small))
quietly graph save lning_error.gph, replace
graph combine ing_error.gph lning_error.gph, title("Distribucion los Errores", size(medium))

*===================RESULTADO BOX COX=========================================*

*Dado los resultados obtenidos por BOX COX conlcuimos que las variables Ingresos años de educacion se deben correr con logaritmos

*Modelo 1 =========================================
reg ln_ingr ln_edu  expe expe_2 , vce(robust)
test ln_edu = 0
test expe = 0
test expe_2 = 0
estimates store M_1
eststo empModel_1: margins, dydx(*) post



*Modelo 2: Analisis de Robustes=========================

gen casado = ( estado_civil =="Casado(a)")
gen urbana = ( area =="Urbana")

reg ln_ingr ln_edu exp_rel exp_rel_2 casado urbana , vce(robust)
test ln_edu = 0
test exp_rel = 0
test exp_rel_2 = 0
test casado = 0
test urbana = 0

estimates store M_2
eststo empModel_2: margins, dydx(*) post
vif

 
*==============================================================================*
*==============================================================================*

**Tabla compartiva efectos margianles
asdoc esttab  empModel_1 empModel_2  , b(3) n r2
asdoc esttab  M_1 M_2  , b(3) n r2

*===================SEGMENTACION POR GENERO=========================================*

*Dado los resultados obtenidos por BOX COX conlcuimos que las variables Ingresos años de educacion se deben correr con logaritmos

*Modelo 1 =========================================
reg ln_ingr ln_edu  expe expe_2 if sexo =="Mujer", vce(robust)
test ln_edu = 0
test expe = 0
test expe_2 = 0

estimates store M_1_m
vif

reg ln_ingr ln_edu  expe expe_2 if sexo =="Hombre", vce(robust)
test ln_edu = 0
test expe = 0
test expe_2 = 0

estimates store M_1_h
vif

*Modelo 2: Analisis de Robustes=========================

gen casado = ( estado_civil =="Casado(a)")
gen urbana = ( area =="Urbana")

reg ln_ingr ln_edu exp_rel exp_rel_2 casado urbana if sexo =="Mujer" , vce(robust)
test ln_edu = 0
test exp_rel = 0
test exp_rel_2 = 0
test casado = 0
test urbana = 0

estimates store M_2_m
vif
reg ln_ingr ln_edu exp_rel exp_rel_2 casado urbana if sexo =="Hombre" , vce(robust)
test ln_edu = 0
test exp_rel = 0
test exp_rel_2 = 0
test casado = 0
test urbana = 0

estimates store M_2_h
vif
 
 asdoc esttab  M_1_h M_2_h M_1_m M_2_m , b(3) n r2 

 
*==============================================================================*
*==============================================================================*

**Tabla compartiva efectos margianles
asdoc esttab  empModel_1 empModel_2  , b(3) n r2
asdoc esttab  M_1 M_2  , b(3) n r2

