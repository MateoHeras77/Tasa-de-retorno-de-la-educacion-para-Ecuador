clear all
cls

use "D:\Mateo Heras\Escritorio\Econometria\Trabajo Final\Modelo\1.-Segementacion de Varaibles (Stata)\enemdu_persona_201912.dta"

*INGRESOS: realizado en base a las FIchas Tecnicas del INEC
*clonevar sexo=p02
*egen npersona=count(sexo), by (idhogar)

ren ingrl ingrl_inec
label drop ingrl
generat ingr= 0
replace ingr= ingr + p63 if p63 <999999
replace ingr= ingr + p64b if p64b<999999
replace ingr= ingr - p65 if p65 <999999
replace ingr= ingr + p66 if p66 <999999
replace ingr= ingr + p67 if p67 <999999
replace ingr= ingr + p68b if p68b<999999
recode ingr (0=.) if (p63==. & p64b==. & p65==. & p66==. & p67==. & p68b==.)
replace ingr=999999 if p63==999999
replace ingr=999999 if p66==999999
generat ingrls= 0
replace ingrls= ingrls + p69 if p69 <999999
replace ingrls= ingrls + p70b if p70b<999999
recode ingrls (0=.) if (p69==. & p70b==.)
recode ingrls (0=999999) if (p69==999999 & p70b==999999)
recode ingrls (0=999999) if (p69==. & p70b==999999)
generat ingrl= 0
replace ingrl= ingrl + ingrls if (ingr<0 & ingrls<999999)
replace ingrl= ingrl + ingr + ingrls if (((ingr>0 & ingr<999999)| ingr==0) & (ingrls<999999))
replace ingrl= ingrl + ingrls if (ingr==. & ingrls<999999)
replace ingrl= ingrl + ingr if (ingrls==. & (ingr>0 & ingr<999999))
replace ingrl= -1 if (ingr<0 & ingrls==.)
replace ingrl= 999999 if (ingr==999999 & ingrls==.)
replace ingrl= 999999 if (ingr==999999 & (ingrls>=0 & ingrls<999999))
replace ingrl= 999999 if (ingrls==999999 & (ingr>0 & ingr<999999))
recode ingrl(0=.) if (ingr==. & ingrls==.)
label var ingrl "Ingreso Laboral"
label define ingrl -1 "Gasta más de lo que gana" 999999 "No informa"
label value ingrl ingrl
label var ingr "Ingreso Trabajo Principal"
label value ingr
label var ingrls "Ingreso Ocupación Secundaria"
label value ingrls ingrls
generat ingrltot = 0
replace ingrltot = ingrltot + p71b if (p71a==1 & p71b<999999)
replace ingrltot = ingrltot + p72b if (p72a==1 & p72b<999999)
replace ingrltot = ingrltot + p73b if (p73a==1 & p73b<999999)
replace ingrltot = ingrltot + p74b if (p74a==1 & p74b<999999)
replace ingrltot = ingrltot + p76 if (p75 ==1 & p76 <999999)
replace ingrltot = 999999 if (ingrl==999999)
replace ingrltot = ingrltot + ingrl if (ingrl>-1 & ingrl<999999)
* PERSONAS QUE NO TIENEN INVERSIONES, TRANSFERENCIAS, BDH Y NO TIENEN
*INGRESOS DEL TRABAJO.
recode ingrltot (0=.) if (p71b==. & p72b==. & p73b==. & p74b==. & p76==. & p78==. & (ingrl==. | ingrl==-1))
recode ingrltot (0=.) if (ingrl==. & ingrltot==0)
label var ingrltot "Ingreso Total"
label define ingrltot 999999 "No informa"
label value ingrltot ingrltot
replace ingrltot = 999999 if (p63 ==999 | p63 ==9999 | p63 ==99999)
replace ingrltot = 999999 if (p64b==999 | p64b==9999 | p64b==99999)
replace ingrltot = 999999 if (p65 ==999 | p65 ==9999 | p65 ==99999)
replace ingrltot = 999999 if (p66 ==999 | p66 ==9999 | p66 ==99999)
replace ingrltot = 999999 if (p67 ==999 | p67 ==9999 | p67 ==99999)
replace ingrltot = 999999 if (p68b==999 | p68b==9999 | p68b==99999)
replace ingrltot = 999999 if (p69 ==999 | p69 ==9999 | p69 ==99999)
replace ingrltot = 999999 if (p70b==999 | p70b==9999 | p70b==99999)
replace ingrltot = 999999 if (p71b==999 | p71b==9999 | p71b==99999)
replace ingrltot = 999999 if (p72b==999 | p72b==9999 | p72b==99999)
replace ingrltot = 999999 if (p73b==999 | p73b==9999 | p73b==99999)
replace ingrltot = 999999 if (p74b==999 | p74b==9999 | p74b==99999)
replace ingrltot = 999999 if (p76 ==999 | p76 ==9999 | p76 ==99999)
recode ingrltot (0=.)
recode ingrltot (999999=.)

* AÑOS DE ESCOLARIDAD: realizado en base a las FIchas Tecnicas del INEC

*clonevar p10a=nivinst
*clonevar p10b=anoinst
generate escola=0 if p10a==1
replace escola=(p10b/2) if p10a==2
replace escola= 0 if p10a==3
replace escola=p10b if p10a==4
replace escola=p10b-1 if p10a==5 & p10b>=1
replace escola=p10b+6 if p10a==6
replace escola=p10b+9 if p10a==7
replace escola=p10b+12 if p10a==8
replace escola=p10b+12 if p10a==9
replace escola=p10b+17 if p10a==10

*Datos a usar en el proyecto 
keep ingrltot area ciudad p02 p03 p06 p10a p10b p45 escola

*La experiencia laboral se calcula en años, y corresponde al residuo entre la edad de la persona y sus años de escolaridad
gen exp = p03 - escola 

*Exportamos la base de datos para hacer un analisis grafico en Rstudio
export excel using "Datos", firstrow(variables) replace

