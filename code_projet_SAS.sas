
data archives;
	infile './archives.txt' firstobs=2 dlm=',';
	input age sex cp trestbps chol fbs restecg thalach exang $ oldpeak slope ca thal target;
run;

data heart; /* Jeu de donnée sur lequel nous allons travailler*/
	set archives;
	keep age trestbps chol thalach oldpeak target; /* Variables que nous avons choisi d'étudier*/
run;
proc print data = heart;
run;

proc sort data = heart;
	by age;
run;

/* On cherche à confirmer la validité de chaque individu du jeu de données*/

/* Macro programme pour les boxplots */

%let var = age trestbps chol thalach oldpeak;
%macro boxplots;
	%do i = 1 %to 5;
		%let variable = %scan(&var,&i);
		proc sgplot data = heart;
			title "Variable : &variable";
			vbox &variable / datalabel = age;
		run;
	%end;
	title;
%mend boxplots;
%boxplots;

/* Vérification de la validitée de chaque individus du jeu de données */

/* trestbps */

data heart;
	set heart;
	if trestbps<172;
run;

/* suppression des individus ayant un trestbps anormalement élevé */

/* Chol*/

data heart;
	set heart;
	if chol<394;
run;

/* suppression des individus du jeu de donné ayant un chol anormalement élevé

/* thalach */

data heart;
	set heart;
	if thalach>71;
run;

/* suppression des individus ayant un thalach anormalement faible */

/*oldpeak */

data heart;
	set heart;
	if oldpeak<4.2;
run;
/* suppression des individus ayant un oldpeak anormalement élevé */

/* Analyse univariée target - age*/

proc means data = heart mean; /* moyenne de trestbps par age (pas sur de garder (inutile))*/
	by target;
	var age;
	output out = age1 mean =; /* Moyenne des trestbps par age*/
run;
proc print data = age1;
run;
title1 "Moyenne des trestbps en fonction de l'âge";
proc sgplot data = age1;
	xaxis values = (25 to 80 by 1);
	yaxis values = (100 to 150 by 1);
	series x = target y = age;
run; 

title1 "Analyse univariée de la trestbps en fonction de l'âge";
proc univariate data = heart normal  ;
	var age;

run;

proc univariate noprint data = heart;
	by target;
	var age;
	output out = heart2 q1 = q1 median = me q3 = q3;
run;
proc print data = heart2;
run;

title "Quartile 1, médiane et quartile 3 de trestbps"; 
proc sgplot data = heart2;
	xaxis values = (25 to 80 by 1);
	yaxis values = (100 to 170 by 2);
	series y = me x = target;
	series y = q3 x = target;
	series y = q1 x = target;
run;

proc npar1way edf data = heart;
	class target;
	var age;
run;

/* On peut voir sur la distribution de age que cette
vaiables semble suivre une bimodale.
Ici, les Test de Kolmogorov-Smirnov, de Cramer-von Mises et
de Anderson-Darling ont été réalisé et chacun d'eux ont rejeté l'hypothèse de normalité
La p-value inférieur au risque d'erreur alpha = 5%
On en conclue donc que la variable age ne suit pas une loi 
normale */ 

/* Analyse univariée target - testbps*/

proc means data = heart mean; /* moyenne de trestbps par age (pas sur de garder (inutile))*/
	by target;
	var trestbps;
	output out = trest mean =; /* Moyenne des trestbps par age*/
run;
proc print data = trest;
run;
title1 "Moyenne des trestbps en fonction de l'âge";
proc sgplot data = trest;
	xaxis values = (25 to 80 by 1);
	yaxis values = (100 to 150 by 1);
	series x = target y = trestbps;
run; 

title1 "Analyse univariée de la trestbps en fonction de l'âge";
proc univariate data = heart normal  ;
	var trestbps;

run;

proc univariate noprint data = heart;
	by target;
	var trestbps;
	output out = heart2 q1 = q1 median = me q3 = q3;
run;
proc print data = heart2;
run;

title "Quartile 1, médiane et quartile 3 de trestbps"; 
proc sgplot data = heart2;
	xaxis values = (25 to 80 by 1);
	yaxis values = (100 to 170 by 2);
	series y = me x = target;
	series y = q3 x = target;
	series y = q1 x = target;
run;

proc npar1way edf data = heart;
	class target;
	var trestbps;
run;
/* On peut voir sur la distribution de trestbps que cette
vaiables n'a pas de distribution gaussienne.
Ici, les Test de Kolmogorov-Smirnov, de Cramer-von Mises et
de Anderson-Darling ont été réalisé et chacun d'eux ont une
p-value inférieur au risque d'erreur alpha = 5%
On en conclue donc l'hypothèse de normalité est rejetée et que trestbps ne suit pas une loi normale */ 

/* Analyse univariée target - chol */

proc means data = heart mean; /* moyenne de chol par target (pas sur de garder (inutile))*/
	by target;
	var chol;
	output out = cho mean =; /* Moyenne des chol par target*/
run;
proc print data = cho;
run;
title1 "Moyenne des chol en fonction de l'âge";
proc sgplot data = cho;
	xaxis values = (25 to 80 by 1);
	yaxis values = (190 to 310 by 1);
	series x = age y = chol;
run; 

title1 "Analyse univariée de la chol en fonction de l'âge";
proc univariate normal data = heart;
	var chol;
run;

proc univariate noprint data = heart;
	by target;
	var chol;
	output out = heart2 q1 = q1 median = me q3 = q3;
run;
proc print data = heart2;
run;

title "Quartile 1, médiane et quartile 3 de chol"; 
proc sgplot data = heart2;
	xaxis values = (25 to 80 by 1);
	yaxis values = (150 to 350 by 2);
	series y = me x = target;
	series y = q3 x = target;
	series y = q1 x = target;
run;

proc npar1way edf data = heart;
	class target;
	var chol;
run;

/* On peut voir sur la distribution de chol que cette
vaiables semble suivre une gaussienne avec comme paramètre
Mu estimé à 242.08 et Sigma estimé à 44.75
Ici, les Test de Kolmogorov-Smirnov, de Cramer-von Mises et
de Anderson-Darling ont été réalisé et chacun d'eux ont une
p-value supérieur au risque d'erreur alpha = 5%
On en conclue donc que la variable chol suit bien une loi 
normale. */ 

/* Analyse univariée target - thalach */

proc means data = heart mean; /* moyenne de thalach par target  */
	by target;
	var thalach;
	output out = tha mean =; /* Moyenne des thalach par target*/
run;
proc print data = tha;
run;
title1 "Moyenne des thalach en fonction de l'âge";
proc sgplot data = tha;
	xaxis values = (25 to 80 by 1);
	yaxis values = (110 to 210 by 1);
	series x = target y = thalach;
run; 

title1 "Analyse univariée de la thalach en fonction de l'âge";
proc univariate normal data = heart;
	var thalach;
run;

proc univariate noprint data = heart;
	by target;
	var thalach;
	output out = heart2 q1 = q1 median = me q3 = q3;
run;
proc print data = heart2;
run;

title "Quartile 1, médiane et quartile 3 de thalach"; 
proc sgplot data = heart2;
	xaxis values = (25 to 80 by 1);
	yaxis values = (100 to 220 by 2);
	series y = me x = target;
	series y = q3 x = target;
	series y = q1 x = target;
run;

proc npar1way edf data = heart;
	class target;
	var thalach;
run;

/* On peut voir sur la distribution de thalach que cette
vaiables ne suit pas une distribution gaussienne.
Ici, les Test de Kolmogorov-Smirnov, de Cramer-von Mises et
de Anderson-Darling ont été réalisé et chacun d'eux ont une
p-value inférieur au risque d'erreur alpha = 5%
On en conclue donc que l'hypothèse de normalité est rejetée et que thalach ne suit pas une loi normale */

/* Analyse univariée age - oldpeak  */

proc means data = heart mean; /* moyenne de oldpeak par target  */
	by target;
	var oldpeak;
	output out = old mean =; /* Moyenne des oldpeak par target*/
run;
proc print data = old;
run;
title1 "Moyenne des oldpeak en fonction de l'âge";
proc sgplot data = old;
	xaxis values = (25 to 80 by 1);
	yaxis values = (0 to 2 by 0.05);
	series x = target y = oldpeak;
run; 

title1 "Analyse univariée de la oldpeak en fonction de l'âge";
proc univariate normal data = heart;
	var oldpeak;
run;

proc univariate noprint data = heart;
	by target;
	var oldpeak;
	output out = heart2 q1 = q1 median = me q3 = q3;
run;
proc print data = heart2;
run;

title "Quartile 1, médiane et quartile 3 de oldpeak"; 
proc sgplot data = heart2;
	xaxis values = (25 to 80 by 1);
	yaxis values = (0 to 4 by 0.05);
	series y = me x = target;
	series y = q3 x = target;
	series y = q1 x = target;
run;

proc npar1way edf data = heart;
	class target;
	var oldpeak;
run;

/* On peut voir sur la distribution de oldpeak que cette
vaiables n'a pas de distribution gaussienne.
Ici, les Test de Kolmogorov-Smirnov, de Cramer-von Mises et
de Anderson-Darling ont été réalisé et chacun d'eux ont une
p-value inférieur au risque d'erreur alpha = 5%
On en conclue donc que l'hypothèse de normalité est rejetée et que oldpeak ne suit pas une loi normale */

/* Analyse bivariée trestbps-chol-thalach-oldpeak*/

proc print data = heart;
run;


proc corr data = heart pearson spearman outp = heart2;
	var age trestbps chol thalach oldpeak target;
run;

proc print data = heart2;
run;

/* Plus grande corrélation avec age(thalach - trestbps - chol - oldpeak)*/

proc corr data = heart pearson;
	var  age trestbps chol thalach oldpeak target;
run;

/*correlation entre target et age : -0,22638
  correlation entre target et trestbps : -0.11375
  correlation entre target et chol : -0.10976
  correlation entre target et thalach : 0.42438
  correlation entre target et oldpeak : -0.43575

/* corrélation entre trestbps et chol : 0.06768 0.2410 
   corrélation entre trestbps et thalach : 0.07339 0.2034 
   corrélation entre trestbps et oldpeak : 0.14332 0.0127 
   corrélation entre chol et thalach : 0.08395 0.1456 
   corrélation entre chol et oldpeak : 0.00950 0.8694 
   corrélation entre thalach et oldpeak : -0.29050 <.0001 
*/

/* Les corrélations semblent toutes très faible avec target. 
Cependant, celles avec le oldpeak et le thalach semblent plus 
forte que les autres. */

proc logistic data=heart;
model target(ref='0') = age / clparm=pl clodds=pl expb;
run;

proc logistic data=heart;
model target(ref='0') = thalach / clparm=pl clodds=pl expb;
run;

proc logistic data=heart;
model target(ref='0') = chol / clparm=pl clodds=pl expb;
run;

proc logistic data=heart;
model target(ref='0') = oldpeak / clparm=pl clodds=pl expb;
run;

proc logistic data=heart;
model target(ref='0') = trestbps / clparm=pl clodds=pl expb;
run;


/* Analyse multivariée */

/* Analyse en composantes Principales du coeur */

proc princomp covariance data = heart out = acpout /* On utilise l'option covariance car les données sont toutes quantitatives*/
	plots = pattern(ncomp = 3 vector) ;
	var age--oldpeak;
run;

/* La première composante oppose age, chol, trestbps et oldpeak à thalach et explique 70.79% de la variation totale.
La seconde composante oppose chol et thalach à age, trestbps et oldpeak et explique 18,75% de la variation totale. 
La dernière composante oppose age, trestbps, oldpeak et thalach à chol et explique 8,38% de la variation totale
On propose de retrnir uniquement la première composante au vu de sa participation à la variation totale*/

/* On compare alors les ages des individus à la proportion de leur trois composantes */

proc sgscatter data = acpout;
	compare y = age x = (prin1 prin2);
run;
proc corr data = acpout;
	var target prin2;
run;

/* Pour la première composante, il semblerait que plus l'individu est âgé, plus la composante est élevée (même chose pour la composante 3).
Le phénomène inverse est observé pour la seconde composante*/

/* Logistic */

proc logistic data = heart desc;
	model target = age trestbps chol thalach oldpeak / selection = backward;
run;

/* clusters*/

proc cluster data=heart outtree = ward method =ward ccc;
	var age--oldpeak;
	copy target;
run;

proc tree data = ward out = clusters n = 5;
copy target chol;
run;
proc sort data = clusters;
by cluster;
run;
proc means data = clusters;
var target;
by cluster;
run;
proc sgplot data = clusters;
	vbox chol;
	by cluster;
run;


/* thalach*/

proc tree data = ward out = clusters n = 5;
copy target thalach;
run;
proc sort data = clusters;
by cluster;
run;
proc means data = clusters;
var target;
by cluster;
run;
proc sgplot data = clusters;
	vbox thalach;
	by cluster;
run;

/* oldpeak */

proc tree data = ward out = clusters n = 5;
copy target oldpeak;
run;
proc sort data = clusters;
by cluster;
run;
proc means data = clusters;
var target;
by cluster;
run;
proc sgplot data = clusters;
	vbox oldpeak;
	by cluster;
run;
/* Cluster 4 plus intéressante */

