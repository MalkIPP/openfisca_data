Ce r�pertoire fournit les scripts R pour pr�parer les donn�es issues des enqu�tes ERF pour openfisca

000 convert_all.R :  Conversion des fichiers SAS en fichiers .Rdata
	Param�tres � modifier
	year = '2006';
	year1 = '2007';
	Aborescence utilis�e:
	## Location of sas.exe
	sasloc ="C:/Program Files/SAS/SAS System/9.0/sas.exe"

	## Fichiers sources SAS
	masterDataDir <- "C:/Users/Utilisateur/Documents/Data/"

	## Dossier de sortie
	outputDir <- "C:/Users/Utilisateur/Documents/Data/R/erf/"
	Le r�pertoire outputDir/year  contient les donn�es issues de l'ERF au format SAS converties en .Rdata


00_config.R         : configuration des noms de fichiers Rdata g�n�rqiues pour traitement ult�rieur
	
01_preproc.R 	    : cr�ation des tables "merg�es" des individus et des m�nage (indiviYRm.Rdata et menageYRm.Rdata)

02_imput_loyers.R   : cr�ation de tables logement et erf comparables et imputation des loyers

03_fip.R            : cr�ation de la table des enfants pr�sents sur les feuilles d'imp�t mais pas dans l'EEC

04_famille.R        : cr�ation de la table famille (On suit la m�thode d�crite dans le Guide ERF_2002_r�tropol�e, page 135)	

05_imput.R          : cr�ation de d�claration d'imp�t pour les individus dont on ne retrouve pas la d�claration


TODO : tester et cr�er les r�pertoires
       probl�me quand concat�nation avec fichier vide (famille)	
	 
	
Etapes de la construction de la table
Travail sur les tables individu:
- r�cup�ration des revenus imput�s (pour les zsalo != zsali, etc.)
- 


Travail sur les tables m�nages:
On utilise les tables m�nage pour r�cup�rer les informations suivantes:
- statut d'occupation
- imputation des loyers
- info pour les zones apl: TU, ...

Travail sur la table foyer:
- r�cup�ration de toutes les variables des d�clarations
- r�cup�ration des personnes fip
- 

Construction des familles
- 
-
-

- 

