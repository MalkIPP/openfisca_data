Ce répertoire fournit les scripts R pour préparer les données issues des enquêtes ERF pour openfisca

000 convert_all.R :  Conversion des fichiers SAS en fichiers .Rdata
	Paramètres à modifier
	year = '2006';
	year1 = '2007';
	Aborescence utilisée:
	## Location of sas.exe
	sasloc ="C:/Program Files/SAS/SAS System/9.0/sas.exe"

	## Fichiers sources SAS
	masterDataDir <- "C:/Users/Utilisateur/Documents/Data/"

	## Dossier de sortie
	outputDir <- "C:/Users/Utilisateur/Documents/Data/R/erf/"
	Le répertoire outputDir/year  contient les données issues de l'ERF au format SAS converties en .Rdata


00_config.R         : configuration des noms de fichiers Rdata générqiues pour traitement ultérieur
	
01_preproc.R 	    : création des tables "mergées" des individus et des ménage (indiviYRm.Rdata et menageYRm.Rdata)

02_imput_loyers.R   : création de tables logement et erf comparables et imputation des loyers

03_fip.R            : création de la table des enfants présents sur les feuilles d'impôt mais pas dans l'EEC

04_famille.R        : création de la table famille (On suit la méthode décrite dans le Guide ERF_2002_rétropolée, page 135)	

05_imput.R          : création de déclaration d'impôt pour les individus dont on ne retrouve pas la déclaration


TODO : tester et créer les répertoires
       problème quand concaténation avec fichier vide (famille)	
	 
	
Etapes de la construction de la table
Travail sur les tables individu:
- récupération des revenus imputés (pour les zsalo != zsali, etc.)
- 


Travail sur les tables ménages:
On utilise les tables ménage pour récupérer les informations suivantes:
- statut d'occupation
- imputation des loyers
- info pour les zones apl: TU, ...

Travail sur la table foyer:
- récupération de toutes les variables des déclarations
- récupération des personnes fip
- 

Construction des familles
- 
-
-

- 

