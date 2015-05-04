#Smarty 3 template engine
##Distribution repository

*Read the NEW_FEATURES file for recent extensions to Smarty 3.1 functionality*

Smarty packages are now on github and can be installed with Composer.
 
**NOTE: Because of this change you must clear your local composer cache with the "composer clearcache" command**

The "smarty/smarty" package will start at libs/....   subfolder.

To get the latest stable version of Smarty 3.1 use

	"require": {
	   "smarty/smarty": "~3.1"
	}

in your composer.json file.
 
 To get the trunk version use

	"require": {
	   "smarty/smarty": "~3.1@dev"
	}

All stable releases since 3.1.11 are available
For a specific version use something like

	"require": {
               	   "smarty/smarty": "3.1.19"
    }

Composer can also be used for Smarty2 versions 2.6.24 to 2.6.28
