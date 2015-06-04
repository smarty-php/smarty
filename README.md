#Smarty 3 template engine
##Distribution repository

*Read the NEW_FEATURES file for recent extensions to Smarty 3.1 functionality*

Smarty versions 3.1.11 or later are now on github and can be installed with Composer.


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

For a specific version use something like

	"require": {
               	   "smarty/smarty": "3.1.19"
    }

PHPUnit test can be installed by corresponding composer entries like

	"require": {
               	   "smarty/smarty-phpunit": "3.1.19"
    }

Similar applies for the lexer/parser generator

	"require": {
               	   "smarty/smarty-lexer": "3.1.19"
    }

Or you could use

	"require": {
               	   "smarty/smarty-dev": "3.1.19"
    }

Which is a wrapper to install all 3 packages


Composer can also be used for Smarty2 versions 2.6.24 to 2.6.28
