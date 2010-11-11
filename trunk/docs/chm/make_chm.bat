@echo off

rem !! Please read the make_chm.README file for information
rem !! about how to build a "php_manual_lang.chm" file.

rem Path of the PHP executable
set PHP_PATH=php

rem Path of the Help Compiler command line tool
set PHP_HELP_COMPILER=C:\progra~1\htmlhe~1\hhc.exe

rem The source directory with the original DSSSL made HTML
set PHP_HELP_COMPILE_DIR=html

rem The directory, where the fancy files need to be copied
set PHP_HELP_COMPILE_FANCYDIR=chm\fancy

rem ==========================================================
rem !!!!!    DO NOT MODIFY ANYTHING BELOW THIS LINE      !!!!!
rem ==========================================================

echo.

set PHP_HELP_COMPILE_LANG=%1
if "%1" == "" set PHP_HELP_COMPILE_LANG=en

echo Language choosen: %PHP_HELP_COMPILE_LANG%

if a%2a == anormala goto skipfancy

echo Now generating the fancy manual in %PHP_HELP_COMPILE_FANCYDIR% dir...
IF NOT EXIST %PHP_HELP_COMPILE_FANCYDIR%\NUL md %PHP_HELP_COMPILE_FANCYDIR%
%PHP_PATH% chm\make_chm_fancy.php

goto normal

:skipfancy
set PHP_HELP_COMPILE_FANCYDIR=
echo Skipping fancy manual generation...

:normal

echo Now running the toc and project file generator script...
%PHP_PATH% chm\make_chm.php

echo Compiling the actual helpfile (smarty_manual_%PHP_HELP_COMPILE_LANG%.chm)...
%PHP_HELP_COMPILER% chm\smarty_manual_%PHP_HELP_COMPILE_LANG%.hhp

echo.
echo Cleaning up the directory
del chm\smarty_manual_%PHP_HELP_COMPILE_LANG%.hh?

echo Done!
echo.
