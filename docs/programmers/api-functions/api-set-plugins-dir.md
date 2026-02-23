setPluginsDir()

set the directories where plugins are stored

Description
===========

Smarty

setPluginsDir

string\|array

plugins\_dir


    <?php

    // set a single directory where the plugins are stored
    $smarty->setPluginsDir('./plugins');

    // view the plugins dir chain
    var_dump($smarty->getPluginsDir());

    // set multiple directoríes where plugins are stored
    $smarty->setPluginsDir(array(
        './plugins',
        './plugins_2',
    ));

    // view the plugins dir chain
    var_dump($smarty->getPluginsDir());

    // chaining of method calls
    $smarty->setTemplateDir('./templates')
           ->setPluginsDir('./plugins')
           ->setCompileDir('./templates_c')
           ->setCacheDir('./cache');

    ?>

       

See also [`getPluginsDir()`](../../programmers/api-functions/api-get-plugins-dir.md),
[`addPluginsDir()`](../../programmers/api-functions/api-add-plugins-dir.md) and
[`$plugins_dir`](#variable.plugins.dir).
