display()

displays the template

Description
===========

void

display

string

template

string

cache\_id

string

compile\_id

This displays the contents of a template. To return the contents of a
template into a variable, use [`fetch()`](../../programmers/api-functions/api-fetch.md). Supply a valid
[template resource](../../api/resources.md) type and path. As an optional second
parameter, you can pass a `$cache_id`, see the [caching
section](../../api/caching/basics.md) for more information.

PARAMETER.COMPILEID


    <?php

    use Smarty\Smarty;

    $smarty = new Smarty();
    $smarty->setCaching(true);

    // only do db calls if cache doesn't exist
    if(!$smarty->isCached('index.tpl')) {

      // dummy up some data
      $address = '245 N 50th';
      $db_data = array(
                   'City' => 'Lincoln',
                   'State' => 'Nebraska',
                   'Zip' => '68502'
                 );

      $smarty->assign('Name', 'Fred');
      $smarty->assign('Address', $address);
      $smarty->assign('data', $db_data);

    }

    // display the output
    $smarty->display('index.tpl');
    ?>

       

Use the syntax for [template resources](../../api/resources.md) to display files
outside of the [`$template_dir`](#variable.template.dir) directory.


    <?php
    // absolute filepath
    $smarty->display('/usr/local/include/templates/header.tpl');

    // absolute filepath (same thing)
    $smarty->display('file:/usr/local/include/templates/header.tpl');

    // windows absolute filepath (MUST use "file:" prefix)
    $smarty->display('file:C:/www/pub/templates/header.tpl');

    // include from template resource named "db"
    $smarty->display('db:header.tpl');
    ?>

       

See also [`fetch()`](../../programmers/api-functions/api-fetch.md) and
[`templateExists()`](#api.template.exists).
