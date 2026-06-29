createTemplate()

returns a template object

Description
===========

Smarty\_Internal\_Template

createTemplate

string

template

object

parent

Smarty\_Internal\_Template

createTemplate

string

template

array

data

Smarty\_Internal\_Template

createTemplate

string

template

string

cache\_id

string

compile\_id

object

parent

Smarty\_Internal\_Template

createTemplate

string

template

string

cache\_id

string

compile\_id

array

data

This creates a template object which later can be rendered by the
[display](../../programmers/api-functions/api-display.md) or [fetch](../../programmers/api-functions/api-fetch.md) method. It uses the
following parameters:

-   `template` must be a valid [template resource](../../api/resources.md) type and
    path.

<!-- -->


    <?php
    use Smarty\Smarty;
    $smarty = new Smarty;

    // create template object with its private variable scope
    $tpl = $smarty->createTemplate('index.tpl');

    // assign variable to template scope
    $tpl->assign('foo','bar');

    // display the template
    $tpl->display();
    ?>

        

See also [`display()`](../../programmers/api-functions/api-display.md), and
[`templateExists()`](#api.template.exists).
