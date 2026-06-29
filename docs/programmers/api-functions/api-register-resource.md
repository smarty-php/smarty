registerResource()

dynamically register resources

Description
===========

void

registerResource

string

name

Smarty\_resource

resource\_handler

Use this to dynamically register a [Resource plugin](../../api/resources.md) with
Smarty. Pass in the `name` of the Resource and the object extending
Smarty\_Resource. See [template resources](../../api/resources.md) for more
information on how to setup a function for fetching templates.

> **Note**
>
> A resource name must be at least two characters in length. One
> character resource names will be ignored and used as part of the file
> path, such as `$smarty->display('c:/path/to/index.tpl');`

> **Note**
>
> Prior to Smarty 3.1 `registerResource()` accepted an array of callback
> functions. While this is still possible for backward compatibility
> reasons, it is strongly discouraged as callback functions have been
> deprecated as of Smarty 3.1.


    <?php
    $smarty->registerResource('mysql', new My_Resource_Mysql());
    ?>

       

See also [`unregisterResource()`](../../programmers/api-functions/api-unregister-resource.md) and the
[template resources](../../api/resources.md) section.
