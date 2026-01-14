unregisterResource()

dynamically unregister a resource plugin

Description
===========

void

unregisterResource

string

name

Pass in the `name` of the resource.


    <?php

    $smarty->unregisterResource('db');

    ?>

       

See also [`registerResource()`](../../programmers/api-functions/api-register-resource.md) 
and [template resources](../../api/resources.md)
