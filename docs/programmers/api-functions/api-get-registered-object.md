getRegisteredObject()

returns a reference to a registered object

Description
===========

array

getRegisteredObject

string

object\_name

This is useful from within a custom function when you need direct access
to a [registered object](../../programmers/api-functions/api-register-object.md). See the
[objects](#advanced.features.objects) page for more info.


    <?php
    function smarty_block_foo($params, $smarty)
    {
      if (isset($params['object'])) {
        // get reference to registered object
        $obj_ref = $smarty->getRegisteredObject($params['object']);
        // use $obj_ref is now a reference to the object
      }
    }
    ?>

       

See also [`registerObject()`](../../programmers/api-functions/api-register-object.md),
[`unregisterObject()`](../../programmers/api-functions/api-unregister-object.md) and [objects
page](#advanced.features.objects)
