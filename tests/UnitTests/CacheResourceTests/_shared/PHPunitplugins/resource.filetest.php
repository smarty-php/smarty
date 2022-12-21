<?php

use Smarty\Resource\FilePlugin;

class Smarty_Resource_FiletestPlugin extends FilePlugin
{
    /**
     * populate Source Object with meta data from Resource
     *
     * @param Smarty_Template_Source   $source    source object
     * @param Smarty_Internal_Template $_template template object
     */
    public function populate(Smarty_Template_Source $source, Smarty_Internal_Template $_template = null)
    {
        parent::populate($source, $_template);
        if ($source->exists) {
            if (isset(CacheResourceTestCommon::$touchResource[$source->filepath])) {
                $source->timestamp = CacheResourceTestCommon::$touchResource[$source->filepath];
            }
        }
    }

}

