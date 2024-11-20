<?php

use Smarty\Resource\FilePlugin;
use Smarty\Template;
use Smarty\Template\Source;

class Smarty_Resource_FiletestPlugin extends FilePlugin
{
    /**
     * populate Source Object with metadata from Resource
     *
     * @param Source   $source    source object
     * @param Template $_template template object
     */
    public function populate(Source $source, ?Template $_template = null)
    {
        parent::populate($source, $_template);
        if ($source->exists) {
            if (isset(CacheResourceTestCommon::$touchResource[$source->getResourceName()])) {
                $source->timestamp = CacheResourceTestCommon::$touchResource[$source->getResourceName()];
            }
        }
    }

}

