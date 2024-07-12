<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * File:     resource.db3.php
 * Type:     resource
 * Name:     db
 * Purpose:  Fetches templates from a database
 * -------------------------------------------------------------
 */

use Smarty\Template;
use Smarty\Template\Source;

class Smarty_Resource_Db3 extends Smarty\Resource\BasePlugin
{
    public function populate(Source $source, ?Template $_template = null)
    {
        $source->uid = sha1($source->resource);
        $source->timestamp = 0;
        $source->exists = true;
    }

    public function getContent(Source $source)
    {
        return '{$x="hello world"}{$x}';
    }

    public function getCompiledFilepath(Template $_template)
    {
        return false;
    }
}
