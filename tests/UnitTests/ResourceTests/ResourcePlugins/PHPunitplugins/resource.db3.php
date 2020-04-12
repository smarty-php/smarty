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

class Smarty_Resource_Db3 extends Smarty_Resource
{
    public function populate(Smarty_Template_Source $source, Smarty_Internal_Template $_template = null)
    {
        $source->filepath = 'db3:';
        $source->uid = sha1($source->resource);
        $source->timestamp = 0;
        $source->exists = true;
    }

    public function getContent(Smarty_Template_Source $source)
    {
        return '{$x="hello world"}{$x}';
    }

    public function getCompiledFilepath(Smarty_Internal_Template $_template)
    {
        return false;
    }
}
