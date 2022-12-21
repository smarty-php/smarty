<?php

/*
 * Smarty plugin
 * -------------------------------------------------------------
 * File:     resource.db.php
 * Type:     resource
 * Name:     db
 * Purpose:  Fetches templates from a database
 * -------------------------------------------------------------
 */

use Smarty\Resource\RecompiledPlugin;

class _DbPlugin extends RecompiledPlugin {

    public function populate(Smarty_Template_Source $source, Smarty_Internal_Template $_template = null) {
        $source->filepath = 'db:';
        $source->uid = sha1($source->resource);
        $source->timestamp = 1000000000;
        $source->exists = true;
    }

    public function getContent(Smarty_Template_Source $source) {
        return '{$x="hello world"}{$x}';
    }
}
