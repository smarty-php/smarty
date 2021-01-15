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

class Smarty_Resource_Db extends Smarty_Resource_Recompiled {

	public function populate(Smarty_Template_Source $source, Smarty_Internal_Template $_template = null) {
		$source->filepath = 'db:';
		$source->uid = sha1($source->resource);
		$source->timestamp = 0;
		$source->exists = true;
	}

	public function populateTimestamp(Smarty_Template_Source $source): int {
		return 1000000000;
	}

	public function getContent(Smarty_Template_Source $source) {
		return '{$x="hello world"}{$x}';
	}
}
