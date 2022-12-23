<?php

namespace Smarty\Extension;

interface ExtensionInterface {

	public function getTagCompiler(string $tag): ?\Smarty\Compile\Tag\Base;

	public function getModifierCompiler(string $modifier): ?\Smarty\Compile\Modifier\Base;

}