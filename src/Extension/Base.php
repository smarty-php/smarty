<?php

namespace Smarty\Extension;

class Base implements ExtensionInterface {

	public function getTagCompiler(string $tag): ?\Smarty\Compile\Tag\Base {
		return null;
	}

	public function getModifierCompiler(string $modifier): ?\Smarty\Compile\Modifier\Base {
		return null;
	}
}