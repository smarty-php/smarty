<?php

namespace Smarty\Extension;

use Smarty\FunctionHandler\FunctionHandlerInterface;

class Base implements ExtensionInterface {

	public function getTagCompiler(string $tag): ?\Smarty\Compile\Tag\TagCompilerInterface {
		return null;
	}

	public function getModifierCompiler(string $modifier): ?\Smarty\Compile\Modifier\ModifierCompilerInterface {
		return null;
	}

	public function getFunctionHandler(string $functionName): ?\Smarty\FunctionHandler\FunctionHandlerInterface {
		return null;
	}

	public function getModifierCallback(string $modifierName) {
		return null;
	}
}