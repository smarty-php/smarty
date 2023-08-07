<?php

namespace Smarty\Extension;

interface ExtensionInterface {

	public function getTagCompiler(string $tag): ?\Smarty\Compile\CompilerInterface;

	public function getModifierCompiler(string $modifier): ?\Smarty\Compile\Modifier\ModifierCompilerInterface;

	public function getFunctionHandler(string $functionName): ?\Smarty\FunctionHandler\FunctionHandlerInterface;

	public function getBlockHandler(string $blockTagName): ?\Smarty\BlockHandler\BlockHandlerInterface;

	public function getModifierCallback(string $modifierName);

	public function getPreFilters(): array;
	public function getPostFilters(): array;
	public function getOutputFilters(): array;

}