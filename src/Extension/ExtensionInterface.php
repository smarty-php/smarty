<?php

namespace Smarty\Extension;

interface ExtensionInterface {

	public function getTagCompiler(string $tag): ?\Smarty\Compile\Tag\TagCompilerInterface;

	public function getModifierCompiler(string $modifier): ?\Smarty\Compile\Modifier\ModifierCompilerInterface;

	public function getFunctionHandler(string $functionName): ?\Smarty\FunctionHandler\FunctionHandlerInterface;

	public function getBlockHandler(string $blockTagName): ?\Smarty\FunctionHandler\BlockHandlerInterface;

	public function getModifierCallback(string $modifierName);

	public function getOutputFilters(): array;

}