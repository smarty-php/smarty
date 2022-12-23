<?php

namespace Smarty\Extension;

class Core extends Base {
	public function getTagCompiler(string $tag): ?\Smarty\Compile\Tag\Base {
		switch ($tag) {
			case 'append': return new \Smarty\Compile\Tag\Append();
			case 'assign': return new \Smarty\Compile\Tag\Assign();
			case 'block': return new \Smarty\Compile\Tag\Block();
			case 'blockclose': return new \Smarty\Compile\Tag\BlockClose();
			case 'break': return new \Smarty\Compile\Tag\BreakTag();
			case 'call': return new \Smarty\Compile\Tag\Call();
			case 'capture': return new \Smarty\Compile\Tag\Capture();
			case 'captureclose': return new \Smarty\Compile\Tag\CaptureClose();
			case 'child': return new \Smarty\Compile\Tag\Child();
			case 'block_child': return new \Smarty\Compile\Tag\BlockChild();
			case 'block_parent': return new \Smarty\Compile\Tag\BlockParent();
			case 'config_load': return new \Smarty\Compile\Tag\ConfigLoad();
			case 'continue': return new \Smarty\Compile\Tag\ContinueTag();
			case 'debug': return new \Smarty\Compile\Tag\Debug();
			case 'eval': return new \Smarty\Compile\Tag\EvalTag();
			case 'extends': return new \Smarty\Compile\Tag\ExtendsTag();
			case 'for': return new \Smarty\Compile\Tag\ForTag();
			case 'foreach': return new \Smarty\Compile\Tag\ForeachTag();
			case 'foreachelse': return new \Smarty\Compile\Tag\ForeachElse();
			case 'foreachclose': return new \Smarty\Compile\Tag\ForeachClose();
			case 'forelse': return new \Smarty\Compile\Tag\ForElse();
			case 'forclose': return new \Smarty\Compile\Tag\ForClose();
			case 'function': return new \Smarty\Compile\Tag\FunctionTag();
			case 'functionclose': return new \Smarty\Compile\Tag\FunctionClose();
			case 'if': return new \Smarty\Compile\Tag\IfTag();
			case 'else': return new \Smarty\Compile\Tag\ElseTag();
			case 'elseif': return new \Smarty\Compile\Tag\ElseIfTag();
			case 'ifclose': return new \Smarty\Compile\Tag\IfClose();
			case 'include': return new \Smarty\Compile\Tag\IncludeTag();
			case 'insert': return new \Smarty\Compile\Inser();
			case 'ldelim': return new \Smarty\Compile\Tag\Ldelim();
			case 'rdelim': return new \Smarty\Compile\Tag\Rdelim();
			case 'make_nocache': return new \Smarty\Compile\Tag\MakeNocache();
			case 'nocache': return new \Smarty\Compile\Tag\Nocache();
			case 'nocacheclose': return new \Smarty\Compile\Tag\NocacheClose();
			case 'parent': return new \Smarty\Compile\Tag\ParentTag();
			case 'private_block_plugin': return new \Smarty\Compile\Tag\PrivateBlockPlugin();
			case 'private_function_plugin': return new \Smarty\Compile\Tag\PrivateFunctionPlugin();
			case 'private_modifier': return new \Smarty\Compile\Tag\PrivateModifier();
			case 'private_object_function': return new \Smarty\Compile\Tag\PrivateObjectFunction();
			case 'private_object_block_function': return new \Smarty\Compile\Tag\PrivateObjectBlockFunction();
			case 'private_print_expression': return new \Smarty\Compile\Tag\PrivatePrintExpression();
			case 'private_registered_function': return new \Smarty\Compile\Tag\PrivateRegisteredFunction();
			case 'private_special_variable': return new \Smarty\Compile\Tag\PrivateSpecialVariable();
			case 'section': return new \Smarty\Compile\Tag\Section();
			case 'sectionelse': return new \Smarty\Compile\Tag\SectionElse();
			case 'sectionclose': return new \Smarty\Compile\Tag\SectionClose();
			case 'setfilter': return new \Smarty\Compile\Tag\Setfilter();
			case 'setfilterclose': return new \Smarty\Compile\Tag\SetfilterClose();
			case 'while': return new \Smarty\Compile\Tag\WhileTag();
			case 'whileclose': return new \Smarty\Compile\Tag\WhileClose();
		}
		return null;
	}

	public function getModifierCompiler(string $modifier): ?\Smarty\Compile\Modifier\Base {
		switch ($modifier) {
			case 'cat': return new \Smarty\Compile\Modifier\CatModifierCompiler();
			case 'count_characters': return new \Smarty\Compile\Modifier\CountCharactersModifierCompiler();
			case 'count_paragraphs': return new \Smarty\Compile\Modifier\CountParagraphsModifierCompiler();
			case 'count_sentences': return new \Smarty\Compile\Modifier\CountSentencesModifierCompiler();
			case 'count_words': return new \Smarty\Compile\Modifier\CountWordsModifierCompiler();
			case 'default': return new \Smarty\Compile\DefaultModifierCompiler();
			case 'escape': return new \Smarty\Compile\Modifier\EscapeModifierCompiler();
			case 'from_charset': return new \Smarty\Compile\Modifier\FromCharsetModifierCompiler();
			case 'indent': return new \Smarty\Compile\Modifier\IndentModifierCompiler();
			case 'lower': return new \Smarty\Compile\Modifier\LowerModifierCompiler();
			case 'nl2br': return new \Smarty\Compile\Modifier\Nl2brModifierCompiler();
			case 'noprint': return new \Smarty\Compile\Modifier\NoPrintModifierCompiler();
			case 'round': return new \Smarty\Compile\Modifier\RoundModifierCompiler();
			case 'str_repeat': return new \Smarty\Compile\Modifier\StrRepeatModifierCompiler();
			case 'string_format': return new \Smarty\Compile\Modifier\StringFormatModifierCompiler();
			case 'strip': return new \Smarty\Compile\Modifier\StripModifierCompiler();
			case 'strip_tags': return new \Smarty\Compile\Modifier\StripTagsModifierCompiler();
			case 'strlen': return new \Smarty\Compile\Modifier\StrlenModifierCompiler();
			case 'to_charset': return new \Smarty\Compile\Modifier\ToCharsetModifierCompiler();
			case 'unescape': return new \Smarty\Compile\Modifier\UnescapeModifierCompiler();
			case 'upper': return new \Smarty\Compile\Modifier\UpperModifierCompiler();
			case 'wordwrap': return new \Smarty\Compile\Modifier\WordWrapModifierCompiler();
		}
		return null;
	}

}