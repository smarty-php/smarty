<?php

namespace Smarty\Compile\Tag;

use Smarty\Compile\Base;

/**
 * Smarty Internal Plugin Compile For Class
 *


 */
class ForTag extends Base {

	/**
	 * Compiles code for the {for} tag
	 * Smarty supports two different syntax's:
	 * - {for $var in $array}
	 * For looping over arrays or iterators
	 * - {for $x=0; $x<$y; $x++}
	 * For general loops
	 * The parser is generating different sets of attribute by which this compiler can
	 * determine which syntax is used.
	 *
	 * @param array $args array with attributes from parser
	 * @param object $compiler compiler object
	 * @param array $parameter array with compilation parameter
	 *
	 * @return string compiled code
	 */
	public function compile($args, \Smarty\Compiler\Template $compiler, $parameter = [], $tag = null, $function = null): string
	{
		$compiler->loopNesting++;
		if ($parameter === 0) {
			$this->required_attributes = ['start', 'to'];
			$this->optional_attributes = ['max', 'step'];
		} else {
			$this->required_attributes = ['start', 'ifexp', 'var', 'step'];
			$this->optional_attributes = [];
		}

		// check and get attributes
		$_attr = $this->getAttributes($compiler, $args);
		$output = "<?php\n";
		if ($parameter === 1) {
			foreach ($_attr['start'] as $_statement) {
				if (is_array($_statement['var'])) {
					$var = $_statement['var']['var'];
					$index = $_statement['var']['smarty_internal_index'];
				} else {
					$var = $_statement['var'];
					$index = '';
				}
                $itemVar = "\$_smarty_tpl->getVariable({$var})";
				$output .= "\$_smarty_tpl->assign($var, []);\n";
				$output .= "{$itemVar}->value{$index} = {$_statement['value']};\n";
			}
			if (is_array($_attr['var'])) {
				$var = $_attr['var']['var'];
				$index = $_attr['var']['smarty_internal_index'];
			} else {
				$var = $_attr['var'];
				$index = '';
			}
            $itemVar = "\$_smarty_tpl->getVariable({$var})";
			$output .= "if ($_attr[ifexp]) {\nfor (\$_foo=true;$_attr[ifexp]; {$itemVar}->value{$index}$_attr[step]) {\n";
		} else {
			$_statement = $_attr['start'];
			if (is_array($_statement['var'])) {
				$var = $_statement['var']['var'];
				$index = $_statement['var']['smarty_internal_index'];
			} else {
				$var = $_statement['var'];
				$index = '';
			}
            $itemVar = "\$_smarty_tpl->getVariable({$var})";
			$output .= "\$_smarty_tpl->assign($var, []);";
			if (isset($_attr['step'])) {
				$output .= "{$itemVar}->step = $_attr[step];";
			} else {
				$output .= "{$itemVar}->step = 1;";
			}
			if (isset($_attr['max'])) {
				$output .= "{$itemVar}->total = (int) min(ceil(({$itemVar}->step > 0 ? $_attr[to]+1 - ($_statement[value]) : $_statement[value]-($_attr[to])+1)/abs({$itemVar}->step)),$_attr[max]);\n";
			} else {
				$output .= "{$itemVar}->total = (int) ceil(({$itemVar}->step > 0 ? $_attr[to]+1 - ($_statement[value]) : $_statement[value]-($_attr[to])+1)/abs({$itemVar}->step));\n";
			}
			$output .= "if ({$itemVar}->total > 0) {\n";
			$output .= "for ({$itemVar}->value{$index} = $_statement[value], {$itemVar}->iteration = 1;{$itemVar}->iteration <= {$itemVar}->total;{$itemVar}->value{$index} += {$itemVar}->step, {$itemVar}->iteration++) {\n";
			$output .= "{$itemVar}->first = {$itemVar}->iteration === 1;";
			$output .= "{$itemVar}->last = {$itemVar}->iteration === {$itemVar}->total;";
		}
		$output .= '?>';

		if ($compiler->tag_nocache) {
			// push a {nocache} tag onto the stack to prevent caching of this for loop
			$this->openTag($compiler, 'nocache');
		}

		$this->openTag($compiler, 'for', ['for', $compiler->tag_nocache]);

		return $output;
	}
}