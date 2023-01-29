<?php

namespace Smarty\Compiler;

use Nette\PhpGenerator\Dumper;
use Smarty\Exception;

/**
 * Smarty Internal Extension
 * This file contains the Smarty template extension to create a code frame
 *
 * @author     Uwe Tews
 */

/**
 * Create code frame for compiled and cached templates
 */
class CodeFrame
{

	/**
	 * @var \Smarty\Template
	 */
	private $_template;

	public function __construct(\Smarty\Template $_template) {
		$this->_template = $_template;
	}

	/**
	 * Create code frame for compiled and cached templates
	 *
	 * @param string $content optional template content
	 * @param string $functions compiled template function and block code
	 * @param bool $cache flag for cache file
	 *
	 * @return string
	 * @throws Exception
*/
    public function create(
        $content = '',
        $functions = '',
        $cache = false
    ) {

	    $properties = [];

	    $properties['version'] = \Smarty\Smarty::SMARTY_VERSION;
	    $properties['codeFrameClass'] = '__CodeFrame_' . str_replace(array('.', ','), '_', uniqid('', true));
	    if (!$cache) {
		    $properties[ 'has_nocache_code' ] = $this->_template->getCompiled()->getNocacheCode();
		    $properties[ 'file_dependency' ] = $this->_template->getCompiled()->file_dependency;
		    $properties[ 'includes' ] = $this->_template->getCompiled()->includes;
	    } else {
		    $properties[ 'file_dependency' ] = $this->_template->getCached()->file_dependency;
		    $properties[ 'cache_lifetime' ] = $this->_template->cache_lifetime;
	    }

	    $file = new \Nette\PhpGenerator\PhpFile;
	    $file->addComment('This file is auto-generated.');
	    $file->addComment($functions); //@TODO
	    $output = (string) $file;

	    $dumper = new Dumper;

	    $output .= 'if (!class_exists(' . $dumper->dump($properties['codeFrameClass']) . ')) {' . "\n";
	    $output .= $this->generateCodeFrameClass($properties['codeFrameClass'], $cache, $content);
		$output .= "}\n";
		$output .= 'return ' . $dumper->dump($properties) . ";\n";

        return $output;
    }

	private function revertPHPTags(string $content) {

		static $PHPSTART = '<' . '?php';
		static $PHPEND = '?' . '>';

		//\<\?\php echo "    <br>hello world";\?\>
		if (substr($content, 0, 5) === $PHPSTART
			&& substr($content, -3) === $PHPEND . "\n"
		) {
			return substr($content, 5, -3);
		}

		if (false) {
		// remove unneeded PHP tags
		if (preg_match('/\s*\?>[\n]?<\?php\s*/', $content)) {
			$curr_split = preg_split(
				'/\s*\?>[\n]?<\?php\s*/',
				$content
			);
			preg_match_all(
				'/\s*\?>[\n]?<\?php\s*/',
				$content,
				$curr_parts
			);
			$content = '';
			foreach ($curr_split as $idx => $curr_output) {
				$content .= $curr_output;
				if (isset($curr_parts[ 0 ][ $idx ])) {
					$content .= "\n";
				}
			}
		}
		if (preg_match('/\?>\s*$/', $content)) {
			$curr_split = preg_split(
				'/\?>\s*$/',
				$content
			);
			$content = '';
			foreach ($curr_split as $idx => $curr_output) {
				$content .= $curr_output;
			}
		}
		}

		return $PHPEND . $content . $PHPSTART;
	}

	/**
	 * @param $codeFrameClass
	 * @param bool $cache
	 * @param string $content
	 *
	 * @return string
	 */
	private function generateCodeFrameClass($codeFrameClass, bool $cache, string $content): string {
		$class = new \Nette\PhpGenerator\ClassType($codeFrameClass);
		$class
			->setExtends($cache ? \Smarty\CodeFrame\Cached::class : \Smarty\CodeFrame\Compiled::class)
			->addComment(sprintf(
				"Created on %s from '%s'",
				date("Y-m-d H:i:s"),
				str_replace('*/', '* /', $this->_template->getSource()->filepath)
			));

		/*           @TODO
		 * if (!$cache && !empty($compiler->tpl_function)) {
            $output .= '$_smarty_tpl->getSmarty()->getRuntime(\'TplFunction\')->registerTplFunctions($_smarty_tpl, ';
            $output .= var_export($compiler->tpl_function, true);
            $output .= ");\n";
        }
		 if ($cache && $this->_template->getSmarty()->hasRuntime('TplFunction')) {
			if ($tplfunctions = $this->_template->getSmarty()->getRuntime('TplFunction')->getTplFunction($this->_template)) {
				$output .= "\$_smarty_tpl->getSmarty()->getRuntime('TplFunction')->registerTplFunctions(\$_smarty_tpl, " .
					var_export($tplfunctions, true) . ");\n";
			}
        }
*/
		$method = $class->addMethod('renderContent');
		$method->setPublic()
			->addParameter('_smarty_tpl')->setType(\Smarty\Template::class);

		$method->setReturnType('void') // method return type
		->setBody($this->revertPHPTags($content));

		return (new CompactPrinter())->printClass($class);
	}

}
