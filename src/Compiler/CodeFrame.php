<?php

namespace Smarty\Compiler;

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
	 * @param Template|null $compiler
	 *
	 * @return string
	 * @throws Exception
*/
    public function create(
        $content = '',
        $functions = '',
        $cache = false,
        \Smarty\Compiler\Template $compiler = null
    ) {

		$className = ($cache ? 'Cached' : 'Compiled') . str_replace(array('.', ','), '_', uniqid('', true));
	    $properties = [];

	    $properties['version'] = \Smarty\Smarty::SMARTY_VERSION;

	    $file = new \Nette\PhpGenerator\PhpFile;
	    $file->addComment('This file is auto-generated.');
	    $class = $file->addClass($className);

		$class
			->setFinal()
			->setExtends($cache ? \Smarty\CodeFrame\Cached::class : \Smarty\CodeFrame\Base::class)
			->addComment(sprintf(
				"Created on %s from '%s'",
				$properties[ 'version' ],
				date("Y-m-d H:i:s"),
				str_replace('*/', '* /', $this->_template->getSource()->filepath)
			));

	    $dumper = new \Nette\PhpGenerator\Dumper;

        // build property code
        $properties[ 'unifunc' ] = 'content_' . str_replace(array('.', ','), '_', uniqid('', true));
        if (!$cache) {
            $properties[ 'has_nocache_code' ] = $this->_template->getCompiled()->getNocacheCode();
            $properties[ 'file_dependency' ] = $this->_template->getCompiled()->file_dependency;
            $properties[ 'includes' ] = $this->_template->getCompiled()->includes;
        } else {
            $properties[ 'has_nocache_code' ] = $this->_template->getCached()->getNocacheCode();
            $properties[ 'file_dependency' ] = $this->_template->getCached()->file_dependency;
            $properties[ 'cache_lifetime' ] = $this->_template->cache_lifetime;
        }

	    $class->addMethod('getProperties')
		    ->setProtected()
		    ->setReturnType('array') // method return type
		    ->setBody('return ' . $dumper->dump($properties) . ';');

	    $output = (string) $file;

        $output .= sprintf(
			"\n/* Created on %s\n  from '%s' */\n\n",
            $properties[ 'version' ],
	        date("Y-m-d H:i:s"),
	        str_replace('*/', '* /', $this->_template->getSource()->filepath)
        );
        $output .= "/* @var \\Smarty\\Template \$_smarty_tpl */\n";
        $dec = "\$_smarty_tpl->" . ($cache ? "getCached()" : "getCompiled()");
		$dec .= "->isFresh(\$_smarty_tpl, " . var_export($properties, true) . ')';
        $output .= "if ({$dec}) {\n";
        $output .= "function {$properties['unifunc']} (\\Smarty\\Template \$_smarty_tpl) {\n";
        if (!$cache && !empty($compiler->tpl_function)) {
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
        $output .= "?>";
        $output .= $content;
        $output .= "<?php }\n?>";
        $output .= $functions;
        $output .= "<?php }\n";
        // remove unneeded PHP tags
        if (preg_match('/\s*\?>[\n]?<\?php\s*/', $output)) {
            $curr_split = preg_split(
                '/\s*\?>[\n]?<\?php\s*/',
                $output
            );
            preg_match_all(
                '/\s*\?>[\n]?<\?php\s*/',
                $output,
                $curr_parts
            );
            $output = '';
            foreach ($curr_split as $idx => $curr_output) {
                $output .= $curr_output;
                if (isset($curr_parts[ 0 ][ $idx ])) {
                    $output .= "\n";
                }
            }
        }
        if (preg_match('/\?>\s*$/', $output)) {
            $curr_split = preg_split(
                '/\?>\s*$/',
                $output
            );
            $output = '';
            foreach ($curr_split as $idx => $curr_output) {
                $output .= $curr_output;
            }
        }
        return $output;
    }
}
