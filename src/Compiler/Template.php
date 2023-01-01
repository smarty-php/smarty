<?php
/**
 * Smarty Internal Plugin Smarty Template Compiler Base
 * This file contains the basic classes and methods for compiling Smarty templates with lexer/parser
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */

namespace Smarty\Compiler;

use Smarty\Compile\BlockCompiler;
use Smarty\Compile\ModifierCompiler;
use Smarty\Compile\ObjectMethodBlockCompiler;
use Smarty\Compile\ObjectMethodCallCompiler;
use Smarty\Compile\FunctionCallCompiler;
use Smarty\Lexer\TemplateLexer;
use Smarty\Parser\TemplateParser;
use Smarty\Smarty;
use Smarty\Compile\Tag\ExtendsTag;
use Smarty\CompilerException;
use Smarty\Exception;
use function array_merge;
use function is_array;
use function strlen;
use function substr;

/**
 * Class SmartyTemplateCompiler
 *
 * @package    Smarty
 * @subpackage Compiler
 */
class Template extends BaseCompiler {

	/**
	 * counter for prefix variable number
	 *
	 * @var int
	 */
	public static $prefixVariableNumber = 0;

	/**
	 * Parser object
	 *
	 * @var \Smarty\Parser\TemplateParser
	 */
	public $parser = null;

	/**
	 * hash for nocache sections
	 *
	 * @var mixed
	 */
	public $nocache_hash = null;

	/**
	 * suppress generation of nocache code
	 *
	 * @var bool
	 */
	public $suppressNocacheProcessing = false;

	/**
	 * caching enabled (copied from template object)
	 *
	 * @var int
	 */
	public $caching = 0;

	/**
	 * tag stack
	 *
	 * @var array
	 */
	public $_tag_stack = [];

	/**
	 * tag stack count
	 *
	 * @var array
	 */
	public $_tag_stack_count = [];

	/**
	 * current template
	 *
	 * @var \Smarty\Template
	 */
	public $template = null;

	/**
	 * merged included sub template data
	 *
	 * @var array
	 */
	public $mergedSubTemplatesData = [];

	/**
	 * merged sub template code
	 *
	 * @var array
	 */
	public $mergedSubTemplatesCode = [];

	/**
	 * source line offset for error messages
	 *
	 * @var int
	 */
	public $trace_line_offset = 0;

	/**
	 * trace uid
	 *
	 * @var string
	 */
	public $trace_uid = '';

	/**
	 * trace file path
	 *
	 * @var string
	 */
	public $trace_filepath = '';

	/**
	 * saved preprocessed modifier list
	 *
	 * @var mixed
	 */
	public $default_modifier_list = null;

	/**
	 * force compilation of complete template as nocache
	 *
	 * @var boolean
	 */
	public $forceNocache = false;

	/**
	 * Template functions
	 *
	 * @var array
	 */
	public $tpl_function = [];

	/**
	 * compiled template or block function code
	 *
	 * @var string
	 */
	public $blockOrFunctionCode = '';

	/**
	 * flags for used modifier plugins
	 *
	 * @var array
	 */
	public $modifier_plugins = [];

	/**
	 * parent compiler object for merged subtemplates and template functions
	 *
	 * @var \Smarty\Compiler\Template
	 */
	public $parent_compiler = null;

	/**
	 * Flag true when compiling nocache section
	 *
	 * @var bool
	 */
	public $nocache = false;

	/**
	 * Flag true when tag is compiled as nocache
	 *
	 * @var bool
	 */
	public $tag_nocache = false;

	/**
	 * Compiled tag prefix code
	 *
	 * @var array
	 */
	public $prefix_code = [];

	/**
	 * Prefix code  stack
	 *
	 * @var array
	 */
	public $prefixCodeStack = [];

	/**
	 * Tag has compiled code
	 *
	 * @var bool
	 */
	public $has_code = false;

	/**
	 * A variable string was compiled
	 *
	 * @var bool
	 */
	public $has_variable_string = false;

	/**
	 * Stack for {setfilter} {/setfilter}
	 *
	 * @var array
	 */
	public $variable_filter_stack = [];

	/**
	 * variable filters for {setfilter} {/setfilter}
	 *
	 * @var array
	 */
	public $variable_filters = [];

	/**
	 * Nesting count of looping tags like {foreach}, {for}, {section}, {while}
	 *
	 * @var int
	 */
	public $loopNesting = 0;

	/**
	 * Strip preg pattern
	 *
	 * @var string
	 */
	public $stripRegEx = '![\t ]*[\r\n]+[\t ]*!';

	/**
	 * General storage area for tag compiler plugins
	 *
	 * @var array
	 */
	public $_cache = array();

	/**
	 * Lexer preg pattern for left delimiter
	 *
	 * @var string
	 */
	private $ldelPreg = '[{]';

	/**
	 * Lexer preg pattern for right delimiter
	 *
	 * @var string
	 */
	private $rdelPreg = '[}]';

	/**
	 * Length of right delimiter
	 *
	 * @var int
	 */
	private $rdelLength = 0;

	/**
	 * Length of left delimiter
	 *
	 * @var int
	 */
	private $ldelLength = 0;

	/**
	 * Lexer preg pattern for user literals
	 *
	 * @var string
	 */
	private $literalPreg = '';

	/**
	 * array of callbacks called when the normal compile process of template is finished
	 *
	 * @var array
	 */
	public $postCompileCallbacks = [];

	/**
	 * prefix code
	 *
	 * @var string
	 */
	public $prefixCompiledCode = '';

	/**
	 * postfix code
	 *
	 * @var string
	 */
	public $postfixCompiledCode = '';

	/**
	 * Initialize compiler
	 *
	 * @param Smarty $smarty global instance
	 */
	public function __construct(Smarty $smarty) {
		$this->smarty = $smarty;
		$this->nocache_hash = str_replace(
			[
				'.',
				',',
			],
			'_',
			uniqid(mt_rand(), true)
		);
	}

	/**
	 * Method to compile a Smarty template
	 *
	 * @param Smarty\Template $template template object to compile
	 * @param null $nocache true is shall be compiled in nocache mode
	 * @param null|Template $parent_compiler
	 *
	 * @return bool true if compiling succeeded, false if it failed
	 * @throws Exception
	 */
	public function compileTemplate(
		\Smarty\Template         $template,
		                                 $nocache = null,
		\Smarty\Compiler\Template $parent_compiler = null
	) {
		// get code frame of compiled template
		$_compiled_code = $template->createCodeFrame(
			$this->compileTemplateSource(
				$template,
				$nocache,
				$parent_compiler
			),
			$this->smarty->runPostFilters($this->blockOrFunctionCode, $this->template) .
			join('', $this->mergedSubTemplatesCode),
			false,
			$this
		);
		return $_compiled_code;
	}

	/**
	 * Compile template source and run optional post filter
	 *
	 * @param \Smarty\Template $template
	 * @param null|bool $nocache flag if template must be compiled in nocache mode
	 * @param \Smarty\Compiler\Template $parent_compiler
	 *
	 * @return string
	 * @throws \Exception
	 */
	public function compileTemplateSource(
		\Smarty\Template         $template,
		                                 $nocache = null,
		\Smarty\Compiler\Template $parent_compiler = null
	) {
		try {
			// save template object in compiler class
			$this->template = $template;
			if ($this->smarty->debugging) {
				if (!isset($this->smarty->_debug)) {
					$this->smarty->_debug = new \Smarty\Debug();
				}
				$this->smarty->_debug->start_compile($this->template);
			}
			$this->parent_compiler = $parent_compiler ? $parent_compiler : $this;
			$nocache = isset($nocache) ? $nocache : false;
			if (empty($template->compiled->nocache_hash)) {
				$template->compiled->nocache_hash = $this->nocache_hash;
			} else {
				$this->nocache_hash = $template->compiled->nocache_hash;
			}
			$this->caching = $template->caching;
			// flag for nocache sections
			$this->nocache = $nocache;
			$this->tag_nocache = false;
			// reset has nocache code flag
			$this->template->compiled->has_nocache_code = false;
			$this->has_variable_string = false;
			$this->prefix_code = [];
			// add file dependency
			if ($this->smarty->merge_compiled_includes || $this->template->source->handler->checkTimestamps()) {
				$this->parent_compiler->template->compiled->file_dependency[$this->template->source->uid] =
					[
						$this->template->source->filepath,
						$this->template->source->getTimeStamp(),
						$this->template->source->type,
					];
			}
			$this->smarty->_current_file = $this->template->source->filepath;
			// get template source
			if (!empty($this->template->source->components)) {
				// we have array of inheritance templates by extends: resource
				// generate corresponding source code sequence
				$_content =
					ExtendsTag::extendsSourceArrayCode($this->template);
			} else {
				// get template source
				$_content = $this->template->source->getContent();
			}
			$_compiled_code = $this->smarty->runPostFilters(
				$this->doCompile(
					$this->smarty->runPreFilters($_content, $this->template),
					true
				),
				$this->template
			);
		} catch (\Exception $e) {
			if ($this->smarty->debugging) {
				$this->smarty->_debug->end_compile($this->template);
			}
			$this->_tag_stack = [];
			// free memory
			$this->parent_compiler = null;
			$this->template = null;
			$this->parser = null;
			throw $e;
		}
		if ($this->smarty->debugging) {
			$this->smarty->_debug->end_compile($this->template);
		}
		$this->parent_compiler = null;
		$this->parser = null;
		return $_compiled_code;
	}

	/**
	 * Compile Tag
	 * This is a call back from the lexer/parser
	 *
	 * Save current prefix code
	 * Compile tag
	 * Merge tag prefix code with saved one
	 * (required nested tags in attributes)
	 *
	 * @param string $tag tag name
	 * @param array $args array with tag attributes
	 * @param array $parameter array with compilation parameter
	 *
	 * @return string compiled code
	 * @throws Exception
	 * @throws CompilerException
	 */
	public function compileTag($tag, $args, $parameter = []) {
		$this->prefixCodeStack[] = $this->prefix_code;
		$this->prefix_code = [];
		$result = $this->compileTag2($tag, $args, $parameter);
		$this->prefix_code = array_merge($this->prefix_code, array_pop($this->prefixCodeStack));
		return $result;
	}

	/**
	 * Compiles code for modifier execution
	 *
	 * @param $modifierlist
	 * @param $value
	 *
	 * @return string compiled code
	 * @throws CompilerException
	 * @throws Exception
	 */
	public function compileModifier($modifierlist, $value) {
		return (new ModifierCompiler())->compile([], $this, ['modifierlist' => $modifierlist, 'value' => $value]);
	}

	/**
	 * compile variable
	 *
	 * @param string $variable
	 *
	 * @return string
	 */
	public function compileVariable($variable) {
		if (!strpos($variable, '(')) {
			// not a variable variable
			$var = trim($variable, '\'');
			$this->tag_nocache = $this->tag_nocache |
				$this->template->_getVariable(
					$var,
					null,
					true,
					false
				)->nocache;
		}
		return '$_smarty_tpl->tpl_vars[' . $variable . ']->value';
	}

	/**
	 * compile config variable
	 *
	 * @param string $variable
	 *
	 * @return string
	 */
	public function compileConfigVariable($variable) {
		// return '$_smarty_tpl->config_vars[' . $variable . ']';
		return '$_smarty_tpl->getConfigVariable(' . $variable . ')';
	}

	/**
	 * compile PHP function call
	 *
	 * @param string $name
	 * @param array $parameter
	 *
	 * @return string
	 * @throws \Smarty\CompilerException
	 */
	public function compilePHPFunctionCall($name, $parameter) {
		if (!$this->smarty->security_policy || $this->smarty->security_policy->isTrustedPhpFunction($name, $this)) {
			if (strcasecmp($name, 'isset') === 0 || strcasecmp($name, 'empty') === 0
				|| strcasecmp($name, 'array') === 0 || is_callable($name)
			) {
				$func_name = smarty_strtolower_ascii($name);

				if ($func_name === 'isset') {
					if (count($parameter) === 0) {
						$this->trigger_template_error('Illegal number of parameter in "isset()"');
					}

					$pa = [];
					foreach ($parameter as $p) {
						$pa[] = $this->syntaxMatchesVariable($p) ? 'isset(' . $p . ')' : '(' . $p . ' !== null )';
					}
					return '(' . implode(' && ', $pa) . ')';

				} elseif (in_array(
					$func_name,
					[
						'empty',
						'reset',
						'current',
						'end',
						'prev',
						'next',
					]
				)
				) {
					if (count($parameter) !== 1) {
						$this->trigger_template_error("Illegal number of parameter in '{$func_name()}'");
					}
					if ($func_name === 'empty') {
						return $func_name . '(' .
							str_replace("')->value", "',null,true,false)->value", $parameter[0]) . ')';
					} else {
						return $func_name . '(' . $parameter[0] . ')';
					}
				} else {
					return $name . '(' . implode(',', $parameter) . ')';
				}
			} else {
				$this->trigger_template_error("unknown function '{$name}'");
			}
		}
	}

	/**
	 * Determines whether the passed string represents a valid (PHP) variable.
	 * This is important, because `isset()` only works on variables and `empty()` can only be passed
	 * a variable prior to php5.5
	 *
	 * @param $string
	 *
	 * @return bool
	 */
	private function syntaxMatchesVariable($string) {
		static $regex_pattern = '/^\$[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*((->)[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*|\[.*]*\])*$/';
		return 1 === preg_match($regex_pattern, trim($string));
	}

	/**
	 * This method is called from parser to process a text content section if strip is enabled
	 * - remove text from inheritance child templates as they may generate output
	 *
	 * @param string $text
	 *
	 * @return string
	 */
	public function processText($text) {

		if (strpos($text, '<') === false) {
			return preg_replace($this->stripRegEx, '', $text);
		}

		$store = [];
		$_store = 0;

		// capture html elements not to be messed with
		$_offset = 0;
		if (preg_match_all(
			'#(<script[^>]*>.*?</script[^>]*>)|(<textarea[^>]*>.*?</textarea[^>]*>)|(<pre[^>]*>.*?</pre[^>]*>)#is',
			$text,
			$matches,
			PREG_OFFSET_CAPTURE | PREG_SET_ORDER
		)
		) {
			foreach ($matches as $match) {
				$store[] = $match[0][0];
				$_length = strlen($match[0][0]);
				$replace = '@!@SMARTY:' . $_store . ':SMARTY@!@';
				$text = substr_replace($text, $replace, $match[0][1] - $_offset, $_length);
				$_offset += $_length - strlen($replace);
				$_store++;
			}
		}
		$expressions = [// replace multiple spaces between tags by a single space
			'#(:SMARTY@!@|>)[\040\011]+(?=@!@SMARTY:|<)#s' => '\1 \2',
			// remove newline between tags
			'#(:SMARTY@!@|>)[\040\011]*[\n]\s*(?=@!@SMARTY:|<)#s' => '\1\2',
			// remove multiple spaces between attributes (but not in attribute values!)
			'#(([a-z0-9]\s*=\s*("[^"]*?")|(\'[^\']*?\'))|<[a-z0-9_]+)\s+([a-z/>])#is' => '\1 \5',
			'#>[\040\011]+$#Ss' => '> ',
			'#>[\040\011]*[\n]\s*$#Ss' => '>',
			$this->stripRegEx => '',
		];
		$text = preg_replace(array_keys($expressions), array_values($expressions), $text);
		$_offset = 0;
		if (preg_match_all(
			'#@!@SMARTY:([0-9]+):SMARTY@!@#is',
			$text,
			$matches,
			PREG_OFFSET_CAPTURE | PREG_SET_ORDER
		)
		) {
			foreach ($matches as $match) {
				$_length = strlen($match[0][0]);
				$replace = $store[$match[1][0]];
				$text = substr_replace($text, $replace, $match[0][1] + $_offset, $_length);
				$_offset += strlen($replace) - $_length;
				$_store++;
			}
		}
		return $text;
	}

	/**
	 * lazy loads internal compile plugin for tag compile objects cached for reuse.
	 *
	 * class name format:  \Smarty\Compile\TagName
	 *
	 * @param string $tag tag name
	 *
	 * @return ?\Smarty\Compile\Tag\TagCompilerInterface tag compiler object or null if not found or untrusted by security policy
	 */
	public function getTagCompiler($tag): ?\Smarty\Compile\Tag\TagCompilerInterface {

		if (isset($this->smarty->security_policy) && !$this->smarty->security_policy->isTrustedTag($tag, $this)) {
			return null;
		}

		foreach ($this->smarty->getExtensions() as $extension) {
			if ($compiler = $extension->getTagCompiler($tag)) {
				return $compiler;
			}
		}

		return null;
	}

	/**
	 * lazy loads internal compile plugin for modifier compile objects cached for reuse.
	 *
	 * @param string $modifier tag name
	 *
	 * @return bool|\Smarty\Compile\Modifier\ModifierCompilerInterface tag compiler object or false if not found or untrusted by security policy
	 */
	public function getModifierCompiler($modifier) {

		if (isset($this->smarty->security_policy) && !$this->smarty->security_policy->isTrustedModifier($modifier, $this)) {
			return false;
		}

		foreach ($this->smarty->getExtensions() as $extension) {
			if ($modifierCompiler = $extension->getModifierCompiler($modifier)) {
				return $modifierCompiler;
			}
		}

		return false;
	}

	/**
	 * Check for plugins by default plugin handler
	 *
	 * @param string $tag name of tag
	 * @param string $plugin_type type of plugin
	 *
	 * @return callback|null
	 * @throws \Smarty\CompilerException
	 */
	public function getPluginFromDefaultHandler($tag, $plugin_type): ?callback {

		$defaultPluginHandlerFunc = $this->smarty->getDefaultPluginHandlerFunc();

		if (!is_callable($defaultPluginHandlerFunc)) {
			return null;
		}
		
		
		$callback = null;
		$script = null;
		$cacheable = true;

		$result = call_user_func_array(
			$defaultPluginHandlerFunc,
			[
				$tag,
				$plugin_type,
				null, // This used to pass $this->template, but this parameter has been removed in 5.0
				&$callback,
				&$script,
				&$cacheable,
			]
		);
		if ($result) {
			$this->tag_nocache = $this->tag_nocache || !$cacheable;
			if ($script !== null) {
				if (is_file($script)) {
					include_once $script;
				} else {
					$this->trigger_template_error("Default plugin handler: Returned script file '{$script}' for '{$tag}' not found");
				}
			}
			if (is_callable($callback)) {
				return $callback;
			} else {
				$this->trigger_template_error("Default plugin handler: Returned callback for '{$tag}' not callable");
			}
		}
		return null;
	}

	/**
	 * Append code segments and remove unneeded ?> <?php transitions
	 *
	 * @param string $left
	 * @param string $right
	 *
	 * @return string
	 */
	public function appendCode($left, $right) {
		if (preg_match('/\s*\?>\s?$/D', $left) && preg_match('/^<\?php\s+/', $right)) {
			$left = preg_replace('/\s*\?>\s?$/D', "\n", $left);
			$left .= preg_replace('/^<\?php\s+/', '', $right);
		} else {
			$left .= $right;
		}
		return $left;
	}

	/**
	 * Inject inline code for nocache template sections
	 * This method gets the content of each template element from the parser.
	 * If the content is compiled code and it should be not cached the code is injected
	 * into the rendered output.
	 *
	 * @param string $content content of template element
	 * @param boolean $is_code true if content is compiled code
	 *
	 * @return string  content
	 */
	public function processNocacheCode($content, $is_code) {
		// If the template is not evaluated and we have a nocache section and or a nocache tag
		if ($is_code && !empty($content)) {
			// generate replacement code
			if ((!($this->template->source->handler->recompiled) || $this->forceNocache) && $this->caching
				&& !$this->suppressNocacheProcessing && ($this->nocache || $this->tag_nocache)
			) {
				$this->template->compiled->has_nocache_code = true;
				$_output = addcslashes($content, '\'\\');
				$_output = str_replace('^#^', '\'', $_output);
				$_output =
					"<?php echo '/*%%SmartyNocache:{$this->nocache_hash}%%*/{$_output}/*/%%SmartyNocache:{$this->nocache_hash}%%*/';?>\n";
			} else {
				$_output = $content;
			}
		} else {
			$_output = $content;
		}
		$this->modifier_plugins = [];
		$this->suppressNocacheProcessing = false;
		$this->tag_nocache = false;
		return $_output;
	}

	/**
	 * Get Id
	 *
	 * @param string $input
	 *
	 * @return bool|string
	 */
	public function getId($input) {
		if (preg_match('~^([\'"]*)([0-9]*[a-zA-Z_]\w*)\1$~', $input, $match)) {
			return $match[2];
		}
		return false;
	}

	/**
	 * Get variable name from string
	 *
	 * @param string $input
	 *
	 * @return bool|string
	 */
	public function getVariableName($input) {
		if (preg_match('~^[$]_smarty_tpl->tpl_vars\[[\'"]*([0-9]*[a-zA-Z_]\w*)[\'"]*\]->value$~', $input, $match)) {
			return $match[1];
		}
		return false;
	}

	/**
	 * Set nocache flag in variable or create new variable
	 *
	 * @param string $varName
	 */
	public function setNocacheInVariable($varName) {
		// create nocache var to make it know for further compiling
		if ($_var = $this->getId($varName)) {
			if (isset($this->template->tpl_vars[$_var])) {
				$this->template->tpl_vars[$_var] = clone $this->template->tpl_vars[$_var];
				$this->template->tpl_vars[$_var]->nocache = true;
			} else {
				$this->template->tpl_vars[$_var] = new \Smarty\Variable(null, true);
			}
		}
	}

	/**
	 * @param array $_attr tag attributes
	 * @param array $validScopes
	 *
	 * @return int|string
	 * @throws \Smarty\CompilerException
	 */
	public function convertScope($_attr, $validScopes) {
		$_scope = 0;
		if (isset($_attr['scope'])) {
			$_scopeName = trim($_attr['scope'], '\'"');
			if (is_numeric($_scopeName) && in_array($_scopeName, $validScopes)) {
				$_scope = $_scopeName;
			} elseif (is_string($_scopeName)) {
				$_scopeName = trim($_scopeName, '\'"');
				$_scope = isset($validScopes[$_scopeName]) ? $validScopes[$_scopeName] : false;
			} else {
				$_scope = false;
			}
			if ($_scope === false) {
				$err = var_export($_scopeName, true);
				$this->trigger_template_error("illegal value '{$err}' for \"scope\" attribute", null, true);
			}
		}
		return $_scope;
	}

	/**
	 * Generate nocache code string
	 *
	 * @param string $code PHP code
	 *
	 * @return string
	 */
	public function makeNocacheCode($code) {
		return "echo '/*%%SmartyNocache:{$this->nocache_hash}%%*/<?php " .
			str_replace('^#^', '\'', addcslashes($code, '\'\\')) .
			"?>/*/%%SmartyNocache:{$this->nocache_hash}%%*/';\n";
	}

	/**
	 * display compiler error messages without dying
	 * If parameter $args is empty it is a parser detected syntax error.
	 * In this case the parser is called to obtain information about expected tokens.
	 * If parameter $args contains a string this is used as error message
	 *
	 * @param string $args individual error message or null
	 * @param string $line line-number
	 * @param null|bool $tagline if true the line number of last tag
	 *
	 * @throws \Smarty\CompilerException when an unexpected token is found
	 */
	public function trigger_template_error($args = null, $line = null, $tagline = null) {
		$lex = $this->parser->lex;
		if ($tagline === true) {
			// get line number of Tag
			$line = $lex->taglineno;
		} elseif (!isset($line)) {
			// get template source line which has error
			$line = $lex->line;
		} else {
			$line = (int)$line;
		}
		if (in_array(
			$this->template->source->type,
			[
				'eval',
				'string',
			]
		)
		) {
			$templateName = $this->template->source->type . ':' . trim(
					preg_replace(
						'![\t\r\n]+!',
						' ',
						strlen($lex->data) > 40 ?
							substr($lex->data, 0, 40) .
							'...' : $lex->data
					)
				);
		} else {
			$templateName = $this->template->source->type . ':' . $this->template->source->filepath;
		}
		//        $line += $this->trace_line_offset;
		$match = preg_split("/\n/", $lex->data);
		$error_text =
			'Syntax error in template "' . (empty($this->trace_filepath) ? $templateName : $this->trace_filepath) .
			'"  on line ' . ($line + $this->trace_line_offset) . ' "' .
			trim(preg_replace('![\t\r\n]+!', ' ', $match[$line - 1])) . '" ';
		if (isset($args)) {
			// individual error message
			$error_text .= $args;
		} else {
			$expect = [];
			// expected token from parser
			$error_text .= ' - Unexpected "' . $lex->value . '"';
			if (count($this->parser->yy_get_expected_tokens($this->parser->yymajor)) <= 4) {
				foreach ($this->parser->yy_get_expected_tokens($this->parser->yymajor) as $token) {
					$exp_token = $this->parser->yyTokenName[$token];
					if (isset($lex->smarty_token_names[$exp_token])) {
						// token type from lexer
						$expect[] = '"' . $lex->smarty_token_names[$exp_token] . '"';
					} else {
						// otherwise internal token name
						$expect[] = $this->parser->yyTokenName[$token];
					}
				}
				$error_text .= ', expected one of: ' . implode(' , ', $expect);
			}
		}
		if ($this->smarty->_parserdebug) {
			$this->parser->errorRunDown();
			echo ob_get_clean();
			flush();
		}
		$e = new CompilerException(
			$error_text,
			0,
			$this->template->source->filepath,
			$line
		);
		$e->source = trim(preg_replace('![\t\r\n]+!', ' ', $match[$line - 1]));
		$e->desc = $args;
		$e->template = $this->template->source->filepath;
		throw $e;
	}

	/**
	 * Return var_export() value with all white spaces removed
	 *
	 * @param mixed $value
	 *
	 * @return string
	 */
	public function getVarExport($value) {
		return preg_replace('/\s/', '', var_export($value, true));
	}

	/**
	 *  enter double quoted string
	 *  - save tag stack count
	 */
	public function enterDoubleQuote() {
		array_push($this->_tag_stack_count, $this->getTagStackCount());
	}

	/**
	 * Return tag stack count
	 *
	 * @return int
	 */
	public function getTagStackCount() {
		return count($this->_tag_stack);
	}

	/**
	 * @param $lexerPreg
	 *
	 * @return mixed
	 */
	public function replaceDelimiter($lexerPreg) {
		return str_replace(
			['SMARTYldel', 'SMARTYliteral', 'SMARTYrdel', 'SMARTYautoliteral', 'SMARTYal'],
			[
				$this->ldelPreg, $this->literalPreg, $this->rdelPreg,
				$this->smarty->getAutoLiteral() ? '{1,}' : '{9}',
				$this->smarty->getAutoLiteral() ? '' : '\\s*',
			],
			$lexerPreg
		);
	}

	/**
	 * Build lexer regular expressions for left and right delimiter and user defined literals
	 */
	public function initDelimiterPreg() {
		$ldel = $this->smarty->getLeftDelimiter();
		$this->ldelLength = strlen($ldel);
		$this->ldelPreg = '';
		foreach (str_split($ldel, 1) as $chr) {
			$this->ldelPreg .= '[' . preg_quote($chr, '/') . ']';
		}
		$rdel = $this->smarty->getRightDelimiter();
		$this->rdelLength = strlen($rdel);
		$this->rdelPreg = '';
		foreach (str_split($rdel, 1) as $chr) {
			$this->rdelPreg .= '[' . preg_quote($chr, '/') . ']';
		}
		$literals = $this->smarty->getLiterals();
		if (!empty($literals)) {
			foreach ($literals as $key => $literal) {
				$literalPreg = '';
				foreach (str_split($literal, 1) as $chr) {
					$literalPreg .= '[' . preg_quote($chr, '/') . ']';
				}
				$literals[$key] = $literalPreg;
			}
			$this->literalPreg = '|' . implode('|', $literals);
		} else {
			$this->literalPreg = '';
		}
	}

	/**
	 *  leave double quoted string
	 *  - throw exception if block in string was not closed
	 *
	 * @throws \Smarty\CompilerException
	 */
	public function leaveDoubleQuote() {
		if (array_pop($this->_tag_stack_count) !== $this->getTagStackCount()) {
			$tag = $this->getOpenBlockTag();
			$this->trigger_template_error(
				"unclosed '{{$tag}}' in doubled quoted string",
				null,
				true
			);
		}
	}

	/**
	 * Get left delimiter preg
	 *
	 * @return string
	 */
	public function getLdelPreg() {
		return $this->ldelPreg;
	}

	/**
	 * Get right delimiter preg
	 *
	 * @return string
	 */
	public function getRdelPreg() {
		return $this->rdelPreg;
	}

	/**
	 * Get length of left delimiter
	 *
	 * @return int
	 */
	public function getLdelLength() {
		return $this->ldelLength;
	}

	/**
	 * Get length of right delimiter
	 *
	 * @return int
	 */
	public function getRdelLength() {
		return $this->rdelLength;
	}

	/**
	 * Get name of current open block tag
	 *
	 * @return string|boolean
	 */
	public function getOpenBlockTag() {
		$tagCount = $this->getTagStackCount();
		if ($tagCount) {
			return $this->_tag_stack[$tagCount - 1][0];
		} else {
			return false;
		}
	}

	/**
	 * Check if $value contains variable elements
	 *
	 * @param mixed $value
	 *
	 * @return bool|int
	 */
	public function isVariable($value) {
		if (is_string($value)) {
			return preg_match('/[$(]/', $value);
		}
		if (is_bool($value) || is_numeric($value)) {
			return false;
		}
		if (is_array($value)) {
			foreach ($value as $k => $v) {
				if ($this->isVariable($k) || $this->isVariable($v)) {
					return true;
				}
			}
			return false;
		}
		return false;
	}

	/**
	 * Get new prefix variable name
	 *
	 * @return string
	 */
	public function getNewPrefixVariable() {
		++self::$prefixVariableNumber;
		return $this->getPrefixVariable();
	}

	/**
	 * Get current prefix variable name
	 *
	 * @return string
	 */
	public function getPrefixVariable() {
		return '$_prefixVariable' . self::$prefixVariableNumber;
	}

	/**
	 * append  code to prefix buffer
	 *
	 * @param string $code
	 */
	public function appendPrefixCode($code) {
		$this->prefix_code[] = $code;
	}

	/**
	 * get prefix code string
	 *
	 * @return string
	 */
	public function getPrefixCode() {
		$code = '';
		$prefixArray = array_merge($this->prefix_code, array_pop($this->prefixCodeStack));
		$this->prefixCodeStack[] = [];
		foreach ($prefixArray as $c) {
			$code = $this->appendCode($code, $c);
		}
		$this->prefix_code = [];
		return $code;
	}

	public function cStyleComment($string) {
		return '/*' . str_replace('*/', '* /', $string) . '*/';
	}

	private function argsContainNocache(array $args): bool {
		foreach ($args as $arg) {
			if (!is_array($arg)) {
				if ($arg === "'nocache'" || $arg === 'nocache') {
					return true;
				}
			} else {
				foreach ($arg as $k => $v) {
					if (($k === "'nocache'" || $k === 'nocache') && (trim($v, "'\" ") === 'true')) {
						return true;
					}
				}
			}
		}
		return false;
	}

	/**
	 * Compile Tag
	 *
	 * @param string $tag tag name
	 * @param array $args array with tag attributes
	 * @param array $parameter array with compilation parameter
	 *
	 * @return string compiled code
	 * @throws Exception
	 * @throws CompilerException
	 */
	private function compileTag2($tag, $args, $parameter) {
		// $args contains the attributes parsed and compiled by the lexer/parser
		// assume that tag does compile into code, but creates no HTML output
		$this->has_code = true;

		// check nocache option flag
		if ($this->argsContainNocache($args)) {
			$this->tag_nocache = true;
		}

		// compile built-in tags
		if ($tagCompiler = $this->getTagCompiler($tag)) {
			if (!isset($this->smarty->security_policy) || $this->smarty->security_policy->isTrustedTag($tag, $this)) {
				$this->tag_nocache = !$tagCompiler->isCacheable();
				$_output = $tagCompiler->compile($args, $this, $parameter);
				return $this->has_code ? $_output : null;
			}
		}

		// template defined by {template} tag
		if ($this->canCompileTemplateCall($tag)) {
			$args['_attr']['name'] = "'{$tag}'";
			$tagCompiler = $this->getTagCompiler($tag);
			// compile this tag
			$_output = $tagCompiler === null ? false : $tagCompiler->compile($args, $this, $parameter);
			return $this->has_code ? $_output : null;
		}

//      @TODO: This code was in here in v4, but it might not do anything (anymore)
//		foreach ($args['_attr'] ?? [] as $attribute) {
//			if (is_array($attribute)) {
//				$args = array_merge($args, $attribute);
//			}
//		}

		// remaining tastes: (object-)function, (object-function-)block, custom-compiler
		// opening and closing tags for these are handled with the same handler
		$base_tag = $this->getBaseTag($tag);

		// check if tag is a registered object
		if (isset($this->smarty->registered_objects[$base_tag]) && isset($parameter['object_method'])) {
			return $this->compileRegisteredObjectMethodCall($base_tag, $args, $parameter, $tag);
		}

		// check if tag is a function
		if ($this->smarty->getFunctionHandler($base_tag)) {
			if (!isset($this->smarty->security_policy) || $this->smarty->security_policy->isTrustedTag($base_tag, $this)) {
				$compiler = new FunctionCallCompiler();
				return $compiler->compile($args, $this, $parameter, $tag, $base_tag);
			}
		}

		// check if tag is a block
		if ($this->smarty->getBlockHandler($base_tag)) {
			if (!isset($this->smarty->security_policy) || $this->smarty->security_policy->isTrustedTag($base_tag, $this)) {
				$compiler = new BlockCompiler();
				return $compiler->compile($args, $this, $parameter, $tag, $base_tag);
			}
		}

		// the default plugin handler is a handler of last resort, it may also handle not specifically registered tags.
		if ($callback = $this->getPluginFromDefaultHandler($tag, Smarty::PLUGIN_COMPILER)) {
			$tagCompiler = new \Smarty\Compile\Tag\BCPluginWrapper($callback);
			$new_args = [];
			foreach ($args as $key => $mixed) {
				if (is_array($mixed)) {
					$new_args = array_merge($new_args, $mixed);
				} else {
					$new_args[$key] = $mixed;
				}
			}
			return $tagCompiler->compile($new_args, $this, $parameter);
		}

		if ($this->getPluginFromDefaultHandler($tag, Smarty::PLUGIN_FUNCTION)) {
			$compiler = new FunctionCallCompiler();
			return $compiler->compile($args, $this, $parameter, $tag, $base_tag);
		}

		if ($this->getPluginFromDefaultHandler($tag, Smarty::PLUGIN_BLOCK)) {
			$compiler = new BlockCompiler();
			return $compiler->compile($args, $this, $parameter, $tag, $base_tag);
		}

		$this->trigger_template_error("unknown tag '{$tag}'", null, true);
	}

	private function getBaseTag($tag) {
		if (strlen($tag) < 6 || substr($tag, -5) !== 'close') {
			return $tag;
		} else {
			return substr($tag, 0, -5);
		}
	}

	/**
	 * method to compile a Smarty template
	 *
	 * @param mixed $_content template source
	 * @param bool $isTemplateSource
	 *
	 * @return bool true if compiling succeeded, false if it failed
	 * @throws \Smarty\CompilerException
	 */
	protected function doCompile($_content, $isTemplateSource = false) {
		/* here is where the compiling takes place. Smarty
		  tags in the templates are replaces with PHP code,
		  then written to compiled files. */
		// init the lexer/parser to compile the template
		$this->parser = new TemplateParser(
				new TemplateLexer(
					str_replace(
						[
							"\r\n",
							"\r",
						],
						"\n",
						$_content
					),
					$this
				),
				$this
			);
		if ($isTemplateSource && $this->template->caching) {
			$this->parser->insertPhpCode("<?php\n\$_smarty_tpl->compiled->nocache_hash = '{$this->nocache_hash}';\n?>\n");
		}
		if (function_exists('mb_internal_encoding')
			&& function_exists('ini_get')
			&& ((int)ini_get('mbstring.func_overload')) & 2
		) {
			$mbEncoding = mb_internal_encoding();
			mb_internal_encoding('ASCII');
		} else {
			$mbEncoding = null;
		}
		if ($this->smarty->_parserdebug) {
			$this->parser->PrintTrace();
			$this->parser->lex->PrintTrace();
		}
		// get tokens from lexer and parse them
		while ($this->parser->lex->yylex()) {
			if ($this->smarty->_parserdebug) {
				echo "Line {$this->parser->lex->line} Parsing  {$this->parser->yyTokenName[$this->parser->lex->token]} Token " .
					$this->parser->lex->value;
			}
			$this->parser->doParse($this->parser->lex->token, $this->parser->lex->value);
		}
		// finish parsing process
		$this->parser->doParse(0, 0);
		if ($mbEncoding) {
			mb_internal_encoding($mbEncoding);
		}
		// check for unclosed tags
		if (count($this->_tag_stack) > 0) {
			// get stacked info
			[$openTag, $_data] = array_pop($this->_tag_stack);
			$this->trigger_template_error(
				"unclosed {$this->smarty->left_delimiter}" . $openTag .
				"{$this->smarty->right_delimiter} tag"
			);
		}
		// call post compile callbacks
		foreach ($this->postCompileCallbacks as $cb) {
			$parameter = $cb;
			$parameter[0] = $this;
			call_user_func_array($cb[0], $parameter);
		}
		// return compiled code
		return $this->prefixCompiledCode . $this->parser->retvalue . $this->postfixCompiledCode;
	}

	/**
	 * Register a post compile callback
	 * - when the callback is called after template compiling the compiler object will be inserted as first parameter
	 *
	 * @param callback $callback
	 * @param array $parameter optional parameter array
	 * @param string $key optional key for callback
	 * @param bool $replace if true replace existing keyed callback
	 */
	public function registerPostCompileCallback($callback, $parameter = [], $key = null, $replace = false) {
		array_unshift($parameter, $callback);
		if (isset($key)) {
			if ($replace || !isset($this->postCompileCallbacks[$key])) {
				$this->postCompileCallbacks[$key] = $parameter;
			}
		} else {
			$this->postCompileCallbacks[] = $parameter;
		}
	}

	/**
	 * Remove a post compile callback
	 *
	 * @param string $key callback key
	 */
	public function unregisterPostCompileCallback($key) {
		unset($this->postCompileCallbacks[$key]);
	}

	/**
	 * @param string $tag
	 *
	 * @return bool
	 * @throws Exception
	 */
	private function canCompileTemplateCall(string $tag): bool {
		return
			isset($this->parent_compiler->tpl_function[$tag])
			|| (
				$this->template->smarty->hasRuntime('TplFunction')
				&& ($this->template->smarty->getRuntime('TplFunction')->getTplFunction($this->template, $tag) !== false)
			);
	}

	/**
	 * @throws CompilerException
	 */
	private function compileRegisteredObjectMethodCall(string $base_tag, array $args, array $parameter, string $tag) {

		$method = $parameter['object_method'];
		$allowedAsBlockFunction = in_array($method, $this->smarty->registered_objects[$base_tag][3]);

		if ($base_tag === $tag) {
			// opening tag

			$allowedAsNormalFunction = empty($this->smarty->registered_objects[$base_tag][1])
				|| in_array($method, $this->smarty->registered_objects[$base_tag][1]);

			if ($allowedAsBlockFunction) {
				return (new ObjectMethodBlockCompiler())->compile($args, $this, $parameter, $tag, $method);
			} elseif ($allowedAsNormalFunction) {
				return (new ObjectMethodCallCompiler())->compile($args, $this, $parameter, $tag, $method);
			}

			$this->trigger_template_error(
				'not allowed method "' . $method . '" in registered object "' .
				$tag . '"',
				null,
				true
			);
		}

		// closing tag
		if ($allowedAsBlockFunction) {
			return (new ObjectMethodBlockCompiler())->compile($args, $this, $parameter, $tag, $method);
		}

		$this->trigger_template_error(
			'not allowed closing tag method "' . $method .
			'" in registered object "' . $base_tag . '"',
			null,
			true
		);
	}

}
