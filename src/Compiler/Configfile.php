<?php
/**
 * Smarty Internal Plugin Config File Compiler
 * This is the config file compiler class. It calls the lexer and parser to
 * perform the compiling.
 *


 * @author     Uwe Tews
 */

namespace Smarty\Compiler;
use Smarty\Lexer\ConfigfileLexer;
use Smarty\Parser\ConfigfileParser;
use Smarty\Smarty;
use Smarty\Template;
use Smarty\CompilerException;

/**
 * Main config file compiler class
 *


 */
class Configfile extends BaseCompiler {

	/**
	 * Lexer object
	 *
	 * @var ConfigfileLexer
	 */
	public $lex;

	/**
	 * Parser object
	 *
	 * @var ConfigfileParser
	 */
	public $parser;

	/**
	 * Smarty object
	 *
	 * @var Smarty object
	 */
	public $smarty;

	/**
	 * Smarty object
	 *
	 * @var Template object
	 */
	public $template;

	/**
	 * Compiled config data sections and variables
	 *
	 * @var array
	 */
	public $config_data = [];

	/**
	 * Initialize compiler
	 *
	 * @param Smarty $smarty global instance
	 */
	public function __construct(Smarty $smarty) {
		$this->smarty = $smarty;
		// get required plugins
		$this->smarty = $smarty;
		$this->config_data['sections'] = [];
		$this->config_data['vars'] = [];
	}

	/**
	 * Method to compile Smarty config source.
	 *
	 * @param Template $template
	 *
	 * @return bool true if compiling succeeded, false if it failed
	 * @throws \Smarty\Exception
	 */
	public function compileTemplate(Template $template) {
		$this->template = $template;
		$this->template->compiled->file_dependency[$this->template->source->uid] =
			[
				$this->template->source->filepath,
				$this->template->source->getTimeStamp(),
				$this->template->source->type,
			];
		if ($this->smarty->debugging) {
			if (!isset($this->smarty->_debug)) {
				$this->smarty->_debug = new \Smarty\Debug();
			}
			$this->smarty->_debug->start_compile($this->template);
		}
		// init the lexer/parser to compile the config file
		/* @var ConfigfileLexer $this->lex */
		$this->lex = new ConfigfileLexer(
			str_replace(
				[
					"\r\n",
					"\r",
				],
				"\n",
				$template->source->getContent()
			) . "\n",
			$this
		);
		/* @var ConfigfileParser $this->parser */
		$this->parser = new ConfigfileParser($this->lex, $this);
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
		}
		// get tokens from lexer and parse them
		while ($this->lex->yylex()) {
			if ($this->smarty->_parserdebug) {
				echo "Parsing  {$this->parser->yyTokenName[$this->lex->token]} Token {$this->lex->value} Line {$this->lex->line} \n";
			}
			$this->parser->doParse($this->lex->token, $this->lex->value);
		}
		// finish parsing process
		$this->parser->doParse(0, 0);
		if ($mbEncoding) {
			mb_internal_encoding($mbEncoding);
		}
		if ($this->smarty->debugging) {
			$this->smarty->_debug->end_compile($this->template);
		}
		// template header code
		$template_header = sprintf(
			"<?php /* Smarty version %s, created on %s\n         compiled from '%s' */ ?>\n",
			\Smarty\Smarty::SMARTY_VERSION,
			date("Y-m-d H:i:s"),
			str_replace('*/', '* /', $this->template->source->filepath)
		);
		$code = '<?php $_smarty_tpl->_loadConfigVars(' .
			var_export($this->config_data, true) . '); ?>';
		return $template_header . $this->template->createCodeFrame($code);
	}

	/**
	 * display compiler error messages without dying
	 * If parameter $args is empty it is a parser detected syntax error.
	 * In this case the parser is called to obtain information about expected tokens.
	 * If parameter $args contains a string this is used as error message
	 *
	 * @param string $args individual error message or null
	 *
	 * @throws CompilerException
	 */
	public function trigger_config_file_error($args = null) {
		// get config source line which has error
		$line = $this->lex->line;
		if (isset($args)) {
			// $line--;
		}
		$match = preg_split("/\n/", $this->lex->data);
		$error_text =
			"Syntax error in config file '{$this->template->source->filepath}' on line {$line} '{$match[$line - 1]}' ";
		if (isset($args)) {
			// individual error message
			$error_text .= $args;
		} else {
			// expected token from parser
			foreach ($this->parser->yy_get_expected_tokens($this->parser->yymajor) as $token) {
				$exp_token = $this->parser->yyTokenName[$token];
				if (isset($this->lex->smarty_token_names[$exp_token])) {
					// token type from lexer
					$expect[] = '"' . $this->lex->smarty_token_names[$exp_token] . '"';
				} else {
					// otherwise internal token name
					$expect[] = $this->parser->yyTokenName[$token];
				}
			}
			// output parser error message
			$error_text .= ' - Unexpected "' . $this->lex->value . '", expected one of: ' . implode(' , ', $expect);
		}
		throw new CompilerException($error_text);
	}
}
