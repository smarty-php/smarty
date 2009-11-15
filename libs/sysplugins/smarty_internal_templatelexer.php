<?php
/**
* Smarty Internal Plugin Templatelexer
*
* This is the lexer to break the template source into tokens 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews 
*/
/**
* Smarty Internal Plugin Templatelexer
*/
class Smarty_Internal_Templatelexer
{
    public $data;
    public $counter;
    public $token;
    public $value;
    public $node;
    public $line;
    public $state = 1;
    public $smarty_token_names = array (		// Text for parser error messages
    				'IDENTITY'	=> '===',
    				'NONEIDENTITY'	=> '!==',
    				'EQUALS'	=> '==',
    				'NOTEQUALS'	=> '!=',
    				'GREATEREQUAL' => '(>=,ge)',
    				'LESSEQUAL' => '(<=,le)',
    				'GREATERTHAN' => '(>,gt)',
    				'LESSTHAN' => '(<,lt)',
    				'MOD' => '(%,mod)',
    				'NOT'			=> '(!,not)',
    				'LAND'		=> '(&&,and)',
    				'LOR'			=> '(||,or)',
    				'LXOR'			=> 'xor',
    				'OPENP'		=> '(',
    				'CLOSEP'	=> ')',
    				'OPENB'		=> '[',
    				'CLOSEB'	=> ']',
    				'PTR'			=> '->',
    				'APTR'		=> '=>',
    				'EQUAL'		=> '=',
    				'NUMBER'	=> 'number',
    				'UNIMATH'	=> '+" , "-',
    				'MATH'		=> '*" , "/" , "%',
    				'INCDEC'	=> '++" , "--',
    				'SPACE'		=> ' ',
    				'DOLLAR'	=> '$',
    				'SEMICOLON' => ';',
    				'COLON'		=> ':',
    				'DOUBLECOLON'		=> '::',
    				'AT'		=> '@',
    				'HATCH'		=> '#',
    				'QUOTE'		=> '"',
    				'SINGLEQUOTE'		=> "'",
    				'BACKTICK'		=> '`',
    				'VERT'		=> '|',
    				'DOT'			=> '.',
    				'COMMA'		=> '","',
    				'ANDSYM'		=> '"&"',
    				'QMARK'		=> '"?"',
    				'ID'			=> 'identifier',
    				'OTHER'		=> 'text',
    				'PHP'			=> 'PHP code',
    				'LDELSLASH' => 'closing tag',
    				'COMMENT' => 'comment',
     				'LITERALEND' => 'literal close',
    				'AS' => 'as',
    				'NULL' => 'null',
    				'BOOLEAN' => 'boolean'
    				);
    				
    				
    function __construct($data,$smarty)
    {
        // set instance object
        self::instance($this); 
        $this->data = preg_replace("/(\r\n|\r|\n)/", "\n", $data);
        $this->counter = 0;
        $this->line = 1;
        $this->smarty = $smarty;
        $this->ldel = preg_quote($this->smarty->left_delimiter,'/'); 
        $this->rdel = preg_quote($this->smarty->right_delimiter,'/');
        $this->smarty_token_names['LDEL'] =	$this->smarty->left_delimiter;
        $this->smarty_token_names['RDEL'] =	$this->smarty->right_delimiter;
     }
    public static function &instance($new_instance = null)
    {
        static $instance = null;
        if (isset($new_instance) && is_object($new_instance))
            $instance = $new_instance;
        return $instance;
    } 



    private $_yy_state = 1;
    private $_yy_stack = array();

    function yylex()
    {
        return $this->{'yylex' . $this->_yy_state}();
    }

    function yypushstate($state)
    {
        array_push($this->_yy_stack, $this->_yy_state);
        $this->_yy_state = $state;
    }

    function yypopstate()
    {
        $this->_yy_state = array_pop($this->_yy_stack);
    }

    function yybegin($state)
    {
        $this->_yy_state = $state;
    }



    function yylex1()
    {
        $tokenMap = array (
              1 => 0,
              2 => 1,
              4 => 0,
              5 => 0,
              6 => 0,
              7 => 0,
              8 => 0,
              9 => 0,
              10 => 0,
              11 => 0,
              12 => 0,
              13 => 2,
              16 => 0,
            );
        if ($this->counter >= strlen($this->data)) {
            return false; // end of input
        }
        $yy_global_pattern = "/^(\\{\\})|^(".$this->ldel."\\*([\S\s]*?)\\*".$this->rdel.")|^(<\\?xml)|^(<\\?php)|^(<\\?=)|^(".$this->ldel."literal".$this->rdel.")|^(".$this->ldel."\/literal".$this->rdel.")|^(".$this->ldel."\\s{1,}\/)|^(".$this->ldel."\\s{1,})|^(".$this->ldel."\/)|^(".$this->ldel.")|^(([\S\s]*?)(".$this->ldel."|<\\?))|^([\S\s]+)/";

        do {
            if (preg_match($yy_global_pattern, substr($this->data, $this->counter), $yymatches)) {
                $yysubmatches = $yymatches;
                $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                if (!count($yymatches)) {
                    throw new Exception('Error: lexing failed because a rule matched' .
                        'an empty string.  Input "' . substr($this->data,
                        $this->counter, 5) . '... state TEXT');
                }
                next($yymatches); // skip global match
                $this->token = key($yymatches); // token number
                if ($tokenMap[$this->token]) {
                    // extract sub-patterns for passing to lex function
                    $yysubmatches = array_slice($yysubmatches, $this->token + 1,
                        $tokenMap[$this->token]);
                } else {
                    $yysubmatches = array();
                }
                $this->value = current($yymatches); // token value
                $r = $this->{'yy_r1_' . $this->token}($yysubmatches);
                if ($r === null) {
                    $this->counter += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    // accept this token
                    return true;
                } elseif ($r === true) {
                    // we have changed state
                    // process this token in the new state
                    return $this->yylex();
                } elseif ($r === false) {
                    $this->counter += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    if ($this->counter >= strlen($this->data)) {
                        return false; // end of input
                    }
                    // skip this token
                    continue;
                } else {                    $yy_yymore_patterns = array(
        1 => array(0, "^(".$this->ldel."\\*([\S\s]*?)\\*".$this->rdel.")|^(<\\?xml)|^(<\\?php)|^(<\\?=)|^(".$this->ldel."literal".$this->rdel.")|^(".$this->ldel."\/literal".$this->rdel.")|^(".$this->ldel."\\s{1,}\/)|^(".$this->ldel."\\s{1,})|^(".$this->ldel."\/)|^(".$this->ldel.")|^(([\S\s]*?)(".$this->ldel."|<\\?))|^([\S\s]+)"),
        2 => array(1, "^(<\\?xml)|^(<\\?php)|^(<\\?=)|^(".$this->ldel."literal".$this->rdel.")|^(".$this->ldel."\/literal".$this->rdel.")|^(".$this->ldel."\\s{1,}\/)|^(".$this->ldel."\\s{1,})|^(".$this->ldel."\/)|^(".$this->ldel.")|^(([\S\s]*?)(".$this->ldel."|<\\?))|^([\S\s]+)"),
        4 => array(1, "^(<\\?php)|^(<\\?=)|^(".$this->ldel."literal".$this->rdel.")|^(".$this->ldel."\/literal".$this->rdel.")|^(".$this->ldel."\\s{1,}\/)|^(".$this->ldel."\\s{1,})|^(".$this->ldel."\/)|^(".$this->ldel.")|^(([\S\s]*?)(".$this->ldel."|<\\?))|^([\S\s]+)"),
        5 => array(1, "^(<\\?=)|^(".$this->ldel."literal".$this->rdel.")|^(".$this->ldel."\/literal".$this->rdel.")|^(".$this->ldel."\\s{1,}\/)|^(".$this->ldel."\\s{1,})|^(".$this->ldel."\/)|^(".$this->ldel.")|^(([\S\s]*?)(".$this->ldel."|<\\?))|^([\S\s]+)"),
        6 => array(1, "^(".$this->ldel."literal".$this->rdel.")|^(".$this->ldel."\/literal".$this->rdel.")|^(".$this->ldel."\\s{1,}\/)|^(".$this->ldel."\\s{1,})|^(".$this->ldel."\/)|^(".$this->ldel.")|^(([\S\s]*?)(".$this->ldel."|<\\?))|^([\S\s]+)"),
        7 => array(1, "^(".$this->ldel."\/literal".$this->rdel.")|^(".$this->ldel."\\s{1,}\/)|^(".$this->ldel."\\s{1,})|^(".$this->ldel."\/)|^(".$this->ldel.")|^(([\S\s]*?)(".$this->ldel."|<\\?))|^([\S\s]+)"),
        8 => array(1, "^(".$this->ldel."\\s{1,}\/)|^(".$this->ldel."\\s{1,})|^(".$this->ldel."\/)|^(".$this->ldel.")|^(([\S\s]*?)(".$this->ldel."|<\\?))|^([\S\s]+)"),
        9 => array(1, "^(".$this->ldel."\\s{1,})|^(".$this->ldel."\/)|^(".$this->ldel.")|^(([\S\s]*?)(".$this->ldel."|<\\?))|^([\S\s]+)"),
        10 => array(1, "^(".$this->ldel."\/)|^(".$this->ldel.")|^(([\S\s]*?)(".$this->ldel."|<\\?))|^([\S\s]+)"),
        11 => array(1, "^(".$this->ldel.")|^(([\S\s]*?)(".$this->ldel."|<\\?))|^([\S\s]+)"),
        12 => array(1, "^(([\S\s]*?)(".$this->ldel."|<\\?))|^([\S\s]+)"),
        13 => array(3, "^([\S\s]+)"),
        16 => array(3, ""),
    );

                    // yymore is needed
                    do {
                        if (!strlen($yy_yymore_patterns[$this->token][1])) {
                            throw new Exception('cannot do yymore for the last token');
                        }
                        $yysubmatches = array();
                        if (preg_match('/' . $yy_yymore_patterns[$this->token][1] . '/',
                              substr($this->data, $this->counter), $yymatches)) {
                            $yysubmatches = $yymatches;
                            $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                            next($yymatches); // skip global match
                            $this->token += key($yymatches) + $yy_yymore_patterns[$this->token][0]; // token number
                            $this->value = current($yymatches); // token value
                            $this->line = substr_count($this->value, "\n");
                            if ($tokenMap[$this->token]) {
                                // extract sub-patterns for passing to lex function
                                $yysubmatches = array_slice($yysubmatches, $this->token + 1,
                                    $tokenMap[$this->token]);
                            } else {
                                $yysubmatches = array();
                            }
                        }
                    	$r = $this->{'yy_r1_' . $this->token}($yysubmatches);
                    } while ($r !== null && !is_bool($r));
			        if ($r === true) {
			            // we have changed state
			            // process this token in the new state
			            return $this->yylex();
                    } elseif ($r === false) {
                        $this->counter += strlen($this->value);
                        $this->line += substr_count($this->value, "\n");
                        if ($this->counter >= strlen($this->data)) {
                            return false; // end of input
                        }
                        // skip this token
                        continue;
			        } else {
	                    // accept
	                    $this->counter += strlen($this->value);
	                    $this->line += substr_count($this->value, "\n");
	                    return true;
			        }
                }
            } else {
                throw new Exception('Unexpected input at line' . $this->line .
                    ': ' . $this->data[$this->counter]);
            }
            break;
        } while (true);

    } // end function


    const TEXT = 1;
    function yy_r1_1($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_OTHER;
    }
    function yy_r1_2($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_COMMENT;
    }
    function yy_r1_4($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_XML;
  $this->yypushstate(self::PHP);
    }
    function yy_r1_5($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_PHP;
  $this->yypushstate(self::PHP);
    }
    function yy_r1_6($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_SHORTTAGSTART;
  $this->yypushstate(self::PHP);
    }
    function yy_r1_7($yy_subpatterns)
    {

  $this->yypushstate(self::LITERAL);
  return true;
    }
    function yy_r1_8($yy_subpatterns)
    {

  return false;  // unexspected here just ignore
    }
    function yy_r1_9($yy_subpatterns)
    {

  if ($this->smarty->auto_literal) {
     $this->token = Smarty_Internal_Templateparser::TP_OTHER;
  } else {
     $this->token = Smarty_Internal_Templateparser::TP_LDELSLASH;
     $this->yypushstate(self::SMARTY);
  }
    }
    function yy_r1_10($yy_subpatterns)
    {

  if ($this->smarty->auto_literal) {
     $this->token = Smarty_Internal_Templateparser::TP_OTHER;
  } else {
     $this->token = Smarty_Internal_Templateparser::TP_LDEL;
     $this->yypushstate(self::SMARTY);
  }
    }
    function yy_r1_11($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_LDELSLASH;
     $this->yypushstate(self::SMARTY);
    }
    function yy_r1_12($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_LDEL;
     $this->yypushstate(self::SMARTY);
    }
    function yy_r1_13($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_OTHER;
  if (substr($this->value,-2) == '<?') {
     $this->value = substr($this->value,0,-2);
  } else {
     $this->value = substr($this->value,0,-strlen($this->smarty->left_delimiter));
  }
  if (strlen($this->value) == 0) {
     return true;		// rescan
  }
    }
    function yy_r1_16($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_OTHER;
    }


    function yylex2()
    {
        $tokenMap = array (
              1 => 1,
              3 => 1,
              5 => 0,
              6 => 0,
              7 => 0,
              8 => 0,
              9 => 0,
              10 => 0,
              11 => 0,
              12 => 0,
              13 => 0,
              14 => 1,
              16 => 0,
              17 => 0,
              18 => 0,
              19 => 0,
              20 => 0,
              21 => 1,
              23 => 1,
              25 => 1,
              27 => 1,
              29 => 1,
              31 => 1,
              33 => 1,
              35 => 1,
              37 => 1,
              39 => 1,
              41 => 1,
              43 => 0,
              44 => 0,
              45 => 0,
              46 => 0,
              47 => 0,
              48 => 0,
              49 => 0,
              50 => 0,
              51 => 0,
              52 => 0,
              53 => 3,
              57 => 0,
              58 => 0,
              59 => 0,
              60 => 0,
              61 => 0,
              62 => 0,
              63 => 0,
              64 => 0,
              65 => 1,
              67 => 1,
              69 => 1,
              71 => 0,
              72 => 0,
              73 => 0,
              74 => 0,
              75 => 0,
              76 => 0,
              77 => 0,
              78 => 0,
              79 => 0,
              80 => 0,
              81 => 0,
              82 => 0,
              83 => 0,
              84 => 0,
              85 => 0,
              86 => 0,
            );
        if ($this->counter >= strlen($this->data)) {
            return false; // end of input
        }
        $yy_global_pattern = "/^((\\\\\"|\\\\'))|^(''|'([\S\s]*?)[^\\\\]')|^(".$this->ldel."literal".$this->rdel.")|^(".$this->ldel."\/literal".$this->rdel.")|^(".$this->ldel."\\s{1,}\/)|^(".$this->ldel."\\s{1,})|^(\\s{1,}".$this->rdel.")|^(".$this->ldel."\/)|^(".$this->ldel.")|^(".$this->rdel.")|^(\\s+is\\s+in\\s+)|^(\\s+(AS|as)\\s+)|^(\\s+instanceof\\s+)|^(true|false)|^(null)|^(\\s*===\\s*)|^(\\s*!==\\s*)|^(\\s*==\\s*|\\s+(EQ|eq)\\s+)|^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)/";

        do {
            if (preg_match($yy_global_pattern, substr($this->data, $this->counter), $yymatches)) {
                $yysubmatches = $yymatches;
                $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                if (!count($yymatches)) {
                    throw new Exception('Error: lexing failed because a rule matched' .
                        'an empty string.  Input "' . substr($this->data,
                        $this->counter, 5) . '... state SMARTY');
                }
                next($yymatches); // skip global match
                $this->token = key($yymatches); // token number
                if ($tokenMap[$this->token]) {
                    // extract sub-patterns for passing to lex function
                    $yysubmatches = array_slice($yysubmatches, $this->token + 1,
                        $tokenMap[$this->token]);
                } else {
                    $yysubmatches = array();
                }
                $this->value = current($yymatches); // token value
                $r = $this->{'yy_r2_' . $this->token}($yysubmatches);
                if ($r === null) {
                    $this->counter += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    // accept this token
                    return true;
                } elseif ($r === true) {
                    // we have changed state
                    // process this token in the new state
                    return $this->yylex();
                } elseif ($r === false) {
                    $this->counter += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    if ($this->counter >= strlen($this->data)) {
                        return false; // end of input
                    }
                    // skip this token
                    continue;
                } else {                    $yy_yymore_patterns = array(
        1 => array(0, "^(''|'([\S\s]*?)[^\\\\]')|^(".$this->ldel."literal".$this->rdel.")|^(".$this->ldel."\/literal".$this->rdel.")|^(".$this->ldel."\\s{1,}\/)|^(".$this->ldel."\\s{1,})|^(\\s{1,}".$this->rdel.")|^(".$this->ldel."\/)|^(".$this->ldel.")|^(".$this->rdel.")|^(\\s+is\\s+in\\s+)|^(\\s+(AS|as)\\s+)|^(\\s+instanceof\\s+)|^(true|false)|^(null)|^(\\s*===\\s*)|^(\\s*!==\\s*)|^(\\s*==\\s*|\\s+(EQ|eq)\\s+)|^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        3 => array(1, "^(".$this->ldel."literal".$this->rdel.")|^(".$this->ldel."\/literal".$this->rdel.")|^(".$this->ldel."\\s{1,}\/)|^(".$this->ldel."\\s{1,})|^(\\s{1,}".$this->rdel.")|^(".$this->ldel."\/)|^(".$this->ldel.")|^(".$this->rdel.")|^(\\s+is\\s+in\\s+)|^(\\s+(AS|as)\\s+)|^(\\s+instanceof\\s+)|^(true|false)|^(null)|^(\\s*===\\s*)|^(\\s*!==\\s*)|^(\\s*==\\s*|\\s+(EQ|eq)\\s+)|^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        5 => array(1, "^(".$this->ldel."\/literal".$this->rdel.")|^(".$this->ldel."\\s{1,}\/)|^(".$this->ldel."\\s{1,})|^(\\s{1,}".$this->rdel.")|^(".$this->ldel."\/)|^(".$this->ldel.")|^(".$this->rdel.")|^(\\s+is\\s+in\\s+)|^(\\s+(AS|as)\\s+)|^(\\s+instanceof\\s+)|^(true|false)|^(null)|^(\\s*===\\s*)|^(\\s*!==\\s*)|^(\\s*==\\s*|\\s+(EQ|eq)\\s+)|^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        6 => array(1, "^(".$this->ldel."\\s{1,}\/)|^(".$this->ldel."\\s{1,})|^(\\s{1,}".$this->rdel.")|^(".$this->ldel."\/)|^(".$this->ldel.")|^(".$this->rdel.")|^(\\s+is\\s+in\\s+)|^(\\s+(AS|as)\\s+)|^(\\s+instanceof\\s+)|^(true|false)|^(null)|^(\\s*===\\s*)|^(\\s*!==\\s*)|^(\\s*==\\s*|\\s+(EQ|eq)\\s+)|^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        7 => array(1, "^(".$this->ldel."\\s{1,})|^(\\s{1,}".$this->rdel.")|^(".$this->ldel."\/)|^(".$this->ldel.")|^(".$this->rdel.")|^(\\s+is\\s+in\\s+)|^(\\s+(AS|as)\\s+)|^(\\s+instanceof\\s+)|^(true|false)|^(null)|^(\\s*===\\s*)|^(\\s*!==\\s*)|^(\\s*==\\s*|\\s+(EQ|eq)\\s+)|^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        8 => array(1, "^(\\s{1,}".$this->rdel.")|^(".$this->ldel."\/)|^(".$this->ldel.")|^(".$this->rdel.")|^(\\s+is\\s+in\\s+)|^(\\s+(AS|as)\\s+)|^(\\s+instanceof\\s+)|^(true|false)|^(null)|^(\\s*===\\s*)|^(\\s*!==\\s*)|^(\\s*==\\s*|\\s+(EQ|eq)\\s+)|^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        9 => array(1, "^(".$this->ldel."\/)|^(".$this->ldel.")|^(".$this->rdel.")|^(\\s+is\\s+in\\s+)|^(\\s+(AS|as)\\s+)|^(\\s+instanceof\\s+)|^(true|false)|^(null)|^(\\s*===\\s*)|^(\\s*!==\\s*)|^(\\s*==\\s*|\\s+(EQ|eq)\\s+)|^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        10 => array(1, "^(".$this->ldel.")|^(".$this->rdel.")|^(\\s+is\\s+in\\s+)|^(\\s+(AS|as)\\s+)|^(\\s+instanceof\\s+)|^(true|false)|^(null)|^(\\s*===\\s*)|^(\\s*!==\\s*)|^(\\s*==\\s*|\\s+(EQ|eq)\\s+)|^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        11 => array(1, "^(".$this->rdel.")|^(\\s+is\\s+in\\s+)|^(\\s+(AS|as)\\s+)|^(\\s+instanceof\\s+)|^(true|false)|^(null)|^(\\s*===\\s*)|^(\\s*!==\\s*)|^(\\s*==\\s*|\\s+(EQ|eq)\\s+)|^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        12 => array(1, "^(\\s+is\\s+in\\s+)|^(\\s+(AS|as)\\s+)|^(\\s+instanceof\\s+)|^(true|false)|^(null)|^(\\s*===\\s*)|^(\\s*!==\\s*)|^(\\s*==\\s*|\\s+(EQ|eq)\\s+)|^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        13 => array(1, "^(\\s+(AS|as)\\s+)|^(\\s+instanceof\\s+)|^(true|false)|^(null)|^(\\s*===\\s*)|^(\\s*!==\\s*)|^(\\s*==\\s*|\\s+(EQ|eq)\\s+)|^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        14 => array(2, "^(\\s+instanceof\\s+)|^(true|false)|^(null)|^(\\s*===\\s*)|^(\\s*!==\\s*)|^(\\s*==\\s*|\\s+(EQ|eq)\\s+)|^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        16 => array(2, "^(true|false)|^(null)|^(\\s*===\\s*)|^(\\s*!==\\s*)|^(\\s*==\\s*|\\s+(EQ|eq)\\s+)|^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        17 => array(2, "^(null)|^(\\s*===\\s*)|^(\\s*!==\\s*)|^(\\s*==\\s*|\\s+(EQ|eq)\\s+)|^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        18 => array(2, "^(\\s*===\\s*)|^(\\s*!==\\s*)|^(\\s*==\\s*|\\s+(EQ|eq)\\s+)|^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        19 => array(2, "^(\\s*!==\\s*)|^(\\s*==\\s*|\\s+(EQ|eq)\\s+)|^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        20 => array(2, "^(\\s*==\\s*|\\s+(EQ|eq)\\s+)|^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        21 => array(3, "^(\\s*!=\\s*|\\s*<>\\s*|\\s+(NE|NEQ|ne|neq)\\s+)|^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        23 => array(4, "^(\\s*>=\\s*|\\s+(GE|GTE|ge|gte)\\s+)|^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        25 => array(5, "^(\\s*<=\\s*|\\s+(LE|LTE|le|lte)\\s+)|^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        27 => array(6, "^(\\s*>\\s*|\\s+(GT|gt)\\s+)|^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        29 => array(7, "^(\\s*<\\s*|\\s+(LT|lt)\\s+)|^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        31 => array(8, "^(\\s+(MOD|mod)\\s+)|^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        33 => array(9, "^(!\\s*|(NOT|not)\\s+)|^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        35 => array(10, "^(\\s*&&\\s*|\\s*(AND|and)\\s+)|^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        37 => array(11, "^(\\s*\\|\\|\\s*|\\s*(OR|or)\\s+)|^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        39 => array(12, "^(\\s*(XOR|xor)\\s+)|^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        41 => array(13, "^(\\s+is\\s+odd\\s+by\\s+)|^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        43 => array(13, "^(\\s+is\\s+not\\s+odd\\s+by\\s+)|^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        44 => array(13, "^(\\s+is\\s+odd)|^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        45 => array(13, "^(\\s+is\\s+not\\s+odd)|^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        46 => array(13, "^(\\s+is\\s+even\\s+by\\s+)|^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        47 => array(13, "^(\\s+is\\s+not\\s+even\\s+by\\s+)|^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        48 => array(13, "^(\\s+is\\s+even)|^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        49 => array(13, "^(\\s+is\\s+not\\s+even)|^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        50 => array(13, "^(\\s+is\\s+div\\s+by\\s+)|^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        51 => array(13, "^(\\s+is\\s+not\\s+div\\s+by\\s+)|^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        52 => array(13, "^(\\((int(eger)?|bool(ean)?|float|double|real|string|binary|array|object)\\))|^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        53 => array(16, "^(\\(\\s*)|^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        57 => array(16, "^(\\s*\\))|^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        58 => array(16, "^(\\[\\s*)|^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        59 => array(16, "^(\\s*\\])|^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        60 => array(16, "^(\\s*->\\s*)|^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        61 => array(16, "^(\\s*=>\\s*)|^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        62 => array(16, "^(\\s*=\\s*)|^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        63 => array(16, "^(\\d+)|^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        64 => array(16, "^((\\+\\+|--)\\s*)|^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        65 => array(17, "^(\\s*(\\+|-)\\s*)|^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        67 => array(18, "^(\\s*(\\*|\/|%)\\s*)|^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        69 => array(19, "^(\\$)|^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        71 => array(19, "^(\\s*;)|^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        72 => array(19, "^(::)|^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        73 => array(19, "^(\\s*:\\s*)|^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        74 => array(19, "^(@)|^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        75 => array(19, "^(#)|^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        76 => array(19, "^(\")|^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        77 => array(19, "^(`)|^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        78 => array(19, "^(\\|)|^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        79 => array(19, "^(\\.)|^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        80 => array(19, "^(\\s*,\\s*)|^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        81 => array(19, "^(\\s*&\\s*)|^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        82 => array(19, "^(\\s*\\?\\s*)|^(\\w+)|^(\\s+)|^(.)"),
        83 => array(19, "^(\\w+)|^(\\s+)|^(.)"),
        84 => array(19, "^(\\s+)|^(.)"),
        85 => array(19, "^(.)"),
        86 => array(19, ""),
    );

                    // yymore is needed
                    do {
                        if (!strlen($yy_yymore_patterns[$this->token][1])) {
                            throw new Exception('cannot do yymore for the last token');
                        }
                        $yysubmatches = array();
                        if (preg_match('/' . $yy_yymore_patterns[$this->token][1] . '/',
                              substr($this->data, $this->counter), $yymatches)) {
                            $yysubmatches = $yymatches;
                            $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                            next($yymatches); // skip global match
                            $this->token += key($yymatches) + $yy_yymore_patterns[$this->token][0]; // token number
                            $this->value = current($yymatches); // token value
                            $this->line = substr_count($this->value, "\n");
                            if ($tokenMap[$this->token]) {
                                // extract sub-patterns for passing to lex function
                                $yysubmatches = array_slice($yysubmatches, $this->token + 1,
                                    $tokenMap[$this->token]);
                            } else {
                                $yysubmatches = array();
                            }
                        }
                    	$r = $this->{'yy_r2_' . $this->token}($yysubmatches);
                    } while ($r !== null && !is_bool($r));
			        if ($r === true) {
			            // we have changed state
			            // process this token in the new state
			            return $this->yylex();
                    } elseif ($r === false) {
                        $this->counter += strlen($this->value);
                        $this->line += substr_count($this->value, "\n");
                        if ($this->counter >= strlen($this->data)) {
                            return false; // end of input
                        }
                        // skip this token
                        continue;
			        } else {
	                    // accept
	                    $this->counter += strlen($this->value);
	                    $this->line += substr_count($this->value, "\n");
	                    return true;
			        }
                }
            } else {
                throw new Exception('Unexpected input at line' . $this->line .
                    ': ' . $this->data[$this->counter]);
            }
            break;
        } while (true);

    } // end function


    const SMARTY = 2;
    function yy_r2_1($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_OTHER;
    }
    function yy_r2_3($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_SINGLEQUOTESTRING;
    }
    function yy_r2_5($yy_subpatterns)
    {

  $this->yypushstate(self::LITERAL);
  return true;
    }
    function yy_r2_6($yy_subpatterns)
    {

  return false;  // unexspected here just ignore
    }
    function yy_r2_7($yy_subpatterns)
    {

  if ($this->smarty->auto_literal) {
     $this->token = Smarty_Internal_Templateparser::TP_OTHER;
  } else {
     $this->token = Smarty_Internal_Templateparser::TP_LDELSLASH;
     $this->yypushstate(self::SMARTY);
  }
    }
    function yy_r2_8($yy_subpatterns)
    {

  if ($this->smarty->auto_literal) {
     $this->token = Smarty_Internal_Templateparser::TP_OTHER;
  } else {
     $this->token = Smarty_Internal_Templateparser::TP_LDEL;
     $this->yypushstate(self::SMARTY);
  }
    }
    function yy_r2_9($yy_subpatterns)
    {

  if ($this->smarty->auto_literal) {
     $this->token = Smarty_Internal_Templateparser::TP_OTHER;
  } else {
     $this->token = Smarty_Internal_Templateparser::TP_RDEL;
     $this->yypopstate();
  }
    }
    function yy_r2_10($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_LDELSLASH;
     $this->yypushstate(self::SMARTY);
    }
    function yy_r2_11($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_LDEL;
     $this->yypushstate(self::SMARTY);
    }
    function yy_r2_12($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_RDEL;
     $this->yypopstate();
    }
    function yy_r2_13($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_ISIN;
    }
    function yy_r2_14($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_AS;
    }
    function yy_r2_16($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_INSTANCEOF;
    }
    function yy_r2_17($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_BOOLEAN;
    }
    function yy_r2_18($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_NULL;
    }
    function yy_r2_19($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_IDENTITY;
    }
    function yy_r2_20($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_NONEIDENTITY;
    }
    function yy_r2_21($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_EQUALS;
    }
    function yy_r2_23($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_NOTEQUALS;
    }
    function yy_r2_25($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_GREATEREQUAL;
    }
    function yy_r2_27($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_LESSEQUAL;
    }
    function yy_r2_29($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_GREATERTHAN;
    }
    function yy_r2_31($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_LESSTHAN;
    }
    function yy_r2_33($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_MOD;
    }
    function yy_r2_35($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_NOT;
    }
    function yy_r2_37($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_LAND;
    }
    function yy_r2_39($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_LOR;
    }
    function yy_r2_41($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_LXOR;
    }
    function yy_r2_43($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_ISODDBY;
    }
    function yy_r2_44($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_ISNOTODDBY;
    }
    function yy_r2_45($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_ISODD;
    }
    function yy_r2_46($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_ISNOTODD;
    }
    function yy_r2_47($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_ISEVENBY;
    }
    function yy_r2_48($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_ISNOTEVENBY;
    }
    function yy_r2_49($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_ISEVEN;
    }
    function yy_r2_50($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_ISNOTEVEN;
    }
    function yy_r2_51($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_ISDIVBY;
    }
    function yy_r2_52($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_ISNOTDIVBY;
    }
    function yy_r2_53($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_TYPECAST;
    }
    function yy_r2_57($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_OPENP;
    }
    function yy_r2_58($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_CLOSEP;
    }
    function yy_r2_59($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_OPENB;
    }
    function yy_r2_60($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_CLOSEB;
    }
    function yy_r2_61($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_PTR; 
    }
    function yy_r2_62($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_APTR;
    }
    function yy_r2_63($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_EQUAL;
    }
    function yy_r2_64($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_INTEGER;
    }
    function yy_r2_65($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_INCDEC;
    }
    function yy_r2_67($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_UNIMATH;
    }
    function yy_r2_69($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_MATH;
    }
    function yy_r2_71($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_DOLLAR;
    }
    function yy_r2_72($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_SEMICOLON;
    }
    function yy_r2_73($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_DOUBLECOLON;
    }
    function yy_r2_74($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_COLON;
    }
    function yy_r2_75($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_AT;
    }
    function yy_r2_76($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_HATCH;
    }
    function yy_r2_77($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_QUOTE;
  $this->yypushstate(self::DOUBLEQUOTEDSTRING);
    }
    function yy_r2_78($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_BACKTICK;
  $this->yypopstate();
    }
    function yy_r2_79($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_VERT;
    }
    function yy_r2_80($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_DOT;
    }
    function yy_r2_81($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_COMMA;
    }
    function yy_r2_82($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_ANDSYM;
    }
    function yy_r2_83($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_QMARK;
    }
    function yy_r2_84($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_ID;
    }
    function yy_r2_85($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_SPACE;
    }
    function yy_r2_86($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_OTHER;
    }


    function yylex3()
    {
        $tokenMap = array (
              1 => 0,
              2 => 1,
              4 => 0,
            );
        if ($this->counter >= strlen($this->data)) {
            return false; // end of input
        }
        $yy_global_pattern = "/^(\\?>)|^(([\S\s]*?)\\?>)|^([\S\s]+)/";

        do {
            if (preg_match($yy_global_pattern, substr($this->data, $this->counter), $yymatches)) {
                $yysubmatches = $yymatches;
                $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                if (!count($yymatches)) {
                    throw new Exception('Error: lexing failed because a rule matched' .
                        'an empty string.  Input "' . substr($this->data,
                        $this->counter, 5) . '... state PHP');
                }
                next($yymatches); // skip global match
                $this->token = key($yymatches); // token number
                if ($tokenMap[$this->token]) {
                    // extract sub-patterns for passing to lex function
                    $yysubmatches = array_slice($yysubmatches, $this->token + 1,
                        $tokenMap[$this->token]);
                } else {
                    $yysubmatches = array();
                }
                $this->value = current($yymatches); // token value
                $r = $this->{'yy_r3_' . $this->token}($yysubmatches);
                if ($r === null) {
                    $this->counter += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    // accept this token
                    return true;
                } elseif ($r === true) {
                    // we have changed state
                    // process this token in the new state
                    return $this->yylex();
                } elseif ($r === false) {
                    $this->counter += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    if ($this->counter >= strlen($this->data)) {
                        return false; // end of input
                    }
                    // skip this token
                    continue;
                } else {                    $yy_yymore_patterns = array(
        1 => array(0, "^(([\S\s]*?)\\?>)|^([\S\s]+)"),
        2 => array(1, "^([\S\s]+)"),
        4 => array(1, ""),
    );

                    // yymore is needed
                    do {
                        if (!strlen($yy_yymore_patterns[$this->token][1])) {
                            throw new Exception('cannot do yymore for the last token');
                        }
                        $yysubmatches = array();
                        if (preg_match('/' . $yy_yymore_patterns[$this->token][1] . '/',
                              substr($this->data, $this->counter), $yymatches)) {
                            $yysubmatches = $yymatches;
                            $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                            next($yymatches); // skip global match
                            $this->token += key($yymatches) + $yy_yymore_patterns[$this->token][0]; // token number
                            $this->value = current($yymatches); // token value
                            $this->line = substr_count($this->value, "\n");
                            if ($tokenMap[$this->token]) {
                                // extract sub-patterns for passing to lex function
                                $yysubmatches = array_slice($yysubmatches, $this->token + 1,
                                    $tokenMap[$this->token]);
                            } else {
                                $yysubmatches = array();
                            }
                        }
                    	$r = $this->{'yy_r3_' . $this->token}($yysubmatches);
                    } while ($r !== null && !is_bool($r));
			        if ($r === true) {
			            // we have changed state
			            // process this token in the new state
			            return $this->yylex();
                    } elseif ($r === false) {
                        $this->counter += strlen($this->value);
                        $this->line += substr_count($this->value, "\n");
                        if ($this->counter >= strlen($this->data)) {
                            return false; // end of input
                        }
                        // skip this token
                        continue;
			        } else {
	                    // accept
	                    $this->counter += strlen($this->value);
	                    $this->line += substr_count($this->value, "\n");
	                    return true;
			        }
                }
            } else {
                throw new Exception('Unexpected input at line' . $this->line .
                    ': ' . $this->data[$this->counter]);
            }
            break;
        } while (true);

    } // end function


    const PHP = 3;
    function yy_r3_1($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_SHORTTAGEND;
  $this->yypopstate();
    }
    function yy_r3_2($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_OTHER;
  $this->value = substr($this->value,0,-2);
    }
    function yy_r3_4($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_OTHER;
    }


    function yylex4()
    {
        $tokenMap = array (
              1 => 0,
              2 => 0,
              3 => 1,
              5 => 0,
            );
        if ($this->counter >= strlen($this->data)) {
            return false; // end of input
        }
        $yy_global_pattern = "/^(".$this->ldel."literal".$this->rdel.")|^(".$this->ldel."\/literal".$this->rdel.")|^(([\S\s]*?)".$this->ldel."\/literal".$this->rdel.")|^([\S\s]+)/";

        do {
            if (preg_match($yy_global_pattern, substr($this->data, $this->counter), $yymatches)) {
                $yysubmatches = $yymatches;
                $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                if (!count($yymatches)) {
                    throw new Exception('Error: lexing failed because a rule matched' .
                        'an empty string.  Input "' . substr($this->data,
                        $this->counter, 5) . '... state LITERAL');
                }
                next($yymatches); // skip global match
                $this->token = key($yymatches); // token number
                if ($tokenMap[$this->token]) {
                    // extract sub-patterns for passing to lex function
                    $yysubmatches = array_slice($yysubmatches, $this->token + 1,
                        $tokenMap[$this->token]);
                } else {
                    $yysubmatches = array();
                }
                $this->value = current($yymatches); // token value
                $r = $this->{'yy_r4_' . $this->token}($yysubmatches);
                if ($r === null) {
                    $this->counter += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    // accept this token
                    return true;
                } elseif ($r === true) {
                    // we have changed state
                    // process this token in the new state
                    return $this->yylex();
                } elseif ($r === false) {
                    $this->counter += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    if ($this->counter >= strlen($this->data)) {
                        return false; // end of input
                    }
                    // skip this token
                    continue;
                } else {                    $yy_yymore_patterns = array(
        1 => array(0, "^(".$this->ldel."\/literal".$this->rdel.")|^(([\S\s]*?)".$this->ldel."\/literal".$this->rdel.")|^([\S\s]+)"),
        2 => array(0, "^(([\S\s]*?)".$this->ldel."\/literal".$this->rdel.")|^([\S\s]+)"),
        3 => array(1, "^([\S\s]+)"),
        5 => array(1, ""),
    );

                    // yymore is needed
                    do {
                        if (!strlen($yy_yymore_patterns[$this->token][1])) {
                            throw new Exception('cannot do yymore for the last token');
                        }
                        $yysubmatches = array();
                        if (preg_match('/' . $yy_yymore_patterns[$this->token][1] . '/',
                              substr($this->data, $this->counter), $yymatches)) {
                            $yysubmatches = $yymatches;
                            $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                            next($yymatches); // skip global match
                            $this->token += key($yymatches) + $yy_yymore_patterns[$this->token][0]; // token number
                            $this->value = current($yymatches); // token value
                            $this->line = substr_count($this->value, "\n");
                            if ($tokenMap[$this->token]) {
                                // extract sub-patterns for passing to lex function
                                $yysubmatches = array_slice($yysubmatches, $this->token + 1,
                                    $tokenMap[$this->token]);
                            } else {
                                $yysubmatches = array();
                            }
                        }
                    	$r = $this->{'yy_r4_' . $this->token}($yysubmatches);
                    } while ($r !== null && !is_bool($r));
			        if ($r === true) {
			            // we have changed state
			            // process this token in the new state
			            return $this->yylex();
                    } elseif ($r === false) {
                        $this->counter += strlen($this->value);
                        $this->line += substr_count($this->value, "\n");
                        if ($this->counter >= strlen($this->data)) {
                            return false; // end of input
                        }
                        // skip this token
                        continue;
			        } else {
	                    // accept
	                    $this->counter += strlen($this->value);
	                    $this->line += substr_count($this->value, "\n");
	                    return true;
			        }
                }
            } else {
                throw new Exception('Unexpected input at line' . $this->line .
                    ': ' . $this->data[$this->counter]);
            }
            break;
        } while (true);

    } // end function


    const LITERAL = 4;
    function yy_r4_1($yy_subpatterns)
    {

  return false;
    }
    function yy_r4_2($yy_subpatterns)
    {

  $this->yypopstate();
  return false;
    }
    function yy_r4_3($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_OTHER;
  $this->value = substr($this->value,0,-strlen($this->smarty->left_delimiter)-strlen($this->smarty->right_delimiter)-8);
  $this->yypopstate();
  if (strlen($this->value) == 0) {
     return false;		// change state directly
  } else {
     return;				// change state after processiing token
  }
    }
    function yy_r4_5($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_OTHER;
    }


    function yylex5()
    {
        $tokenMap = array (
              1 => 0,
              2 => 0,
              3 => 0,
              4 => 0,
              5 => 0,
              6 => 0,
              7 => 0,
              8 => 0,
              9 => 2,
              12 => 0,
            );
        if ($this->counter >= strlen($this->data)) {
            return false; // end of input
        }
        $yy_global_pattern = "/^(".$this->ldel."\\s{1,}\/)|^(".$this->ldel."\\s{1,})|^(".$this->ldel."\/)|^(".$this->ldel.")|^(\")|^(`\\$)|^(\\$\\w+)|^(\\$)|^(([\S\s]*?)(\\\\\\\\|[^\\\\]\"|".$this->ldel."|\\$|`\\$))|^([\S\s]+)/";

        do {
            if (preg_match($yy_global_pattern, substr($this->data, $this->counter), $yymatches)) {
                $yysubmatches = $yymatches;
                $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                if (!count($yymatches)) {
                    throw new Exception('Error: lexing failed because a rule matched' .
                        'an empty string.  Input "' . substr($this->data,
                        $this->counter, 5) . '... state DOUBLEQUOTEDSTRING');
                }
                next($yymatches); // skip global match
                $this->token = key($yymatches); // token number
                if ($tokenMap[$this->token]) {
                    // extract sub-patterns for passing to lex function
                    $yysubmatches = array_slice($yysubmatches, $this->token + 1,
                        $tokenMap[$this->token]);
                } else {
                    $yysubmatches = array();
                }
                $this->value = current($yymatches); // token value
                $r = $this->{'yy_r5_' . $this->token}($yysubmatches);
                if ($r === null) {
                    $this->counter += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    // accept this token
                    return true;
                } elseif ($r === true) {
                    // we have changed state
                    // process this token in the new state
                    return $this->yylex();
                } elseif ($r === false) {
                    $this->counter += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    if ($this->counter >= strlen($this->data)) {
                        return false; // end of input
                    }
                    // skip this token
                    continue;
                } else {                    $yy_yymore_patterns = array(
        1 => array(0, "^(".$this->ldel."\\s{1,})|^(".$this->ldel."\/)|^(".$this->ldel.")|^(\")|^(`\\$)|^(\\$\\w+)|^(\\$)|^(([\S\s]*?)(\\\\\\\\|[^\\\\]\"|".$this->ldel."|\\$|`\\$))|^([\S\s]+)"),
        2 => array(0, "^(".$this->ldel."\/)|^(".$this->ldel.")|^(\")|^(`\\$)|^(\\$\\w+)|^(\\$)|^(([\S\s]*?)(\\\\\\\\|[^\\\\]\"|".$this->ldel."|\\$|`\\$))|^([\S\s]+)"),
        3 => array(0, "^(".$this->ldel.")|^(\")|^(`\\$)|^(\\$\\w+)|^(\\$)|^(([\S\s]*?)(\\\\\\\\|[^\\\\]\"|".$this->ldel."|\\$|`\\$))|^([\S\s]+)"),
        4 => array(0, "^(\")|^(`\\$)|^(\\$\\w+)|^(\\$)|^(([\S\s]*?)(\\\\\\\\|[^\\\\]\"|".$this->ldel."|\\$|`\\$))|^([\S\s]+)"),
        5 => array(0, "^(`\\$)|^(\\$\\w+)|^(\\$)|^(([\S\s]*?)(\\\\\\\\|[^\\\\]\"|".$this->ldel."|\\$|`\\$))|^([\S\s]+)"),
        6 => array(0, "^(\\$\\w+)|^(\\$)|^(([\S\s]*?)(\\\\\\\\|[^\\\\]\"|".$this->ldel."|\\$|`\\$))|^([\S\s]+)"),
        7 => array(0, "^(\\$)|^(([\S\s]*?)(\\\\\\\\|[^\\\\]\"|".$this->ldel."|\\$|`\\$))|^([\S\s]+)"),
        8 => array(0, "^(([\S\s]*?)(\\\\\\\\|[^\\\\]\"|".$this->ldel."|\\$|`\\$))|^([\S\s]+)"),
        9 => array(2, "^([\S\s]+)"),
        12 => array(2, ""),
    );

                    // yymore is needed
                    do {
                        if (!strlen($yy_yymore_patterns[$this->token][1])) {
                            throw new Exception('cannot do yymore for the last token');
                        }
                        $yysubmatches = array();
                        if (preg_match('/' . $yy_yymore_patterns[$this->token][1] . '/',
                              substr($this->data, $this->counter), $yymatches)) {
                            $yysubmatches = $yymatches;
                            $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                            next($yymatches); // skip global match
                            $this->token += key($yymatches) + $yy_yymore_patterns[$this->token][0]; // token number
                            $this->value = current($yymatches); // token value
                            $this->line = substr_count($this->value, "\n");
                            if ($tokenMap[$this->token]) {
                                // extract sub-patterns for passing to lex function
                                $yysubmatches = array_slice($yysubmatches, $this->token + 1,
                                    $tokenMap[$this->token]);
                            } else {
                                $yysubmatches = array();
                            }
                        }
                    	$r = $this->{'yy_r5_' . $this->token}($yysubmatches);
                    } while ($r !== null && !is_bool($r));
			        if ($r === true) {
			            // we have changed state
			            // process this token in the new state
			            return $this->yylex();
                    } elseif ($r === false) {
                        $this->counter += strlen($this->value);
                        $this->line += substr_count($this->value, "\n");
                        if ($this->counter >= strlen($this->data)) {
                            return false; // end of input
                        }
                        // skip this token
                        continue;
			        } else {
	                    // accept
	                    $this->counter += strlen($this->value);
	                    $this->line += substr_count($this->value, "\n");
	                    return true;
			        }
                }
            } else {
                throw new Exception('Unexpected input at line' . $this->line .
                    ': ' . $this->data[$this->counter]);
            }
            break;
        } while (true);

    } // end function


    const DOUBLEQUOTEDSTRING = 5;
    function yy_r5_1($yy_subpatterns)
    {

  if ($this->smarty->auto_literal) {
     $this->token = Smarty_Internal_Templateparser::TP_OTHER;
  } else {
     $this->token = Smarty_Internal_Templateparser::TP_LDELSLASH;
     $this->yypushstate(self::SMARTY);
  }
    }
    function yy_r5_2($yy_subpatterns)
    {

  if ($this->smarty->auto_literal) {
     $this->token = Smarty_Internal_Templateparser::TP_OTHER;
  } else {
     $this->token = Smarty_Internal_Templateparser::TP_LDEL;
     $this->yypushstate(self::SMARTY);
  }
    }
    function yy_r5_3($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_LDELSLASH;
     $this->yypushstate(self::SMARTY);
    }
    function yy_r5_4($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_LDEL;
     $this->yypushstate(self::SMARTY);
    }
    function yy_r5_5($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_QUOTE;
  $this->yypopstate();
    }
    function yy_r5_6($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_BACKTICK;
  $this->value = substr($this->value,0,-1);
  $this->yypushstate(self::SMARTY);
    }
    function yy_r5_7($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_DOLLARID;
    }
    function yy_r5_8($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_OTHER;
    }
    function yy_r5_9($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_OTHER;
  if (substr($this->value,-strlen($this->smarty->left_delimiter)) == $this->smarty->left_delimiter) {
     $this->value = substr($this->value,0,-strlen($this->smarty->left_delimiter));
  } elseif (substr($this->value,-2) == '`$') {
    $this->value = substr($this->value,0,-2);  
  } elseif (strpbrk(substr($this->value,-1),'"$') !== false) {
    $this->value = substr($this->value,0,-1);
  } 
  if (strlen($this->value) == 0) {
     return true;		// rescan
  }
    }
    function yy_r5_12($yy_subpatterns)
    {

  $this->token = Smarty_Internal_Templateparser::TP_OTHER;
    }

}
?>
