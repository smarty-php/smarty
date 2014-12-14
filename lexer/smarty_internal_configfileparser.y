/**
* Smarty Internal Plugin Configfileparser
*
* This is the config file parser
* 
* 
* @package Smarty
* @subpackage Config
* @author Uwe Tews
*/
%name TPC_
%declare_class {class Smarty_Internal_Configfileparser}
%include_class
{
    // states whether the parse was successful or not
    public $successful = true;
    public $retvalue = 0;
    private $lex;
    private $internalError = false;

    function __construct($lex, $compiler) {
        // set instance object
        self::instance($this); 
        $this->lex = $lex;
        $this->smarty = $compiler->smarty; 
        $this->compiler = $compiler;
    }
    public static function &instance($new_instance = null)
    {
        static $instance = null;
        if (isset($new_instance) && is_object($new_instance))
            $instance = $new_instance;
        return $instance;
    }

    private function parse_bool($str) {
        if (in_array(strtolower($str) ,array('on','yes','true'))) {
            $res = true;
        } else {
            $res = false;
        }
        return $res;
    }

    private static $escapes_single = Array('\\' => '\\',
                                          '\'' => '\'');
    private static function parse_single_quoted_string($qstr) {
        $escaped_string = substr($qstr, 1, strlen($qstr)-2); //remove outer quotes

        $ss = preg_split('/(\\\\.)/', $escaped_string, -1, PREG_SPLIT_DELIM_CAPTURE);

        $str = "";
        foreach ($ss as $s) {
            if (strlen($s) === 2 && $s[0] === '\\') {
                if (isset(self::$escapes_single[$s[1]])) {
                    $s = self::$escapes_single[$s[1]];
                }
             }

             $str .= $s;
        }

        return $str;
    }

    private static function parse_double_quoted_string($qstr) {
        $inner_str = substr($qstr, 1, strlen($qstr)-2);
        return stripcslashes($inner_str);
    }

    private static function parse_tripple_double_quoted_string($qstr) {
        return stripcslashes($qstr);
    }

    private function set_var(Array $var, Array &$target_array) {
        $key = $var["key"];
        $value = $var["value"];

        if ($this->smarty->config_overwrite || !isset($target_array['vars'][$key])) {
            $target_array['vars'][$key] = $value;
        } else {
            settype($target_array['vars'][$key], 'array');
            $target_array['vars'][$key][] = $value;
        }
    }

    private function add_global_vars(Array $vars) {
        if (!isset($this->compiler->config_data['vars'])) {
      $this->compiler->config_data['vars'] = Array();
        }
        foreach ($vars as $var) {
            $this->set_var($var, $this->compiler->config_data);
        }
    }

    private function add_section_vars($section_name, Array $vars) {
        if (!isset($this->compiler->config_data['sections'][$section_name]['vars'])) {
            $this->compiler->config_data['sections'][$section_name]['vars'] = Array();
        }
        foreach ($vars as $var) {
            $this->set_var($var, $this->compiler->config_data['sections'][$section_name]);
        }
    }
} 


%token_prefix TPC_

%parse_accept
{
    $this->successful = !$this->internalError;
    $this->internalError = false;
    $this->retvalue = $this->_retvalue;
    //echo $this->retvalue."\n\n";
}

%syntax_error
{
    $this->internalError = true;
    $this->yymajor = $yymajor;
    $this->compiler->trigger_config_file_error();
}

%stack_overflow
{
    $this->internalError = true;
    $this->compiler->trigger_config_file_error("Stack overflow in configfile parser");
}

// Complete config file
start(res) ::= global_vars sections. {
    res = null;
}

// Global vars
global_vars(res) ::= var_list(vl). {
    $this->add_global_vars(vl); res = null;
}

// Sections
sections(res) ::= sections section. {
    res = null;
}

sections(res) ::= . {
    res = null;
}

section(res) ::= OPENB SECTION(i) CLOSEB newline var_list(vars). {
    $this->add_section_vars(i, vars);
    res = null;
}

section(res) ::= OPENB DOT SECTION(i) CLOSEB newline var_list(vars). {
    if ($this->smarty->config_read_hidden) {
        $this->add_section_vars(i, vars);
    }
    res = null;
}

// Var list
var_list(res) ::= var_list(vl) newline. {
    res = vl;
}

var_list(res) ::= var_list(vl) var(v). {
    res = array_merge(vl, Array(v));
}

var_list(res) ::= . {
    res = Array();
}


// Var
var(res) ::= ID(id) EQUAL value(v). {
    res = Array("key" => id, "value" => v);
}


value(res) ::= FLOAT(i). {
    res = (float) i;
}

value(res) ::= INT(i). {
    res = (int) i;
}

value(res) ::= BOOL(i). {
    res = $this->parse_bool(i);
}

value(res) ::= SINGLE_QUOTED_STRING(i). {
    res = self::parse_single_quoted_string(i);
}

value(res) ::= DOUBLE_QUOTED_STRING(i). {
    res = self::parse_double_quoted_string(i);
}

value(res) ::= TRIPPLE_QUOTES(i) TRIPPLE_TEXT(c) TRIPPLE_QUOTES_END(ii). {
    res = self::parse_tripple_double_quoted_string(c);
}

value(res) ::= TRIPPLE_QUOTES(i) TRIPPLE_QUOTES_END(ii). {
    res = '';
}

value(res) ::= NAKED_STRING(i). {
    res = i;
}

// NOTE: this is not a valid rule
// It is added hier to produce a usefull error message on a missing '=';
value(res) ::= OTHER(i). {
    res = i;
}


// Newline and comments
newline(res) ::= NEWLINE. {
    res = null;
}

newline(res) ::= COMMENTSTART NEWLINE. {
    res = null;
}

newline(res) ::= COMMENTSTART NAKED_STRING NEWLINE. {
    res = null;
}
