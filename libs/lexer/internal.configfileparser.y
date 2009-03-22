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
        $this->smarty = Smarty::instance(); 
        $this->compiler = $compiler;
        $this->current_section = null;
        $this->hidden_section = false;
    }
    public static function &instance($new_instance = null)
    {
        static $instance = null;
        if (isset($new_instance) && is_object($new_instance))
            $instance = $new_instance;
        return $instance;
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

//
// fallback definition to catch all non Smarty template text
//
%fallback     OTHER COMMENTSTART NUMBER OPENB CLOSEB DOT BOOLEANTRUE BOOLEANFALSE SI_QSTR DO_QSTR EQUAL SPACE ID.
              

//
// complete config file
//
start(res)       ::= config(r). { res = r; }

//
// loop over config file elements
//
											// single config element
config(res)       ::= config_element(e). {res = e;}
											// loop of elements
config(res)       ::= config(c) config_element(e). {res = c.e;}

//
// config elements
//
											// Section defifinition
config_element(res) ::= OPENB ID(i) CLOSEB EOL. { $this->hidden_section = false; $this->current_section = i; res ='';}
											// Hidden section defifinition
config_element(res) ::= OPENB DOT ID(i) CLOSEB EOL. { if ($this->smarty->config_read_hidden) {
                                                       $this->hidden_section = false; $this->current_section = i;
                                                      } else {$this->hidden_section = true; } res ='';}
// variable assignment
config_element(res) ::= ID(i) EQUAL value(v) EOL. {if (!$this->hidden_section) {
                                                   $value=v;
                                                   if ($this->smarty->config_booleanize) {
                                                       if (in_array(strtolower($value),array('on','yes','true')))
                                                          $value = true;
                                                       else if (in_array(strtolower($value),array('off','no','false')))
                                                         $value = false;
                                                   }
                                                   if ($this->current_section == null) {
                                                      if ($this->smarty->config_overwrite) {
                                                           $this->compiler->config_data['vars'][i]=$value;
                                                        } else {
                                                          settype($this->compiler->config_data['vars'][i], 'array');
                                                          $this->compiler->config_data['vars'][i][]=$value;
                                                        }
                                                     } else {
                                                      if ($this->smarty->config_overwrite) {
                                                          $this->compiler->config_data['sections'][$this->current_section]['vars'][i]=$value;
                                                      } else {
                                                          settype($this->compiler->config_data['sections'][$this->current_section]['vars'][i], 'array');
                                                          $this->compiler->config_data['sections'][$this->current_section]['vars'][i][]=$value;
                                                      }
                                                     }}  res ='';}
// empty and comment lines
config_element(res) ::= EOL. { res ='';}
config_element(res) ::= COMMENTSTART text(t) EOL. { res ='';}

value(res)         ::= text(t). {res = t;}
value(res)         ::= SI_QSTR(s). {res = trim(s,"'");}
value(res)         ::= DO_QSTR(s). {res = trim(s,'"');}
value(res)         ::= ML_QSTR(s). {res = trim(s,'"');}
value(res)         ::= NUMBER(n). {res = (int)n;}


text(res)          ::= text(t) textelement(e). {res = t.e;}
text(res)          ::= textelement(e). {res = e;}
textelement(res)          ::= OTHER(o). {res = o;}
