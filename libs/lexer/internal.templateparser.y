/**
* Smarty Internal Plugin Templateparser
*
* This is the template parser
* 
* 
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews
*/
%name TP_
%declare_class {class Smarty_Internal_Templateparser}
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
        $this->template = $this->compiler->template;
        $this->cacher = $this->template->cacher_object; 
				$this->nocache = false;
				$this->prefix_code = array();
				$this->prefix_number = 0;
    }
    public static function &instance($new_instance = null)
    {
        static $instance = null;
        if (isset($new_instance) && is_object($new_instance))
            $instance = $new_instance;
        return $instance;
    }
    
} 


%token_prefix TP_

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
    $this->compiler->trigger_template_error();
}

//
// fallback definition to catch all non Smarty template text
//
%fallback     OTHER LDELSLASH LDEL RDEL XML PHP SHORTTAGSTART SHORTTAGEND COMMENTEND COMMENTSTART NUMBER MATH UNIMATH INCDEC OPENP CLOSEP OPENB CLOSEB DOLLAR DOT COMMA COLON DOUBLECOLON SEMICOLON
              VERT EQUAL SPACE PTR APTR ID EQUALS NOTEQUALS GREATERTHAN LESSTHAN GREATEREQUAL LESSEQUAL IDENTITY NONEIDENTITY
              NOT LAND LOR QUOTE SINGLEQUOTE BOOLEAN NULL IN ANDSYM BACKTICK HATCH AT ISODD ISNOTODD ISEVEN ISNOTEVEN ISODDBY ISNOTODDBY
              ISEVENBY ISNOTEVENBY ISDIVBY ISNOTDIVBY.
              

//
// complete template
//
start(res)       ::= template(t). { res = t; }

//
// loop over template elements
//
											// single template element
template(res)       ::= template_element(e). {res = e;}
											// loop of elements
template(res)       ::= template(t) template_element(e). {res = t.e;}

//
// template elements
//
											// Smarty tag
template_element(res)::= smartytag(st). {if ($this->compiler->has_code) {
                                            $tmp =''; foreach ($this->prefix_code as $code) {$tmp.=$code;} $this->prefix_code=array();
                                            res = $this->cacher->processNocacheCode($tmp.st, $this->compiler,$this->nocache,true);
                                         } $this->nocache=false;}	
											// comments
//template_element(res)::= COMMENT(t). { res = $this->cacher->processNocacheCode('<?php /* comment placeholder */?>', $this->compiler,false,false);}	
//template_element(res)::= COMMENTSTART text(t) COMMENTEND. {if ($this->smarty->comment_mode ==0) {
//                                                            res = '';
//                                                           }elseif ($this->smarty->comment_mode ==1){
//                                                            res = $this->cacher->processNocacheCode('<?php /* comment placeholder */?>', $this->compiler,false,false);
//                                                           }else{
//                                                            res = $this->cacher->processNocacheCode('<?php /* '.str_replace('*/', '', t).'*/?>', $this->compiler,false,false);
//                                                           }}	
template_element(res)::= COMMENTSTART text(t) COMMENTEND. { res = '';}

											// Literal
template_element(res)::= LITERALSTART text(t) LITERALEND. {res = $this->cacher->processNocacheCode(t, $this->compiler,false,false);}	
											// {ldelim}
template_element(res)::= LDELIMTAG. {res = $this->cacher->processNocacheCode($this->smarty->left_delimiter, $this->compiler,false,false);}	
											// {rdelim}
template_element(res)::= RDELIMTAG. {res = $this->cacher->processNocacheCode($this->smarty->right_delimiter, $this->compiler,false,false);}	
											// <?php> tag
template_element(res)::= PHP(phpt). {if (!$this->template->security) { 
                                       res = $this->cacher->processNocacheCode(phpt, $this->compiler, false,true);
                                      } elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_QUOTE) {
                                       res = $this->cacher->processNocacheCode(htmlspecialchars(phpt, ENT_QUOTES), $this->compiler, false, false);
                                      }elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_PASSTHRU || $this->smarty->security_policy->php_handling == SMARTY_PHP_ALLOW) {
                                       res = $this->cacher->processNocacheCode("<?php echo '".phpt."';?>\n", $this->compiler, false, false);
                                      }elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_REMOVE) {
                                       res = '';
                                      }	}
											// {PHP} tag
template_element(res)::= PHPSTART text(t) PHPEND. {if (!$this->template->security) { 
                                        res = $this->cacher->processNocacheCode('<?php '.t.' ?>', $this->compiler, false,true);
                                      } elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_QUOTE) {
                                        res = $this->cacher->processNocacheCode(htmlspecialchars('<?php '.t.' ?>', ENT_QUOTES), $this->compiler, false, false);	
                                      }elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_PASSTHRU || $this->smarty->security_policy->php_handling == SMARTY_PHP_ALLOW) {
                                        res = $this->cacher->processNocacheCode("<?php echo '<?php ".t." ?>';?>\n", $this->compiler, false, false);
                                      }elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_REMOVE) {
                                        res = '';
                                      }	}

template_element(res)::= SHORTTAGSTART  variable(v) SHORTTAGEND. {if (!$this->template->security) { 
                                        res = $this->cacher->processNocacheCode($this->compiler->compileTag('print_expression',array('value'=>v)), $this->compiler, false,true);
                                      } elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_QUOTE) {
                                        res = $this->cacher->processNocacheCode(htmlspecialchars('<?php '.t.' ?>', ENT_QUOTES), $this->compiler, false, false);	
                                      }elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_PASSTHRU || $this->smarty->security_policy->php_handling == SMARTY_PHP_ALLOW) {
                                        res = $this->cacher->processNocacheCode("<?php echo '<?php ".t." ?>';?>\n", $this->compiler, false, false);
                                      }elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_REMOVE) {
                                        res = '';
                                      }	}
											// XML tag
template_element(res)::= XML(xml). {res = $this->cacher->processNocacheCode("<?php echo '".xml."';?>\n", $this->compiler, true, true);}	
											// Other template text
template_element(res)::= OTHER(o). {res = $this->cacher->processNocacheCode(o, $this->compiler,false,false);}	
//template_element(res)::= text(t). {res = $this->cacher->processNocacheCode(t, $this->compiler,false,false);}	


//
// all Smarty tags start here
//
									// output with optional attributes
smartytag(res)   ::= LDEL expr(e) attributes(a) RDEL. { res = $this->compiler->compileTag('print_expression',array_merge(array('value'=>e),a));}
									// assign new style
smartytag(res)   ::= LDEL statement(s) RDEL. { res = $this->compiler->compileTag('assign',s);}									
									// tag with optional Smarty2 style attributes
smartytag(res)   ::= LDEL ID(i) attributes(a) RDEL. { res =  $this->compiler->compileTag(i,a);}
									// registered object tag
smartytag(res)   ::= LDEL ID(i) PTR ID(m) attributes(a) RDEL. { res =  $this->compiler->compileTag(i,array_merge(array('object_methode'=>m),a));}
									// tag with modifier and optional Smarty2 style attributes
smartytag(res)   ::= LDEL ID(i) modifier(m) modparameters(p) attributes(a) RDEL. { res =  '<?php ob_start();?>'.$this->compiler->compileTag(i,a).'<?php echo ';
																					                       if ($this->smarty->plugin_handler->loadSmartyPlugin(m[0],'modifier')) {
                                                                      res .= "\$_smarty_tpl->smarty->plugin_handler->".m[0] . "(array(ob_get_clean()". p ."),'modifier');?>";
                                                                 } else {
                                                                   if (m[0] == 'isset' || m[0] == 'empty' || is_callable(m[0])) {
																					                            if (!$this->template->security || $this->smarty->security_handler->isTrustedModifier(m[0], $this->compiler)) {
																					                              res .= m[0] . "(ob_get_clean()". p .");?>";
																					                            }
																					                         } else {
                                                                      $this->compiler->trigger_template_error ("unknown modifier \"" . m[0] . "\"");
                                                                 }
                                                              }
                                                            }
									// end of block tag  {/....}									
smartytag(res)   ::= LDELSLASH ID(i) attributes(a) RDEL. { res =  $this->compiler->compileTag(i.'close',a);}
									// end of block object tag  {/....}									
smartytag(res)   ::= LDELSLASH ID(i) PTR ID(m) RDEL. { res =  $this->compiler->compileTag(i.'close',array('object_methode'=>m));}
									// {if}, {elseif} and {while} tag
smartytag(res)   ::= LDEL ID(i)SPACE ifexprs(ie) RDEL. { res =  $this->compiler->compileTag(i,array('if condition'=>ie));}
									// {for} tag
smartytag(res)   ::= LDEL ID(i) SPACE statements(s) SEMICOLON ifexprs(ie) SEMICOLON DOLLAR varvar(v2) foraction(e2) RDEL. { res =  $this->compiler->compileTag(i,array('start'=>s,'ifexp'=>ie,'varloop'=>v2,'loop'=>e2));}
  foraction(res)	 ::= EQUAL expr(e). { res = '='.e;}
  foraction(res)	 ::= INCDEC(e). { res = e;}
									// {for $var in $array} tag
// replaced with next line because config vars could an array!! smartytag(res)   ::= LDEL ID(i) SPACE DOLLAR varvar(v0) IN variable(v1) RDEL. { res =  $this->compiler->compileTag(i,array('from'=>v1,'item'=>v0));}
smartytag(res)   ::= LDEL ID(i) SPACE DOLLAR varvar(v0) IN value(v1) RDEL. { res =  $this->compiler->compileTag(i,array('from'=>v1,'item'=>v0));}
smartytag(res)   ::= LDEL ID(i) SPACE DOLLAR varvar(v0) IN array(a) RDEL. { res =  $this->compiler->compileTag(i,array('from'=>a,'item'=>v0));}

//
//Attributes of Smarty tags 
//
									// list of attributes
attributes(res)  ::= attributes(a1) attribute(a2). { res = array_merge(a1,a2);}
									// single attribute
attributes(res)  ::= attribute(a). { res = a;}
									// no attributes
attributes(res)  ::= . { res = array();}
									
									// different formats of attribute
//attribute(res)   ::= SPACE ID(v) EQUAL ID(i). { res = array(v=>'\''.i.'\'');}
attribute(res)   ::= SPACE ID(v) EQUAL expr(e). { res = array(v=>e);}

//
// statement
//
statements(res)		::= statement(s). { res = array(s);}
statements(res)		::= statements(s1) COMMA statement(s). { s1[]=s; res = s1;}

statement(res)		::= DOLLAR varvar(v) EQUAL expr(e). { res = array('var' => v, 'value'=>e);}
//statement(res)		::= DOLLAR varvar(v) EQUAL ID(i). { res = array('var' => v, 'value'=>'\''.i.'\'');}

//
// expressions
//
									// simple expression
expr(res)				 ::= ID(i). { res = '\''.i.'\''; }
//expr(res)				 ::= UNQ_STR(s). { res = '\''.s.'\''; }
//expr(res)				 ::= ID(i). { res = i; }
expr(res)				 ::= exprs(e).	{res = e;}
expr(res)        ::= expr(e) modifier(m) modparameters(p). {             
                                                            if ($this->smarty->plugin_handler->loadSmartyPlugin(m[0],'modifier')) {
                                                                      res = "\$_smarty_tpl->smarty->plugin_handler->".m[0] . "(array(". e . p ."),'modifier')";
                                                                 } else {
                                                                   if (m[0] == 'isset' || m[0] == 'empty' || is_callable(m[0])) {
																					                            if (!$this->template->security || $this->smarty->security_handler->isTrustedModifier(m[0], $this->compiler)) {
																					                               res = m[0] . "(". e . p .")";
																					                            }
																					                         } else {
                                                                      $this->compiler->trigger_template_error ("unknown modifier \"" . m[0] . "\"");
                                                                 }
                                                              }
                                                            }
exprs(res)				 ::= array(a).	{res = a;}

									// single value
exprs(res)        ::= value(v). { res = v; }
									// +/- value
exprs(res)        ::= UNIMATH(m) value(v). { res = m.v; }
									// arithmetic expression
exprs(res)        ::= exprs(e) math(m) value(v). { res = e . m . v; } 
									// catenate
exprs(res)        ::= exprs(e) ANDSYM value(v). { res = '('. e . ').(' . v. ')'; } 

//
// mathematical operators
//
									// +,-
math(res)        ::= UNIMATH(m). {res = m;}
									// *,/,%
math(res)        ::= MATH(m). {res = m;}


									// value
value(res)		   ::= variable(v). { res = v; }
                  // config variable
value(res)	     ::= HATCH ID(i) HATCH. {res = '$_smarty_tpl->getConfigVariable(\''. i .'\')';}
                 // numeric
value(res)       ::= NUMBER(n). { res = n; }
									// boolean
value(res)       ::= BOOLEAN(b). { res = b; }
									// null
value(res)       ::= NULL(n). { res = n; }

									// function call
value(res)	     ::= function(f). { res = f; }
									// expression
value(res)       ::= OPENP expr(e) CLOSEP. { res = "(". e .")"; }

									// singele quoted string
value(res)	     ::= SINGLEQUOTE text(t) SINGLEQUOTE. { res = "'".t."'"; }
value(res)	     ::= SINGLEQUOTE SINGLEQUOTE. { res = "''"; }
									// double quoted string
value(res)	     ::= QUOTE doublequoted(s) QUOTE. { res = "'".str_replace('\"','"',s)."'"; }
//value(res)	     ::= QUOTE doublequoted(s) QUOTE. { res = "'".addcslashes(str_replace(array('\"'),array('"'),s),"'")."'"; }
//value(res)	     ::= QUOTE doublequoted(s) QUOTE. { res = "'".s."'"; var_dump(s);}
value(res)	     ::= QUOTE QUOTE. { res = "''"; }

									// static class methode call
value(res)	     ::= ID(c) DOUBLECOLON method(m). { res = c.'::'.m; }
value(res)	     ::= ID(c) DOUBLECOLON DOLLAR ID(f) OPENP params(p) CLOSEP. { $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. f .'\')->value;?>'; res = c.'::$_tmp'.$this->prefix_number.'('. p .')'; }
									// static class methode call with object chainig
value(res)	     ::= ID(c) DOUBLECOLON method(m) objectchain(oc). { res = c.'::'.m.oc; }
value(res)	     ::= ID(c) DOUBLECOLON DOLLAR ID(f) OPENP params(p) CLOSEP objectchain(oc). { $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. f .'\')->value;?>'; res = c.'::$_tmp'.$this->prefix_number.'('. p .')'.oc; }
									// static class constant
value(res)       ::= ID(c) DOUBLECOLON ID(v). { res = c.'::'.v;}
									// static class variables
value(res)       ::= ID(c) DOUBLECOLON DOLLAR ID(v) arrayindex(a). { res = c.'::$'.v.a;}
									// static class variables with object chain
value(res)       ::= ID(c) DOUBLECOLON DOLLAR ID(v) arrayindex(a) objectchain(oc). { res = c.'::$'.v.a.oc;}


//
// variables 
//
									// simple Smarty variable (optional array)
variable(res)    ::= DOLLAR varvar(v) arrayindex(a). { if (v == '\'smarty\'') { res =  $this->compiler->compileTag(trim(v,"'"),a);} else {
                                                         res = '$_smarty_tpl->getVariable('. v .')->value'.a; $this->nocache=$this->template->getVariable(trim(v,"'"))->nocache;}}
									// variable with property
variable(res)    ::= DOLLAR varvar(v) AT ID(p). { res = '$_smarty_tpl->getVariable('. v .')->'.p; $this->nocache=$this->template->getVariable(trim(v,"'"))->nocache;}
									// object
variable(res)    ::= object(o). { res = o; }
                  // config variable
//variable(res)	   ::= HATCH ID(i) HATCH. {res = '$_smarty_tpl->getConfigVariable(\''. i .'\')';}

//
// array index
//
										// multiple array index
arrayindex(res)  ::= arrayindex(a1) indexdef(a2). {res = a1.a2;}
										// no array index
arrayindex        ::= . {return;}

// single index definition
										// Smarty2 style index 
indexdef(res)   ::= DOT ID(i). { res = "['". i ."']";}
indexdef(res)   ::= DOT exprs(e). { res = "[". e ."]";}
										// section tag index
indexdef(res)   ::= OPENB ID(i)CLOSEB. { res = '['.$this->compiler->compileTag('smarty','[\'section\'][\''.i.'\'][\'index\']').']';}
										// PHP style index
indexdef(res)   ::= OPENB exprs(e) CLOSEB. { res = "[". e ."]";}

//
// variable variable names
//
										// singel identifier element
varvar(res)			 ::= varvarele(v). {res = v;}
										// sequence of identifier elements
varvar(res)			 ::= varvar(v1) varvarele(v2). {res = v1.'.'.v2;}
										// fix sections of element
varvarele(res)	 ::= ID(s). {res = '\''.s.'\'';}
										// variable sections of element
varvarele(res)	 ::= LDEL expr(e) RDEL. {res = '('.e.')';}

//
// objects
//
object(res)      ::= DOLLAR varvar(v) arrayindex(a) objectchain(oc). { res = '$_smarty_tpl->getVariable('. v .')->value'.a.oc; $this->nocache=$this->template->getVariable(trim(v,"'"))->nocache;}
										// single element
objectchain(res) ::= objectelement(oe). {res  = oe; }
										// chain of elements 
objectchain(res) ::= objectchain(oc) objectelement(oe). {res  = oc.oe; }
										// variable
objectelement(res)::= PTR ID(i) arrayindex(a).	    { res = '->'.i.a;}
//objectelement(res)::= PTR varvar(v) arrayindex(a).	{ res = '->'.v.a;}
										// method
objectelement(res)::= PTR method(f).	{ res = '->'.f;}


//
// function
//
function(res)     ::= ID(f) OPENP params(p) CLOSEP.	{if (!$this->template->security || $this->smarty->security_handler->isTrustedPhpFunction(f, $this->compiler)) {
																					            if (f == 'isset' || f == 'empty' || is_callable(f)) {
																					                res = f . "(". p .")";
																					            } else {
                                                       $this->compiler->trigger_template_error ("unknown function \"" . f . "\"");
                                                      }
                                                    }}

//
// method
//
method(res)     ::= ID(f) OPENP params(p) CLOSEP.	{ res = f . "(". p .")";}

// function/method parameter
										// multiple parameters
params(res)       ::= expr(e) COMMA params(p). { res = e.",".p;}
										// single parameter
params(res)       ::= expr(e). { res = e;}
										// kein parameter
params            ::= . { return;}

//
// modifier
//  
modifier(res)    ::= VERT AT ID(m). { res =  array(m,true);}
modifier(res)    ::= VERT ID(m). { res =  array(m,false);}


//
// modifier parameter
//
										// multiple parameter
modparameters(res) ::= modparameters(mps) modparameter(mp). { res = mps.mp;}
										// no parameter
modparameters      ::= . {return;}
										// parameter expression
modparameter(res) ::= COLON ID(mp). {res = ',\''.mp.'\'';}
modparameter(res) ::= COLON exprs(mp). {res = ','.mp;}

//
// if expressions
//
										// single if expression
ifexprs(res)			 ::= ifexpr(e).	{res = e;}
ifexprs(res)			 ::= NOT ifexprs(e).	{res = '!'.e;}
ifexprs(res)			 ::= OPENP ifexprs(e) CLOSEP.	{res = '('.e.')';}

// if expression
										// simple expression
ifexpr(res)        ::= expr(e). {res =e;}
ifexpr(res)        ::= expr(e1) ifcond(c) expr(e2). {res = e1.c.e2;}
ifexpr(res)			   ::= ifexprs(e1) lop(o) ifexprs(e2).	{res = e1.o.e2;}
ifexpr(res)			   ::= ifexprs(e1) ISDIVBY ifexprs(e2).	{res = '!('.e1.' % '.e2.')';}
ifexpr(res)			   ::= ifexprs(e1) ISNOTDIVBY ifexprs(e2).	{res = '('.e1.' % '.e2.')';}
ifexpr(res)			   ::= ifexprs(e1) ISEVEN.	{res = '!(1 & '.e1.')';}
ifexpr(res)			   ::= ifexprs(e1) ISNOTEVEN.	{res = '(1 & '.e1.')';}
ifexpr(res)			   ::= ifexprs(e1) ISEVENBY ifexprs(e2).	{res = '!(1 & '.e1.' / '.e2.')';}
ifexpr(res)			   ::= ifexprs(e1) ISNOTEVENBY ifexprs(e2).	{res = '(1 & '.e1.' / '.e2.')';}
ifexpr(res)			   ::= ifexprs(e1) ISODD.	{res = '(1 & '.e1.')';}
ifexpr(res)			   ::= ifexprs(e1) ISNOTODD.	{res = '!(1 & '.e1.')';}
ifexpr(res)			   ::= ifexprs(e1) ISODDBY ifexprs(e2).	{res = '(1 & '.e1.' / '.e2.')';}
ifexpr(res)			   ::= ifexprs(e1) ISNOTODDBY ifexprs(e2).	{res = '!(1 & '.e1.' / '.e2.')';}

ifcond(res)        ::= EQUALS. {res = '==';}
ifcond(res)        ::= NOTEQUALS. {res = '!=';}
ifcond(res)        ::= GREATERTHAN. {res = '>';}
ifcond(res)        ::= LESSTHAN. {res = '<';}
ifcond(res)        ::= GREATEREQUAL. {res = '>=';}
ifcond(res)        ::= LESSEQUAL. {res = '<=';}
ifcond(res)        ::= IDENTITY. {res = '===';}
ifcond(res)        ::= NONEIDENTITY. {res = '!==';}

lop(res)        ::= LAND. {res = '&&';}
lop(res)        ::= LOR. {res = '||';}

//
// ARRAY element assignment
//
array(res)		       ::=  OPENB arrayelements(a) CLOSEB.  { res = 'array('.a.')';}
arrayelements(res)   ::=  arrayelement(a).  { res = a; }
arrayelements(res)   ::=  arrayelements(a1) COMMA arrayelement(a).  { res = a1.','.a; }
arrayelements        ::=  .  { return; }
arrayelement(res)		 ::=  expr(e). { res = e;}
arrayelement(res)		 ::=  expr(e1) APTR expr(e2). { res = e1.'=>'.e2;}
arrayelement(res)		 ::=  ID(i) APTR expr(e2). { res = '\''.i.'\'=>'.e2;}

//
// double qouted strings
//
doublequoted(res)          ::= doublequoted(o1) doublequotedcontent(o2). {res = o1.o2;}
doublequoted(res)          ::= doublequotedcontent(o). {res = o;}
doublequotedcontent(res)           ::=  variable(v). {res = "'.".v.".'";}
doublequotedcontent(res)           ::=  BACKTICK variable(v) BACKTICK. {res = "'.".v.".'";}
doublequotedcontent(res)           ::=  LDEL expr(e) RDEL. {res = "'.(".e.").'";}
doublequotedcontent(res)           ::= OTHER(o). {res = addcslashes(o,"'");}
//doublequotedcontent(res)           ::= OTHER(o). {res = o;}
//doublequotedcontent(res)           ::= text(t). {res = addcslashes(t,"'");}

//
// text string
//
text(res)          ::= text(t) textelement(e). {res = t.e;}
text(res)          ::= textelement(e). {res = e;}
textelement(res)          ::= OTHER(o). {res = o;}
textelement(res)          ::= LDEL(o). {res = o;}
