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
%stack_size 500
%name TP_
%declare_class {
/**
* Smarty Internal Plugin Templateparser
*
* This is the template parser.
* It is generated from the smarty_internal_templateparser.y file
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews
*/
class Smarty_Internal_Templateparser
}
%include_class
{
    const Err1 = "Security error: Call to private object member not allowed";
    const Err2 = "Security error: Call to dynamic object member not allowed";
    const Err3 = "PHP in template not allowed. Use SmartyBC to enable it";

    /**
     * result status
     *
     * @var bool
     */
    public $successful = true;
    /**
     * return value
     *
     * @var mixed
     */
    public $retvalue = 0;
    /**
     * counter for prefix code
     *
     * @var int
     */
    public static $prefix_number = 0;
    /**
     * @var
     */
    public $yymajor;
    /**
     * last index of array variable
     *
     * @var mixed
     */
    public $last_index;
    /**
     * last variable name
     *
     * @var string
     */
    public $last_variable;
    /**
     * root parse tree buffer
     *
     * @var Smarty_Internal_ParseTree
     */
    public $root_buffer;
    /**
     * current parse tree object
     *
     * @var Smarty_Internal_ParseTree
     */
    public $current_buffer;
    /**
     * lexer object
     *
     * @var Smarty_Internal_Templatelexer
     */
    private $lex;
    /**
     * internal error flag
     *
     * @var bool
     */
    private $internalError = false;
    /**
     * {strip} status
     *
     * @var bool
     */
    private $strip = false;
    /**
     * compiler object
     *
     * @var Smarty_Internal_TemplateCompilerBase
     */
    public $compiler = null;
    /**
     * smarty object
     *
     * @var Smarty
     */
    public $smarty = null;
    /**
     * template object
     *
     * @var Smarty_Internal_Template
     */
    public $template = null;
    /**
     * block nesting level
     *
     * @var int
     */
    public $block_nesting_level = 0;
    /**
     * xml tag flag
     *
     * @var bool
     */
    private $is_xml = false;
    /**
     * security object
     *
     * @var Smarty_Security
     */
    private $security = null;
    /**
     * asp enabled
     *
     * @var bool
     */
    private $asp_tags = false;
    /**
     * PHP tag handling mode
     *
     * @var int
     */
    private $php_handling = 0;

    /**
     * constructor
     *
     * @param Smarty_Internal_Templatelexer        $lex
     * @param Smarty_Internal_TemplateCompilerBase $compiler
     */
    function __construct(Smarty_Internal_Templatelexer $lex, Smarty_Internal_TemplateCompilerBase $compiler)
    {
        $this->lex = $lex;
        $this->compiler = $compiler;
        $this->template = $this->compiler->template;
        $this->smarty = $this->template->smarty;
        $this->compiler->has_variable_string = false;
        $this->compiler->prefix_code = array();
        if ($this->security = isset($this->smarty->security_policy)) {
            $this->php_handling = $this->smarty->security_policy->php_handling;
        } else {
            $this->php_handling = $this->smarty->php_handling;
        }
        $this->asp_tags = (ini_get('asp_tags') != '0');
        $this->current_buffer = $this->root_buffer = new Smarty_Internal_ParseTree_Template($this);
    }

    /**
     * insert PHP code in current buffer
     *
     * @param string $code
     */
    public function insertPhpCode($code)
    {
        $this->current_buffer->append_subtree(new Smarty_Internal_ParseTree_Tag($this, $code));
    }

    /**
     * compile variable
     *
     * @param string $variable
     *
     * @return string
     */
    public function compileVariable($variable)
    {
        if (strpos($variable, '(') == 0) {
            // not a variable variable
            $var = trim($variable, '\'');
            $this->compiler->tag_nocache = $this->compiler->tag_nocache | $this->template->getVariable($var, null, true, false)->nocache;
            $this->template->properties['variables'][$var] = $this->compiler->tag_nocache | $this->compiler->nocache;
        }
        return '$_smarty_tpl->tpl_vars[' . $variable . ']->value';
    }
}

%token_prefix TP_

%parse_accept
{
    $this->successful = !$this->internalError;
    $this->internalError = false;
    $this->retvalue = $this->_retvalue;
}

%syntax_error
{
    $this->internalError = true;
    $this->yymajor = $yymajor;
    $this->compiler->trigger_template_error();
}

%stack_overflow
{
    $this->internalError = true;
    $this->compiler->trigger_template_error("Stack overflow in template parser");
}

%left VERT.
%left COLON.

    //
    // complete template
    //
start(res)       ::= template. {
    res = $this->root_buffer->to_smarty_php();
}

    //
    // loop over template elements
    //
                      // single template element
template       ::= template_element(e). {
    if (e != null) {
        $this->current_buffer->append_subtree(e);
    }
}

                      // loop of elements
template       ::= template template_element(e). {
    if (e != null) {
        // because of possible code injection
        $this->current_buffer->append_subtree(e);
    }
}

                      // empty template
template       ::= . 

//
// template elements
//
                      // Smarty tag
template_element(res)::= smartytag(st) RDEL. {
    if ($this->compiler->has_code) {
        $tmp =''; foreach ($this->compiler->prefix_code as $code) {$tmp.=$code;} $this->compiler->prefix_code=array();
        res = new Smarty_Internal_ParseTree_Tag($this, $this->compiler->processNocacheCode($tmp.st,true));
    } else { 
        res = null;
    }  
    $this->compiler->has_variable_string = false;
    $this->block_nesting_level = count($this->compiler->_tag_stack);
} 

                      // comments
template_element(res)::= COMMENT(c). {
    res = null;
}

                      // Literal
template_element(res) ::= literal(l). {
    res = new Smarty_Internal_ParseTree_Text($this, l);
}

                      // '<?php' | '<script language=php>' tag
template_element(res)::= PHPSTARTTAG(st). {
    if (strpos(st, '<s') === 0) {
        $this->lex->is_phpScript = true;
    }
    if ($this->php_handling == Smarty::PHP_PASSTHRU) {
        if ($this->lex->is_phpScript) {
            $s = addcslashes(st, "'");
            res = new Smarty_Internal_ParseTree_Text($this, $s);
        } else {
            res = new Smarty_Internal_ParseTree_Text($this, st);
        }
    } elseif ($this->php_handling == Smarty::PHP_QUOTE) {
        res = new Smarty_Internal_ParseTree_Text($this, htmlspecialchars(st, ENT_QUOTES));
    } elseif ($this->php_handling == Smarty::PHP_ALLOW) {
        if (!($this->smarty instanceof SmartyBC)) {
            $this->compiler->trigger_template_error (self::Err3);
        }
        res = new Smarty_Internal_ParseTree_Tag($this, $this->compiler->processNocacheCode('<?php ', true));
    } elseif ($this->php_handling == Smarty::PHP_REMOVE) {
        res = null;
    }
}

                      // '?>' tag
template_element(res)::= PHPENDTAG(st). {
    if ($this->is_xml) {
        $this->compiler->tag_nocache = true; 
        $this->is_xml = false;
        $save = $this->template->has_nocache_code; 
        res = new Smarty_Internal_ParseTree_Tag($this, $this->compiler->processNocacheCode("<?php echo '?>';?>\n", $this->compiler, true));
        $this->template->has_nocache_code = $save;
    } elseif ($this->php_handling == Smarty::PHP_PASSTHRU) {
        res = new Smarty_Internal_ParseTree_Text($this, st);
    } elseif ($this->php_handling == Smarty::PHP_QUOTE) {
        res = new Smarty_Internal_ParseTree_Text($this, htmlspecialchars('?>', ENT_QUOTES));
    } elseif ($this->php_handling == Smarty::PHP_ALLOW) {
        res = new Smarty_Internal_ParseTree_Tag($this, $this->compiler->processNocacheCode('?>', true));
    } elseif ($this->php_handling == Smarty::PHP_REMOVE) {
        res = null;
    }
}
                      // '</script>' tag (only for PHP)
template_element(res)::= PHPENDSCRIPT(st). {
    if (!$this->lex->is_phpScript) {
        res = new Smarty_Internal_ParseTree_Text($this, st);
    } else {
        $this->lex->is_phpScript = false;
        if ($this->php_handling == Smarty::PHP_PASSTHRU) {
            res = new Smarty_Internal_ParseTree_Text($this, st);
        } elseif ($this->php_handling == Smarty::PHP_QUOTE) {
            res = new Smarty_Internal_ParseTree_Text($this, htmlspecialchars(st, ENT_QUOTES));
        } elseif ($this->php_handling == Smarty::PHP_ALLOW) {
            res = new Smarty_Internal_ParseTree_Tag($this, $this->compiler->processNocacheCode('?>', true));
        } elseif ($this->php_handling == Smarty::PHP_REMOVE) {
            res = null;
        }
    }
}

                      // '<%' tag
template_element(res)::= ASPSTARTTAG(st). {
    if ($this->php_handling == Smarty::PHP_PASSTHRU) {
        res = new Smarty_Internal_ParseTree_Text($this, st);
    } elseif ($this->php_handling == Smarty::PHP_QUOTE) {
        res = new Smarty_Internal_ParseTree_Text($this, htmlspecialchars(st, ENT_QUOTES));
    } elseif ($this->php_handling == Smarty::PHP_ALLOW) {
        if ($this->asp_tags) {
            if (!($this->smarty instanceof SmartyBC)) {
                $this->compiler->trigger_template_error (self::Err3);
            }
            res = new Smarty_Internal_ParseTree_Tag($this, $this->compiler->processNocacheCode('<%', true));
        } else {
            res = new Smarty_Internal_ParseTree_Text($this, st);
        }
    } elseif ($this->php_handling == Smarty::PHP_REMOVE) {
        if ($this->asp_tags) {
            res = null;
        } else {
            res = new Smarty_Internal_ParseTree_Text($this, st);
        }
    }
}
  
                      // '%>' tag
template_element(res)::= ASPENDTAG(st). {
    if ($this->php_handling == Smarty::PHP_PASSTHRU) {
        res = new Smarty_Internal_ParseTree_Text($this, st);
    } elseif ($this->php_handling == Smarty::PHP_QUOTE) {
        res = new Smarty_Internal_ParseTree_Text($this, htmlspecialchars('%>', ENT_QUOTES));
    } elseif ($this->php_handling == Smarty::PHP_ALLOW) {
        if ($this->asp_tags) {
            res = new Smarty_Internal_ParseTree_Tag($this, $this->compiler->processNocacheCode('%>', true));
        } else {
            res = new Smarty_Internal_ParseTree_Text($this, st);
        }
    } elseif ($this->php_handling == Smarty::PHP_REMOVE) {
        if ($this->asp_tags) {
            res = null;
        } else {
            res = new Smarty_Internal_ParseTree_Text($this, st);
        }
    }
}


                      // XML tag
template_element(res)::= XMLTAG. {
    $this->compiler->tag_nocache = true;
    $this->is_xml = true; 
    $save = $this->template->has_nocache_code; 
    res = new Smarty_Internal_ParseTree_Tag($this, $this->compiler->processNocacheCode("<?php echo '<?xml';?>", $this->compiler, true));
    $this->template->has_nocache_code = $save;
}

                      // template text
template_element(res)::= TEXT(o). {
        if ($this->strip) {
            res = new Smarty_Internal_ParseTree_Text($this, preg_replace('![\t ]*[\r\n]+[\t ]*!', '', o));
        } else {
            res = new Smarty_Internal_ParseTree_Text($this, o);
        }
}

                      // strip on
template_element ::= STRIPON(d). {
    $this->strip = true;
}
                      // strip off
template_element ::= STRIPOFF(d). {
    $this->strip = false;
}
                      // process source of inheritance child block
template_element ::= BLOCKSOURCE(s). {
        if ($this->strip) {
            SMARTY_INTERNAL_COMPILE_BLOCK::blockSource($this->compiler, preg_replace('![\t ]*[\r\n]+[\t ]*!', '', s));
        } else {
            SMARTY_INTERNAL_COMPILE_BLOCK::blockSource($this->compiler, s);
        }
}

                    // Litteral
literal(res) ::= LITERALSTART LITERALEND. {
    res = '';
}

literal(res) ::= LITERALSTART literal_elements(l) LITERALEND. {
    res = l;
}
 
literal_elements(res) ::= literal_elements(l1) literal_element(l2). {
    res = l1.l2;
}

literal_elements(res) ::= . {
    res = '';
}

literal_element(res) ::= literal(l). {
    res = l;
}

literal_element(res) ::= LITERAL(l). {
    res = l;
}

//
// output tags start here
//

                  // output with optional attributes
smartytag(res)   ::= LDEL value(e). {
    res = $this->compiler->compileTag('private_print_expression',array(),array('value'=>e));
}

smartytag(res)   ::= LDEL value(e) modifierlist(l) attributes(a). {
    res = $this->compiler->compileTag('private_print_expression',a,array('value'=>e, 'modifierlist'=>l));
}

smartytag(res)   ::= LDEL value(e) attributes(a). {
    res = $this->compiler->compileTag('private_print_expression',a,array('value'=>e));
}

smartytag(res)   ::= LDEL expr(e) modifierlist(l) attributes(a). {
    res = $this->compiler->compileTag('private_print_expression',a,array('value'=>e,'modifierlist'=>l));
}

smartytag(res)   ::= LDEL expr(e) attributes(a). {
    res = $this->compiler->compileTag('private_print_expression',a,array('value'=>e));
}

//
// Smarty tags start here
//

                  // assign new style
smartytag(res)   ::= LDEL DOLLAR ID(i) EQUAL value(e). {
    res = $this->compiler->compileTag('assign',array(array('value'=>e),array('var'=>"'".i."'")));
}
                  
smartytag(res)   ::= LDEL DOLLAR ID(i) EQUAL expr(e). {
    res = $this->compiler->compileTag('assign',array(array('value'=>e),array('var'=>"'".i."'")));
}
                 
smartytag(res)   ::= LDEL DOLLAR ID(i) EQUAL expr(e) attributes(a). {
    res = $this->compiler->compileTag('assign',array_merge(array(array('value'=>e),array('var'=>"'".i."'")),a));
}                  

smartytag(res)   ::= LDEL varindexed(vi) EQUAL expr(e) attributes(a). {
    res = $this->compiler->compileTag('assign',array_merge(array(array('value'=>e),array('var'=>vi['var'])),a),array('smarty_internal_index'=>vi['smarty_internal_index']));
} 
                 
                  // tag with optional Smarty2 style attributes
smartytag(res)   ::= LDEL ID(i) attributes(a). {
        if (defined(i)) {
            res = $this->compiler->compileTag('private_print_expression',a,array('value'=>i));
        } else {
            res = $this->compiler->compileTag(i,a);
        }
}
smartytag(res)   ::= LDEL ID(i). {
        if (defined(i)) {
            res = $this->compiler->compileTag('private_print_expression',array(),array('value'=>i));
        } else {
            res = $this->compiler->compileTag(i,array());
        }
}


                  // tag with modifier and optional Smarty2 style attributes
smartytag(res)   ::= LDEL ID(i) modifierlist(l)attributes(a). {
        if (defined(i)) {
            res = $this->compiler->compileTag('private_print_expression',a,array('value'=>i, 'modifierlist'=>l));
        } else {
            res = '<?php ob_start();?>'.$this->compiler->compileTag(i,a).'<?php echo ';
            res .= $this->compiler->compileTag('private_modifier',array(),array('modifierlist'=>l,'value'=>'ob_get_clean()')).';?>';
        }
}

                  // registered object tag
smartytag(res)   ::= LDEL ID(i) PTR ID(m) attributes(a). {
    res = $this->compiler->compileTag(i,a,array('object_method'=>m));
}

                  // registered object tag with modifiers
smartytag(res)   ::= LDEL ID(i) PTR ID(me) modifierlist(l) attributes(a). {
    res = '<?php ob_start();?>'.$this->compiler->compileTag(i,a,array('object_method'=>me)).'<?php echo ';
    res .= $this->compiler->compileTag('private_modifier',array(),array('modifierlist'=>l,'value'=>'ob_get_clean()')).';?>';
}

                  // {if}, {elseif} and {while} tag
smartytag(res)   ::= LDELIF(i) expr(ie). {
    $tag = trim(substr(i,$this->lex->ldel_length)); 
    res = $this->compiler->compileTag(($tag == 'else if')? 'elseif' : $tag,array(),array('if condition'=>ie));
}

smartytag(res)   ::= LDELIF(i) expr(ie) attributes(a). {
    $tag = trim(substr(i,$this->lex->ldel_length));
    res = $this->compiler->compileTag(($tag == 'else if')? 'elseif' : $tag,a,array('if condition'=>ie));
}

smartytag(res)   ::= LDELIF(i) statement(ie). {
    $tag = trim(substr(i,$this->lex->ldel_length));
    res = $this->compiler->compileTag(($tag == 'else if')? 'elseif' : $tag,array(),array('if condition'=>ie));
}

smartytag(res)   ::= LDELIF(i) statement(ie)  attributes(a). {
    $tag = trim(substr(i,$this->lex->ldel_length));
    res = $this->compiler->compileTag(($tag == 'else if')? 'elseif' : $tag,a,array('if condition'=>ie));
}

                  // {for} tag
smartytag(res)   ::= LDELFOR statements(st) SEMICOLON optspace expr(ie) SEMICOLON optspace DOLLAR varvar(v2) foraction(e2) attributes(a). {
    res = $this->compiler->compileTag('for',array_merge(a,array(array('start'=>st),array('ifexp'=>ie),array('var'=>v2),array('step'=>e2))),1);
}

  foraction(res)   ::= EQUAL expr(e). {
    res = '='.e;
}

  foraction(res)   ::= INCDEC(e). {
    res = e;
}

smartytag(res)   ::= LDELFOR statement(st) TO expr(v) attributes(a). {
    res = $this->compiler->compileTag('for',array_merge(a,array(array('start'=>st),array('to'=>v))),0);
}

smartytag(res)   ::= LDELFOR statement(st) TO expr(v) STEP expr(v2) attributes(a). {
    res = $this->compiler->compileTag('for',array_merge(a,array(array('start'=>st),array('to'=>v),array('step'=>v2))),0);
}

                  // {foreach} tag
smartytag(res)   ::= LDELFOREACH attributes(a). {
    res = $this->compiler->compileTag('foreach',a);
}

                  // {foreach $array as $var} tag
smartytag(res)   ::= LDELFOREACH SPACE value(v1) AS DOLLAR varvar(v0) attributes(a). {
    res = $this->compiler->compileTag('foreach',array_merge(a,array(array('from'=>v1),array('item'=>v0))));
}

smartytag(res)   ::= LDELFOREACH SPACE value(v1) AS DOLLAR varvar(v2) APTR DOLLAR varvar(v0) attributes(a). {
    res = $this->compiler->compileTag('foreach',array_merge(a,array(array('from'=>v1),array('item'=>v0),array('key'=>v2))));
}

smartytag(res)   ::= LDELFOREACH SPACE expr(e) AS DOLLAR varvar(v0) attributes(a). {
    res = $this->compiler->compileTag('foreach',array_merge(a,array(array('from'=>e),array('item'=>v0))));
}

smartytag(res)   ::= LDELFOREACH SPACE expr(e) AS DOLLAR varvar(v1) APTR DOLLAR varvar(v0) attributes(a). {
    res = $this->compiler->compileTag('foreach',array_merge(a,array(array('from'=>e),array('item'=>v0),array('key'=>v1))));
}

                  // {setfilter}
smartytag(res)   ::= LDELSETFILTER ID(m) modparameters(p). {
    res = $this->compiler->compileTag('setfilter',array(),array('modifier_list'=>array(array_merge(array(m),p))));
}

smartytag(res)   ::= LDELSETFILTER ID(m) modparameters(p) modifierlist(l). {
    res = $this->compiler->compileTag('setfilter',array(),array('modifier_list'=>array_merge(array(array_merge(array(m),p)),l)));
}

                  // {$smarty.block.child} or {$smarty.block.parent}
smartytag(res)   ::= LDEL SMARTYBLOCKCHILDPARENT(i). {
    $j = strrpos(i,'.');
    if (i[$j+1] == 'c') {
        // {$smarty.block.child}
        res = SMARTY_INTERNAL_COMPILE_BLOCK::compileChildBlock($this->compiler);
    } else {
        // {$smarty.block.parent}
        res = SMARTY_INTERNAL_COMPILE_BLOCK::compileParentBlock($this->compiler);
    }
}

                  
                  // end of block tag  {/....}                  
smartytag(res)   ::= LDELSLASH ID(i). {
    res = $this->compiler->compileTag(i.'close',array());
}

smartytag(res)   ::= LDELSLASH ID(i) modifierlist(l). {
    res = $this->compiler->compileTag(i.'close',array(),array('modifier_list'=>l));
}

                  // end of block object tag  {/....}                 
smartytag(res)   ::= LDELSLASH ID(i) PTR ID(m). {
    res = $this->compiler->compileTag(i.'close',array(),array('object_method'=>m));
}

smartytag(res)   ::= LDELSLASH ID(i) PTR ID(m) modifierlist(l). {
    res = $this->compiler->compileTag(i.'close',array(),array('object_method'=>m, 'modifier_list'=>l));
}

//
//Attributes of Smarty tags 
//
                  // list of attributes
attributes(res)  ::= attributes(a1) attribute(a2). {
    res = a1;
    res[] = a2;
}

                  // single attribute
attributes(res)  ::= attribute(a). {
    res = array(a);
}

                  // no attributes
attributes(res)  ::= . {
    res = array();
}
                  
                  // attribute
attribute(res)   ::= SPACE ID(v) EQUAL ID(id). {
    if (defined(id)) {
        res = array(v=>id);
    } else {
        res = array(v=>"'".id."'");
    }
}

attribute(res)   ::= ATTR(v) expr(e). {
    res = array(trim(v," =\n\r\t")=>e);
}

attribute(res)   ::= ATTR(v) value(e). {
    res = array(trim(v," =\n\r\t")=>e);
}

attribute(res)   ::= SPACE ID(v). {
    res = "'".v."'";
}

attribute(res)   ::= SPACE expr(e). {
    res = e;
}

attribute(res)   ::= SPACE value(v). {
    res = v;
}

attribute(res)   ::= SPACE INTEGER(i) EQUAL expr(e). {
    res = array(i=>e);
}

                  

//
// statement
//
statements(res)   ::= statement(s). {
    res = array(s);
}

statements(res)   ::= statements(s1) COMMA statement(s). {
    s1[]=s;
    res = s1;
}

statement(res)    ::= DOLLAR varvar(v) EQUAL expr(e). {
    res = array('var' => v, 'value'=>e);
}

statement(res)    ::= varindexed(vi) EQUAL expr(e). {
    res = array('var' => vi, 'value'=>e);
}

statement(res)    ::= OPENP statement(st) CLOSEP. {
    res = st;
}


//
// expressions
//

                  // single value
expr(res)        ::= value(v). {
    res = v;
}

                 // ternary
expr(res)        ::= ternary(v). {
    res = v;
}

                 // resources/streams
expr(res)        ::= DOLLAR ID(i) COLON ID(i2). {
    res = '$_smarty_tpl->getStreamVariable(\''. i .'://'. i2 . '\')';
}

                  // arithmetic expression
expr(res)        ::= expr(e) MATH(m) value(v). {
    res = e . trim(m) . v;
}

expr(res)        ::= expr(e) UNIMATH(m) value(v). {
    res = e . trim(m) . v;
}
 
                  // bit operation 
expr(res)        ::= expr(e) ANDSYM(m) value(v). {
    res = e . trim(m) . v;
} 

                  // array
expr(res)       ::= array(a). {
    res = a;
}

                  // modifier
expr(res)        ::= expr(e) modifierlist(l). {
    res = $this->compiler->compileTag('private_modifier',array(),array('value'=>e,'modifierlist'=>l));
}

// if expression
                    // simple expression
expr(res)        ::= expr(e1) ifcond(c) expr(e2). {
    res = e1.c.e2;
}

expr(res)        ::= expr(e1) ISIN array(a).  {
    res = 'in_array('.e1.','.a.')';
}

expr(res)        ::= expr(e1) ISIN value(v).  {
    res = 'in_array('.e1.',(array)'.v.')';
}

expr(res)        ::= expr(e1) lop(o) expr(e2).  {
    res = e1.o.e2;
}

expr(res)        ::= expr(e1) ISDIVBY expr(e2). {
    res = '!('.e1.' % '.e2.')';
}

expr(res)        ::= expr(e1) ISNOTDIVBY expr(e2).  {
    res = '('.e1.' % '.e2.')';
}

expr(res)        ::= expr(e1) ISEVEN. {
    res = '!(1 & '.e1.')';
}

expr(res)        ::= expr(e1) ISNOTEVEN.  {
    res = '(1 & '.e1.')';
}

expr(res)        ::= expr(e1) ISEVENBY expr(e2).  {
    res = '!(1 & '.e1.' / '.e2.')';
}

expr(res)        ::= expr(e1) ISNOTEVENBY expr(e2). {
    res = '(1 & '.e1.' / '.e2.')';
}

expr(res)        ::= expr(e1) ISODD.  {
    res = '(1 & '.e1.')';
}

expr(res)        ::= expr(e1) ISNOTODD. {
    res = '!(1 & '.e1.')';
}

expr(res)        ::= expr(e1) ISODDBY expr(e2). {
    res = '(1 & '.e1.' / '.e2.')';
}

expr(res)        ::= expr(e1) ISNOTODDBY expr(e2).  {
    res = '!(1 & '.e1.' / '.e2.')';
}

expr(res)        ::= variable(v1) INSTANCEOF(i) ns1(v2). {
      res = v1.i.v2;
    }


//
// ternary
//
ternary(res)        ::= OPENP expr(v) CLOSEP  QMARK DOLLAR ID(e1) COLON  expr(e2). {
    res = v.' ? '. $this->compileVariable("'".e1."'") . ' : '.e2;
}

ternary(res)        ::= OPENP expr(v) CLOSEP  QMARK  expr(e1) COLON  expr(e2). {
    res = v.' ? '.e1.' : '.e2;
}

                 // value
value(res)       ::= variable(v). {
    res = v;
}

                  // +/- value
value(res)        ::= UNIMATH(m) value(v). {
    res = m.v;
}

                  // logical negation
value(res)       ::= NOT value(v). {
    res = '!'.v;
}

value(res)       ::= TYPECAST(t) value(v). {
    res = t.v;
}

value(res)       ::= variable(v) INCDEC(o). {
    res = v.o;
}

                 // numeric
value(res)       ::= HEX(n). {
    res = n;
}

value(res)       ::= INTEGER(n). {
    res = n;
}

value(res)       ::= INTEGER(n1) DOT INTEGER(n2). {
    res = n1.'.'.n2;
}

value(res)       ::= INTEGER(n1) DOT. {
    res = n1.'.';
}

value(res)       ::= DOT INTEGER(n1). {
    res = '.'.n1;
}

                 // ID, true, false, null
value(res)       ::= ID(id). {
    if (defined(id)) {
        res = id;
    } else {
        res = "'".id."'";
    }
}

                  // function call
value(res)       ::= function(f). {
    res = f;
}

                  // expression
value(res)       ::= OPENP expr(e) CLOSEP. {
    res = "(". e .")";
}

                  // singele quoted string
value(res)       ::= SINGLEQUOTESTRING(t). {
    res = t;
}

                  // double quoted string
value(res)       ::= doublequoted_with_quotes(s). {
    res = s;
}


value(res)    ::= varindexed(vi) DOUBLECOLON static_class_access(r). {
    self::$prefix_number++;
    if (vi['var'] == '\'smarty\'') {
        $this->compiler->prefix_code[] = '<?php $_tmp'.self::$prefix_number.' = '. $this->compiler->compileTag('private_special_variable',array(),vi['smarty_internal_index']).';?>';
    } else {
        $this->compiler->prefix_code[] = '<?php $_tmp'.self::$prefix_number.' = '. $this->compileVariable(vi['var']).vi['smarty_internal_index'].';?>';
    }
    res = '$_tmp'.self::$prefix_number.'::'.r[0].r[1];

}

                  // Smarty tag
value(res)       ::= smartytag(st) RDEL. {
    self::$prefix_number++;
    $this->compiler->prefix_code[] = '<?php ob_start();?>'.st.'<?php $_tmp'.self::$prefix_number.'=ob_get_clean();?>';
    res = '$_tmp'.self::$prefix_number;
}

value(res)       ::= value(v) modifierlist(l). {
    res = $this->compiler->compileTag('private_modifier',array(),array('value'=>v,'modifierlist'=>l));
}
                  // name space constant
value(res)       ::= NAMESPACE(c). {
    res = c;
}


                  // static class access
value(res)       ::= ns1(c)DOUBLECOLON static_class_access(s). {
    if (!in_array(strtolower(c), array('self', 'parent')) && (!$this->security || $this->smarty->security_policy->isTrustedStaticClassAccess(c, s, $this->compiler))) {
        if (isset($this->smarty->registered_classes[c])) {
            res = $this->smarty->registered_classes[c].'::'.s[0].s[1];
        } else {
            res = c.'::'.s[0].s[1];
        } 
    } else {
        $this->compiler->trigger_template_error ("static class '".c."' is undefined or not allowed by security setting");
    }
}
//
// namespace stuff
//

ns1(res)           ::= ID(i). {
    res = i;
}

ns1(res)           ::= NAMESPACE(i). {
    res = i;
}

ns1(res)           ::= variable(v). {
    res = v;
}




//
// variables 
//
                  // Smarty variable (optional array)
variable(res)    ::= varindexed(vi). {
    if (vi['var'] == '\'smarty\'') {
        $smarty_var = $this->compiler->compileTag('private_special_variable',array(),vi['smarty_internal_index']);
        res = $smarty_var;
    } else {
        // used for array reset,next,prev,end,current 
        $this->last_variable = vi['var'];
        $this->last_index = vi['smarty_internal_index'];
        res = $this->compileVariable(vi['var']).vi['smarty_internal_index'];
    }
}

                  // variable with property
variable(res)    ::= DOLLAR varvar(v) AT ID(p). {
    res = '$_smarty_tpl->tpl_vars['. v .']->'.p;
}

                  // object
variable(res)    ::= object(o). {
    res = o;
}

                  // config variable
variable(res)    ::= HATCH ID(i) HATCH. {
    res = '$_smarty_tpl->getConfigVariable( \''. i .'\')';
}

variable(res)    ::= HATCH ID(i) HATCH arrayindex(a). {
    res = '(is_array($tmp = $_smarty_tpl->getConfigVariable( \''. i .'\')) ? $tmp'.a.' :null)';
}

variable(res)    ::= HATCH variable(v) HATCH. {
    res = '$_smarty_tpl->getConfigVariable( '. v .')';
}

variable(res)    ::= HATCH variable(v) HATCH arrayindex(a). {
    res = '(is_array($tmp = $_smarty_tpl->getConfigVariable( '. v .')) ? $tmp'.a.' : null)';
}

varindexed(res)  ::= DOLLAR varvar(v) arrayindex(a). {
    res = array('var'=>v, 'smarty_internal_index'=>a);
}

//
// array index
//
                    // multiple array index
arrayindex(res)  ::= arrayindex(a1) indexdef(a2). {
    res = a1.a2;
}

                    // no array index
arrayindex        ::= . {
    return;
}

// single index definition
                    // Smarty2 style index 
indexdef(res)    ::= DOT DOLLAR varvar(v).  {
    res = '['.$this->compileVariable(v).']';
}

indexdef(res)    ::= DOT DOLLAR varvar(v) AT ID(p). {
    res = '['.$this->compileVariable(v).'->'.p.']';
}

indexdef(res)   ::= DOT ID(i). {
    if (defined(i)) {
            res = "[". i ."]";
        } else {
            res = "['". i ."']";
        }
}

indexdef(res)   ::= DOT INTEGER(n). {
    res = "[". n ."]";
}


indexdef(res)   ::= DOT LDEL expr(e) RDEL. {
    res = "[". e ."]";
}

                    // section tag index
indexdef(res)   ::= OPENB ID(i)CLOSEB. {
    res = '['.$this->compiler->compileTag('private_special_variable',array(),'[\'section\'][\''.i.'\'][\'index\']').']';
}

indexdef(res)   ::= OPENB ID(i) DOT ID(i2) CLOSEB. {
    res = '['.$this->compiler->compileTag('private_special_variable',array(),'[\'section\'][\''.i.'\'][\''.i2.'\']').']';
}

                    // PHP style index
indexdef(res)   ::= OPENB expr(e) CLOSEB. {
    res = "[". e ."]";
}

                    // for assign append array
indexdef(res)  ::= OPENB CLOSEB. {
    res = '[]';
}


//
// variable variable names
//
                    // singel identifier element
varvar(res)      ::= varvarele(v). {
    res = v;
}

                    // sequence of identifier elements
varvar(res)      ::= varvar(v1) varvarele(v2). {
    res = v1.'.'.v2;
}

                    // fix sections of element
varvarele(res)   ::= ID(s). {
    res = '\''.s.'\'';
}

                    // variable sections of element
varvarele(res)   ::= LDEL expr(e) RDEL. {
    res = '('.e.')';
}

//
// objects
//
object(res)    ::= varindexed(vi) objectchain(oc). {
    if (vi['var'] == '\'smarty\'') {
        res =  $this->compiler->compileTag('private_special_variable',array(),vi['smarty_internal_index']).oc;
    } else {
        res = $this->compileVariable(vi['var']).vi['smarty_internal_index'].oc;
    }
}

                    // single element
objectchain(res) ::= objectelement(oe). {
    res  = oe;
}

                    // chain of elements 
objectchain(res) ::= objectchain(oc) objectelement(oe). {
    res  = oc.oe;
}

                    // variable
objectelement(res)::= PTR ID(i) arrayindex(a). {
    if ($this->security && substr(i,0,1) == '_') {
        $this->compiler->trigger_template_error (self::Err1);
    }
    res = '->'.i.a;
}

objectelement(res)::= PTR DOLLAR varvar(v) arrayindex(a). {
    if ($this->security) {
        $this->compiler->trigger_template_error (self::Err2);
    }
    res = '->{'.$this->compileVariable(v).a.'}';
}

objectelement(res)::= PTR LDEL expr(e) RDEL arrayindex(a). {
    if ($this->security) {
        $this->compiler->trigger_template_error (self::Err2);
    }
    res = '->{'.e.a.'}';
}

objectelement(res)::= PTR ID(ii) LDEL expr(e) RDEL arrayindex(a). {
    if ($this->security) {
        $this->compiler->trigger_template_error (self::Err2);
    }
    res = '->{\''.ii.'\'.'.e.a.'}';
}

                    // method
objectelement(res)::= PTR method(f).  {
    res = '->'.f;
}


//
// function
//
function(res)     ::= ns1(f) OPENP params(p) CLOSEP. {
    if (!$this->security || $this->smarty->security_policy->isTrustedPhpFunction(f, $this->compiler)) {
        if (strcasecmp(f,'isset') === 0 || strcasecmp(f,'empty') === 0 || strcasecmp(f,'array') === 0 || is_callable(f)) {
            $func_name = strtolower(f);
            if ($func_name == 'isset') {
                if (count(p) == 0) {
                    $this->compiler->trigger_template_error ('Illegal number of paramer in "isset()"');
                }
                $par = implode(',',p);
                if (strncasecmp($par,'$_smarty_tpl->getConfigVariable',strlen('$_smarty_tpl->getConfigVariable')) === 0) {
                    self::$prefix_number++;
                    $this->compiler->prefix_code[] = '<?php $_tmp'.self::$prefix_number.'='.str_replace(')',', false)',$par).';?>';
                    $isset_par = '$_tmp'.self::$prefix_number;
                } else {
                    $isset_par=str_replace("')->value","',null,true,false)->value",$par);
                }
                res = f . "(". $isset_par .")";
            } elseif (in_array($func_name,array('empty','reset','current','end','prev','next'))){
                if (count(p) != 1) {
                    $this->compiler->trigger_template_error ('Illegal number of paramer in "empty()"');
                }
                if ($func_name == 'empty') {
                    res = $func_name.'('.str_replace("')->value","',null,true,false)->value",p[0]).')';
                } else {
                    res = $func_name.'('.p[0].')';
                }
            } else {
                res = f . "(". implode(',',p) .")";
            }
        } else {
            $this->compiler->trigger_template_error ("unknown function \"" . f . "\"");
        }
    }
}


//
// method
//
method(res)     ::= ID(f) OPENP params(p) CLOSEP. {
    if ($this->security && substr(f,0,1) == '_') {
        $this->compiler->trigger_template_error (self::Err1);
    }
    res = f . "(". implode(',',p) .")";
}

method(res)     ::= DOLLAR ID(f) OPENP params(p) CLOSEP.  {
    if ($this->security) {
        $this->compiler->trigger_template_error (self::Err2);
    }
    self::$prefix_number++;
    $this->compiler->prefix_code[] = '<?php $_tmp'.self::$prefix_number.'='.$this->compileVariable("'".f."'").';?>';
    res = '$_tmp'.self::$prefix_number.'('. implode(',',p) .')';
}

// function/method parameter
                    // multiple parameters
params(res)       ::= params(p) COMMA expr(e). {
    res = array_merge(p,array(e));
}

                    // single parameter
params(res)       ::= expr(e). {
    res = array(e);
}

                    // kein parameter
params(res)       ::= . {
    res = array();
}

//
// modifier
// 
modifierlist(res) ::= modifierlist(l) modifier(m) modparameters(p). {
    res = array_merge(l,array(array_merge(m,p)));
}

modifierlist(res) ::= modifier(m) modparameters(p). {
    res = array(array_merge(m,p));
}
 
modifier(res)    ::= VERT AT ID(m). {
    res = array(m);
}

modifier(res)    ::= VERT ID(m). {
    res =  array(m);
}

//
// modifier parameter
//
                    // multiple parameter
modparameters(res) ::= modparameters(mps) modparameter(mp). {
    res = array_merge(mps,mp);
}

                    // no parameter
modparameters(res)      ::= . {
    res = array();
}

                    // parameter expression
modparameter(res) ::= COLON value(mp). {
    res = array(mp);
}

modparameter(res) ::= COLON array(mp). {
    res = array(mp);
}

                  // static class methode call
static_class_access(res)       ::= method(m). {
    res = array(m, '', 'method');
}

                  // static class methode call with object chainig
static_class_access(res)       ::= method(m) objectchain(oc). {
    res = array(m, oc, 'method');
}

                  // static class constant
static_class_access(res)       ::= ID(v). {
    res = array(v, '');
}

                  // static class variables
static_class_access(res)       ::=  DOLLAR ID(v) arrayindex(a). {
    res = array('$'.v, a, 'property');
}

                  // static class variables with object chain
static_class_access(res)       ::= DOLLAR ID(v) arrayindex(a) objectchain(oc). {
    res = array('$'.v, a.oc, 'property');
}


// if conditions and operators
ifcond(res)        ::= EQUALS. {
    res = '==';
}

ifcond(res)        ::= NOTEQUALS. {
    res = '!=';
}

ifcond(res)        ::= GREATERTHAN. {
    res = '>';
}

ifcond(res)        ::= LESSTHAN. {
    res = '<';
}

ifcond(res)        ::= GREATEREQUAL. {
    res = '>=';
}

ifcond(res)        ::= LESSEQUAL. {
    res = '<=';
}

ifcond(res)        ::= IDENTITY. {
    res = '===';
}

ifcond(res)        ::= NONEIDENTITY. {
    res = '!==';
}

ifcond(res)        ::= MOD. {
    res = '%';
}

lop(res)        ::= LAND. {
    res = '&&';
}

lop(res)        ::= LOR. {
    res = '||';
}

lop(res)        ::= LXOR. {
    res = ' XOR ';
}

//
// ARRAY element assignment
//
array(res)           ::=  OPENB arrayelements(a) CLOSEB.  {
    res = 'array('.a.')';
}

arrayelements(res)   ::=  arrayelement(a).  {
    res = a;
}

arrayelements(res)   ::=  arrayelements(a1) COMMA arrayelement(a).  {
    res = a1.','.a;
}

arrayelements        ::=  .  {
    return;
}

arrayelement(res)    ::=  value(e1) APTR expr(e2). {
    res = e1.'=>'.e2;
}

arrayelement(res)    ::=  ID(i) APTR expr(e2). { 
    res = '\''.i.'\'=>'.e2;
}

arrayelement(res)    ::=  expr(e). {
    res = e;
}


//
// double qouted strings
//
doublequoted_with_quotes(res) ::= QUOTE QUOTE. {
    res = "''";
}

doublequoted_with_quotes(res) ::= QUOTE doublequoted(s) QUOTE. {
    res = s->to_smarty_php();
}


doublequoted(res)          ::= doublequoted(o1) doublequotedcontent(o2). {
    o1->append_subtree(o2);
    res = o1;
}

doublequoted(res)          ::= doublequotedcontent(o). {
    res = new Smarty_Internal_ParseTree_Dq($this, o);
}

doublequotedcontent(res)           ::=  BACKTICK variable(v) BACKTICK. {
    res = new Smarty_Internal_ParseTree_Code($this, '(string)'.v);
}

doublequotedcontent(res)           ::=  BACKTICK expr(e) BACKTICK. {
    res = new Smarty_Internal_ParseTree_Code($this, '(string)'.e);
}

doublequotedcontent(res)           ::=  DOLLARID(i). {
    res = new Smarty_Internal_ParseTree_Code($this, '(string)$_smarty_tpl->tpl_vars[\''. substr(i,1) .'\']->value');
}

doublequotedcontent(res)           ::=  LDEL variable(v) RDEL. {
    res = new Smarty_Internal_ParseTree_Code($this, '(string)'.v);
}

doublequotedcontent(res)           ::=  LDEL expr(e) RDEL. {
    res = new Smarty_Internal_ParseTree_Code($this, '(string)('.e.')');
}

doublequotedcontent(res)     ::=  smartytag(st) RDEL. {
    res = new Smarty_Internal_ParseTree_Tag($this, st);
}

doublequotedcontent(res)           ::=  TEXT(o). {
    res = new Smarty_Internal_ParseTree_DqContent($this, o);
}


//
// optional space
//
optspace(res)     ::= SPACE(s).  {
    res = s;
}

optspace(res)     ::= .          {
    res = '';
}
