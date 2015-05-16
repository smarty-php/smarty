<?php

class TP_yyToken implements ArrayAccess
{

    public $string = '';

    public $metadata = array();

    public function __construct($s, $m = array())
    {
        if ($s instanceof TP_yyToken) {
            $this->string = $s->string;
            $this->metadata = $s->metadata;
        } else {
            $this->string = (string) $s;
            if ($m instanceof TP_yyToken) {
                $this->metadata = $m->metadata;
            } elseif (is_array($m)) {
                $this->metadata = $m;
            }
        }
    }

    public function __toString()
    {
        return $this->string;
    }

    public function offsetExists($offset)
    {
        return isset($this->metadata[$offset]);
    }

    public function offsetGet($offset)
    {
        return $this->metadata[$offset];
    }

    public function offsetSet($offset, $value)
    {
        if ($offset === null) {
            if (isset($value[0])) {
                $x = ($value instanceof TP_yyToken) ? $value->metadata : $value;
                $this->metadata = array_merge($this->metadata, $x);

                return;
            }
            $offset = count($this->metadata);
        }
        if ($value === null) {
            return;
        }
        if ($value instanceof TP_yyToken) {
            if ($value->metadata) {
                $this->metadata[$offset] = $value->metadata;
            }
        } elseif ($value) {
            $this->metadata[$offset] = $value;
        }
    }

    public function offsetUnset($offset)
    {
        unset($this->metadata[$offset]);
    }
}

class TP_yyStackEntry
{

    public $stateno;       /* The state-number */
    public $major;         /* The major token value.  This is the code
                     ** number for the token at this stack level */
    public $minor; /* The user-supplied minor token value.  This
                     ** is the value of the token  */
}

;

#line 13 "../smarty/lexer/smarty_internal_templateparser.y"

/**
 * Smarty Internal Plugin Templateparser
 *
 * This is the template parser.
 * It is generated from the smarty_internal_templateparser.y file
 *
 * @package    Smarty
 * @subpackage Compiler
 * @author     Uwe Tews
 */
class Smarty_Internal_Templateparser
{

    #line 26 "../smarty/lexer/smarty_internal_templateparser.y"

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
    public $strip = false;

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
     * security object
     *
     * @var Smarty_Security
     */
    private $security = null;

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
     *  merge PHP code with prefix code and return parse tree tag object
     *
     * @param string $code
     *
     * @return Smarty_Internal_ParseTree_Tag
     */
    public function mergePrefixCode($code)
    {
        $tmp = '';
        foreach ($this->compiler->prefix_code as $preCode) {
            $tmp = empty($tmp) ? $preCode : $this->compiler->appendCode($tmp, $preCode);
        }
        $this->compiler->prefix_code = array();
        $tmp = empty($tmp) ? $code : $this->compiler->appendCode($tmp, $code);
        return new Smarty_Internal_ParseTree_Tag($this, $this->compiler->processNocacheCode($tmp, true));
    }

    const TP_VERT = 1;

    const TP_COLON = 2;

    const TP_RDEL = 3;

    const TP_COMMENT = 4;

    const TP_PHP = 5;

    const TP_XMLTAG = 6;

    const TP_TEXT = 7;

    const TP_STRIPON = 8;

    const TP_STRIPOFF = 9;

    const TP_BLOCKSOURCE = 10;

    const TP_LITERALSTART = 11;

    const TP_LITERALEND = 12;

    const TP_LITERAL = 13;

    const TP_LDEL = 14;

    const TP_DOLLAR = 15;

    const TP_ID = 16;

    const TP_EQUAL = 17;

    const TP_PTR = 18;

    const TP_LDELIF = 19;

    const TP_LDELFOR = 20;

    const TP_SEMICOLON = 21;

    const TP_INCDEC = 22;

    const TP_TO = 23;

    const TP_STEP = 24;

    const TP_LDELFOREACH = 25;

    const TP_SPACE = 26;

    const TP_AS = 27;

    const TP_APTR = 28;

    const TP_LDELSETFILTER = 29;

    const TP_SMARTYBLOCKCHILDPARENT = 30;

    const TP_LDELSLASH = 31;

    const TP_ATTR = 32;

    const TP_INTEGER = 33;

    const TP_COMMA = 34;

    const TP_OPENP = 35;

    const TP_CLOSEP = 36;

    const TP_MATH = 37;

    const TP_UNIMATH = 38;

    const TP_ANDSYM = 39;

    const TP_ISIN = 40;

    const TP_INSTANCEOF = 41;

    const TP_QMARK = 42;

    const TP_NOT = 43;

    const TP_TYPECAST = 44;

    const TP_HEX = 45;

    const TP_DOT = 46;

    const TP_SINGLEQUOTESTRING = 47;

    const TP_DOUBLECOLON = 48;

    const TP_NAMESPACE = 49;

    const TP_AT = 50;

    const TP_HATCH = 51;

    const TP_OPENB = 52;

    const TP_CLOSEB = 53;

    const TP_LOGOP = 54;

    const TP_TLOGOP = 55;

    const TP_SINGLECOND = 56;

    const TP_QUOTE = 57;

    const TP_BACKTICK = 58;

    const TP_DOLLARID = 59;

    const YY_NO_ACTION = 519;

    const YY_ACCEPT_ACTION = 518;

    const YY_ERROR_ACTION = 517;

    const YY_SZ_ACTTAB = 1543;

    static public $yy_action = array(10, 121, 77, 199, 306, 5, 94, 106, 20, 8, 270, 130, 13, 112, 161, 234, 305, 240, 83, 230, 24, 31, 242, 265, 40, 20, 12, 270, 17, 43, 42, 266, 216, 323, 275, 211, 443, 92, 1, 10, 114, 198, 79, 82, 5, 94, 166, 88, 163, 443, 130, 186, 182, 171, 234, 280, 240, 280, 230, 81, 31, 227, 280, 40, 127, 36, 319, 268, 43, 42, 266, 216, 323, 192, 211, 189, 92, 1, 262, 314, 189, 158, 82, 10, 107, 204, 9, 167, 5, 94, 280, 5, 94, 20, 130, 270, 280, 130, 234, 209, 240, 234, 230, 240, 16, 24, 265, 40, 20, 269, 270, 12, 43, 42, 266, 216, 323, 34, 211, 443, 92, 1, 10, 114, 204, 79, 82, 5, 94, 304, 30, 307, 443, 130, 453, 264, 182, 234, 6, 240, 453, 230, 302, 31, 223, 20, 40, 270, 17, 36, 319, 43, 42, 266, 216, 323, 20, 211, 270, 92, 1, 10, 114, 195, 189, 82, 5, 94, 24, 235, 214, 20, 130, 270, 12, 233, 234, 23, 240, 253, 208, 20, 31, 270, 33, 40, 179, 294, 293, 261, 43, 42, 266, 216, 323, 209, 211, 356, 92, 1, 10, 114, 194, 209, 82, 5, 94, 227, 89, 168, 136, 130, 120, 95, 315, 234, 37, 240, 280, 230, 24, 31, 329, 330, 40, 144, 12, 325, 24, 43, 42, 266, 216, 323, 12, 211, 189, 92, 1, 10, 119, 204, 226, 82, 5, 94, 11, 128, 298, 24, 130, 107, 327, 199, 234, 12, 240, 29, 230, 8, 31, 14, 144, 40, 273, 299, 178, 316, 43, 42, 266, 216, 323, 209, 211, 393, 92, 1, 10, 124, 204, 160, 82, 5, 94, 328, 23, 92, 310, 130, 280, 21, 125, 234, 19, 240, 185, 230, 24, 7, 12, 265, 40, 269, 12, 27, 15, 43, 42, 266, 216, 323, 441, 211, 443, 92, 1, 10, 114, 202, 79, 82, 5, 94, 189, 441, 192, 443, 130, 176, 316, 23, 234, 252, 240, 238, 230, 187, 31, 22, 144, 40, 228, 243, 206, 233, 43, 42, 266, 216, 323, 209, 211, 396, 92, 1, 10, 107, 204, 157, 82, 5, 94, 287, 222, 285, 246, 130, 280, 292, 125, 234, 291, 240, 215, 230, 396, 16, 175, 134, 40, 269, 396, 129, 300, 43, 42, 266, 216, 323, 265, 211, 320, 92, 2, 201, 212, 278, 324, 82, 325, 397, 111, 443, 286, 282, 283, 289, 290, 296, 297, 179, 174, 313, 10, 219, 443, 215, 275, 5, 94, 280, 132, 184, 397, 130, 129, 215, 172, 234, 397, 240, 117, 441, 269, 73, 129, 280, 201, 212, 278, 324, 136, 325, 279, 263, 441, 159, 200, 212, 278, 324, 215, 325, 217, 192, 280, 117, 441, 325, 73, 129, 155, 205, 309, 93, 180, 86, 156, 279, 263, 441, 441, 200, 212, 278, 324, 280, 325, 215, 36, 319, 177, 221, 133, 441, 442, 67, 129, 312, 249, 3, 107, 218, 189, 189, 279, 263, 154, 442, 200, 212, 278, 324, 110, 325, 20, 280, 270, 215, 189, 268, 213, 18, 133, 181, 316, 67, 129, 518, 52, 254, 222, 285, 246, 192, 279, 263, 92, 215, 200, 212, 278, 324, 133, 325, 118, 67, 129, 209, 37, 20, 207, 245, 103, 108, 279, 263, 105, 215, 200, 212, 278, 324, 133, 325, 122, 56, 101, 231, 275, 20, 203, 220, 275, 99, 279, 263, 244, 224, 200, 212, 278, 324, 215, 325, 209, 284, 317, 133, 151, 275, 48, 101, 116, 39, 41, 44, 38, 90, 169, 279, 263, 209, 85, 200, 212, 278, 324, 280, 325, 24, 332, 333, 331, 209, 209, 12, 361, 164, 275, 269, 39, 41, 44, 38, 215, 189, 209, 229, 28, 133, 232, 226, 56, 129, 165, 408, 408, 332, 333, 331, 260, 279, 263, 280, 126, 200, 212, 278, 324, 35, 325, 24, 209, 209, 399, 366, 20, 12, 237, 269, 318, 192, 39, 41, 44, 38, 146, 140, 269, 441, 241, 408, 408, 408, 408, 123, 87, 399, 24, 332, 333, 331, 441, 399, 12, 143, 215, 441, 408, 408, 408, 133, 275, 238, 68, 129, 209, 215, 144, 22, 441, 275, 133, 279, 263, 69, 129, 200, 212, 278, 324, 209, 325, 399, 279, 263, 183, 150, 200, 212, 278, 324, 215, 325, 4, 84, 280, 133, 125, 288, 47, 129, 39, 41, 44, 38, 399, 225, 97, 279, 263, 275, 399, 200, 212, 278, 324, 215, 325, 332, 333, 331, 133, 322, 275, 55, 129, 228, 250, 257, 140, 228, 231, 274, 279, 263, 188, 215, 200, 212, 278, 324, 133, 325, 277, 76, 129, 215, 113, 20, 6, 210, 135, 255, 279, 263, 129, 215, 200, 212, 278, 324, 133, 325, 189, 70, 129, 248, 201, 212, 278, 324, 153, 325, 279, 263, 267, 191, 200, 212, 278, 324, 215, 325, 276, 271, 32, 133, 109, 272, 66, 129, 148, 137, 321, 26, 308, 152, 147, 279, 263, 273, 215, 200, 212, 278, 324, 133, 325, 281, 54, 129, 215, 144, 149, 269, 311, 141, 104, 279, 263, 129, 215, 200, 212, 278, 324, 133, 325, 326, 75, 129, 295, 201, 212, 278, 324, 91, 325, 279, 263, 247, 288, 200, 212, 278, 324, 215, 325, 288, 288, 288, 80, 288, 288, 49, 102, 288, 288, 288, 288, 288, 288, 288, 279, 263, 288, 215, 200, 212, 278, 324, 133, 325, 288, 51, 129, 215, 288, 288, 288, 288, 139, 288, 279, 263, 129, 215, 200, 212, 278, 324, 133, 325, 288, 59, 129, 288, 201, 212, 278, 324, 288, 325, 279, 263, 288, 288, 200, 212, 278, 324, 215, 325, 288, 288, 288, 133, 288, 288, 60, 129, 288, 288, 288, 288, 288, 288, 288, 279, 263, 288, 215, 196, 212, 278, 324, 133, 325, 288, 58, 129, 215, 288, 288, 288, 288, 138, 288, 279, 263, 129, 215, 200, 212, 278, 324, 80, 325, 288, 45, 102, 288, 201, 212, 278, 324, 288, 325, 279, 263, 288, 288, 197, 212, 278, 324, 215, 325, 288, 288, 288, 98, 288, 288, 64, 129, 288, 288, 288, 288, 288, 288, 288, 279, 263, 288, 215, 200, 212, 278, 324, 96, 325, 288, 53, 129, 215, 288, 288, 288, 288, 131, 288, 279, 263, 129, 215, 200, 212, 278, 324, 133, 325, 288, 46, 129, 288, 201, 212, 278, 324, 288, 325, 279, 263, 288, 288, 200, 212, 278, 324, 215, 325, 182, 145, 288, 133, 288, 288, 61, 129, 288, 288, 280, 288, 288, 36, 319, 279, 263, 288, 215, 200, 212, 278, 324, 133, 325, 288, 71, 129, 189, 288, 288, 288, 288, 288, 288, 279, 263, 288, 215, 200, 212, 278, 324, 133, 325, 288, 63, 129, 288, 288, 288, 288, 288, 288, 288, 279, 263, 288, 288, 200, 212, 278, 324, 215, 325, 182, 173, 288, 115, 288, 288, 50, 129, 288, 288, 280, 288, 288, 36, 319, 279, 263, 288, 215, 200, 212, 278, 324, 133, 325, 288, 78, 129, 189, 288, 288, 288, 288, 288, 288, 279, 263, 288, 215, 200, 212, 278, 324, 100, 325, 288, 72, 129, 288, 288, 288, 288, 288, 288, 288, 279, 263, 288, 288, 200, 212, 278, 324, 215, 325, 182, 162, 288, 133, 288, 288, 74, 129, 288, 288, 280, 288, 288, 36, 319, 279, 263, 288, 215, 200, 212, 278, 324, 133, 325, 288, 65, 129, 189, 288, 288, 288, 288, 288, 288, 279, 263, 288, 215, 200, 212, 278, 324, 133, 325, 288, 57, 129, 288, 288, 288, 288, 288, 288, 288, 279, 263, 209, 288, 200, 212, 278, 324, 215, 325, 288, 288, 288, 133, 288, 288, 62, 129, 288, 288, 288, 288, 288, 288, 288, 279, 263, 24, 288, 200, 212, 278, 324, 12, 325, 288, 209, 288, 39, 41, 44, 38, 288, 288, 288, 288, 314, 288, 288, 288, 288, 288, 288, 9, 288, 332, 333, 331, 5, 94, 288, 288, 288, 288, 130, 288, 288, 288, 234, 288, 240, 256, 39, 41, 44, 38, 288, 288, 288, 288, 288, 288, 288, 288, 288, 288, 288, 288, 288, 332, 333, 331, 288, 209, 215, 259, 303, 30, 307, 142, 209, 288, 288, 129, 288, 209, 288, 190, 288, 288, 288, 288, 258, 288, 288, 201, 212, 278, 324, 288, 325, 288, 288, 288, 288, 288, 288, 288, 288, 39, 41, 44, 38, 209, 288, 288, 39, 41, 44, 38, 288, 39, 41, 44, 38, 288, 332, 333, 331, 288, 288, 288, 334, 332, 333, 331, 209, 25, 332, 333, 331, 209, 288, 251, 288, 209, 288, 193, 239, 39, 41, 44, 38, 288, 288, 288, 288, 288, 288, 288, 288, 288, 288, 288, 288, 288, 332, 333, 331, 209, 288, 288, 39, 41, 44, 38, 209, 39, 41, 44, 38, 39, 41, 44, 38, 288, 288, 288, 288, 332, 333, 331, 288, 288, 332, 333, 331, 209, 332, 333, 331, 288, 236, 288, 288, 39, 41, 44, 38, 288, 288, 288, 39, 41, 44, 38, 288, 288, 288, 288, 288, 288, 332, 333, 331, 288, 301, 288, 288, 332, 333, 331, 288, 39, 41, 44, 38, 288, 288, 288, 288, 288, 288, 182, 170, 288, 288, 288, 288, 288, 332, 333, 331, 280, 288, 288, 36, 319, 288, 288, 288, 288, 288, 288, 288, 288, 288, 288, 288, 288, 288, 189,);

    static public $yy_lookahead = array(14, 15, 16, 46, 3, 19, 20, 15, 14, 52, 16, 25, 14, 15, 16, 29, 30, 31, 77, 33, 26, 35, 28, 22, 38, 14, 32, 16, 17, 43, 44, 45, 46, 47, 93, 49, 35, 51, 52, 14, 15, 16, 41, 57, 19, 20, 71, 70, 71, 48, 25, 21, 70, 71, 29, 80, 31, 80, 33, 16, 35, 50, 80, 38, 34, 83, 84, 99, 43, 44, 45, 46, 47, 98, 49, 98, 51, 52, 53, 7, 98, 71, 57, 14, 15, 16, 14, 71, 19, 20, 80, 19, 20, 14, 25, 16, 80, 25, 29, 1, 31, 29, 33, 31, 35, 26, 22, 38, 14, 93, 16, 32, 43, 44, 45, 46, 47, 14, 49, 35, 51, 52, 14, 15, 16, 41, 57, 19, 20, 57, 58, 59, 48, 25, 46, 16, 70, 29, 35, 31, 52, 33, 58, 35, 50, 14, 38, 16, 17, 83, 84, 43, 44, 45, 46, 47, 14, 49, 16, 51, 52, 14, 15, 16, 98, 57, 19, 20, 26, 50, 28, 14, 25, 16, 32, 64, 29, 34, 31, 36, 33, 14, 35, 16, 17, 38, 11, 12, 13, 22, 43, 44, 45, 46, 47, 1, 49, 3, 51, 52, 14, 15, 16, 1, 57, 19, 20, 50, 70, 71, 73, 25, 16, 102, 103, 29, 2, 31, 80, 33, 26, 35, 85, 86, 38, 18, 32, 90, 26, 43, 44, 45, 46, 47, 32, 49, 98, 51, 52, 14, 15, 16, 2, 57, 19, 20, 14, 15, 16, 26, 25, 15, 16, 46, 29, 32, 31, 17, 33, 52, 35, 17, 18, 38, 92, 33, 94, 95, 43, 44, 45, 46, 47, 1, 49, 3, 51, 52, 14, 15, 16, 71, 57, 19, 20, 49, 34, 51, 36, 25, 80, 76, 48, 29, 26, 31, 70, 33, 26, 35, 32, 22, 38, 93, 32, 28, 17, 43, 44, 45, 46, 47, 35, 49, 35, 51, 52, 14, 15, 16, 41, 57, 19, 20, 98, 48, 98, 48, 25, 94, 95, 34, 29, 36, 31, 46, 33, 79, 35, 17, 18, 38, 73, 74, 75, 64, 43, 44, 45, 46, 47, 1, 49, 3, 51, 52, 14, 15, 16, 71, 57, 19, 20, 63, 64, 65, 66, 25, 80, 65, 48, 29, 68, 31, 64, 33, 26, 35, 79, 69, 38, 93, 32, 73, 103, 43, 44, 45, 46, 47, 22, 49, 82, 51, 34, 85, 86, 87, 88, 57, 90, 3, 77, 35, 4, 5, 6, 7, 8, 9, 10, 11, 71, 53, 14, 17, 48, 64, 93, 19, 20, 80, 69, 16, 26, 25, 73, 64, 71, 29, 32, 31, 69, 35, 93, 72, 73, 80, 85, 86, 87, 88, 73, 90, 81, 82, 48, 71, 85, 86, 87, 88, 64, 90, 85, 98, 80, 69, 35, 90, 72, 73, 91, 100, 101, 79, 70, 70, 71, 81, 82, 48, 35, 85, 86, 87, 88, 80, 90, 64, 83, 84, 70, 46, 69, 48, 35, 72, 73, 101, 53, 35, 15, 16, 98, 98, 81, 82, 71, 48, 85, 86, 87, 88, 48, 90, 14, 80, 16, 64, 98, 99, 97, 23, 69, 94, 95, 72, 73, 61, 62, 63, 64, 65, 66, 98, 81, 82, 51, 64, 85, 86, 87, 88, 69, 90, 15, 72, 73, 1, 2, 14, 97, 16, 77, 15, 81, 82, 77, 64, 85, 86, 87, 88, 69, 90, 35, 72, 73, 74, 93, 14, 97, 16, 93, 77, 81, 82, 15, 16, 85, 86, 87, 88, 64, 90, 1, 7, 3, 69, 91, 93, 72, 73, 74, 37, 38, 39, 40, 70, 71, 81, 82, 1, 77, 85, 86, 87, 88, 80, 90, 26, 54, 55, 56, 1, 1, 32, 3, 91, 93, 93, 37, 38, 39, 40, 64, 98, 1, 16, 28, 69, 18, 2, 72, 73, 71, 1, 2, 54, 55, 56, 26, 81, 82, 80, 78, 85, 86, 87, 88, 24, 90, 26, 1, 1, 3, 3, 14, 32, 16, 93, 89, 98, 37, 38, 39, 40, 91, 96, 93, 35, 18, 37, 38, 39, 40, 15, 77, 26, 26, 54, 55, 56, 48, 32, 32, 77, 64, 35, 54, 55, 56, 69, 93, 46, 72, 73, 1, 64, 18, 17, 48, 93, 69, 81, 82, 72, 73, 85, 86, 87, 88, 1, 90, 3, 81, 82, 21, 71, 85, 86, 87, 88, 64, 90, 35, 77, 80, 69, 48, 3, 72, 73, 37, 38, 39, 40, 26, 27, 77, 81, 82, 93, 32, 85, 86, 87, 88, 64, 90, 54, 55, 56, 69, 89, 93, 72, 73, 73, 74, 16, 96, 73, 74, 16, 81, 82, 70, 64, 85, 86, 87, 88, 69, 90, 16, 72, 73, 64, 15, 14, 35, 16, 69, 53, 81, 82, 73, 64, 85, 86, 87, 88, 69, 90, 98, 72, 73, 36, 85, 86, 87, 88, 51, 90, 81, 82, 16, 16, 85, 86, 87, 88, 64, 90, 16, 33, 2, 69, 15, 33, 72, 73, 51, 16, 3, 42, 3, 91, 91, 81, 82, 92, 64, 85, 86, 87, 88, 69, 90, 80, 72, 73, 64, 18, 91, 93, 96, 69, 67, 81, 82, 73, 64, 85, 86, 87, 88, 69, 90, 95, 72, 73, 12, 85, 86, 87, 88, 91, 90, 81, 82, 76, 104, 85, 86, 87, 88, 64, 90, 104, 104, 104, 69, 104, 104, 72, 73, 104, 104, 104, 104, 104, 104, 104, 81, 82, 104, 64, 85, 86, 87, 88, 69, 90, 104, 72, 73, 64, 104, 104, 104, 104, 69, 104, 81, 82, 73, 64, 85, 86, 87, 88, 69, 90, 104, 72, 73, 104, 85, 86, 87, 88, 104, 90, 81, 82, 104, 104, 85, 86, 87, 88, 64, 90, 104, 104, 104, 69, 104, 104, 72, 73, 104, 104, 104, 104, 104, 104, 104, 81, 82, 104, 64, 85, 86, 87, 88, 69, 90, 104, 72, 73, 64, 104, 104, 104, 104, 69, 104, 81, 82, 73, 64, 85, 86, 87, 88, 69, 90, 104, 72, 73, 104, 85, 86, 87, 88, 104, 90, 81, 82, 104, 104, 85, 86, 87, 88, 64, 90, 104, 104, 104, 69, 104, 104, 72, 73, 104, 104, 104, 104, 104, 104, 104, 81, 82, 104, 64, 85, 86, 87, 88, 69, 90, 104, 72, 73, 64, 104, 104, 104, 104, 69, 104, 81, 82, 73, 64, 85, 86, 87, 88, 69, 90, 104, 72, 73, 104, 85, 86, 87, 88, 104, 90, 81, 82, 104, 104, 85, 86, 87, 88, 64, 90, 70, 71, 104, 69, 104, 104, 72, 73, 104, 104, 80, 104, 104, 83, 84, 81, 82, 104, 64, 85, 86, 87, 88, 69, 90, 104, 72, 73, 98, 104, 104, 104, 104, 104, 104, 81, 82, 104, 64, 85, 86, 87, 88, 69, 90, 104, 72, 73, 104, 104, 104, 104, 104, 104, 104, 81, 82, 104, 104, 85, 86, 87, 88, 64, 90, 70, 71, 104, 69, 104, 104, 72, 73, 104, 104, 80, 104, 104, 83, 84, 81, 82, 104, 64, 85, 86, 87, 88, 69, 90, 104, 72, 73, 98, 104, 104, 104, 104, 104, 104, 81, 82, 104, 64, 85, 86, 87, 88, 69, 90, 104, 72, 73, 104, 104, 104, 104, 104, 104, 104, 81, 82, 104, 104, 85, 86, 87, 88, 64, 90, 70, 71, 104, 69, 104, 104, 72, 73, 104, 104, 80, 104, 104, 83, 84, 81, 82, 104, 64, 85, 86, 87, 88, 69, 90, 104, 72, 73, 98, 104, 104, 104, 104, 104, 104, 81, 82, 104, 64, 85, 86, 87, 88, 69, 90, 104, 72, 73, 104, 104, 104, 104, 104, 104, 104, 81, 82, 1, 104, 85, 86, 87, 88, 64, 90, 104, 104, 104, 69, 104, 104, 72, 73, 104, 104, 104, 104, 104, 104, 104, 81, 82, 26, 104, 85, 86, 87, 88, 32, 90, 104, 1, 104, 37, 38, 39, 40, 104, 104, 104, 104, 7, 104, 104, 104, 104, 104, 104, 14, 104, 54, 55, 56, 19, 20, 104, 104, 104, 104, 25, 104, 104, 104, 29, 104, 31, 36, 37, 38, 39, 40, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 54, 55, 56, 104, 1, 64, 3, 57, 58, 59, 69, 1, 104, 104, 73, 104, 1, 104, 3, 104, 104, 104, 104, 82, 104, 104, 85, 86, 87, 88, 104, 90, 104, 104, 104, 104, 104, 104, 104, 104, 37, 38, 39, 40, 1, 104, 104, 37, 38, 39, 40, 104, 37, 38, 39, 40, 104, 54, 55, 56, 104, 104, 104, 53, 54, 55, 56, 1, 2, 54, 55, 56, 1, 104, 3, 104, 1, 104, 3, 36, 37, 38, 39, 40, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 54, 55, 56, 1, 104, 104, 37, 38, 39, 40, 1, 37, 38, 39, 40, 37, 38, 39, 40, 104, 104, 104, 104, 54, 55, 56, 104, 104, 54, 55, 56, 1, 54, 55, 56, 104, 27, 104, 104, 37, 38, 39, 40, 104, 104, 104, 37, 38, 39, 40, 104, 104, 104, 104, 104, 104, 54, 55, 56, 104, 58, 104, 104, 54, 55, 56, 104, 37, 38, 39, 40, 104, 104, 104, 104, 104, 104, 70, 71, 104, 104, 104, 104, 104, 54, 55, 56, 80, 104, 104, 83, 84, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 98,);

    const YY_SHIFT_USE_DFLT = - 44;

    const YY_SHIFT_MAX = 247;

    static public $yy_shift_ofst = array(400, 303, 303, 108, 108, 264, 108, 264, 25, - 14, - 14, 108, 108, 108, 108, 108, 108, 108, 108, 147, 108, 108, 108, 108, 147, 108, 225, 108, 108, 108, 108, 108, 186, 108, 108, 108, 108, 69, 69, 342, 342, 342, 342, 342, 342, 575, 617, 1252, 1252, 1252, 1252, 1252, 400, 1446, 1286, 1355, 1383, 1411, 1343, 1415, 1439, 1406, 687, 1350, 1467, 1467, 1467, 1467, 1467, 1467, 1467, 1467, 1467, 1467, 1467, 1467, 548, 644, 548, 236, 194, 202, 72, 142, - 6, 79, 272, 79, 272, 202, 202, 207, 477, 538, 521, 1290, 702, 11, 350, 167, 643, 322, 244, 157, 175, 131, 492, 492, 492, 492, 553, 492, 547, 492, 527, 605, 223, 592, 492, 634, 604, 757, 521, 492, 527, 553, 223, 521, 492, 672, 268, 98, 98, 98, 98, 98, 817, 98, 98, 98, 817, 98, - 44, 94, - 2, 223, - 43, - 43, 88, - 43, 223, - 43, - 43, 88, 223, - 43, 223, 223, 223, 223, 223, 103, 223, 223, - 43, 223, 223, 223, 223, 223, 223, 223, 223, 223, 223, 214, 817, 98, 817, 842, 98, 817, 98, 606, 681, 98, 606, 214, 98, - 44, - 44, - 44, - 44, - 44, 626, 398, 84, 1, 437, 232, 279, 368, 277, 143, 423, 360, 30, 297, 289, 119, 240, 451, 456, 252, 795, 813, 778, 763, 743, 735, 681, 603, 718, 739, 737, 755, 750, 790, 674, 722, 639, 753, 799, 815, 783, 782, 652, 806, 774, 775, 196, 43, 530, 490, 407, 621, 570, - 8,);

    const YY_REDUCE_USE_DFLT = - 60;

    const YY_REDUCE_MAX = 193;

    static public $yy_reduce_ofst = array(458, 363, 388, 445, 415, 510, 465, 485, 1040, 910, 805, 890, 935, 845, 825, 740, 760, 780, 975, 955, 1170, 1195, 1150, 1130, 1105, 1085, 1000, 1020, 715, 1065, 870, 552, 695, 625, 675, 650, 614, 1281, 310, 965, 900, 835, 353, 705, 770, 397, 1126, 996, - 18, 397, 1061, 1444, 300, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 519, 66, 137, - 23, 138, 111, 210, 288, 341, 427, 16, 555, - 25, 357, 172, 369, 412, 269, 281, 226, 567, 226, 558, 226, 421, 421, 567, 304, 567, 488, 467, 517, 591, 563, 518, 325, - 59, 467, 226, 10, 226, 471, 467, 688, 467, 680, 640, 653, 656, 376, 676, 600, 421, 638, 226, 226, 226, 226, 226, 421, 396, 226, 226, 235, 226, 226, 744, 742, 751, 731, 731, 745, 731, 751, 731, 731, 729, 751, 731, 751, 751, 751, 751, 751, 728, 751, 751, 731, 751, 751, 751, 751, 751, 751, 751, 751, 751, 751, - 32, 756, 228, 756, 773, 228, 756, 228, 787, 768, 228, 215, - 32, 228, 258, 489, 386, 299, 371,);

    static public $yyExpectedTokens = array(array(4, 5, 6, 7, 8, 9, 10, 11, 14, 19, 20, 25, 29, 31,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 53, 57,), array(14, 15, 16, 19, 20, 25, 29, 30, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 30, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 52, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 57,), array(14, 15, 16, 19, 20, 25, 29, 31, 33, 35, 38, 43, 44, 45, 46, 47, 49, 51, 57,), array(1, 3, 26, 32, 37, 38, 39, 40, 54, 55, 56,), array(1, 24, 26, 32, 37, 38, 39, 40, 54, 55, 56,), array(1, 26, 32, 37, 38, 39, 40, 54, 55, 56,), array(1, 26, 32, 37, 38, 39, 40, 54, 55, 56,), array(1, 26, 32, 37, 38, 39, 40, 54, 55, 56,), array(1, 26, 32, 37, 38, 39, 40, 54, 55, 56,), array(1, 26, 32, 37, 38, 39, 40, 54, 55, 56,), array(4, 5, 6, 7, 8, 9, 10, 11, 14, 19, 20, 25, 29, 31,), array(1, 27, 37, 38, 39, 40, 54, 55, 56,), array(1, 36, 37, 38, 39, 40, 54, 55, 56,), array(1, 3, 37, 38, 39, 40, 54, 55, 56,), array(1, 36, 37, 38, 39, 40, 54, 55, 56,), array(1, 3, 37, 38, 39, 40, 54, 55, 56,), array(1, 3, 37, 38, 39, 40, 54, 55, 56,), array(1, 3, 37, 38, 39, 40, 54, 55, 56,), array(1, 37, 38, 39, 40, 54, 55, 56, 58,), array(1, 2, 37, 38, 39, 40, 54, 55, 56,), array(1, 21, 37, 38, 39, 40, 54, 55, 56,), array(1, 37, 38, 39, 40, 53, 54, 55, 56,), array(1, 37, 38, 39, 40, 54, 55, 56,), array(1, 37, 38, 39, 40, 54, 55, 56,), array(1, 37, 38, 39, 40, 54, 55, 56,), array(1, 37, 38, 39, 40, 54, 55, 56,), array(1, 37, 38, 39, 40, 54, 55, 56,), array(1, 37, 38, 39, 40, 54, 55, 56,), array(1, 37, 38, 39, 40, 54, 55, 56,), array(1, 37, 38, 39, 40, 54, 55, 56,), array(1, 37, 38, 39, 40, 54, 55, 56,), array(1, 37, 38, 39, 40, 54, 55, 56,), array(1, 37, 38, 39, 40, 54, 55, 56,), array(1, 37, 38, 39, 40, 54, 55, 56,), array(37, 38, 39, 40, 54, 55, 56,), array(1, 3, 18, 26, 32, 35, 48,), array(37, 38, 39, 40, 54, 55, 56,), array(15, 16, 49, 51,), array(1, 3, 26, 32,), array(1, 26, 32,), array(7, 14, 19, 20, 25, 29, 31, 57, 58, 59,), array(14, 16, 26, 28, 32,), array(14, 16, 26, 28, 32,), array(14, 16, 26, 32,), array(1, 3, 26, 32,), array(14, 16, 26, 32,), array(1, 3, 26, 32,), array(1, 26, 32,), array(1, 26, 32,), array(18, 46, 52,), array(15, 16, 51,), array(1, 2,), array(15, 35,), array(7, 14, 19, 20, 25, 29, 31, 57, 58, 59,), array(1, 3, 26, 27, 32,), array(14, 16, 17, 50,), array(1, 3, 26, 32,), array(14, 16, 17, 22,), array(1, 3, 26, 32,), array(17, 18, 48,), array(17, 18, 48,), array(14, 16, 50,), array(11, 12, 13,), array(14, 16, 17,), array(14, 16,), array(14, 16,), array(14, 16,), array(14, 16,), array(15, 16,), array(14, 16,), array(14, 16,), array(14, 16,), array(14, 16,), array(1, 3,), array(26, 32,), array(1, 28,), array(14, 16,), array(14, 16,), array(1, 18,), array(14, 16,), array(15, 35,), array(14, 16,), array(14, 16,), array(15, 16,), array(26, 32,), array(15, 35,), array(14, 16,), array(18, 48,), array(26, 32,), array(1,), array(1,), array(1,), array(1,), array(1,), array(18,), array(1,), array(1,), array(1,), array(18,), array(1,), array(), array(14, 16, 50,), array(14, 15, 16,), array(26, 32,), array(46, 52,), array(46, 52,), array(46, 52,), array(46, 52,), array(26, 32,), array(46, 52,), array(46, 52,), array(46, 52,), array(26, 32,), array(46, 52,), array(26, 32,), array(26, 32,), array(26, 32,), array(26, 32,), array(26, 32,), array(14, 35,), array(26, 32,), array(26, 32,), array(46, 52,), array(26, 32,), array(26, 32,), array(26, 32,), array(26, 32,), array(26, 32,), array(26, 32,), array(26, 32,), array(26, 32,), array(26, 32,), array(26, 32,), array(2,), array(18,), array(1,), array(18,), array(12,), array(1,), array(18,), array(1,), array(26,), array(35,), array(1,), array(26,), array(2,), array(1,), array(), array(), array(), array(), array(), array(1, 2, 35, 37, 38, 39, 40, 48, 54, 55, 56,), array(3, 17, 26, 32, 35, 48,), array(22, 35, 41, 48, 58,), array(3, 22, 35, 41, 48,), array(35, 46, 48, 53,), array(14, 15, 16, 33,), array(22, 35, 41, 48,), array(22, 35, 48,), array(28, 35, 48,), array(34, 36,), array(35, 48,), array(34, 53,), array(21, 34,), array(34, 36,), array(17, 46,), array(16, 50,), array(2, 17,), array(35, 48,), array(35, 48,), array(34, 36,), array(15,), array(3,), array(33,), array(51,), array(51,), array(16,), array(35,), array(16,), array(3,), array(16,), array(35,), array(15,), array(16,), array(16,), array(17,), array(53,), array(46,), array(36,), array(16,), array(3,), array(16,), array(16,), array(15,), array(2,), array(33,), array(42,), array(16,), array(16,), array(15,), array(23,), array(16,), array(2,), array(7,), array(15,), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(), array(),);

    static public $yy_default = array(338, 500, 517, 480, 480, 517, 480, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 393, 393, 370, 393, 362, 393, 335, 398, 517, 517, 517, 517, 517, 517, 517, 517, 517, 517, 395, 404, 400, 479, 414, 375, 501, 502, 398, 503, 478, 403, 419, 431, 420, 517, 406, 393, 517, 393, 393, 393, 413, 393, 438, 393, 393, 492, 517, 384, 517, 517, 406, 453, 406, 517, 406, 444, 444, 453, 517, 453, 517, 517, 517, 517, 517, 453, 517, 517, 517, 406, 372, 406, 517, 517, 387, 517, 517, 517, 517, 517, 393, 517, 517, 444, 393, 409, 424, 406, 417, 423, 444, 389, 422, 410, 489, 411, 487, 454, 517, 378, 451, 448, 449, 450, 379, 473, 470, 447, 359, 472, 360, 382, 373, 374, 380, 453, 377, 358, 471, 357, 369, 381, 368, 365, 364, 371, 367, 363, 383, 481, 490, 385, 493, 353, 390, 467, 413, 516, 453, 438, 516, 482, 388, 486, 453, 486, 486, 453, 431, 431, 421, 421, 431, 517, 421, 421, 431, 517, 431, 517, 517, 517, 427, 517, 465, 439, 517, 517, 517, 517, 517, 517, 517, 517, 465, 517, 517, 517, 491, 517, 517, 517, 517, 517, 427, 517, 517, 517, 517, 517, 517, 465, 429, 433, 517, 517, 517, 401, 517, 465, 344, 517, 405, 459, 402, 466, 475, 476, 336, 460, 433, 394, 488, 458, 515, 376, 462, 412, 484, 425, 426, 483, 485, 464, 465, 428, 430, 452, 455, 463, 445, 408, 432, 407, 392, 391, 342, 343, 346, 341, 340, 337, 339, 345, 347, 352, 354, 355, 351, 350, 348, 349, 456, 457, 506, 509, 508, 505, 504, 386, 511, 510, 513, 498, 477, 474, 499, 497, 514, 507, 468, 512, 440, 415, 416, 437, 436, 434, 435, 446, 469, 441, 442, 443, 418, 496, 494, 495, 461,);

    const YYNOCODE = 105;

    const YYSTACKDEPTH = 500;

    const YYNSTATE = 335;

    const YYNRULE = 182;

    const YYERRORSYMBOL = 60;

    const YYERRSYMDT = 'yy0';

    const YYFALLBACK = 0;

    public static $yyFallback = array();

    public function Trace($TraceFILE, $zTracePrompt)
    {
        if (!$TraceFILE) {
            $zTracePrompt = 0;
        } elseif (!$zTracePrompt) {
            $TraceFILE = 0;
        }
        $this->yyTraceFILE = $TraceFILE;
        $this->yyTracePrompt = $zTracePrompt;
    }

    public function PrintTrace()
    {
        $this->yyTraceFILE = fopen('php://output', 'w');
        $this->yyTracePrompt = '<br>';
    }

    public $yyTraceFILE;

    public $yyTracePrompt;

    public $yyidx;                    /* Index of top element in stack */
    public $yyerrcnt;                 /* Shifts left before out of the error */
    public $yystack = array();  /* The parser's stack */

    public $yyTokenName = array('$', 'VERT', 'COLON', 'RDEL', 'COMMENT', 'PHP', 'XMLTAG', 'TEXT', 'STRIPON', 'STRIPOFF', 'BLOCKSOURCE', 'LITERALSTART', 'LITERALEND', 'LITERAL', 'LDEL', 'DOLLAR', 'ID', 'EQUAL', 'PTR', 'LDELIF', 'LDELFOR', 'SEMICOLON', 'INCDEC', 'TO', 'STEP', 'LDELFOREACH', 'SPACE', 'AS', 'APTR', 'LDELSETFILTER', 'SMARTYBLOCKCHILDPARENT', 'LDELSLASH', 'ATTR', 'INTEGER', 'COMMA', 'OPENP', 'CLOSEP', 'MATH', 'UNIMATH', 'ANDSYM', 'ISIN', 'INSTANCEOF', 'QMARK', 'NOT', 'TYPECAST', 'HEX', 'DOT', 'SINGLEQUOTESTRING', 'DOUBLECOLON', 'NAMESPACE', 'AT', 'HATCH', 'OPENB', 'CLOSEB', 'LOGOP', 'TLOGOP', 'SINGLECOND', 'QUOTE', 'BACKTICK', 'DOLLARID', 'error', 'start', 'template', 'template_element', 'smartytag', 'literal', 'text_content', 'literal_elements', 'literal_element', 'value', 'modifierlist', 'attributes', 'expr', 'varindexed', 'statement', 'statements', 'optspace', 'varvar', 'foraction', 'modparameters', 'attribute', 'ternary', 'array', 'lop', 'scond', 'variable', 'ns1', 'function', 'doublequoted_with_quotes', 'static_class_access', 'object', 'arrayindex', 'indexdef', 'varvarele', 'objectchain', 'objectelement', 'method', 'params', 'modifier', 'modparameter', 'arrayelements', 'arrayelement', 'doublequoted', 'doublequotedcontent',);

    public static $yyRuleName = array('start ::= template', 'template ::= template_element', 'template ::= template template_element', 'template ::=', 'template_element ::= smartytag RDEL', 'template_element ::= COMMENT', 'template_element ::= literal', 'template_element ::= PHP', 'template_element ::= XMLTAG', 'template_element ::= text_content', 'text_content ::= TEXT', 'text_content ::= text_content TEXT', 'template_element ::= STRIPON', 'template_element ::= STRIPOFF', 'template_element ::= BLOCKSOURCE', 'literal ::= LITERALSTART LITERALEND', 'literal ::= LITERALSTART literal_elements LITERALEND', 'literal_elements ::= literal_elements literal_element', 'literal_elements ::=', 'literal_element ::= literal', 'literal_element ::= LITERAL', 'smartytag ::= LDEL value', 'smartytag ::= LDEL value modifierlist attributes', 'smartytag ::= LDEL value attributes', 'smartytag ::= LDEL expr modifierlist attributes', 'smartytag ::= LDEL expr attributes', 'smartytag ::= LDEL DOLLAR ID EQUAL value', 'smartytag ::= LDEL DOLLAR ID EQUAL expr', 'smartytag ::= LDEL DOLLAR ID EQUAL expr attributes', 'smartytag ::= LDEL varindexed EQUAL expr attributes', 'smartytag ::= LDEL ID attributes', 'smartytag ::= LDEL ID', 'smartytag ::= LDEL ID modifierlist attributes', 'smartytag ::= LDEL ID PTR ID attributes', 'smartytag ::= LDEL ID PTR ID modifierlist attributes', 'smartytag ::= LDELIF expr', 'smartytag ::= LDELIF expr attributes', 'smartytag ::= LDELIF statement', 'smartytag ::= LDELIF statement attributes', 'smartytag ::= LDELFOR statements SEMICOLON optspace expr SEMICOLON optspace DOLLAR varvar foraction attributes', 'foraction ::= EQUAL expr', 'foraction ::= INCDEC', 'smartytag ::= LDELFOR statement TO expr attributes', 'smartytag ::= LDELFOR statement TO expr STEP expr attributes', 'smartytag ::= LDELFOREACH attributes', 'smartytag ::= LDELFOREACH SPACE value AS DOLLAR varvar attributes', 'smartytag ::= LDELFOREACH SPACE value AS DOLLAR varvar APTR DOLLAR varvar attributes', 'smartytag ::= LDELFOREACH SPACE expr AS DOLLAR varvar attributes', 'smartytag ::= LDELFOREACH SPACE expr AS DOLLAR varvar APTR DOLLAR varvar attributes', 'smartytag ::= LDELSETFILTER ID modparameters', 'smartytag ::= LDELSETFILTER ID modparameters modifierlist', 'smartytag ::= LDEL SMARTYBLOCKCHILDPARENT', 'smartytag ::= LDELSLASH ID', 'smartytag ::= LDELSLASH ID modifierlist', 'smartytag ::= LDELSLASH ID PTR ID', 'smartytag ::= LDELSLASH ID PTR ID modifierlist', 'attributes ::= attributes attribute', 'attributes ::= attribute', 'attributes ::=', 'attribute ::= SPACE ID EQUAL ID', 'attribute ::= ATTR expr', 'attribute ::= ATTR value', 'attribute ::= SPACE ID', 'attribute ::= SPACE expr', 'attribute ::= SPACE value', 'attribute ::= SPACE INTEGER EQUAL expr', 'statements ::= statement', 'statements ::= statements COMMA statement', 'statement ::= DOLLAR varvar EQUAL expr', 'statement ::= varindexed EQUAL expr', 'statement ::= OPENP statement CLOSEP', 'expr ::= value', 'expr ::= ternary', 'expr ::= DOLLAR ID COLON ID', 'expr ::= expr MATH value', 'expr ::= expr UNIMATH value', 'expr ::= expr ANDSYM value', 'expr ::= array', 'expr ::= expr modifierlist', 'expr ::= expr lop expr', 'expr ::= expr scond', 'expr ::= expr ISIN array', 'expr ::= expr ISIN value', 'expr ::= variable INSTANCEOF ns1', 'ternary ::= OPENP expr CLOSEP QMARK DOLLAR ID COLON expr', 'ternary ::= OPENP expr CLOSEP QMARK expr COLON expr', 'value ::= variable', 'value ::= UNIMATH value', 'value ::= NOT value', 'value ::= TYPECAST value', 'value ::= variable INCDEC', 'value ::= HEX', 'value ::= INTEGER', 'value ::= INTEGER DOT INTEGER', 'value ::= INTEGER DOT', 'value ::= DOT INTEGER', 'value ::= ID', 'value ::= function', 'value ::= OPENP expr CLOSEP', 'value ::= SINGLEQUOTESTRING', 'value ::= doublequoted_with_quotes', 'value ::= varindexed DOUBLECOLON static_class_access', 'value ::= smartytag RDEL', 'value ::= value modifierlist', 'value ::= NAMESPACE', 'value ::= ns1 DOUBLECOLON static_class_access', 'ns1 ::= ID', 'ns1 ::= NAMESPACE', 'ns1 ::= variable', 'variable ::= varindexed', 'variable ::= DOLLAR varvar AT ID', 'variable ::= object', 'variable ::= HATCH ID HATCH', 'variable ::= HATCH ID HATCH arrayindex', 'variable ::= HATCH variable HATCH', 'variable ::= HATCH variable HATCH arrayindex', 'varindexed ::= DOLLAR varvar arrayindex', 'arrayindex ::= arrayindex indexdef', 'arrayindex ::=', 'indexdef ::= DOT DOLLAR varvar', 'indexdef ::= DOT DOLLAR varvar AT ID', 'indexdef ::= DOT ID', 'indexdef ::= DOT INTEGER', 'indexdef ::= DOT LDEL expr RDEL', 'indexdef ::= OPENB ID CLOSEB', 'indexdef ::= OPENB ID DOT ID CLOSEB', 'indexdef ::= OPENB expr CLOSEB', 'indexdef ::= OPENB CLOSEB', 'varvar ::= varvarele', 'varvar ::= varvar varvarele', 'varvarele ::= ID', 'varvarele ::= LDEL expr RDEL', 'object ::= varindexed objectchain', 'objectchain ::= objectelement', 'objectchain ::= objectchain objectelement', 'objectelement ::= PTR ID arrayindex', 'objectelement ::= PTR DOLLAR varvar arrayindex', 'objectelement ::= PTR LDEL expr RDEL arrayindex', 'objectelement ::= PTR ID LDEL expr RDEL arrayindex', 'objectelement ::= PTR method', 'function ::= ns1 OPENP params CLOSEP', 'method ::= ID OPENP params CLOSEP', 'method ::= DOLLAR ID OPENP params CLOSEP', 'params ::= params COMMA expr', 'params ::= expr', 'params ::=', 'modifierlist ::= modifierlist modifier modparameters', 'modifierlist ::= modifier modparameters', 'modifier ::= VERT AT ID', 'modifier ::= VERT ID', 'modparameters ::= modparameters modparameter', 'modparameters ::=', 'modparameter ::= COLON value', 'modparameter ::= COLON array', 'static_class_access ::= method', 'static_class_access ::= method objectchain', 'static_class_access ::= ID', 'static_class_access ::= DOLLAR ID arrayindex', 'static_class_access ::= DOLLAR ID arrayindex objectchain', 'lop ::= LOGOP', 'lop ::= TLOGOP', 'scond ::= SINGLECOND', 'array ::= OPENB arrayelements CLOSEB', 'arrayelements ::= arrayelement', 'arrayelements ::= arrayelements COMMA arrayelement', 'arrayelements ::=', 'arrayelement ::= value APTR expr', 'arrayelement ::= ID APTR expr', 'arrayelement ::= expr', 'doublequoted_with_quotes ::= QUOTE QUOTE', 'doublequoted_with_quotes ::= QUOTE doublequoted QUOTE', 'doublequoted ::= doublequoted doublequotedcontent', 'doublequoted ::= doublequotedcontent', 'doublequotedcontent ::= BACKTICK variable BACKTICK', 'doublequotedcontent ::= BACKTICK expr BACKTICK', 'doublequotedcontent ::= DOLLARID', 'doublequotedcontent ::= LDEL variable RDEL', 'doublequotedcontent ::= LDEL expr RDEL', 'doublequotedcontent ::= smartytag RDEL', 'doublequotedcontent ::= TEXT', 'optspace ::= SPACE', 'optspace ::=',);

    public function tokenName($tokenType)
    {
        if ($tokenType === 0) {
            return 'End of Input';
        }
        if ($tokenType > 0 && $tokenType < count($this->yyTokenName)) {
            return $this->yyTokenName[$tokenType];
        } else {
            return "Unknown";
        }
    }

    public static function yy_destructor($yymajor, $yypminor)
    {
        switch ($yymajor) {
            default:
                break;   /* If no destructor action specified: do nothing */
        }
    }

    public function yy_pop_parser_stack()
    {
        if (empty($this->yystack)) {
            return;
        }
        $yytos = array_pop($this->yystack);
        if ($this->yyTraceFILE && $this->yyidx >= 0) {
            fwrite($this->yyTraceFILE, $this->yyTracePrompt . 'Popping ' . $this->yyTokenName[$yytos->major] . "\n");
        }
        $yymajor = $yytos->major;
        self::yy_destructor($yymajor, $yytos->minor);
        $this->yyidx --;

        return $yymajor;
    }

    public function __destruct()
    {
        while ($this->yystack !== Array()) {
            $this->yy_pop_parser_stack();
        }
        if (is_resource($this->yyTraceFILE)) {
            fclose($this->yyTraceFILE);
        }
    }

    public function yy_get_expected_tokens($token)
    {
        static $res3 = array();
        static $res4 = array();
        $state = $this->yystack[$this->yyidx]->stateno;
        $expected = self::$yyExpectedTokens[$state];
        if (isset($res3[$state][$token])) {
            if ($res3[$state][$token]) {
                return $expected;
            }
        } else {
            if ($res3[$state][$token] = in_array($token, self::$yyExpectedTokens[$state], true)) {
                return $expected;
            }
        }
        $stack = $this->yystack;
        $yyidx = $this->yyidx;
        do {
            $yyact = $this->yy_find_shift_action($token);
            if ($yyact >= self::YYNSTATE && $yyact < self::YYNSTATE + self::YYNRULE) {
                // reduce action
                $done = 0;
                do {
                    if ($done ++ == 100) {
                        $this->yyidx = $yyidx;
                        $this->yystack = $stack;
                        // too much recursion prevents proper detection
                        // so give up
                        return array_unique($expected);
                    }
                    $yyruleno = $yyact - self::YYNSTATE;
                    $this->yyidx -= self::$yyRuleInfo[$yyruleno][1];
                    $nextstate = $this->yy_find_reduce_action($this->yystack[$this->yyidx]->stateno, self::$yyRuleInfo[$yyruleno][0]);
                    if (isset(self::$yyExpectedTokens[$nextstate])) {
                        $expected = array_merge($expected, self::$yyExpectedTokens[$nextstate]);
                        if (isset($res4[$nextstate][$token])) {
                            if ($res4[$nextstate][$token]) {
                                $this->yyidx = $yyidx;
                                $this->yystack = $stack;
                                return array_unique($expected);
                            }
                        } else {
                            if ($res4[$nextstate][$token] = in_array($token, self::$yyExpectedTokens[$nextstate], true)) {
                                $this->yyidx = $yyidx;
                                $this->yystack = $stack;
                                return array_unique($expected);
                            }
                        }
                    }
                    if ($nextstate < self::YYNSTATE) {
                        // we need to shift a non-terminal
                        $this->yyidx ++;
                        $x = new TP_yyStackEntry;
                        $x->stateno = $nextstate;
                        $x->major = self::$yyRuleInfo[$yyruleno][0];
                        $this->yystack[$this->yyidx] = $x;
                        continue 2;
                    } elseif ($nextstate == self::YYNSTATE + self::YYNRULE + 1) {
                        $this->yyidx = $yyidx;
                        $this->yystack = $stack;
                        // the last token was just ignored, we can't accept
                        // by ignoring input, this is in essence ignoring a
                        // syntax error!
                        return array_unique($expected);
                    } elseif ($nextstate === self::YY_NO_ACTION) {
                        $this->yyidx = $yyidx;
                        $this->yystack = $stack;
                        // input accepted, but not shifted (I guess)
                        return $expected;
                    } else {
                        $yyact = $nextstate;
                    }
                } while (true);
            }
            break;
        } while (true);
        $this->yyidx = $yyidx;
        $this->yystack = $stack;

        return array_unique($expected);
    }

    public function yy_is_expected_token($token)
    {
        static $res = array();
        static $res2 = array();
        if ($token === 0) {
            return true; // 0 is not part of this
        }
        $state = $this->yystack[$this->yyidx]->stateno;
        if (isset($res[$state][$token])) {
            if ($res[$state][$token]) {
                return true;
            }
        } else {
            if ($res[$state][$token] = in_array($token, self::$yyExpectedTokens[$state], true)) {
                return true;
            }
        }
        $stack = $this->yystack;
        $yyidx = $this->yyidx;
        do {
            $yyact = $this->yy_find_shift_action($token);
            if ($yyact >= self::YYNSTATE && $yyact < self::YYNSTATE + self::YYNRULE) {
                // reduce action
                $done = 0;
                do {
                    if ($done ++ == 100) {
                        $this->yyidx = $yyidx;
                        $this->yystack = $stack;
                        // too much recursion prevents proper detection
                        // so give up
                        return true;
                    }
                    $yyruleno = $yyact - self::YYNSTATE;
                    $this->yyidx -= self::$yyRuleInfo[$yyruleno][1];
                    $nextstate = $this->yy_find_reduce_action($this->yystack[$this->yyidx]->stateno, self::$yyRuleInfo[$yyruleno][0]);
                    if (isset($res2[$nextstate][$token])) {
                        if ($res2[$nextstate][$token]) {
                            $this->yyidx = $yyidx;
                            $this->yystack = $stack;
                            return true;
                        }
                    } else {
                        if ($res2[$nextstate][$token] = (isset(self::$yyExpectedTokens[$nextstate]) && in_array($token, self::$yyExpectedTokens[$nextstate], true))) {
                            $this->yyidx = $yyidx;
                            $this->yystack = $stack;
                            return true;
                        }
                    }
                    if ($nextstate < self::YYNSTATE) {
                        // we need to shift a non-terminal
                        $this->yyidx ++;
                        $x = new TP_yyStackEntry;
                        $x->stateno = $nextstate;
                        $x->major = self::$yyRuleInfo[$yyruleno][0];
                        $this->yystack[$this->yyidx] = $x;
                        continue 2;
                    } elseif ($nextstate == self::YYNSTATE + self::YYNRULE + 1) {
                        $this->yyidx = $yyidx;
                        $this->yystack = $stack;
                        if (!$token) {
                            // end of input: this is valid
                            return true;
                        }
                        // the last token was just ignored, we can't accept
                        // by ignoring input, this is in essence ignoring a
                        // syntax error!
                        return false;
                    } elseif ($nextstate === self::YY_NO_ACTION) {
                        $this->yyidx = $yyidx;
                        $this->yystack = $stack;
                        // input accepted, but not shifted (I guess)
                        return true;
                    } else {
                        $yyact = $nextstate;
                    }
                } while (true);
            }
            break;
        } while (true);
        $this->yyidx = $yyidx;
        $this->yystack = $stack;

        return true;
    }

    public function yy_find_shift_action($iLookAhead)
    {
        $stateno = $this->yystack[$this->yyidx]->stateno;

        /* if ($this->yyidx < 0) return self::YY_NO_ACTION;  */
        if (!isset(self::$yy_shift_ofst[$stateno])) {
            // no shift actions
            return self::$yy_default[$stateno];
        }
        $i = self::$yy_shift_ofst[$stateno];
        if ($i === self::YY_SHIFT_USE_DFLT) {
            return self::$yy_default[$stateno];
        }
        if ($iLookAhead == self::YYNOCODE) {
            return self::YY_NO_ACTION;
        }
        $i += $iLookAhead;
        if ($i < 0 || $i >= self::YY_SZ_ACTTAB || self::$yy_lookahead[$i] != $iLookAhead) {
            if (count(self::$yyFallback) && $iLookAhead < count(self::$yyFallback) && ($iFallback = self::$yyFallback[$iLookAhead]) != 0) {
                if ($this->yyTraceFILE) {
                    fwrite($this->yyTraceFILE, $this->yyTracePrompt . "FALLBACK " . $this->yyTokenName[$iLookAhead] . " => " . $this->yyTokenName[$iFallback] . "\n");
                }

                return $this->yy_find_shift_action($iFallback);
            }

            return self::$yy_default[$stateno];
        } else {
            return self::$yy_action[$i];
        }
    }

    public function yy_find_reduce_action($stateno, $iLookAhead)
    {
        /* $stateno = $this->yystack[$this->yyidx]->stateno; */

        if (!isset(self::$yy_reduce_ofst[$stateno])) {
            return self::$yy_default[$stateno];
        }
        $i = self::$yy_reduce_ofst[$stateno];
        if ($i == self::YY_REDUCE_USE_DFLT) {
            return self::$yy_default[$stateno];
        }
        if ($iLookAhead == self::YYNOCODE) {
            return self::YY_NO_ACTION;
        }
        $i += $iLookAhead;
        if ($i < 0 || $i >= self::YY_SZ_ACTTAB || self::$yy_lookahead[$i] != $iLookAhead) {
            return self::$yy_default[$stateno];
        } else {
            return self::$yy_action[$i];
        }
    }

    public function yy_shift($yyNewState, $yyMajor, $yypMinor)
    {
        $this->yyidx ++;
        if ($this->yyidx >= self::YYSTACKDEPTH) {
            $this->yyidx --;
            if ($this->yyTraceFILE) {
                fprintf($this->yyTraceFILE, "%sStack Overflow!\n", $this->yyTracePrompt);
            }
            while ($this->yyidx >= 0) {
                $this->yy_pop_parser_stack();
            }
            #line 203 "../smarty/lexer/smarty_internal_templateparser.y"

            $this->internalError = true;
            $this->compiler->trigger_template_error("Stack overflow in template parser");

            return;
        }
        $yytos = new TP_yyStackEntry;
        $yytos->stateno = $yyNewState;
        $yytos->major = $yyMajor;
        $yytos->minor = $yypMinor;
        $this->yystack[] = $yytos;
        if ($this->yyTraceFILE && $this->yyidx > 0) {
            fprintf($this->yyTraceFILE, "%sShift %d\n", $this->yyTracePrompt, $yyNewState);
            fprintf($this->yyTraceFILE, "%sStack:", $this->yyTracePrompt);
            for ($i = 1; $i <= $this->yyidx; $i ++) {
                fprintf($this->yyTraceFILE, " %s", $this->yyTokenName[$this->yystack[$i]->major]);
            }
            fwrite($this->yyTraceFILE, "\n");
        }
    }

    public static $yyRuleInfo = array(array(0 => 61, 1 => 1), array(0 => 62, 1 => 1), array(0 => 62, 1 => 2), array(0 => 62, 1 => 0), array(0 => 63, 1 => 2), array(0 => 63, 1 => 1), array(0 => 63, 1 => 1), array(0 => 63, 1 => 1), array(0 => 63, 1 => 1), array(0 => 63, 1 => 1), array(0 => 66, 1 => 1), array(0 => 66, 1 => 2), array(0 => 63, 1 => 1), array(0 => 63, 1 => 1), array(0 => 63, 1 => 1), array(0 => 65, 1 => 2), array(0 => 65, 1 => 3), array(0 => 67, 1 => 2), array(0 => 67, 1 => 0), array(0 => 68, 1 => 1), array(0 => 68, 1 => 1), array(0 => 64, 1 => 2), array(0 => 64, 1 => 4), array(0 => 64, 1 => 3), array(0 => 64, 1 => 4), array(0 => 64, 1 => 3), array(0 => 64, 1 => 5), array(0 => 64, 1 => 5), array(0 => 64, 1 => 6), array(0 => 64, 1 => 5), array(0 => 64, 1 => 3), array(0 => 64, 1 => 2), array(0 => 64, 1 => 4), array(0 => 64, 1 => 5), array(0 => 64, 1 => 6), array(0 => 64, 1 => 2), array(0 => 64, 1 => 3), array(0 => 64, 1 => 2), array(0 => 64, 1 => 3), array(0 => 64, 1 => 11), array(0 => 78, 1 => 2), array(0 => 78, 1 => 1), array(0 => 64, 1 => 5), array(0 => 64, 1 => 7), array(0 => 64, 1 => 2), array(0 => 64, 1 => 7), array(0 => 64, 1 => 10), array(0 => 64, 1 => 7), array(0 => 64, 1 => 10), array(0 => 64, 1 => 3), array(0 => 64, 1 => 4), array(0 => 64, 1 => 2), array(0 => 64, 1 => 2), array(0 => 64, 1 => 3), array(0 => 64, 1 => 4), array(0 => 64, 1 => 5), array(0 => 71, 1 => 2), array(0 => 71, 1 => 1), array(0 => 71, 1 => 0), array(0 => 80, 1 => 4), array(0 => 80, 1 => 2), array(0 => 80, 1 => 2), array(0 => 80, 1 => 2), array(0 => 80, 1 => 2), array(0 => 80, 1 => 2), array(0 => 80, 1 => 4), array(0 => 75, 1 => 1), array(0 => 75, 1 => 3), array(0 => 74, 1 => 4), array(0 => 74, 1 => 3), array(0 => 74, 1 => 3), array(0 => 72, 1 => 1), array(0 => 72, 1 => 1), array(0 => 72, 1 => 4), array(0 => 72, 1 => 3), array(0 => 72, 1 => 3), array(0 => 72, 1 => 3), array(0 => 72, 1 => 1), array(0 => 72, 1 => 2), array(0 => 72, 1 => 3), array(0 => 72, 1 => 2), array(0 => 72, 1 => 3), array(0 => 72, 1 => 3), array(0 => 72, 1 => 3), array(0 => 81, 1 => 8), array(0 => 81, 1 => 7), array(0 => 69, 1 => 1), array(0 => 69, 1 => 2), array(0 => 69, 1 => 2), array(0 => 69, 1 => 2), array(0 => 69, 1 => 2), array(0 => 69, 1 => 1), array(0 => 69, 1 => 1), array(0 => 69, 1 => 3), array(0 => 69, 1 => 2), array(0 => 69, 1 => 2), array(0 => 69, 1 => 1), array(0 => 69, 1 => 1), array(0 => 69, 1 => 3), array(0 => 69, 1 => 1), array(0 => 69, 1 => 1), array(0 => 69, 1 => 3), array(0 => 69, 1 => 2), array(0 => 69, 1 => 2), array(0 => 69, 1 => 1), array(0 => 69, 1 => 3), array(0 => 86, 1 => 1), array(0 => 86, 1 => 1), array(0 => 86, 1 => 1), array(0 => 85, 1 => 1), array(0 => 85, 1 => 4), array(0 => 85, 1 => 1), array(0 => 85, 1 => 3), array(0 => 85, 1 => 4), array(0 => 85, 1 => 3), array(0 => 85, 1 => 4), array(0 => 73, 1 => 3), array(0 => 91, 1 => 2), array(0 => 91, 1 => 0), array(0 => 92, 1 => 3), array(0 => 92, 1 => 5), array(0 => 92, 1 => 2), array(0 => 92, 1 => 2), array(0 => 92, 1 => 4), array(0 => 92, 1 => 3), array(0 => 92, 1 => 5), array(0 => 92, 1 => 3), array(0 => 92, 1 => 2), array(0 => 77, 1 => 1), array(0 => 77, 1 => 2), array(0 => 93, 1 => 1), array(0 => 93, 1 => 3), array(0 => 90, 1 => 2), array(0 => 94, 1 => 1), array(0 => 94, 1 => 2), array(0 => 95, 1 => 3), array(0 => 95, 1 => 4), array(0 => 95, 1 => 5), array(0 => 95, 1 => 6), array(0 => 95, 1 => 2), array(0 => 87, 1 => 4), array(0 => 96, 1 => 4), array(0 => 96, 1 => 5), array(0 => 97, 1 => 3), array(0 => 97, 1 => 1), array(0 => 97, 1 => 0), array(0 => 70, 1 => 3), array(0 => 70, 1 => 2), array(0 => 98, 1 => 3), array(0 => 98, 1 => 2), array(0 => 79, 1 => 2), array(0 => 79, 1 => 0), array(0 => 99, 1 => 2), array(0 => 99, 1 => 2), array(0 => 89, 1 => 1), array(0 => 89, 1 => 2), array(0 => 89, 1 => 1), array(0 => 89, 1 => 3), array(0 => 89, 1 => 4), array(0 => 83, 1 => 1), array(0 => 83, 1 => 1), array(0 => 84, 1 => 1), array(0 => 82, 1 => 3), array(0 => 100, 1 => 1), array(0 => 100, 1 => 3), array(0 => 100, 1 => 0), array(0 => 101, 1 => 3), array(0 => 101, 1 => 3), array(0 => 101, 1 => 1), array(0 => 88, 1 => 2), array(0 => 88, 1 => 3), array(0 => 102, 1 => 2), array(0 => 102, 1 => 1), array(0 => 103, 1 => 3), array(0 => 103, 1 => 3), array(0 => 103, 1 => 1), array(0 => 103, 1 => 3), array(0 => 103, 1 => 3), array(0 => 103, 1 => 2), array(0 => 103, 1 => 1), array(0 => 76, 1 => 1), array(0 => 76, 1 => 0),);

    public static $yyReduceMap = array(0 => 0, 1 => 1, 2 => 2, 4 => 4, 5 => 5, 6 => 6, 7 => 7, 8 => 8, 9 => 9, 10 => 10, 19 => 10, 20 => 10, 41 => 10, 63 => 10, 64 => 10, 71 => 10, 72 => 10, 77 => 10, 86 => 10, 91 => 10, 92 => 10, 97 => 10, 99 => 10, 100 => 10, 104 => 10, 106 => 10, 107 => 10, 108 => 10, 111 => 10, 128 => 10, 163 => 10, 168 => 10, 180 => 10, 11 => 11, 12 => 12, 13 => 13, 14 => 14, 15 => 15, 18 => 15, 181 => 15, 16 => 16, 70 => 16, 17 => 17, 87 => 17, 89 => 17, 90 => 17, 117 => 17, 21 => 21, 22 => 22, 23 => 23, 25 => 23, 24 => 24, 26 => 26, 27 => 26, 28 => 28, 29 => 29, 30 => 30, 31 => 31, 32 => 32, 33 => 33, 34 => 34, 35 => 35, 36 => 36, 38 => 36, 37 => 37, 39 => 39, 40 => 40, 42 => 42, 43 => 43, 44 => 44, 45 => 45, 47 => 45, 46 => 46, 48 => 46, 49 => 49, 50 => 50, 51 => 51, 52 => 52, 53 => 53, 54 => 54, 55 => 55, 56 => 56, 57 => 57, 66 => 57, 144 => 57, 148 => 57, 152 => 57, 153 => 57, 58 => 58, 145 => 58, 151 => 58, 59 => 59, 60 => 60, 61 => 60, 62 => 62, 65 => 65, 67 => 67, 68 => 68, 69 => 68, 73 => 73, 74 => 74, 75 => 74, 76 => 74, 78 => 78, 103 => 78, 79 => 79, 80 => 80, 81 => 81, 82 => 82, 83 => 83, 84 => 84, 85 => 85, 88 => 88, 93 => 93, 94 => 94, 95 => 95, 96 => 96, 98 => 98, 101 => 101, 102 => 102, 105 => 105, 109 => 109, 110 => 110, 112 => 112, 113 => 113, 114 => 114, 115 => 115, 116 => 116, 118 => 118, 165 => 118, 119 => 119, 120 => 120, 121 => 121, 122 => 122, 123 => 123, 126 => 123, 124 => 124, 125 => 125, 127 => 127, 129 => 129, 130 => 130, 131 => 131, 132 => 132, 133 => 133, 134 => 134, 135 => 135, 136 => 136, 137 => 137, 138 => 138, 139 => 139, 140 => 140, 141 => 141, 142 => 142, 143 => 143, 146 => 146, 147 => 147, 149 => 149, 150 => 150, 154 => 154, 155 => 155, 156 => 156, 157 => 157, 158 => 158, 159 => 159, 160 => 160, 161 => 161, 162 => 162, 164 => 164, 166 => 166, 167 => 167, 169 => 169, 170 => 170, 171 => 171, 172 => 172, 173 => 173, 174 => 173, 176 => 173, 175 => 175, 177 => 177, 178 => 178, 179 => 179,);

    #line 214 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r0()
    {
        $this->_retvalue = $this->root_buffer->to_smarty_php();
    }

    #line 222 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r1()
    {
        if ($this->yystack[$this->yyidx + 0]->minor != null) {
            $this->current_buffer->append_subtree($this->yystack[$this->yyidx + 0]->minor);
        }
    }

    #line 229 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r2()
    {
        if ($this->yystack[$this->yyidx + 0]->minor != null) {
            // because of possible code injection
            $this->current_buffer->append_subtree($this->yystack[$this->yyidx + 0]->minor);
        }
    }

    #line 243 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r4()
    {
        if ($this->compiler->has_code) {
            $this->_retvalue = $this->mergePrefixCode($this->yystack[$this->yyidx + - 1]->minor);
        } else {
            $this->_retvalue = null;
        }
        $this->compiler->has_variable_string = false;
        $this->block_nesting_level = count($this->compiler->_tag_stack);
    }

    #line 254 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r5()
    {
        $this->_retvalue = null;
    }

    #line 259 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r6()
    {
        $this->_retvalue = new Smarty_Internal_ParseTree_Text($this, $this->yystack[$this->yyidx + 0]->minor);
    }

    #line 263 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r7()
    {
        $code = $this->compiler->compileTag('private_php', array(array('code' => $this->yystack[$this->yyidx + 0]->minor), array('type' => $this->lex->phpType)), array());
        if ($this->compiler->has_code && !empty($code)) {
            $tmp = '';
            foreach ($this->compiler->prefix_code as $code) {
                $tmp .= $code;
            }
            $this->compiler->prefix_code = array();
            $this->_retvalue = new Smarty_Internal_ParseTree_Tag($this, $this->compiler->processNocacheCode($tmp . $code, true));
        } else {
            $this->_retvalue = null;
        }
    }

    #line 276 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r8()
    {
        $this->compiler->tag_nocache = true;
        $xml = $this->yystack[$this->yyidx + 0]->minor;
        $save = $this->template->has_nocache_code;
        $this->_retvalue = new Smarty_Internal_ParseTree_Tag($this, $this->compiler->processNocacheCode("<?php echo '{$xml}';?>", $this->compiler, true));
        $this->template->has_nocache_code = $save;
    }

    #line 285 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r9()
    {
        $this->_retvalue = $this->compiler->processText($this->yystack[$this->yyidx + 0]->minor);
    }

    #line 289 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r10()
    {
        $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 293 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r11()
    {
        $this->_retvalue = $this->yystack[$this->yyidx + - 1]->minor . $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 298 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r12()
    {
        $this->strip = true;
    }

    #line 302 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r13()
    {
        $this->strip = false;
    }

    #line 306 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r14()
    {
        if ($this->strip) {
            SMARTY_INTERNAL_COMPILE_BLOCK::blockSource($this->compiler, preg_replace('![\t ]*[\r\n]+[\t ]*!', '', $this->yystack[$this->yyidx + 0]->minor));
        } else {
            SMARTY_INTERNAL_COMPILE_BLOCK::blockSource($this->compiler, $this->yystack[$this->yyidx + 0]->minor);
        }
    }

    #line 315 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r15()
    {
        $this->_retvalue = '';
    }

    #line 319 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r16()
    {
        $this->_retvalue = $this->yystack[$this->yyidx + - 1]->minor;
    }

    #line 323 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r17()
    {
        $this->_retvalue = $this->yystack[$this->yyidx + - 1]->minor . $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 344 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r21()
    {
        $this->_retvalue = $this->compiler->compileTag('private_print_expression', array(), array('value' => $this->yystack[$this->yyidx + 0]->minor));
    }

    #line 348 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r22()
    {
        $this->_retvalue = $this->compiler->compileTag('private_print_expression', $this->yystack[$this->yyidx + 0]->minor, array('value' => $this->yystack[$this->yyidx + - 2]->minor, 'modifierlist' => $this->yystack[$this->yyidx + - 1]->minor));
    }

    #line 352 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r23()
    {
        $this->_retvalue = $this->compiler->compileTag('private_print_expression', $this->yystack[$this->yyidx + 0]->minor, array('value' => $this->yystack[$this->yyidx + - 1]->minor));
    }

    #line 356 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r24()
    {
        $this->_retvalue = $this->compiler->compileTag('private_print_expression', $this->yystack[$this->yyidx + 0]->minor, array('value' => $this->yystack[$this->yyidx + - 2]->minor, 'modifierlist' => $this->yystack[$this->yyidx + - 1]->minor));
    }

    #line 369 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r26()
    {
        $this->_retvalue = $this->compiler->compileTag('assign', array(array('value' => $this->yystack[$this->yyidx + 0]->minor), array('var' => "'" . $this->yystack[$this->yyidx + - 2]->minor . "'")));
    }

    #line 377 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r28()
    {
        $this->_retvalue = $this->compiler->compileTag('assign', array_merge(array(array('value' => $this->yystack[$this->yyidx + - 1]->minor), array('var' => "'" . $this->yystack[$this->yyidx + - 3]->minor . "'")), $this->yystack[$this->yyidx + 0]->minor));
    }

    #line 381 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r29()
    {
        $this->_retvalue = $this->compiler->compileTag('assign', array_merge(array(array('value' => $this->yystack[$this->yyidx + - 1]->minor), array('var' => $this->yystack[$this->yyidx + - 3]->minor['var'])), $this->yystack[$this->yyidx + 0]->minor), array('smarty_internal_index' => $this->yystack[$this->yyidx + - 3]->minor['smarty_internal_index']));
    }

    #line 386 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r30()
    {
        if (defined($this->yystack[$this->yyidx + - 1]->minor)) {
            if (isset($this->smarty->security_policy)) {
                $this->smarty->security_policy->isTrustedConstant($this->yystack[$this->yyidx + - 1]->minor, $this->compiler);
            }
            $this->_retvalue = $this->compiler->compileTag('private_print_expression', $this->yystack[$this->yyidx + 0]->minor, array('value' => $this->yystack[$this->yyidx + - 1]->minor));
        } else {
            $this->_retvalue = $this->compiler->compileTag($this->yystack[$this->yyidx + - 1]->minor, $this->yystack[$this->yyidx + 0]->minor);
        }
    }

    #line 396 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r31()
    {
        if (defined($this->yystack[$this->yyidx + 0]->minor)) {
            if (isset($this->smarty->security_policy)) {
                $this->smarty->security_policy->isTrustedConstant($this->yystack[$this->yyidx + 0]->minor, $this->compiler);
            }
            $this->_retvalue = $this->compiler->compileTag('private_print_expression', array(), array('value' => $this->yystack[$this->yyidx + 0]->minor));
        } else {
            $this->_retvalue = $this->compiler->compileTag($this->yystack[$this->yyidx + 0]->minor, array());
        }
    }

    #line 409 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r32()
    {
        if (defined($this->yystack[$this->yyidx + - 2]->minor)) {
            if (isset($this->smarty->security_policy)) {
                $this->smarty->security_policy->isTrustedConstant($this->yystack[$this->yyidx + - 2]->minor, $this->compiler);
            }
            $this->_retvalue = $this->compiler->compileTag('private_print_expression', $this->yystack[$this->yyidx + 0]->minor, array('value' => $this->yystack[$this->yyidx + - 2]->minor, 'modifierlist' => $this->yystack[$this->yyidx + - 1]->minor));
        } else {
            $this->_retvalue = '<?php ob_start();?>' . $this->compiler->compileTag($this->yystack[$this->yyidx + - 2]->minor, $this->yystack[$this->yyidx + 0]->minor) . '<?php echo ';
            $this->_retvalue .= $this->compiler->compileTag('private_modifier', array(), array('modifierlist' => $this->yystack[$this->yyidx + - 1]->minor, 'value' => 'ob_get_clean()')) . ';?>';
        }
    }

    #line 422 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r33()
    {
        $this->_retvalue = $this->compiler->compileTag($this->yystack[$this->yyidx + - 3]->minor, $this->yystack[$this->yyidx + 0]->minor, array('object_method' => $this->yystack[$this->yyidx + - 1]->minor));
    }

    #line 427 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r34()
    {
        $this->_retvalue = '<?php ob_start();?>' . $this->compiler->compileTag($this->yystack[$this->yyidx + - 4]->minor, $this->yystack[$this->yyidx + 0]->minor, array('object_method' => $this->yystack[$this->yyidx + - 2]->minor)) . '<?php echo ';
        $this->_retvalue .= $this->compiler->compileTag('private_modifier', array(), array('modifierlist' => $this->yystack[$this->yyidx + - 1]->minor, 'value' => 'ob_get_clean()')) . ';?>';
    }

    #line 433 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r35()
    {
        $tag = trim(substr($this->yystack[$this->yyidx + - 1]->minor, $this->lex->ldel_length));
        $this->_retvalue = $this->compiler->compileTag(($tag == 'else if') ? 'elseif' : $tag, array(), array('if condition' => $this->yystack[$this->yyidx + 0]->minor));
    }

    #line 438 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r36()
    {
        $tag = trim(substr($this->yystack[$this->yyidx + - 2]->minor, $this->lex->ldel_length));
        $this->_retvalue = $this->compiler->compileTag(($tag == 'else if') ? 'elseif' : $tag, $this->yystack[$this->yyidx + 0]->minor, array('if condition' => $this->yystack[$this->yyidx + - 1]->minor));
    }

    #line 443 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r37()
    {
        $tag = trim(substr($this->yystack[$this->yyidx + - 1]->minor, $this->lex->ldel_length));
        $this->_retvalue = $this->compiler->compileTag(($tag == 'else if') ? 'elseif' : $tag, array(), array('if condition' => $this->yystack[$this->yyidx + 0]->minor));
    }

    #line 454 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r39()
    {
        $this->_retvalue = $this->compiler->compileTag('for', array_merge($this->yystack[$this->yyidx + 0]->minor, array(array('start' => $this->yystack[$this->yyidx + - 9]->minor), array('ifexp' => $this->yystack[$this->yyidx + - 6]->minor), array('var' => $this->yystack[$this->yyidx + - 2]->minor), array('step' => $this->yystack[$this->yyidx + - 1]->minor))), 1);
    }

    #line 458 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r40()
    {
        $this->_retvalue = '=' . $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 466 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r42()
    {
        $this->_retvalue = $this->compiler->compileTag('for', array_merge($this->yystack[$this->yyidx + 0]->minor, array(array('start' => $this->yystack[$this->yyidx + - 3]->minor), array('to' => $this->yystack[$this->yyidx + - 1]->minor))), 0);
    }

    #line 470 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r43()
    {
        $this->_retvalue = $this->compiler->compileTag('for', array_merge($this->yystack[$this->yyidx + 0]->minor, array(array('start' => $this->yystack[$this->yyidx + - 5]->minor), array('to' => $this->yystack[$this->yyidx + - 3]->minor), array('step' => $this->yystack[$this->yyidx + - 1]->minor))), 0);
    }

    #line 475 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r44()
    {
        $this->_retvalue = $this->compiler->compileTag('foreach', $this->yystack[$this->yyidx + 0]->minor);
    }

    #line 480 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r45()
    {
        $this->_retvalue = $this->compiler->compileTag('foreach', array_merge($this->yystack[$this->yyidx + 0]->minor, array(array('from' => $this->yystack[$this->yyidx + - 4]->minor), array('item' => $this->yystack[$this->yyidx + - 1]->minor))));
    }

    #line 484 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r46()
    {
        $this->_retvalue = $this->compiler->compileTag('foreach', array_merge($this->yystack[$this->yyidx + 0]->minor, array(array('from' => $this->yystack[$this->yyidx + - 7]->minor), array('item' => $this->yystack[$this->yyidx + - 1]->minor), array('key' => $this->yystack[$this->yyidx + - 4]->minor))));
    }

    #line 497 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r49()
    {
        $this->_retvalue = $this->compiler->compileTag('setfilter', array(), array('modifier_list' => array(array_merge(array($this->yystack[$this->yyidx + - 1]->minor), $this->yystack[$this->yyidx + 0]->minor))));
    }

    #line 501 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r50()
    {
        $this->_retvalue = $this->compiler->compileTag('setfilter', array(), array('modifier_list' => array_merge(array(array_merge(array($this->yystack[$this->yyidx + - 2]->minor), $this->yystack[$this->yyidx + - 1]->minor)), $this->yystack[$this->yyidx + 0]->minor)));
    }

    #line 506 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r51()
    {
        $j = strrpos($this->yystack[$this->yyidx + 0]->minor, '.');
        if ($this->yystack[$this->yyidx + 0]->minor[$j + 1] == 'c') {
            // {$smarty.block.child}
            $this->_retvalue = SMARTY_INTERNAL_COMPILE_BLOCK::compileChildBlock($this->compiler);
        } else {
            // {$smarty.block.parent}
            $this->_retvalue = SMARTY_INTERNAL_COMPILE_BLOCK::compileParentBlock($this->compiler);
        }
    }

    #line 519 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r52()
    {
        $this->_retvalue = $this->compiler->compileTag($this->yystack[$this->yyidx + 0]->minor . 'close', array());
    }

    #line 523 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r53()
    {
        $this->_retvalue = $this->compiler->compileTag($this->yystack[$this->yyidx + - 1]->minor . 'close', array(), array('modifier_list' => $this->yystack[$this->yyidx + 0]->minor));
    }

    #line 528 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r54()
    {
        $this->_retvalue = $this->compiler->compileTag($this->yystack[$this->yyidx + - 2]->minor . 'close', array(), array('object_method' => $this->yystack[$this->yyidx + 0]->minor));
    }

    #line 532 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r55()
    {
        $this->_retvalue = $this->compiler->compileTag($this->yystack[$this->yyidx + - 3]->minor . 'close', array(), array('object_method' => $this->yystack[$this->yyidx + - 1]->minor, 'modifier_list' => $this->yystack[$this->yyidx + 0]->minor));
    }

    #line 540 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r56()
    {
        $this->_retvalue = $this->yystack[$this->yyidx + - 1]->minor;
        $this->_retvalue[] = $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 546 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r57()
    {
        $this->_retvalue = array($this->yystack[$this->yyidx + 0]->minor);
    }

    #line 551 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r58()
    {
        $this->_retvalue = array();
    }

    #line 556 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r59()
    {
        if (defined($this->yystack[$this->yyidx + 0]->minor)) {
            if (isset($this->smarty->security_policy)) {
                $this->smarty->security_policy->isTrustedConstant($this->yystack[$this->yyidx + 0]->minor, $this->compiler);
            }
            $this->_retvalue = array($this->yystack[$this->yyidx + - 2]->minor => $this->yystack[$this->yyidx + 0]->minor);
        } else {
            $this->_retvalue = array($this->yystack[$this->yyidx + - 2]->minor => "'" . $this->yystack[$this->yyidx + 0]->minor . "'");
        }
    }

    #line 567 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r60()
    {
        $this->_retvalue = array(trim($this->yystack[$this->yyidx + - 1]->minor, " =\n\r\t") => $this->yystack[$this->yyidx + 0]->minor);
    }

    #line 575 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r62()
    {
        $this->_retvalue = "'" . $this->yystack[$this->yyidx + 0]->minor . "'";
    }

    #line 587 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r65()
    {
        $this->_retvalue = array($this->yystack[$this->yyidx + - 2]->minor => $this->yystack[$this->yyidx + 0]->minor);
    }

    #line 600 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r67()
    {
        $this->yystack[$this->yyidx + - 2]->minor[] = $this->yystack[$this->yyidx + 0]->minor;
        $this->_retvalue = $this->yystack[$this->yyidx + - 2]->minor;
    }

    #line 605 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r68()
    {
        $this->_retvalue = array('var' => $this->yystack[$this->yyidx + - 2]->minor, 'value' => $this->yystack[$this->yyidx + 0]->minor);
    }

    #line 633 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r73()
    {
        $this->_retvalue = '$_smarty_tpl->getStreamVariable(\'' . $this->yystack[$this->yyidx + - 2]->minor . '://' . $this->yystack[$this->yyidx + 0]->minor . '\')';
    }

    #line 638 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r74()
    {
        $this->_retvalue = $this->yystack[$this->yyidx + - 2]->minor . trim($this->yystack[$this->yyidx + - 1]->minor) . $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 657 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r78()
    {
        $this->_retvalue = $this->compiler->compileTag('private_modifier', array(), array('value' => $this->yystack[$this->yyidx + - 1]->minor, 'modifierlist' => $this->yystack[$this->yyidx + 0]->minor));
    }

    #line 663 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r79()
    {
        $this->_retvalue = (isset($this->yystack[$this->yyidx + - 1]->minor['pre']) ? $this->yystack[$this->yyidx + - 1]->minor['pre'] : '') . $this->yystack[$this->yyidx + - 2]->minor . $this->yystack[$this->yyidx + - 1]->minor['op'] . $this->yystack[$this->yyidx + 0]->minor . (isset($this->yystack[$this->yyidx + - 1]->minor['pre']) ? ')' : '');
    }

    #line 666 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r80()
    {
        $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor . $this->yystack[$this->yyidx + - 1]->minor . ')';
    }

    #line 670 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r81()
    {
        $this->_retvalue = 'in_array(' . $this->yystack[$this->yyidx + - 2]->minor . ',' . $this->yystack[$this->yyidx + 0]->minor . ')';
    }

    #line 674 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r82()
    {
        $this->_retvalue = 'in_array(' . $this->yystack[$this->yyidx + - 2]->minor . ',(array)' . $this->yystack[$this->yyidx + 0]->minor . ')';
    }

    #line 678 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r83()
    {
        $this->_retvalue = $this->yystack[$this->yyidx + - 2]->minor . $this->yystack[$this->yyidx + - 1]->minor . $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 686 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r84()
    {
        $this->_retvalue = $this->yystack[$this->yyidx + - 6]->minor . ' ? ' . $this->compiler->compileVariable("'" . $this->yystack[$this->yyidx + - 2]->minor . "'") . ' : ' . $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 690 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r85()
    {
        $this->_retvalue = $this->yystack[$this->yyidx + - 5]->minor . ' ? ' . $this->yystack[$this->yyidx + - 2]->minor . ' : ' . $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 705 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r88()
    {
        $this->_retvalue = '!' . $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 726 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r93()
    {
        $this->_retvalue = $this->yystack[$this->yyidx + - 2]->minor . '.' . $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 730 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r94()
    {
        $this->_retvalue = $this->yystack[$this->yyidx + - 1]->minor . '.';
    }

    #line 734 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r95()
    {
        $this->_retvalue = '.' . $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 739 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r96()
    {
        if (defined($this->yystack[$this->yyidx + 0]->minor)) {
            if (isset($this->smarty->security_policy)) {
                $this->smarty->security_policy->isTrustedConstant($this->yystack[$this->yyidx + 0]->minor, $this->compiler);
            }
            $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;
        } else {
            $this->_retvalue = "'" . $this->yystack[$this->yyidx + 0]->minor . "'";
        }
    }

    #line 756 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r98()
    {
        $this->_retvalue = "(" . $this->yystack[$this->yyidx + - 1]->minor . ")";
    }

    #line 771 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r101()
    {
        self::$prefix_number ++;
        if ($this->yystack[$this->yyidx + - 2]->minor['var'] == '\'smarty\'') {
            $this->compiler->prefix_code[] = '<?php $_tmp' . self::$prefix_number . ' = ' . $this->compiler->compileTag('private_special_variable', array(), $this->yystack[$this->yyidx + - 2]->minor['smarty_internal_index']) . ';?>';
        } else {
            $this->compiler->prefix_code[] = '<?php $_tmp' . self::$prefix_number . ' = ' . $this->compiler->compileVariable($this->yystack[$this->yyidx + - 2]->minor['var']) . $this->yystack[$this->yyidx + - 2]->minor['smarty_internal_index'] . ';?>';
        }
        $this->_retvalue = '$_tmp' . self::$prefix_number . '::' . $this->yystack[$this->yyidx + 0]->minor[0] . $this->yystack[$this->yyidx + 0]->minor[1];
    }

    #line 783 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r102()
    {
        self::$prefix_number ++;
        $tmp = $this->compiler->appendCode('<?php ob_start();?>', $this->yystack[$this->yyidx + - 1]->minor);
        $this->compiler->prefix_code[] = $this->compiler->appendCode($tmp, '<?php $_tmp' . self::$prefix_number . '=ob_get_clean();?>');
        $this->_retvalue = '$_tmp' . self::$prefix_number;
    }

    #line 800 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r105()
    {
        if (!in_array(strtolower($this->yystack[$this->yyidx + - 2]->minor), array('self', 'parent')) && (!$this->security || $this->smarty->security_policy->isTrustedStaticClassAccess($this->yystack[$this->yyidx + - 2]->minor, $this->yystack[$this->yyidx + 0]->minor, $this->compiler))) {
            if (isset($this->smarty->registered_classes[$this->yystack[$this->yyidx + - 2]->minor])) {
                $this->_retvalue = $this->smarty->registered_classes[$this->yystack[$this->yyidx + - 2]->minor] . '::' . $this->yystack[$this->yyidx + 0]->minor[0] . $this->yystack[$this->yyidx + 0]->minor[1];
            } else {
                $this->_retvalue = $this->yystack[$this->yyidx + - 2]->minor . '::' . $this->yystack[$this->yyidx + 0]->minor[0] . $this->yystack[$this->yyidx + 0]->minor[1];
            }
        } else {
            $this->compiler->trigger_template_error("static class '" . $this->yystack[$this->yyidx + - 2]->minor . "' is undefined or not allowed by security setting");
        }
    }

    #line 834 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r109()
    {
        if ($this->yystack[$this->yyidx + 0]->minor['var'] == '\'smarty\'') {
            $smarty_var = $this->compiler->compileTag('private_special_variable', array(), $this->yystack[$this->yyidx + 0]->minor['smarty_internal_index']);
            $this->_retvalue = $smarty_var;
        } else {
            // used for array reset,next,prev,end,current 
            $this->last_variable = $this->yystack[$this->yyidx + 0]->minor['var'];
            $this->last_index = $this->yystack[$this->yyidx + 0]->minor['smarty_internal_index'];
            $this->_retvalue = $this->compiler->compileVariable($this->yystack[$this->yyidx + 0]->minor['var']) . $this->yystack[$this->yyidx + 0]->minor['smarty_internal_index'];
        }
    }

    #line 847 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r110()
    {
        $this->_retvalue = '$_smarty_tpl->tpl_vars[' . $this->yystack[$this->yyidx + - 2]->minor . ']->' . $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 857 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r112()
    {
        $this->_retvalue = '$_smarty_tpl->getConfigVariable( \'' . $this->yystack[$this->yyidx + - 1]->minor . '\')';
    }

    #line 861 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r113()
    {
        $this->_retvalue = '(is_array($tmp = $_smarty_tpl->getConfigVariable( \'' . $this->yystack[$this->yyidx + - 2]->minor . '\')) ? $tmp' . $this->yystack[$this->yyidx + 0]->minor . ' :null)';
    }

    #line 865 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r114()
    {
        $this->_retvalue = '$_smarty_tpl->getConfigVariable( ' . $this->yystack[$this->yyidx + - 1]->minor . ')';
    }

    #line 869 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r115()
    {
        $this->_retvalue = '(is_array($tmp = $_smarty_tpl->getConfigVariable( ' . $this->yystack[$this->yyidx + - 2]->minor . ')) ? $tmp' . $this->yystack[$this->yyidx + 0]->minor . ' : null)';
    }

    #line 873 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r116()
    {
        $this->_retvalue = array('var' => $this->yystack[$this->yyidx + - 1]->minor, 'smarty_internal_index' => $this->yystack[$this->yyidx + 0]->minor);
    }

    #line 886 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r118()
    {
        return;
    }

    #line 892 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r119()
    {
        $this->_retvalue = '[' . $this->compiler->compileVariable($this->yystack[$this->yyidx + 0]->minor) . ']';
    }

    #line 896 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r120()
    {
        $this->_retvalue = '[' . $this->compiler->compileVariable($this->yystack[$this->yyidx + - 2]->minor) . '->' . $this->yystack[$this->yyidx + 0]->minor . ']';
    }

    #line 900 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r121()
    {
        if (defined($this->yystack[$this->yyidx + 0]->minor)) {
            if (isset($this->smarty->security_policy)) {
                $this->smarty->security_policy->isTrustedConstant($this->yystack[$this->yyidx + 0]->minor, $this->compiler);
            }
            $this->_retvalue = "[" . $this->yystack[$this->yyidx + 0]->minor . "]";
        } else {
            $this->_retvalue = "['" . $this->yystack[$this->yyidx + 0]->minor . "']";
        }
    }

    #line 911 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r122()
    {
        $this->_retvalue = "[" . $this->yystack[$this->yyidx + 0]->minor . "]";
    }

    #line 916 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r123()
    {
        $this->_retvalue = "[" . $this->yystack[$this->yyidx + - 1]->minor . "]";
    }

    #line 921 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r124()
    {
        $this->_retvalue = '[' . $this->compiler->compileTag('private_special_variable', array(), '[\'section\'][\'' . $this->yystack[$this->yyidx + - 1]->minor . '\'][\'index\']') . ']';
    }

    #line 925 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r125()
    {
        $this->_retvalue = '[' . $this->compiler->compileTag('private_special_variable', array(), '[\'section\'][\'' . $this->yystack[$this->yyidx + - 3]->minor . '\'][\'' . $this->yystack[$this->yyidx + - 1]->minor . '\']') . ']';
    }

    #line 935 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r127()
    {
        $this->_retvalue = '[]';
    }

    #line 949 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r129()
    {
        $this->_retvalue = $this->yystack[$this->yyidx + - 1]->minor . '.' . $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 954 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r130()
    {
        $this->_retvalue = '\'' . $this->yystack[$this->yyidx + 0]->minor . '\'';
    }

    #line 959 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r131()
    {
        $this->_retvalue = '(' . $this->yystack[$this->yyidx + - 1]->minor . ')';
    }

    #line 966 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r132()
    {
        if ($this->yystack[$this->yyidx + - 1]->minor['var'] == '\'smarty\'') {
            $this->_retvalue = $this->compiler->compileTag('private_special_variable', array(), $this->yystack[$this->yyidx + - 1]->minor['smarty_internal_index']) . $this->yystack[$this->yyidx + 0]->minor;
        } else {
            $this->_retvalue = $this->compiler->compileVariable($this->yystack[$this->yyidx + - 1]->minor['var']) . $this->yystack[$this->yyidx + - 1]->minor['smarty_internal_index'] . $this->yystack[$this->yyidx + 0]->minor;
        }
    }

    #line 975 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r133()
    {
        $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 980 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r134()
    {
        $this->_retvalue = $this->yystack[$this->yyidx + - 1]->minor . $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 985 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r135()
    {
        if ($this->security && substr($this->yystack[$this->yyidx + - 1]->minor, 0, 1) == '_') {
            $this->compiler->trigger_template_error(self::Err1);
        }
        $this->_retvalue = '->' . $this->yystack[$this->yyidx + - 1]->minor . $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 992 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r136()
    {
        if ($this->security) {
            $this->compiler->trigger_template_error(self::Err2);
        }
        $this->_retvalue = '->{' . $this->compiler->compileVariable($this->yystack[$this->yyidx + - 1]->minor) . $this->yystack[$this->yyidx + 0]->minor . '}';
    }

    #line 999 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r137()
    {
        if ($this->security) {
            $this->compiler->trigger_template_error(self::Err2);
        }
        $this->_retvalue = '->{' . $this->yystack[$this->yyidx + - 2]->minor . $this->yystack[$this->yyidx + 0]->minor . '}';
    }

    #line 1006 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r138()
    {
        if ($this->security) {
            $this->compiler->trigger_template_error(self::Err2);
        }
        $this->_retvalue = '->{\'' . $this->yystack[$this->yyidx + - 4]->minor . '\'.' . $this->yystack[$this->yyidx + - 2]->minor . $this->yystack[$this->yyidx + 0]->minor . '}';
    }

    #line 1014 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r139()
    {
        $this->_retvalue = '->' . $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 1022 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r140()
    {
        if (!$this->security || $this->smarty->security_policy->isTrustedPhpFunction($this->yystack[$this->yyidx + - 3]->minor, $this->compiler)) {
            if (strcasecmp($this->yystack[$this->yyidx + - 3]->minor, 'isset') === 0 || strcasecmp($this->yystack[$this->yyidx + - 3]->minor, 'empty') === 0 || strcasecmp($this->yystack[$this->yyidx + - 3]->minor, 'array') === 0 || is_callable($this->yystack[$this->yyidx + - 3]->minor)) {
                $func_name = strtolower($this->yystack[$this->yyidx + - 3]->minor);
                if ($func_name == 'isset') {
                    if (count($this->yystack[$this->yyidx + - 1]->minor) == 0) {
                        $this->compiler->trigger_template_error('Illegal number of paramer in "isset()"');
                    }
                    $par = implode(',', $this->yystack[$this->yyidx + - 1]->minor);
                    if (strncasecmp($par, '$_smarty_tpl->getConfigVariable', strlen('$_smarty_tpl->getConfigVariable')) === 0) {
                        self::$prefix_number ++;
                        $this->compiler->prefix_code[] = '<?php $_tmp' . self::$prefix_number . '=' . str_replace(')', ', false)', $par) . ';?>';
                        $isset_par = '$_tmp' . self::$prefix_number;
                    } else {
                        $isset_par = str_replace("')->value", "',null,true,false)->value", $par);
                    }
                    $this->_retvalue = $this->yystack[$this->yyidx + - 3]->minor . "(" . $isset_par . ")";
                } elseif (in_array($func_name, array('empty', 'reset', 'current', 'end', 'prev', 'next'))) {
                    if (count($this->yystack[$this->yyidx + - 1]->minor) != 1) {
                        $this->compiler->trigger_template_error('Illegal number of paramer in "empty()"');
                    }
                    if ($func_name == 'empty') {
                        $this->_retvalue = $func_name . '(' . str_replace("')->value", "',null,true,false)->value", $this->yystack[$this->yyidx + - 1]->minor[0]) . ')';
                    } else {
                        $this->_retvalue = $func_name . '(' . $this->yystack[$this->yyidx + - 1]->minor[0] . ')';
                    }
                } else {
                    $this->_retvalue = $this->yystack[$this->yyidx + - 3]->minor . "(" . implode(',', $this->yystack[$this->yyidx + - 1]->minor) . ")";
                }
            } else {
                $this->compiler->trigger_template_error("unknown function \"" . $this->yystack[$this->yyidx + - 3]->minor . "\"");
            }
        }
    }

    #line 1061 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r141()
    {
        if ($this->security && substr($this->yystack[$this->yyidx + - 3]->minor, 0, 1) == '_') {
            $this->compiler->trigger_template_error(self::Err1);
        }
        $this->_retvalue = $this->yystack[$this->yyidx + - 3]->minor . "(" . implode(',', $this->yystack[$this->yyidx + - 1]->minor) . ")";
    }

    #line 1068 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r142()
    {
        if ($this->security) {
            $this->compiler->trigger_template_error(self::Err2);
        }
        self::$prefix_number ++;
        $this->compiler->prefix_code[] = '<?php $_tmp' . self::$prefix_number . '=' . $this->compiler->compileVariable("'" . $this->yystack[$this->yyidx + - 3]->minor . "'") . ';?>';
        $this->_retvalue = '$_tmp' . self::$prefix_number . '(' . implode(',', $this->yystack[$this->yyidx + - 1]->minor) . ')';
    }

    #line 1079 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r143()
    {
        $this->_retvalue = array_merge($this->yystack[$this->yyidx + - 2]->minor, array($this->yystack[$this->yyidx + 0]->minor));
    }

    #line 1096 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r146()
    {
        $this->_retvalue = array_merge($this->yystack[$this->yyidx + - 2]->minor, array(array_merge($this->yystack[$this->yyidx + - 1]->minor, $this->yystack[$this->yyidx + 0]->minor)));
    }

    #line 1100 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r147()
    {
        $this->_retvalue = array(array_merge($this->yystack[$this->yyidx + - 1]->minor, $this->yystack[$this->yyidx + 0]->minor));
    }

    #line 1108 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r149()
    {
        $this->_retvalue = array($this->yystack[$this->yyidx + 0]->minor);
    }

    #line 1116 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r150()
    {
        $this->_retvalue = array_merge($this->yystack[$this->yyidx + - 1]->minor, $this->yystack[$this->yyidx + 0]->minor);
    }

    #line 1135 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r154()
    {
        $this->_retvalue = array($this->yystack[$this->yyidx + 0]->minor, '', 'method');
    }

    #line 1140 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r155()
    {
        $this->_retvalue = array($this->yystack[$this->yyidx + - 1]->minor, $this->yystack[$this->yyidx + 0]->minor, 'method');
    }

    #line 1145 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r156()
    {
        $this->_retvalue = array($this->yystack[$this->yyidx + 0]->minor, '');
    }

    #line 1150 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r157()
    {
        $this->_retvalue = array('$' . $this->yystack[$this->yyidx + - 1]->minor, $this->yystack[$this->yyidx + 0]->minor, 'property');
    }

    #line 1155 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r158()
    {
        $this->_retvalue = array('$' . $this->yystack[$this->yyidx + - 2]->minor, $this->yystack[$this->yyidx + - 1]->minor . $this->yystack[$this->yyidx + 0]->minor, 'property');
    }

    #line 1161 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r159()
    {
        $this->_retvalue['op'] = ' ' . trim($this->yystack[$this->yyidx + 0]->minor) . ' ';
    }

    #line 1165 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r160()
    {
        static $lops = array('eq' => array('op' => ' == ', 'pre' => null), 'ne' => array('op' => ' != ', 'pre' => null), 'neq' => array('op' => ' != ', 'pre' => null), 'gt' => array('op' => ' > ', 'pre' => null), 'ge' => array('op' => ' >= ', 'pre' => null), 'gte' => array('op' => ' >= ', 'pre' => null), 'lt' => array('op' => ' < ', 'pre' => null), 'le' => array('op' => ' <= ', 'pre' => null), 'lte' => array('op' => ' <= ', 'pre' => null), 'mod' => array('op' => ' % ', 'pre' => null), 'and' => array('op' => ' && ', 'pre' => null), 'or' => array('op' => ' || ', 'pre' => null), 'xor' => array('op' => ' xor ', 'pre' => null), 'isdivby' => array('op' => ' % ', 'pre' => '!('), 'isnotdivby' => array('op' => ' % ', 'pre' => '('), 'isevenby' => array('op' => ' / ', 'pre' => '!(1 & '), 'isnotevenby' => array('op' => ' / ', 'pre' => '(1 & '), 'isoddby' => array('op' => ' / ', 'pre' => '(1 & '), 'isnotoddby' => array('op' => ' / ', 'pre' => '!(1 & '),);
        $op = strtolower(str_replace(' ', '', $this->yystack[$this->yyidx + 0]->minor));
        $this->_retvalue = $lops[$op];
    }

    #line 1191 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r161()
    {
        static $scond = array('iseven' => '!(1 & ', 'isnoteven' => '(1 & ', 'isodd' => '(1 & ', 'isnotodd' => '!(1 & ',);
        $op = strtolower(str_replace(' ', '', $this->yystack[$this->yyidx + 0]->minor));
        $this->_retvalue = $scond[$op];
    }

    #line 1205 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r162()
    {
        $this->_retvalue = 'array(' . $this->yystack[$this->yyidx + - 1]->minor . ')';
    }

    #line 1213 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r164()
    {
        $this->_retvalue = $this->yystack[$this->yyidx + - 2]->minor . ',' . $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 1221 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r166()
    {
        $this->_retvalue = $this->yystack[$this->yyidx + - 2]->minor . '=>' . $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 1225 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r167()
    {
        $this->_retvalue = '\'' . $this->yystack[$this->yyidx + - 2]->minor . '\'=>' . $this->yystack[$this->yyidx + 0]->minor;
    }

    #line 1237 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r169()
    {
        $this->_retvalue = "''";
    }

    #line 1241 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r170()
    {
        $this->_retvalue = $this->yystack[$this->yyidx + - 1]->minor->to_smarty_php();
    }

    #line 1246 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r171()
    {
        $this->yystack[$this->yyidx + - 1]->minor->append_subtree($this->yystack[$this->yyidx + 0]->minor);
        $this->_retvalue = $this->yystack[$this->yyidx + - 1]->minor;
    }

    #line 1251 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r172()
    {
        $this->_retvalue = new Smarty_Internal_ParseTree_Dq($this, $this->yystack[$this->yyidx + 0]->minor);
    }

    #line 1255 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r173()
    {
        $this->_retvalue = new Smarty_Internal_ParseTree_Code($this, '(string)' . $this->yystack[$this->yyidx + - 1]->minor);
    }

    #line 1263 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r175()
    {
        $this->_retvalue = new Smarty_Internal_ParseTree_Code($this, '(string)$_smarty_tpl->tpl_vars[\'' . substr($this->yystack[$this->yyidx + 0]->minor, 1) . '\']->value');
    }

    #line 1271 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r177()
    {
        $this->_retvalue = new Smarty_Internal_ParseTree_Code($this, '(string)(' . $this->yystack[$this->yyidx + - 1]->minor . ')');
    }

    #line 1275 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r178()
    {
        $this->_retvalue = new Smarty_Internal_ParseTree_Tag($this, $this->yystack[$this->yyidx + - 1]->minor);
    }

    #line 1279 "../smarty/lexer/smarty_internal_templateparser.y"
    function yy_r179()
    {
        $this->_retvalue = new Smarty_Internal_ParseTree_DqContent($this, $this->yystack[$this->yyidx + 0]->minor);
    }

    private $_retvalue;

    public function yy_reduce($yyruleno)
    {
        if ($this->yyTraceFILE && $yyruleno >= 0 && $yyruleno < count(self::$yyRuleName)) {
            fprintf($this->yyTraceFILE, "%sReduce (%d) [%s].\n", $this->yyTracePrompt, $yyruleno, self::$yyRuleName[$yyruleno]);
        }

        $this->_retvalue = $yy_lefthand_side = null;
        if (isset(self::$yyReduceMap[$yyruleno])) {
            // call the action
            $this->_retvalue = null;
            $this->{'yy_r' . self::$yyReduceMap[$yyruleno]}();
            $yy_lefthand_side = $this->_retvalue;
        }
        $yygoto = self::$yyRuleInfo[$yyruleno][0];
        $yysize = self::$yyRuleInfo[$yyruleno][1];
        $this->yyidx -= $yysize;
        for ($i = $yysize; $i; $i --) {
            // pop all of the right-hand side parameters
            array_pop($this->yystack);
        }
        $yyact = $this->yy_find_reduce_action($this->yystack[$this->yyidx]->stateno, $yygoto);
        if ($yyact < self::YYNSTATE) {
            if (!$this->yyTraceFILE && $yysize) {
                $this->yyidx ++;
                $x = new TP_yyStackEntry;
                $x->stateno = $yyact;
                $x->major = $yygoto;
                $x->minor = $yy_lefthand_side;
                $this->yystack[$this->yyidx] = $x;
            } else {
                $this->yy_shift($yyact, $yygoto, $yy_lefthand_side);
            }
        } elseif ($yyact == self::YYNSTATE + self::YYNRULE + 1) {
            $this->yy_accept();
        }
    }

    public function yy_parse_failed()
    {
        if ($this->yyTraceFILE) {
            fprintf($this->yyTraceFILE, "%sFail!\n", $this->yyTracePrompt);
        }
        while ($this->yyidx >= 0) {
            $this->yy_pop_parser_stack();
        }
    }

    public function yy_syntax_error($yymajor, $TOKEN)
    {
        #line 196 "../smarty/lexer/smarty_internal_templateparser.y"

        $this->internalError = true;
        $this->yymajor = $yymajor;
        $this->compiler->trigger_template_error();
    }

    public function yy_accept()
    {
        if ($this->yyTraceFILE) {
            fprintf($this->yyTraceFILE, "%sAccept!\n", $this->yyTracePrompt);
        }
        while ($this->yyidx >= 0) {
            $this->yy_pop_parser_stack();
        }
        #line 189 "../smarty/lexer/smarty_internal_templateparser.y"

        $this->successful = !$this->internalError;
        $this->internalError = false;
        $this->retvalue = $this->_retvalue;
    }

    public function doParse($yymajor, $yytokenvalue)
    {
        $yyerrorhit = 0;   /* True if yymajor has invoked an error */

        if ($this->yyidx === null || $this->yyidx < 0) {
            $this->yyidx = 0;
            $this->yyerrcnt = - 1;
            $x = new TP_yyStackEntry;
            $x->stateno = 0;
            $x->major = 0;
            $this->yystack = array();
            $this->yystack[] = $x;
        }
        $yyendofinput = ($yymajor == 0);

        if ($this->yyTraceFILE) {
            fprintf($this->yyTraceFILE, "%sInput %s\n", $this->yyTracePrompt, $this->yyTokenName[$yymajor]);
        }

        do {
            $yyact = $this->yy_find_shift_action($yymajor);
            if ($yymajor < self::YYERRORSYMBOL && !$this->yy_is_expected_token($yymajor)) {
                // force a syntax error
                $yyact = self::YY_ERROR_ACTION;
            }
            if ($yyact < self::YYNSTATE) {
                $this->yy_shift($yyact, $yymajor, $yytokenvalue);
                $this->yyerrcnt --;
                if ($yyendofinput && $this->yyidx >= 0) {
                    $yymajor = 0;
                } else {
                    $yymajor = self::YYNOCODE;
                }
            } elseif ($yyact < self::YYNSTATE + self::YYNRULE) {
                $this->yy_reduce($yyact - self::YYNSTATE);
            } elseif ($yyact == self::YY_ERROR_ACTION) {
                if ($this->yyTraceFILE) {
                    fprintf($this->yyTraceFILE, "%sSyntax Error!\n", $this->yyTracePrompt);
                }
                if (self::YYERRORSYMBOL) {
                    if ($this->yyerrcnt < 0) {
                        $this->yy_syntax_error($yymajor, $yytokenvalue);
                    }
                    $yymx = $this->yystack[$this->yyidx]->major;
                    if ($yymx == self::YYERRORSYMBOL || $yyerrorhit) {
                        if ($this->yyTraceFILE) {
                            fprintf($this->yyTraceFILE, "%sDiscard input token %s\n", $this->yyTracePrompt, $this->yyTokenName[$yymajor]);
                        }
                        $this->yy_destructor($yymajor, $yytokenvalue);
                        $yymajor = self::YYNOCODE;
                    } else {
                        while ($this->yyidx >= 0 && $yymx != self::YYERRORSYMBOL && ($yyact = $this->yy_find_shift_action(self::YYERRORSYMBOL)) >= self::YYNSTATE) {
                            $this->yy_pop_parser_stack();
                        }
                        if ($this->yyidx < 0 || $yymajor == 0) {
                            $this->yy_destructor($yymajor, $yytokenvalue);
                            $this->yy_parse_failed();
                            $yymajor = self::YYNOCODE;
                        } elseif ($yymx != self::YYERRORSYMBOL) {
                            $u2 = 0;
                            $this->yy_shift($yyact, self::YYERRORSYMBOL, $u2);
                        }
                    }
                    $this->yyerrcnt = 3;
                    $yyerrorhit = 1;
                } else {
                    if ($this->yyerrcnt <= 0) {
                        $this->yy_syntax_error($yymajor, $yytokenvalue);
                    }
                    $this->yyerrcnt = 3;
                    $this->yy_destructor($yymajor, $yytokenvalue);
                    if ($yyendofinput) {
                        $this->yy_parse_failed();
                    }
                    $yymajor = self::YYNOCODE;
                }
            } else {
                $this->yy_accept();
                $yymajor = self::YYNOCODE;
            }
        } while ($yymajor != self::YYNOCODE && $this->yyidx >= 0);
    }
}

