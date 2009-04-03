<?php
/**
* Smarty Internal Plugin Templateparser
*
* This is the template parser.
* It is generated from the internal.templateparser.y file
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews
*/

/**
 * This can be used to store both the string representation of
 * a token, and any useful meta-data associated with the token.
 *
 * meta-data should be stored as an array
 */
class TP_yyToken implements ArrayAccess
{
    public $string = '';
    public $metadata = array();

    function __construct($s, $m = array())
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

    function __toString()
    {
        return $this->_string;
    }

    function offsetExists($offset)
    {
        return isset($this->metadata[$offset]);
    }

    function offsetGet($offset)
    {
        return $this->metadata[$offset];
    }

    function offsetSet($offset, $value)
    {
        if ($offset === null) {
            if (isset($value[0])) {
                $x = ($value instanceof TP_yyToken) ?
                    $value->metadata : $value;
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

    function offsetUnset($offset)
    {
        unset($this->metadata[$offset]);
    }
}

/** The following structure represents a single element of the
 * parser's stack.  Information stored includes:
 *
 *   +  The state number for the parser at this level of the stack.
 *
 *   +  The value of the token stored at this level of the stack.
 *      (In other words, the "major" token.)
 *
 *   +  The semantic value stored at this level of the stack.  This is
 *      the information used by the action routines in the grammar.
 *      It is sometimes called the "minor" token.
 */
class TP_yyStackEntry
{
    public $stateno;       /* The state-number */
    public $major;         /* The major token value.  This is the code
                     ** number for the token at this stack level */
    public $minor; /* The user-supplied minor token value.  This
                     ** is the value of the token  */
};

// code external to the class is included here

// declare_class is output here
#line 12 "internal.templateparser.y"
class Smarty_Internal_Templateparser#line 109 "internal.templateparser.php"
{
/* First off, code is included which follows the "include_class" declaration
** in the input file. */
#line 14 "internal.templateparser.y"

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
    
#line 142 "internal.templateparser.php"

/* Next is all token values, as class constants
*/
/* 
** These constants (all generated automatically by the parser generator)
** specify the various kinds of tokens (terminals) that the parser
** understands. 
**
** Each symbol here is a terminal symbol in the grammar.
*/
    const TP_OTHER                          =  1;
    const TP_LDELSLASH                      =  2;
    const TP_LDEL                           =  3;
    const TP_RDEL                           =  4;
    const TP_XML                            =  5;
    const TP_PHP                            =  6;
    const TP_SHORTTAGSTART                  =  7;
    const TP_SHORTTAGEND                    =  8;
    const TP_COMMENTEND                     =  9;
    const TP_COMMENTSTART                   = 10;
    const TP_NUMBER                         = 11;
    const TP_MATH                           = 12;
    const TP_UNIMATH                        = 13;
    const TP_INCDEC                         = 14;
    const TP_OPENP                          = 15;
    const TP_CLOSEP                         = 16;
    const TP_OPENB                          = 17;
    const TP_CLOSEB                         = 18;
    const TP_DOLLAR                         = 19;
    const TP_DOT                            = 20;
    const TP_COMMA                          = 21;
    const TP_COLON                          = 22;
    const TP_DOUBLECOLON                    = 23;
    const TP_SEMICOLON                      = 24;
    const TP_VERT                           = 25;
    const TP_EQUAL                          = 26;
    const TP_SPACE                          = 27;
    const TP_PTR                            = 28;
    const TP_APTR                           = 29;
    const TP_ID                             = 30;
    const TP_EQUALS                         = 31;
    const TP_NOTEQUALS                      = 32;
    const TP_GREATERTHAN                    = 33;
    const TP_LESSTHAN                       = 34;
    const TP_GREATEREQUAL                   = 35;
    const TP_LESSEQUAL                      = 36;
    const TP_IDENTITY                       = 37;
    const TP_NONEIDENTITY                   = 38;
    const TP_NOT                            = 39;
    const TP_LAND                           = 40;
    const TP_LOR                            = 41;
    const TP_QUOTE                          = 42;
    const TP_SINGLEQUOTE                    = 43;
    const TP_BOOLEAN                        = 44;
    const TP_NULL                           = 45;
    const TP_IN                             = 46;
    const TP_ANDSYM                         = 47;
    const TP_BACKTICK                       = 48;
    const TP_HATCH                          = 49;
    const TP_AT                             = 50;
    const TP_ISODD                          = 51;
    const TP_ISNOTODD                       = 52;
    const TP_ISEVEN                         = 53;
    const TP_ISNOTEVEN                      = 54;
    const TP_ISODDBY                        = 55;
    const TP_ISNOTODDBY                     = 56;
    const TP_ISEVENBY                       = 57;
    const TP_ISNOTEVENBY                    = 58;
    const TP_ISDIVBY                        = 59;
    const TP_ISNOTDIVBY                     = 60;
    const TP_LITERALSTART                   = 61;
    const TP_LITERALEND                     = 62;
    const TP_LDELIMTAG                      = 63;
    const TP_RDELIMTAG                      = 64;
    const TP_PHPSTART                       = 65;
    const TP_PHPEND                         = 66;
    const YY_NO_ACTION = 379;
    const YY_ACCEPT_ACTION = 378;
    const YY_ERROR_ACTION = 377;

/* Next are that tables used to determine what action to take based on the
** current state and lookahead token.  These tables are used to implement
** functions that take a state number and lookahead value and return an
** action integer.  
**
** Suppose the action integer is N.  Then the action is determined as
** follows
**
**   0 <= N < self::YYNSTATE                              Shift N.  That is,
**                                                        push the lookahead
**                                                        token onto the stack
**                                                        and goto state N.
**
**   self::YYNSTATE <= N < self::YYNSTATE+self::YYNRULE   Reduce by rule N-YYNSTATE.
**
**   N == self::YYNSTATE+self::YYNRULE                    A syntax error has occurred.
**
**   N == self::YYNSTATE+self::YYNRULE+1                  The parser accepts its
**                                                        input. (and concludes parsing)
**
**   N == self::YYNSTATE+self::YYNRULE+2                  No such action.  Denotes unused
**                                                        slots in the yy_action[] table.
**
** The action table is constructed as a single large static array $yy_action.
** Given state S and lookahead X, the action is computed as
**
**      self::$yy_action[self::$yy_shift_ofst[S] + X ]
**
** If the index value self::$yy_shift_ofst[S]+X is out of range or if the value
** self::$yy_lookahead[self::$yy_shift_ofst[S]+X] is not equal to X or if
** self::$yy_shift_ofst[S] is equal to self::YY_SHIFT_USE_DFLT, it means that
** the action is not in the table and that self::$yy_default[S] should be used instead.  
**
** The formula above is for computing the action when the lookahead is
** a terminal symbol.  If the lookahead is a non-terminal (as occurs after
** a reduce action) then the static $yy_reduce_ofst array is used in place of
** the static $yy_shift_ofst array and self::YY_REDUCE_USE_DFLT is used in place of
** self::YY_SHIFT_USE_DFLT.
**
** The following are the tables generated in this section:
**
**  self::$yy_action        A single table containing all actions.
**  self::$yy_lookahead     A table containing the lookahead for each entry in
**                          yy_action.  Used to detect hash collisions.
**  self::$yy_shift_ofst    For each state, the offset into self::$yy_action for
**                          shifting terminals.
**  self::$yy_reduce_ofst   For each state, the offset into self::$yy_action for
**                          shifting non-terminals after a reduce.
**  self::$yy_default       Default action for each state.
*/
    const YY_SZ_ACTTAB = 830;
static public $yy_action = array(
 /*     0 */   156,  166,  165,  124,   24,  378,   42,  176,  169,  215,
 /*    10 */   171,  146,  160,  159,  226,  225,    2,    4,    6,    3,
 /*    20 */    10,    7,  141,   24,  166,  165,  195,   27,   33,  133,
 /*    30 */    19,  197,   12,  150,   59,  160,  159,  226,  225,    2,
 /*    40 */     4,    6,    3,   10,    7,  116,   27,   31,   15,  132,
 /*    50 */   197,  143,  201,  187,  218,  182,   93,   35,   49,  198,
 /*    60 */   213,  124,   21,  205,  140,  166,  165,  231,  235,  236,
 /*    70 */   240,  239,  238,  237,  230,  114,  160,  159,  226,  225,
 /*    80 */     2,    4,    6,    3,   10,    7,  126,  170,  169,   90,
 /*    90 */   166,  165,  195,  124,   33,   24,   11,   22,   12,  214,
 /*   100 */    59,  160,  159,  226,  225,    2,    4,    6,    3,   10,
 /*   110 */     7,  123,  193,  195,  155,   33,   16,   11,   27,   12,
 /*   120 */     5,   56,  197,   35,   49,  198,  213,  208,  233,  195,
 /*   130 */   140,   33,  117,   19,  189,   12,   20,   59,  138,  137,
 /*   140 */    41,    5,  143,  216,   35,   49,  198,  213,  119,  167,
 /*   150 */   126,  140,   59,  187,   28,  182,  131,   30,  233,   44,
 /*   160 */    35,   49,  198,  213,   67,  104,   24,  140,  229,  222,
 /*   170 */   194,   82,  195,  216,   33,  157,   19,  163,   12,  167,
 /*   180 */    59,   54,  188,  189,  221,   20,  104,  233,  195,   26,
 /*   190 */    33,  123,   19,  197,   12,  203,   59,  229,  222,   50,
 /*   200 */    79,   59,  216,   35,   49,  198,  213,  121,  167,  187,
 /*   210 */   140,  182,   17,  233,  228,  211,  124,  233,   59,   35,
 /*   220 */    49,  198,  213,  232,  186,   83,  140,  164,  216,  139,
 /*   230 */    54,  195,  216,   33,  167,   19,  124,   12,  167,   59,
 /*   240 */   219,  224,  234,   24,  130,   14,  233,  195,   62,   33,
 /*   250 */   115,   19,  187,   12,  182,   52,  229,  222,  184,   84,
 /*   260 */   190,  216,   35,   49,  198,  213,   36,  167,  206,  140,
 /*   270 */   197,  187,  181,  182,  185,   32,  204,   46,   35,   49,
 /*   280 */   198,  213,  241,  136,   13,  140,  210,  161,   63,  167,
 /*   290 */   143,   51,  209,  124,  233,   44,  196,  172,   15,  148,
 /*   300 */    66,  122,  153,  175,  229,  222,   93,   82,  124,  216,
 /*   310 */     1,  129,  187,  227,  182,  167,   15,  195,   61,  126,
 /*   320 */   221,   19,   15,   12,   93,   59,  126,   23,  219,  224,
 /*   330 */    93,  177,   15,  195,  192,  151,  118,   19,  206,   57,
 /*   340 */    93,   59,   53,  102,  147,  149,   55,   88,   35,   49,
 /*   350 */   198,  213,  118,  106,   37,  140,  220,  214,  158,  167,
 /*   360 */    92,   23,  114,   32,   35,   49,  198,  213,  124,  183,
 /*   370 */   214,  140,  200,   24,  231,  235,  236,  240,  239,  238,
 /*   380 */   237,  230,  233,  233,   44,  125,   80,   99,  202,   70,
 /*   390 */     9,  180,  217,  229,  222,  126,   82,  216,  216,   28,
 /*   400 */   197,   77,   30,  167,  167,  126,  128,  233,   96,  221,
 /*   410 */   110,  126,  201,  220,   91,  158,  113,  229,  222,  184,
 /*   420 */    82,  124,  216,   47,  214,  127,   45,   29,  167,  233,
 /*   430 */    44,   78,  126,  154,  184,   73,  120,  179,  209,  229,
 /*   440 */   222,  209,   82,   38,  216,  124,  194,  126,  233,   43,
 /*   450 */   167,  162,  109,  114,   65,  221,  152,  158,  229,  222,
 /*   460 */    81,   82,  135,  216,  184,  100,  134,  233,   44,  167,
 /*   470 */   158,   48,  199,   75,  221,  209,   25,  229,  222,   95,
 /*   480 */    82,  173,  216,  167,   89,  223,  209,  167,  167,  214,
 /*   490 */   212,  233,   44,  221,  214,  207,  191,   72,   34,   98,
 /*   500 */     8,  229,  222,  194,   82,   59,  216,  220,  103,  233,
 /*   510 */    44,  126,  167,  168,   58,   71,   85,  221,   64,  229,
 /*   520 */   222,   18,   82,  193,  216,   29,   39,  243,  233,   44,
 /*   530 */   167,  243,  243,  243,   74,  221,  243,  243,  229,  222,
 /*   540 */   243,   82,  243,  216,  243,  243,  243,  233,   44,  167,
 /*   550 */   243,  243,  243,   68,  221,  243,  243,  229,  222,  243,
 /*   560 */    82,  243,  216,  243,  243,  233,   44,  243,  167,  243,
 /*   570 */   243,   69,  243,  221,  243,  229,  222,  243,   82,  243,
 /*   580 */   216,  243,  243,  233,   97,  243,  167,  243,  243,  243,
 /*   590 */   243,  221,  243,  229,  222,  243,   82,  243,  216,  243,
 /*   600 */   243,  144,  233,   96,  167,  243,  243,  243,  243,  233,
 /*   610 */    97,  243,  229,  222,  243,   82,  243,  216,  243,  229,
 /*   620 */   222,  243,   82,  167,  216,  243,  243,  142,  233,   40,
 /*   630 */   167,   60,  178,  243,  243,  243,  243,  243,  229,  222,
 /*   640 */   243,   82,  243,  216,  243,  233,   97,  243,  243,  167,
 /*   650 */   243,  243,  243,  243,  243,  229,  222,  243,   82,  243,
 /*   660 */   216,  243,  243,  174,  243,  243,  167,  233,   97,  243,
 /*   670 */   243,  243,  243,  243,  243,  243,  233,  229,  222,  243,
 /*   680 */    82,  243,  216,  233,  111,  145,  229,  222,  167,   76,
 /*   690 */   243,  216,  243,  229,  222,  243,   82,  167,  216,  243,
 /*   700 */   243,  233,  105,  243,  167,  243,  243,  243,  243,  243,
 /*   710 */   243,  229,  222,  243,   82,  243,  216,  233,  101,  243,
 /*   720 */   243,  243,  167,  243,  243,  243,  243,  229,  222,  243,
 /*   730 */    82,  243,  216,  243,  233,  108,  243,  243,  167,  243,
 /*   740 */   243,  233,  112,  243,  229,  222,  243,   82,  243,  216,
 /*   750 */   243,  229,  222,  243,   82,  167,  216,  233,  107,  243,
 /*   760 */   243,  243,  167,  243,  243,  243,  243,  229,  222,  243,
 /*   770 */    82,  243,  216,  243,  243,  233,   87,  243,  167,  243,
 /*   780 */   243,  243,  243,  243,  243,  229,  222,  243,   82,  243,
 /*   790 */   216,  233,   86,  243,  243,  243,  167,  243,  243,  243,
 /*   800 */   243,  229,  222,  243,   82,  243,  216,  243,  233,   94,
 /*   810 */   243,  243,  167,  243,  243,  243,  243,  243,  229,  222,
 /*   820 */   243,   82,  243,  216,  243,  243,  243,  243,  243,  167,
    );
    static public $yy_lookahead = array(
 /*     0 */    16,   40,   41,   25,    3,   68,   69,   70,   71,   30,
 /*    10 */    48,   19,   51,   52,   53,   54,   55,   56,   57,   58,
 /*    20 */    59,   60,   30,    3,   40,   41,   11,   26,   13,   50,
 /*    30 */    15,   30,   17,   18,   19,   51,   52,   53,   54,   55,
 /*    40 */    56,   57,   58,   59,   60,   30,   26,   46,   15,   24,
 /*    50 */    30,   50,   16,    1,    4,    3,   23,   42,   43,   44,
 /*    60 */    45,   25,   29,    4,   49,   40,   41,   31,   32,   33,
 /*    70 */    34,   35,   36,   37,   38,   77,   51,   52,   53,   54,
 /*    80 */    55,   56,   57,   58,   59,   60,   27,   70,   71,   75,
 /*    90 */    40,   41,   11,   25,   13,    3,   15,   29,   17,   85,
 /*   100 */    19,   51,   52,   53,   54,   55,   56,   57,   58,   59,
 /*   110 */    60,   30,   98,   11,   62,   13,   15,   15,   26,   17,
 /*   120 */    39,   19,   30,   42,   43,   44,   45,    4,   73,   11,
 /*   130 */    49,   13,   30,   15,    1,   17,    3,   19,   83,   84,
 /*   140 */    78,   39,   50,   88,   42,   43,   44,   45,   30,   94,
 /*   150 */    27,   49,   19,    1,   17,    3,   82,   20,   73,   74,
 /*   160 */    42,   43,   44,   45,   79,   28,    3,   49,   83,   84,
 /*   170 */    96,   86,   11,   88,   13,   42,   15,   14,   17,   94,
 /*   180 */    19,   48,    4,    1,   99,    3,   28,   73,   11,   26,
 /*   190 */    13,   30,   15,   30,   17,   43,   19,   83,   84,   30,
 /*   200 */    86,   19,   88,   42,   43,   44,   45,   30,   94,    1,
 /*   210 */    49,    3,   21,   73,   97,    4,   25,   73,   19,   42,
 /*   220 */    43,   44,   45,   83,   42,   72,   49,   83,   88,   30,
 /*   230 */    48,   11,   88,   13,   94,   15,   25,   17,   94,   19,
 /*   240 */    12,   13,   18,    3,   30,   21,   73,   11,   16,   13,
 /*   250 */    30,   15,    1,   17,    3,   19,   83,   84,  105,   86,
 /*   260 */     9,   88,   42,   43,   44,   45,   30,   94,   73,   49,
 /*   270 */    30,    1,    4,    3,   66,   47,   49,   81,   42,   43,
 /*   280 */    44,   45,    1,    2,    3,   49,    5,    6,    7,   94,
 /*   290 */    50,   10,   96,   25,   73,   74,    4,   76,   15,  104,
 /*   300 */    79,   80,    4,    4,   83,   84,   23,   86,   25,   88,
 /*   310 */    27,   28,    1,   43,    3,   94,   15,   11,   30,   27,
 /*   320 */    99,   15,   15,   17,   23,   19,   27,   26,   12,   13,
 /*   330 */    23,    4,   15,   11,   18,   18,   30,   15,   73,   19,
 /*   340 */    23,   19,   61,   30,   63,   64,   65,   75,   42,   43,
 /*   350 */    44,   45,   30,   92,   89,   49,   95,   85,   97,   94,
 /*   360 */    75,   26,   77,   47,   42,   43,   44,   45,   25,  104,
 /*   370 */    85,   49,    4,    3,   31,   32,   33,   34,   35,   36,
 /*   380 */    37,   38,   73,   73,   74,   30,   72,   21,    4,   79,
 /*   390 */    24,    4,   83,   83,   84,   27,   86,   88,   88,   17,
 /*   400 */    30,   72,   20,   94,   94,   27,   28,   73,   74,   99,
 /*   410 */    92,   27,   16,   95,   75,   97,   77,   83,   84,  105,
 /*   420 */    86,   25,   88,   81,   85,   30,   81,   22,   94,   73,
 /*   430 */    74,   72,   27,   16,  105,   79,  102,  103,   96,   83,
 /*   440 */    84,   96,   86,   93,   88,   25,   96,   27,   73,   74,
 /*   450 */    94,   90,   92,   77,   79,   99,   48,   97,   83,   84,
 /*   460 */    81,   86,   73,   88,  105,   92,   73,   73,   74,   94,
 /*   470 */    97,   81,   30,   79,   99,   96,  100,   83,   84,   75,
 /*   480 */    86,   16,   88,   94,   75,    8,   96,   94,   94,   85,
 /*   490 */    30,   73,   74,   99,   85,   85,  105,   79,   87,   78,
 /*   500 */   101,   83,   84,   96,   86,   19,   88,   95,   30,   73,
 /*   510 */    74,   27,   94,   76,   19,   79,   93,   99,   90,   83,
 /*   520 */    84,   15,   86,   98,   88,   22,   93,  106,   73,   74,
 /*   530 */    94,  106,  106,  106,   79,   99,  106,  106,   83,   84,
 /*   540 */   106,   86,  106,   88,  106,  106,  106,   73,   74,   94,
 /*   550 */   106,  106,  106,   79,   99,  106,  106,   83,   84,  106,
 /*   560 */    86,  106,   88,  106,  106,   73,   74,  106,   94,  106,
 /*   570 */   106,   79,  106,   99,  106,   83,   84,  106,   86,  106,
 /*   580 */    88,  106,  106,   73,   74,  106,   94,  106,  106,  106,
 /*   590 */   106,   99,  106,   83,   84,  106,   86,  106,   88,  106,
 /*   600 */   106,   91,   73,   74,   94,  106,  106,  106,  106,   73,
 /*   610 */    74,  106,   83,   84,  106,   86,  106,   88,  106,   83,
 /*   620 */    84,  106,   86,   94,   88,  106,  106,   91,   73,   74,
 /*   630 */    94,   76,  103,  106,  106,  106,  106,  106,   83,   84,
 /*   640 */   106,   86,  106,   88,  106,   73,   74,  106,  106,   94,
 /*   650 */   106,  106,  106,  106,  106,   83,   84,  106,   86,  106,
 /*   660 */    88,  106,  106,   91,  106,  106,   94,   73,   74,  106,
 /*   670 */   106,  106,  106,  106,  106,  106,   73,   83,   84,  106,
 /*   680 */    86,  106,   88,   73,   74,   91,   83,   84,   94,   86,
 /*   690 */   106,   88,  106,   83,   84,  106,   86,   94,   88,  106,
 /*   700 */   106,   73,   74,  106,   94,  106,  106,  106,  106,  106,
 /*   710 */   106,   83,   84,  106,   86,  106,   88,   73,   74,  106,
 /*   720 */   106,  106,   94,  106,  106,  106,  106,   83,   84,  106,
 /*   730 */    86,  106,   88,  106,   73,   74,  106,  106,   94,  106,
 /*   740 */   106,   73,   74,  106,   83,   84,  106,   86,  106,   88,
 /*   750 */   106,   83,   84,  106,   86,   94,   88,   73,   74,  106,
 /*   760 */   106,  106,   94,  106,  106,  106,  106,   83,   84,  106,
 /*   770 */    86,  106,   88,  106,  106,   73,   74,  106,   94,  106,
 /*   780 */   106,  106,  106,  106,  106,   83,   84,  106,   86,  106,
 /*   790 */    88,   73,   74,  106,  106,  106,   94,  106,  106,  106,
 /*   800 */   106,   83,   84,  106,   86,  106,   88,  106,   73,   74,
 /*   810 */   106,  106,   94,  106,  106,  106,  106,  106,   83,   84,
 /*   820 */   106,   86,  106,   88,  106,  106,  106,  106,  106,   94,
);
    const YY_SHIFT_USE_DFLT = -40;
    const YY_SHIFT_MAX = 146;
    static public $yy_shift_ofst = array(
 /*     0 */   281,  102,   81,   81,   81,   81,   81,   81,   81,   81,
 /*    10 */    81,   81,  220,  236,  220,  161,  161,  161,  161,  161,
 /*    20 */   161,  161,  161,  161,  161,  161,  161,  161,   15,  118,
 /*    30 */   177,  306,  322,  322,  322,  182,  283,  133,  137,  137,
 /*    40 */   420,  405,  281,   36,  343,    1,   92,  163,  240,  270,
 /*    50 */   378,  311,  370,  311,  199,  311,  370,  370,  370,  370,
 /*    60 */   484,  484,  158,  486,  158,  -16,   50,   25,  -39,  -39,
 /*    70 */   -39,  -39,  -39,  -39,  -39,  -39,  316,  208,  152,  228,
 /*    80 */   251,   20,  228,   52,  228,  382,  396,  268,  299,  368,
 /*    90 */   384,  292,  123,   -8,  211,   59,   68,  191,  503,  495,
 /*   100 */   158,  -22,  506,  101,  478,  -22,  158,  -22,  -22,  158,
 /*   110 */   158,  -22,  -22,  -40,  -40,   33,  317,  301,  307,  307,
 /*   120 */   224,  307,  366,  307,  -21,  327,  395,  335,  355,  288,
 /*   130 */   227,  298,  320,  460,  477,  408,  169,  387,  178,  -38,
 /*   140 */   214,  101,  417,  442,  465,  232,  313,
);
    const YY_REDUCE_USE_DFLT = -64;
    const YY_REDUCE_MAX = 114;
    static public $yy_reduce_ofst = array(
 /*     0 */   -63,  221,  418,  474,  455,  436,  492,  356,  394,   85,
 /*    10 */   310,  375,  334,  555,  529,  510,  536,  572,  594,  718,
 /*    20 */   702,  684,  610,  661,  735,  668,  644,  628,  603,  173,
 /*    30 */   114,   55,  144,  140,  309,  265,  339,  195,  318,  261,
 /*    40 */   285,   14,   17,  376,  376,  350,  350,   74,  350,  359,
 /*    50 */   272,  314,  196,  153,  389,  329,  345,  342,  379,  390,
 /*    60 */   404,  409,  373,  393,  360,  399,  399,  399,  399,  399,
 /*    70 */   399,  399,  399,  399,  399,  399,  411,  391,  391,  411,
 /*    80 */   391,  407,  411,  391,  411,  412,   -2,   -2,  410,  410,
 /*    90 */   410,  410,  410,  428,   -2,  410,   -2,   -2,  425,  437,
 /*   100 */   117,   -2,  433,  423,  361,   -2,  117,   -2,   -2,  117,
 /*   110 */   117,   -2,   -2,   62,  421,
);
    static public $yyExpectedTokens = array(
        /* 0 */ array(1, 2, 3, 5, 6, 7, 10, 61, 63, 64, 65, ),
        /* 1 */ array(11, 13, 15, 17, 19, 30, 39, 42, 43, 44, 45, 49, ),
        /* 2 */ array(11, 13, 15, 17, 19, 30, 39, 42, 43, 44, 45, 49, ),
        /* 3 */ array(11, 13, 15, 17, 19, 30, 39, 42, 43, 44, 45, 49, ),
        /* 4 */ array(11, 13, 15, 17, 19, 30, 39, 42, 43, 44, 45, 49, ),
        /* 5 */ array(11, 13, 15, 17, 19, 30, 39, 42, 43, 44, 45, 49, ),
        /* 6 */ array(11, 13, 15, 17, 19, 30, 39, 42, 43, 44, 45, 49, ),
        /* 7 */ array(11, 13, 15, 17, 19, 30, 39, 42, 43, 44, 45, 49, ),
        /* 8 */ array(11, 13, 15, 17, 19, 30, 39, 42, 43, 44, 45, 49, ),
        /* 9 */ array(11, 13, 15, 17, 19, 30, 39, 42, 43, 44, 45, 49, ),
        /* 10 */ array(11, 13, 15, 17, 19, 30, 39, 42, 43, 44, 45, 49, ),
        /* 11 */ array(11, 13, 15, 17, 19, 30, 39, 42, 43, 44, 45, 49, ),
        /* 12 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 13 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 14 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 15 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 16 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 17 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 18 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 19 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 20 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 21 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 22 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 23 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 24 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 25 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 26 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 27 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 28 */ array(11, 13, 15, 17, 18, 19, 30, 42, 43, 44, 45, 49, ),
        /* 29 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 30 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 31 */ array(11, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 32 */ array(11, 15, 19, 30, 42, 43, 44, 45, 49, ),
        /* 33 */ array(11, 15, 19, 30, 42, 43, 44, 45, 49, ),
        /* 34 */ array(11, 15, 19, 30, 42, 43, 44, 45, 49, ),
        /* 35 */ array(1, 3, 19, 42, 48, ),
        /* 36 */ array(15, 23, 25, 27, 28, ),
        /* 37 */ array(1, 3, 19, 42, 48, ),
        /* 38 */ array(17, 20, 28, ),
        /* 39 */ array(17, 20, 28, ),
        /* 40 */ array(25, 27, ),
        /* 41 */ array(22, 27, ),
        /* 42 */ array(1, 2, 3, 5, 6, 7, 10, 61, 63, 64, 65, ),
        /* 43 */ array(16, 25, 31, 32, 33, 34, 35, 36, 37, 38, ),
        /* 44 */ array(25, 31, 32, 33, 34, 35, 36, 37, 38, ),
        /* 45 */ array(3, 26, 30, 46, 50, ),
        /* 46 */ array(3, 26, 30, 50, ),
        /* 47 */ array(3, 14, 26, 30, ),
        /* 48 */ array(3, 30, 50, ),
        /* 49 */ array(1, 3, 43, ),
        /* 50 */ array(27, 28, ),
        /* 51 */ array(1, 3, ),
        /* 52 */ array(3, 30, ),
        /* 53 */ array(1, 3, ),
        /* 54 */ array(19, 30, ),
        /* 55 */ array(1, 3, ),
        /* 56 */ array(3, 30, ),
        /* 57 */ array(3, 30, ),
        /* 58 */ array(3, 30, ),
        /* 59 */ array(3, 30, ),
        /* 60 */ array(27, ),
        /* 61 */ array(27, ),
        /* 62 */ array(28, ),
        /* 63 */ array(19, ),
        /* 64 */ array(28, ),
        /* 65 */ array(16, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 66 */ array(4, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 67 */ array(24, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 68 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 69 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 70 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 71 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 72 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 73 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 74 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 75 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 76 */ array(12, 13, 18, 47, ),
        /* 77 */ array(1, 3, 66, ),
        /* 78 */ array(1, 3, 43, ),
        /* 79 */ array(12, 13, 47, ),
        /* 80 */ array(1, 3, 9, ),
        /* 81 */ array(3, 26, 30, ),
        /* 82 */ array(12, 13, 47, ),
        /* 83 */ array(1, 3, 62, ),
        /* 84 */ array(12, 13, 47, ),
        /* 85 */ array(17, 20, ),
        /* 86 */ array(16, 25, ),
        /* 87 */ array(4, 25, ),
        /* 88 */ array(4, 27, ),
        /* 89 */ array(4, 27, ),
        /* 90 */ array(4, 27, ),
        /* 91 */ array(4, 27, ),
        /* 92 */ array(4, 27, ),
        /* 93 */ array(19, 30, ),
        /* 94 */ array(4, 25, ),
        /* 95 */ array(4, 27, ),
        /* 96 */ array(25, 29, ),
        /* 97 */ array(21, 25, ),
        /* 98 */ array(22, ),
        /* 99 */ array(19, ),
        /* 100 */ array(28, ),
        /* 101 */ array(25, ),
        /* 102 */ array(15, ),
        /* 103 */ array(15, ),
        /* 104 */ array(30, ),
        /* 105 */ array(25, ),
        /* 106 */ array(28, ),
        /* 107 */ array(25, ),
        /* 108 */ array(25, ),
        /* 109 */ array(28, ),
        /* 110 */ array(28, ),
        /* 111 */ array(25, ),
        /* 112 */ array(25, ),
        /* 113 */ array(),
        /* 114 */ array(),
        /* 115 */ array(15, 23, 29, ),
        /* 116 */ array(15, 18, 23, ),
        /* 117 */ array(15, 23, 26, ),
        /* 118 */ array(15, 23, ),
        /* 119 */ array(15, 23, ),
        /* 120 */ array(18, 21, ),
        /* 121 */ array(15, 23, ),
        /* 122 */ array(21, 24, ),
        /* 123 */ array(15, 23, ),
        /* 124 */ array(30, 50, ),
        /* 125 */ array(4, ),
        /* 126 */ array(30, ),
        /* 127 */ array(26, ),
        /* 128 */ array(30, ),
        /* 129 */ array(30, ),
        /* 130 */ array(49, ),
        /* 131 */ array(4, ),
        /* 132 */ array(19, ),
        /* 133 */ array(30, ),
        /* 134 */ array(8, ),
        /* 135 */ array(48, ),
        /* 136 */ array(30, ),
        /* 137 */ array(4, ),
        /* 138 */ array(4, ),
        /* 139 */ array(48, ),
        /* 140 */ array(30, ),
        /* 141 */ array(15, ),
        /* 142 */ array(16, ),
        /* 143 */ array(30, ),
        /* 144 */ array(16, ),
        /* 145 */ array(16, ),
        /* 146 */ array(30, ),
        /* 147 */ array(),
        /* 148 */ array(),
        /* 149 */ array(),
        /* 150 */ array(),
        /* 151 */ array(),
        /* 152 */ array(),
        /* 153 */ array(),
        /* 154 */ array(),
        /* 155 */ array(),
        /* 156 */ array(),
        /* 157 */ array(),
        /* 158 */ array(),
        /* 159 */ array(),
        /* 160 */ array(),
        /* 161 */ array(),
        /* 162 */ array(),
        /* 163 */ array(),
        /* 164 */ array(),
        /* 165 */ array(),
        /* 166 */ array(),
        /* 167 */ array(),
        /* 168 */ array(),
        /* 169 */ array(),
        /* 170 */ array(),
        /* 171 */ array(),
        /* 172 */ array(),
        /* 173 */ array(),
        /* 174 */ array(),
        /* 175 */ array(),
        /* 176 */ array(),
        /* 177 */ array(),
        /* 178 */ array(),
        /* 179 */ array(),
        /* 180 */ array(),
        /* 181 */ array(),
        /* 182 */ array(),
        /* 183 */ array(),
        /* 184 */ array(),
        /* 185 */ array(),
        /* 186 */ array(),
        /* 187 */ array(),
        /* 188 */ array(),
        /* 189 */ array(),
        /* 190 */ array(),
        /* 191 */ array(),
        /* 192 */ array(),
        /* 193 */ array(),
        /* 194 */ array(),
        /* 195 */ array(),
        /* 196 */ array(),
        /* 197 */ array(),
        /* 198 */ array(),
        /* 199 */ array(),
        /* 200 */ array(),
        /* 201 */ array(),
        /* 202 */ array(),
        /* 203 */ array(),
        /* 204 */ array(),
        /* 205 */ array(),
        /* 206 */ array(),
        /* 207 */ array(),
        /* 208 */ array(),
        /* 209 */ array(),
        /* 210 */ array(),
        /* 211 */ array(),
        /* 212 */ array(),
        /* 213 */ array(),
        /* 214 */ array(),
        /* 215 */ array(),
        /* 216 */ array(),
        /* 217 */ array(),
        /* 218 */ array(),
        /* 219 */ array(),
        /* 220 */ array(),
        /* 221 */ array(),
        /* 222 */ array(),
        /* 223 */ array(),
        /* 224 */ array(),
        /* 225 */ array(),
        /* 226 */ array(),
        /* 227 */ array(),
        /* 228 */ array(),
        /* 229 */ array(),
        /* 230 */ array(),
        /* 231 */ array(),
        /* 232 */ array(),
        /* 233 */ array(),
        /* 234 */ array(),
        /* 235 */ array(),
        /* 236 */ array(),
        /* 237 */ array(),
        /* 238 */ array(),
        /* 239 */ array(),
        /* 240 */ array(),
        /* 241 */ array(),
);
    static public $yy_default = array(
 /*     0 */   377,  377,  377,  377,  377,  377,  377,  377,  377,  377,
 /*    10 */   377,  377,  362,  377,  377,  326,  326,  326,  326,  377,
 /*    20 */   377,  377,  377,  377,  377,  377,  377,  377,  377,  377,
 /*    30 */   377,  377,  377,  377,  377,  377,  270,  377,  303,  301,
 /*    40 */   270,  270,  242,  336,  336,  307,  307,  377,  307,  377,
 /*    50 */   270,  377,  377,  377,  377,  377,  377,  377,  377,  377,
 /*    60 */   270,  270,  297,  377,  296,  377,  377,  377,  344,  343,
 /*    70 */   339,  334,  347,  340,  348,  338,  377,  377,  377,  309,
 /*    80 */   377,  377,  276,  377,  331,  320,  377,  377,  377,  377,
 /*    90 */   377,  377,  377,  377,  377,  377,  363,  325,  277,  377,
 /*   100 */   299,  264,  307,  307,  377,  274,  302,  365,  271,  298,
 /*   110 */   317,  364,  337,  330,  330,  275,  377,  275,  377,  332,
 /*   120 */   377,  308,  377,  275,  377,  377,  377,  377,  377,  377,
 /*   130 */   377,  377,  377,  377,  377,  377,  377,  377,  377,  377,
 /*   140 */   377,  300,  377,  377,  377,  377,  377,  248,  366,  249,
 /*   150 */   312,  310,  370,  263,  323,  247,  335,  294,  318,  346,
 /*   160 */   345,  250,  321,  265,  282,  358,  357,  305,  273,  245,
 /*   170 */   244,  369,  272,  322,  324,  260,  243,  261,  361,  360,
 /*   180 */   267,  371,  376,  367,  374,  251,  295,  375,  266,  372,
 /*   190 */   246,  373,  311,  329,  314,  287,  257,  315,  288,  304,
 /*   200 */   258,  291,  259,  292,  286,  256,  368,  268,  255,  313,
 /*   210 */   253,  316,  327,  289,  269,  328,  290,  281,  262,  284,
 /*   220 */   306,  333,  278,  252,  283,  342,  341,  293,  319,  279,
 /*   230 */   356,  349,  280,  285,  359,  350,  351,  355,  354,  353,
 /*   240 */   352,  254,
);
/* The next thing included is series of defines which control
** various aspects of the generated parser.
**    self::YYNOCODE      is a number which corresponds
**                        to no legal terminal or nonterminal number.  This
**                        number is used to fill in empty slots of the hash 
**                        table.
**    self::YYFALLBACK    If defined, this indicates that one or more tokens
**                        have fall-back values which should be used if the
**                        original value of the token will not parse.
**    self::YYSTACKDEPTH  is the maximum depth of the parser's stack.
**    self::YYNSTATE      the combined number of states.
**    self::YYNRULE       the number of rules in the grammar
**    self::YYERRORSYMBOL is the code number of the error symbol.  If not
**                        defined, then do no error processing.
*/
    const YYNOCODE = 107;
    const YYSTACKDEPTH = 100;
    const YYNSTATE = 242;
    const YYNRULE = 135;
    const YYERRORSYMBOL = 67;
    const YYERRSYMDT = 'yy0';
    const YYFALLBACK = 1;
    /** The next table maps tokens into fallback tokens.  If a construct
     * like the following:
     * 
     *      %fallback ID X Y Z.
     *
     * appears in the grammer, then ID becomes a fallback token for X, Y,
     * and Z.  Whenever one of the tokens X, Y, or Z is input to the parser
     * but it does not parse, the type of the token is changed to ID and
     * the parse is retried before an error is thrown.
     */
    static public $yyFallback = array(
    0,  /*          $ => nothing */
    0,  /*      OTHER => nothing */
    1,  /*  LDELSLASH => OTHER */
    1,  /*       LDEL => OTHER */
    1,  /*       RDEL => OTHER */
    1,  /*        XML => OTHER */
    1,  /*        PHP => OTHER */
    1,  /* SHORTTAGSTART => OTHER */
    1,  /* SHORTTAGEND => OTHER */
    1,  /* COMMENTEND => OTHER */
    1,  /* COMMENTSTART => OTHER */
    1,  /*     NUMBER => OTHER */
    1,  /*       MATH => OTHER */
    1,  /*    UNIMATH => OTHER */
    1,  /*     INCDEC => OTHER */
    1,  /*      OPENP => OTHER */
    1,  /*     CLOSEP => OTHER */
    1,  /*      OPENB => OTHER */
    1,  /*     CLOSEB => OTHER */
    1,  /*     DOLLAR => OTHER */
    1,  /*        DOT => OTHER */
    1,  /*      COMMA => OTHER */
    1,  /*      COLON => OTHER */
    1,  /* DOUBLECOLON => OTHER */
    1,  /*  SEMICOLON => OTHER */
    1,  /*       VERT => OTHER */
    1,  /*      EQUAL => OTHER */
    1,  /*      SPACE => OTHER */
    1,  /*        PTR => OTHER */
    1,  /*       APTR => OTHER */
    1,  /*         ID => OTHER */
    1,  /*     EQUALS => OTHER */
    1,  /*  NOTEQUALS => OTHER */
    1,  /* GREATERTHAN => OTHER */
    1,  /*   LESSTHAN => OTHER */
    1,  /* GREATEREQUAL => OTHER */
    1,  /*  LESSEQUAL => OTHER */
    1,  /*   IDENTITY => OTHER */
    1,  /* NONEIDENTITY => OTHER */
    1,  /*        NOT => OTHER */
    1,  /*       LAND => OTHER */
    1,  /*        LOR => OTHER */
    1,  /*      QUOTE => OTHER */
    1,  /* SINGLEQUOTE => OTHER */
    1,  /*    BOOLEAN => OTHER */
    1,  /*       NULL => OTHER */
    1,  /*         IN => OTHER */
    1,  /*     ANDSYM => OTHER */
    1,  /*   BACKTICK => OTHER */
    1,  /*      HATCH => OTHER */
    1,  /*         AT => OTHER */
    1,  /*      ISODD => OTHER */
    1,  /*   ISNOTODD => OTHER */
    1,  /*     ISEVEN => OTHER */
    1,  /*  ISNOTEVEN => OTHER */
    1,  /*    ISODDBY => OTHER */
    1,  /* ISNOTODDBY => OTHER */
    1,  /*   ISEVENBY => OTHER */
    1,  /* ISNOTEVENBY => OTHER */
    1,  /*    ISDIVBY => OTHER */
    1,  /* ISNOTDIVBY => OTHER */
    0,  /* LITERALSTART => nothing */
    0,  /* LITERALEND => nothing */
    0,  /*  LDELIMTAG => nothing */
    0,  /*  RDELIMTAG => nothing */
    0,  /*   PHPSTART => nothing */
    0,  /*     PHPEND => nothing */
    );
    /**
     * Turn parser tracing on by giving a stream to which to write the trace
     * and a prompt to preface each trace message.  Tracing is turned off
     * by making either argument NULL 
     *
     * Inputs:
     * 
     * - A stream resource to which trace output should be written.
     *   If NULL, then tracing is turned off.
     * - A prefix string written at the beginning of every
     *   line of trace output.  If NULL, then tracing is
     *   turned off.
     *
     * Outputs:
     * 
     * - None.
     * @param resource
     * @param string
     */
    static function Trace($TraceFILE, $zTracePrompt)
    {
        if (!$TraceFILE) {
            $zTracePrompt = 0;
        } elseif (!$zTracePrompt) {
            $TraceFILE = 0;
        }
        self::$yyTraceFILE = $TraceFILE;
        self::$yyTracePrompt = $zTracePrompt;
    }

    /**
     * Output debug information to output (php://output stream)
     */
    static function PrintTrace()
    {
        self::$yyTraceFILE = fopen('php://output', 'w');
        self::$yyTracePrompt = '<br>';
    }

    /**
     * @var resource|0
     */
    static public $yyTraceFILE;
    /**
     * String to prepend to debug output
     * @var string|0
     */
    static public $yyTracePrompt;
    /**
     * @var int
     */
    public $yyidx;                    /* Index of top element in stack */
    /**
     * @var int
     */
    public $yyerrcnt;                 /* Shifts left before out of the error */
    /**
     * @var array
     */
    public $yystack = array();  /* The parser's stack */

    /**
     * For tracing shifts, the names of all terminals and nonterminals
     * are required.  The following table supplies these names
     * @var array
     */
    public $yyTokenName = array( 
  '$',             'OTHER',         'LDELSLASH',     'LDEL',        
  'RDEL',          'XML',           'PHP',           'SHORTTAGSTART',
  'SHORTTAGEND',   'COMMENTEND',    'COMMENTSTART',  'NUMBER',      
  'MATH',          'UNIMATH',       'INCDEC',        'OPENP',       
  'CLOSEP',        'OPENB',         'CLOSEB',        'DOLLAR',      
  'DOT',           'COMMA',         'COLON',         'DOUBLECOLON', 
  'SEMICOLON',     'VERT',          'EQUAL',         'SPACE',       
  'PTR',           'APTR',          'ID',            'EQUALS',      
  'NOTEQUALS',     'GREATERTHAN',   'LESSTHAN',      'GREATEREQUAL',
  'LESSEQUAL',     'IDENTITY',      'NONEIDENTITY',  'NOT',         
  'LAND',          'LOR',           'QUOTE',         'SINGLEQUOTE', 
  'BOOLEAN',       'NULL',          'IN',            'ANDSYM',      
  'BACKTICK',      'HATCH',         'AT',            'ISODD',       
  'ISNOTODD',      'ISEVEN',        'ISNOTEVEN',     'ISODDBY',     
  'ISNOTODDBY',    'ISEVENBY',      'ISNOTEVENBY',   'ISDIVBY',     
  'ISNOTDIVBY',    'LITERALSTART',  'LITERALEND',    'LDELIMTAG',   
  'RDELIMTAG',     'PHPSTART',      'PHPEND',        'error',       
  'start',         'template',      'template_element',  'smartytag',   
  'text',          'variable',      'expr',          'attributes',  
  'statement',     'modifier',      'modparameters',  'ifexprs',     
  'statements',    'varvar',        'foraction',     'value',       
  'array',         'attribute',     'exprs',         'math',        
  'function',      'doublequoted',  'method',        'params',      
  'objectchain',   'arrayindex',    'object',        'indexdef',    
  'varvarele',     'objectelement',  'modparameter',  'ifexpr',      
  'ifcond',        'lop',           'arrayelements',  'arrayelement',
  'doublequotedcontent',  'textelement', 
    );

    /**
     * For tracing reduce actions, the names of all rules are required.
     * @var array
     */
    static public $yyRuleName = array(
 /*   0 */ "start ::= template",
 /*   1 */ "template ::= template_element",
 /*   2 */ "template ::= template template_element",
 /*   3 */ "template_element ::= smartytag",
 /*   4 */ "template_element ::= COMMENTSTART text COMMENTEND",
 /*   5 */ "template_element ::= LITERALSTART text LITERALEND",
 /*   6 */ "template_element ::= LDELIMTAG",
 /*   7 */ "template_element ::= RDELIMTAG",
 /*   8 */ "template_element ::= PHP",
 /*   9 */ "template_element ::= PHPSTART text PHPEND",
 /*  10 */ "template_element ::= SHORTTAGSTART variable SHORTTAGEND",
 /*  11 */ "template_element ::= XML",
 /*  12 */ "template_element ::= OTHER",
 /*  13 */ "smartytag ::= LDEL expr attributes RDEL",
 /*  14 */ "smartytag ::= LDEL statement attributes RDEL",
 /*  15 */ "smartytag ::= LDEL ID attributes RDEL",
 /*  16 */ "smartytag ::= LDEL ID PTR ID attributes RDEL",
 /*  17 */ "smartytag ::= LDEL ID modifier modparameters attributes RDEL",
 /*  18 */ "smartytag ::= LDELSLASH ID attributes RDEL",
 /*  19 */ "smartytag ::= LDELSLASH ID PTR ID RDEL",
 /*  20 */ "smartytag ::= LDEL ID SPACE ifexprs RDEL",
 /*  21 */ "smartytag ::= LDEL ID SPACE statements SEMICOLON ifexprs SEMICOLON DOLLAR varvar foraction RDEL",
 /*  22 */ "foraction ::= EQUAL expr",
 /*  23 */ "foraction ::= INCDEC",
 /*  24 */ "smartytag ::= LDEL ID SPACE DOLLAR varvar IN value RDEL",
 /*  25 */ "smartytag ::= LDEL ID SPACE DOLLAR varvar IN array RDEL",
 /*  26 */ "attributes ::= attributes attribute",
 /*  27 */ "attributes ::= attribute",
 /*  28 */ "attributes ::=",
 /*  29 */ "attribute ::= SPACE ID EQUAL expr",
 /*  30 */ "statements ::= statement",
 /*  31 */ "statements ::= statements COMMA statement",
 /*  32 */ "statement ::= DOLLAR varvar EQUAL expr",
 /*  33 */ "expr ::= ID",
 /*  34 */ "expr ::= exprs",
 /*  35 */ "expr ::= expr modifier modparameters",
 /*  36 */ "exprs ::= array",
 /*  37 */ "exprs ::= value",
 /*  38 */ "exprs ::= UNIMATH value",
 /*  39 */ "exprs ::= exprs math value",
 /*  40 */ "exprs ::= exprs ANDSYM value",
 /*  41 */ "math ::= UNIMATH",
 /*  42 */ "math ::= MATH",
 /*  43 */ "value ::= variable",
 /*  44 */ "value ::= HATCH ID HATCH",
 /*  45 */ "value ::= NUMBER",
 /*  46 */ "value ::= BOOLEAN",
 /*  47 */ "value ::= NULL",
 /*  48 */ "value ::= function",
 /*  49 */ "value ::= OPENP expr CLOSEP",
 /*  50 */ "value ::= SINGLEQUOTE text SINGLEQUOTE",
 /*  51 */ "value ::= SINGLEQUOTE SINGLEQUOTE",
 /*  52 */ "value ::= QUOTE doublequoted QUOTE",
 /*  53 */ "value ::= QUOTE QUOTE",
 /*  54 */ "value ::= ID DOUBLECOLON method",
 /*  55 */ "value ::= ID DOUBLECOLON DOLLAR ID OPENP params CLOSEP",
 /*  56 */ "value ::= ID DOUBLECOLON method objectchain",
 /*  57 */ "value ::= ID DOUBLECOLON DOLLAR ID OPENP params CLOSEP objectchain",
 /*  58 */ "value ::= ID DOUBLECOLON ID",
 /*  59 */ "value ::= ID DOUBLECOLON DOLLAR ID arrayindex",
 /*  60 */ "value ::= ID DOUBLECOLON DOLLAR ID arrayindex objectchain",
 /*  61 */ "variable ::= DOLLAR varvar arrayindex",
 /*  62 */ "variable ::= DOLLAR varvar AT ID",
 /*  63 */ "variable ::= object",
 /*  64 */ "arrayindex ::= arrayindex indexdef",
 /*  65 */ "arrayindex ::=",
 /*  66 */ "indexdef ::= DOT ID",
 /*  67 */ "indexdef ::= DOT exprs",
 /*  68 */ "indexdef ::= OPENB ID CLOSEB",
 /*  69 */ "indexdef ::= OPENB exprs CLOSEB",
 /*  70 */ "indexdef ::= OPENB CLOSEB",
 /*  71 */ "varvar ::= varvarele",
 /*  72 */ "varvar ::= varvar varvarele",
 /*  73 */ "varvarele ::= ID",
 /*  74 */ "varvarele ::= LDEL expr RDEL",
 /*  75 */ "object ::= DOLLAR varvar arrayindex objectchain",
 /*  76 */ "objectchain ::= objectelement",
 /*  77 */ "objectchain ::= objectchain objectelement",
 /*  78 */ "objectelement ::= PTR ID arrayindex",
 /*  79 */ "objectelement ::= PTR method",
 /*  80 */ "function ::= ID OPENP params CLOSEP",
 /*  81 */ "method ::= ID OPENP params CLOSEP",
 /*  82 */ "params ::= expr COMMA params",
 /*  83 */ "params ::= expr",
 /*  84 */ "params ::=",
 /*  85 */ "modifier ::= VERT AT ID",
 /*  86 */ "modifier ::= VERT ID",
 /*  87 */ "modparameters ::= modparameters modparameter",
 /*  88 */ "modparameters ::=",
 /*  89 */ "modparameter ::= COLON exprs",
 /*  90 */ "modparameter ::= COLON ID",
 /*  91 */ "ifexprs ::= ifexpr",
 /*  92 */ "ifexprs ::= NOT ifexprs",
 /*  93 */ "ifexprs ::= OPENP ifexprs CLOSEP",
 /*  94 */ "ifexpr ::= expr",
 /*  95 */ "ifexpr ::= expr ifcond expr",
 /*  96 */ "ifexpr ::= ifexprs lop ifexprs",
 /*  97 */ "ifexpr ::= ifexprs ISDIVBY ifexprs",
 /*  98 */ "ifexpr ::= ifexprs ISNOTDIVBY ifexprs",
 /*  99 */ "ifexpr ::= ifexprs ISEVEN",
 /* 100 */ "ifexpr ::= ifexprs ISNOTEVEN",
 /* 101 */ "ifexpr ::= ifexprs ISEVENBY ifexprs",
 /* 102 */ "ifexpr ::= ifexprs ISNOTEVENBY ifexprs",
 /* 103 */ "ifexpr ::= ifexprs ISODD",
 /* 104 */ "ifexpr ::= ifexprs ISNOTODD",
 /* 105 */ "ifexpr ::= ifexprs ISODDBY ifexprs",
 /* 106 */ "ifexpr ::= ifexprs ISNOTODDBY ifexprs",
 /* 107 */ "ifcond ::= EQUALS",
 /* 108 */ "ifcond ::= NOTEQUALS",
 /* 109 */ "ifcond ::= GREATERTHAN",
 /* 110 */ "ifcond ::= LESSTHAN",
 /* 111 */ "ifcond ::= GREATEREQUAL",
 /* 112 */ "ifcond ::= LESSEQUAL",
 /* 113 */ "ifcond ::= IDENTITY",
 /* 114 */ "ifcond ::= NONEIDENTITY",
 /* 115 */ "lop ::= LAND",
 /* 116 */ "lop ::= LOR",
 /* 117 */ "array ::= OPENB arrayelements CLOSEB",
 /* 118 */ "arrayelements ::= arrayelement",
 /* 119 */ "arrayelements ::= arrayelements COMMA arrayelement",
 /* 120 */ "arrayelements ::=",
 /* 121 */ "arrayelement ::= expr",
 /* 122 */ "arrayelement ::= expr APTR expr",
 /* 123 */ "arrayelement ::= ID APTR expr",
 /* 124 */ "doublequoted ::= doublequoted doublequotedcontent",
 /* 125 */ "doublequoted ::= doublequotedcontent",
 /* 126 */ "doublequotedcontent ::= variable",
 /* 127 */ "doublequotedcontent ::= BACKTICK ID BACKTICK",
 /* 128 */ "doublequotedcontent ::= BACKTICK variable BACKTICK",
 /* 129 */ "doublequotedcontent ::= LDEL expr RDEL",
 /* 130 */ "doublequotedcontent ::= OTHER",
 /* 131 */ "text ::= text textelement",
 /* 132 */ "text ::= textelement",
 /* 133 */ "textelement ::= OTHER",
 /* 134 */ "textelement ::= LDEL",
    );

    /**
     * This function returns the symbolic name associated with a token
     * value.
     * @param int
     * @return string
     */
    function tokenName($tokenType)
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

    /**
     * The following function deletes the value associated with a
     * symbol.  The symbol can be either a terminal or nonterminal.
     * @param int the symbol code
     * @param mixed the symbol's value
     */
    static function yy_destructor($yymajor, $yypminor)
    {
        switch ($yymajor) {
        /* Here is inserted the actions which take place when a
        ** terminal or non-terminal is destroyed.  This can happen
        ** when the symbol is popped from the stack during a
        ** reduce or during error processing or when a parser is 
        ** being destroyed before it is finished parsing.
        **
        ** Note: during a reduce, the only symbols destroyed are those
        ** which appear on the RHS of the rule, but which are not used
        ** inside the C code.
        */
            default:  break;   /* If no destructor action specified: do nothing */
        }
    }

    /**
     * Pop the parser's stack once.
     *
     * If there is a destructor routine associated with the token which
     * is popped from the stack, then call it.
     *
     * Return the major token number for the symbol popped.
     * @param TP_yyParser
     * @return int
     */
    function yy_pop_parser_stack()
    {
        if (!count($this->yystack)) {
            return;
        }
        $yytos = array_pop($this->yystack);
        if (self::$yyTraceFILE && $this->yyidx >= 0) {
            fwrite(self::$yyTraceFILE,
                self::$yyTracePrompt . 'Popping ' . $this->yyTokenName[$yytos->major] .
                    "\n");
        }
        $yymajor = $yytos->major;
        self::yy_destructor($yymajor, $yytos->minor);
        $this->yyidx--;
        return $yymajor;
    }

    /**
     * Deallocate and destroy a parser.  Destructors are all called for
     * all stack elements before shutting the parser down.
     */
    function __destruct()
    {
        while ($this->yyidx >= 0) {
            $this->yy_pop_parser_stack();
        }
        if (is_resource(self::$yyTraceFILE)) {
            fclose(self::$yyTraceFILE);
        }
    }

    /**
     * Based on the current state and parser stack, get a list of all
     * possible lookahead tokens
     * @param int
     * @return array
     */
    function yy_get_expected_tokens($token)
    {
        $state = $this->yystack[$this->yyidx]->stateno;
        $expected = self::$yyExpectedTokens[$state];
        if (in_array($token, self::$yyExpectedTokens[$state], true)) {
            return $expected;
        }
        $stack = $this->yystack;
        $yyidx = $this->yyidx;
        do {
            $yyact = $this->yy_find_shift_action($token);
            if ($yyact >= self::YYNSTATE && $yyact < self::YYNSTATE + self::YYNRULE) {
                // reduce action
                $done = 0;
                do {
                    if ($done++ == 100) {
                        $this->yyidx = $yyidx;
                        $this->yystack = $stack;
                        // too much recursion prevents proper detection
                        // so give up
                        return array_unique($expected);
                    }
                    $yyruleno = $yyact - self::YYNSTATE;
                    $this->yyidx -= self::$yyRuleInfo[$yyruleno]['rhs'];
                    $nextstate = $this->yy_find_reduce_action(
                        $this->yystack[$this->yyidx]->stateno,
                        self::$yyRuleInfo[$yyruleno]['lhs']);
                    if (isset(self::$yyExpectedTokens[$nextstate])) {
                        $expected += self::$yyExpectedTokens[$nextstate];
                            if (in_array($token,
                                  self::$yyExpectedTokens[$nextstate], true)) {
                            $this->yyidx = $yyidx;
                            $this->yystack = $stack;
                            return array_unique($expected);
                        }
                    }
                    if ($nextstate < self::YYNSTATE) {
                        // we need to shift a non-terminal
                        $this->yyidx++;
                        $x = new TP_yyStackEntry;
                        $x->stateno = $nextstate;
                        $x->major = self::$yyRuleInfo[$yyruleno]['lhs'];
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
        return array_unique($expected);
    }

    /**
     * Based on the parser state and current parser stack, determine whether
     * the lookahead token is possible.
     * 
     * The parser will convert the token value to an error token if not.  This
     * catches some unusual edge cases where the parser would fail.
     * @param int
     * @return bool
     */
    function yy_is_expected_token($token)
    {
        if ($token === 0) {
            return true; // 0 is not part of this
        }
        $state = $this->yystack[$this->yyidx]->stateno;
        if (in_array($token, self::$yyExpectedTokens[$state], true)) {
            return true;
        }
        $stack = $this->yystack;
        $yyidx = $this->yyidx;
        do {
            $yyact = $this->yy_find_shift_action($token);
            if ($yyact >= self::YYNSTATE && $yyact < self::YYNSTATE + self::YYNRULE) {
                // reduce action
                $done = 0;
                do {
                    if ($done++ == 100) {
                        $this->yyidx = $yyidx;
                        $this->yystack = $stack;
                        // too much recursion prevents proper detection
                        // so give up
                        return true;
                    }
                    $yyruleno = $yyact - self::YYNSTATE;
                    $this->yyidx -= self::$yyRuleInfo[$yyruleno]['rhs'];
                    $nextstate = $this->yy_find_reduce_action(
                        $this->yystack[$this->yyidx]->stateno,
                        self::$yyRuleInfo[$yyruleno]['lhs']);
                    if (isset(self::$yyExpectedTokens[$nextstate]) &&
                          in_array($token, self::$yyExpectedTokens[$nextstate], true)) {
                        $this->yyidx = $yyidx;
                        $this->yystack = $stack;
                        return true;
                    }
                    if ($nextstate < self::YYNSTATE) {
                        // we need to shift a non-terminal
                        $this->yyidx++;
                        $x = new TP_yyStackEntry;
                        $x->stateno = $nextstate;
                        $x->major = self::$yyRuleInfo[$yyruleno]['lhs'];
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

    /**
     * Find the appropriate action for a parser given the terminal
     * look-ahead token iLookAhead.
     *
     * If the look-ahead token is YYNOCODE, then check to see if the action is
     * independent of the look-ahead.  If it is, return the action, otherwise
     * return YY_NO_ACTION.
     * @param int The look-ahead token
     */
    function yy_find_shift_action($iLookAhead)
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
        if ($i < 0 || $i >= self::YY_SZ_ACTTAB ||
              self::$yy_lookahead[$i] != $iLookAhead) {
            if (count(self::$yyFallback) && $iLookAhead < count(self::$yyFallback)
                   && ($iFallback = self::$yyFallback[$iLookAhead]) != 0) {
                if (self::$yyTraceFILE) {
                    fwrite(self::$yyTraceFILE, self::$yyTracePrompt . "FALLBACK " .
                        $this->yyTokenName[$iLookAhead] . " => " .
                        $this->yyTokenName[$iFallback] . "\n");
                }
                return $this->yy_find_shift_action($iFallback);
            }
            return self::$yy_default[$stateno];
        } else {
            return self::$yy_action[$i];
        }
    }

    /**
     * Find the appropriate action for a parser given the non-terminal
     * look-ahead token $iLookAhead.
     *
     * If the look-ahead token is self::YYNOCODE, then check to see if the action is
     * independent of the look-ahead.  If it is, return the action, otherwise
     * return self::YY_NO_ACTION.
     * @param int Current state number
     * @param int The look-ahead token
     */
    function yy_find_reduce_action($stateno, $iLookAhead)
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
        if ($i < 0 || $i >= self::YY_SZ_ACTTAB ||
              self::$yy_lookahead[$i] != $iLookAhead) {
            return self::$yy_default[$stateno];
        } else {
            return self::$yy_action[$i];
        }
    }

    /**
     * Perform a shift action.
     * @param int The new state to shift in
     * @param int The major token to shift in
     * @param mixed the minor token to shift in
     */
    function yy_shift($yyNewState, $yyMajor, $yypMinor)
    {
        $this->yyidx++;
        if ($this->yyidx >= self::YYSTACKDEPTH) {
            $this->yyidx--;
            if (self::$yyTraceFILE) {
                fprintf(self::$yyTraceFILE, "%sStack Overflow!\n", self::$yyTracePrompt);
            }
            while ($this->yyidx >= 0) {
                $this->yy_pop_parser_stack();
            }
            /* Here code is inserted which will execute if the parser
            ** stack ever overflows */
            return;
        }
        $yytos = new TP_yyStackEntry;
        $yytos->stateno = $yyNewState;
        $yytos->major = $yyMajor;
        $yytos->minor = $yypMinor;
        array_push($this->yystack, $yytos);
        if (self::$yyTraceFILE && $this->yyidx > 0) {
            fprintf(self::$yyTraceFILE, "%sShift %d\n", self::$yyTracePrompt,
                $yyNewState);
            fprintf(self::$yyTraceFILE, "%sStack:", self::$yyTracePrompt);
            for($i = 1; $i <= $this->yyidx; $i++) {
                fprintf(self::$yyTraceFILE, " %s",
                    $this->yyTokenName[$this->yystack[$i]->major]);
            }
            fwrite(self::$yyTraceFILE,"\n");
        }
    }

    /**
     * The following table contains information about every rule that
     * is used during the reduce.
     *
     * <pre>
     * array(
     *  array(
     *   int $lhs;         Symbol on the left-hand side of the rule
     *   int $nrhs;     Number of right-hand side symbols in the rule
     *  ),...
     * );
     * </pre>
     */
    static public $yyRuleInfo = array(
  array( 'lhs' => 68, 'rhs' => 1 ),
  array( 'lhs' => 69, 'rhs' => 1 ),
  array( 'lhs' => 69, 'rhs' => 2 ),
  array( 'lhs' => 70, 'rhs' => 1 ),
  array( 'lhs' => 70, 'rhs' => 3 ),
  array( 'lhs' => 70, 'rhs' => 3 ),
  array( 'lhs' => 70, 'rhs' => 1 ),
  array( 'lhs' => 70, 'rhs' => 1 ),
  array( 'lhs' => 70, 'rhs' => 1 ),
  array( 'lhs' => 70, 'rhs' => 3 ),
  array( 'lhs' => 70, 'rhs' => 3 ),
  array( 'lhs' => 70, 'rhs' => 1 ),
  array( 'lhs' => 70, 'rhs' => 1 ),
  array( 'lhs' => 71, 'rhs' => 4 ),
  array( 'lhs' => 71, 'rhs' => 4 ),
  array( 'lhs' => 71, 'rhs' => 4 ),
  array( 'lhs' => 71, 'rhs' => 6 ),
  array( 'lhs' => 71, 'rhs' => 6 ),
  array( 'lhs' => 71, 'rhs' => 4 ),
  array( 'lhs' => 71, 'rhs' => 5 ),
  array( 'lhs' => 71, 'rhs' => 5 ),
  array( 'lhs' => 71, 'rhs' => 11 ),
  array( 'lhs' => 82, 'rhs' => 2 ),
  array( 'lhs' => 82, 'rhs' => 1 ),
  array( 'lhs' => 71, 'rhs' => 8 ),
  array( 'lhs' => 71, 'rhs' => 8 ),
  array( 'lhs' => 75, 'rhs' => 2 ),
  array( 'lhs' => 75, 'rhs' => 1 ),
  array( 'lhs' => 75, 'rhs' => 0 ),
  array( 'lhs' => 85, 'rhs' => 4 ),
  array( 'lhs' => 80, 'rhs' => 1 ),
  array( 'lhs' => 80, 'rhs' => 3 ),
  array( 'lhs' => 76, 'rhs' => 4 ),
  array( 'lhs' => 74, 'rhs' => 1 ),
  array( 'lhs' => 74, 'rhs' => 1 ),
  array( 'lhs' => 74, 'rhs' => 3 ),
  array( 'lhs' => 86, 'rhs' => 1 ),
  array( 'lhs' => 86, 'rhs' => 1 ),
  array( 'lhs' => 86, 'rhs' => 2 ),
  array( 'lhs' => 86, 'rhs' => 3 ),
  array( 'lhs' => 86, 'rhs' => 3 ),
  array( 'lhs' => 87, 'rhs' => 1 ),
  array( 'lhs' => 87, 'rhs' => 1 ),
  array( 'lhs' => 83, 'rhs' => 1 ),
  array( 'lhs' => 83, 'rhs' => 3 ),
  array( 'lhs' => 83, 'rhs' => 1 ),
  array( 'lhs' => 83, 'rhs' => 1 ),
  array( 'lhs' => 83, 'rhs' => 1 ),
  array( 'lhs' => 83, 'rhs' => 1 ),
  array( 'lhs' => 83, 'rhs' => 3 ),
  array( 'lhs' => 83, 'rhs' => 3 ),
  array( 'lhs' => 83, 'rhs' => 2 ),
  array( 'lhs' => 83, 'rhs' => 3 ),
  array( 'lhs' => 83, 'rhs' => 2 ),
  array( 'lhs' => 83, 'rhs' => 3 ),
  array( 'lhs' => 83, 'rhs' => 7 ),
  array( 'lhs' => 83, 'rhs' => 4 ),
  array( 'lhs' => 83, 'rhs' => 8 ),
  array( 'lhs' => 83, 'rhs' => 3 ),
  array( 'lhs' => 83, 'rhs' => 5 ),
  array( 'lhs' => 83, 'rhs' => 6 ),
  array( 'lhs' => 73, 'rhs' => 3 ),
  array( 'lhs' => 73, 'rhs' => 4 ),
  array( 'lhs' => 73, 'rhs' => 1 ),
  array( 'lhs' => 93, 'rhs' => 2 ),
  array( 'lhs' => 93, 'rhs' => 0 ),
  array( 'lhs' => 95, 'rhs' => 2 ),
  array( 'lhs' => 95, 'rhs' => 2 ),
  array( 'lhs' => 95, 'rhs' => 3 ),
  array( 'lhs' => 95, 'rhs' => 3 ),
  array( 'lhs' => 95, 'rhs' => 2 ),
  array( 'lhs' => 81, 'rhs' => 1 ),
  array( 'lhs' => 81, 'rhs' => 2 ),
  array( 'lhs' => 96, 'rhs' => 1 ),
  array( 'lhs' => 96, 'rhs' => 3 ),
  array( 'lhs' => 94, 'rhs' => 4 ),
  array( 'lhs' => 92, 'rhs' => 1 ),
  array( 'lhs' => 92, 'rhs' => 2 ),
  array( 'lhs' => 97, 'rhs' => 3 ),
  array( 'lhs' => 97, 'rhs' => 2 ),
  array( 'lhs' => 88, 'rhs' => 4 ),
  array( 'lhs' => 90, 'rhs' => 4 ),
  array( 'lhs' => 91, 'rhs' => 3 ),
  array( 'lhs' => 91, 'rhs' => 1 ),
  array( 'lhs' => 91, 'rhs' => 0 ),
  array( 'lhs' => 77, 'rhs' => 3 ),
  array( 'lhs' => 77, 'rhs' => 2 ),
  array( 'lhs' => 78, 'rhs' => 2 ),
  array( 'lhs' => 78, 'rhs' => 0 ),
  array( 'lhs' => 98, 'rhs' => 2 ),
  array( 'lhs' => 98, 'rhs' => 2 ),
  array( 'lhs' => 79, 'rhs' => 1 ),
  array( 'lhs' => 79, 'rhs' => 2 ),
  array( 'lhs' => 79, 'rhs' => 3 ),
  array( 'lhs' => 99, 'rhs' => 1 ),
  array( 'lhs' => 99, 'rhs' => 3 ),
  array( 'lhs' => 99, 'rhs' => 3 ),
  array( 'lhs' => 99, 'rhs' => 3 ),
  array( 'lhs' => 99, 'rhs' => 3 ),
  array( 'lhs' => 99, 'rhs' => 2 ),
  array( 'lhs' => 99, 'rhs' => 2 ),
  array( 'lhs' => 99, 'rhs' => 3 ),
  array( 'lhs' => 99, 'rhs' => 3 ),
  array( 'lhs' => 99, 'rhs' => 2 ),
  array( 'lhs' => 99, 'rhs' => 2 ),
  array( 'lhs' => 99, 'rhs' => 3 ),
  array( 'lhs' => 99, 'rhs' => 3 ),
  array( 'lhs' => 100, 'rhs' => 1 ),
  array( 'lhs' => 100, 'rhs' => 1 ),
  array( 'lhs' => 100, 'rhs' => 1 ),
  array( 'lhs' => 100, 'rhs' => 1 ),
  array( 'lhs' => 100, 'rhs' => 1 ),
  array( 'lhs' => 100, 'rhs' => 1 ),
  array( 'lhs' => 100, 'rhs' => 1 ),
  array( 'lhs' => 100, 'rhs' => 1 ),
  array( 'lhs' => 101, 'rhs' => 1 ),
  array( 'lhs' => 101, 'rhs' => 1 ),
  array( 'lhs' => 84, 'rhs' => 3 ),
  array( 'lhs' => 102, 'rhs' => 1 ),
  array( 'lhs' => 102, 'rhs' => 3 ),
  array( 'lhs' => 102, 'rhs' => 0 ),
  array( 'lhs' => 103, 'rhs' => 1 ),
  array( 'lhs' => 103, 'rhs' => 3 ),
  array( 'lhs' => 103, 'rhs' => 3 ),
  array( 'lhs' => 89, 'rhs' => 2 ),
  array( 'lhs' => 89, 'rhs' => 1 ),
  array( 'lhs' => 104, 'rhs' => 1 ),
  array( 'lhs' => 104, 'rhs' => 3 ),
  array( 'lhs' => 104, 'rhs' => 3 ),
  array( 'lhs' => 104, 'rhs' => 3 ),
  array( 'lhs' => 104, 'rhs' => 1 ),
  array( 'lhs' => 72, 'rhs' => 2 ),
  array( 'lhs' => 72, 'rhs' => 1 ),
  array( 'lhs' => 105, 'rhs' => 1 ),
  array( 'lhs' => 105, 'rhs' => 1 ),
    );

    /**
     * The following table contains a mapping of reduce action to method name
     * that handles the reduction.
     * 
     * If a rule is not set, it has no handler.
     */
    static public $yyReduceMap = array(
        0 => 0,
        37 => 0,
        43 => 0,
        45 => 0,
        46 => 0,
        47 => 0,
        48 => 0,
        63 => 0,
        118 => 0,
        1 => 1,
        34 => 1,
        36 => 1,
        41 => 1,
        42 => 1,
        71 => 1,
        91 => 1,
        125 => 1,
        132 => 1,
        133 => 1,
        134 => 1,
        2 => 2,
        64 => 2,
        124 => 2,
        131 => 2,
        3 => 3,
        4 => 4,
        5 => 5,
        6 => 6,
        7 => 7,
        8 => 8,
        9 => 9,
        10 => 10,
        11 => 11,
        12 => 12,
        13 => 13,
        14 => 14,
        15 => 15,
        16 => 16,
        17 => 17,
        18 => 18,
        19 => 19,
        20 => 20,
        21 => 21,
        22 => 22,
        23 => 23,
        27 => 23,
        83 => 23,
        121 => 23,
        24 => 24,
        25 => 24,
        26 => 26,
        28 => 28,
        29 => 29,
        30 => 30,
        31 => 31,
        32 => 32,
        33 => 33,
        35 => 35,
        38 => 38,
        39 => 39,
        40 => 40,
        44 => 44,
        49 => 49,
        50 => 50,
        51 => 51,
        53 => 51,
        52 => 52,
        54 => 54,
        55 => 55,
        56 => 56,
        57 => 57,
        58 => 58,
        59 => 59,
        60 => 60,
        61 => 61,
        62 => 62,
        65 => 65,
        88 => 65,
        66 => 66,
        67 => 67,
        68 => 68,
        69 => 69,
        70 => 70,
        72 => 72,
        73 => 73,
        74 => 74,
        93 => 74,
        75 => 75,
        76 => 76,
        77 => 77,
        78 => 78,
        79 => 79,
        80 => 80,
        81 => 81,
        82 => 82,
        84 => 84,
        85 => 85,
        86 => 86,
        87 => 87,
        89 => 89,
        90 => 90,
        92 => 92,
        94 => 94,
        95 => 95,
        96 => 95,
        97 => 97,
        98 => 98,
        99 => 99,
        104 => 99,
        100 => 100,
        103 => 100,
        101 => 101,
        106 => 101,
        102 => 102,
        105 => 102,
        107 => 107,
        108 => 108,
        109 => 109,
        110 => 110,
        111 => 111,
        112 => 112,
        113 => 113,
        114 => 114,
        115 => 115,
        116 => 116,
        117 => 117,
        119 => 119,
        120 => 120,
        122 => 122,
        123 => 123,
        126 => 126,
        127 => 127,
        128 => 128,
        129 => 129,
        130 => 130,
    );
    /* Beginning here are the reduction cases.  A typical example
    ** follows:
    **  #line <lineno> <grammarfile>
    **   function yy_r0($yymsp){ ... }           // User supplied code
    **  #line <lineno> <thisfile>
    */
#line 73 "internal.templateparser.y"
    function yy_r0(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;     }
#line 1741 "internal.templateparser.php"
#line 79 "internal.templateparser.y"
    function yy_r1(){$this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1744 "internal.templateparser.php"
#line 81 "internal.templateparser.y"
    function yy_r2(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1747 "internal.templateparser.php"
#line 87 "internal.templateparser.y"
    function yy_r3(){if ($this->compiler->has_code) {
                                            $tmp =''; foreach ($this->prefix_code as $code) {$tmp.=$code;} $this->prefix_code=array();
                                            $this->_retvalue = $this->cacher->processNocacheCode($tmp.$this->yystack[$this->yyidx + 0]->minor, $this->compiler,$this->nocache,true);
                                         } $this->nocache=false;    }
#line 1753 "internal.templateparser.php"
#line 100 "internal.templateparser.y"
    function yy_r4(){ $this->_retvalue = '';    }
#line 1756 "internal.templateparser.php"
#line 103 "internal.templateparser.y"
    function yy_r5(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + -1]->minor, $this->compiler,false,false);    }
#line 1759 "internal.templateparser.php"
#line 105 "internal.templateparser.y"
    function yy_r6(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->left_delimiter, $this->compiler,false,false);    }
#line 1762 "internal.templateparser.php"
#line 107 "internal.templateparser.y"
    function yy_r7(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->right_delimiter, $this->compiler,false,false);    }
#line 1765 "internal.templateparser.php"
#line 109 "internal.templateparser.y"
    function yy_r8(){if (!$this->template->security) { 
                                       $this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + 0]->minor, $this->compiler, false,true);
                                      } elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_QUOTE) {
                                       $this->_retvalue = $this->cacher->processNocacheCode(htmlspecialchars($this->yystack[$this->yyidx + 0]->minor, ENT_QUOTES), $this->compiler, false, false);
                                      }elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_PASSTHRU || $this->smarty->security_policy->php_handling == SMARTY_PHP_ALLOW) {
                                       $this->_retvalue = $this->cacher->processNocacheCode("<?php echo '".$this->yystack[$this->yyidx + 0]->minor."';?>\n", $this->compiler, false, false);
                                      }elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_REMOVE) {
                                       $this->_retvalue = '';
                                      }	    }
#line 1776 "internal.templateparser.php"
#line 119 "internal.templateparser.y"
    function yy_r9(){if (!$this->template->security) { 
                                        $this->_retvalue = $this->cacher->processNocacheCode('<?php '.$this->yystack[$this->yyidx + -1]->minor.' ?>', $this->compiler, false,true);
                                      } elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_QUOTE) {
                                        $this->_retvalue = $this->cacher->processNocacheCode(htmlspecialchars('<?php '.$this->yystack[$this->yyidx + -1]->minor.' ?>', ENT_QUOTES), $this->compiler, false, false);	
                                      }elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_PASSTHRU || $this->smarty->security_policy->php_handling == SMARTY_PHP_ALLOW) {
                                        $this->_retvalue = $this->cacher->processNocacheCode("<?php echo '<?php ".$this->yystack[$this->yyidx + -1]->minor." ?>';?>\n", $this->compiler, false, false);
                                      }elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_REMOVE) {
                                        $this->_retvalue = '';
                                      }	    }
#line 1787 "internal.templateparser.php"
#line 129 "internal.templateparser.y"
    function yy_r10(){if (!$this->template->security) { 
                                        $this->_retvalue = $this->cacher->processNocacheCode($this->compiler->compileTag('print_expression',array('value'=>$this->yystack[$this->yyidx + -1]->minor)), $this->compiler, false,true);
                                      } elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_QUOTE) {
                                        $this->_retvalue = $this->cacher->processNocacheCode(htmlspecialchars('<?php '.t.' ?>', ENT_QUOTES), $this->compiler, false, false);	
                                      }elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_PASSTHRU || $this->smarty->security_policy->php_handling == SMARTY_PHP_ALLOW) {
                                        $this->_retvalue = $this->cacher->processNocacheCode("<?php echo '<?php ".t." ?>';?>\n", $this->compiler, false, false);
                                      }elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_REMOVE) {
                                        $this->_retvalue = '';
                                      }	    }
#line 1798 "internal.templateparser.php"
#line 139 "internal.templateparser.y"
    function yy_r11(){$this->_retvalue = $this->cacher->processNocacheCode("<?php echo '".$this->yystack[$this->yyidx + 0]->minor."';?>\n", $this->compiler, true, true);    }
#line 1801 "internal.templateparser.php"
#line 141 "internal.templateparser.y"
    function yy_r12(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + 0]->minor, $this->compiler,false,false);    }
#line 1804 "internal.templateparser.php"
#line 149 "internal.templateparser.y"
    function yy_r13(){ $this->_retvalue = $this->compiler->compileTag('print_expression',array_merge(array('value'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1807 "internal.templateparser.php"
#line 151 "internal.templateparser.y"
    function yy_r14(){ $this->_retvalue = $this->compiler->compileTag('assign',array_merge($this->yystack[$this->yyidx + -2]->minor,$this->yystack[$this->yyidx + -1]->minor));    }
#line 1810 "internal.templateparser.php"
#line 153 "internal.templateparser.y"
    function yy_r15(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor,$this->yystack[$this->yyidx + -1]->minor);    }
#line 1813 "internal.templateparser.php"
#line 155 "internal.templateparser.y"
    function yy_r16(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -4]->minor,array_merge(array('object_methode'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1816 "internal.templateparser.php"
#line 157 "internal.templateparser.y"
    function yy_r17(){ $this->_retvalue =  '<?php ob_start();?>'.$this->compiler->compileTag($this->yystack[$this->yyidx + -4]->minor,$this->yystack[$this->yyidx + -1]->minor).'<?php echo ';
																					                       if ($this->smarty->plugin_handler->loadSmartyPlugin($this->yystack[$this->yyidx + -3]->minor[0],'modifier')) {
                                                                      $this->_retvalue .= "\$_smarty_tpl->smarty->plugin_handler->".$this->yystack[$this->yyidx + -3]->minor[0] . "(array(ob_get_clean()". $this->yystack[$this->yyidx + -2]->minor ."),'modifier');?>";
                                                                 } else {
                                                                   if ($this->yystack[$this->yyidx + -3]->minor[0] == 'isset' || $this->yystack[$this->yyidx + -3]->minor[0] == 'empty' || is_callable($this->yystack[$this->yyidx + -3]->minor[0])) {
																					                            if (!$this->template->security || $this->smarty->security_handler->isTrustedModifier($this->yystack[$this->yyidx + -3]->minor[0], $this->compiler)) {
																					                              $this->_retvalue .= $this->yystack[$this->yyidx + -3]->minor[0] . "(ob_get_clean()". $this->yystack[$this->yyidx + -2]->minor .");?>";
																					                            }
																					                         } else {
                                                                      $this->compiler->trigger_template_error ("unknown modifier \"" . $this->yystack[$this->yyidx + -3]->minor[0] . "\"");
                                                                 }
                                                              }
                                                                }
#line 1831 "internal.templateparser.php"
#line 171 "internal.templateparser.y"
    function yy_r18(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor.'close',$this->yystack[$this->yyidx + -1]->minor);    }
#line 1834 "internal.templateparser.php"
#line 173 "internal.templateparser.y"
    function yy_r19(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor.'close',array('object_methode'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1837 "internal.templateparser.php"
#line 175 "internal.templateparser.y"
    function yy_r20(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor,array('if condition'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1840 "internal.templateparser.php"
#line 177 "internal.templateparser.y"
    function yy_r21(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -9]->minor,array('start'=>$this->yystack[$this->yyidx + -7]->minor,'ifexp'=>$this->yystack[$this->yyidx + -5]->minor,'varloop'=>$this->yystack[$this->yyidx + -2]->minor,'loop'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1843 "internal.templateparser.php"
#line 178 "internal.templateparser.y"
    function yy_r22(){ $this->_retvalue = '='.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1846 "internal.templateparser.php"
#line 179 "internal.templateparser.y"
    function yy_r23(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1849 "internal.templateparser.php"
#line 182 "internal.templateparser.y"
    function yy_r24(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -6]->minor,array('from'=>$this->yystack[$this->yyidx + -1]->minor,'item'=>$this->yystack[$this->yyidx + -3]->minor));    }
#line 1852 "internal.templateparser.php"
#line 189 "internal.templateparser.y"
    function yy_r26(){ $this->_retvalue = array_merge($this->yystack[$this->yyidx + -1]->minor,$this->yystack[$this->yyidx + 0]->minor);    }
#line 1855 "internal.templateparser.php"
#line 193 "internal.templateparser.y"
    function yy_r28(){ $this->_retvalue = array();    }
#line 1858 "internal.templateparser.php"
#line 197 "internal.templateparser.y"
    function yy_r29(){ $this->_retvalue = array($this->yystack[$this->yyidx + -2]->minor=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1861 "internal.templateparser.php"
#line 202 "internal.templateparser.y"
    function yy_r30(){ $this->_retvalue = array($this->yystack[$this->yyidx + 0]->minor);    }
#line 1864 "internal.templateparser.php"
#line 203 "internal.templateparser.y"
    function yy_r31(){ $this->yystack[$this->yyidx + -2]->minor[]=$this->yystack[$this->yyidx + 0]->minor; $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor;    }
#line 1867 "internal.templateparser.php"
#line 205 "internal.templateparser.y"
    function yy_r32(){ $this->_retvalue = array('var' => $this->yystack[$this->yyidx + -2]->minor, 'value'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1870 "internal.templateparser.php"
#line 213 "internal.templateparser.y"
    function yy_r33(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';     }
#line 1873 "internal.templateparser.php"
#line 217 "internal.templateparser.y"
    function yy_r35(){             
                                                            if ($this->smarty->plugin_handler->loadSmartyPlugin($this->yystack[$this->yyidx + -1]->minor[0],'modifier')) {
                                                                      $this->_retvalue = "\$_smarty_tpl->smarty->plugin_handler->".$this->yystack[$this->yyidx + -1]->minor[0] . "(array(". $this->yystack[$this->yyidx + -2]->minor . $this->yystack[$this->yyidx + 0]->minor ."),'modifier')";
                                                                 } else {
                                                                   if ($this->yystack[$this->yyidx + -1]->minor[0] == 'isset' || $this->yystack[$this->yyidx + -1]->minor[0] == 'empty' || is_callable($this->yystack[$this->yyidx + -1]->minor[0])) {
																					                            if (!$this->template->security || $this->smarty->security_handler->isTrustedModifier($this->yystack[$this->yyidx + -1]->minor[0], $this->compiler)) {
																					                               $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor[0] . "(". $this->yystack[$this->yyidx + -2]->minor . $this->yystack[$this->yyidx + 0]->minor .")";
																					                            }
																					                         } else {
                                                                      $this->compiler->trigger_template_error ("unknown modifier \"" . $this->yystack[$this->yyidx + -1]->minor[0] . "\"");
                                                                 }
                                                              }
                                                                }
#line 1888 "internal.templateparser.php"
#line 235 "internal.templateparser.y"
    function yy_r38(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1891 "internal.templateparser.php"
#line 237 "internal.templateparser.y"
    function yy_r39(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor . $this->yystack[$this->yyidx + -1]->minor . $this->yystack[$this->yyidx + 0]->minor;     }
#line 1894 "internal.templateparser.php"
#line 239 "internal.templateparser.y"
    function yy_r40(){ $this->_retvalue = '('. $this->yystack[$this->yyidx + -2]->minor . ').(' . $this->yystack[$this->yyidx + 0]->minor. ')';     }
#line 1897 "internal.templateparser.php"
#line 253 "internal.templateparser.y"
    function yy_r44(){$this->_retvalue = '$_smarty_tpl->getConfigVariable(\''. $this->yystack[$this->yyidx + -1]->minor .'\')';    }
#line 1900 "internal.templateparser.php"
#line 264 "internal.templateparser.y"
    function yy_r49(){ $this->_retvalue = "(". $this->yystack[$this->yyidx + -1]->minor .")";     }
#line 1903 "internal.templateparser.php"
#line 267 "internal.templateparser.y"
    function yy_r50(){ $this->_retvalue = "'".$this->yystack[$this->yyidx + -1]->minor."'";     }
#line 1906 "internal.templateparser.php"
#line 268 "internal.templateparser.y"
    function yy_r51(){ $this->_retvalue = "''";     }
#line 1909 "internal.templateparser.php"
#line 270 "internal.templateparser.y"
    function yy_r52(){ $this->_retvalue = "'".str_replace('\"','"',$this->yystack[$this->yyidx + -1]->minor)."'";     }
#line 1912 "internal.templateparser.php"
#line 276 "internal.templateparser.y"
    function yy_r54(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1915 "internal.templateparser.php"
#line 277 "internal.templateparser.y"
    function yy_r55(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -3]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -6]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -1]->minor .')';     }
#line 1918 "internal.templateparser.php"
#line 279 "internal.templateparser.y"
    function yy_r56(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor.'::'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1921 "internal.templateparser.php"
#line 280 "internal.templateparser.y"
    function yy_r57(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -4]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -7]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -2]->minor .')'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1924 "internal.templateparser.php"
#line 282 "internal.templateparser.y"
    function yy_r58(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1927 "internal.templateparser.php"
#line 284 "internal.templateparser.y"
    function yy_r59(){ $this->_retvalue = $this->yystack[$this->yyidx + -4]->minor.'::$'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1930 "internal.templateparser.php"
#line 286 "internal.templateparser.y"
    function yy_r60(){ $this->_retvalue = $this->yystack[$this->yyidx + -5]->minor.'::$'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1933 "internal.templateparser.php"
#line 293 "internal.templateparser.y"
    function yy_r61(){ if ($this->yystack[$this->yyidx + -1]->minor == '\'smarty\'') { $this->_retvalue =  $this->compiler->compileTag(trim($this->yystack[$this->yyidx + -1]->minor,"'"),$this->yystack[$this->yyidx + 0]->minor);} else {
                                                         $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -1]->minor .')->value'.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -1]->minor,"'"))->nocache;}    }
#line 1937 "internal.templateparser.php"
#line 296 "internal.templateparser.y"
    function yy_r62(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->'.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 1940 "internal.templateparser.php"
#line 308 "internal.templateparser.y"
    function yy_r65(){return;    }
#line 1943 "internal.templateparser.php"
#line 312 "internal.templateparser.y"
    function yy_r66(){ $this->_retvalue = "['". $this->yystack[$this->yyidx + 0]->minor ."']";    }
#line 1946 "internal.templateparser.php"
#line 313 "internal.templateparser.y"
    function yy_r67(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + 0]->minor ."]";    }
#line 1949 "internal.templateparser.php"
#line 315 "internal.templateparser.y"
    function yy_r68(){ $this->_retvalue = '['.$this->compiler->compileTag('smarty','[\'section\'][\''.$this->yystack[$this->yyidx + -1]->minor.'\'][\'index\']').']';    }
#line 1952 "internal.templateparser.php"
#line 317 "internal.templateparser.y"
    function yy_r69(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + -1]->minor ."]";    }
#line 1955 "internal.templateparser.php"
#line 318 "internal.templateparser.y"
    function yy_r70(){ $this->_retvalue = "[]";    }
#line 1958 "internal.templateparser.php"
#line 326 "internal.templateparser.y"
    function yy_r72(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1961 "internal.templateparser.php"
#line 328 "internal.templateparser.y"
    function yy_r73(){$this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 1964 "internal.templateparser.php"
#line 330 "internal.templateparser.y"
    function yy_r74(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 1967 "internal.templateparser.php"
#line 335 "internal.templateparser.y"
    function yy_r75(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->value'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 1970 "internal.templateparser.php"
#line 337 "internal.templateparser.y"
    function yy_r76(){$this->_retvalue  = $this->yystack[$this->yyidx + 0]->minor;     }
#line 1973 "internal.templateparser.php"
#line 339 "internal.templateparser.y"
    function yy_r77(){$this->_retvalue  = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1976 "internal.templateparser.php"
#line 341 "internal.templateparser.y"
    function yy_r78(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1979 "internal.templateparser.php"
#line 344 "internal.templateparser.y"
    function yy_r79(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1982 "internal.templateparser.php"
#line 350 "internal.templateparser.y"
    function yy_r80(){if (!$this->template->security || $this->smarty->security_handler->isTrustedPhpFunction($this->yystack[$this->yyidx + -3]->minor, $this->compiler)) {
																					            if ($this->yystack[$this->yyidx + -3]->minor == 'isset' || $this->yystack[$this->yyidx + -3]->minor == 'empty' || $this->yystack[$this->yyidx + -3]->minor == 'array' || is_callable($this->yystack[$this->yyidx + -3]->minor)) {
																					                $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";
																					            } else {
                                                       $this->compiler->trigger_template_error ("unknown function \"" . $this->yystack[$this->yyidx + -3]->minor . "\"");
                                                      }
                                                    }    }
#line 1991 "internal.templateparser.php"
#line 361 "internal.templateparser.y"
    function yy_r81(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";    }
#line 1994 "internal.templateparser.php"
#line 365 "internal.templateparser.y"
    function yy_r82(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.",".$this->yystack[$this->yyidx + 0]->minor;    }
#line 1997 "internal.templateparser.php"
#line 369 "internal.templateparser.y"
    function yy_r84(){ return;    }
#line 2000 "internal.templateparser.php"
#line 374 "internal.templateparser.y"
    function yy_r85(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,true);    }
#line 2003 "internal.templateparser.php"
#line 375 "internal.templateparser.y"
    function yy_r86(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,false);    }
#line 2006 "internal.templateparser.php"
#line 382 "internal.templateparser.y"
    function yy_r87(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2009 "internal.templateparser.php"
#line 386 "internal.templateparser.y"
    function yy_r89(){$this->_retvalue = ','.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2012 "internal.templateparser.php"
#line 387 "internal.templateparser.y"
    function yy_r90(){$this->_retvalue = ',\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2015 "internal.templateparser.php"
#line 394 "internal.templateparser.y"
    function yy_r92(){$this->_retvalue = '!'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2018 "internal.templateparser.php"
#line 399 "internal.templateparser.y"
    function yy_r94(){$this->_retvalue =$this->yystack[$this->yyidx + 0]->minor;    }
#line 2021 "internal.templateparser.php"
#line 400 "internal.templateparser.y"
    function yy_r95(){$this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2024 "internal.templateparser.php"
#line 402 "internal.templateparser.y"
    function yy_r97(){$this->_retvalue = '!('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2027 "internal.templateparser.php"
#line 403 "internal.templateparser.y"
    function yy_r98(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2030 "internal.templateparser.php"
#line 404 "internal.templateparser.y"
    function yy_r99(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2033 "internal.templateparser.php"
#line 405 "internal.templateparser.y"
    function yy_r100(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2036 "internal.templateparser.php"
#line 406 "internal.templateparser.y"
    function yy_r101(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2039 "internal.templateparser.php"
#line 407 "internal.templateparser.y"
    function yy_r102(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2042 "internal.templateparser.php"
#line 413 "internal.templateparser.y"
    function yy_r107(){$this->_retvalue = '==';    }
#line 2045 "internal.templateparser.php"
#line 414 "internal.templateparser.y"
    function yy_r108(){$this->_retvalue = '!=';    }
#line 2048 "internal.templateparser.php"
#line 415 "internal.templateparser.y"
    function yy_r109(){$this->_retvalue = '>';    }
#line 2051 "internal.templateparser.php"
#line 416 "internal.templateparser.y"
    function yy_r110(){$this->_retvalue = '<';    }
#line 2054 "internal.templateparser.php"
#line 417 "internal.templateparser.y"
    function yy_r111(){$this->_retvalue = '>=';    }
#line 2057 "internal.templateparser.php"
#line 418 "internal.templateparser.y"
    function yy_r112(){$this->_retvalue = '<=';    }
#line 2060 "internal.templateparser.php"
#line 419 "internal.templateparser.y"
    function yy_r113(){$this->_retvalue = '===';    }
#line 2063 "internal.templateparser.php"
#line 420 "internal.templateparser.y"
    function yy_r114(){$this->_retvalue = '!==';    }
#line 2066 "internal.templateparser.php"
#line 422 "internal.templateparser.y"
    function yy_r115(){$this->_retvalue = '&&';    }
#line 2069 "internal.templateparser.php"
#line 423 "internal.templateparser.y"
    function yy_r116(){$this->_retvalue = '||';    }
#line 2072 "internal.templateparser.php"
#line 428 "internal.templateparser.y"
    function yy_r117(){ $this->_retvalue = 'array('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2075 "internal.templateparser.php"
#line 430 "internal.templateparser.y"
    function yy_r119(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.','.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2078 "internal.templateparser.php"
#line 431 "internal.templateparser.y"
    function yy_r120(){ return;     }
#line 2081 "internal.templateparser.php"
#line 433 "internal.templateparser.y"
    function yy_r122(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2084 "internal.templateparser.php"
#line 434 "internal.templateparser.y"
    function yy_r123(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + -2]->minor.'\'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2087 "internal.templateparser.php"
#line 441 "internal.templateparser.y"
    function yy_r126(){$this->_retvalue = "'.".$this->yystack[$this->yyidx + 0]->minor.".'";    }
#line 2090 "internal.templateparser.php"
#line 442 "internal.templateparser.y"
    function yy_r127(){$this->_retvalue = "`".$this->yystack[$this->yyidx + -1]->minor."`";    }
#line 2093 "internal.templateparser.php"
#line 443 "internal.templateparser.y"
    function yy_r128(){$this->_retvalue = "'.".$this->yystack[$this->yyidx + -1]->minor.".'";    }
#line 2096 "internal.templateparser.php"
#line 444 "internal.templateparser.y"
    function yy_r129(){$this->_retvalue = "'.(".$this->yystack[$this->yyidx + -1]->minor.").'";    }
#line 2099 "internal.templateparser.php"
#line 445 "internal.templateparser.y"
    function yy_r130(){$this->_retvalue = addcslashes($this->yystack[$this->yyidx + 0]->minor,"'");    }
#line 2102 "internal.templateparser.php"

    /**
     * placeholder for the left hand side in a reduce operation.
     * 
     * For a parser with a rule like this:
     * <pre>
     * rule(A) ::= B. { A = 1; }
     * </pre>
     * 
     * The parser will translate to something like:
     * 
     * <code>
     * function yy_r0(){$this->_retvalue = 1;}
     * </code>
     */
    private $_retvalue;

    /**
     * Perform a reduce action and the shift that must immediately
     * follow the reduce.
     * 
     * For a rule such as:
     * 
     * <pre>
     * A ::= B blah C. { dosomething(); }
     * </pre>
     * 
     * This function will first call the action, if any, ("dosomething();" in our
     * example), and then it will pop three states from the stack,
     * one for each entry on the right-hand side of the expression
     * (B, blah, and C in our example rule), and then push the result of the action
     * back on to the stack with the resulting state reduced to (as described in the .out
     * file)
     * @param int Number of the rule by which to reduce
     */
    function yy_reduce($yyruleno)
    {
        //int $yygoto;                     /* The next state */
        //int $yyact;                      /* The next action */
        //mixed $yygotominor;        /* The LHS of the rule reduced */
        //TP_yyStackEntry $yymsp;            /* The top of the parser's stack */
        //int $yysize;                     /* Amount to pop the stack */
        $yymsp = $this->yystack[$this->yyidx];
        if (self::$yyTraceFILE && $yyruleno >= 0 
              && $yyruleno < count(self::$yyRuleName)) {
            fprintf(self::$yyTraceFILE, "%sReduce (%d) [%s].\n",
                self::$yyTracePrompt, $yyruleno,
                self::$yyRuleName[$yyruleno]);
        }

        $this->_retvalue = $yy_lefthand_side = null;
        if (array_key_exists($yyruleno, self::$yyReduceMap)) {
            // call the action
            $this->_retvalue = null;
            $this->{'yy_r' . self::$yyReduceMap[$yyruleno]}();
            $yy_lefthand_side = $this->_retvalue;
        }
        $yygoto = self::$yyRuleInfo[$yyruleno]['lhs'];
        $yysize = self::$yyRuleInfo[$yyruleno]['rhs'];
        $this->yyidx -= $yysize;
        for($i = $yysize; $i; $i--) {
            // pop all of the right-hand side parameters
            array_pop($this->yystack);
        }
        $yyact = $this->yy_find_reduce_action($this->yystack[$this->yyidx]->stateno, $yygoto);
        if ($yyact < self::YYNSTATE) {
            /* If we are not debugging and the reduce action popped at least
            ** one element off the stack, then we can push the new element back
            ** onto the stack here, and skip the stack overflow test in yy_shift().
            ** That gives a significant speed improvement. */
            if (!self::$yyTraceFILE && $yysize) {
                $this->yyidx++;
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

    /**
     * The following code executes when the parse fails
     * 
     * Code from %parse_fail is inserted here
     */
    function yy_parse_failed()
    {
        if (self::$yyTraceFILE) {
            fprintf(self::$yyTraceFILE, "%sFail!\n", self::$yyTracePrompt);
        }
        while ($this->yyidx >= 0) {
            $this->yy_pop_parser_stack();
        }
        /* Here code is inserted which will be executed whenever the
        ** parser fails */
    }

    /**
     * The following code executes when a syntax error first occurs.
     * 
     * %syntax_error code is inserted here
     * @param int The major type of the error token
     * @param mixed The minor type of the error token
     */
    function yy_syntax_error($yymajor, $TOKEN)
    {
#line 55 "internal.templateparser.y"

    $this->internalError = true;
    $this->yymajor = $yymajor;
    $this->compiler->trigger_template_error();
#line 2220 "internal.templateparser.php"
    }

    /**
     * The following is executed when the parser accepts
     * 
     * %parse_accept code is inserted here
     */
    function yy_accept()
    {
        if (self::$yyTraceFILE) {
            fprintf(self::$yyTraceFILE, "%sAccept!\n", self::$yyTracePrompt);
        }
        while ($this->yyidx >= 0) {
            $stack = $this->yy_pop_parser_stack();
        }
        /* Here code is inserted which will be executed whenever the
        ** parser accepts */
#line 47 "internal.templateparser.y"

    $this->successful = !$this->internalError;
    $this->internalError = false;
    $this->retvalue = $this->_retvalue;
    //echo $this->retvalue."\n\n";
#line 2245 "internal.templateparser.php"
    }

    /**
     * The main parser program.
     * 
     * The first argument is the major token number.  The second is
     * the token value string as scanned from the input.
     *
     * @param int the token number
     * @param mixed the token value
     * @param mixed any extra arguments that should be passed to handlers
     */
    function doParse($yymajor, $yytokenvalue)
    {
//        $yyact;            /* The parser action. */
//        $yyendofinput;     /* True if we are at the end of input */
        $yyerrorhit = 0;   /* True if yymajor has invoked an error */
        
        /* (re)initialize the parser, if necessary */
        if ($this->yyidx === null || $this->yyidx < 0) {
            /* if ($yymajor == 0) return; // not sure why this was here... */
            $this->yyidx = 0;
            $this->yyerrcnt = -1;
            $x = new TP_yyStackEntry;
            $x->stateno = 0;
            $x->major = 0;
            $this->yystack = array();
            array_push($this->yystack, $x);
        }
        $yyendofinput = ($yymajor==0);
        
        if (self::$yyTraceFILE) {
            fprintf(self::$yyTraceFILE, "%sInput %s\n",
                self::$yyTracePrompt, $this->yyTokenName[$yymajor]);
        }
        
        do {
            $yyact = $this->yy_find_shift_action($yymajor);
            if ($yymajor < self::YYERRORSYMBOL &&
                  !$this->yy_is_expected_token($yymajor)) {
                // force a syntax error
                $yyact = self::YY_ERROR_ACTION;
            }
            if ($yyact < self::YYNSTATE) {
                $this->yy_shift($yyact, $yymajor, $yytokenvalue);
                $this->yyerrcnt--;
                if ($yyendofinput && $this->yyidx >= 0) {
                    $yymajor = 0;
                } else {
                    $yymajor = self::YYNOCODE;
                }
            } elseif ($yyact < self::YYNSTATE + self::YYNRULE) {
                $this->yy_reduce($yyact - self::YYNSTATE);
            } elseif ($yyact == self::YY_ERROR_ACTION) {
                if (self::$yyTraceFILE) {
                    fprintf(self::$yyTraceFILE, "%sSyntax Error!\n",
                        self::$yyTracePrompt);
                }
                if (self::YYERRORSYMBOL) {
                    /* A syntax error has occurred.
                    ** The response to an error depends upon whether or not the
                    ** grammar defines an error token "ERROR".  
                    **
                    ** This is what we do if the grammar does define ERROR:
                    **
                    **  * Call the %syntax_error function.
                    **
                    **  * Begin popping the stack until we enter a state where
                    **    it is legal to shift the error symbol, then shift
                    **    the error symbol.
                    **
                    **  * Set the error count to three.
                    **
                    **  * Begin accepting and shifting new tokens.  No new error
                    **    processing will occur until three tokens have been
                    **    shifted successfully.
                    **
                    */
                    if ($this->yyerrcnt < 0) {
                        $this->yy_syntax_error($yymajor, $yytokenvalue);
                    }
                    $yymx = $this->yystack[$this->yyidx]->major;
                    if ($yymx == self::YYERRORSYMBOL || $yyerrorhit ){
                        if (self::$yyTraceFILE) {
                            fprintf(self::$yyTraceFILE, "%sDiscard input token %s\n",
                                self::$yyTracePrompt, $this->yyTokenName[$yymajor]);
                        }
                        $this->yy_destructor($yymajor, $yytokenvalue);
                        $yymajor = self::YYNOCODE;
                    } else {
                        while ($this->yyidx >= 0 &&
                                 $yymx != self::YYERRORSYMBOL &&
        ($yyact = $this->yy_find_shift_action(self::YYERRORSYMBOL)) >= self::YYNSTATE
                              ){
                            $this->yy_pop_parser_stack();
                        }
                        if ($this->yyidx < 0 || $yymajor==0) {
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
                    /* YYERRORSYMBOL is not defined */
                    /* This is what we do if the grammar does not define ERROR:
                    **
                    **  * Report an error message, and throw away the input token.
                    **
                    **  * If the input token is $, then fail the parse.
                    **
                    ** As before, subsequent error messages are suppressed until
                    ** three input tokens have been successfully shifted.
                    */
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
