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
    const TP_INTEGER                        = 11;
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
    const TP_AS                             = 46;
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
    const TP_ISIN                           = 61;
    const TP_LITERALSTART                   = 62;
    const TP_LITERALEND                     = 63;
    const TP_LDELIMTAG                      = 64;
    const TP_RDELIMTAG                      = 65;
    const TP_PHPSTART                       = 66;
    const TP_PHPEND                         = 67;
    const YY_NO_ACTION = 419;
    const YY_ACCEPT_ACTION = 418;
    const YY_ERROR_ACTION = 417;

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
    const YY_SZ_ACTTAB = 884;
static public $yy_action = array(
 /*     0 */   248,  149,  250,   38,  138,    3,  138,   12,  140,   65,
 /*    10 */   247,  270,  197,  173,  172,  174,  175,  171,   29,   15,
 /*    20 */   133,  144,  212,  213,  222,  140,   22,  106,  209,    2,
 /*    30 */    27,   29,   52,   55,  229,  217,  184,  183,   15,  157,
 /*    40 */    35,   21,  127,  219,  169,  241,  106,  193,  185,  181,
 /*    50 */   180,    9,   10,    7,   11,    6,    5,   37,  241,   29,
 /*    60 */    14,  184,  183,  138,  189,  154,  122,  140,   24,  218,
 /*    70 */   190,  267,  193,  185,  181,  180,    9,   10,    7,   11,
 /*    80 */     6,    5,   18,  196,   65,  191,  241,   65,  184,  183,
 /*    90 */   149,  187,   38,   13,    3,  101,   12,  138,   61,  193,
 /*   100 */   185,  181,  180,    9,   10,    7,   11,    6,    5,  131,
 /*   110 */   266,  138,   15,  153,  157,   31,   45,  157,    2,   22,
 /*   120 */   106,   52,   55,  229,  217,   15,   19,   23,  157,  184,
 /*   130 */   183,  138,   22,  106,  196,  138,  191,    1,  145,   14,
 /*   140 */   193,  185,  181,  180,    9,   10,    7,   11,    6,    5,
 /*   150 */   227,   51,   15,  222,   95,  210,   71,  147,  136,  265,
 /*   160 */   106,  165,  156,  149,   85,   38,  216,   30,  138,   12,
 /*   170 */   261,   65,  219,  418,   49,  177,  186,  198,   65,  149,
 /*   180 */   159,   38,  134,   30,  253,   12,  196,   65,  191,  162,
 /*   190 */    54,  161,  212,  213,   52,   55,  229,  217,  133,  130,
 /*   200 */   223,  157,  196,  129,  191,  240,  222,  196,  157,  191,
 /*   210 */    52,   55,  229,  217,  236,  167,   28,  157,  239,  211,
 /*   220 */    47,  269,  149,   57,   38,  219,   30,   37,   12,   32,
 /*   230 */    65,  149,   41,   38,  196,   30,  191,   12,   34,   65,
 /*   240 */    39,  135,  258,  140,  206,   16,  212,  213,  262,  148,
 /*   250 */   139,   69,  222,   52,   55,  229,  217,   29,  176,  115,
 /*   260 */   157,  138,   52,   55,  229,  217,  149,  201,   38,  157,
 /*   270 */    30,  219,   12,  265,   65,   62,  204,  257,  235,   59,
 /*   280 */    66,   37,  138,   29,  241,   40,   29,  170,  247,  270,
 /*   290 */   197,  173,  172,  174,  175,  171,  112,   52,   55,  229,
 /*   300 */   217,  149,  242,   38,  157,   30,   21,   12,  118,   65,
 /*   310 */   241,    8,   39,  241,  227,  114,  124,  222,   35,  218,
 /*   320 */   132,  267,  260,  138,  232,  231,  233,  222,   85,  215,
 /*   330 */   216,  249,   52,   55,  229,  217,  219,  227,   51,  157,
 /*   340 */   222,  104,  155,   73,  227,   51,  219,  222,  231,  233,
 /*   350 */    75,   85,  243,  216,  140,  231,  233,   94,   85,  219,
 /*   360 */   216,   97,  166,  130,  198,  208,  219,  227,   51,  100,
 /*   370 */   222,  198,  243,   70,  227,   51,  252,  222,  231,  233,
 /*   380 */    78,   85,   93,  216,  130,  231,  233,   92,   85,  219,
 /*   390 */   216,   15,   32,  243,  198,   41,  219,   29,   22,  106,
 /*   400 */   227,  198,  240,  222,  237,  194,  186,  227,   51,  251,
 /*   410 */   222,  246,  245,   77,  227,   51,  216,  222,  231,  233,
 /*   420 */    76,   85,  219,  216,  241,  231,  233,  140,   85,  219,
 /*   430 */   216,  168,  140,  126,  198,   84,  219,  140,  150,  227,
 /*   440 */    50,  198,  222,  195,  154,   72,  227,   51,   81,  222,
 /*   450 */   231,  233,   79,   85,  138,  216,  244,  231,  233,  202,
 /*   460 */    85,  219,  216,  264,  227,   51,  198,  222,  219,  178,
 /*   470 */    74,  227,   51,  198,  222,  231,  233,   80,   85,  140,
 /*   480 */   216,  220,  231,  233,  149,   85,  219,  216,   30,  221,
 /*   490 */    12,  198,   65,  219,  107,  199,  128,   20,  198,   53,
 /*   500 */    86,  116,  130,  135,   90,  243,  267,   89,   44,  227,
 /*   510 */   110,  252,  222,  163,  240,   52,   55,  229,  217,  240,
 /*   520 */   231,  233,  157,   85,  121,  216,   26,  105,   88,  267,
 /*   530 */    56,  219,  227,  109,  178,  222,  203,  142,  243,  137,
 /*   540 */   255,  178,   43,  231,  233,  240,   85,   98,  216,  227,
 /*   550 */   110,  160,  222,  227,  219,   58,  222,   87,  243,   25,
 /*   560 */   231,  233,  178,   85,  214,  216,   27,  227,  109,  216,
 /*   570 */   222,  219,  240,  224,  188,  219,  256,  151,  231,  233,
 /*   580 */   254,   85,  179,  216,   68,  227,  146,  268,  222,  219,
 /*   590 */   149,    4,  227,  109,   30,  222,  231,  233,   65,   91,
 /*   600 */    64,  216,  259,  231,  233,  207,   85,  219,  216,  135,
 /*   610 */   143,  164,  228,  158,  219,  227,  109,   63,  222,  152,
 /*   620 */   200,   52,   55,  229,  217,   33,  231,  233,  157,   85,
 /*   630 */    60,  216,  238,  226,  234,  227,   46,  219,  222,  192,
 /*   640 */   218,   65,  227,  120,  103,  222,  231,  233,   36,   85,
 /*   650 */   252,  216,  225,  231,  233,  182,   85,  219,  216,  140,
 /*   660 */    34,   17,  208,  256,  219,  227,  119,  256,  222,   42,
 /*   670 */   256,  157,  256,  256,   67,  256,  231,  233,  199,   85,
 /*   680 */    20,  216,  256,  227,   99,  256,  222,  219,  256,  256,
 /*   690 */   227,  113,  256,  222,  231,  233,  163,   85,  256,  216,
 /*   700 */   256,  231,  233,  256,   85,  219,  216,  256,  256,  256,
 /*   710 */   227,  123,  219,  222,  256,  256,  256,  227,  108,  263,
 /*   720 */   222,  231,  233,  256,   85,   43,  216,  256,  231,  233,
 /*   730 */   256,   85,  219,  216,  256,  256,  256,  256,  256,  219,
 /*   740 */   227,  117,  256,  222,  256,  256,  256,  256,  256,  256,
 /*   750 */   256,  231,  233,  256,   85,  256,  216,  256,  227,   48,
 /*   760 */   256,  141,  219,  256,  256,  227,  111,  256,  222,  231,
 /*   770 */   233,  256,   85,  256,  216,  256,  231,  233,  256,   85,
 /*   780 */   219,  216,  256,  256,  256,  227,  102,  219,  222,  256,
 /*   790 */   256,  256,  227,  125,  256,  222,  231,  233,  256,   85,
 /*   800 */   256,  216,  256,  231,  233,  256,   85,  219,  216,  256,
 /*   810 */   256,  256,  256,  256,  219,  227,   96,  256,  222,  256,
 /*   820 */   256,  256,  256,  256,  256,  256,  231,  233,  256,   85,
 /*   830 */   256,  216,  256,  227,  256,  256,  222,  219,  256,  256,
 /*   840 */   227,  256,  256,  222,  231,  233,  256,   83,  256,  216,
 /*   850 */   256,  231,  233,  256,   82,  219,  216,  256,  256,  256,
 /*   860 */   227,  227,  219,  222,  222,  256,  256,  256,  256,  256,
 /*   870 */   256,  205,  230,  256,  256,  256,  216,  216,  256,  256,
 /*   880 */   256,  256,  219,  219,
    );
    static public $yy_lookahead = array(
 /*     0 */     4,   11,    4,   13,   25,   15,   25,   17,   27,   19,
 /*    10 */    31,   32,   33,   34,   35,   36,   37,   38,    3,   15,
 /*    20 */    30,   74,   12,   13,   77,   27,   22,   23,   18,   39,
 /*    30 */    26,    3,   42,   43,   44,   45,   40,   41,   15,   49,
 /*    40 */    61,   26,    4,   96,    4,   30,   23,   51,   52,   53,
 /*    50 */    54,   55,   56,   57,   58,   59,   60,   47,   30,    3,
 /*    60 */    15,   40,   41,   25,   16,   50,   94,   27,    3,   97,
 /*    70 */    14,   99,   51,   52,   53,   54,   55,   56,   57,   58,
 /*    80 */    59,   60,   26,    1,   19,    3,   30,   19,   40,   41,
 /*    90 */    11,    9,   13,   21,   15,   30,   17,   25,   19,   51,
 /*   100 */    52,   53,   54,   55,   56,   57,   58,   59,   60,   30,
 /*   110 */     4,   25,   15,   24,   49,   29,   79,   49,   39,   22,
 /*   120 */    23,   42,   43,   44,   45,   15,   29,    3,   49,   40,
 /*   130 */    41,   25,   22,   23,    1,   25,    3,   27,   28,   15,
 /*   140 */    51,   52,   53,   54,   55,   56,   57,   58,   59,   60,
 /*   150 */    74,   75,   15,   77,   95,   18,   80,   81,   82,   16,
 /*   160 */    23,   85,   86,   11,   88,   13,   90,   15,   25,   17,
 /*   170 */    18,   19,   96,   69,   70,   71,   72,  101,   19,   11,
 /*   180 */    19,   13,   30,   15,    4,   17,    1,   19,    3,   30,
 /*   190 */    83,   30,   12,   13,   42,   43,   44,   45,   30,   78,
 /*   200 */    67,   49,    1,   74,    3,   98,   77,    1,   49,    3,
 /*   210 */    42,   43,   44,   45,    1,    2,    3,   49,    5,    6,
 /*   220 */     7,   92,   11,   10,   13,   96,   15,   47,   17,   17,
 /*   230 */    19,   11,   20,   13,    1,   15,    3,   17,   22,   19,
 /*   240 */    28,   30,   18,   27,   43,   21,   12,   13,   63,   74,
 /*   250 */    30,   16,   77,   42,   43,   44,   45,    3,    4,   30,
 /*   260 */    49,   25,   42,   43,   44,   45,   11,   48,   13,   49,
 /*   270 */    15,   96,   17,   16,   19,   62,   43,   64,   65,   66,
 /*   280 */    19,   47,   25,    3,   30,   30,    3,    4,   31,   32,
 /*   290 */    33,   34,   35,   36,   37,   38,   79,   42,   43,   44,
 /*   300 */    45,   11,    4,   13,   49,   15,   26,   17,   21,   19,
 /*   310 */    30,   24,   28,   30,   74,   75,   94,   77,   61,   97,
 /*   320 */    30,   99,   30,   25,   74,   85,   86,   77,   88,   99,
 /*   330 */    90,    4,   42,   43,   44,   45,   96,   74,   75,   49,
 /*   340 */    77,   76,   50,   80,   74,   75,   96,   77,   85,   86,
 /*   350 */    80,   88,   87,   90,   27,   85,   86,   95,   88,   96,
 /*   360 */    90,   76,   84,   78,  101,  100,   96,   74,   75,   95,
 /*   370 */    77,  101,   87,   80,   74,   75,   98,   77,   85,   86,
 /*   380 */    80,   88,   76,   90,   78,   85,   86,   83,   88,   96,
 /*   390 */    90,   15,   17,   87,  101,   20,   96,    3,   22,   23,
 /*   400 */    74,  101,   98,   77,    4,   71,   72,   74,   75,    4,
 /*   410 */    77,   85,   86,   80,   74,   75,   90,   77,   85,   86,
 /*   420 */    80,   88,   96,   90,   30,   85,   86,   27,   88,   96,
 /*   430 */    90,   30,   27,    4,  101,   73,   96,   27,   28,   74,
 /*   440 */    75,  101,   77,    4,   50,   80,   74,   75,   91,   77,
 /*   450 */    85,   86,   80,   88,   25,   90,    4,   85,   86,   48,
 /*   460 */    88,   96,   90,  106,   74,   75,  101,   77,   96,  107,
 /*   470 */    80,   74,   75,  101,   77,   85,   86,   80,   88,   27,
 /*   480 */    90,   49,   85,   86,   11,   88,   96,   90,   15,   30,
 /*   490 */    17,  101,   19,   96,   76,    1,   78,    3,  101,   83,
 /*   500 */    73,   94,   78,   30,   83,   87,   99,   73,   95,   74,
 /*   510 */    75,   98,   77,   19,   98,   42,   43,   44,   45,   98,
 /*   520 */    85,   86,   49,   88,   94,   90,  102,   76,   73,   99,
 /*   530 */    83,   96,   74,   75,  107,   77,   42,   30,   87,  104,
 /*   540 */   105,  107,   48,   85,   86,   98,   88,   76,   90,   74,
 /*   550 */    75,   93,   77,   74,   96,   30,   77,   83,   87,   26,
 /*   560 */    85,   86,  107,   88,   85,   90,   26,   74,   75,   90,
 /*   570 */    77,   96,   98,    8,    4,   96,   16,   20,   85,   86,
 /*   580 */   105,   88,    4,   90,   30,   74,   93,   16,   77,   96,
 /*   590 */    11,  103,   74,   75,   15,   77,   85,   86,   19,   88,
 /*   600 */    19,   90,   30,   85,   86,   30,   88,   96,   90,   30,
 /*   610 */    30,   93,   11,   46,   96,   74,   75,   19,   77,   46,
 /*   620 */   106,   42,   43,   44,   45,    3,   85,   86,   49,   88,
 /*   630 */    19,   90,   87,   11,   93,   74,   75,   96,   77,   81,
 /*   640 */    97,   19,   74,   75,   95,   77,   85,   86,   89,   88,
 /*   650 */    98,   90,   30,   85,   86,  107,   88,   96,   90,   27,
 /*   660 */    22,   15,  100,  108,   96,   74,   75,  108,   77,   95,
 /*   670 */   108,   49,  108,  108,   92,  108,   85,   86,    1,   88,
 /*   680 */     3,   90,  108,   74,   75,  108,   77,   96,  108,  108,
 /*   690 */    74,   75,  108,   77,   85,   86,   19,   88,  108,   90,
 /*   700 */   108,   85,   86,  108,   88,   96,   90,  108,  108,  108,
 /*   710 */    74,   75,   96,   77,  108,  108,  108,   74,   75,   42,
 /*   720 */    77,   85,   86,  108,   88,   48,   90,  108,   85,   86,
 /*   730 */   108,   88,   96,   90,  108,  108,  108,  108,  108,   96,
 /*   740 */    74,   75,  108,   77,  108,  108,  108,  108,  108,  108,
 /*   750 */   108,   85,   86,  108,   88,  108,   90,  108,   74,   75,
 /*   760 */   108,   77,   96,  108,  108,   74,   75,  108,   77,   85,
 /*   770 */    86,  108,   88,  108,   90,  108,   85,   86,  108,   88,
 /*   780 */    96,   90,  108,  108,  108,   74,   75,   96,   77,  108,
 /*   790 */   108,  108,   74,   75,  108,   77,   85,   86,  108,   88,
 /*   800 */   108,   90,  108,   85,   86,  108,   88,   96,   90,  108,
 /*   810 */   108,  108,  108,  108,   96,   74,   75,  108,   77,  108,
 /*   820 */   108,  108,  108,  108,  108,  108,   85,   86,  108,   88,
 /*   830 */   108,   90,  108,   74,  108,  108,   77,   96,  108,  108,
 /*   840 */    74,  108,  108,   77,   85,   86,  108,   88,  108,   90,
 /*   850 */   108,   85,   86,  108,   88,   96,   90,  108,  108,  108,
 /*   860 */    74,   74,   96,   77,   77,  108,  108,  108,  108,  108,
 /*   870 */   108,   85,   85,  108,  108,  108,   90,   90,  108,  108,
 /*   880 */   108,  108,   96,   96,
);
    const YY_SHIFT_USE_DFLT = -22;
    const YY_SHIFT_MAX = 168;
    static public $yy_shift_ofst = array(
 /*     0 */   213,   79,  -10,  -10,  -10,  -10,  -10,  -10,  -10,  -10,
 /*    10 */   -10,  -10,  290,  168,  168,  168,  290,  168,  168,  168,
 /*    20 */   168,  168,  168,  168,  168,  168,  168,  168,  255,  168,
 /*    30 */   168,  168,  152,  211,  220,  473,  579,  579,  579,   65,
 /*    40 */   110,  622,  212,  159,  212,  216,  -19,   68,  -19,  213,
 /*    50 */   257,  -21,  677,   56,   15,  233,  394,  206,  410,  206,
 /*    60 */    28,   28,  206,   28,   28,   28,   28,  284,  632,  284,
 /*    70 */    89,   -4,   48,   21,   21,   21,   21,   21,   21,   21,
 /*    80 */    21,  494,  180,   10,   82,  234,  201,  283,  185,  133,
 /*    90 */   254,  234,  280,  452,  375,  375,  298,  400,  405,  429,
 /*   100 */   375,  124,  143,  375,  327,   40,  161,   -2,   38,   72,
 /*   110 */    86,  106,  638,  236,  236,  646,  284,  236,  611,  236,
 /*   120 */   236,  284,  284,  236,  284,  236,  -22,  -22,  -22,  -22,
 /*   130 */   -22,    4,   97,  376,  137,   23,  287,  224,  292,   23,
 /*   140 */   507,  533,  540,  570,  565,  554,  560,  439,  411,  557,
 /*   150 */   580,  601,  598,  581,  459,  572,  573,  401,  261,  229,
 /*   160 */   235,   45,  219,  575,  571,  567,  578,  525,  432,
);
    const YY_REDUCE_USE_DFLT = -54;
    const YY_REDUCE_MAX = 130;
    static public $yy_reduce_ofst = array(
 /*     0 */   104,   76,  340,  365,  372,  333,  300,  263,  293,  270,
 /*    10 */   390,  397,  435,  541,  518,  493,  475,  458,  591,  616,
 /*    20 */   691,  636,  240,  643,  609,  561,  568,  666,  684,  741,
 /*    30 */   711,  718,  759,  766,  511,  326,  786,  479,  787,  129,
 /*    40 */   418,  250,  -28,  175,  222,  265,  306,  -53,  285,  334,
 /*    50 */   424,  424,  357,  278,  413,  427,  413,  362,  451,  434,
 /*    60 */   304,  107,  455,  421,  416,  447,  474,  430,  471,  407,
 /*    70 */   488,  488,  488,  488,  488,  488,  488,  488,  488,  488,
 /*    80 */   488,  514,  559,  559,  548,  559,  548,  552,  548,  548,
 /*    90 */   552,  559,  552,  545,  543,  543,  121,  545,  545,  121,
 /*   100 */   543,  549,  121,  543,  545,  545,  582,  545,  121,  121,
 /*   110 */   121,  121,  562,  121,  121,  574,  230,  121,  558,  121,
 /*   120 */   121,  230,  230,  121,  230,  121,  274,   59,   37,  262,
 /*   130 */   217,
);
    static public $yyExpectedTokens = array(
        /* 0 */ array(1, 2, 3, 5, 6, 7, 10, 62, 64, 65, 66, ),
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
        /* 28 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 29 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 30 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 31 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 32 */ array(11, 13, 15, 17, 18, 19, 30, 42, 43, 44, 45, 49, ),
        /* 33 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 34 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 35 */ array(11, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 36 */ array(11, 15, 19, 30, 42, 43, 44, 45, 49, ),
        /* 37 */ array(11, 15, 19, 30, 42, 43, 44, 45, 49, ),
        /* 38 */ array(11, 15, 19, 30, 42, 43, 44, 45, 49, ),
        /* 39 */ array(3, 19, 30, 49, ),
        /* 40 */ array(15, 22, 23, 25, 27, 28, ),
        /* 41 */ array(3, 11, 19, 30, 49, ),
        /* 42 */ array(17, 20, 28, ),
        /* 43 */ array(19, 30, 49, ),
        /* 44 */ array(17, 20, 28, ),
        /* 45 */ array(22, 27, ),
        /* 46 */ array(25, 27, ),
        /* 47 */ array(19, 49, ),
        /* 48 */ array(25, 27, ),
        /* 49 */ array(1, 2, 3, 5, 6, 7, 10, 62, 64, 65, 66, ),
        /* 50 */ array(16, 25, 31, 32, 33, 34, 35, 36, 37, 38, 61, ),
        /* 51 */ array(25, 31, 32, 33, 34, 35, 36, 37, 38, 61, ),
        /* 52 */ array(1, 3, 19, 42, 48, ),
        /* 53 */ array(3, 14, 26, 30, ),
        /* 54 */ array(3, 26, 30, 50, ),
        /* 55 */ array(1, 3, 43, ),
        /* 56 */ array(3, 30, 50, ),
        /* 57 */ array(1, 3, ),
        /* 58 */ array(27, 28, ),
        /* 59 */ array(1, 3, ),
        /* 60 */ array(3, 30, ),
        /* 61 */ array(3, 30, ),
        /* 62 */ array(1, 3, ),
        /* 63 */ array(3, 30, ),
        /* 64 */ array(3, 30, ),
        /* 65 */ array(3, 30, ),
        /* 66 */ array(3, 30, ),
        /* 67 */ array(28, ),
        /* 68 */ array(27, ),
        /* 69 */ array(28, ),
        /* 70 */ array(24, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 71 */ array(4, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 72 */ array(16, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 73 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 74 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 75 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 76 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 77 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 78 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 79 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 80 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 81 */ array(1, 3, 19, 42, 48, ),
        /* 82 */ array(4, 12, 13, 47, ),
        /* 83 */ array(12, 13, 18, 47, ),
        /* 84 */ array(1, 3, 9, ),
        /* 85 */ array(12, 13, 47, ),
        /* 86 */ array(1, 3, 43, ),
        /* 87 */ array(3, 4, 30, ),
        /* 88 */ array(1, 3, 63, ),
        /* 89 */ array(1, 3, 67, ),
        /* 90 */ array(3, 4, 30, ),
        /* 91 */ array(12, 13, 47, ),
        /* 92 */ array(3, 26, 30, ),
        /* 93 */ array(4, 27, ),
        /* 94 */ array(17, 20, ),
        /* 95 */ array(17, 20, ),
        /* 96 */ array(4, 25, ),
        /* 97 */ array(4, 27, ),
        /* 98 */ array(4, 27, ),
        /* 99 */ array(4, 25, ),
        /* 100 */ array(17, 20, ),
        /* 101 */ array(3, 15, ),
        /* 102 */ array(16, 25, ),
        /* 103 */ array(17, 20, ),
        /* 104 */ array(4, 27, ),
        /* 105 */ array(4, 27, ),
        /* 106 */ array(19, 30, ),
        /* 107 */ array(4, 27, ),
        /* 108 */ array(4, 25, ),
        /* 109 */ array(21, 25, ),
        /* 110 */ array(25, 29, ),
        /* 111 */ array(4, 25, ),
        /* 112 */ array(22, ),
        /* 113 */ array(25, ),
        /* 114 */ array(25, ),
        /* 115 */ array(15, ),
        /* 116 */ array(28, ),
        /* 117 */ array(25, ),
        /* 118 */ array(19, ),
        /* 119 */ array(25, ),
        /* 120 */ array(25, ),
        /* 121 */ array(28, ),
        /* 122 */ array(28, ),
        /* 123 */ array(25, ),
        /* 124 */ array(28, ),
        /* 125 */ array(25, ),
        /* 126 */ array(),
        /* 127 */ array(),
        /* 128 */ array(),
        /* 129 */ array(),
        /* 130 */ array(),
        /* 131 */ array(15, 22, 23, 26, ),
        /* 132 */ array(15, 22, 23, 29, ),
        /* 133 */ array(15, 22, 23, ),
        /* 134 */ array(15, 18, 23, ),
        /* 135 */ array(15, 23, ),
        /* 136 */ array(21, 24, ),
        /* 137 */ array(18, 21, ),
        /* 138 */ array(30, 50, ),
        /* 139 */ array(15, 23, ),
        /* 140 */ array(30, ),
        /* 141 */ array(26, ),
        /* 142 */ array(26, ),
        /* 143 */ array(4, ),
        /* 144 */ array(8, ),
        /* 145 */ array(30, ),
        /* 146 */ array(16, ),
        /* 147 */ array(4, ),
        /* 148 */ array(48, ),
        /* 149 */ array(20, ),
        /* 150 */ array(30, ),
        /* 151 */ array(11, ),
        /* 152 */ array(19, ),
        /* 153 */ array(19, ),
        /* 154 */ array(30, ),
        /* 155 */ array(30, ),
        /* 156 */ array(46, ),
        /* 157 */ array(30, ),
        /* 158 */ array(19, ),
        /* 159 */ array(30, ),
        /* 160 */ array(16, ),
        /* 161 */ array(15, ),
        /* 162 */ array(48, ),
        /* 163 */ array(30, ),
        /* 164 */ array(16, ),
        /* 165 */ array(46, ),
        /* 166 */ array(4, ),
        /* 167 */ array(30, ),
        /* 168 */ array(49, ),
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
        /* 242 */ array(),
        /* 243 */ array(),
        /* 244 */ array(),
        /* 245 */ array(),
        /* 246 */ array(),
        /* 247 */ array(),
        /* 248 */ array(),
        /* 249 */ array(),
        /* 250 */ array(),
        /* 251 */ array(),
        /* 252 */ array(),
        /* 253 */ array(),
        /* 254 */ array(),
        /* 255 */ array(),
        /* 256 */ array(),
        /* 257 */ array(),
        /* 258 */ array(),
        /* 259 */ array(),
        /* 260 */ array(),
        /* 261 */ array(),
        /* 262 */ array(),
        /* 263 */ array(),
        /* 264 */ array(),
        /* 265 */ array(),
        /* 266 */ array(),
        /* 267 */ array(),
        /* 268 */ array(),
        /* 269 */ array(),
        /* 270 */ array(),
);
    static public $yy_default = array(
 /*     0 */   417,  417,  417,  417,  417,  417,  417,  417,  417,  417,
 /*    10 */   417,  417,  402,  364,  364,  364,  417,  364,  417,  417,
 /*    20 */   417,  417,  417,  417,  417,  417,  417,  417,  417,  417,
 /*    30 */   417,  417,  417,  417,  417,  417,  417,  417,  417,  417,
 /*    40 */   300,  417,  332,  417,  338,  300,  300,  417,  300,  271,
 /*    50 */   374,  374,  417,  417,  340,  417,  340,  417,  300,  417,
 /*    60 */   417,  417,  417,  417,  417,  417,  417,  327,  300,  328,
 /*    70 */   417,  417,  417,  383,  388,  387,  372,  380,  379,  378,
 /*    80 */   384,  417,  417,  417,  417,  306,  417,  417,  417,  417,
 /*    90 */   417,  369,  417,  417,  356,  358,  417,  417,  417,  417,
 /*   100 */   357,  340,  417,  355,  417,  417,  417,  417,  417,  363,
 /*   110 */   403,  417,  308,  405,  307,  340,  330,  301,  417,  294,
 /*   120 */   375,  329,  333,  304,  352,  404,  340,  340,  368,  340,
 /*   130 */   368,  305,  305,  305,  417,  417,  417,  417,  417,  370,
 /*   140 */   417,  334,  417,  417,  417,  417,  417,  302,  417,  317,
 /*   150 */   417,  417,  417,  417,  417,  417,  309,  417,  417,  417,
 /*   160 */   417,  331,  417,  417,  417,  310,  417,  417,  417,  289,
 /*   170 */   296,  396,  393,  392,  394,  395,  297,  272,  414,  293,
 /*   180 */   382,  381,  413,  398,  397,  386,  274,  275,  290,  373,
 /*   190 */   295,  416,  303,  385,  273,  292,  415,  391,  371,  412,
 /*   200 */   406,  408,  409,  325,  324,  312,  323,  410,  367,  346,
 /*   210 */   345,  279,  315,  314,  313,  354,  321,  320,  339,  336,
 /*   220 */   337,  335,  334,  280,  281,  341,  342,  316,  318,  319,
 /*   230 */   311,  310,  343,  309,  362,  278,  283,  284,  298,  282,
 /*   240 */   348,  350,  351,  299,  285,  376,  377,  389,  291,  288,
 /*   250 */   286,  287,  349,  344,  401,  400,  360,  277,  399,  365,
 /*   260 */   366,  347,  276,  326,  407,  322,  411,  353,  361,  359,
 /*   270 */   390,
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
    const YYNOCODE = 109;
    const YYSTACKDEPTH = 100;
    const YYNSTATE = 271;
    const YYNRULE = 146;
    const YYERRORSYMBOL = 68;
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
    1,  /*    INTEGER => OTHER */
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
    1,  /*         AS => OTHER */
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
    1,  /*       ISIN => OTHER */
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
  'SHORTTAGEND',   'COMMENTEND',    'COMMENTSTART',  'INTEGER',     
  'MATH',          'UNIMATH',       'INCDEC',        'OPENP',       
  'CLOSEP',        'OPENB',         'CLOSEB',        'DOLLAR',      
  'DOT',           'COMMA',         'COLON',         'DOUBLECOLON', 
  'SEMICOLON',     'VERT',          'EQUAL',         'SPACE',       
  'PTR',           'APTR',          'ID',            'EQUALS',      
  'NOTEQUALS',     'GREATERTHAN',   'LESSTHAN',      'GREATEREQUAL',
  'LESSEQUAL',     'IDENTITY',      'NONEIDENTITY',  'NOT',         
  'LAND',          'LOR',           'QUOTE',         'SINGLEQUOTE', 
  'BOOLEAN',       'NULL',          'AS',            'ANDSYM',      
  'BACKTICK',      'HATCH',         'AT',            'ISODD',       
  'ISNOTODD',      'ISEVEN',        'ISNOTEVEN',     'ISODDBY',     
  'ISNOTODDBY',    'ISEVENBY',      'ISNOTEVENBY',   'ISDIVBY',     
  'ISNOTDIVBY',    'ISIN',          'LITERALSTART',  'LITERALEND',  
  'LDELIMTAG',     'RDELIMTAG',     'PHPSTART',      'PHPEND',      
  'error',         'start',         'template',      'template_element',
  'smartytag',     'text',          'variable',      'expr',        
  'attributes',    'varindexed',    'modifier',      'modparameters',
  'ifexprs',       'statement',     'statements',    'varvar',      
  'foraction',     'value',         'array',         'attribute',   
  'exprs',         'math',          'function',      'doublequoted',
  'method',        'params',        'objectchain',   'arrayindex',  
  'object',        'indexdef',      'varvarele',     'objectelement',
  'modparameter',  'ifexpr',        'ifcond',        'lop',         
  'arrayelements',  'arrayelement',  'doublequotedcontent',  'textelement', 
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
 /*  14 */ "smartytag ::= LDEL varindexed EQUAL expr attributes RDEL",
 /*  15 */ "smartytag ::= LDEL ID attributes RDEL",
 /*  16 */ "smartytag ::= LDEL ID PTR ID attributes RDEL",
 /*  17 */ "smartytag ::= LDEL ID modifier modparameters attributes RDEL",
 /*  18 */ "smartytag ::= LDELSLASH ID attributes RDEL",
 /*  19 */ "smartytag ::= LDELSLASH ID PTR ID RDEL",
 /*  20 */ "smartytag ::= LDEL ID SPACE ifexprs RDEL",
 /*  21 */ "smartytag ::= LDEL ID SPACE statement RDEL",
 /*  22 */ "smartytag ::= LDEL ID SPACE statements SEMICOLON ifexprs SEMICOLON DOLLAR varvar foraction RDEL",
 /*  23 */ "foraction ::= EQUAL expr",
 /*  24 */ "foraction ::= INCDEC",
 /*  25 */ "smartytag ::= LDEL ID SPACE value AS DOLLAR varvar RDEL",
 /*  26 */ "smartytag ::= LDEL ID SPACE array AS DOLLAR varvar RDEL",
 /*  27 */ "attributes ::= attributes attribute",
 /*  28 */ "attributes ::= attribute",
 /*  29 */ "attributes ::=",
 /*  30 */ "attribute ::= SPACE ID EQUAL expr",
 /*  31 */ "statements ::= statement",
 /*  32 */ "statements ::= statements COMMA statement",
 /*  33 */ "statement ::= DOLLAR varvar EQUAL expr",
 /*  34 */ "expr ::= ID",
 /*  35 */ "expr ::= exprs",
 /*  36 */ "expr ::= ID COLON expr",
 /*  37 */ "expr ::= expr modifier modparameters",
 /*  38 */ "exprs ::= array",
 /*  39 */ "exprs ::= value",
 /*  40 */ "exprs ::= UNIMATH value",
 /*  41 */ "exprs ::= exprs math value",
 /*  42 */ "exprs ::= exprs ANDSYM value",
 /*  43 */ "math ::= UNIMATH",
 /*  44 */ "math ::= MATH",
 /*  45 */ "value ::= variable",
 /*  46 */ "value ::= INTEGER",
 /*  47 */ "value ::= INTEGER DOT INTEGER",
 /*  48 */ "value ::= BOOLEAN",
 /*  49 */ "value ::= NULL",
 /*  50 */ "value ::= function",
 /*  51 */ "value ::= OPENP expr CLOSEP",
 /*  52 */ "value ::= SINGLEQUOTE text SINGLEQUOTE",
 /*  53 */ "value ::= SINGLEQUOTE SINGLEQUOTE",
 /*  54 */ "value ::= QUOTE doublequoted QUOTE",
 /*  55 */ "value ::= QUOTE QUOTE",
 /*  56 */ "value ::= ID DOUBLECOLON method",
 /*  57 */ "value ::= ID DOUBLECOLON DOLLAR ID OPENP params CLOSEP",
 /*  58 */ "value ::= ID DOUBLECOLON method objectchain",
 /*  59 */ "value ::= ID DOUBLECOLON DOLLAR ID OPENP params CLOSEP objectchain",
 /*  60 */ "value ::= ID DOUBLECOLON ID",
 /*  61 */ "value ::= ID DOUBLECOLON DOLLAR ID arrayindex",
 /*  62 */ "value ::= ID DOUBLECOLON DOLLAR ID arrayindex objectchain",
 /*  63 */ "variable ::= varindexed",
 /*  64 */ "variable ::= DOLLAR varvar AT ID",
 /*  65 */ "variable ::= object",
 /*  66 */ "variable ::= HATCH ID HATCH",
 /*  67 */ "varindexed ::= DOLLAR varvar arrayindex",
 /*  68 */ "arrayindex ::= arrayindex indexdef",
 /*  69 */ "arrayindex ::=",
 /*  70 */ "indexdef ::= DOT ID",
 /*  71 */ "indexdef ::= DOT INTEGER",
 /*  72 */ "indexdef ::= DOT variable",
 /*  73 */ "indexdef ::= DOT LDEL exprs RDEL",
 /*  74 */ "indexdef ::= OPENB ID CLOSEB",
 /*  75 */ "indexdef ::= OPENB exprs CLOSEB",
 /*  76 */ "indexdef ::= OPENB CLOSEB",
 /*  77 */ "varvar ::= varvarele",
 /*  78 */ "varvar ::= varvar varvarele",
 /*  79 */ "varvarele ::= ID",
 /*  80 */ "varvarele ::= LDEL expr RDEL",
 /*  81 */ "object ::= DOLLAR varvar arrayindex objectchain",
 /*  82 */ "objectchain ::= objectelement",
 /*  83 */ "objectchain ::= objectchain objectelement",
 /*  84 */ "objectelement ::= PTR ID arrayindex",
 /*  85 */ "objectelement ::= PTR variable arrayindex",
 /*  86 */ "objectelement ::= PTR LDEL expr RDEL arrayindex",
 /*  87 */ "objectelement ::= PTR ID LDEL expr RDEL arrayindex",
 /*  88 */ "objectelement ::= PTR method",
 /*  89 */ "function ::= ID OPENP params CLOSEP",
 /*  90 */ "method ::= ID OPENP params CLOSEP",
 /*  91 */ "params ::= expr COMMA params",
 /*  92 */ "params ::= expr",
 /*  93 */ "params ::=",
 /*  94 */ "modifier ::= VERT AT ID",
 /*  95 */ "modifier ::= VERT ID",
 /*  96 */ "modparameters ::= modparameters modparameter",
 /*  97 */ "modparameters ::=",
 /*  98 */ "modparameter ::= COLON exprs",
 /*  99 */ "modparameter ::= COLON ID",
 /* 100 */ "ifexprs ::= ifexpr",
 /* 101 */ "ifexprs ::= NOT ifexprs",
 /* 102 */ "ifexprs ::= OPENP ifexprs CLOSEP",
 /* 103 */ "ifexpr ::= expr",
 /* 104 */ "ifexpr ::= expr ifcond expr",
 /* 105 */ "ifexpr ::= expr ISIN array",
 /* 106 */ "ifexpr ::= expr ISIN value",
 /* 107 */ "ifexpr ::= ifexprs lop ifexprs",
 /* 108 */ "ifexpr ::= ifexprs ISDIVBY ifexprs",
 /* 109 */ "ifexpr ::= ifexprs ISNOTDIVBY ifexprs",
 /* 110 */ "ifexpr ::= ifexprs ISEVEN",
 /* 111 */ "ifexpr ::= ifexprs ISNOTEVEN",
 /* 112 */ "ifexpr ::= ifexprs ISEVENBY ifexprs",
 /* 113 */ "ifexpr ::= ifexprs ISNOTEVENBY ifexprs",
 /* 114 */ "ifexpr ::= ifexprs ISODD",
 /* 115 */ "ifexpr ::= ifexprs ISNOTODD",
 /* 116 */ "ifexpr ::= ifexprs ISODDBY ifexprs",
 /* 117 */ "ifexpr ::= ifexprs ISNOTODDBY ifexprs",
 /* 118 */ "ifcond ::= EQUALS",
 /* 119 */ "ifcond ::= NOTEQUALS",
 /* 120 */ "ifcond ::= GREATERTHAN",
 /* 121 */ "ifcond ::= LESSTHAN",
 /* 122 */ "ifcond ::= GREATEREQUAL",
 /* 123 */ "ifcond ::= LESSEQUAL",
 /* 124 */ "ifcond ::= IDENTITY",
 /* 125 */ "ifcond ::= NONEIDENTITY",
 /* 126 */ "lop ::= LAND",
 /* 127 */ "lop ::= LOR",
 /* 128 */ "array ::= OPENB arrayelements CLOSEB",
 /* 129 */ "arrayelements ::= arrayelement",
 /* 130 */ "arrayelements ::= arrayelements COMMA arrayelement",
 /* 131 */ "arrayelements ::=",
 /* 132 */ "arrayelement ::= expr",
 /* 133 */ "arrayelement ::= expr APTR expr",
 /* 134 */ "arrayelement ::= ID APTR expr",
 /* 135 */ "doublequoted ::= doublequoted doublequotedcontent",
 /* 136 */ "doublequoted ::= doublequotedcontent",
 /* 137 */ "doublequotedcontent ::= BACKTICK ID BACKTICK",
 /* 138 */ "doublequotedcontent ::= BACKTICK variable BACKTICK",
 /* 139 */ "doublequotedcontent ::= DOLLAR ID",
 /* 140 */ "doublequotedcontent ::= LDEL expr RDEL",
 /* 141 */ "doublequotedcontent ::= OTHER",
 /* 142 */ "text ::= text textelement",
 /* 143 */ "text ::= textelement",
 /* 144 */ "textelement ::= OTHER",
 /* 145 */ "textelement ::= LDEL",
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
  array( 'lhs' => 69, 'rhs' => 1 ),
  array( 'lhs' => 70, 'rhs' => 1 ),
  array( 'lhs' => 70, 'rhs' => 2 ),
  array( 'lhs' => 71, 'rhs' => 1 ),
  array( 'lhs' => 71, 'rhs' => 3 ),
  array( 'lhs' => 71, 'rhs' => 3 ),
  array( 'lhs' => 71, 'rhs' => 1 ),
  array( 'lhs' => 71, 'rhs' => 1 ),
  array( 'lhs' => 71, 'rhs' => 1 ),
  array( 'lhs' => 71, 'rhs' => 3 ),
  array( 'lhs' => 71, 'rhs' => 3 ),
  array( 'lhs' => 71, 'rhs' => 1 ),
  array( 'lhs' => 71, 'rhs' => 1 ),
  array( 'lhs' => 72, 'rhs' => 4 ),
  array( 'lhs' => 72, 'rhs' => 6 ),
  array( 'lhs' => 72, 'rhs' => 4 ),
  array( 'lhs' => 72, 'rhs' => 6 ),
  array( 'lhs' => 72, 'rhs' => 6 ),
  array( 'lhs' => 72, 'rhs' => 4 ),
  array( 'lhs' => 72, 'rhs' => 5 ),
  array( 'lhs' => 72, 'rhs' => 5 ),
  array( 'lhs' => 72, 'rhs' => 5 ),
  array( 'lhs' => 72, 'rhs' => 11 ),
  array( 'lhs' => 84, 'rhs' => 2 ),
  array( 'lhs' => 84, 'rhs' => 1 ),
  array( 'lhs' => 72, 'rhs' => 8 ),
  array( 'lhs' => 72, 'rhs' => 8 ),
  array( 'lhs' => 76, 'rhs' => 2 ),
  array( 'lhs' => 76, 'rhs' => 1 ),
  array( 'lhs' => 76, 'rhs' => 0 ),
  array( 'lhs' => 87, 'rhs' => 4 ),
  array( 'lhs' => 82, 'rhs' => 1 ),
  array( 'lhs' => 82, 'rhs' => 3 ),
  array( 'lhs' => 81, 'rhs' => 4 ),
  array( 'lhs' => 75, 'rhs' => 1 ),
  array( 'lhs' => 75, 'rhs' => 1 ),
  array( 'lhs' => 75, 'rhs' => 3 ),
  array( 'lhs' => 75, 'rhs' => 3 ),
  array( 'lhs' => 88, 'rhs' => 1 ),
  array( 'lhs' => 88, 'rhs' => 1 ),
  array( 'lhs' => 88, 'rhs' => 2 ),
  array( 'lhs' => 88, 'rhs' => 3 ),
  array( 'lhs' => 88, 'rhs' => 3 ),
  array( 'lhs' => 89, 'rhs' => 1 ),
  array( 'lhs' => 89, 'rhs' => 1 ),
  array( 'lhs' => 85, 'rhs' => 1 ),
  array( 'lhs' => 85, 'rhs' => 1 ),
  array( 'lhs' => 85, 'rhs' => 3 ),
  array( 'lhs' => 85, 'rhs' => 1 ),
  array( 'lhs' => 85, 'rhs' => 1 ),
  array( 'lhs' => 85, 'rhs' => 1 ),
  array( 'lhs' => 85, 'rhs' => 3 ),
  array( 'lhs' => 85, 'rhs' => 3 ),
  array( 'lhs' => 85, 'rhs' => 2 ),
  array( 'lhs' => 85, 'rhs' => 3 ),
  array( 'lhs' => 85, 'rhs' => 2 ),
  array( 'lhs' => 85, 'rhs' => 3 ),
  array( 'lhs' => 85, 'rhs' => 7 ),
  array( 'lhs' => 85, 'rhs' => 4 ),
  array( 'lhs' => 85, 'rhs' => 8 ),
  array( 'lhs' => 85, 'rhs' => 3 ),
  array( 'lhs' => 85, 'rhs' => 5 ),
  array( 'lhs' => 85, 'rhs' => 6 ),
  array( 'lhs' => 74, 'rhs' => 1 ),
  array( 'lhs' => 74, 'rhs' => 4 ),
  array( 'lhs' => 74, 'rhs' => 1 ),
  array( 'lhs' => 74, 'rhs' => 3 ),
  array( 'lhs' => 77, 'rhs' => 3 ),
  array( 'lhs' => 95, 'rhs' => 2 ),
  array( 'lhs' => 95, 'rhs' => 0 ),
  array( 'lhs' => 97, 'rhs' => 2 ),
  array( 'lhs' => 97, 'rhs' => 2 ),
  array( 'lhs' => 97, 'rhs' => 2 ),
  array( 'lhs' => 97, 'rhs' => 4 ),
  array( 'lhs' => 97, 'rhs' => 3 ),
  array( 'lhs' => 97, 'rhs' => 3 ),
  array( 'lhs' => 97, 'rhs' => 2 ),
  array( 'lhs' => 83, 'rhs' => 1 ),
  array( 'lhs' => 83, 'rhs' => 2 ),
  array( 'lhs' => 98, 'rhs' => 1 ),
  array( 'lhs' => 98, 'rhs' => 3 ),
  array( 'lhs' => 96, 'rhs' => 4 ),
  array( 'lhs' => 94, 'rhs' => 1 ),
  array( 'lhs' => 94, 'rhs' => 2 ),
  array( 'lhs' => 99, 'rhs' => 3 ),
  array( 'lhs' => 99, 'rhs' => 3 ),
  array( 'lhs' => 99, 'rhs' => 5 ),
  array( 'lhs' => 99, 'rhs' => 6 ),
  array( 'lhs' => 99, 'rhs' => 2 ),
  array( 'lhs' => 90, 'rhs' => 4 ),
  array( 'lhs' => 92, 'rhs' => 4 ),
  array( 'lhs' => 93, 'rhs' => 3 ),
  array( 'lhs' => 93, 'rhs' => 1 ),
  array( 'lhs' => 93, 'rhs' => 0 ),
  array( 'lhs' => 78, 'rhs' => 3 ),
  array( 'lhs' => 78, 'rhs' => 2 ),
  array( 'lhs' => 79, 'rhs' => 2 ),
  array( 'lhs' => 79, 'rhs' => 0 ),
  array( 'lhs' => 100, 'rhs' => 2 ),
  array( 'lhs' => 100, 'rhs' => 2 ),
  array( 'lhs' => 80, 'rhs' => 1 ),
  array( 'lhs' => 80, 'rhs' => 2 ),
  array( 'lhs' => 80, 'rhs' => 3 ),
  array( 'lhs' => 101, 'rhs' => 1 ),
  array( 'lhs' => 101, 'rhs' => 3 ),
  array( 'lhs' => 101, 'rhs' => 3 ),
  array( 'lhs' => 101, 'rhs' => 3 ),
  array( 'lhs' => 101, 'rhs' => 3 ),
  array( 'lhs' => 101, 'rhs' => 3 ),
  array( 'lhs' => 101, 'rhs' => 3 ),
  array( 'lhs' => 101, 'rhs' => 2 ),
  array( 'lhs' => 101, 'rhs' => 2 ),
  array( 'lhs' => 101, 'rhs' => 3 ),
  array( 'lhs' => 101, 'rhs' => 3 ),
  array( 'lhs' => 101, 'rhs' => 2 ),
  array( 'lhs' => 101, 'rhs' => 2 ),
  array( 'lhs' => 101, 'rhs' => 3 ),
  array( 'lhs' => 101, 'rhs' => 3 ),
  array( 'lhs' => 102, 'rhs' => 1 ),
  array( 'lhs' => 102, 'rhs' => 1 ),
  array( 'lhs' => 102, 'rhs' => 1 ),
  array( 'lhs' => 102, 'rhs' => 1 ),
  array( 'lhs' => 102, 'rhs' => 1 ),
  array( 'lhs' => 102, 'rhs' => 1 ),
  array( 'lhs' => 102, 'rhs' => 1 ),
  array( 'lhs' => 102, 'rhs' => 1 ),
  array( 'lhs' => 103, 'rhs' => 1 ),
  array( 'lhs' => 103, 'rhs' => 1 ),
  array( 'lhs' => 86, 'rhs' => 3 ),
  array( 'lhs' => 104, 'rhs' => 1 ),
  array( 'lhs' => 104, 'rhs' => 3 ),
  array( 'lhs' => 104, 'rhs' => 0 ),
  array( 'lhs' => 105, 'rhs' => 1 ),
  array( 'lhs' => 105, 'rhs' => 3 ),
  array( 'lhs' => 105, 'rhs' => 3 ),
  array( 'lhs' => 91, 'rhs' => 2 ),
  array( 'lhs' => 91, 'rhs' => 1 ),
  array( 'lhs' => 106, 'rhs' => 3 ),
  array( 'lhs' => 106, 'rhs' => 3 ),
  array( 'lhs' => 106, 'rhs' => 2 ),
  array( 'lhs' => 106, 'rhs' => 3 ),
  array( 'lhs' => 106, 'rhs' => 1 ),
  array( 'lhs' => 73, 'rhs' => 2 ),
  array( 'lhs' => 73, 'rhs' => 1 ),
  array( 'lhs' => 107, 'rhs' => 1 ),
  array( 'lhs' => 107, 'rhs' => 1 ),
    );

    /**
     * The following table contains a mapping of reduce action to method name
     * that handles the reduction.
     * 
     * If a rule is not set, it has no handler.
     */
    static public $yyReduceMap = array(
        0 => 0,
        39 => 0,
        45 => 0,
        46 => 0,
        48 => 0,
        49 => 0,
        50 => 0,
        65 => 0,
        129 => 0,
        1 => 1,
        35 => 1,
        38 => 1,
        43 => 1,
        44 => 1,
        77 => 1,
        100 => 1,
        136 => 1,
        143 => 1,
        144 => 1,
        145 => 1,
        2 => 2,
        68 => 2,
        135 => 2,
        142 => 2,
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
        24 => 24,
        28 => 24,
        92 => 24,
        132 => 24,
        25 => 25,
        26 => 26,
        27 => 27,
        29 => 29,
        30 => 30,
        31 => 31,
        32 => 32,
        33 => 33,
        34 => 34,
        36 => 36,
        37 => 37,
        40 => 40,
        41 => 41,
        42 => 42,
        47 => 47,
        51 => 51,
        52 => 52,
        53 => 53,
        55 => 53,
        54 => 54,
        56 => 56,
        57 => 57,
        58 => 58,
        59 => 59,
        60 => 60,
        61 => 61,
        62 => 62,
        63 => 63,
        64 => 64,
        66 => 66,
        67 => 67,
        69 => 69,
        97 => 69,
        70 => 70,
        71 => 71,
        72 => 72,
        73 => 73,
        75 => 73,
        74 => 74,
        76 => 76,
        78 => 78,
        79 => 79,
        80 => 80,
        102 => 80,
        81 => 81,
        82 => 82,
        83 => 83,
        84 => 84,
        85 => 85,
        86 => 86,
        87 => 87,
        88 => 88,
        89 => 89,
        90 => 90,
        91 => 91,
        93 => 93,
        94 => 94,
        95 => 95,
        96 => 96,
        98 => 98,
        99 => 99,
        101 => 101,
        103 => 103,
        104 => 104,
        107 => 104,
        105 => 105,
        106 => 106,
        108 => 108,
        109 => 109,
        110 => 110,
        115 => 110,
        111 => 111,
        114 => 111,
        112 => 112,
        117 => 112,
        113 => 113,
        116 => 113,
        118 => 118,
        119 => 119,
        120 => 120,
        121 => 121,
        122 => 122,
        123 => 123,
        124 => 124,
        125 => 125,
        126 => 126,
        127 => 127,
        128 => 128,
        130 => 130,
        131 => 131,
        133 => 133,
        134 => 134,
        137 => 137,
        138 => 138,
        139 => 139,
        140 => 140,
        141 => 141,
    );
    /* Beginning here are the reduction cases.  A typical example
    ** follows:
    **  #line <lineno> <grammarfile>
    **   function yy_r0($yymsp){ ... }           // User supplied code
    **  #line <lineno> <thisfile>
    */
#line 73 "internal.templateparser.y"
    function yy_r0(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;     }
#line 1824 "internal.templateparser.php"
#line 79 "internal.templateparser.y"
    function yy_r1(){$this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1827 "internal.templateparser.php"
#line 81 "internal.templateparser.y"
    function yy_r2(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1830 "internal.templateparser.php"
#line 87 "internal.templateparser.y"
    function yy_r3(){if ($this->compiler->has_code) {
                                            $tmp =''; foreach ($this->prefix_code as $code) {$tmp.=$code;} $this->prefix_code=array();
                                            $this->_retvalue = $this->cacher->processNocacheCode($tmp.$this->yystack[$this->yyidx + 0]->minor, $this->compiler,$this->nocache,true);
                                         } $this->nocache=false;    }
#line 1836 "internal.templateparser.php"
#line 92 "internal.templateparser.y"
    function yy_r4(){ $this->_retvalue = '';    }
#line 1839 "internal.templateparser.php"
#line 95 "internal.templateparser.y"
    function yy_r5(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + -1]->minor, $this->compiler,false,false);    }
#line 1842 "internal.templateparser.php"
#line 97 "internal.templateparser.y"
    function yy_r6(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->left_delimiter, $this->compiler,false,false);    }
#line 1845 "internal.templateparser.php"
#line 99 "internal.templateparser.y"
    function yy_r7(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->right_delimiter, $this->compiler,false,false);    }
#line 1848 "internal.templateparser.php"
#line 101 "internal.templateparser.y"
    function yy_r8(){if (!$this->template->security) { 
                                       $this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + 0]->minor, $this->compiler, false,true);
                                      } elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_QUOTE) {
                                       $this->_retvalue = $this->cacher->processNocacheCode(htmlspecialchars($this->yystack[$this->yyidx + 0]->minor, ENT_QUOTES), $this->compiler, false, false);
                                      }elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_PASSTHRU || $this->smarty->security_policy->php_handling == SMARTY_PHP_ALLOW) {
                                       $this->_retvalue = $this->cacher->processNocacheCode("<?php echo '".$this->yystack[$this->yyidx + 0]->minor."';?>\n", $this->compiler, false, false);
                                      }elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_REMOVE) {
                                       $this->_retvalue = '';
                                      }	    }
#line 1859 "internal.templateparser.php"
#line 111 "internal.templateparser.y"
    function yy_r9(){if (!$this->template->security) { 
                                        $this->_retvalue = $this->cacher->processNocacheCode('<?php '.$this->yystack[$this->yyidx + -1]->minor.' ?>', $this->compiler, false,true);
                                      } elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_QUOTE) {
                                        $this->_retvalue = $this->cacher->processNocacheCode(htmlspecialchars('<?php '.$this->yystack[$this->yyidx + -1]->minor.' ?>', ENT_QUOTES), $this->compiler, false, false);	
                                      }elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_PASSTHRU || $this->smarty->security_policy->php_handling == SMARTY_PHP_ALLOW) {
                                        $this->_retvalue = $this->cacher->processNocacheCode("<?php echo '<?php ".$this->yystack[$this->yyidx + -1]->minor." ?>';?>\n", $this->compiler, false, false);
                                      }elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_REMOVE) {
                                        $this->_retvalue = '';
                                      }	    }
#line 1870 "internal.templateparser.php"
#line 121 "internal.templateparser.y"
    function yy_r10(){if (!$this->template->security) { 
                                        $this->_retvalue = $this->cacher->processNocacheCode($this->compiler->compileTag('print_expression',array('value'=>$this->yystack[$this->yyidx + -1]->minor)), $this->compiler, false,true);
                                      } elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_QUOTE) {
                                        $this->_retvalue = $this->cacher->processNocacheCode(htmlspecialchars('<?php '.t.' ?>', ENT_QUOTES), $this->compiler, false, false);	
                                      }elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_PASSTHRU || $this->smarty->security_policy->php_handling == SMARTY_PHP_ALLOW) {
                                        $this->_retvalue = $this->cacher->processNocacheCode("<?php echo '<?php ".t." ?>';?>\n", $this->compiler, false, false);
                                      }elseif ($this->smarty->security_policy->php_handling == SMARTY_PHP_REMOVE) {
                                        $this->_retvalue = '';
                                      }	    }
#line 1881 "internal.templateparser.php"
#line 131 "internal.templateparser.y"
    function yy_r11(){$this->_retvalue = $this->cacher->processNocacheCode("<?php echo '".$this->yystack[$this->yyidx + 0]->minor."';?>\n", $this->compiler, true, true);    }
#line 1884 "internal.templateparser.php"
#line 133 "internal.templateparser.y"
    function yy_r12(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + 0]->minor, $this->compiler,false,false);    }
#line 1887 "internal.templateparser.php"
#line 140 "internal.templateparser.y"
    function yy_r13(){ $this->_retvalue = $this->compiler->compileTag('print_expression',array_merge(array('value'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1890 "internal.templateparser.php"
#line 142 "internal.templateparser.y"
    function yy_r14(){ $this->_retvalue = $this->compiler->compileTag('assign',array_merge(array('value'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -4]->minor,$this->yystack[$this->yyidx + -1]->minor));    }
#line 1893 "internal.templateparser.php"
#line 144 "internal.templateparser.y"
    function yy_r15(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor,$this->yystack[$this->yyidx + -1]->minor);    }
#line 1896 "internal.templateparser.php"
#line 146 "internal.templateparser.y"
    function yy_r16(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -4]->minor,array_merge(array('object_methode'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1899 "internal.templateparser.php"
#line 148 "internal.templateparser.y"
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
#line 1914 "internal.templateparser.php"
#line 162 "internal.templateparser.y"
    function yy_r18(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor.'close',$this->yystack[$this->yyidx + -1]->minor);    }
#line 1917 "internal.templateparser.php"
#line 164 "internal.templateparser.y"
    function yy_r19(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor.'close',array('object_methode'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1920 "internal.templateparser.php"
#line 166 "internal.templateparser.y"
    function yy_r20(){if (!in_array($this->yystack[$this->yyidx + -3]->minor,array('if','elseif','while'))) {
                                                            $this->compiler->trigger_template_error ("wrong syntax for tag \"" . $this->yystack[$this->yyidx + -3]->minor . "\""); 
                                                            }
                                                           $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor,array('if condition'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1926 "internal.templateparser.php"
#line 170 "internal.templateparser.y"
    function yy_r21(){ if (!in_array($this->yystack[$this->yyidx + -3]->minor,array('if','elseif','while'))) {
                                                            $this->compiler->trigger_template_error ("wrong syntax for tag \"" . $this->yystack[$this->yyidx + -3]->minor . "\""); 
                                                            }
                                                           $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor,array('if condition'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1932 "internal.templateparser.php"
#line 175 "internal.templateparser.y"
    function yy_r22(){
                                                            if ($this->yystack[$this->yyidx + -9]->minor != 'for') {
                                                               $this->compiler->trigger_template_error ("wrong syntax for tag \"" . $this->yystack[$this->yyidx + -9]->minor . "\""); 
                                                            }
                                                            $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -9]->minor,array('start'=>$this->yystack[$this->yyidx + -7]->minor,'ifexp'=>$this->yystack[$this->yyidx + -5]->minor,'varloop'=>$this->yystack[$this->yyidx + -2]->minor,'loop'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1939 "internal.templateparser.php"
#line 180 "internal.templateparser.y"
    function yy_r23(){ $this->_retvalue = '='.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1942 "internal.templateparser.php"
#line 181 "internal.templateparser.y"
    function yy_r24(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1945 "internal.templateparser.php"
#line 183 "internal.templateparser.y"
    function yy_r25(){
                                                            if ($this->yystack[$this->yyidx + -6]->minor != 'foreach') {
                                                               $this->compiler->trigger_template_error ("wrong syntax for tag \"" . $this->yystack[$this->yyidx + -6]->minor . "\""); 
                                                            }
                                                            $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -6]->minor,array('from'=>$this->yystack[$this->yyidx + -4]->minor,'item'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1952 "internal.templateparser.php"
#line 188 "internal.templateparser.y"
    function yy_r26(){ 
                                                            if ($this->yystack[$this->yyidx + -6]->minor != 'foreach') {
                                                               $this->compiler->trigger_template_error ("wrong syntax for tag \"" . $this->yystack[$this->yyidx + -6]->minor . "\""); 
                                                            }
                                                            $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -6]->minor,array('from'=>$this->yystack[$this->yyidx + -4]->minor,'item'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1959 "internal.templateparser.php"
#line 198 "internal.templateparser.y"
    function yy_r27(){ $this->_retvalue = array_merge($this->yystack[$this->yyidx + -1]->minor,$this->yystack[$this->yyidx + 0]->minor);    }
#line 1962 "internal.templateparser.php"
#line 202 "internal.templateparser.y"
    function yy_r29(){ $this->_retvalue = array();    }
#line 1965 "internal.templateparser.php"
#line 205 "internal.templateparser.y"
    function yy_r30(){ $this->_retvalue = array($this->yystack[$this->yyidx + -2]->minor=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1968 "internal.templateparser.php"
#line 210 "internal.templateparser.y"
    function yy_r31(){ $this->_retvalue = array($this->yystack[$this->yyidx + 0]->minor);    }
#line 1971 "internal.templateparser.php"
#line 211 "internal.templateparser.y"
    function yy_r32(){ $this->yystack[$this->yyidx + -2]->minor[]=$this->yystack[$this->yyidx + 0]->minor; $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor;    }
#line 1974 "internal.templateparser.php"
#line 213 "internal.templateparser.y"
    function yy_r33(){ $this->_retvalue = array('var' => $this->yystack[$this->yyidx + -2]->minor, 'value'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1977 "internal.templateparser.php"
#line 219 "internal.templateparser.y"
    function yy_r34(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';     }
#line 1980 "internal.templateparser.php"
#line 222 "internal.templateparser.y"
    function yy_r36(){$this->_retvalue = '$_smarty_tpl->getStreamVariable(\''. $this->yystack[$this->yyidx + -2]->minor .'://'. trim($this->yystack[$this->yyidx + 0]->minor,"'"). '\')';    }
#line 1983 "internal.templateparser.php"
#line 223 "internal.templateparser.y"
    function yy_r37(){             
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
#line 1998 "internal.templateparser.php"
#line 241 "internal.templateparser.y"
    function yy_r40(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2001 "internal.templateparser.php"
#line 243 "internal.templateparser.y"
    function yy_r41(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor . $this->yystack[$this->yyidx + -1]->minor . $this->yystack[$this->yyidx + 0]->minor;     }
#line 2004 "internal.templateparser.php"
#line 245 "internal.templateparser.y"
    function yy_r42(){ $this->_retvalue = '('. $this->yystack[$this->yyidx + -2]->minor . ').(' . $this->yystack[$this->yyidx + 0]->minor. ')';     }
#line 2007 "internal.templateparser.php"
#line 262 "internal.templateparser.y"
    function yy_r47(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2010 "internal.templateparser.php"
#line 271 "internal.templateparser.y"
    function yy_r51(){ $this->_retvalue = "(". $this->yystack[$this->yyidx + -1]->minor .")";     }
#line 2013 "internal.templateparser.php"
#line 274 "internal.templateparser.y"
    function yy_r52(){ $this->_retvalue = "'".$this->yystack[$this->yyidx + -1]->minor."'";     }
#line 2016 "internal.templateparser.php"
#line 275 "internal.templateparser.y"
    function yy_r53(){ $this->_retvalue = "''";     }
#line 2019 "internal.templateparser.php"
#line 277 "internal.templateparser.y"
    function yy_r54(){ $this->_retvalue = "'".str_replace('\"','"',$this->yystack[$this->yyidx + -1]->minor)."'";     }
#line 2022 "internal.templateparser.php"
#line 281 "internal.templateparser.y"
    function yy_r56(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2025 "internal.templateparser.php"
#line 282 "internal.templateparser.y"
    function yy_r57(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -3]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -6]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -1]->minor .')';     }
#line 2028 "internal.templateparser.php"
#line 284 "internal.templateparser.y"
    function yy_r58(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor.'::'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2031 "internal.templateparser.php"
#line 285 "internal.templateparser.y"
    function yy_r59(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -4]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -7]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -2]->minor .')'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2034 "internal.templateparser.php"
#line 287 "internal.templateparser.y"
    function yy_r60(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2037 "internal.templateparser.php"
#line 289 "internal.templateparser.y"
    function yy_r61(){ $this->_retvalue = $this->yystack[$this->yyidx + -4]->minor.'::$'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2040 "internal.templateparser.php"
#line 291 "internal.templateparser.y"
    function yy_r62(){ $this->_retvalue = $this->yystack[$this->yyidx + -5]->minor.'::$'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2043 "internal.templateparser.php"
#line 298 "internal.templateparser.y"
    function yy_r63(){ if ($this->yystack[$this->yyidx + 0]->minor['var'] == '\'smarty\'') { $this->_retvalue =  $this->compiler->compileTag('internal_smarty_var',$this->yystack[$this->yyidx + 0]->minor['index']);} else {
                                                         $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + 0]->minor['var'] .')->value'.$this->yystack[$this->yyidx + 0]->minor['index']; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + 0]->minor['var'],"'"))->nocache;}    }
#line 2047 "internal.templateparser.php"
#line 301 "internal.templateparser.y"
    function yy_r64(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->'.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 2050 "internal.templateparser.php"
#line 305 "internal.templateparser.y"
    function yy_r66(){$this->_retvalue = '$_smarty_tpl->getConfigVariable(\''. $this->yystack[$this->yyidx + -1]->minor .'\')';    }
#line 2053 "internal.templateparser.php"
#line 308 "internal.templateparser.y"
    function yy_r67(){$this->_retvalue = array('var'=>$this->yystack[$this->yyidx + -1]->minor, 'index'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 2056 "internal.templateparser.php"
#line 316 "internal.templateparser.y"
    function yy_r69(){return;    }
#line 2059 "internal.templateparser.php"
#line 320 "internal.templateparser.y"
    function yy_r70(){ $this->_retvalue = "['". $this->yystack[$this->yyidx + 0]->minor ."']";    }
#line 2062 "internal.templateparser.php"
#line 321 "internal.templateparser.y"
    function yy_r71(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + 0]->minor ."]";    }
#line 2065 "internal.templateparser.php"
#line 322 "internal.templateparser.y"
    function yy_r72(){ $this->_retvalue = "[".$this->yystack[$this->yyidx + 0]->minor."]";    }
#line 2068 "internal.templateparser.php"
#line 323 "internal.templateparser.y"
    function yy_r73(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + -1]->minor ."]";    }
#line 2071 "internal.templateparser.php"
#line 325 "internal.templateparser.y"
    function yy_r74(){ $this->_retvalue = '['.$this->compiler->compileTag('internal_smarty_var','[\'section\'][\''.$this->yystack[$this->yyidx + -1]->minor.'\'][\'index\']').']';    }
#line 2074 "internal.templateparser.php"
#line 329 "internal.templateparser.y"
    function yy_r76(){$this->_retvalue = '';    }
#line 2077 "internal.templateparser.php"
#line 337 "internal.templateparser.y"
    function yy_r78(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2080 "internal.templateparser.php"
#line 339 "internal.templateparser.y"
    function yy_r79(){$this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2083 "internal.templateparser.php"
#line 341 "internal.templateparser.y"
    function yy_r80(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2086 "internal.templateparser.php"
#line 346 "internal.templateparser.y"
    function yy_r81(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->value'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 2089 "internal.templateparser.php"
#line 348 "internal.templateparser.y"
    function yy_r82(){$this->_retvalue  = $this->yystack[$this->yyidx + 0]->minor;     }
#line 2092 "internal.templateparser.php"
#line 350 "internal.templateparser.y"
    function yy_r83(){$this->_retvalue  = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2095 "internal.templateparser.php"
#line 352 "internal.templateparser.y"
    function yy_r84(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2098 "internal.templateparser.php"
#line 353 "internal.templateparser.y"
    function yy_r85(){ $this->_retvalue = '->{'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2101 "internal.templateparser.php"
#line 354 "internal.templateparser.y"
    function yy_r86(){ $this->_retvalue = '->{'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2104 "internal.templateparser.php"
#line 355 "internal.templateparser.y"
    function yy_r87(){ $this->_retvalue = '->{\''.$this->yystack[$this->yyidx + -4]->minor.'\'.'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2107 "internal.templateparser.php"
#line 357 "internal.templateparser.y"
    function yy_r88(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2110 "internal.templateparser.php"
#line 363 "internal.templateparser.y"
    function yy_r89(){if (!$this->template->security || $this->smarty->security_handler->isTrustedPhpFunction($this->yystack[$this->yyidx + -3]->minor, $this->compiler)) {
																					            if ($this->yystack[$this->yyidx + -3]->minor == 'isset' || $this->yystack[$this->yyidx + -3]->minor == 'empty' || $this->yystack[$this->yyidx + -3]->minor == 'array' || is_callable($this->yystack[$this->yyidx + -3]->minor)) {
																					                $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";
																					            } else {
                                                       $this->compiler->trigger_template_error ("unknown function \"" . $this->yystack[$this->yyidx + -3]->minor . "\"");
                                                      }
                                                    }    }
#line 2119 "internal.templateparser.php"
#line 374 "internal.templateparser.y"
    function yy_r90(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";    }
#line 2122 "internal.templateparser.php"
#line 378 "internal.templateparser.y"
    function yy_r91(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.",".$this->yystack[$this->yyidx + 0]->minor;    }
#line 2125 "internal.templateparser.php"
#line 382 "internal.templateparser.y"
    function yy_r93(){ return;    }
#line 2128 "internal.templateparser.php"
#line 387 "internal.templateparser.y"
    function yy_r94(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,true);    }
#line 2131 "internal.templateparser.php"
#line 388 "internal.templateparser.y"
    function yy_r95(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,false);    }
#line 2134 "internal.templateparser.php"
#line 395 "internal.templateparser.y"
    function yy_r96(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2137 "internal.templateparser.php"
#line 399 "internal.templateparser.y"
    function yy_r98(){$this->_retvalue = ','.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2140 "internal.templateparser.php"
#line 400 "internal.templateparser.y"
    function yy_r99(){$this->_retvalue = ',\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2143 "internal.templateparser.php"
#line 407 "internal.templateparser.y"
    function yy_r101(){$this->_retvalue = '!'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2146 "internal.templateparser.php"
#line 412 "internal.templateparser.y"
    function yy_r103(){$this->_retvalue =$this->yystack[$this->yyidx + 0]->minor;    }
#line 2149 "internal.templateparser.php"
#line 413 "internal.templateparser.y"
    function yy_r104(){$this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2152 "internal.templateparser.php"
#line 414 "internal.templateparser.y"
    function yy_r105(){$this->_retvalue = 'in_array('.$this->yystack[$this->yyidx + -2]->minor.','.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2155 "internal.templateparser.php"
#line 415 "internal.templateparser.y"
    function yy_r106(){$this->_retvalue = 'in_array('.$this->yystack[$this->yyidx + -2]->minor.',(array)'.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2158 "internal.templateparser.php"
#line 417 "internal.templateparser.y"
    function yy_r108(){$this->_retvalue = '!('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2161 "internal.templateparser.php"
#line 418 "internal.templateparser.y"
    function yy_r109(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2164 "internal.templateparser.php"
#line 419 "internal.templateparser.y"
    function yy_r110(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2167 "internal.templateparser.php"
#line 420 "internal.templateparser.y"
    function yy_r111(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2170 "internal.templateparser.php"
#line 421 "internal.templateparser.y"
    function yy_r112(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2173 "internal.templateparser.php"
#line 422 "internal.templateparser.y"
    function yy_r113(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2176 "internal.templateparser.php"
#line 428 "internal.templateparser.y"
    function yy_r118(){$this->_retvalue = '==';    }
#line 2179 "internal.templateparser.php"
#line 429 "internal.templateparser.y"
    function yy_r119(){$this->_retvalue = '!=';    }
#line 2182 "internal.templateparser.php"
#line 430 "internal.templateparser.y"
    function yy_r120(){$this->_retvalue = '>';    }
#line 2185 "internal.templateparser.php"
#line 431 "internal.templateparser.y"
    function yy_r121(){$this->_retvalue = '<';    }
#line 2188 "internal.templateparser.php"
#line 432 "internal.templateparser.y"
    function yy_r122(){$this->_retvalue = '>=';    }
#line 2191 "internal.templateparser.php"
#line 433 "internal.templateparser.y"
    function yy_r123(){$this->_retvalue = '<=';    }
#line 2194 "internal.templateparser.php"
#line 434 "internal.templateparser.y"
    function yy_r124(){$this->_retvalue = '===';    }
#line 2197 "internal.templateparser.php"
#line 435 "internal.templateparser.y"
    function yy_r125(){$this->_retvalue = '!==';    }
#line 2200 "internal.templateparser.php"
#line 437 "internal.templateparser.y"
    function yy_r126(){$this->_retvalue = '&&';    }
#line 2203 "internal.templateparser.php"
#line 438 "internal.templateparser.y"
    function yy_r127(){$this->_retvalue = '||';    }
#line 2206 "internal.templateparser.php"
#line 443 "internal.templateparser.y"
    function yy_r128(){ $this->_retvalue = 'array('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2209 "internal.templateparser.php"
#line 445 "internal.templateparser.y"
    function yy_r130(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.','.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2212 "internal.templateparser.php"
#line 446 "internal.templateparser.y"
    function yy_r131(){ return;     }
#line 2215 "internal.templateparser.php"
#line 448 "internal.templateparser.y"
    function yy_r133(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2218 "internal.templateparser.php"
#line 449 "internal.templateparser.y"
    function yy_r134(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + -2]->minor.'\'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2221 "internal.templateparser.php"
#line 456 "internal.templateparser.y"
    function yy_r137(){$this->_retvalue = "`".$this->yystack[$this->yyidx + -1]->minor."`";    }
#line 2224 "internal.templateparser.php"
#line 457 "internal.templateparser.y"
    function yy_r138(){$this->_retvalue = "'.".$this->yystack[$this->yyidx + -1]->minor.".'";    }
#line 2227 "internal.templateparser.php"
#line 458 "internal.templateparser.y"
    function yy_r139(){$this->_retvalue = "'.".'$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + 0]->minor .'\')->value'.".'"; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + 0]->minor,"'"))->nocache;    }
#line 2230 "internal.templateparser.php"
#line 459 "internal.templateparser.y"
    function yy_r140(){$this->_retvalue = "'.(".$this->yystack[$this->yyidx + -1]->minor.").'";    }
#line 2233 "internal.templateparser.php"
#line 460 "internal.templateparser.y"
    function yy_r141(){$this->_retvalue = addcslashes($this->yystack[$this->yyidx + 0]->minor,"'");    }
#line 2236 "internal.templateparser.php"

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
#line 2354 "internal.templateparser.php"
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
#line 2379 "internal.templateparser.php"
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
