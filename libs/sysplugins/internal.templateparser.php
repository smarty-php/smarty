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
    const YY_NO_ACTION = 420;
    const YY_ACCEPT_ACTION = 419;
    const YY_ERROR_ACTION = 418;

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
    const YY_SZ_ACTTAB = 922;
static public $yy_action = array(
 /*     0 */   257,  170,  169,  419,   48,  194,  193,  117,  190,  202,
 /*    10 */   191,  240,  182,  180,  172,  173,   10,    7,    6,   11,
 /*    20 */     8,    9,  126,  165,  111,  204,  202,  143,  240,   36,
 /*    30 */    13,   19,  109,   12,  256,   64,  170,  169,  105,  216,
 /*    40 */   133,  235,    1,  159,  183,   18,  131,  182,  180,  172,
 /*    50 */   173,   10,    7,    6,   11,    8,    9,   16,   51,   55,
 /*    60 */   176,  271,  100,  234,  211,  148,  128,   24,  175,   50,
 /*    70 */   199,  204,   99,  229,  242,   21,   71,  161,  137,   58,
 /*    80 */   163,  158,   26,   89,  133,  252,  241,  262,  170,  169,
 /*    90 */   183,  147,  233,   42,  213,  205,   24,  189,   64,  182,
 /*   100 */   180,  172,  173,   10,    7,    6,   11,    8,    9,  106,
 /*   110 */   217,  162,  215,  138,  228,  190,  226,  191,  241,  262,
 /*   120 */    41,   35,  157,  213,  114,  133,   45,    5,  148,  170,
 /*   130 */   169,  243,  244,  249,  250,  255,  254,  253,  251,   24,
 /*   140 */   182,  180,  172,  173,   10,    7,    6,   11,    8,    9,
 /*   150 */   209,   32,  143,   35,   36,  269,    4,  212,   12,  171,
 /*   160 */    64,   34,   30,  241,  262,   15,  213,   64,  143,  133,
 /*   170 */    36,  136,    4,  190,   12,  191,   56,  190,  181,  191,
 /*   180 */     2,  188,   24,   51,   55,  176,  271,  129,   24,  192,
 /*   190 */   148,  166,   31,   64,  204,   40,    2,  148,   35,   51,
 /*   200 */    55,  176,  271,   38,  164,   27,  148,  219,  143,  213,
 /*   210 */    36,   13,   19,  183,   12,  213,   64,  133,  143,  105,
 /*   220 */    36,   25,   19,  148,   12,   23,   64,  136,  133,  223,
 /*   230 */   167,   22,  265,  224,  206,   47,   13,   39,   59,   51,
 /*   240 */    55,  176,  271,  201,  105,  156,  148,   28,  204,   51,
 /*   250 */    55,  176,  271,  143,   94,   36,  148,   19,  229,   12,
 /*   260 */    21,   64,   13,   33,  121,  232,  242,  183,  150,  143,
 /*   270 */   105,   36,  134,   19,  221,   12,  147,   64,  263,   61,
 /*   280 */   190,   13,  191,  152,   51,   55,  176,  271,  132,  105,
 /*   290 */    62,  148,  208,  210,   63,  227,  215,  150,   17,  266,
 /*   300 */    51,   55,  176,  271,  143,   41,   36,  148,   19,  190,
 /*   310 */    12,  191,   64,  143,  108,  226,   24,   19,  128,   12,
 /*   320 */   218,   64,  267,  130,  133,  260,  242,   43,  184,  103,
 /*   330 */    24,  204,  132,  127,   87,   51,   55,  176,  271,   27,
 /*   340 */   154,  242,  148,  213,   51,   55,  176,  271,  150,  125,
 /*   350 */   183,  148,  175,   50,  225,  204,   31,  213,   38,   40,
 /*   360 */    75,  128,  270,  168,  178,  179,   14,   89,  264,  252,
 /*   370 */   133,  133,  261,  150,  183,  175,   50,  168,  204,  205,
 /*   380 */   124,  175,   50,   76,  204,  150,  196,  178,  179,   79,
 /*   390 */    89,  150,  252,  178,  179,  150,   89,  183,  252,   88,
 /*   400 */    64,  133,  205,  183,   85,   84,  263,   24,  205,  150,
 /*   410 */   175,   50,  133,  204,  104,   53,  150,  140,   77,  197,
 /*   420 */   193,  248,  178,  179,  128,   89,  242,  252,  175,   50,
 /*   430 */   148,  204,  183,  198,  144,  225,   69,  205,  198,  198,
 /*   440 */   178,  179,   90,   89,  120,  252,   20,   80,  240,   33,
 /*   450 */   183,  175,   50,   93,  204,  205,  119,  175,   50,   72,
 /*   460 */   204,  230,   86,  178,  179,   73,   89,  160,  252,  178,
 /*   470 */   179,   54,   89,  183,  252,   83,  198,  175,  205,  183,
 /*   480 */   204,  145,  225,  116,  205,  175,   49,  240,  204,  178,
 /*   490 */   179,  225,   82,   70,  252,  225,   98,  178,  179,  183,
 /*   500 */    89,   52,  252,  175,   50,  151,  204,  183,  242,   29,
 /*   510 */    28,   78,  205,  177,  231,  178,  179,    3,   89,  142,
 /*   520 */   252,  225,  259,  141,   65,  183,  175,   92,   16,  204,
 /*   530 */   205,  195,  175,   50,  149,  204,   66,   60,  178,  179,
 /*   540 */    74,   89,  200,  252,  178,  179,  214,   89,  183,  252,
 /*   550 */   186,  239,   68,  203,  183,  133,  135,  236,  185,  205,
 /*   560 */   187,  243,  244,  249,  250,  255,  254,  253,  251,  143,
 /*   570 */   220,   37,  215,   19,  150,  175,  222,   64,  204,  202,
 /*   580 */    57,  256,  268,  207,   67,  256,   96,  174,  132,  256,
 /*   590 */   256,   34,  252,  175,  101,  256,  204,  183,  256,  256,
 /*   600 */    51,   55,  176,  271,  256,  178,  179,  148,   89,  256,
 /*   610 */   252,  256,  256,  153,  256,  183,  175,  101,  256,  204,
 /*   620 */   256,  256,  175,  101,  256,  204,  256,  256,  178,  179,
 /*   630 */   256,   89,  256,  252,  178,  179,  155,   89,  183,  252,
 /*   640 */   175,  101,  238,  204,  183,  256,  256,  256,  256,  256,
 /*   650 */   256,  256,  178,  179,  256,   89,  256,  252,  256,  256,
 /*   660 */   146,  256,  183,  175,   92,  256,  204,  256,  256,  175,
 /*   670 */   123,  256,  204,  256,  256,  178,  179,  256,   89,  256,
 /*   680 */   252,  178,  179,  256,   89,  183,  252,  256,  256,  256,
 /*   690 */   256,  183,  175,   95,  237,  204,  256,  256,  175,  122,
 /*   700 */   256,  204,  256,  256,  178,  179,  256,   89,  256,  252,
 /*   710 */   178,  179,  256,   89,  183,  252,  175,  112,  256,  204,
 /*   720 */   183,  256,  256,  256,  256,  256,  256,  256,  178,  179,
 /*   730 */   256,   89,  256,  252,  256,  256,  256,  256,  183,  175,
 /*   740 */   102,  256,  204,  256,  256,  175,   97,  256,  204,  256,
 /*   750 */   256,  178,  179,  256,   89,  256,  252,  178,  179,  256,
 /*   760 */    89,  183,  252,  256,  256,  256,  256,  183,  175,  107,
 /*   770 */   256,  204,  256,  256,  175,   44,  256,  204,  256,  256,
 /*   780 */   178,  179,  256,   89,  256,  252,  178,  179,  256,   89,
 /*   790 */   183,  252,  175,  113,  256,  204,  183,  256,  256,  256,
 /*   800 */   256,  256,  256,  256,  178,  179,  256,   89,  256,  252,
 /*   810 */   256,  256,  256,  256,  183,  175,  110,  256,  204,  256,
 /*   820 */   256,  175,  115,  256,  204,  256,  256,  178,  179,  256,
 /*   830 */    89,  256,  252,  178,  179,  256,   89,  183,  252,  256,
 /*   840 */   256,  256,  256,  183,  175,   46,  256,  139,  256,  256,
 /*   850 */   175,  118,  175,  204,  256,  204,  178,  179,  256,   89,
 /*   860 */   256,  252,  178,  179,  258,   89,  183,  252,  175,  252,
 /*   870 */   256,  204,  183,  256,  183,  256,  256,  256,  256,  175,
 /*   880 */   178,  179,  204,   91,  256,  252,  256,  256,  256,  256,
 /*   890 */   183,  178,  179,  256,   81,  256,  252,  175,  256,  175,
 /*   900 */   204,  183,  204,  256,  256,  256,  256,  256,  256,  245,
 /*   910 */   246,  247,  256,  256,  252,  256,  252,  256,  256,  183,
 /*   920 */   256,  183,
    );
    static public $yy_lookahead = array(
 /*     0 */     4,   40,   41,   69,   70,   71,   72,   95,    1,   97,
 /*    10 */     3,   99,   51,   52,   53,   54,   55,   56,   57,   58,
 /*    20 */    59,   60,   74,   46,   95,   77,   97,   11,   99,   13,
 /*    30 */    15,   15,   79,   17,   18,   19,   40,   41,   23,   30,
 /*    40 */    25,   93,   27,   28,   96,    3,   30,   51,   52,   53,
 /*    50 */    54,   55,   56,   57,   58,   59,   60,   15,   42,   43,
 /*    60 */    44,   45,   76,    4,   16,   49,   80,    3,   74,   75,
 /*    70 */    63,   77,   79,    1,   88,    3,   82,   83,   84,   30,
 /*    80 */    86,   87,    3,   89,   25,   91,   12,   13,   40,   41,
 /*    90 */    96,   19,   18,   79,   30,  101,    3,    4,   19,   51,
 /*   100 */    52,   53,   54,   55,   56,   57,   58,   59,   60,   30,
 /*   110 */     4,   19,   98,   24,   42,    1,   16,    3,   12,   13,
 /*   120 */    48,   47,   30,   30,   21,   25,   81,   24,   49,   40,
 /*   130 */    41,   31,   32,   33,   34,   35,   36,   37,   38,    3,
 /*   140 */    51,   52,   53,   54,   55,   56,   57,   58,   59,   60,
 /*   150 */    14,    3,   11,   47,   13,   48,   15,   43,   17,   11,
 /*   160 */    19,   61,   26,   12,   13,   21,   30,   19,   11,   25,
 /*   170 */    13,   30,   15,    1,   17,    3,   19,    1,   30,    3,
 /*   180 */    39,    9,    3,   42,   43,   44,   45,   30,    3,    4,
 /*   190 */    49,   74,   17,   19,   77,   20,   39,   49,   47,   42,
 /*   200 */    43,   44,   45,   28,   30,   26,   49,    4,   11,   30,
 /*   210 */    13,   15,   15,   96,   17,   30,   19,   25,   11,   23,
 /*   220 */    13,   29,   15,   49,   17,   29,   19,   30,   25,    1,
 /*   230 */     2,    3,   48,    5,    6,    7,   15,   30,   10,   42,
 /*   240 */    43,   44,   45,   67,   23,   74,   49,   26,   77,   42,
 /*   250 */    43,   44,   45,   11,   76,   13,   49,   15,    1,   17,
 /*   260 */     3,   19,   15,   22,   81,   18,   88,   96,   27,   11,
 /*   270 */    23,   13,   30,   15,    4,   17,   19,   19,  100,   19,
 /*   280 */     1,   15,    3,   85,   42,   43,   44,   45,   30,   23,
 /*   290 */    62,   49,   64,   65,   66,   18,   98,   27,   21,   42,
 /*   300 */    42,   43,   44,   45,   11,   48,   13,   49,   15,    1,
 /*   310 */    17,    3,   19,   11,   76,   16,    3,   15,   80,   17,
 /*   320 */    30,   19,   43,   30,   25,    4,   88,   79,   74,   76,
 /*   330 */     3,   77,   30,   80,   78,   42,   43,   44,   45,   26,
 /*   340 */    50,   88,   49,   30,   42,   43,   44,   45,   27,    4,
 /*   350 */    96,   49,   74,   75,   98,   77,   17,   30,   28,   20,
 /*   360 */    82,   80,    4,   50,   86,   87,   15,   89,    4,   91,
 /*   370 */    25,   25,    4,   27,   96,   74,   75,   50,   77,  101,
 /*   380 */     4,   74,   75,   82,   77,   27,    4,   86,   87,   82,
 /*   390 */    89,   27,   91,   86,   87,   27,   89,   96,   91,   73,
 /*   400 */    19,   25,  101,   96,   73,   73,  100,    3,  101,   27,
 /*   410 */    74,   75,   25,   77,   76,   78,   27,   28,   82,   71,
 /*   420 */    72,   99,   86,   87,   80,   89,   88,   91,   74,   75,
 /*   430 */    49,   77,   96,  107,   30,   98,   82,  101,  107,  107,
 /*   440 */    86,   87,   73,   89,   95,   91,  102,   92,   99,   22,
 /*   450 */    96,   74,   75,   79,   77,  101,   30,   74,   75,   82,
 /*   460 */    77,  106,   78,   86,   87,   82,   89,   46,   91,   86,
 /*   470 */    87,   78,   89,   96,   91,   78,  107,   74,  101,   96,
 /*   480 */    77,   30,   98,   95,  101,   74,   75,   99,   77,   86,
 /*   490 */    87,   98,   89,   82,   91,   98,   76,   86,   87,   96,
 /*   500 */    89,   78,   91,   74,   75,   30,   77,   96,   88,   26,
 /*   510 */    26,   82,  101,   11,   16,   86,   87,  103,   89,   22,
 /*   520 */    91,   98,   30,   20,   19,   96,   74,   75,   15,   77,
 /*   530 */   101,    4,   74,   75,   30,   77,   30,   19,   86,   87,
 /*   540 */    82,   89,    8,   91,   86,   87,   30,   89,   96,   91,
 /*   550 */     4,   16,   16,    4,   96,   25,  104,  105,   49,  101,
 /*   560 */   107,   31,   32,   33,   34,   35,   36,   37,   38,   11,
 /*   570 */    30,   90,   98,   15,   27,   74,   88,   19,   77,   97,
 /*   580 */    19,  108,  106,   83,   93,  108,   79,   86,   30,  108,
 /*   590 */   108,   61,   91,   74,   75,  108,   77,   96,  108,  108,
 /*   600 */    42,   43,   44,   45,  108,   86,   87,   49,   89,  108,
 /*   610 */    91,  108,  108,   94,  108,   96,   74,   75,  108,   77,
 /*   620 */   108,  108,   74,   75,  108,   77,  108,  108,   86,   87,
 /*   630 */   108,   89,  108,   91,   86,   87,   94,   89,   96,   91,
 /*   640 */    74,   75,   94,   77,   96,  108,  108,  108,  108,  108,
 /*   650 */   108,  108,   86,   87,  108,   89,  108,   91,  108,  108,
 /*   660 */    94,  108,   96,   74,   75,  108,   77,  108,  108,   74,
 /*   670 */    75,  108,   77,  108,  108,   86,   87,  108,   89,  108,
 /*   680 */    91,   86,   87,  108,   89,   96,   91,  108,  108,  108,
 /*   690 */   108,   96,   74,   75,  105,   77,  108,  108,   74,   75,
 /*   700 */   108,   77,  108,  108,   86,   87,  108,   89,  108,   91,
 /*   710 */    86,   87,  108,   89,   96,   91,   74,   75,  108,   77,
 /*   720 */    96,  108,  108,  108,  108,  108,  108,  108,   86,   87,
 /*   730 */   108,   89,  108,   91,  108,  108,  108,  108,   96,   74,
 /*   740 */    75,  108,   77,  108,  108,   74,   75,  108,   77,  108,
 /*   750 */   108,   86,   87,  108,   89,  108,   91,   86,   87,  108,
 /*   760 */    89,   96,   91,  108,  108,  108,  108,   96,   74,   75,
 /*   770 */   108,   77,  108,  108,   74,   75,  108,   77,  108,  108,
 /*   780 */    86,   87,  108,   89,  108,   91,   86,   87,  108,   89,
 /*   790 */    96,   91,   74,   75,  108,   77,   96,  108,  108,  108,
 /*   800 */   108,  108,  108,  108,   86,   87,  108,   89,  108,   91,
 /*   810 */   108,  108,  108,  108,   96,   74,   75,  108,   77,  108,
 /*   820 */   108,   74,   75,  108,   77,  108,  108,   86,   87,  108,
 /*   830 */    89,  108,   91,   86,   87,  108,   89,   96,   91,  108,
 /*   840 */   108,  108,  108,   96,   74,   75,  108,   77,  108,  108,
 /*   850 */    74,   75,   74,   77,  108,   77,   86,   87,  108,   89,
 /*   860 */   108,   91,   86,   87,   86,   89,   96,   91,   74,   91,
 /*   870 */   108,   77,   96,  108,   96,  108,  108,  108,  108,   74,
 /*   880 */    86,   87,   77,   89,  108,   91,  108,  108,  108,  108,
 /*   890 */    96,   86,   87,  108,   89,  108,   91,   74,  108,   74,
 /*   900 */    77,   96,   77,  108,  108,  108,  108,  108,  108,   86,
 /*   910 */    87,   86,  108,  108,   91,  108,   91,  108,  108,   96,
 /*   920 */   108,   96,
);
    const YY_SHIFT_USE_DFLT = -40;
    const YY_SHIFT_MAX = 168;
    static public $yy_shift_ofst = array(
 /*     0 */   228,  157,  141,  141,  141,  141,  141,  141,  141,  141,
 /*    10 */   141,  141,  293,  197,  197,  197,  197,  293,  197,  197,
 /*    20 */   197,  197,  207,  197,  197,  197,  197,  197,  197,  197,
 /*    30 */   197,   16,  258,  242,  302,  558,  558,  558,   79,   15,
 /*    40 */   148,  174,  175,  175,  346,  241,  346,  381,  228,  100,
 /*    50 */   530,   72,  136,  313,  327,  279,  404,   64,  389,  308,
 /*    60 */    64,   64,  308,  308,  404,   64,  547,  330,  330,   89,
 /*    70 */    48,   -4,  -39,  -39,  -39,  -39,  -39,  -39,  -39,  -39,
 /*    80 */   257,   74,  106,   93,    7,  176,  179,  185,  172,  151,
 /*    90 */   114,  151,  192,  339,  358,  299,  339,  376,  364,  339,
 /*   100 */   368,  144,  345,  321,  382,   92,   42,   59,  270,  339,
 /*   110 */   203,  330,  387,  387,  561,  387,  330,  330,  387,  351,
 /*   120 */   330,  427,  387,  387,  -40,  -40,  -40,  -40,  -40,  221,
 /*   130 */   196,  247,  266,  290,  266,  277,  266,  103,  505,  483,
 /*   140 */   451,  502,  540,  503,  497,  527,  498,  492,  475,  484,
 /*   150 */   504,  509,  546,  536,  516,  535,  534,  513,  421,  506,
 /*   160 */   518,  549,  426,  -23,  107,  260,  184,   49,    9,
);
    const YY_REDUCE_USE_DFLT = -89;
    const YY_REDUCE_MAX = 128;
    static public $yy_reduce_ofst = array(
 /*     0 */   -66,   -6,  383,  429,  411,  354,  307,  458,  336,  301,
 /*    10 */   278,  377,  452,  542,  519,  548,  566,  589,  665,  618,
 /*    20 */   642,  694,  770,  624,  741,  595,  671,  776,  747,  700,
 /*    30 */   718,  805,  403,  794,  823,  778,  501,  825,  -52,  253,
 /*    40 */   254,  117,  -71,  -88,  -14,  178,  238,  171,  348,  344,
 /*    50 */   344,  355,  198,   14,   14,  369,  337,  384,  338,  326,
 /*    60 */   256,  397,  332,  331,  393,  423,  420,  349,  388,  414,
 /*    70 */   414,  414,  414,  414,  414,  414,  414,  414,  414,  414,
 /*    80 */   476,  481,  481,  474,  453,  453,  474,  474,  453,  481,
 /*    90 */   453,  481,  281,  482,  488,  281,  482,  281,  488,  482,
 /*   100 */   488,  281,  281,  488,  488,  491,  507,  281,  488,  482,
 /*   110 */   281,  322,  281,  281,  500,  281,  322,  322,  281,  248,
 /*   120 */   322,  306,  281,  281,  374,   -7,  -47,   45,  183,
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
        /* 31 */ array(11, 13, 15, 17, 18, 19, 30, 42, 43, 44, 45, 49, ),
        /* 32 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 33 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 34 */ array(11, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 35 */ array(11, 15, 19, 30, 42, 43, 44, 45, 49, ),
        /* 36 */ array(11, 15, 19, 30, 42, 43, 44, 45, 49, ),
        /* 37 */ array(11, 15, 19, 30, 42, 43, 44, 45, 49, ),
        /* 38 */ array(3, 19, 30, 49, ),
        /* 39 */ array(15, 23, 25, 27, 28, ),
        /* 40 */ array(3, 11, 19, 30, 49, ),
        /* 41 */ array(19, 30, 49, ),
        /* 42 */ array(17, 20, 28, ),
        /* 43 */ array(17, 20, 28, ),
        /* 44 */ array(25, 27, ),
        /* 45 */ array(22, 27, ),
        /* 46 */ array(25, 27, ),
        /* 47 */ array(19, 49, ),
        /* 48 */ array(1, 2, 3, 5, 6, 7, 10, 62, 64, 65, 66, ),
        /* 49 */ array(16, 25, 31, 32, 33, 34, 35, 36, 37, 38, 61, ),
        /* 50 */ array(25, 31, 32, 33, 34, 35, 36, 37, 38, 61, ),
        /* 51 */ array(1, 3, 19, 42, 48, ),
        /* 52 */ array(3, 14, 26, 30, ),
        /* 53 */ array(3, 26, 30, 50, ),
        /* 54 */ array(3, 30, 50, ),
        /* 55 */ array(1, 3, 43, ),
        /* 56 */ array(3, 30, ),
        /* 57 */ array(3, 30, ),
        /* 58 */ array(27, 28, ),
        /* 59 */ array(1, 3, ),
        /* 60 */ array(3, 30, ),
        /* 61 */ array(3, 30, ),
        /* 62 */ array(1, 3, ),
        /* 63 */ array(1, 3, ),
        /* 64 */ array(3, 30, ),
        /* 65 */ array(3, 30, ),
        /* 66 */ array(27, ),
        /* 67 */ array(28, ),
        /* 68 */ array(28, ),
        /* 69 */ array(24, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 70 */ array(16, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 71 */ array(4, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 72 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 73 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 74 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 75 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 76 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 77 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 78 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 79 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 80 */ array(1, 3, 19, 42, 48, ),
        /* 81 */ array(12, 13, 18, 47, ),
        /* 82 */ array(4, 12, 13, 47, ),
        /* 83 */ array(3, 4, 30, ),
        /* 84 */ array(1, 3, 63, ),
        /* 85 */ array(1, 3, 67, ),
        /* 86 */ array(3, 26, 30, ),
        /* 87 */ array(3, 4, 30, ),
        /* 88 */ array(1, 3, 9, ),
        /* 89 */ array(12, 13, 47, ),
        /* 90 */ array(1, 3, 43, ),
        /* 91 */ array(12, 13, 47, ),
        /* 92 */ array(25, 29, ),
        /* 93 */ array(17, 20, ),
        /* 94 */ array(4, 27, ),
        /* 95 */ array(16, 25, ),
        /* 96 */ array(17, 20, ),
        /* 97 */ array(4, 25, ),
        /* 98 */ array(4, 27, ),
        /* 99 */ array(17, 20, ),
        /* 100 */ array(4, 27, ),
        /* 101 */ array(21, 25, ),
        /* 102 */ array(4, 25, ),
        /* 103 */ array(4, 27, ),
        /* 104 */ array(4, 27, ),
        /* 105 */ array(19, 30, ),
        /* 106 */ array(3, 15, ),
        /* 107 */ array(4, 25, ),
        /* 108 */ array(4, 27, ),
        /* 109 */ array(17, 20, ),
        /* 110 */ array(4, 25, ),
        /* 111 */ array(28, ),
        /* 112 */ array(25, ),
        /* 113 */ array(25, ),
        /* 114 */ array(19, ),
        /* 115 */ array(25, ),
        /* 116 */ array(28, ),
        /* 117 */ array(28, ),
        /* 118 */ array(25, ),
        /* 119 */ array(15, ),
        /* 120 */ array(28, ),
        /* 121 */ array(22, ),
        /* 122 */ array(25, ),
        /* 123 */ array(25, ),
        /* 124 */ array(),
        /* 125 */ array(),
        /* 126 */ array(),
        /* 127 */ array(),
        /* 128 */ array(),
        /* 129 */ array(15, 23, 26, ),
        /* 130 */ array(15, 23, 29, ),
        /* 131 */ array(15, 18, 23, ),
        /* 132 */ array(15, 23, ),
        /* 133 */ array(30, 50, ),
        /* 134 */ array(15, 23, ),
        /* 135 */ array(18, 21, ),
        /* 136 */ array(15, 23, ),
        /* 137 */ array(21, 24, ),
        /* 138 */ array(19, ),
        /* 139 */ array(26, ),
        /* 140 */ array(30, ),
        /* 141 */ array(11, ),
        /* 142 */ array(30, ),
        /* 143 */ array(20, ),
        /* 144 */ array(22, ),
        /* 145 */ array(4, ),
        /* 146 */ array(16, ),
        /* 147 */ array(30, ),
        /* 148 */ array(30, ),
        /* 149 */ array(26, ),
        /* 150 */ array(30, ),
        /* 151 */ array(49, ),
        /* 152 */ array(4, ),
        /* 153 */ array(16, ),
        /* 154 */ array(30, ),
        /* 155 */ array(16, ),
        /* 156 */ array(8, ),
        /* 157 */ array(15, ),
        /* 158 */ array(46, ),
        /* 159 */ array(30, ),
        /* 160 */ array(19, ),
        /* 161 */ array(4, ),
        /* 162 */ array(30, ),
        /* 163 */ array(46, ),
        /* 164 */ array(48, ),
        /* 165 */ array(19, ),
        /* 166 */ array(48, ),
        /* 167 */ array(30, ),
        /* 168 */ array(30, ),
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
        /* 271 */ array(),
);
    static public $yy_default = array(
 /*     0 */   418,  418,  418,  418,  418,  418,  418,  418,  418,  418,
 /*    10 */   418,  418,  403,  365,  365,  365,  365,  418,  418,  418,
 /*    20 */   418,  418,  418,  418,  418,  418,  418,  418,  418,  418,
 /*    30 */   418,  418,  418,  418,  418,  418,  418,  418,  418,  302,
 /*    40 */   418,  418,  287,  333,  302,  302,  302,  418,  272,  375,
 /*    50 */   375,  418,  418,  341,  341,  418,  418,  418,  302,  418,
 /*    60 */   418,  418,  418,  418,  418,  418,  302,  328,  329,  418,
 /*    70 */   418,  418,  385,  373,  389,  388,  381,  380,  379,  384,
 /*    80 */   418,  418,  418,  418,  418,  418,  418,  418,  418,  308,
 /*    90 */   418,  370,  404,  358,  418,  418,  356,  418,  418,  359,
 /*   100 */   418,  364,  418,  418,  418,  418,  341,  418,  418,  357,
 /*   110 */   418,  353,  376,  296,  418,  303,  331,  334,  306,  341,
 /*   120 */   330,  309,  406,  405,  341,  341,  341,  369,  369,  307,
 /*   130 */   307,  418,  418,  418,  371,  418,  307,  418,  418,  335,
 /*   140 */   418,  418,  418,  318,  351,  418,  418,  418,  418,  418,
 /*   150 */   418,  418,  418,  418,  418,  418,  418,  332,  310,  418,
 /*   160 */   418,  304,  418,  311,  418,  418,  418,  418,  418,  399,
 /*   170 */   398,  343,  382,  383,  312,  317,  320,  319,  311,  310,
 /*   180 */   387,  342,  386,  337,  344,  338,  295,  414,  276,  298,
 /*   190 */   416,  417,  299,  275,  273,  292,  291,  274,  415,  277,
 /*   200 */   282,  281,  340,  294,  335,  372,  280,  305,  278,  297,
 /*   210 */   279,  374,  324,  351,  366,  350,  336,  345,  367,  352,
 /*   220 */   339,  285,  300,  284,  283,  349,  323,  400,  327,  413,
 /*   230 */   408,  362,  346,  347,  412,  360,  401,  402,  363,  361,
 /*   240 */   354,  316,  301,  390,  391,  378,  377,  313,  355,  392,
 /*   250 */   393,  397,  322,  396,  395,  394,  348,  293,  314,  411,
 /*   260 */   288,  286,  315,  368,  289,  410,  326,  325,  407,  409,
 /*   270 */   290,  321,
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
    const YYNSTATE = 272;
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
  'attributes',    'varindexed',    'varvar',        'arrayindex',  
  'modifier',      'modparameters',  'ifexprs',       'statement',   
  'statements',    'foraction',     'value',         'array',       
  'attribute',     'exprs',         'math',          'function',    
  'doublequoted',  'method',        'params',        'objectchain', 
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
 /*  15 */ "varindexed ::= DOLLAR varvar arrayindex",
 /*  16 */ "smartytag ::= LDEL ID attributes RDEL",
 /*  17 */ "smartytag ::= LDEL ID PTR ID attributes RDEL",
 /*  18 */ "smartytag ::= LDEL ID modifier modparameters attributes RDEL",
 /*  19 */ "smartytag ::= LDELSLASH ID attributes RDEL",
 /*  20 */ "smartytag ::= LDELSLASH ID PTR ID RDEL",
 /*  21 */ "smartytag ::= LDEL ID SPACE ifexprs RDEL",
 /*  22 */ "smartytag ::= LDEL ID SPACE statement RDEL",
 /*  23 */ "smartytag ::= LDEL ID SPACE statements SEMICOLON ifexprs SEMICOLON DOLLAR varvar foraction RDEL",
 /*  24 */ "foraction ::= EQUAL expr",
 /*  25 */ "foraction ::= INCDEC",
 /*  26 */ "smartytag ::= LDEL ID SPACE value AS DOLLAR varvar RDEL",
 /*  27 */ "smartytag ::= LDEL ID SPACE array AS DOLLAR varvar RDEL",
 /*  28 */ "attributes ::= attributes attribute",
 /*  29 */ "attributes ::= attribute",
 /*  30 */ "attributes ::=",
 /*  31 */ "attribute ::= SPACE ID EQUAL expr",
 /*  32 */ "statements ::= statement",
 /*  33 */ "statements ::= statements COMMA statement",
 /*  34 */ "statement ::= DOLLAR varvar EQUAL expr",
 /*  35 */ "expr ::= ID",
 /*  36 */ "expr ::= exprs",
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
 /*  67 */ "variable ::= DOLLAR ID COLON ID",
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
  array( 'lhs' => 77, 'rhs' => 3 ),
  array( 'lhs' => 72, 'rhs' => 4 ),
  array( 'lhs' => 72, 'rhs' => 6 ),
  array( 'lhs' => 72, 'rhs' => 6 ),
  array( 'lhs' => 72, 'rhs' => 4 ),
  array( 'lhs' => 72, 'rhs' => 5 ),
  array( 'lhs' => 72, 'rhs' => 5 ),
  array( 'lhs' => 72, 'rhs' => 5 ),
  array( 'lhs' => 72, 'rhs' => 11 ),
  array( 'lhs' => 85, 'rhs' => 2 ),
  array( 'lhs' => 85, 'rhs' => 1 ),
  array( 'lhs' => 72, 'rhs' => 8 ),
  array( 'lhs' => 72, 'rhs' => 8 ),
  array( 'lhs' => 76, 'rhs' => 2 ),
  array( 'lhs' => 76, 'rhs' => 1 ),
  array( 'lhs' => 76, 'rhs' => 0 ),
  array( 'lhs' => 88, 'rhs' => 4 ),
  array( 'lhs' => 84, 'rhs' => 1 ),
  array( 'lhs' => 84, 'rhs' => 3 ),
  array( 'lhs' => 83, 'rhs' => 4 ),
  array( 'lhs' => 75, 'rhs' => 1 ),
  array( 'lhs' => 75, 'rhs' => 1 ),
  array( 'lhs' => 75, 'rhs' => 3 ),
  array( 'lhs' => 89, 'rhs' => 1 ),
  array( 'lhs' => 89, 'rhs' => 1 ),
  array( 'lhs' => 89, 'rhs' => 2 ),
  array( 'lhs' => 89, 'rhs' => 3 ),
  array( 'lhs' => 89, 'rhs' => 3 ),
  array( 'lhs' => 90, 'rhs' => 1 ),
  array( 'lhs' => 90, 'rhs' => 1 ),
  array( 'lhs' => 86, 'rhs' => 1 ),
  array( 'lhs' => 86, 'rhs' => 1 ),
  array( 'lhs' => 86, 'rhs' => 3 ),
  array( 'lhs' => 86, 'rhs' => 1 ),
  array( 'lhs' => 86, 'rhs' => 1 ),
  array( 'lhs' => 86, 'rhs' => 1 ),
  array( 'lhs' => 86, 'rhs' => 3 ),
  array( 'lhs' => 86, 'rhs' => 3 ),
  array( 'lhs' => 86, 'rhs' => 2 ),
  array( 'lhs' => 86, 'rhs' => 3 ),
  array( 'lhs' => 86, 'rhs' => 2 ),
  array( 'lhs' => 86, 'rhs' => 3 ),
  array( 'lhs' => 86, 'rhs' => 7 ),
  array( 'lhs' => 86, 'rhs' => 4 ),
  array( 'lhs' => 86, 'rhs' => 8 ),
  array( 'lhs' => 86, 'rhs' => 3 ),
  array( 'lhs' => 86, 'rhs' => 5 ),
  array( 'lhs' => 86, 'rhs' => 6 ),
  array( 'lhs' => 74, 'rhs' => 1 ),
  array( 'lhs' => 74, 'rhs' => 4 ),
  array( 'lhs' => 74, 'rhs' => 1 ),
  array( 'lhs' => 74, 'rhs' => 3 ),
  array( 'lhs' => 74, 'rhs' => 4 ),
  array( 'lhs' => 79, 'rhs' => 2 ),
  array( 'lhs' => 79, 'rhs' => 0 ),
  array( 'lhs' => 97, 'rhs' => 2 ),
  array( 'lhs' => 97, 'rhs' => 2 ),
  array( 'lhs' => 97, 'rhs' => 2 ),
  array( 'lhs' => 97, 'rhs' => 4 ),
  array( 'lhs' => 97, 'rhs' => 3 ),
  array( 'lhs' => 97, 'rhs' => 3 ),
  array( 'lhs' => 97, 'rhs' => 2 ),
  array( 'lhs' => 78, 'rhs' => 1 ),
  array( 'lhs' => 78, 'rhs' => 2 ),
  array( 'lhs' => 98, 'rhs' => 1 ),
  array( 'lhs' => 98, 'rhs' => 3 ),
  array( 'lhs' => 96, 'rhs' => 4 ),
  array( 'lhs' => 95, 'rhs' => 1 ),
  array( 'lhs' => 95, 'rhs' => 2 ),
  array( 'lhs' => 99, 'rhs' => 3 ),
  array( 'lhs' => 99, 'rhs' => 3 ),
  array( 'lhs' => 99, 'rhs' => 5 ),
  array( 'lhs' => 99, 'rhs' => 6 ),
  array( 'lhs' => 99, 'rhs' => 2 ),
  array( 'lhs' => 91, 'rhs' => 4 ),
  array( 'lhs' => 93, 'rhs' => 4 ),
  array( 'lhs' => 94, 'rhs' => 3 ),
  array( 'lhs' => 94, 'rhs' => 1 ),
  array( 'lhs' => 94, 'rhs' => 0 ),
  array( 'lhs' => 80, 'rhs' => 3 ),
  array( 'lhs' => 80, 'rhs' => 2 ),
  array( 'lhs' => 81, 'rhs' => 2 ),
  array( 'lhs' => 81, 'rhs' => 0 ),
  array( 'lhs' => 100, 'rhs' => 2 ),
  array( 'lhs' => 100, 'rhs' => 2 ),
  array( 'lhs' => 82, 'rhs' => 1 ),
  array( 'lhs' => 82, 'rhs' => 2 ),
  array( 'lhs' => 82, 'rhs' => 3 ),
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
  array( 'lhs' => 87, 'rhs' => 3 ),
  array( 'lhs' => 104, 'rhs' => 1 ),
  array( 'lhs' => 104, 'rhs' => 3 ),
  array( 'lhs' => 104, 'rhs' => 0 ),
  array( 'lhs' => 105, 'rhs' => 1 ),
  array( 'lhs' => 105, 'rhs' => 3 ),
  array( 'lhs' => 105, 'rhs' => 3 ),
  array( 'lhs' => 92, 'rhs' => 2 ),
  array( 'lhs' => 92, 'rhs' => 1 ),
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
        36 => 1,
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
        22 => 21,
        23 => 23,
        24 => 24,
        25 => 25,
        29 => 25,
        92 => 25,
        132 => 25,
        26 => 26,
        27 => 26,
        28 => 28,
        30 => 30,
        31 => 31,
        32 => 32,
        33 => 33,
        34 => 34,
        35 => 35,
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
#line 1832 "internal.templateparser.php"
#line 79 "internal.templateparser.y"
    function yy_r1(){$this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1835 "internal.templateparser.php"
#line 81 "internal.templateparser.y"
    function yy_r2(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1838 "internal.templateparser.php"
#line 87 "internal.templateparser.y"
    function yy_r3(){if ($this->compiler->has_code) {
                                            $tmp =''; foreach ($this->prefix_code as $code) {$tmp.=$code;} $this->prefix_code=array();
                                            $this->_retvalue = $this->cacher->processNocacheCode($tmp.$this->yystack[$this->yyidx + 0]->minor, $this->compiler,$this->nocache,true);
                                         } $this->nocache=false;    }
#line 1844 "internal.templateparser.php"
#line 100 "internal.templateparser.y"
    function yy_r4(){ $this->_retvalue = '';    }
#line 1847 "internal.templateparser.php"
#line 103 "internal.templateparser.y"
    function yy_r5(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + -1]->minor, $this->compiler,false,false);    }
#line 1850 "internal.templateparser.php"
#line 105 "internal.templateparser.y"
    function yy_r6(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->left_delimiter, $this->compiler,false,false);    }
#line 1853 "internal.templateparser.php"
#line 107 "internal.templateparser.y"
    function yy_r7(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->right_delimiter, $this->compiler,false,false);    }
#line 1856 "internal.templateparser.php"
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
#line 1867 "internal.templateparser.php"
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
#line 1878 "internal.templateparser.php"
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
#line 1889 "internal.templateparser.php"
#line 139 "internal.templateparser.y"
    function yy_r11(){$this->_retvalue = $this->cacher->processNocacheCode("<?php echo '".$this->yystack[$this->yyidx + 0]->minor."';?>\n", $this->compiler, true, true);    }
#line 1892 "internal.templateparser.php"
#line 141 "internal.templateparser.y"
    function yy_r12(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + 0]->minor, $this->compiler,false,false);    }
#line 1895 "internal.templateparser.php"
#line 149 "internal.templateparser.y"
    function yy_r13(){ $this->_retvalue = $this->compiler->compileTag('print_expression',array_merge(array('value'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1898 "internal.templateparser.php"
#line 151 "internal.templateparser.y"
    function yy_r14(){ $this->_retvalue = $this->compiler->compileTag('assign',array_merge(array('value'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -4]->minor,$this->yystack[$this->yyidx + -1]->minor));    }
#line 1901 "internal.templateparser.php"
#line 152 "internal.templateparser.y"
    function yy_r15(){$this->_retvalue = array('var'=>$this->yystack[$this->yyidx + -1]->minor, 'index'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1904 "internal.templateparser.php"
#line 155 "internal.templateparser.y"
    function yy_r16(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor,$this->yystack[$this->yyidx + -1]->minor);    }
#line 1907 "internal.templateparser.php"
#line 157 "internal.templateparser.y"
    function yy_r17(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -4]->minor,array_merge(array('object_methode'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1910 "internal.templateparser.php"
#line 159 "internal.templateparser.y"
    function yy_r18(){ $this->_retvalue =  '<?php ob_start();?>'.$this->compiler->compileTag($this->yystack[$this->yyidx + -4]->minor,$this->yystack[$this->yyidx + -1]->minor).'<?php echo ';
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
#line 1925 "internal.templateparser.php"
#line 173 "internal.templateparser.y"
    function yy_r19(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor.'close',$this->yystack[$this->yyidx + -1]->minor);    }
#line 1928 "internal.templateparser.php"
#line 175 "internal.templateparser.y"
    function yy_r20(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor.'close',array('object_methode'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1931 "internal.templateparser.php"
#line 177 "internal.templateparser.y"
    function yy_r21(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor,array('if condition'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1934 "internal.templateparser.php"
#line 180 "internal.templateparser.y"
    function yy_r23(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -9]->minor,array('start'=>$this->yystack[$this->yyidx + -7]->minor,'ifexp'=>$this->yystack[$this->yyidx + -5]->minor,'varloop'=>$this->yystack[$this->yyidx + -2]->minor,'loop'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1937 "internal.templateparser.php"
#line 181 "internal.templateparser.y"
    function yy_r24(){ $this->_retvalue = '='.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1940 "internal.templateparser.php"
#line 182 "internal.templateparser.y"
    function yy_r25(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1943 "internal.templateparser.php"
#line 185 "internal.templateparser.y"
    function yy_r26(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -6]->minor,array('from'=>$this->yystack[$this->yyidx + -4]->minor,'item'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1946 "internal.templateparser.php"
#line 192 "internal.templateparser.y"
    function yy_r28(){ $this->_retvalue = array_merge($this->yystack[$this->yyidx + -1]->minor,$this->yystack[$this->yyidx + 0]->minor);    }
#line 1949 "internal.templateparser.php"
#line 196 "internal.templateparser.y"
    function yy_r30(){ $this->_retvalue = array();    }
#line 1952 "internal.templateparser.php"
#line 200 "internal.templateparser.y"
    function yy_r31(){ $this->_retvalue = array($this->yystack[$this->yyidx + -2]->minor=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1955 "internal.templateparser.php"
#line 205 "internal.templateparser.y"
    function yy_r32(){ $this->_retvalue = array($this->yystack[$this->yyidx + 0]->minor);    }
#line 1958 "internal.templateparser.php"
#line 206 "internal.templateparser.y"
    function yy_r33(){ $this->yystack[$this->yyidx + -2]->minor[]=$this->yystack[$this->yyidx + 0]->minor; $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor;    }
#line 1961 "internal.templateparser.php"
#line 208 "internal.templateparser.y"
    function yy_r34(){ $this->_retvalue = array('var' => $this->yystack[$this->yyidx + -2]->minor, 'value'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1964 "internal.templateparser.php"
#line 215 "internal.templateparser.y"
    function yy_r35(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';     }
#line 1967 "internal.templateparser.php"
#line 219 "internal.templateparser.y"
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
#line 1982 "internal.templateparser.php"
#line 237 "internal.templateparser.y"
    function yy_r40(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1985 "internal.templateparser.php"
#line 239 "internal.templateparser.y"
    function yy_r41(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor . $this->yystack[$this->yyidx + -1]->minor . $this->yystack[$this->yyidx + 0]->minor;     }
#line 1988 "internal.templateparser.php"
#line 241 "internal.templateparser.y"
    function yy_r42(){ $this->_retvalue = '('. $this->yystack[$this->yyidx + -2]->minor . ').(' . $this->yystack[$this->yyidx + 0]->minor. ')';     }
#line 1991 "internal.templateparser.php"
#line 258 "internal.templateparser.y"
    function yy_r47(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1994 "internal.templateparser.php"
#line 267 "internal.templateparser.y"
    function yy_r51(){ $this->_retvalue = "(". $this->yystack[$this->yyidx + -1]->minor .")";     }
#line 1997 "internal.templateparser.php"
#line 270 "internal.templateparser.y"
    function yy_r52(){ $this->_retvalue = "'".$this->yystack[$this->yyidx + -1]->minor."'";     }
#line 2000 "internal.templateparser.php"
#line 271 "internal.templateparser.y"
    function yy_r53(){ $this->_retvalue = "''";     }
#line 2003 "internal.templateparser.php"
#line 273 "internal.templateparser.y"
    function yy_r54(){ $this->_retvalue = "'".str_replace('\"','"',$this->yystack[$this->yyidx + -1]->minor)."'";     }
#line 2006 "internal.templateparser.php"
#line 279 "internal.templateparser.y"
    function yy_r56(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2009 "internal.templateparser.php"
#line 280 "internal.templateparser.y"
    function yy_r57(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -3]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -6]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -1]->minor .')';     }
#line 2012 "internal.templateparser.php"
#line 282 "internal.templateparser.y"
    function yy_r58(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor.'::'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2015 "internal.templateparser.php"
#line 283 "internal.templateparser.y"
    function yy_r59(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -4]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -7]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -2]->minor .')'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2018 "internal.templateparser.php"
#line 285 "internal.templateparser.y"
    function yy_r60(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2021 "internal.templateparser.php"
#line 287 "internal.templateparser.y"
    function yy_r61(){ $this->_retvalue = $this->yystack[$this->yyidx + -4]->minor.'::$'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2024 "internal.templateparser.php"
#line 289 "internal.templateparser.y"
    function yy_r62(){ $this->_retvalue = $this->yystack[$this->yyidx + -5]->minor.'::$'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2027 "internal.templateparser.php"
#line 296 "internal.templateparser.y"
    function yy_r63(){ if ($this->yystack[$this->yyidx + 0]->minor['var'] == '\'smarty\'') { $this->_retvalue =  $this->compiler->compileTag('internal_smarty_var',$this->yystack[$this->yyidx + 0]->minor['index']);} else {
                                                         $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + 0]->minor['var'] .')->value'.$this->yystack[$this->yyidx + 0]->minor['index']; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + 0]->minor['var'],"'"))->nocache;}    }
#line 2031 "internal.templateparser.php"
#line 299 "internal.templateparser.y"
    function yy_r64(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->'.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 2034 "internal.templateparser.php"
#line 303 "internal.templateparser.y"
    function yy_r66(){$this->_retvalue = '$_smarty_tpl->getConfigVariable(\''. $this->yystack[$this->yyidx + -1]->minor .'\')';    }
#line 2037 "internal.templateparser.php"
#line 305 "internal.templateparser.y"
    function yy_r67(){$this->_retvalue = '$_smarty_tpl->getStreamVariable(\''. $this->yystack[$this->yyidx + -2]->minor .'://'. $this->yystack[$this->yyidx + 0]->minor. '\')';    }
#line 2040 "internal.templateparser.php"
#line 313 "internal.templateparser.y"
    function yy_r69(){return;    }
#line 2043 "internal.templateparser.php"
#line 317 "internal.templateparser.y"
    function yy_r70(){ $this->_retvalue = "['". $this->yystack[$this->yyidx + 0]->minor ."']";    }
#line 2046 "internal.templateparser.php"
#line 318 "internal.templateparser.y"
    function yy_r71(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + 0]->minor ."]";    }
#line 2049 "internal.templateparser.php"
#line 320 "internal.templateparser.y"
    function yy_r72(){ $this->_retvalue = "[".$this->yystack[$this->yyidx + 0]->minor."]";    }
#line 2052 "internal.templateparser.php"
#line 321 "internal.templateparser.y"
    function yy_r73(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + -1]->minor ."]";    }
#line 2055 "internal.templateparser.php"
#line 323 "internal.templateparser.y"
    function yy_r74(){ $this->_retvalue = '['.$this->compiler->compileTag('internal_smarty_var','[\'section\'][\''.$this->yystack[$this->yyidx + -1]->minor.'\'][\'index\']').']';    }
#line 2058 "internal.templateparser.php"
#line 327 "internal.templateparser.y"
    function yy_r76(){$this->_retvalue = '';    }
#line 2061 "internal.templateparser.php"
#line 335 "internal.templateparser.y"
    function yy_r78(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2064 "internal.templateparser.php"
#line 337 "internal.templateparser.y"
    function yy_r79(){$this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2067 "internal.templateparser.php"
#line 339 "internal.templateparser.y"
    function yy_r80(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2070 "internal.templateparser.php"
#line 344 "internal.templateparser.y"
    function yy_r81(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->value'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 2073 "internal.templateparser.php"
#line 346 "internal.templateparser.y"
    function yy_r82(){$this->_retvalue  = $this->yystack[$this->yyidx + 0]->minor;     }
#line 2076 "internal.templateparser.php"
#line 348 "internal.templateparser.y"
    function yy_r83(){$this->_retvalue  = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2079 "internal.templateparser.php"
#line 350 "internal.templateparser.y"
    function yy_r84(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2082 "internal.templateparser.php"
#line 351 "internal.templateparser.y"
    function yy_r85(){ $this->_retvalue = '->{'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2085 "internal.templateparser.php"
#line 352 "internal.templateparser.y"
    function yy_r86(){ $this->_retvalue = '->{'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2088 "internal.templateparser.php"
#line 353 "internal.templateparser.y"
    function yy_r87(){ $this->_retvalue = '->{\''.$this->yystack[$this->yyidx + -4]->minor.'\'.'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2091 "internal.templateparser.php"
#line 355 "internal.templateparser.y"
    function yy_r88(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2094 "internal.templateparser.php"
#line 361 "internal.templateparser.y"
    function yy_r89(){if (!$this->template->security || $this->smarty->security_handler->isTrustedPhpFunction($this->yystack[$this->yyidx + -3]->minor, $this->compiler)) {
																					            if ($this->yystack[$this->yyidx + -3]->minor == 'isset' || $this->yystack[$this->yyidx + -3]->minor == 'empty' || $this->yystack[$this->yyidx + -3]->minor == 'array' || is_callable($this->yystack[$this->yyidx + -3]->minor)) {
																					                $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";
																					            } else {
                                                       $this->compiler->trigger_template_error ("unknown function \"" . $this->yystack[$this->yyidx + -3]->minor . "\"");
                                                      }
                                                    }    }
#line 2103 "internal.templateparser.php"
#line 372 "internal.templateparser.y"
    function yy_r90(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";    }
#line 2106 "internal.templateparser.php"
#line 376 "internal.templateparser.y"
    function yy_r91(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.",".$this->yystack[$this->yyidx + 0]->minor;    }
#line 2109 "internal.templateparser.php"
#line 380 "internal.templateparser.y"
    function yy_r93(){ return;    }
#line 2112 "internal.templateparser.php"
#line 385 "internal.templateparser.y"
    function yy_r94(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,true);    }
#line 2115 "internal.templateparser.php"
#line 386 "internal.templateparser.y"
    function yy_r95(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,false);    }
#line 2118 "internal.templateparser.php"
#line 393 "internal.templateparser.y"
    function yy_r96(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2121 "internal.templateparser.php"
#line 397 "internal.templateparser.y"
    function yy_r98(){$this->_retvalue = ','.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2124 "internal.templateparser.php"
#line 398 "internal.templateparser.y"
    function yy_r99(){$this->_retvalue = ',\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2127 "internal.templateparser.php"
#line 405 "internal.templateparser.y"
    function yy_r101(){$this->_retvalue = '!'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2130 "internal.templateparser.php"
#line 410 "internal.templateparser.y"
    function yy_r103(){$this->_retvalue =$this->yystack[$this->yyidx + 0]->minor;    }
#line 2133 "internal.templateparser.php"
#line 411 "internal.templateparser.y"
    function yy_r104(){$this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2136 "internal.templateparser.php"
#line 412 "internal.templateparser.y"
    function yy_r105(){$this->_retvalue = 'in_array('.$this->yystack[$this->yyidx + -2]->minor.','.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2139 "internal.templateparser.php"
#line 413 "internal.templateparser.y"
    function yy_r106(){$this->_retvalue = 'in_array('.$this->yystack[$this->yyidx + -2]->minor.',(array)'.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2142 "internal.templateparser.php"
#line 415 "internal.templateparser.y"
    function yy_r108(){$this->_retvalue = '!('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2145 "internal.templateparser.php"
#line 416 "internal.templateparser.y"
    function yy_r109(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2148 "internal.templateparser.php"
#line 417 "internal.templateparser.y"
    function yy_r110(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2151 "internal.templateparser.php"
#line 418 "internal.templateparser.y"
    function yy_r111(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2154 "internal.templateparser.php"
#line 419 "internal.templateparser.y"
    function yy_r112(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2157 "internal.templateparser.php"
#line 420 "internal.templateparser.y"
    function yy_r113(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2160 "internal.templateparser.php"
#line 426 "internal.templateparser.y"
    function yy_r118(){$this->_retvalue = '==';    }
#line 2163 "internal.templateparser.php"
#line 427 "internal.templateparser.y"
    function yy_r119(){$this->_retvalue = '!=';    }
#line 2166 "internal.templateparser.php"
#line 428 "internal.templateparser.y"
    function yy_r120(){$this->_retvalue = '>';    }
#line 2169 "internal.templateparser.php"
#line 429 "internal.templateparser.y"
    function yy_r121(){$this->_retvalue = '<';    }
#line 2172 "internal.templateparser.php"
#line 430 "internal.templateparser.y"
    function yy_r122(){$this->_retvalue = '>=';    }
#line 2175 "internal.templateparser.php"
#line 431 "internal.templateparser.y"
    function yy_r123(){$this->_retvalue = '<=';    }
#line 2178 "internal.templateparser.php"
#line 432 "internal.templateparser.y"
    function yy_r124(){$this->_retvalue = '===';    }
#line 2181 "internal.templateparser.php"
#line 433 "internal.templateparser.y"
    function yy_r125(){$this->_retvalue = '!==';    }
#line 2184 "internal.templateparser.php"
#line 435 "internal.templateparser.y"
    function yy_r126(){$this->_retvalue = '&&';    }
#line 2187 "internal.templateparser.php"
#line 436 "internal.templateparser.y"
    function yy_r127(){$this->_retvalue = '||';    }
#line 2190 "internal.templateparser.php"
#line 441 "internal.templateparser.y"
    function yy_r128(){ $this->_retvalue = 'array('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2193 "internal.templateparser.php"
#line 443 "internal.templateparser.y"
    function yy_r130(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.','.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2196 "internal.templateparser.php"
#line 444 "internal.templateparser.y"
    function yy_r131(){ return;     }
#line 2199 "internal.templateparser.php"
#line 446 "internal.templateparser.y"
    function yy_r133(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2202 "internal.templateparser.php"
#line 447 "internal.templateparser.y"
    function yy_r134(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + -2]->minor.'\'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2205 "internal.templateparser.php"
#line 454 "internal.templateparser.y"
    function yy_r137(){$this->_retvalue = "`".$this->yystack[$this->yyidx + -1]->minor."`";    }
#line 2208 "internal.templateparser.php"
#line 455 "internal.templateparser.y"
    function yy_r138(){$this->_retvalue = "'.".$this->yystack[$this->yyidx + -1]->minor.".'";    }
#line 2211 "internal.templateparser.php"
#line 456 "internal.templateparser.y"
    function yy_r139(){$this->_retvalue = "'.".'$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + 0]->minor .'\')->value'.".'"; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + 0]->minor,"'"))->nocache;    }
#line 2214 "internal.templateparser.php"
#line 457 "internal.templateparser.y"
    function yy_r140(){$this->_retvalue = "'.(".$this->yystack[$this->yyidx + -1]->minor.").'";    }
#line 2217 "internal.templateparser.php"
#line 458 "internal.templateparser.y"
    function yy_r141(){$this->_retvalue = addcslashes($this->yystack[$this->yyidx + 0]->minor,"'");    }
#line 2220 "internal.templateparser.php"

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
#line 2338 "internal.templateparser.php"
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
#line 2363 "internal.templateparser.php"
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
