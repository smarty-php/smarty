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
    const YY_NO_ACTION = 403;
    const YY_ACCEPT_ACTION = 402;
    const YY_ERROR_ACTION = 401;

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
    const YY_SZ_ACTTAB = 863;
static public $yy_action = array(
 /*     0 */   238,  201,  200,  121,  221,  223,  202,  107,  133,  192,
 /*    10 */   226,  218,  195,  193,  163,  162,    9,    7,    6,    8,
 /*    20 */     2,    4,  220,  138,   15,  177,  202,  143,  133,   36,
 /*    30 */    16,   21,  120,   12,  228,   63,  201,  200,  105,   37,
 /*    40 */   133,  250,    1,  146,  202,  177,  126,  195,  193,  163,
 /*    50 */   162,    9,    7,    6,    8,    2,    4,  225,   38,   56,
 /*    60 */   237,  232,   30,  177,  191,  147,  133,   11,  167,   52,
 /*    70 */   194,  202,  189,  249,   13,  167,   70,  130,  202,  175,
 /*    80 */   174,   31,  199,   83,   41,  235,  145,  144,  201,  200,
 /*    90 */   177,   24,  235,  221,  223,  176,  194,  177,  189,  195,
 /*   100 */   193,  163,  162,    9,    7,    6,    8,    2,    4,   23,
 /*   110 */    65,  254,  251,  139,   19,  215,  151,   29,  206,  210,
 /*   120 */   198,   47,  219,  108,   61,   63,    3,   63,   37,  201,
 /*   130 */   200,  143,  133,   36,   34,   21,  103,   12,  156,   63,
 /*   140 */   195,  193,  163,  162,    9,    7,    6,    8,    2,    4,
 /*   150 */   128,  194,  143,  189,   36,  147,   10,  147,   12,  190,
 /*   160 */    63,  197,   38,   56,  237,  232,   13,  122,  143,  147,
 /*   170 */    36,  128,   10,  208,   12,   59,   57,  179,  182,   64,
 /*   180 */     5,   97,   24,   38,   56,  237,  232,  127,  133,  250,
 /*   190 */   147,  173,  202,  188,  202,  217,    5,  100,   24,   38,
 /*   200 */    56,  237,  232,  221,  223,   25,  147,   39,  143,  206,
 /*   210 */    36,  177,   21,  177,   12,  114,   63,  192,  143,  218,
 /*   220 */    36,  216,   21,  229,   12,  206,   63,  128,  143,  194,
 /*   230 */    36,  189,   21,  194,   12,  189,   63,   42,   37,   38,
 /*   240 */    56,  237,  232,  157,   94,  156,  147,  129,  120,   38,
 /*   250 */    56,  237,  232,  153,   24,  231,  147,   63,  124,   38,
 /*   260 */    56,  237,  232,  143,  155,   36,  147,   21,  134,   12,
 /*   270 */   109,   63,  143,   40,   36,  247,   21,   19,   12,  133,
 /*   280 */    63,  206,  132,  402,   50,  171,  178,  147,   95,   99,
 /*   290 */   184,  125,  120,  123,   38,   56,  237,  232,   43,  231,
 /*   300 */   231,  147,   98,   38,   56,  237,  232,   16,  167,   51,
 /*   310 */   147,  202,   85,  231,  252,  105,   69,  207,   22,  175,
 /*   320 */   174,  167,   52,   83,  202,  235,  224,  159,  178,   72,
 /*   330 */   177,  149,  175,  174,  202,  176,   83,  150,  235,  167,
 /*   340 */   133,  143,  202,  177,   28,   21,  185,   12,  176,   63,
 /*   350 */   172,   16,   90,  177,  167,   52,  235,  202,  245,  105,
 /*   360 */   129,  177,   73,  231,  253,  175,  174,   14,  256,   83,
 /*   370 */    26,  235,   38,   56,  237,  232,  177,  167,   52,  147,
 /*   380 */   202,  176,  205,  211,   88,   78,   63,   48,  175,  174,
 /*   390 */   167,   52,   83,  202,  235,  150,  154,  167,   75,  177,
 /*   400 */   202,  175,  174,  133,  176,   83,  150,  235,  204,  248,
 /*   410 */   167,   52,  177,  202,  235,   45,  147,  176,   77,  177,
 /*   420 */   222,  175,  174,  167,   52,   83,  202,  235,   16,   55,
 /*   430 */   167,   74,  177,  202,  175,  174,  105,  176,   83,  255,
 /*   440 */   235,  257,   27,  167,   52,  177,  202,  235,   84,  209,
 /*   450 */   176,   76,  177,   53,  175,  174,   31,  243,   83,   41,
 /*   460 */   235,  246,  150,  167,   52,  177,  202,   40,  209,   54,
 /*   470 */   176,   68,  142,  209,  175,  174,   32,  225,   83,   16,
 /*   480 */   235,  150,  227,  244,  150,  177,  133,  105,  120,  209,
 /*   490 */   176,  164,  241,  239,  240,  258,  183,  170,  169,  168,
 /*   500 */   167,   52,  194,  202,  189,  143,  150,   24,   71,   21,
 /*   510 */    18,  175,  174,   63,  150,   83,  115,  235,  167,  102,
 /*   520 */   218,  202,  177,  133,  129,  150,  165,  176,   58,  175,
 /*   530 */   174,   86,   82,   83,  206,  235,   38,   56,  237,  232,
 /*   540 */   177,  110,  196,  147,  133,  218,   87,   91,  131,  242,
 /*   550 */   241,  239,  240,  258,  183,  170,  169,  168,  231,  167,
 /*   560 */   102,  187,  202,  135,  214,  185,  185,  203,  213,  234,
 /*   570 */   175,  174,  167,  104,   83,  202,  235,  207,   62,  166,
 /*   580 */   185,  177,  112,  175,  174,  167,  104,   83,  202,  235,
 /*   590 */   186,  136,  148,   22,  177,  236,  175,  174,  167,  104,
 /*   600 */    83,  202,  235,   66,  140,  158,  152,  177,   20,  175,
 /*   610 */   174,  167,  104,   83,  202,  235,  181,  150,  141,  192,
 /*   620 */   177,  212,  175,  174,  167,  101,   83,  202,  235,   35,
 /*   630 */   207,  233,   44,  177,   67,  175,  174,  106,   60,   83,
 /*   640 */   180,  235,   17,   32,  167,   92,  177,  202,  224,  251,
 /*   650 */   251,  251,  251,  251,  251,  175,  174,  167,  117,   83,
 /*   660 */   202,  235,  251,  251,  251,  251,  177,  251,  175,  174,
 /*   670 */   167,  113,   83,  202,  235,  256,  251,   26,  251,  177,
 /*   680 */   251,  175,  174,  167,   93,   83,  202,  235,  251,  251,
 /*   690 */   251,  251,  177,   63,  175,  174,  167,   49,   83,  202,
 /*   700 */   235,  251,  251,  251,  251,  177,  251,  175,  174,  167,
 /*   710 */   119,   83,  202,  235,  251,  251,  230,  251,  177,  251,
 /*   720 */   175,  174,   45,  147,   83,  251,  235,  251,  251,  167,
 /*   730 */    96,  177,  202,  251,  251,  251,  251,  251,  251,  251,
 /*   740 */   175,  174,  167,  111,   83,  202,  235,  251,  251,  251,
 /*   750 */   251,  177,  251,  175,  174,  167,  118,   83,  202,  235,
 /*   760 */   251,  251,  251,  251,  177,  251,  175,  174,  167,  116,
 /*   770 */    83,  202,  235,  251,  251,  251,  251,  177,  251,  175,
 /*   780 */   174,  167,   89,   83,  202,  235,  251,  251,  251,  251,
 /*   790 */   177,  251,  175,  174,  167,   46,   83,  137,  235,  251,
 /*   800 */   251,  251,  251,  177,  251,  175,  174,   33,  251,   83,
 /*   810 */   251,  235,  251,  251,  167,  160,  177,  202,  251,  251,
 /*   820 */   251,  251,  251,   63,  251,  175,  174,  167,  251,   80,
 /*   830 */   202,  235,  251,  251,  161,  251,  177,  251,  175,  174,
 /*   840 */   167,  251,   79,  202,  235,  251,  251,  251,  251,  177,
 /*   850 */   251,  175,  174,  147,  251,   81,  251,  235,  251,  251,
 /*   860 */   251,  251,  177,
    );
    static public $yy_lookahead = array(
 /*     0 */     4,   40,   41,   73,   12,   13,   76,   94,   25,   96,
 /*    10 */    18,   98,   51,   52,   53,   54,   55,   56,   57,   58,
 /*    20 */    59,   60,   92,   73,   21,   95,   76,   11,   25,   13,
 /*    30 */    15,   15,   79,   17,   18,   19,   40,   41,   23,   47,
 /*    40 */    25,   73,   27,   28,   76,   95,   30,   51,   52,   53,
 /*    50 */    54,   55,   56,   57,   58,   59,   60,   16,   42,   43,
 /*    60 */    44,   45,    3,   95,   16,   49,   25,  102,   73,   74,
 /*    70 */     1,   76,    3,  105,   15,   73,   81,   82,   76,   84,
 /*    80 */    85,   17,   87,   88,   20,   90,   84,   85,   40,   41,
 /*    90 */    95,    3,   90,   12,   13,  100,    1,   95,    3,   51,
 /*   100 */    52,   53,   54,   55,   56,   57,   58,   59,   60,    3,
 /*   110 */    16,    4,   43,   24,   26,    1,    2,    3,   30,    5,
 /*   120 */     6,    7,   30,   21,   10,   19,   24,   19,   47,   40,
 /*   130 */    41,   11,   25,   13,   46,   15,   30,   17,   50,   19,
 /*   140 */    51,   52,   53,   54,   55,   56,   57,   58,   59,   60,
 /*   150 */    30,    1,   11,    3,   13,   49,   15,   49,   17,    9,
 /*   160 */    19,   66,   42,   43,   44,   45,   15,    4,   11,   49,
 /*   170 */    13,   30,   15,   30,   17,   61,   19,   63,   64,   65,
 /*   180 */    39,   78,    3,   42,   43,   44,   45,   30,   25,   73,
 /*   190 */    49,   73,   76,   14,   76,    4,   39,   78,    3,   42,
 /*   200 */    43,   44,   45,   12,   13,   26,   49,   91,   11,   30,
 /*   210 */    13,   95,   15,   95,   17,   94,   19,   96,   11,   98,
 /*   220 */    13,  105,   15,   30,   17,   30,   19,   30,   11,    1,
 /*   230 */    13,    3,   15,    1,   17,    3,   19,   30,   47,   42,
 /*   240 */    43,   44,   45,   50,   75,   50,   49,   30,   79,   42,
 /*   250 */    43,   44,   45,   19,    3,   86,   49,   19,    4,   42,
 /*   260 */    43,   44,   45,   11,   30,   13,   49,   15,   30,   17,
 /*   270 */    80,   19,   11,   28,   13,   43,   15,   26,   17,   25,
 /*   280 */    19,   30,   30,   68,   69,   70,   71,   49,   75,   75,
 /*   290 */    62,   30,   79,   79,   42,   43,   44,   45,   78,   86,
 /*   300 */    86,   49,   75,   42,   43,   44,   45,   15,   73,   74,
 /*   310 */    49,   76,   72,   86,    4,   23,   81,   97,   26,   84,
 /*   320 */    85,   73,   74,   88,   76,   90,   99,   70,   71,   81,
 /*   330 */    95,   73,   84,   85,   76,  100,   88,   27,   90,   73,
 /*   340 */    25,   11,   76,   95,   29,   15,  106,   17,  100,   19,
 /*   350 */    84,   15,   75,   95,   73,   74,   90,   76,   98,   23,
 /*   360 */    30,   95,   81,   86,   18,   84,   85,   21,    1,   88,
 /*   370 */     3,   90,   42,   43,   44,   45,   95,   73,   74,   49,
 /*   380 */    76,  100,    4,    4,   78,   81,   19,   80,   84,   85,
 /*   390 */    73,   74,   88,   76,   90,   27,   28,   73,   81,   95,
 /*   400 */    76,   84,   85,   25,  100,   88,   27,   90,   84,   42,
 /*   410 */    73,   74,   95,   76,   90,   48,   49,  100,   81,   95,
 /*   420 */    16,   84,   85,   73,   74,   88,   76,   90,   15,   77,
 /*   430 */    73,   81,   95,   76,   84,   85,   23,  100,   88,    4,
 /*   440 */    90,   84,   29,   73,   74,   95,   76,   90,   77,   97,
 /*   450 */   100,   81,   95,   77,   84,   85,   17,   11,   88,   20,
 /*   460 */    90,    4,   27,   73,   74,   95,   76,   28,   97,   77,
 /*   470 */   100,   81,   20,   97,   84,   85,   22,   16,   88,   15,
 /*   480 */    90,   27,   18,    4,   27,   95,   25,   23,   79,   97,
 /*   490 */   100,    4,   31,   32,   33,   34,   35,   36,   37,   38,
 /*   500 */    73,   74,    1,   76,    3,   11,   27,    3,   81,   15,
 /*   510 */   101,   84,   85,   19,   27,   88,   94,   90,   73,   74,
 /*   520 */    98,   76,   95,   25,   30,   27,   49,  100,   19,   84,
 /*   530 */    85,   72,   72,   88,   30,   90,   42,   43,   44,   45,
 /*   540 */    95,   94,    8,   49,   25,   98,   72,   75,  103,  104,
 /*   550 */    31,   32,   33,   34,   35,   36,   37,   38,   86,   73,
 /*   560 */    74,    4,   76,   83,   48,  106,  106,    4,   48,    4,
 /*   570 */    84,   85,   73,   74,   88,   76,   90,   97,   30,    4,
 /*   580 */   106,   95,   30,   84,   85,   73,   74,   88,   76,   90,
 /*   590 */   104,   30,   93,   26,   95,   16,   84,   85,   73,   74,
 /*   600 */    88,   76,   90,   30,   30,   93,   30,   95,   26,   84,
 /*   610 */    85,   73,   74,   88,   76,   90,  106,   27,   93,   96,
 /*   620 */    95,   86,   84,   85,   73,   74,   88,   76,   90,   89,
 /*   630 */    97,   93,   78,   95,   92,   84,   85,   78,   19,   88,
 /*   640 */    87,   90,   15,   22,   73,   74,   95,   76,   99,  107,
 /*   650 */   107,  107,  107,  107,  107,   84,   85,   73,   74,   88,
 /*   660 */    76,   90,  107,  107,  107,  107,   95,  107,   84,   85,
 /*   670 */    73,   74,   88,   76,   90,    1,  107,    3,  107,   95,
 /*   680 */   107,   84,   85,   73,   74,   88,   76,   90,  107,  107,
 /*   690 */   107,  107,   95,   19,   84,   85,   73,   74,   88,   76,
 /*   700 */    90,  107,  107,  107,  107,   95,  107,   84,   85,   73,
 /*   710 */    74,   88,   76,   90,  107,  107,   42,  107,   95,  107,
 /*   720 */    84,   85,   48,   49,   88,  107,   90,  107,  107,   73,
 /*   730 */    74,   95,   76,  107,  107,  107,  107,  107,  107,  107,
 /*   740 */    84,   85,   73,   74,   88,   76,   90,  107,  107,  107,
 /*   750 */   107,   95,  107,   84,   85,   73,   74,   88,   76,   90,
 /*   760 */   107,  107,  107,  107,   95,  107,   84,   85,   73,   74,
 /*   770 */    88,   76,   90,  107,  107,  107,  107,   95,  107,   84,
 /*   780 */    85,   73,   74,   88,   76,   90,  107,  107,  107,  107,
 /*   790 */    95,  107,   84,   85,   73,   74,   88,   76,   90,  107,
 /*   800 */   107,  107,  107,   95,  107,   84,   85,    3,  107,   88,
 /*   810 */   107,   90,  107,  107,   73,   11,   95,   76,  107,  107,
 /*   820 */   107,  107,  107,   19,  107,   84,   85,   73,  107,   88,
 /*   830 */    76,   90,  107,  107,   30,  107,   95,  107,   84,   85,
 /*   840 */    73,  107,   88,   76,   90,  107,  107,  107,  107,   95,
 /*   850 */   107,   84,   85,   49,  107,   88,  107,   90,  107,  107,
 /*   860 */   107,  107,   95,
);
    const YY_SHIFT_USE_DFLT = -40;
    const YY_SHIFT_MAX = 158;
    static public $yy_shift_ofst = array(
 /*     0 */   114,  157,  141,  141,  141,  141,  141,  141,  141,  141,
 /*    10 */   141,  141,  261,  120,  261,  120,  120,  120,  120,  120,
 /*    20 */   120,  197,  120,  197,  197,  120,  120,  120,  120,  207,
 /*    30 */   120,   16,  252,  217,  330,  494,  494,  494,  674,  367,
 /*    40 */   106,  804,   15,  439,  439,  238,  498,  108,  454,  498,
 /*    50 */   114,  461,  519,   88,  179,  195,  232,  504,  504,  501,
 /*    60 */   504,  501,  368,  504,  501,  245,  590,  245,   89,   48,
 /*    70 */    -4,  -39,  -39,  -39,  -39,  -39,  -39,  -39,  -39,  191,
 /*    80 */    -8,   81,  228,   81,  251,  150,   69,   95,   64,  107,
 /*    90 */   487,  435,  254,   41,  379,  457,  378,   64,  479,  310,
 /*   100 */    64,  163,  315,   59,    3,  234,   64,  245,  619,  621,
 /*   110 */   245,  -17,  627,  -17,  245,  245,  -17,  -17,  -17,  -17,
 /*   120 */   -40,  -40,  -40,  -40,  -40,  413,  464,  292,  336,  336,
 /*   130 */   102,  346,  336,  193,  520,  557,  567,  582,  516,  509,
 /*   140 */   477,  404,  446,  452,  563,  565,  573,  574,  579,  534,
 /*   150 */   561,  548,  575,  552,  576,  151,  143,   92,   94,
);
    const YY_REDUCE_USE_DFLT = -88;
    const YY_REDUCE_MAX = 124;
    static public $yy_reduce_ofst = array(
 /*     0 */   215,   -5,  427,  390,  370,  248,  317,  350,  304,  281,
 /*    10 */   235,  337,  445,  525,  486,  538,  499,  512,  584,  597,
 /*    20 */   623,  610,  636,  551,  656,  682,  708,  669,  695,  721,
 /*    30 */   571,  741,  767,  754,    2,  357,  266,  324,  116,  -32,
 /*    40 */   -70,  118,  214,  121,  -87,  -50,  169,  258,  227,  213,
 /*    50 */   257,  409,  409,  220,  480,  220,  459,  376,  392,  460,
 /*    60 */   371,  240,  277,  352,  474,  447,  472,  422,  -35,  -35,
 /*    70 */   -35,  -35,  -35,  -35,  -35,  -35,  -35,  -35,  -35,  540,
 /*    80 */   540,  540,  510,  540,  533,  510,  510,  510,  523,  -47,
 /*    90 */   535,  535,  -47,  -47,  535,  535,  -47,  523,  535,  535,
 /*   100 */   523,  -47,  -47,  559,  -47,  542,  523,  260,  553,  549,
 /*   110 */   260,  -47,  554,  -47,  260,  260,  -47,  -47,  -47,  -47,
 /*   120 */   190,  119,  103,  307,  306,
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
        /* 38 */ array(1, 3, 19, 42, 48, 49, ),
        /* 39 */ array(1, 3, 19, 42, 48, 49, ),
        /* 40 */ array(3, 19, 30, 49, ),
        /* 41 */ array(3, 11, 19, 30, 49, ),
        /* 42 */ array(15, 23, 25, 27, 28, ),
        /* 43 */ array(17, 20, 28, ),
        /* 44 */ array(17, 20, 28, ),
        /* 45 */ array(19, 30, 49, ),
        /* 46 */ array(25, 27, ),
        /* 47 */ array(19, 49, ),
        /* 48 */ array(22, 27, ),
        /* 49 */ array(25, 27, ),
        /* 50 */ array(1, 2, 3, 5, 6, 7, 10, 61, 63, 64, 65, ),
        /* 51 */ array(16, 25, 31, 32, 33, 34, 35, 36, 37, 38, ),
        /* 52 */ array(25, 31, 32, 33, 34, 35, 36, 37, 38, ),
        /* 53 */ array(3, 26, 30, 46, 50, ),
        /* 54 */ array(3, 14, 26, 30, ),
        /* 55 */ array(3, 30, 50, ),
        /* 56 */ array(1, 3, 43, ),
        /* 57 */ array(3, 30, ),
        /* 58 */ array(3, 30, ),
        /* 59 */ array(1, 3, ),
        /* 60 */ array(3, 30, ),
        /* 61 */ array(1, 3, ),
        /* 62 */ array(27, 28, ),
        /* 63 */ array(3, 30, ),
        /* 64 */ array(1, 3, ),
        /* 65 */ array(28, ),
        /* 66 */ array(27, ),
        /* 67 */ array(28, ),
        /* 68 */ array(24, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 69 */ array(16, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 70 */ array(4, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 71 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 72 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 73 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 74 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 75 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 76 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 77 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 78 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 79 */ array(4, 12, 13, 47, ),
        /* 80 */ array(12, 13, 18, 47, ),
        /* 81 */ array(12, 13, 47, ),
        /* 82 */ array(1, 3, 62, ),
        /* 83 */ array(12, 13, 47, ),
        /* 84 */ array(3, 26, 30, ),
        /* 85 */ array(1, 3, 9, ),
        /* 86 */ array(1, 3, 43, ),
        /* 87 */ array(1, 3, 66, ),
        /* 88 */ array(17, 20, ),
        /* 89 */ array(4, 25, ),
        /* 90 */ array(4, 27, ),
        /* 91 */ array(4, 27, ),
        /* 92 */ array(4, 25, ),
        /* 93 */ array(16, 25, ),
        /* 94 */ array(4, 27, ),
        /* 95 */ array(4, 27, ),
        /* 96 */ array(4, 25, ),
        /* 97 */ array(17, 20, ),
        /* 98 */ array(4, 27, ),
        /* 99 */ array(4, 27, ),
        /* 100 */ array(17, 20, ),
        /* 101 */ array(4, 25, ),
        /* 102 */ array(25, 29, ),
        /* 103 */ array(3, 15, ),
        /* 104 */ array(21, 25, ),
        /* 105 */ array(19, 30, ),
        /* 106 */ array(17, 20, ),
        /* 107 */ array(28, ),
        /* 108 */ array(19, ),
        /* 109 */ array(22, ),
        /* 110 */ array(28, ),
        /* 111 */ array(25, ),
        /* 112 */ array(15, ),
        /* 113 */ array(25, ),
        /* 114 */ array(28, ),
        /* 115 */ array(28, ),
        /* 116 */ array(25, ),
        /* 117 */ array(25, ),
        /* 118 */ array(25, ),
        /* 119 */ array(25, ),
        /* 120 */ array(),
        /* 121 */ array(),
        /* 122 */ array(),
        /* 123 */ array(),
        /* 124 */ array(),
        /* 125 */ array(15, 23, 29, ),
        /* 126 */ array(15, 18, 23, ),
        /* 127 */ array(15, 23, 26, ),
        /* 128 */ array(15, 23, ),
        /* 129 */ array(15, 23, ),
        /* 130 */ array(21, 24, ),
        /* 131 */ array(18, 21, ),
        /* 132 */ array(15, 23, ),
        /* 133 */ array(30, 50, ),
        /* 134 */ array(48, ),
        /* 135 */ array(4, ),
        /* 136 */ array(26, ),
        /* 137 */ array(26, ),
        /* 138 */ array(48, ),
        /* 139 */ array(19, ),
        /* 140 */ array(49, ),
        /* 141 */ array(16, ),
        /* 142 */ array(11, ),
        /* 143 */ array(20, ),
        /* 144 */ array(4, ),
        /* 145 */ array(4, ),
        /* 146 */ array(30, ),
        /* 147 */ array(30, ),
        /* 148 */ array(16, ),
        /* 149 */ array(8, ),
        /* 150 */ array(30, ),
        /* 151 */ array(30, ),
        /* 152 */ array(4, ),
        /* 153 */ array(30, ),
        /* 154 */ array(30, ),
        /* 155 */ array(15, ),
        /* 156 */ array(30, ),
        /* 157 */ array(30, ),
        /* 158 */ array(16, ),
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
);
    static public $yy_default = array(
 /*     0 */   401,  401,  401,  401,  401,  401,  401,  401,  401,  401,
 /*    10 */   401,  401,  386,  350,  401,  350,  350,  350,  401,  401,
 /*    20 */   401,  401,  401,  401,  401,  401,  401,  401,  401,  401,
 /*    30 */   401,  401,  401,  401,  401,  401,  401,  401,  401,  401,
 /*    40 */   401,  401,  288,  274,  319,  401,  288,  401,  288,  288,
 /*    50 */   259,  360,  360,  326,  401,  326,  401,  401,  401,  401,
 /*    60 */   401,  401,  288,  401,  401,  315,  288,  314,  401,  401,
 /*    70 */   401,  363,  358,  371,  372,  367,  364,  362,  368,  401,
 /*    80 */   401,  355,  401,  294,  401,  401,  401,  401,  344,  401,
 /*    90 */   401,  401,  401,  401,  401,  401,  401,  343,  401,  401,
 /*   100 */   342,  401,  387,  326,  349,  401,  341,  320,  401,  295,
 /*   110 */   317,  389,  326,  292,  338,  316,  388,  361,  282,  289,
 /*   120 */   354,  326,  326,  354,  326,  293,  401,  293,  293,  401,
 /*   130 */   401,  401,  356,  401,  401,  401,  401,  321,  401,  401,
 /*   140 */   401,  401,  401,  304,  401,  401,  401,  401,  401,  401,
 /*   150 */   401,  401,  401,  401,  401,  318,  401,  401,  401,  261,
 /*   160 */   328,  327,  366,  365,  278,  324,  279,  303,  380,  379,
 /*   170 */   378,  260,  298,  329,  296,  297,  357,  323,  262,  265,
 /*   180 */   291,  397,  266,  377,  264,  398,  385,  281,  283,  400,
 /*   190 */   263,  359,  325,  370,  399,  369,  269,  268,  267,  290,
 /*   200 */   382,  381,  321,  285,  300,  337,  336,  335,  322,  334,
 /*   210 */   270,  272,  286,  393,  394,  271,  391,  330,  339,  351,
 /*   220 */   345,  302,  347,  301,  353,  309,  332,  331,  333,  352,
 /*   230 */   313,  287,  307,  348,  284,  308,  346,  306,  280,  374,
 /*   240 */   375,  373,  384,  305,  277,  340,  273,  311,  312,  390,
 /*   250 */   392,  310,  275,  383,  395,  276,  396,  299,  376,
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
    const YYNOCODE = 108;
    const YYSTACKDEPTH = 100;
    const YYNSTATE = 259;
    const YYNRULE = 142;
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
  'SHORTTAGEND',   'COMMENTEND',    'COMMENTSTART',  'INTEGER',     
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
  'varindexed',    'varvar',        'arrayindex',    'modifier',    
  'modparameters',  'ifexprs',       'statements',    'foraction',   
  'value',         'array',         'attribute',     'statement',   
  'exprs',         'math',          'function',      'doublequoted',
  'method',        'params',        'objectchain',   'object',      
  'indexdef',      'varvarele',     'objectelement',  'modparameter',
  'ifexpr',        'ifcond',        'lop',           'arrayelements',
  'arrayelement',  'doublequotedcontent',  'textelement', 
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
 /*  22 */ "smartytag ::= LDEL ID SPACE statements SEMICOLON ifexprs SEMICOLON DOLLAR varvar foraction RDEL",
 /*  23 */ "foraction ::= EQUAL expr",
 /*  24 */ "foraction ::= INCDEC",
 /*  25 */ "smartytag ::= LDEL ID SPACE DOLLAR varvar IN value RDEL",
 /*  26 */ "smartytag ::= LDEL ID SPACE DOLLAR varvar IN array RDEL",
 /*  27 */ "attributes ::= attributes attribute",
 /*  28 */ "attributes ::= attribute",
 /*  29 */ "attributes ::=",
 /*  30 */ "attribute ::= SPACE ID EQUAL expr",
 /*  31 */ "statements ::= statement",
 /*  32 */ "statements ::= statements COMMA statement",
 /*  33 */ "statement ::= DOLLAR varvar EQUAL expr",
 /*  34 */ "expr ::= ID",
 /*  35 */ "expr ::= exprs",
 /*  36 */ "expr ::= expr modifier modparameters",
 /*  37 */ "exprs ::= array",
 /*  38 */ "exprs ::= value",
 /*  39 */ "exprs ::= UNIMATH value",
 /*  40 */ "exprs ::= exprs math value",
 /*  41 */ "exprs ::= exprs ANDSYM value",
 /*  42 */ "math ::= UNIMATH",
 /*  43 */ "math ::= MATH",
 /*  44 */ "value ::= variable",
 /*  45 */ "value ::= INTEGER",
 /*  46 */ "value ::= INTEGER DOT INTEGER",
 /*  47 */ "value ::= BOOLEAN",
 /*  48 */ "value ::= NULL",
 /*  49 */ "value ::= function",
 /*  50 */ "value ::= OPENP expr CLOSEP",
 /*  51 */ "value ::= SINGLEQUOTE text SINGLEQUOTE",
 /*  52 */ "value ::= SINGLEQUOTE SINGLEQUOTE",
 /*  53 */ "value ::= QUOTE doublequoted QUOTE",
 /*  54 */ "value ::= QUOTE QUOTE",
 /*  55 */ "value ::= ID DOUBLECOLON method",
 /*  56 */ "value ::= ID DOUBLECOLON DOLLAR ID OPENP params CLOSEP",
 /*  57 */ "value ::= ID DOUBLECOLON method objectchain",
 /*  58 */ "value ::= ID DOUBLECOLON DOLLAR ID OPENP params CLOSEP objectchain",
 /*  59 */ "value ::= ID DOUBLECOLON ID",
 /*  60 */ "value ::= ID DOUBLECOLON DOLLAR ID arrayindex",
 /*  61 */ "value ::= ID DOUBLECOLON DOLLAR ID arrayindex objectchain",
 /*  62 */ "variable ::= varindexed",
 /*  63 */ "variable ::= DOLLAR varvar AT ID",
 /*  64 */ "variable ::= object",
 /*  65 */ "variable ::= HATCH ID HATCH",
 /*  66 */ "arrayindex ::= arrayindex indexdef",
 /*  67 */ "arrayindex ::=",
 /*  68 */ "indexdef ::= DOT ID",
 /*  69 */ "indexdef ::= DOT INTEGER",
 /*  70 */ "indexdef ::= DOT variable",
 /*  71 */ "indexdef ::= DOT LDEL exprs RDEL",
 /*  72 */ "indexdef ::= OPENB ID CLOSEB",
 /*  73 */ "indexdef ::= OPENB exprs CLOSEB",
 /*  74 */ "indexdef ::= OPENB CLOSEB",
 /*  75 */ "varvar ::= varvarele",
 /*  76 */ "varvar ::= varvar varvarele",
 /*  77 */ "varvarele ::= ID",
 /*  78 */ "varvarele ::= LDEL expr RDEL",
 /*  79 */ "object ::= DOLLAR varvar arrayindex objectchain",
 /*  80 */ "objectchain ::= objectelement",
 /*  81 */ "objectchain ::= objectchain objectelement",
 /*  82 */ "objectelement ::= PTR ID arrayindex",
 /*  83 */ "objectelement ::= PTR variable arrayindex",
 /*  84 */ "objectelement ::= PTR LDEL expr RDEL arrayindex",
 /*  85 */ "objectelement ::= PTR ID LDEL expr RDEL arrayindex",
 /*  86 */ "objectelement ::= PTR method",
 /*  87 */ "function ::= ID OPENP params CLOSEP",
 /*  88 */ "method ::= ID OPENP params CLOSEP",
 /*  89 */ "params ::= expr COMMA params",
 /*  90 */ "params ::= expr",
 /*  91 */ "params ::=",
 /*  92 */ "modifier ::= VERT AT ID",
 /*  93 */ "modifier ::= VERT ID",
 /*  94 */ "modparameters ::= modparameters modparameter",
 /*  95 */ "modparameters ::=",
 /*  96 */ "modparameter ::= COLON exprs",
 /*  97 */ "modparameter ::= COLON ID",
 /*  98 */ "ifexprs ::= ifexpr",
 /*  99 */ "ifexprs ::= NOT ifexprs",
 /* 100 */ "ifexprs ::= OPENP ifexprs CLOSEP",
 /* 101 */ "ifexpr ::= expr",
 /* 102 */ "ifexpr ::= expr ifcond expr",
 /* 103 */ "ifexpr ::= ifexprs lop ifexprs",
 /* 104 */ "ifexpr ::= ifexprs ISDIVBY ifexprs",
 /* 105 */ "ifexpr ::= ifexprs ISNOTDIVBY ifexprs",
 /* 106 */ "ifexpr ::= ifexprs ISEVEN",
 /* 107 */ "ifexpr ::= ifexprs ISNOTEVEN",
 /* 108 */ "ifexpr ::= ifexprs ISEVENBY ifexprs",
 /* 109 */ "ifexpr ::= ifexprs ISNOTEVENBY ifexprs",
 /* 110 */ "ifexpr ::= ifexprs ISODD",
 /* 111 */ "ifexpr ::= ifexprs ISNOTODD",
 /* 112 */ "ifexpr ::= ifexprs ISODDBY ifexprs",
 /* 113 */ "ifexpr ::= ifexprs ISNOTODDBY ifexprs",
 /* 114 */ "ifcond ::= EQUALS",
 /* 115 */ "ifcond ::= NOTEQUALS",
 /* 116 */ "ifcond ::= GREATERTHAN",
 /* 117 */ "ifcond ::= LESSTHAN",
 /* 118 */ "ifcond ::= GREATEREQUAL",
 /* 119 */ "ifcond ::= LESSEQUAL",
 /* 120 */ "ifcond ::= IDENTITY",
 /* 121 */ "ifcond ::= NONEIDENTITY",
 /* 122 */ "lop ::= LAND",
 /* 123 */ "lop ::= LOR",
 /* 124 */ "array ::= OPENB arrayelements CLOSEB",
 /* 125 */ "arrayelements ::= arrayelement",
 /* 126 */ "arrayelements ::= arrayelements COMMA arrayelement",
 /* 127 */ "arrayelements ::=",
 /* 128 */ "arrayelement ::= expr",
 /* 129 */ "arrayelement ::= expr APTR expr",
 /* 130 */ "arrayelement ::= ID APTR expr",
 /* 131 */ "doublequoted ::= doublequoted doublequotedcontent",
 /* 132 */ "doublequoted ::= doublequotedcontent",
 /* 133 */ "doublequotedcontent ::= variable",
 /* 134 */ "doublequotedcontent ::= BACKTICK ID BACKTICK",
 /* 135 */ "doublequotedcontent ::= BACKTICK variable BACKTICK",
 /* 136 */ "doublequotedcontent ::= LDEL expr RDEL",
 /* 137 */ "doublequotedcontent ::= OTHER",
 /* 138 */ "text ::= text textelement",
 /* 139 */ "text ::= textelement",
 /* 140 */ "textelement ::= OTHER",
 /* 141 */ "textelement ::= LDEL",
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
  array( 'lhs' => 71, 'rhs' => 6 ),
  array( 'lhs' => 76, 'rhs' => 3 ),
  array( 'lhs' => 71, 'rhs' => 4 ),
  array( 'lhs' => 71, 'rhs' => 6 ),
  array( 'lhs' => 71, 'rhs' => 6 ),
  array( 'lhs' => 71, 'rhs' => 4 ),
  array( 'lhs' => 71, 'rhs' => 5 ),
  array( 'lhs' => 71, 'rhs' => 5 ),
  array( 'lhs' => 71, 'rhs' => 11 ),
  array( 'lhs' => 83, 'rhs' => 2 ),
  array( 'lhs' => 83, 'rhs' => 1 ),
  array( 'lhs' => 71, 'rhs' => 8 ),
  array( 'lhs' => 71, 'rhs' => 8 ),
  array( 'lhs' => 75, 'rhs' => 2 ),
  array( 'lhs' => 75, 'rhs' => 1 ),
  array( 'lhs' => 75, 'rhs' => 0 ),
  array( 'lhs' => 86, 'rhs' => 4 ),
  array( 'lhs' => 82, 'rhs' => 1 ),
  array( 'lhs' => 82, 'rhs' => 3 ),
  array( 'lhs' => 87, 'rhs' => 4 ),
  array( 'lhs' => 74, 'rhs' => 1 ),
  array( 'lhs' => 74, 'rhs' => 1 ),
  array( 'lhs' => 74, 'rhs' => 3 ),
  array( 'lhs' => 88, 'rhs' => 1 ),
  array( 'lhs' => 88, 'rhs' => 1 ),
  array( 'lhs' => 88, 'rhs' => 2 ),
  array( 'lhs' => 88, 'rhs' => 3 ),
  array( 'lhs' => 88, 'rhs' => 3 ),
  array( 'lhs' => 89, 'rhs' => 1 ),
  array( 'lhs' => 89, 'rhs' => 1 ),
  array( 'lhs' => 84, 'rhs' => 1 ),
  array( 'lhs' => 84, 'rhs' => 1 ),
  array( 'lhs' => 84, 'rhs' => 3 ),
  array( 'lhs' => 84, 'rhs' => 1 ),
  array( 'lhs' => 84, 'rhs' => 1 ),
  array( 'lhs' => 84, 'rhs' => 1 ),
  array( 'lhs' => 84, 'rhs' => 3 ),
  array( 'lhs' => 84, 'rhs' => 3 ),
  array( 'lhs' => 84, 'rhs' => 2 ),
  array( 'lhs' => 84, 'rhs' => 3 ),
  array( 'lhs' => 84, 'rhs' => 2 ),
  array( 'lhs' => 84, 'rhs' => 3 ),
  array( 'lhs' => 84, 'rhs' => 7 ),
  array( 'lhs' => 84, 'rhs' => 4 ),
  array( 'lhs' => 84, 'rhs' => 8 ),
  array( 'lhs' => 84, 'rhs' => 3 ),
  array( 'lhs' => 84, 'rhs' => 5 ),
  array( 'lhs' => 84, 'rhs' => 6 ),
  array( 'lhs' => 73, 'rhs' => 1 ),
  array( 'lhs' => 73, 'rhs' => 4 ),
  array( 'lhs' => 73, 'rhs' => 1 ),
  array( 'lhs' => 73, 'rhs' => 3 ),
  array( 'lhs' => 78, 'rhs' => 2 ),
  array( 'lhs' => 78, 'rhs' => 0 ),
  array( 'lhs' => 96, 'rhs' => 2 ),
  array( 'lhs' => 96, 'rhs' => 2 ),
  array( 'lhs' => 96, 'rhs' => 2 ),
  array( 'lhs' => 96, 'rhs' => 4 ),
  array( 'lhs' => 96, 'rhs' => 3 ),
  array( 'lhs' => 96, 'rhs' => 3 ),
  array( 'lhs' => 96, 'rhs' => 2 ),
  array( 'lhs' => 77, 'rhs' => 1 ),
  array( 'lhs' => 77, 'rhs' => 2 ),
  array( 'lhs' => 97, 'rhs' => 1 ),
  array( 'lhs' => 97, 'rhs' => 3 ),
  array( 'lhs' => 95, 'rhs' => 4 ),
  array( 'lhs' => 94, 'rhs' => 1 ),
  array( 'lhs' => 94, 'rhs' => 2 ),
  array( 'lhs' => 98, 'rhs' => 3 ),
  array( 'lhs' => 98, 'rhs' => 3 ),
  array( 'lhs' => 98, 'rhs' => 5 ),
  array( 'lhs' => 98, 'rhs' => 6 ),
  array( 'lhs' => 98, 'rhs' => 2 ),
  array( 'lhs' => 90, 'rhs' => 4 ),
  array( 'lhs' => 92, 'rhs' => 4 ),
  array( 'lhs' => 93, 'rhs' => 3 ),
  array( 'lhs' => 93, 'rhs' => 1 ),
  array( 'lhs' => 93, 'rhs' => 0 ),
  array( 'lhs' => 79, 'rhs' => 3 ),
  array( 'lhs' => 79, 'rhs' => 2 ),
  array( 'lhs' => 80, 'rhs' => 2 ),
  array( 'lhs' => 80, 'rhs' => 0 ),
  array( 'lhs' => 99, 'rhs' => 2 ),
  array( 'lhs' => 99, 'rhs' => 2 ),
  array( 'lhs' => 81, 'rhs' => 1 ),
  array( 'lhs' => 81, 'rhs' => 2 ),
  array( 'lhs' => 81, 'rhs' => 3 ),
  array( 'lhs' => 100, 'rhs' => 1 ),
  array( 'lhs' => 100, 'rhs' => 3 ),
  array( 'lhs' => 100, 'rhs' => 3 ),
  array( 'lhs' => 100, 'rhs' => 3 ),
  array( 'lhs' => 100, 'rhs' => 3 ),
  array( 'lhs' => 100, 'rhs' => 2 ),
  array( 'lhs' => 100, 'rhs' => 2 ),
  array( 'lhs' => 100, 'rhs' => 3 ),
  array( 'lhs' => 100, 'rhs' => 3 ),
  array( 'lhs' => 100, 'rhs' => 2 ),
  array( 'lhs' => 100, 'rhs' => 2 ),
  array( 'lhs' => 100, 'rhs' => 3 ),
  array( 'lhs' => 100, 'rhs' => 3 ),
  array( 'lhs' => 101, 'rhs' => 1 ),
  array( 'lhs' => 101, 'rhs' => 1 ),
  array( 'lhs' => 101, 'rhs' => 1 ),
  array( 'lhs' => 101, 'rhs' => 1 ),
  array( 'lhs' => 101, 'rhs' => 1 ),
  array( 'lhs' => 101, 'rhs' => 1 ),
  array( 'lhs' => 101, 'rhs' => 1 ),
  array( 'lhs' => 101, 'rhs' => 1 ),
  array( 'lhs' => 102, 'rhs' => 1 ),
  array( 'lhs' => 102, 'rhs' => 1 ),
  array( 'lhs' => 85, 'rhs' => 3 ),
  array( 'lhs' => 103, 'rhs' => 1 ),
  array( 'lhs' => 103, 'rhs' => 3 ),
  array( 'lhs' => 103, 'rhs' => 0 ),
  array( 'lhs' => 104, 'rhs' => 1 ),
  array( 'lhs' => 104, 'rhs' => 3 ),
  array( 'lhs' => 104, 'rhs' => 3 ),
  array( 'lhs' => 91, 'rhs' => 2 ),
  array( 'lhs' => 91, 'rhs' => 1 ),
  array( 'lhs' => 105, 'rhs' => 1 ),
  array( 'lhs' => 105, 'rhs' => 3 ),
  array( 'lhs' => 105, 'rhs' => 3 ),
  array( 'lhs' => 105, 'rhs' => 3 ),
  array( 'lhs' => 105, 'rhs' => 1 ),
  array( 'lhs' => 72, 'rhs' => 2 ),
  array( 'lhs' => 72, 'rhs' => 1 ),
  array( 'lhs' => 106, 'rhs' => 1 ),
  array( 'lhs' => 106, 'rhs' => 1 ),
    );

    /**
     * The following table contains a mapping of reduce action to method name
     * that handles the reduction.
     * 
     * If a rule is not set, it has no handler.
     */
    static public $yyReduceMap = array(
        0 => 0,
        38 => 0,
        44 => 0,
        45 => 0,
        47 => 0,
        48 => 0,
        49 => 0,
        64 => 0,
        125 => 0,
        1 => 1,
        35 => 1,
        37 => 1,
        42 => 1,
        43 => 1,
        75 => 1,
        98 => 1,
        132 => 1,
        139 => 1,
        140 => 1,
        141 => 1,
        2 => 2,
        66 => 2,
        131 => 2,
        138 => 2,
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
        90 => 24,
        128 => 24,
        25 => 25,
        26 => 25,
        27 => 27,
        29 => 29,
        30 => 30,
        31 => 31,
        32 => 32,
        33 => 33,
        34 => 34,
        36 => 36,
        39 => 39,
        40 => 40,
        41 => 41,
        46 => 46,
        50 => 50,
        51 => 51,
        52 => 52,
        54 => 52,
        53 => 53,
        55 => 55,
        56 => 56,
        57 => 57,
        58 => 58,
        59 => 59,
        60 => 60,
        61 => 61,
        62 => 62,
        63 => 63,
        65 => 65,
        67 => 67,
        95 => 67,
        68 => 68,
        69 => 69,
        70 => 70,
        71 => 71,
        73 => 71,
        72 => 72,
        74 => 74,
        76 => 76,
        77 => 77,
        78 => 78,
        100 => 78,
        79 => 79,
        80 => 80,
        81 => 81,
        82 => 82,
        83 => 83,
        84 => 84,
        85 => 85,
        86 => 86,
        87 => 87,
        88 => 88,
        89 => 89,
        91 => 91,
        92 => 92,
        93 => 93,
        94 => 94,
        96 => 96,
        97 => 97,
        99 => 99,
        101 => 101,
        102 => 102,
        103 => 102,
        104 => 104,
        105 => 105,
        106 => 106,
        111 => 106,
        107 => 107,
        110 => 107,
        108 => 108,
        113 => 108,
        109 => 109,
        112 => 109,
        114 => 114,
        115 => 115,
        116 => 116,
        117 => 117,
        118 => 118,
        119 => 119,
        120 => 120,
        121 => 121,
        122 => 122,
        123 => 123,
        124 => 124,
        126 => 126,
        127 => 127,
        129 => 129,
        130 => 130,
        133 => 133,
        134 => 134,
        135 => 135,
        136 => 136,
        137 => 137,
    );
    /* Beginning here are the reduction cases.  A typical example
    ** follows:
    **  #line <lineno> <grammarfile>
    **   function yy_r0($yymsp){ ... }           // User supplied code
    **  #line <lineno> <thisfile>
    */
#line 73 "internal.templateparser.y"
    function yy_r0(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;     }
#line 1790 "internal.templateparser.php"
#line 79 "internal.templateparser.y"
    function yy_r1(){$this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1793 "internal.templateparser.php"
#line 81 "internal.templateparser.y"
    function yy_r2(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1796 "internal.templateparser.php"
#line 87 "internal.templateparser.y"
    function yy_r3(){if ($this->compiler->has_code) {
                                            $tmp =''; foreach ($this->prefix_code as $code) {$tmp.=$code;} $this->prefix_code=array();
                                            $this->_retvalue = $this->cacher->processNocacheCode($tmp.$this->yystack[$this->yyidx + 0]->minor, $this->compiler,$this->nocache,true);
                                         } $this->nocache=false;    }
#line 1802 "internal.templateparser.php"
#line 100 "internal.templateparser.y"
    function yy_r4(){ $this->_retvalue = '';    }
#line 1805 "internal.templateparser.php"
#line 103 "internal.templateparser.y"
    function yy_r5(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + -1]->minor, $this->compiler,false,false);    }
#line 1808 "internal.templateparser.php"
#line 105 "internal.templateparser.y"
    function yy_r6(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->left_delimiter, $this->compiler,false,false);    }
#line 1811 "internal.templateparser.php"
#line 107 "internal.templateparser.y"
    function yy_r7(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->right_delimiter, $this->compiler,false,false);    }
#line 1814 "internal.templateparser.php"
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
#line 1825 "internal.templateparser.php"
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
#line 1836 "internal.templateparser.php"
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
#line 1847 "internal.templateparser.php"
#line 139 "internal.templateparser.y"
    function yy_r11(){$this->_retvalue = $this->cacher->processNocacheCode("<?php echo '".$this->yystack[$this->yyidx + 0]->minor."';?>\n", $this->compiler, true, true);    }
#line 1850 "internal.templateparser.php"
#line 141 "internal.templateparser.y"
    function yy_r12(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + 0]->minor, $this->compiler,false,false);    }
#line 1853 "internal.templateparser.php"
#line 149 "internal.templateparser.y"
    function yy_r13(){ $this->_retvalue = $this->compiler->compileTag('print_expression',array_merge(array('value'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1856 "internal.templateparser.php"
#line 152 "internal.templateparser.y"
    function yy_r14(){ $this->_retvalue = $this->compiler->compileTag('assign',array_merge(array('value'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -4]->minor,$this->yystack[$this->yyidx + -1]->minor));    }
#line 1859 "internal.templateparser.php"
#line 154 "internal.templateparser.y"
    function yy_r15(){$this->_retvalue = array('var'=>$this->yystack[$this->yyidx + -1]->minor, 'index'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1862 "internal.templateparser.php"
#line 158 "internal.templateparser.y"
    function yy_r16(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor,$this->yystack[$this->yyidx + -1]->minor);    }
#line 1865 "internal.templateparser.php"
#line 160 "internal.templateparser.y"
    function yy_r17(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -4]->minor,array_merge(array('object_methode'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1868 "internal.templateparser.php"
#line 162 "internal.templateparser.y"
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
#line 1883 "internal.templateparser.php"
#line 176 "internal.templateparser.y"
    function yy_r19(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor.'close',$this->yystack[$this->yyidx + -1]->minor);    }
#line 1886 "internal.templateparser.php"
#line 178 "internal.templateparser.y"
    function yy_r20(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor.'close',array('object_methode'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1889 "internal.templateparser.php"
#line 180 "internal.templateparser.y"
    function yy_r21(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor,array('if condition'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1892 "internal.templateparser.php"
#line 182 "internal.templateparser.y"
    function yy_r22(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -9]->minor,array('start'=>$this->yystack[$this->yyidx + -7]->minor,'ifexp'=>$this->yystack[$this->yyidx + -5]->minor,'varloop'=>$this->yystack[$this->yyidx + -2]->minor,'loop'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1895 "internal.templateparser.php"
#line 183 "internal.templateparser.y"
    function yy_r23(){ $this->_retvalue = '='.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1898 "internal.templateparser.php"
#line 184 "internal.templateparser.y"
    function yy_r24(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1901 "internal.templateparser.php"
#line 187 "internal.templateparser.y"
    function yy_r25(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -6]->minor,array('from'=>$this->yystack[$this->yyidx + -1]->minor,'item'=>$this->yystack[$this->yyidx + -3]->minor));    }
#line 1904 "internal.templateparser.php"
#line 194 "internal.templateparser.y"
    function yy_r27(){ $this->_retvalue = array_merge($this->yystack[$this->yyidx + -1]->minor,$this->yystack[$this->yyidx + 0]->minor);    }
#line 1907 "internal.templateparser.php"
#line 198 "internal.templateparser.y"
    function yy_r29(){ $this->_retvalue = array();    }
#line 1910 "internal.templateparser.php"
#line 202 "internal.templateparser.y"
    function yy_r30(){ $this->_retvalue = array($this->yystack[$this->yyidx + -2]->minor=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1913 "internal.templateparser.php"
#line 207 "internal.templateparser.y"
    function yy_r31(){ $this->_retvalue = array($this->yystack[$this->yyidx + 0]->minor);    }
#line 1916 "internal.templateparser.php"
#line 208 "internal.templateparser.y"
    function yy_r32(){ $this->yystack[$this->yyidx + -2]->minor[]=$this->yystack[$this->yyidx + 0]->minor; $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor;    }
#line 1919 "internal.templateparser.php"
#line 210 "internal.templateparser.y"
    function yy_r33(){ $this->_retvalue = array('var' => $this->yystack[$this->yyidx + -2]->minor, 'value'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1922 "internal.templateparser.php"
#line 217 "internal.templateparser.y"
    function yy_r34(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';     }
#line 1925 "internal.templateparser.php"
#line 221 "internal.templateparser.y"
    function yy_r36(){             
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
#line 1940 "internal.templateparser.php"
#line 239 "internal.templateparser.y"
    function yy_r39(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1943 "internal.templateparser.php"
#line 241 "internal.templateparser.y"
    function yy_r40(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor . $this->yystack[$this->yyidx + -1]->minor . $this->yystack[$this->yyidx + 0]->minor;     }
#line 1946 "internal.templateparser.php"
#line 243 "internal.templateparser.y"
    function yy_r41(){ $this->_retvalue = '('. $this->yystack[$this->yyidx + -2]->minor . ').(' . $this->yystack[$this->yyidx + 0]->minor. ')';     }
#line 1949 "internal.templateparser.php"
#line 260 "internal.templateparser.y"
    function yy_r46(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1952 "internal.templateparser.php"
#line 269 "internal.templateparser.y"
    function yy_r50(){ $this->_retvalue = "(". $this->yystack[$this->yyidx + -1]->minor .")";     }
#line 1955 "internal.templateparser.php"
#line 272 "internal.templateparser.y"
    function yy_r51(){ $this->_retvalue = "'".$this->yystack[$this->yyidx + -1]->minor."'";     }
#line 1958 "internal.templateparser.php"
#line 273 "internal.templateparser.y"
    function yy_r52(){ $this->_retvalue = "''";     }
#line 1961 "internal.templateparser.php"
#line 275 "internal.templateparser.y"
    function yy_r53(){ $this->_retvalue = "'".str_replace('\"','"',$this->yystack[$this->yyidx + -1]->minor)."'";     }
#line 1964 "internal.templateparser.php"
#line 281 "internal.templateparser.y"
    function yy_r55(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1967 "internal.templateparser.php"
#line 282 "internal.templateparser.y"
    function yy_r56(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -3]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -6]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -1]->minor .')';     }
#line 1970 "internal.templateparser.php"
#line 284 "internal.templateparser.y"
    function yy_r57(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor.'::'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1973 "internal.templateparser.php"
#line 285 "internal.templateparser.y"
    function yy_r58(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -4]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -7]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -2]->minor .')'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1976 "internal.templateparser.php"
#line 287 "internal.templateparser.y"
    function yy_r59(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1979 "internal.templateparser.php"
#line 289 "internal.templateparser.y"
    function yy_r60(){ $this->_retvalue = $this->yystack[$this->yyidx + -4]->minor.'::$'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1982 "internal.templateparser.php"
#line 291 "internal.templateparser.y"
    function yy_r61(){ $this->_retvalue = $this->yystack[$this->yyidx + -5]->minor.'::$'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1985 "internal.templateparser.php"
#line 300 "internal.templateparser.y"
    function yy_r62(){ if ($this->yystack[$this->yyidx + 0]->minor['var'] == '\'smarty\'') { $this->_retvalue =  $this->compiler->compileTag(trim($this->yystack[$this->yyidx + 0]->minor['var'],"'"),$this->yystack[$this->yyidx + 0]->minor['index']);} else {
                                                         $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + 0]->minor['var'] .')->value'.$this->yystack[$this->yyidx + 0]->minor['index']; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + 0]->minor['var'],"'"))->nocache;}    }
#line 1989 "internal.templateparser.php"
#line 303 "internal.templateparser.y"
    function yy_r63(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->'.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 1992 "internal.templateparser.php"
#line 307 "internal.templateparser.y"
    function yy_r65(){$this->_retvalue = '$_smarty_tpl->getConfigVariable(\''. $this->yystack[$this->yyidx + -1]->minor .'\')';    }
#line 1995 "internal.templateparser.php"
#line 315 "internal.templateparser.y"
    function yy_r67(){return;    }
#line 1998 "internal.templateparser.php"
#line 319 "internal.templateparser.y"
    function yy_r68(){ $this->_retvalue = "['". $this->yystack[$this->yyidx + 0]->minor ."']";    }
#line 2001 "internal.templateparser.php"
#line 320 "internal.templateparser.y"
    function yy_r69(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + 0]->minor ."]";    }
#line 2004 "internal.templateparser.php"
#line 322 "internal.templateparser.y"
    function yy_r70(){ $this->_retvalue = "[".$this->yystack[$this->yyidx + 0]->minor."]";    }
#line 2007 "internal.templateparser.php"
#line 323 "internal.templateparser.y"
    function yy_r71(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + -1]->minor ."]";    }
#line 2010 "internal.templateparser.php"
#line 325 "internal.templateparser.y"
    function yy_r72(){ $this->_retvalue = '['.$this->compiler->compileTag('smarty','[\'section\'][\''.$this->yystack[$this->yyidx + -1]->minor.'\'][\'index\']').']';    }
#line 2013 "internal.templateparser.php"
#line 329 "internal.templateparser.y"
    function yy_r74(){$this->_retvalue = '';    }
#line 2016 "internal.templateparser.php"
#line 337 "internal.templateparser.y"
    function yy_r76(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2019 "internal.templateparser.php"
#line 339 "internal.templateparser.y"
    function yy_r77(){$this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2022 "internal.templateparser.php"
#line 341 "internal.templateparser.y"
    function yy_r78(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2025 "internal.templateparser.php"
#line 346 "internal.templateparser.y"
    function yy_r79(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->value'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 2028 "internal.templateparser.php"
#line 348 "internal.templateparser.y"
    function yy_r80(){$this->_retvalue  = $this->yystack[$this->yyidx + 0]->minor;     }
#line 2031 "internal.templateparser.php"
#line 350 "internal.templateparser.y"
    function yy_r81(){$this->_retvalue  = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2034 "internal.templateparser.php"
#line 352 "internal.templateparser.y"
    function yy_r82(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2037 "internal.templateparser.php"
#line 353 "internal.templateparser.y"
    function yy_r83(){ $this->_retvalue = '->{'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2040 "internal.templateparser.php"
#line 354 "internal.templateparser.y"
    function yy_r84(){ $this->_retvalue = '->{'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2043 "internal.templateparser.php"
#line 355 "internal.templateparser.y"
    function yy_r85(){ $this->_retvalue = '->{\''.$this->yystack[$this->yyidx + -4]->minor.'\'.'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2046 "internal.templateparser.php"
#line 357 "internal.templateparser.y"
    function yy_r86(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2049 "internal.templateparser.php"
#line 363 "internal.templateparser.y"
    function yy_r87(){if (!$this->template->security || $this->smarty->security_handler->isTrustedPhpFunction($this->yystack[$this->yyidx + -3]->minor, $this->compiler)) {
																					            if ($this->yystack[$this->yyidx + -3]->minor == 'isset' || $this->yystack[$this->yyidx + -3]->minor == 'empty' || $this->yystack[$this->yyidx + -3]->minor == 'array' || is_callable($this->yystack[$this->yyidx + -3]->minor)) {
																					                $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";
																					            } else {
                                                       $this->compiler->trigger_template_error ("unknown function \"" . $this->yystack[$this->yyidx + -3]->minor . "\"");
                                                      }
                                                    }    }
#line 2058 "internal.templateparser.php"
#line 374 "internal.templateparser.y"
    function yy_r88(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";    }
#line 2061 "internal.templateparser.php"
#line 378 "internal.templateparser.y"
    function yy_r89(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.",".$this->yystack[$this->yyidx + 0]->minor;    }
#line 2064 "internal.templateparser.php"
#line 382 "internal.templateparser.y"
    function yy_r91(){ return;    }
#line 2067 "internal.templateparser.php"
#line 387 "internal.templateparser.y"
    function yy_r92(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,true);    }
#line 2070 "internal.templateparser.php"
#line 388 "internal.templateparser.y"
    function yy_r93(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,false);    }
#line 2073 "internal.templateparser.php"
#line 395 "internal.templateparser.y"
    function yy_r94(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2076 "internal.templateparser.php"
#line 399 "internal.templateparser.y"
    function yy_r96(){$this->_retvalue = ','.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2079 "internal.templateparser.php"
#line 400 "internal.templateparser.y"
    function yy_r97(){$this->_retvalue = ',\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2082 "internal.templateparser.php"
#line 407 "internal.templateparser.y"
    function yy_r99(){$this->_retvalue = '!'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2085 "internal.templateparser.php"
#line 412 "internal.templateparser.y"
    function yy_r101(){$this->_retvalue =$this->yystack[$this->yyidx + 0]->minor;    }
#line 2088 "internal.templateparser.php"
#line 413 "internal.templateparser.y"
    function yy_r102(){$this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2091 "internal.templateparser.php"
#line 415 "internal.templateparser.y"
    function yy_r104(){$this->_retvalue = '!('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2094 "internal.templateparser.php"
#line 416 "internal.templateparser.y"
    function yy_r105(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2097 "internal.templateparser.php"
#line 417 "internal.templateparser.y"
    function yy_r106(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2100 "internal.templateparser.php"
#line 418 "internal.templateparser.y"
    function yy_r107(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2103 "internal.templateparser.php"
#line 419 "internal.templateparser.y"
    function yy_r108(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2106 "internal.templateparser.php"
#line 420 "internal.templateparser.y"
    function yy_r109(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2109 "internal.templateparser.php"
#line 426 "internal.templateparser.y"
    function yy_r114(){$this->_retvalue = '==';    }
#line 2112 "internal.templateparser.php"
#line 427 "internal.templateparser.y"
    function yy_r115(){$this->_retvalue = '!=';    }
#line 2115 "internal.templateparser.php"
#line 428 "internal.templateparser.y"
    function yy_r116(){$this->_retvalue = '>';    }
#line 2118 "internal.templateparser.php"
#line 429 "internal.templateparser.y"
    function yy_r117(){$this->_retvalue = '<';    }
#line 2121 "internal.templateparser.php"
#line 430 "internal.templateparser.y"
    function yy_r118(){$this->_retvalue = '>=';    }
#line 2124 "internal.templateparser.php"
#line 431 "internal.templateparser.y"
    function yy_r119(){$this->_retvalue = '<=';    }
#line 2127 "internal.templateparser.php"
#line 432 "internal.templateparser.y"
    function yy_r120(){$this->_retvalue = '===';    }
#line 2130 "internal.templateparser.php"
#line 433 "internal.templateparser.y"
    function yy_r121(){$this->_retvalue = '!==';    }
#line 2133 "internal.templateparser.php"
#line 435 "internal.templateparser.y"
    function yy_r122(){$this->_retvalue = '&&';    }
#line 2136 "internal.templateparser.php"
#line 436 "internal.templateparser.y"
    function yy_r123(){$this->_retvalue = '||';    }
#line 2139 "internal.templateparser.php"
#line 441 "internal.templateparser.y"
    function yy_r124(){ $this->_retvalue = 'array('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2142 "internal.templateparser.php"
#line 443 "internal.templateparser.y"
    function yy_r126(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.','.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2145 "internal.templateparser.php"
#line 444 "internal.templateparser.y"
    function yy_r127(){ return;     }
#line 2148 "internal.templateparser.php"
#line 446 "internal.templateparser.y"
    function yy_r129(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2151 "internal.templateparser.php"
#line 447 "internal.templateparser.y"
    function yy_r130(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + -2]->minor.'\'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2154 "internal.templateparser.php"
#line 454 "internal.templateparser.y"
    function yy_r133(){$this->_retvalue = "'.".$this->yystack[$this->yyidx + 0]->minor.".'";    }
#line 2157 "internal.templateparser.php"
#line 455 "internal.templateparser.y"
    function yy_r134(){$this->_retvalue = "`".$this->yystack[$this->yyidx + -1]->minor."`";    }
#line 2160 "internal.templateparser.php"
#line 456 "internal.templateparser.y"
    function yy_r135(){$this->_retvalue = "'.".$this->yystack[$this->yyidx + -1]->minor.".'";    }
#line 2163 "internal.templateparser.php"
#line 457 "internal.templateparser.y"
    function yy_r136(){$this->_retvalue = "'.(".$this->yystack[$this->yyidx + -1]->minor.").'";    }
#line 2166 "internal.templateparser.php"
#line 458 "internal.templateparser.y"
    function yy_r137(){$this->_retvalue = addcslashes($this->yystack[$this->yyidx + 0]->minor,"'");    }
#line 2169 "internal.templateparser.php"

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
#line 2287 "internal.templateparser.php"
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
#line 2312 "internal.templateparser.php"
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
