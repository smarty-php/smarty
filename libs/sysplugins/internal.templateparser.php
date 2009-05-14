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
    const YY_NO_ACTION = 421;
    const YY_ACCEPT_ACTION = 420;
    const YY_ERROR_ACTION = 419;

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
    const YY_SZ_ACTTAB = 889;
static public $yy_action = array(
 /*     0 */   272,  150,  134,   37,  134,    6,   28,   12,  257,   57,
 /*    10 */   175,  176,  172,  196,  191,  189,  192,  194,   24,   24,
 /*    20 */   137,  420,   48,  170,  223,  255,  258,  159,  162,    7,
 /*    30 */   179,  253,   51,   55,  200,  206,  183,  181,  165,  157,
 /*    40 */    34,   21,   19,   24,  174,  238,  238,  186,  184,  193,
 /*    50 */   185,    4,    2,    8,    3,    9,   11,  100,  103,  129,
 /*    60 */    35,  183,  181,   16,  182,  140,   27,  134,  250,  250,
 /*    70 */   238,  134,  186,  184,  193,  185,    4,    2,    8,    3,
 /*    80 */     9,   11,   60,   17,   31,  255,  258,   40,  183,  181,
 /*    90 */   150,   97,   37,  108,   18,   38,   12,  242,   60,  186,
 /*   100 */   184,  193,  185,    4,    2,    8,    3,    9,   11,  132,
 /*   110 */   236,  115,  157,  143,    5,   20,  239,   89,  255,  258,
 /*   120 */    35,   51,   55,  200,  206,  134,   41,   15,  157,  183,
 /*   130 */   181,  175,  176,  172,  196,  191,  189,  192,  194,  129,
 /*   140 */   186,  184,  193,  185,    4,    2,    8,    3,    9,   11,
 /*   150 */    24,  218,  150,   35,   37,   24,    6,   13,   12,  152,
 /*   160 */    61,   34,  230,  245,  158,   30,   17,  244,  232,   46,
 /*   170 */   106,  130,   66,   21,   97,  128,  198,  238,  230,  230,
 /*   180 */     7,  229,  168,   51,   55,  200,  206,  214,  198,   50,
 /*   190 */   157,  230,  207,  243,   72,  167,  138,  229,  229,  147,
 /*   200 */   144,  150,   84,   37,  207,   18,  220,   12,  217,   57,
 /*   210 */   229,   60,  266,  118,   23,  188,  228,  150,  240,   37,
 /*   220 */   137,   18,  142,   12,   59,   57,  225,  226,   62,  220,
 /*   230 */   156,  217,   51,   55,  200,  206,   39,   90,  150,  157,
 /*   240 */    37,  157,   18,  129,   12,  220,   60,  217,   51,   55,
 /*   250 */   200,  206,  262,  267,  150,  157,   37,  136,   18,   42,
 /*   260 */    12,  266,   60,   23,   24,  178,   17,   25,  219,   51,
 /*   270 */    55,  200,  206,  133,   97,  220,  157,  217,  198,  156,
 /*   280 */    29,  230,  220,  224,  217,   51,   55,  200,  206,  173,
 /*   290 */   180,  238,  157,  117,  207,  233,  228,  150,  240,   37,
 /*   300 */   229,   18,  208,   12,   60,   57,  213,   17,   42,   24,
 /*   310 */    94,  198,   96,  150,  230,   97,  131,   18,   22,   12,
 /*   320 */   247,   60,  202,  205,  211,   84,  113,  207,   51,   55,
 /*   330 */   200,  206,  136,  229,  157,  157,  238,  198,   38,   54,
 /*   340 */   230,  135,  270,  155,   51,   55,  200,  206,  260,   33,
 /*   350 */    99,  157,  145,  207,  262,  230,  140,  203,   17,  229,
 /*   360 */   263,  198,   50,   14,  230,   60,   97,   77,  134,  111,
 /*   370 */     1,  154,  202,  205,  229,   84,  215,  207,  198,   50,
 /*   380 */   250,  230,  239,  229,   76,  155,  161,  210,  188,  202,
 /*   390 */   205,  134,   84,  259,  207,  157,  198,   49,  204,  230,
 /*   400 */   229,  230,   71,  198,   50,  188,  230,  202,  205,   73,
 /*   410 */    84,  268,  207,   31,  202,  205,   40,   84,  229,  207,
 /*   420 */   229,  198,   50,  188,  230,  229,   17,   70,  265,  241,
 /*   430 */   188,  163,  202,  205,   97,   84,  104,  207,  129,  198,
 /*   440 */    50,  248,  230,  229,  256,   79,  249,  250,  188,  134,
 /*   450 */   202,  205,  102,   84,  126,  207,  198,   50,  271,  230,
 /*   460 */    32,  229,   74,  250,  155,  155,  188,  202,  205,  155,
 /*   470 */    84,  177,  207,   43,  198,   50,  237,  230,  229,  134,
 /*   480 */    78,  198,   50,  188,  230,  202,  205,   75,   84,  119,
 /*   490 */   207,   24,  202,  205,  155,   84,  229,  207,  127,  198,
 /*   500 */    50,  188,  230,  229,  246,   80,  251,  125,  188,   81,
 /*   510 */   202,  205,  150,   84,  198,  207,   18,  230,  238,  134,
 /*   520 */    60,  229,  222,  223,  254,  201,  188,  155,  134,  155,
 /*   530 */   207,  136,   52,  134,   65,  155,  229,  198,   96,   91,
 /*   540 */   230,  235,   87,   51,   55,  200,  206,  262,  202,  205,
 /*   550 */   157,   84,  124,  207,  262,  198,   98,  240,  230,  229,
 /*   560 */   220,  149,  217,   88,  171,   53,  202,  205,  264,   84,
 /*   570 */    86,  207,  198,   98,  164,  230,  218,  229,  262,  141,
 /*   580 */   262,  105,   92,  202,  205,  114,   84,   68,  207,   10,
 /*   590 */   240,  166,  250,  237,  229,  198,   98,  169,  230,  261,
 /*   600 */   187,   15,  212,  197,  218,  231,  202,  205,  146,   84,
 /*   610 */    67,  207,  198,   98,  269,  230,  218,  229,  139,   56,
 /*   620 */   227,  195,  148,  202,  205,  234,   84,   22,  207,  199,
 /*   630 */    26,  160,  153,   58,  229,   45,  228,  216,   63,  221,
 /*   640 */   155,  198,  112,   69,  230,  252,   36,  237,  198,  120,
 /*   650 */   209,  230,  202,  205,   32,   84,   64,  207,  107,  202,
 /*   660 */   205,  259,   84,  229,  207,  190,  198,  109,  256,  230,
 /*   670 */   229,  256,  256,  198,  122,  256,  230,  202,  205,  256,
 /*   680 */    84,  256,  207,  256,  202,  205,  256,   84,  229,  207,
 /*   690 */   198,  101,  256,  230,  256,  229,  256,  256,  256,  256,
 /*   700 */   256,  202,  205,  256,   84,  256,  207,  256,  256,  256,
 /*   710 */   256,  256,  229,  256,  256,  256,  198,   47,  256,  230,
 /*   720 */   256,  256,  256,  198,   93,  256,  230,  202,  205,  256,
 /*   730 */    84,  256,  207,  256,  202,  205,  256,   84,  229,  207,
 /*   740 */   256,  198,  123,  256,  230,  229,  256,  256,  198,   44,
 /*   750 */   256,  151,  202,  205,  256,   84,  256,  207,  256,  202,
 /*   760 */   205,  256,   84,  229,  207,  198,  110,  256,  230,  256,
 /*   770 */   229,  256,  256,  256,  256,  256,  202,  205,  256,   84,
 /*   780 */   256,  207,  256,  256,  256,  256,  256,  229,  256,  256,
 /*   790 */   256,  198,  121,  256,  230,  256,  256,  256,  198,  116,
 /*   800 */   256,  230,  202,  205,  256,   84,  256,  207,  256,  202,
 /*   810 */   205,  256,   84,  229,  207,  256,  198,   95,  256,  230,
 /*   820 */   229,  256,  256,  198,  256,  256,  230,  202,  205,  256,
 /*   830 */    84,  256,  207,  256,  202,  205,  256,   85,  229,  207,
 /*   840 */   198,  256,  256,  230,  256,  229,  256,  256,  256,  256,
 /*   850 */   256,  202,  205,  256,   82,  256,  207,  256,  256,  256,
 /*   860 */   256,  256,  229,  256,  256,  256,  198,  256,  256,  230,
 /*   870 */   256,  256,  256,  256,  256,  256,  256,  202,  205,  256,
 /*   880 */    83,  256,  207,  256,  256,  256,  256,  256,  229,
    );
    static public $yy_lookahead = array(
 /*     0 */     4,   11,   25,   13,   25,   15,   29,   17,   30,   19,
 /*    10 */    31,   32,   33,   34,   35,   36,   37,   38,    3,    3,
 /*    20 */    30,   69,   70,   71,   72,   12,   13,   19,   50,   39,
 /*    30 */    14,   18,   42,   43,   44,   45,   40,   41,   30,   49,
 /*    40 */    61,   26,   26,    3,    4,   30,   30,   51,   52,   53,
 /*    50 */    54,   55,   56,   57,   58,   59,   60,   76,   76,   78,
 /*    60 */    47,   40,   41,   21,   16,   50,    3,   25,   87,   87,
 /*    70 */    30,   25,   51,   52,   53,   54,   55,   56,   57,   58,
 /*    80 */    59,   60,   19,   15,   17,   12,   13,   20,   40,   41,
 /*    90 */    11,   23,   13,   30,   15,   28,   17,   18,   19,   51,
 /*   100 */    52,   53,   54,   55,   56,   57,   58,   59,   60,   30,
 /*   110 */     4,   21,   49,   24,   24,    3,   16,   73,   12,   13,
 /*   120 */    47,   42,   43,   44,   45,   25,   95,   15,   49,   40,
 /*   130 */    41,   31,   32,   33,   34,   35,   36,   37,   38,   78,
 /*   140 */    51,   52,   53,   54,   55,   56,   57,   58,   59,   60,
 /*   150 */     3,  107,   11,   47,   13,    3,   15,   15,   17,   74,
 /*   160 */    19,   61,   77,    1,    2,    3,   15,    5,    6,    7,
 /*   170 */    95,   30,   10,   26,   23,   74,   74,   30,   77,   77,
 /*   180 */    39,   96,   30,   42,   43,   44,   45,   85,   74,   75,
 /*   190 */    49,   77,   90,   92,   80,   81,   82,   96,   96,   85,
 /*   200 */    86,   11,   88,   13,   90,   15,    1,   17,    3,   19,
 /*   210 */    96,   19,    1,   94,    3,  101,   97,   11,   99,   13,
 /*   220 */    30,   15,   30,   17,   62,   19,   64,   65,   66,    1,
 /*   230 */    19,    3,   42,   43,   44,   45,   30,   83,   11,   49,
 /*   240 */    13,   49,   15,   78,   17,    1,   19,    3,   42,   43,
 /*   250 */    44,   45,   98,   42,   11,   49,   13,   30,   15,   48,
 /*   260 */    17,    1,   19,    3,    3,    4,   15,  102,   63,   42,
 /*   270 */    43,   44,   45,   30,   23,    1,   49,    3,   74,   19,
 /*   280 */    29,   77,    1,    9,    3,   42,   43,   44,   45,   85,
 /*   290 */    86,   30,   49,   94,   90,   67,   97,   11,   99,   13,
 /*   300 */    96,   15,   42,   17,   19,   19,   99,   15,   48,    3,
 /*   310 */    95,   74,   75,   11,   77,   23,   30,   15,   26,   17,
 /*   320 */     4,   19,   85,   86,   43,   88,   79,   90,   42,   43,
 /*   330 */    44,   45,   30,   96,   49,   49,   30,   74,   28,   83,
 /*   340 */    77,  104,  105,   27,   42,   43,   44,   45,   85,    3,
 /*   350 */    95,   49,   74,   90,   98,   77,   50,   11,   15,   96,
 /*   360 */    18,   74,   75,   21,   77,   19,   23,   80,   25,   76,
 /*   370 */    27,   28,   85,   86,   96,   88,   30,   90,   74,   75,
 /*   380 */    87,   77,   16,   96,   80,   27,   28,   48,  101,   85,
 /*   390 */    86,   25,   88,  100,   90,   49,   74,   75,   74,   77,
 /*   400 */    96,   77,   80,   74,   75,  101,   77,   85,   86,   80,
 /*   410 */    88,   16,   90,   17,   85,   86,   20,   88,   96,   90,
 /*   420 */    96,   74,   75,  101,   77,   96,   15,   80,    4,   18,
 /*   430 */   101,   30,   85,   86,   23,   88,   76,   90,   78,   74,
 /*   440 */    75,    4,   77,   96,   30,   80,    4,   87,  101,   25,
 /*   450 */    85,   86,   76,   88,   78,   90,   74,   75,    4,   77,
 /*   460 */    22,   96,   80,   87,   27,   27,  101,   85,   86,   27,
 /*   470 */    88,    4,   90,   95,   74,   75,   98,   77,   96,   25,
 /*   480 */    80,   74,   75,  101,   77,   85,   86,   80,   88,   30,
 /*   490 */    90,    3,   85,   86,   27,   88,   96,   90,    4,   74,
 /*   500 */    75,  101,   77,   96,    4,   80,    4,    4,  101,   91,
 /*   510 */    85,   86,   11,   88,   74,   90,   15,   77,   30,   25,
 /*   520 */    19,   96,   71,   72,  106,   85,  101,   27,   25,   27,
 /*   530 */    90,   30,   83,   25,   30,   27,   96,   74,   75,   83,
 /*   540 */    77,   30,   73,   42,   43,   44,   45,   98,   85,   86,
 /*   550 */    49,   88,   94,   90,   98,   74,   75,   99,   77,   96,
 /*   560 */     1,   30,    3,   83,    4,   83,   85,   86,  105,   88,
 /*   570 */    73,   90,   74,   75,   93,   77,  107,   96,   98,   84,
 /*   580 */    98,   76,   73,   85,   86,   94,   88,   16,   90,  103,
 /*   590 */    99,   93,   87,   98,   96,   74,   75,   22,   77,   30,
 /*   600 */     4,   15,   43,   16,  107,    8,   85,   86,   30,   88,
 /*   610 */    30,   90,   74,   75,   93,   77,  107,   96,   46,   19,
 /*   620 */    30,    4,   46,   85,   86,   48,   88,   26,   90,   11,
 /*   630 */    26,   93,   20,   19,   96,   79,   97,   49,   19,  107,
 /*   640 */    27,   74,   75,   92,   77,   87,   89,   98,   74,   75,
 /*   650 */   106,   77,   85,   86,   22,   88,   19,   90,   95,   85,
 /*   660 */    86,  100,   88,   96,   90,   81,   74,   75,  108,   77,
 /*   670 */    96,  108,  108,   74,   75,  108,   77,   85,   86,  108,
 /*   680 */    88,  108,   90,  108,   85,   86,  108,   88,   96,   90,
 /*   690 */    74,   75,  108,   77,  108,   96,  108,  108,  108,  108,
 /*   700 */   108,   85,   86,  108,   88,  108,   90,  108,  108,  108,
 /*   710 */   108,  108,   96,  108,  108,  108,   74,   75,  108,   77,
 /*   720 */   108,  108,  108,   74,   75,  108,   77,   85,   86,  108,
 /*   730 */    88,  108,   90,  108,   85,   86,  108,   88,   96,   90,
 /*   740 */   108,   74,   75,  108,   77,   96,  108,  108,   74,   75,
 /*   750 */   108,   77,   85,   86,  108,   88,  108,   90,  108,   85,
 /*   760 */    86,  108,   88,   96,   90,   74,   75,  108,   77,  108,
 /*   770 */    96,  108,  108,  108,  108,  108,   85,   86,  108,   88,
 /*   780 */   108,   90,  108,  108,  108,  108,  108,   96,  108,  108,
 /*   790 */   108,   74,   75,  108,   77,  108,  108,  108,   74,   75,
 /*   800 */   108,   77,   85,   86,  108,   88,  108,   90,  108,   85,
 /*   810 */    86,  108,   88,   96,   90,  108,   74,   75,  108,   77,
 /*   820 */    96,  108,  108,   74,  108,  108,   77,   85,   86,  108,
 /*   830 */    88,  108,   90,  108,   85,   86,  108,   88,   96,   90,
 /*   840 */    74,  108,  108,   77,  108,   96,  108,  108,  108,  108,
 /*   850 */   108,   85,   86,  108,   88,  108,   90,  108,  108,  108,
 /*   860 */   108,  108,   96,  108,  108,  108,   74,  108,  108,   77,
 /*   870 */   108,  108,  108,  108,  108,  108,  108,   85,   86,  108,
 /*   880 */    88,  108,   90,  108,  108,  108,  108,  108,   96,
);
    const YY_SHIFT_USE_DFLT = -24;
    const YY_SHIFT_MAX = 169;
    static public $yy_shift_ofst = array(
 /*     0 */   162,  141,  -10,  -10,  -10,  -10,  -10,  -10,  -10,  -10,
 /*    10 */   -10,  -10,  286,  190,  286,  190,  190,  190,  190,  190,
 /*    20 */   190,  190,  190,  190,  190,  190,  190,  190,  190,  190,
 /*    30 */   206,   79,  243,  227,  302,  501,  501,  501,   63,  343,
 /*    40 */   346,   67,  192,   67,  508,  438,  285,  508,  162,  100,
 /*    50 */   -21,  211,   16,   15,  306,  281,  488,  152,  488,  244,
 /*    60 */   488,  152,  244,  488,  488,  358,  244,  613,  310,  310,
 /*    70 */    89,   48,   -4,   21,   21,   21,   21,   21,   21,   21,
 /*    80 */    21,  260,  106,   13,   73,   73,  274,  205,  261,  228,
 /*    90 */    40,  147,  559,  503,  396,  494,  -23,    8,   42,  396,
 /*   100 */   502,  366,  500,  467,  442,  316,  396,  396,  112,  424,
 /*   110 */   454,  437,   46,  632,  310,  637,   46,  310,  310,  142,
 /*   120 */    46,   46,   46,   46,  310,  -24,  -24,  -24,  -24,  -24,
 /*   130 */   292,  251,  411,  151,  -22,  342,   68,  151,   90,  619,
 /*   140 */   590,  617,  339,  600,  572,  597,  601,  576,  614,  588,
 /*   150 */   612,  604,  577,  618,  580,  578,  511,  531,  504,  459,
 /*   160 */   395,  401,  414,  560,  571,  586,  587,  596,  575,  569,
);
    const YY_REDUCE_USE_DFLT = -49;
    const YY_REDUCE_MAX = 129;
    static public $yy_reduce_ofst = array(
 /*     0 */   -48,  114,  382,  407,  400,  347,  322,  425,  329,  304,
 /*    10 */   287,  365,  237,  481,  463,  498,  521,  538,  616,  574,
 /*    20 */   649,  599,  724,  592,  691,  567,  642,  742,  717,  667,
 /*    30 */   674,  792,  749,  766,  204,  263,  102,  440,  101,  376,
 /*    40 */   324,  199,   85,  119,  -19,  293,  278,  360,  451,  165,
 /*    50 */   165,  418,  495,  378,  378,  509,  449,  256,  480,  469,
 /*    60 */   256,  482,   44,  154,  456,  -18,  497,  505,  491,  458,
 /*    70 */   486,  486,  486,  486,  486,  486,  486,  486,  486,  486,
 /*    80 */   486,  544,  557,  557,  557,  557,  532,  532,  549,  532,
 /*    90 */   549,  549,  532,   61,  539,   61,   61,  551,   61,  539,
 /*   100 */   558,   61,  558,  558,  558,  558,  539,  539,  563,   61,
 /*   110 */    61,  558,   61,  561,  207,  584,   61,  207,  207,   31,
 /*   120 */    61,   61,   61,   61,  207,   75,  556,  255,  215,  247,
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
        /* 41 */ array(17, 20, 28, ),
        /* 42 */ array(19, 30, 49, ),
        /* 43 */ array(17, 20, 28, ),
        /* 44 */ array(25, 27, ),
        /* 45 */ array(22, 27, ),
        /* 46 */ array(19, 49, ),
        /* 47 */ array(25, 27, ),
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
        /* 58 */ array(3, 30, ),
        /* 59 */ array(1, 3, ),
        /* 60 */ array(3, 30, ),
        /* 61 */ array(3, 30, ),
        /* 62 */ array(1, 3, ),
        /* 63 */ array(3, 30, ),
        /* 64 */ array(3, 30, ),
        /* 65 */ array(27, 28, ),
        /* 66 */ array(1, 3, ),
        /* 67 */ array(27, ),
        /* 68 */ array(28, ),
        /* 69 */ array(28, ),
        /* 70 */ array(24, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 71 */ array(16, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 72 */ array(4, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
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
        /* 84 */ array(12, 13, 47, ),
        /* 85 */ array(12, 13, 47, ),
        /* 86 */ array(1, 3, 9, ),
        /* 87 */ array(1, 3, 63, ),
        /* 88 */ array(3, 4, 30, ),
        /* 89 */ array(1, 3, 67, ),
        /* 90 */ array(3, 4, 30, ),
        /* 91 */ array(3, 26, 30, ),
        /* 92 */ array(1, 3, 43, ),
        /* 93 */ array(4, 25, ),
        /* 94 */ array(17, 20, ),
        /* 95 */ array(4, 25, ),
        /* 96 */ array(25, 29, ),
        /* 97 */ array(19, 30, ),
        /* 98 */ array(21, 25, ),
        /* 99 */ array(17, 20, ),
        /* 100 */ array(4, 27, ),
        /* 101 */ array(16, 25, ),
        /* 102 */ array(4, 27, ),
        /* 103 */ array(4, 27, ),
        /* 104 */ array(4, 27, ),
        /* 105 */ array(4, 27, ),
        /* 106 */ array(17, 20, ),
        /* 107 */ array(17, 20, ),
        /* 108 */ array(3, 15, ),
        /* 109 */ array(4, 25, ),
        /* 110 */ array(4, 25, ),
        /* 111 */ array(4, 27, ),
        /* 112 */ array(25, ),
        /* 113 */ array(22, ),
        /* 114 */ array(28, ),
        /* 115 */ array(19, ),
        /* 116 */ array(25, ),
        /* 117 */ array(28, ),
        /* 118 */ array(28, ),
        /* 119 */ array(15, ),
        /* 120 */ array(25, ),
        /* 121 */ array(25, ),
        /* 122 */ array(25, ),
        /* 123 */ array(25, ),
        /* 124 */ array(28, ),
        /* 125 */ array(),
        /* 126 */ array(),
        /* 127 */ array(),
        /* 128 */ array(),
        /* 129 */ array(),
        /* 130 */ array(15, 23, 26, ),
        /* 131 */ array(15, 23, 29, ),
        /* 132 */ array(15, 18, 23, ),
        /* 133 */ array(15, 23, ),
        /* 134 */ array(30, 50, ),
        /* 135 */ array(18, 21, ),
        /* 136 */ array(15, 23, ),
        /* 137 */ array(15, 23, ),
        /* 138 */ array(21, 24, ),
        /* 139 */ array(19, ),
        /* 140 */ array(30, ),
        /* 141 */ array(4, ),
        /* 142 */ array(48, ),
        /* 143 */ array(19, ),
        /* 144 */ array(46, ),
        /* 145 */ array(8, ),
        /* 146 */ array(26, ),
        /* 147 */ array(46, ),
        /* 148 */ array(19, ),
        /* 149 */ array(49, ),
        /* 150 */ array(20, ),
        /* 151 */ array(26, ),
        /* 152 */ array(48, ),
        /* 153 */ array(11, ),
        /* 154 */ array(30, ),
        /* 155 */ array(30, ),
        /* 156 */ array(30, ),
        /* 157 */ array(30, ),
        /* 158 */ array(30, ),
        /* 159 */ array(30, ),
        /* 160 */ array(16, ),
        /* 161 */ array(30, ),
        /* 162 */ array(30, ),
        /* 163 */ array(4, ),
        /* 164 */ array(16, ),
        /* 165 */ array(15, ),
        /* 166 */ array(16, ),
        /* 167 */ array(4, ),
        /* 168 */ array(22, ),
        /* 169 */ array(30, ),
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
        /* 272 */ array(),
);
    static public $yy_default = array(
 /*     0 */   419,  419,  419,  419,  419,  419,  419,  419,  419,  419,
 /*    10 */   419,  419,  404,  366,  419,  366,  366,  366,  419,  419,
 /*    20 */   419,  419,  419,  419,  419,  419,  419,  419,  419,  419,
 /*    30 */   419,  419,  419,  419,  419,  419,  419,  419,  419,  302,
 /*    40 */   419,  334,  419,  340,  302,  302,  419,  302,  273,  376,
 /*    50 */   376,  419,  419,  342,  342,  419,  419,  419,  419,  419,
 /*    60 */   419,  419,  419,  419,  419,  302,  419,  302,  330,  329,
 /*    70 */   419,  419,  419,  385,  390,  386,  381,  380,  389,  382,
 /*    80 */   374,  419,  419,  419,  308,  371,  419,  419,  419,  419,
 /*    90 */   419,  419,  419,  419,  358,  419,  405,  419,  365,  359,
 /*   100 */   419,  419,  419,  419,  419,  419,  360,  357,  342,  419,
 /*   110 */   419,  419,  377,  310,  332,  419,  303,  335,  354,  342,
 /*   120 */   296,  406,  306,  407,  331,  342,  370,  342,  342,  370,
 /*   130 */   307,  307,  419,  372,  419,  419,  419,  307,  419,  419,
 /*   140 */   419,  419,  419,  419,  311,  419,  419,  312,  419,  419,
 /*   150 */   319,  336,  419,  419,  419,  419,  419,  419,  419,  419,
 /*   160 */   419,  419,  419,  419,  419,  333,  419,  304,  352,  419,
 /*   170 */   274,  292,  393,  379,  299,  391,  392,  291,  298,  297,
 /*   180 */   378,  400,  375,  399,  388,  384,  387,  294,  373,  396,
 /*   190 */   305,  395,  397,  383,  398,  295,  394,  363,  318,  320,
 /*   200 */   321,  313,  312,  344,  345,  311,  322,  323,  327,  408,
 /*   210 */   410,  326,  325,  356,  314,  343,  339,  418,  416,  278,
 /*   220 */   417,  415,  275,  276,  277,  279,  280,  337,  341,  338,
 /*   230 */   336,  283,  281,  282,  411,  412,  346,  351,  352,  324,
 /*   240 */   355,  347,  349,  361,  284,  285,  288,  289,  290,  287,
 /*   250 */   301,  286,  300,  348,  409,  317,  367,  368,  316,  369,
 /*   260 */   315,  309,  350,  401,  403,  413,  414,  328,  362,  364,
 /*   270 */   402,  353,  293,
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
    const YYNSTATE = 273;
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
 /*  36 */ "expr ::= DOLLAR ID COLON ID",
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
  array( 'lhs' => 75, 'rhs' => 4 ),
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
#line 1825 "internal.templateparser.php"
#line 79 "internal.templateparser.y"
    function yy_r1(){$this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1828 "internal.templateparser.php"
#line 81 "internal.templateparser.y"
    function yy_r2(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1831 "internal.templateparser.php"
#line 87 "internal.templateparser.y"
    function yy_r3(){if ($this->compiler->has_code) {
                                            $tmp =''; foreach ($this->prefix_code as $code) {$tmp.=$code;} $this->prefix_code=array();
                                            $this->_retvalue = $this->cacher->processNocacheCode($tmp.$this->yystack[$this->yyidx + 0]->minor, $this->compiler,$this->nocache,true);
                                         } $this->nocache=false;    }
#line 1837 "internal.templateparser.php"
#line 92 "internal.templateparser.y"
    function yy_r4(){ $this->_retvalue = '';    }
#line 1840 "internal.templateparser.php"
#line 95 "internal.templateparser.y"
    function yy_r5(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + -1]->minor, $this->compiler,false,false);    }
#line 1843 "internal.templateparser.php"
#line 97 "internal.templateparser.y"
    function yy_r6(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->left_delimiter, $this->compiler,false,false);    }
#line 1846 "internal.templateparser.php"
#line 99 "internal.templateparser.y"
    function yy_r7(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->right_delimiter, $this->compiler,false,false);    }
#line 1849 "internal.templateparser.php"
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
#line 1860 "internal.templateparser.php"
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
#line 1871 "internal.templateparser.php"
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
#line 1882 "internal.templateparser.php"
#line 131 "internal.templateparser.y"
    function yy_r11(){$this->_retvalue = $this->cacher->processNocacheCode("<?php echo '".$this->yystack[$this->yyidx + 0]->minor."';?>\n", $this->compiler, true, true);    }
#line 1885 "internal.templateparser.php"
#line 133 "internal.templateparser.y"
    function yy_r12(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + 0]->minor, $this->compiler,false,false);    }
#line 1888 "internal.templateparser.php"
#line 140 "internal.templateparser.y"
    function yy_r13(){ $this->_retvalue = $this->compiler->compileTag('print_expression',array_merge(array('value'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1891 "internal.templateparser.php"
#line 142 "internal.templateparser.y"
    function yy_r14(){ $this->_retvalue = $this->compiler->compileTag('assign',array_merge(array('value'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -4]->minor,$this->yystack[$this->yyidx + -1]->minor));    }
#line 1894 "internal.templateparser.php"
#line 144 "internal.templateparser.y"
    function yy_r15(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor,$this->yystack[$this->yyidx + -1]->minor);    }
#line 1897 "internal.templateparser.php"
#line 146 "internal.templateparser.y"
    function yy_r16(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -4]->minor,array_merge(array('object_methode'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1900 "internal.templateparser.php"
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
#line 1915 "internal.templateparser.php"
#line 162 "internal.templateparser.y"
    function yy_r18(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor.'close',$this->yystack[$this->yyidx + -1]->minor);    }
#line 1918 "internal.templateparser.php"
#line 164 "internal.templateparser.y"
    function yy_r19(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor.'close',array('object_methode'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1921 "internal.templateparser.php"
#line 166 "internal.templateparser.y"
    function yy_r20(){if (!in_array($this->yystack[$this->yyidx + -3]->minor,array('if','elseif','while'))) {
                                                            $this->compiler->trigger_template_error ("wrong syntax for tag \"" . $this->yystack[$this->yyidx + -3]->minor . "\""); 
                                                            }
                                                           $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor,array('if condition'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1927 "internal.templateparser.php"
#line 170 "internal.templateparser.y"
    function yy_r21(){ if (!in_array($this->yystack[$this->yyidx + -3]->minor,array('if','elseif','while'))) {
                                                            $this->compiler->trigger_template_error ("wrong syntax for tag \"" . $this->yystack[$this->yyidx + -3]->minor . "\""); 
                                                            }
                                                           $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor,array('if condition'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1933 "internal.templateparser.php"
#line 175 "internal.templateparser.y"
    function yy_r22(){
                                                            if ($this->yystack[$this->yyidx + -9]->minor != 'for') {
                                                               $this->compiler->trigger_template_error ("wrong syntax for tag \"" . $this->yystack[$this->yyidx + -9]->minor . "\""); 
                                                            }
                                                            $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -9]->minor,array('start'=>$this->yystack[$this->yyidx + -7]->minor,'ifexp'=>$this->yystack[$this->yyidx + -5]->minor,'varloop'=>$this->yystack[$this->yyidx + -2]->minor,'loop'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1940 "internal.templateparser.php"
#line 180 "internal.templateparser.y"
    function yy_r23(){ $this->_retvalue = '='.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1943 "internal.templateparser.php"
#line 181 "internal.templateparser.y"
    function yy_r24(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1946 "internal.templateparser.php"
#line 183 "internal.templateparser.y"
    function yy_r25(){
                                                            if ($this->yystack[$this->yyidx + -6]->minor != 'foreach') {
                                                               $this->compiler->trigger_template_error ("wrong syntax for tag \"" . $this->yystack[$this->yyidx + -6]->minor . "\""); 
                                                            }
                                                            $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -6]->minor,array('from'=>$this->yystack[$this->yyidx + -4]->minor,'item'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1953 "internal.templateparser.php"
#line 188 "internal.templateparser.y"
    function yy_r26(){ 
                                                            if ($this->yystack[$this->yyidx + -6]->minor != 'foreach') {
                                                               $this->compiler->trigger_template_error ("wrong syntax for tag \"" . $this->yystack[$this->yyidx + -6]->minor . "\""); 
                                                            }
                                                            $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -6]->minor,array('from'=>$this->yystack[$this->yyidx + -4]->minor,'item'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1960 "internal.templateparser.php"
#line 198 "internal.templateparser.y"
    function yy_r27(){ $this->_retvalue = array_merge($this->yystack[$this->yyidx + -1]->minor,$this->yystack[$this->yyidx + 0]->minor);    }
#line 1963 "internal.templateparser.php"
#line 202 "internal.templateparser.y"
    function yy_r29(){ $this->_retvalue = array();    }
#line 1966 "internal.templateparser.php"
#line 205 "internal.templateparser.y"
    function yy_r30(){ $this->_retvalue = array($this->yystack[$this->yyidx + -2]->minor=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1969 "internal.templateparser.php"
#line 210 "internal.templateparser.y"
    function yy_r31(){ $this->_retvalue = array($this->yystack[$this->yyidx + 0]->minor);    }
#line 1972 "internal.templateparser.php"
#line 211 "internal.templateparser.y"
    function yy_r32(){ $this->yystack[$this->yyidx + -2]->minor[]=$this->yystack[$this->yyidx + 0]->minor; $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor;    }
#line 1975 "internal.templateparser.php"
#line 213 "internal.templateparser.y"
    function yy_r33(){ $this->_retvalue = array('var' => $this->yystack[$this->yyidx + -2]->minor, 'value'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1978 "internal.templateparser.php"
#line 219 "internal.templateparser.y"
    function yy_r34(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';     }
#line 1981 "internal.templateparser.php"
#line 223 "internal.templateparser.y"
    function yy_r36(){$this->_retvalue = '$_smarty_tpl->getStreamVariable(\''. $this->yystack[$this->yyidx + -2]->minor .'://'. $this->yystack[$this->yyidx + 0]->minor . '\')';    }
#line 1984 "internal.templateparser.php"
#line 224 "internal.templateparser.y"
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
#line 1999 "internal.templateparser.php"
#line 242 "internal.templateparser.y"
    function yy_r40(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2002 "internal.templateparser.php"
#line 244 "internal.templateparser.y"
    function yy_r41(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor . $this->yystack[$this->yyidx + -1]->minor . $this->yystack[$this->yyidx + 0]->minor;     }
#line 2005 "internal.templateparser.php"
#line 246 "internal.templateparser.y"
    function yy_r42(){ $this->_retvalue = '('. $this->yystack[$this->yyidx + -2]->minor . ').(' . $this->yystack[$this->yyidx + 0]->minor. ')';     }
#line 2008 "internal.templateparser.php"
#line 263 "internal.templateparser.y"
    function yy_r47(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2011 "internal.templateparser.php"
#line 272 "internal.templateparser.y"
    function yy_r51(){ $this->_retvalue = "(". $this->yystack[$this->yyidx + -1]->minor .")";     }
#line 2014 "internal.templateparser.php"
#line 275 "internal.templateparser.y"
    function yy_r52(){ $this->_retvalue = "'".$this->yystack[$this->yyidx + -1]->minor."'";     }
#line 2017 "internal.templateparser.php"
#line 276 "internal.templateparser.y"
    function yy_r53(){ $this->_retvalue = "''";     }
#line 2020 "internal.templateparser.php"
#line 278 "internal.templateparser.y"
    function yy_r54(){ $this->_retvalue = "'".str_replace('\"','"',$this->yystack[$this->yyidx + -1]->minor)."'";     }
#line 2023 "internal.templateparser.php"
#line 282 "internal.templateparser.y"
    function yy_r56(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2026 "internal.templateparser.php"
#line 283 "internal.templateparser.y"
    function yy_r57(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -3]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -6]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -1]->minor .')';     }
#line 2029 "internal.templateparser.php"
#line 285 "internal.templateparser.y"
    function yy_r58(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor.'::'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2032 "internal.templateparser.php"
#line 286 "internal.templateparser.y"
    function yy_r59(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -4]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -7]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -2]->minor .')'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2035 "internal.templateparser.php"
#line 288 "internal.templateparser.y"
    function yy_r60(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2038 "internal.templateparser.php"
#line 290 "internal.templateparser.y"
    function yy_r61(){ $this->_retvalue = $this->yystack[$this->yyidx + -4]->minor.'::$'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2041 "internal.templateparser.php"
#line 292 "internal.templateparser.y"
    function yy_r62(){ $this->_retvalue = $this->yystack[$this->yyidx + -5]->minor.'::$'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2044 "internal.templateparser.php"
#line 299 "internal.templateparser.y"
    function yy_r63(){ if ($this->yystack[$this->yyidx + 0]->minor['var'] == '\'smarty\'') { $this->_retvalue =  $this->compiler->compileTag('internal_smarty_var',$this->yystack[$this->yyidx + 0]->minor['index']);} else {
                                                         $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + 0]->minor['var'] .')->value'.$this->yystack[$this->yyidx + 0]->minor['index']; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + 0]->minor['var'],"'"))->nocache;}    }
#line 2048 "internal.templateparser.php"
#line 302 "internal.templateparser.y"
    function yy_r64(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->'.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 2051 "internal.templateparser.php"
#line 306 "internal.templateparser.y"
    function yy_r66(){$this->_retvalue = '$_smarty_tpl->getConfigVariable(\''. $this->yystack[$this->yyidx + -1]->minor .'\')';    }
#line 2054 "internal.templateparser.php"
#line 309 "internal.templateparser.y"
    function yy_r67(){$this->_retvalue = array('var'=>$this->yystack[$this->yyidx + -1]->minor, 'index'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 2057 "internal.templateparser.php"
#line 317 "internal.templateparser.y"
    function yy_r69(){return;    }
#line 2060 "internal.templateparser.php"
#line 321 "internal.templateparser.y"
    function yy_r70(){ $this->_retvalue = "['". $this->yystack[$this->yyidx + 0]->minor ."']";    }
#line 2063 "internal.templateparser.php"
#line 322 "internal.templateparser.y"
    function yy_r71(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + 0]->minor ."]";    }
#line 2066 "internal.templateparser.php"
#line 323 "internal.templateparser.y"
    function yy_r72(){ $this->_retvalue = "[".$this->yystack[$this->yyidx + 0]->minor."]";    }
#line 2069 "internal.templateparser.php"
#line 324 "internal.templateparser.y"
    function yy_r73(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + -1]->minor ."]";    }
#line 2072 "internal.templateparser.php"
#line 326 "internal.templateparser.y"
    function yy_r74(){ $this->_retvalue = '['.$this->compiler->compileTag('internal_smarty_var','[\'section\'][\''.$this->yystack[$this->yyidx + -1]->minor.'\'][\'index\']').']';    }
#line 2075 "internal.templateparser.php"
#line 330 "internal.templateparser.y"
    function yy_r76(){$this->_retvalue = '';    }
#line 2078 "internal.templateparser.php"
#line 338 "internal.templateparser.y"
    function yy_r78(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2081 "internal.templateparser.php"
#line 340 "internal.templateparser.y"
    function yy_r79(){$this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2084 "internal.templateparser.php"
#line 342 "internal.templateparser.y"
    function yy_r80(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2087 "internal.templateparser.php"
#line 347 "internal.templateparser.y"
    function yy_r81(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->value'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 2090 "internal.templateparser.php"
#line 349 "internal.templateparser.y"
    function yy_r82(){$this->_retvalue  = $this->yystack[$this->yyidx + 0]->minor;     }
#line 2093 "internal.templateparser.php"
#line 351 "internal.templateparser.y"
    function yy_r83(){$this->_retvalue  = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2096 "internal.templateparser.php"
#line 353 "internal.templateparser.y"
    function yy_r84(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2099 "internal.templateparser.php"
#line 354 "internal.templateparser.y"
    function yy_r85(){ $this->_retvalue = '->{'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2102 "internal.templateparser.php"
#line 355 "internal.templateparser.y"
    function yy_r86(){ $this->_retvalue = '->{'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2105 "internal.templateparser.php"
#line 356 "internal.templateparser.y"
    function yy_r87(){ $this->_retvalue = '->{\''.$this->yystack[$this->yyidx + -4]->minor.'\'.'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2108 "internal.templateparser.php"
#line 358 "internal.templateparser.y"
    function yy_r88(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2111 "internal.templateparser.php"
#line 364 "internal.templateparser.y"
    function yy_r89(){if (!$this->template->security || $this->smarty->security_handler->isTrustedPhpFunction($this->yystack[$this->yyidx + -3]->minor, $this->compiler)) {
																					            if ($this->yystack[$this->yyidx + -3]->minor == 'isset' || $this->yystack[$this->yyidx + -3]->minor == 'empty' || $this->yystack[$this->yyidx + -3]->minor == 'array' || is_callable($this->yystack[$this->yyidx + -3]->minor)) {
																					                $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";
																					            } else {
                                                       $this->compiler->trigger_template_error ("unknown function \"" . $this->yystack[$this->yyidx + -3]->minor . "\"");
                                                      }
                                                    }    }
#line 2120 "internal.templateparser.php"
#line 375 "internal.templateparser.y"
    function yy_r90(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";    }
#line 2123 "internal.templateparser.php"
#line 379 "internal.templateparser.y"
    function yy_r91(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.",".$this->yystack[$this->yyidx + 0]->minor;    }
#line 2126 "internal.templateparser.php"
#line 383 "internal.templateparser.y"
    function yy_r93(){ return;    }
#line 2129 "internal.templateparser.php"
#line 388 "internal.templateparser.y"
    function yy_r94(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,true);    }
#line 2132 "internal.templateparser.php"
#line 389 "internal.templateparser.y"
    function yy_r95(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,false);    }
#line 2135 "internal.templateparser.php"
#line 396 "internal.templateparser.y"
    function yy_r96(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2138 "internal.templateparser.php"
#line 400 "internal.templateparser.y"
    function yy_r98(){$this->_retvalue = ','.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2141 "internal.templateparser.php"
#line 401 "internal.templateparser.y"
    function yy_r99(){$this->_retvalue = ',\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2144 "internal.templateparser.php"
#line 408 "internal.templateparser.y"
    function yy_r101(){$this->_retvalue = '!'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2147 "internal.templateparser.php"
#line 413 "internal.templateparser.y"
    function yy_r103(){$this->_retvalue =$this->yystack[$this->yyidx + 0]->minor;    }
#line 2150 "internal.templateparser.php"
#line 414 "internal.templateparser.y"
    function yy_r104(){$this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2153 "internal.templateparser.php"
#line 415 "internal.templateparser.y"
    function yy_r105(){$this->_retvalue = 'in_array('.$this->yystack[$this->yyidx + -2]->minor.','.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2156 "internal.templateparser.php"
#line 416 "internal.templateparser.y"
    function yy_r106(){$this->_retvalue = 'in_array('.$this->yystack[$this->yyidx + -2]->minor.',(array)'.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2159 "internal.templateparser.php"
#line 418 "internal.templateparser.y"
    function yy_r108(){$this->_retvalue = '!('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2162 "internal.templateparser.php"
#line 419 "internal.templateparser.y"
    function yy_r109(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2165 "internal.templateparser.php"
#line 420 "internal.templateparser.y"
    function yy_r110(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2168 "internal.templateparser.php"
#line 421 "internal.templateparser.y"
    function yy_r111(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2171 "internal.templateparser.php"
#line 422 "internal.templateparser.y"
    function yy_r112(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2174 "internal.templateparser.php"
#line 423 "internal.templateparser.y"
    function yy_r113(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2177 "internal.templateparser.php"
#line 429 "internal.templateparser.y"
    function yy_r118(){$this->_retvalue = '==';    }
#line 2180 "internal.templateparser.php"
#line 430 "internal.templateparser.y"
    function yy_r119(){$this->_retvalue = '!=';    }
#line 2183 "internal.templateparser.php"
#line 431 "internal.templateparser.y"
    function yy_r120(){$this->_retvalue = '>';    }
#line 2186 "internal.templateparser.php"
#line 432 "internal.templateparser.y"
    function yy_r121(){$this->_retvalue = '<';    }
#line 2189 "internal.templateparser.php"
#line 433 "internal.templateparser.y"
    function yy_r122(){$this->_retvalue = '>=';    }
#line 2192 "internal.templateparser.php"
#line 434 "internal.templateparser.y"
    function yy_r123(){$this->_retvalue = '<=';    }
#line 2195 "internal.templateparser.php"
#line 435 "internal.templateparser.y"
    function yy_r124(){$this->_retvalue = '===';    }
#line 2198 "internal.templateparser.php"
#line 436 "internal.templateparser.y"
    function yy_r125(){$this->_retvalue = '!==';    }
#line 2201 "internal.templateparser.php"
#line 438 "internal.templateparser.y"
    function yy_r126(){$this->_retvalue = '&&';    }
#line 2204 "internal.templateparser.php"
#line 439 "internal.templateparser.y"
    function yy_r127(){$this->_retvalue = '||';    }
#line 2207 "internal.templateparser.php"
#line 444 "internal.templateparser.y"
    function yy_r128(){ $this->_retvalue = 'array('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2210 "internal.templateparser.php"
#line 446 "internal.templateparser.y"
    function yy_r130(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.','.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2213 "internal.templateparser.php"
#line 447 "internal.templateparser.y"
    function yy_r131(){ return;     }
#line 2216 "internal.templateparser.php"
#line 449 "internal.templateparser.y"
    function yy_r133(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2219 "internal.templateparser.php"
#line 450 "internal.templateparser.y"
    function yy_r134(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + -2]->minor.'\'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2222 "internal.templateparser.php"
#line 457 "internal.templateparser.y"
    function yy_r137(){$this->_retvalue = "`".$this->yystack[$this->yyidx + -1]->minor."`";    }
#line 2225 "internal.templateparser.php"
#line 458 "internal.templateparser.y"
    function yy_r138(){$this->_retvalue = "'.".$this->yystack[$this->yyidx + -1]->minor.".'";    }
#line 2228 "internal.templateparser.php"
#line 459 "internal.templateparser.y"
    function yy_r139(){$this->_retvalue = "'.".'$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + 0]->minor .'\')->value'.".'"; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + 0]->minor,"'"))->nocache;    }
#line 2231 "internal.templateparser.php"
#line 460 "internal.templateparser.y"
    function yy_r140(){$this->_retvalue = "'.(".$this->yystack[$this->yyidx + -1]->minor.").'";    }
#line 2234 "internal.templateparser.php"
#line 461 "internal.templateparser.y"
    function yy_r141(){$this->_retvalue = addcslashes($this->yystack[$this->yyidx + 0]->minor,"'");    }
#line 2237 "internal.templateparser.php"

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
#line 2355 "internal.templateparser.php"
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
#line 2380 "internal.templateparser.php"
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
