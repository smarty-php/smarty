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
    const YY_NO_ACTION = 394;
    const YY_ACCEPT_ACTION = 393;
    const YY_ERROR_ACTION = 392;

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
    const YY_SZ_ACTTAB = 921;
static public $yy_action = array(
 /*     0 */   134,  240,  217,  137,   29,  206,  216,  200,   45,  139,
 /*    10 */   133,   56,  125,  177,  125,  180,  160,  163,  229,  230,
 /*    20 */   234,  235,  202,  238,  237,  236,  242,  172,  165,  169,
 /*    30 */   168,    4,   10,   11,    9,    5,    8,  160,  163,   41,
 /*    40 */    30,  212,  211,  121,  166,  189,  149,  214,  172,  165,
 /*    50 */   169,  168,    4,   10,   11,    9,    5,    8,  393,   46,
 /*    60 */   184,  187,   58,   87,  197,  199,   55,  119,  160,  163,
 /*    70 */   177,  177,  180,  180,  207,   59,   34,  117,  195,  172,
 /*    80 */   165,  169,  168,    4,   10,   11,    9,    5,    8,  160,
 /*    90 */   163,  170,  109,   36,  190,    3,  221,   12,  125,   54,
 /*   100 */   172,  165,  169,  168,    4,   10,   11,    9,    5,    8,
 /*   110 */   128,  113,  175,   19,    2,   40,  212,  211,  177,    6,
 /*   120 */   180,   82,   37,   51,  167,  164,  182,  206,   14,  146,
 /*   130 */    19,  170,  188,   36,  176,   27,  125,   12,  223,   54,
 /*   140 */   194,  239,  229,  230,  234,  235,  202,  238,  237,  236,
 /*   150 */   123,   34,  111,   21,  190,  158,  221,  194,  193,  125,
 /*   160 */   142,  139,   37,   51,  167,  164,  173,  158,   48,  146,
 /*   170 */   193,  177,  155,  180,   24,   65,  130,  252,  159,  161,
 /*   180 */   232,  179,   78,  193,  155,   30,   16,   31,  121,  252,
 /*   190 */   204,  170,  139,   36,  162,    3,   94,   12,   38,   61,
 /*   200 */    32,  170,  252,   36,  150,   27,   89,   12,  226,   54,
 /*   210 */   124,  125,  205,  174,   19,   99,   60,  207,   15,    6,
 /*   220 */   128,  215,   37,   51,  167,  164,   93,  192,   94,  146,
 /*   230 */   248,  139,   37,   51,  167,  164,  170,   21,   36,  146,
 /*   240 */    27,  194,   12,  227,   54,  170,  125,   36,   19,   27,
 /*   250 */    22,   12,  125,   54,  170,  127,   36,   33,   27,  198,
 /*   260 */    12,  142,   54,  208,  129,   18,  139,   37,   51,  167,
 /*   270 */   164,   28,   15,   39,  146,  194,   37,   51,  167,  164,
 /*   280 */    93,   54,   19,  146,  244,   37,   51,  167,  164,  170,
 /*   290 */    15,   36,  146,   27,  120,   12,  209,   54,   93,   49,
 /*   300 */   125,   86,    1,  138,  157,  120,  183,  139,  122,  194,
 /*   310 */    44,  213,  207,  158,   91,  158,  193,  125,  193,  239,
 /*   320 */    37,   51,  167,  164,  159,  161,  156,  146,   78,  139,
 /*   330 */   155,  220,  155,  243,  139,  252,   17,  252,  158,   48,
 /*   340 */   208,  193,   18,  126,  228,  232,   66,   13,  193,  159,
 /*   350 */   161,  125,  125,   78,  177,  155,  180,  154,   54,  201,
 /*   360 */   252,  158,   47,  158,  193,  162,  193,  252,  152,   67,
 /*   370 */   135,   15,  159,  161,  140,  131,   78,  231,  155,   93,
 /*   380 */   155,  203,   23,  252,  176,  252,  219,   44,  162,  147,
 /*   390 */   158,   48,  193,  193,  212,  211,  145,  101,   72,  193,
 /*   400 */   170,  159,  161,   97,   27,   78,   12,  155,   54,   54,
 /*   410 */    43,  252,  252,  158,   48,  107,  193,  162,  252,  129,
 /*   420 */   151,   71,   95,   80,  159,  161,  246,   79,   78,   34,
 /*   430 */   155,   37,   51,  167,  164,  252,  158,   48,  146,  193,
 /*   440 */   162,  108,  239,   84,   68,  221,   81,  159,  161,  158,
 /*   450 */    54,   78,  193,  155,   15,  104,  120,  181,  252,  221,
 /*   460 */   250,  181,   93,  162,  158,   48,  155,  193,   26,  186,
 /*   470 */   187,  252,   70,  210,   52,  159,  161,  181,   20,   78,
 /*   480 */   181,  155,   88,   50,  191,  100,  252,  158,   48,  144,
 /*   490 */   193,  162,  241,  207,  239,   75,  207,  118,  159,  161,
 /*   500 */   233,  112,   78,  239,  155,    7,   64,   16,  196,  252,
 /*   510 */   158,   48,  171,  193,  162,  224,   53,   25,   74,  185,
 /*   520 */   249,  159,  161,   57,   63,   78,  218,  155,   23,  251,
 /*   530 */   141,  132,  252,  176,   35,  139,  178,  162,  158,   48,
 /*   540 */   225,  193,  190,   31,   90,   62,   69,  248,  222,  159,
 /*   550 */   161,  248,  248,   78,  248,  155,  248,  248,  248,  248,
 /*   560 */   252,  158,   48,  248,  193,  162,  248,  170,  248,   73,
 /*   570 */   248,   27,  159,  161,  248,   54,   78,  248,  155,  248,
 /*   580 */   248,  248,  248,  252,  248,  248,  129,  248,  162,  248,
 /*   590 */   248,  248,  158,   92,  248,  193,  248,  248,   37,   51,
 /*   600 */   167,  164,  248,  159,  161,  146,  248,   78,  248,  155,
 /*   610 */   248,  248,  143,  248,  252,  158,   91,  248,  193,  248,
 /*   620 */   248,  248,  248,  248,  248,  248,  159,  161,  248,  248,
 /*   630 */    78,  248,  155,  248,  248,  248,  248,  252,  158,   92,
 /*   640 */   248,  193,  248,  248,  248,  248,  245,  248,  248,  159,
 /*   650 */   161,  248,  248,   78,  248,  155,  248,  248,  148,  248,
 /*   660 */   252,  248,  158,   92,  248,  193,  248,  248,  248,  248,
 /*   670 */   248,  248,  248,  159,  161,  158,   92,   78,  193,  155,
 /*   680 */   248,  248,  153,  248,  252,  248,  159,  161,  158,   42,
 /*   690 */    78,  136,  155,  248,  248,  247,  248,  252,  248,  159,
 /*   700 */   161,  158,  114,   78,  193,  155,  248,  248,  248,  248,
 /*   710 */   252,  248,  159,  161,  248,  248,   78,  248,  155,  248,
 /*   720 */   248,  248,  248,  252,  158,  110,  248,  193,  248,  248,
 /*   730 */   248,  248,  248,  248,  248,  159,  161,  158,  116,   78,
 /*   740 */   193,  155,  248,  248,  248,  248,  252,  248,  159,  161,
 /*   750 */   158,   98,   78,  193,  155,  248,  248,  248,  248,  252,
 /*   760 */   248,  159,  161,  158,  105,   78,  193,  155,  248,  248,
 /*   770 */   248,  248,  252,  248,  159,  161,  158,   85,   78,  193,
 /*   780 */   155,  248,  248,  248,  248,  252,  248,  159,  161,  248,
 /*   790 */   248,   78,  248,  155,  248,  248,  248,  248,  252,  158,
 /*   800 */   115,  248,  193,  248,  248,  248,  248,  248,  248,  248,
 /*   810 */   159,  161,  158,  103,   78,  193,  155,  248,  248,  248,
 /*   820 */   248,  252,  248,  159,  161,  158,  106,   78,  193,  155,
 /*   830 */   248,  248,  248,  248,  252,  248,  159,  161,  158,  102,
 /*   840 */    78,  193,  155,  248,  248,  248,  248,  252,  248,  159,
 /*   850 */   161,  158,   96,   78,  193,  155,  248,  248,  248,  248,
 /*   860 */   252,  248,  159,  161,  248,  248,   78,  158,  155,  248,
 /*   870 */   193,  248,  248,  252,  248,  248,  248,  248,  159,  161,
 /*   880 */   248,  248,   77,  248,  155,  158,  248,  248,  193,  252,
 /*   890 */   248,  248,  248,  248,  248,  248,  159,  161,  158,  248,
 /*   900 */    83,  193,  155,  248,  248,  248,  248,  252,  248,  159,
 /*   910 */   161,  248,  248,   76,  248,  155,  248,  248,  248,  248,
 /*   920 */   252,
    );
    static public $yy_lookahead = array(
 /*     0 */    24,    4,    1,    2,    3,   16,    5,    6,    7,   27,
 /*    10 */    28,   10,   25,    1,   25,    3,   40,   41,   31,   32,
 /*    20 */    33,   34,   35,   36,   37,   38,   30,   51,   52,   53,
 /*    30 */    54,   55,   56,   57,   58,   59,   60,   40,   41,   78,
 /*    40 */    17,   12,   13,   20,   16,   87,   50,   18,   51,   52,
 /*    50 */    53,   54,   55,   56,   57,   58,   59,   60,   68,   69,
 /*    60 */    70,   71,   61,   75,   63,   64,   65,   79,   40,   41,
 /*    70 */     1,    1,    3,    3,   86,   19,   47,    4,   66,   51,
 /*    80 */    52,   53,   54,   55,   56,   57,   58,   59,   60,   40,
 /*    90 */    41,   11,   94,   13,   96,   15,   98,   17,   25,   19,
 /*   100 */    51,   52,   53,   54,   55,   56,   57,   58,   59,   60,
 /*   110 */    30,   21,   43,    3,   24,   78,   12,   13,    1,   39,
 /*   120 */     3,   77,   42,   43,   44,   45,    9,   16,   15,   49,
 /*   130 */     3,   11,   62,   13,   97,   15,   25,   17,   18,   19,
 /*   140 */    30,   97,   31,   32,   33,   34,   35,   36,   37,   38,
 /*   150 */    30,   47,   94,   26,   96,   73,   98,   30,   76,   25,
 /*   160 */    50,   27,   42,   43,   44,   45,   84,   73,   74,   49,
 /*   170 */    76,    1,   90,    3,    3,   81,   82,   95,   84,   85,
 /*   180 */    73,   87,   88,   76,   90,   17,   15,   22,   20,   95,
 /*   190 */     4,   11,   27,   13,  100,   15,   28,   17,   91,   19,
 /*   200 */     3,   11,   95,   13,   19,   15,   75,   17,    4,   19,
 /*   210 */    30,   25,  105,   43,    3,   30,   19,   86,   15,   39,
 /*   220 */    30,   18,   42,   43,   44,   45,   23,   30,   28,   49,
 /*   230 */    99,   27,   42,   43,   44,   45,   11,   26,   13,   49,
 /*   240 */    15,   30,   17,    4,   19,   11,   25,   13,    3,   15,
 /*   250 */    29,   17,   25,   19,   11,   30,   13,   46,   15,   14,
 /*   260 */    17,   50,   19,    1,   30,    3,   27,   42,   43,   44,
 /*   270 */    45,   26,   15,   30,   49,   30,   42,   43,   44,   45,
 /*   280 */    23,   19,    3,   49,    4,   42,   43,   44,   45,   11,
 /*   290 */    15,   13,   49,   15,   79,   17,    4,   19,   23,   77,
 /*   300 */    25,   75,   27,   28,   42,   79,    4,   27,   30,   30,
 /*   310 */    48,    4,   86,   73,   74,   73,   76,   25,   76,   97,
 /*   320 */    42,   43,   44,   45,   84,   85,   84,   49,   88,   27,
 /*   330 */    90,    4,   90,   18,   27,   95,   21,   95,   73,   74,
 /*   340 */     1,   76,    3,  103,  104,   73,   81,   21,   76,   84,
 /*   350 */    85,   25,   25,   88,    1,   90,    3,   19,   19,   98,
 /*   360 */    95,   73,   74,   73,   76,  100,   76,   95,   30,   81,
 /*   370 */    83,   15,   84,   85,   84,   85,   88,  105,   90,   23,
 /*   380 */    90,   42,   26,   95,   97,   95,    4,   48,  100,   73,
 /*   390 */    73,   74,   76,   76,   12,   13,   73,   78,   81,   76,
 /*   400 */    11,   84,   85,   78,   15,   88,   17,   90,   19,   19,
 /*   410 */    80,   95,   95,   73,   74,   80,   76,  100,   95,   30,
 /*   420 */    30,   81,   77,   72,   84,   85,   30,   72,   88,   47,
 /*   430 */    90,   42,   43,   44,   45,   95,   73,   74,   49,   76,
 /*   440 */   100,   94,   97,   72,   81,   98,   72,   84,   85,   73,
 /*   450 */    19,   88,   76,   90,   15,   94,   79,  106,   95,   98,
 /*   460 */    84,  106,   23,  100,   73,   74,   90,   76,   29,   70,
 /*   470 */    71,   95,   81,   16,   77,   84,   85,  106,  101,   88,
 /*   480 */   106,   90,   75,   77,    8,   75,   95,   73,   74,   30,
 /*   490 */    76,  100,   48,   86,   97,   81,   86,   30,   84,   85,
 /*   500 */    48,   30,   88,   97,   90,  102,   16,   15,    4,   95,
 /*   510 */    73,   74,   49,   76,  100,   16,   19,   26,   81,    4,
 /*   520 */     4,   84,   85,   30,   30,   88,   30,   90,   26,    4,
 /*   530 */    30,   30,   95,   97,   89,   27,  106,  100,   73,   74,
 /*   540 */    86,   76,   96,   22,   78,   92,   81,   99,   92,   84,
 /*   550 */    85,  107,  107,   88,  107,   90,  107,  107,  107,  107,
 /*   560 */    95,   73,   74,  107,   76,  100,  107,   11,  107,   81,
 /*   570 */   107,   15,   84,   85,  107,   19,   88,  107,   90,  107,
 /*   580 */   107,  107,  107,   95,  107,  107,   30,  107,  100,  107,
 /*   590 */   107,  107,   73,   74,  107,   76,  107,  107,   42,   43,
 /*   600 */    44,   45,  107,   84,   85,   49,  107,   88,  107,   90,
 /*   610 */   107,  107,   93,  107,   95,   73,   74,  107,   76,  107,
 /*   620 */   107,  107,  107,  107,  107,  107,   84,   85,  107,  107,
 /*   630 */    88,  107,   90,  107,  107,  107,  107,   95,   73,   74,
 /*   640 */   107,   76,  107,  107,  107,  107,  104,  107,  107,   84,
 /*   650 */    85,  107,  107,   88,  107,   90,  107,  107,   93,  107,
 /*   660 */    95,  107,   73,   74,  107,   76,  107,  107,  107,  107,
 /*   670 */   107,  107,  107,   84,   85,   73,   74,   88,   76,   90,
 /*   680 */   107,  107,   93,  107,   95,  107,   84,   85,   73,   74,
 /*   690 */    88,   76,   90,  107,  107,   93,  107,   95,  107,   84,
 /*   700 */    85,   73,   74,   88,   76,   90,  107,  107,  107,  107,
 /*   710 */    95,  107,   84,   85,  107,  107,   88,  107,   90,  107,
 /*   720 */   107,  107,  107,   95,   73,   74,  107,   76,  107,  107,
 /*   730 */   107,  107,  107,  107,  107,   84,   85,   73,   74,   88,
 /*   740 */    76,   90,  107,  107,  107,  107,   95,  107,   84,   85,
 /*   750 */    73,   74,   88,   76,   90,  107,  107,  107,  107,   95,
 /*   760 */   107,   84,   85,   73,   74,   88,   76,   90,  107,  107,
 /*   770 */   107,  107,   95,  107,   84,   85,   73,   74,   88,   76,
 /*   780 */    90,  107,  107,  107,  107,   95,  107,   84,   85,  107,
 /*   790 */   107,   88,  107,   90,  107,  107,  107,  107,   95,   73,
 /*   800 */    74,  107,   76,  107,  107,  107,  107,  107,  107,  107,
 /*   810 */    84,   85,   73,   74,   88,   76,   90,  107,  107,  107,
 /*   820 */   107,   95,  107,   84,   85,   73,   74,   88,   76,   90,
 /*   830 */   107,  107,  107,  107,   95,  107,   84,   85,   73,   74,
 /*   840 */    88,   76,   90,  107,  107,  107,  107,   95,  107,   84,
 /*   850 */    85,   73,   74,   88,   76,   90,  107,  107,  107,  107,
 /*   860 */    95,  107,   84,   85,  107,  107,   88,   73,   90,  107,
 /*   870 */    76,  107,  107,   95,  107,  107,  107,  107,   84,   85,
 /*   880 */   107,  107,   88,  107,   90,   73,  107,  107,   76,   95,
 /*   890 */   107,  107,  107,  107,  107,  107,   84,   85,   73,  107,
 /*   900 */    88,   76,   90,  107,  107,  107,  107,   95,  107,   84,
 /*   910 */    85,  107,  107,   88,  107,   90,  107,  107,  107,  107,
 /*   920 */    95,
);
    const YY_SHIFT_USE_DFLT = -25;
    const YY_SHIFT_MAX = 154;
    static public $yy_shift_ofst = array(
 /*     0 */     1,  180,   80,   80,   80,   80,   80,   80,   80,   80,
 /*    10 */    80,   80,  278,  190,  190,  190,  190,  278,  190,  190,
 /*    20 */   190,  190,  190,  190,  190,  190,  190,  190,  190,  243,
 /*    30 */   120,  225,  234,  389,  556,  556,  556,  339,  262,  275,
 /*    40 */   168,  168,  134,  165,  390,  431,    1,  111,  -13,  211,
 /*    50 */   245,  170,  110,  279,  279,  353,  353,  -18,  353,  279,
 /*    60 */   279,  279,  200,  508,  200,   -3,  -24,   28,   49,   49,
 /*    70 */    49,   49,   49,   49,   49,   49,  382,   29,  104,   70,
 /*    80 */    12,  117,  127,  104,   69,   73,  204,  307,  302,  280,
 /*    90 */    23,  221,  326,  338,  185,  279,  327,   23,  292,  171,
 /*   100 */   239,   23,  186,  -11,  200,  227,  227,  521,  200,  200,
 /*   110 */   227,  200,  113,   56,  227,  227,  227,  -25,  -25,  -25,
 /*   120 */   -25,  197,  439,  203,  356,   -4,  315,  257,  257,  257,
 /*   130 */    90,  516,  515,  501,  497,  504,  491,  493,  494,  500,
 /*   140 */   525,  502,  496,  499,  463,  444,  459,  476,  457,  396,
 /*   150 */   467,  452,  492,  490,  471,
);
    const YY_REDUCE_USE_DFLT = -43;
    const YY_REDUCE_MAX = 120;
    static public $yy_reduce_ofst = array(
 /*     0 */   -10,   94,  265,  288,  391,  363,  414,  437,  465,  340,
 /*    10 */   488,  317,  240,  602,  589,  565,  519,  542,  677,  778,
 /*    20 */   664,  651,  628,  690,  703,  765,  752,  739,  726,  615,
 /*    30 */   794,  812,  825,  290,  376,   82,  242,  107,  272,  -12,
 /*    40 */    58,   -2,  226,  131,  323,  316,  399,  377,  377,   37,
 /*    50 */   287,  371,   37,  406,  397,  351,  374,  407,  355,   44,
 /*    60 */   345,  222,  361,  410,  347,  403,  403,  403,  403,  403,
 /*    70 */   403,  403,  403,  403,  403,  403,  445,  445,  445,  430,
 /*    80 */   430,  430,  436,  445,  430,  215,  454,  454,  454,  454,
 /*    90 */   446,  215,  215,  453,  456,  436,  215,  446,  215,  466,
 /*   100 */   454,  446,  215,  215,  261,  215,  215,  448,  261,  261,
 /*   110 */   215,  261,  -39,  -42,  215,  215,  215,  319,  325,  330,
 /*   120 */   335,
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
        /* 30 */ array(11, 13, 15, 17, 18, 19, 30, 42, 43, 44, 45, 49, ),
        /* 31 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 32 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 33 */ array(11, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 34 */ array(11, 15, 19, 30, 42, 43, 44, 45, 49, ),
        /* 35 */ array(11, 15, 19, 30, 42, 43, 44, 45, 49, ),
        /* 36 */ array(11, 15, 19, 30, 42, 43, 44, 45, 49, ),
        /* 37 */ array(1, 3, 19, 42, 48, ),
        /* 38 */ array(1, 3, 19, 42, 48, ),
        /* 39 */ array(15, 23, 25, 27, 28, ),
        /* 40 */ array(17, 20, 28, ),
        /* 41 */ array(17, 20, 28, ),
        /* 42 */ array(25, 27, ),
        /* 43 */ array(22, 27, ),
        /* 44 */ array(19, 30, ),
        /* 45 */ array(19, ),
        /* 46 */ array(1, 2, 3, 5, 6, 7, 10, 61, 63, 64, 65, ),
        /* 47 */ array(16, 25, 31, 32, 33, 34, 35, 36, 37, 38, ),
        /* 48 */ array(25, 31, 32, 33, 34, 35, 36, 37, 38, ),
        /* 49 */ array(3, 26, 30, 46, 50, ),
        /* 50 */ array(3, 14, 26, 30, ),
        /* 51 */ array(1, 3, 43, ),
        /* 52 */ array(3, 30, 50, ),
        /* 53 */ array(3, 30, ),
        /* 54 */ array(3, 30, ),
        /* 55 */ array(1, 3, ),
        /* 56 */ array(1, 3, ),
        /* 57 */ array(27, 28, ),
        /* 58 */ array(1, 3, ),
        /* 59 */ array(3, 30, ),
        /* 60 */ array(3, 30, ),
        /* 61 */ array(3, 30, ),
        /* 62 */ array(28, ),
        /* 63 */ array(27, ),
        /* 64 */ array(28, ),
        /* 65 */ array(4, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 66 */ array(24, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 67 */ array(16, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 68 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 69 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 70 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 71 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 72 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 73 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 74 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 75 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 76 */ array(4, 12, 13, 47, ),
        /* 77 */ array(12, 13, 18, 47, ),
        /* 78 */ array(12, 13, 47, ),
        /* 79 */ array(1, 3, 62, ),
        /* 80 */ array(1, 3, 66, ),
        /* 81 */ array(1, 3, 9, ),
        /* 82 */ array(3, 26, 30, ),
        /* 83 */ array(12, 13, 47, ),
        /* 84 */ array(1, 3, 43, ),
        /* 85 */ array(4, 25, ),
        /* 86 */ array(4, 27, ),
        /* 87 */ array(4, 27, ),
        /* 88 */ array(4, 27, ),
        /* 89 */ array(4, 27, ),
        /* 90 */ array(17, 20, ),
        /* 91 */ array(25, 29, ),
        /* 92 */ array(21, 25, ),
        /* 93 */ array(19, 30, ),
        /* 94 */ array(19, 30, ),
        /* 95 */ array(3, 30, ),
        /* 96 */ array(4, 25, ),
        /* 97 */ array(17, 20, ),
        /* 98 */ array(4, 25, ),
        /* 99 */ array(3, 15, ),
        /* 100 */ array(4, 27, ),
        /* 101 */ array(17, 20, ),
        /* 102 */ array(4, 25, ),
        /* 103 */ array(16, 25, ),
        /* 104 */ array(28, ),
        /* 105 */ array(25, ),
        /* 106 */ array(25, ),
        /* 107 */ array(22, ),
        /* 108 */ array(28, ),
        /* 109 */ array(28, ),
        /* 110 */ array(25, ),
        /* 111 */ array(28, ),
        /* 112 */ array(15, ),
        /* 113 */ array(19, ),
        /* 114 */ array(25, ),
        /* 115 */ array(25, ),
        /* 116 */ array(25, ),
        /* 117 */ array(),
        /* 118 */ array(),
        /* 119 */ array(),
        /* 120 */ array(),
        /* 121 */ array(3, 19, 30, ),
        /* 122 */ array(15, 23, 29, ),
        /* 123 */ array(15, 18, 23, ),
        /* 124 */ array(15, 23, 26, ),
        /* 125 */ array(30, 50, ),
        /* 126 */ array(18, 21, ),
        /* 127 */ array(15, 23, ),
        /* 128 */ array(15, 23, ),
        /* 129 */ array(15, 23, ),
        /* 130 */ array(21, 24, ),
        /* 131 */ array(4, ),
        /* 132 */ array(4, ),
        /* 133 */ array(30, ),
        /* 134 */ array(19, ),
        /* 135 */ array(4, ),
        /* 136 */ array(26, ),
        /* 137 */ array(30, ),
        /* 138 */ array(30, ),
        /* 139 */ array(30, ),
        /* 140 */ array(4, ),
        /* 141 */ array(26, ),
        /* 142 */ array(30, ),
        /* 143 */ array(16, ),
        /* 144 */ array(49, ),
        /* 145 */ array(48, ),
        /* 146 */ array(30, ),
        /* 147 */ array(8, ),
        /* 148 */ array(16, ),
        /* 149 */ array(30, ),
        /* 150 */ array(30, ),
        /* 151 */ array(48, ),
        /* 152 */ array(15, ),
        /* 153 */ array(16, ),
        /* 154 */ array(30, ),
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
);
    static public $yy_default = array(
 /*     0 */   392,  392,  392,  392,  392,  392,  392,  392,  392,  392,
 /*    10 */   392,  392,  377,  341,  341,  341,  341,  392,  392,  392,
 /*    20 */   392,  392,  392,  392,  392,  392,  392,  392,  392,  392,
 /*    30 */   392,  392,  392,  392,  392,  392,  392,  392,  392,  282,
 /*    40 */   268,  313,  282,  282,  392,  392,  253,  351,  351,  319,
 /*    50 */   392,  392,  319,  392,  392,  392,  392,  282,  392,  392,
 /*    60 */   392,  392,  308,  282,  309,  392,  392,  392,  354,  355,
 /*    70 */   362,  359,  358,  363,  353,  349,  392,  392,  288,  392,
 /*    80 */   392,  392,  392,  346,  392,  392,  392,  392,  392,  392,
 /*    90 */   333,  378,  340,  392,  392,  321,  392,  334,  392,  319,
 /*   100 */   392,  335,  392,  392,  310,  283,  380,  289,  311,  314,
 /*   110 */   286,  330,  319,  392,  379,  276,  352,  319,  319,  345,
 /*   120 */   345,  392,  287,  392,  287,  392,  392,  347,  287,  392,
 /*   130 */   392,  392,  392,  392,  392,  392,  315,  392,  392,  392,
 /*   140 */   392,  392,  392,  392,  392,  392,  392,  392,  392,  392,
 /*   150 */   392,  392,  312,  392,  392,  302,  292,  306,  297,  291,
 /*   160 */   372,  290,  348,  373,  301,  361,  350,  300,  357,  356,
 /*   170 */   299,  298,  360,  293,  305,  304,  327,  390,  388,  284,
 /*   180 */   391,  389,  257,  272,  254,  273,  255,  256,  258,  285,
 /*   190 */   318,  263,  320,  315,  328,  262,  275,  259,  277,  260,
 /*   200 */   261,  332,  368,  307,  267,  382,  303,  281,  387,  386,
 /*   210 */   337,  295,  296,  269,  324,  323,  264,  265,  316,  322,
 /*   220 */   329,  331,  336,  325,  338,  280,  266,  270,  375,  364,
 /*   230 */   365,  381,  383,  384,  366,  367,  371,  370,  369,  326,
 /*   240 */   274,  385,  343,  374,  271,  376,  342,  339,  344,  279,
 /*   250 */   294,  278,  317,
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
    const YYNSTATE = 253;
    const YYNRULE = 139;
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
 /*  14 */ "smartytag ::= LDEL varindexed EQUAL expr RDEL",
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
 /*  45 */ "value ::= HATCH ID HATCH",
 /*  46 */ "value ::= NUMBER",
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
 /*  65 */ "arrayindex ::= arrayindex indexdef",
 /*  66 */ "arrayindex ::=",
 /*  67 */ "indexdef ::= DOT ID",
 /*  68 */ "indexdef ::= DOT DOLLAR varvar",
 /*  69 */ "indexdef ::= DOT LDEL exprs RDEL",
 /*  70 */ "indexdef ::= OPENB ID CLOSEB",
 /*  71 */ "indexdef ::= OPENB exprs CLOSEB",
 /*  72 */ "indexdef ::= OPENB CLOSEB",
 /*  73 */ "varvar ::= varvarele",
 /*  74 */ "varvar ::= varvar varvarele",
 /*  75 */ "varvarele ::= ID",
 /*  76 */ "varvarele ::= LDEL expr RDEL",
 /*  77 */ "object ::= DOLLAR varvar arrayindex objectchain",
 /*  78 */ "objectchain ::= objectelement",
 /*  79 */ "objectchain ::= objectchain objectelement",
 /*  80 */ "objectelement ::= PTR ID arrayindex",
 /*  81 */ "objectelement ::= PTR DOLLAR ID arrayindex",
 /*  82 */ "objectelement ::= PTR ID LDEL expr RDEL arrayindex",
 /*  83 */ "objectelement ::= PTR method",
 /*  84 */ "function ::= ID OPENP params CLOSEP",
 /*  85 */ "method ::= ID OPENP params CLOSEP",
 /*  86 */ "params ::= expr COMMA params",
 /*  87 */ "params ::= expr",
 /*  88 */ "params ::=",
 /*  89 */ "modifier ::= VERT AT ID",
 /*  90 */ "modifier ::= VERT ID",
 /*  91 */ "modparameters ::= modparameters modparameter",
 /*  92 */ "modparameters ::=",
 /*  93 */ "modparameter ::= COLON exprs",
 /*  94 */ "modparameter ::= COLON ID",
 /*  95 */ "ifexprs ::= ifexpr",
 /*  96 */ "ifexprs ::= NOT ifexprs",
 /*  97 */ "ifexprs ::= OPENP ifexprs CLOSEP",
 /*  98 */ "ifexpr ::= expr",
 /*  99 */ "ifexpr ::= expr ifcond expr",
 /* 100 */ "ifexpr ::= ifexprs lop ifexprs",
 /* 101 */ "ifexpr ::= ifexprs ISDIVBY ifexprs",
 /* 102 */ "ifexpr ::= ifexprs ISNOTDIVBY ifexprs",
 /* 103 */ "ifexpr ::= ifexprs ISEVEN",
 /* 104 */ "ifexpr ::= ifexprs ISNOTEVEN",
 /* 105 */ "ifexpr ::= ifexprs ISEVENBY ifexprs",
 /* 106 */ "ifexpr ::= ifexprs ISNOTEVENBY ifexprs",
 /* 107 */ "ifexpr ::= ifexprs ISODD",
 /* 108 */ "ifexpr ::= ifexprs ISNOTODD",
 /* 109 */ "ifexpr ::= ifexprs ISODDBY ifexprs",
 /* 110 */ "ifexpr ::= ifexprs ISNOTODDBY ifexprs",
 /* 111 */ "ifcond ::= EQUALS",
 /* 112 */ "ifcond ::= NOTEQUALS",
 /* 113 */ "ifcond ::= GREATERTHAN",
 /* 114 */ "ifcond ::= LESSTHAN",
 /* 115 */ "ifcond ::= GREATEREQUAL",
 /* 116 */ "ifcond ::= LESSEQUAL",
 /* 117 */ "ifcond ::= IDENTITY",
 /* 118 */ "ifcond ::= NONEIDENTITY",
 /* 119 */ "lop ::= LAND",
 /* 120 */ "lop ::= LOR",
 /* 121 */ "array ::= OPENB arrayelements CLOSEB",
 /* 122 */ "arrayelements ::= arrayelement",
 /* 123 */ "arrayelements ::= arrayelements COMMA arrayelement",
 /* 124 */ "arrayelements ::=",
 /* 125 */ "arrayelement ::= expr",
 /* 126 */ "arrayelement ::= expr APTR expr",
 /* 127 */ "arrayelement ::= ID APTR expr",
 /* 128 */ "doublequoted ::= doublequoted doublequotedcontent",
 /* 129 */ "doublequoted ::= doublequotedcontent",
 /* 130 */ "doublequotedcontent ::= variable",
 /* 131 */ "doublequotedcontent ::= BACKTICK ID BACKTICK",
 /* 132 */ "doublequotedcontent ::= BACKTICK variable BACKTICK",
 /* 133 */ "doublequotedcontent ::= LDEL expr RDEL",
 /* 134 */ "doublequotedcontent ::= OTHER",
 /* 135 */ "text ::= text textelement",
 /* 136 */ "text ::= textelement",
 /* 137 */ "textelement ::= OTHER",
 /* 138 */ "textelement ::= LDEL",
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
  array( 'lhs' => 71, 'rhs' => 5 ),
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
  array( 'lhs' => 84, 'rhs' => 3 ),
  array( 'lhs' => 84, 'rhs' => 1 ),
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
  array( 'lhs' => 78, 'rhs' => 2 ),
  array( 'lhs' => 78, 'rhs' => 0 ),
  array( 'lhs' => 96, 'rhs' => 2 ),
  array( 'lhs' => 96, 'rhs' => 3 ),
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
  array( 'lhs' => 98, 'rhs' => 4 ),
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
        46 => 0,
        47 => 0,
        48 => 0,
        49 => 0,
        64 => 0,
        122 => 0,
        1 => 1,
        35 => 1,
        37 => 1,
        42 => 1,
        43 => 1,
        73 => 1,
        95 => 1,
        129 => 1,
        136 => 1,
        137 => 1,
        138 => 1,
        2 => 2,
        65 => 2,
        128 => 2,
        135 => 2,
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
        87 => 24,
        125 => 24,
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
        45 => 45,
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
        66 => 66,
        92 => 66,
        67 => 67,
        68 => 68,
        69 => 69,
        71 => 69,
        70 => 70,
        72 => 72,
        74 => 74,
        75 => 75,
        76 => 76,
        97 => 76,
        77 => 77,
        78 => 78,
        79 => 79,
        80 => 80,
        81 => 81,
        82 => 82,
        83 => 83,
        84 => 84,
        85 => 85,
        86 => 86,
        88 => 88,
        89 => 89,
        90 => 90,
        91 => 91,
        93 => 93,
        94 => 94,
        96 => 96,
        98 => 98,
        99 => 99,
        100 => 99,
        101 => 101,
        102 => 102,
        103 => 103,
        108 => 103,
        104 => 104,
        107 => 104,
        105 => 105,
        110 => 105,
        106 => 106,
        109 => 106,
        111 => 111,
        112 => 112,
        113 => 113,
        114 => 114,
        115 => 115,
        116 => 116,
        117 => 117,
        118 => 118,
        119 => 119,
        120 => 120,
        121 => 121,
        123 => 123,
        124 => 124,
        126 => 126,
        127 => 127,
        130 => 130,
        131 => 131,
        132 => 132,
        133 => 133,
        134 => 134,
    );
    /* Beginning here are the reduction cases.  A typical example
    ** follows:
    **  #line <lineno> <grammarfile>
    **   function yy_r0($yymsp){ ... }           // User supplied code
    **  #line <lineno> <thisfile>
    */
#line 73 "internal.templateparser.y"
    function yy_r0(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;     }
#line 1787 "internal.templateparser.php"
#line 79 "internal.templateparser.y"
    function yy_r1(){$this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1790 "internal.templateparser.php"
#line 81 "internal.templateparser.y"
    function yy_r2(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1793 "internal.templateparser.php"
#line 87 "internal.templateparser.y"
    function yy_r3(){if ($this->compiler->has_code) {
                                            $tmp =''; foreach ($this->prefix_code as $code) {$tmp.=$code;} $this->prefix_code=array();
                                            $this->_retvalue = $this->cacher->processNocacheCode($tmp.$this->yystack[$this->yyidx + 0]->minor, $this->compiler,$this->nocache,true);
                                         } $this->nocache=false;    }
#line 1799 "internal.templateparser.php"
#line 100 "internal.templateparser.y"
    function yy_r4(){ $this->_retvalue = '';    }
#line 1802 "internal.templateparser.php"
#line 103 "internal.templateparser.y"
    function yy_r5(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + -1]->minor, $this->compiler,false,false);    }
#line 1805 "internal.templateparser.php"
#line 105 "internal.templateparser.y"
    function yy_r6(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->left_delimiter, $this->compiler,false,false);    }
#line 1808 "internal.templateparser.php"
#line 107 "internal.templateparser.y"
    function yy_r7(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->right_delimiter, $this->compiler,false,false);    }
#line 1811 "internal.templateparser.php"
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
#line 1822 "internal.templateparser.php"
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
#line 1833 "internal.templateparser.php"
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
#line 1844 "internal.templateparser.php"
#line 139 "internal.templateparser.y"
    function yy_r11(){$this->_retvalue = $this->cacher->processNocacheCode("<?php echo '".$this->yystack[$this->yyidx + 0]->minor."';?>\n", $this->compiler, true, true);    }
#line 1847 "internal.templateparser.php"
#line 141 "internal.templateparser.y"
    function yy_r12(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + 0]->minor, $this->compiler,false,false);    }
#line 1850 "internal.templateparser.php"
#line 149 "internal.templateparser.y"
    function yy_r13(){ $this->_retvalue = $this->compiler->compileTag('print_expression',array_merge(array('value'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1853 "internal.templateparser.php"
#line 152 "internal.templateparser.y"
    function yy_r14(){ $this->_retvalue = $this->compiler->compileTag('assign',array_merge(array('value'=>$this->yystack[$this->yyidx + -1]->minor),$this->yystack[$this->yyidx + -3]->minor));    }
#line 1856 "internal.templateparser.php"
#line 154 "internal.templateparser.y"
    function yy_r15(){$this->_retvalue = array('var'=>$this->yystack[$this->yyidx + -1]->minor, 'index'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1859 "internal.templateparser.php"
#line 158 "internal.templateparser.y"
    function yy_r16(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor,$this->yystack[$this->yyidx + -1]->minor);    }
#line 1862 "internal.templateparser.php"
#line 160 "internal.templateparser.y"
    function yy_r17(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -4]->minor,array_merge(array('object_methode'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1865 "internal.templateparser.php"
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
#line 1880 "internal.templateparser.php"
#line 176 "internal.templateparser.y"
    function yy_r19(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor.'close',$this->yystack[$this->yyidx + -1]->minor);    }
#line 1883 "internal.templateparser.php"
#line 178 "internal.templateparser.y"
    function yy_r20(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor.'close',array('object_methode'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1886 "internal.templateparser.php"
#line 180 "internal.templateparser.y"
    function yy_r21(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor,array('if condition'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1889 "internal.templateparser.php"
#line 182 "internal.templateparser.y"
    function yy_r22(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -9]->minor,array('start'=>$this->yystack[$this->yyidx + -7]->minor,'ifexp'=>$this->yystack[$this->yyidx + -5]->minor,'varloop'=>$this->yystack[$this->yyidx + -2]->minor,'loop'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1892 "internal.templateparser.php"
#line 183 "internal.templateparser.y"
    function yy_r23(){ $this->_retvalue = '='.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1895 "internal.templateparser.php"
#line 184 "internal.templateparser.y"
    function yy_r24(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1898 "internal.templateparser.php"
#line 187 "internal.templateparser.y"
    function yy_r25(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -6]->minor,array('from'=>$this->yystack[$this->yyidx + -1]->minor,'item'=>$this->yystack[$this->yyidx + -3]->minor));    }
#line 1901 "internal.templateparser.php"
#line 194 "internal.templateparser.y"
    function yy_r27(){ $this->_retvalue = array_merge($this->yystack[$this->yyidx + -1]->minor,$this->yystack[$this->yyidx + 0]->minor);    }
#line 1904 "internal.templateparser.php"
#line 198 "internal.templateparser.y"
    function yy_r29(){ $this->_retvalue = array();    }
#line 1907 "internal.templateparser.php"
#line 202 "internal.templateparser.y"
    function yy_r30(){ $this->_retvalue = array($this->yystack[$this->yyidx + -2]->minor=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1910 "internal.templateparser.php"
#line 207 "internal.templateparser.y"
    function yy_r31(){ $this->_retvalue = array($this->yystack[$this->yyidx + 0]->minor);    }
#line 1913 "internal.templateparser.php"
#line 208 "internal.templateparser.y"
    function yy_r32(){ $this->yystack[$this->yyidx + -2]->minor[]=$this->yystack[$this->yyidx + 0]->minor; $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor;    }
#line 1916 "internal.templateparser.php"
#line 210 "internal.templateparser.y"
    function yy_r33(){ $this->_retvalue = array('var' => $this->yystack[$this->yyidx + -2]->minor, 'value'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1919 "internal.templateparser.php"
#line 217 "internal.templateparser.y"
    function yy_r34(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';     }
#line 1922 "internal.templateparser.php"
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
#line 1937 "internal.templateparser.php"
#line 239 "internal.templateparser.y"
    function yy_r39(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1940 "internal.templateparser.php"
#line 241 "internal.templateparser.y"
    function yy_r40(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor . $this->yystack[$this->yyidx + -1]->minor . $this->yystack[$this->yyidx + 0]->minor;     }
#line 1943 "internal.templateparser.php"
#line 243 "internal.templateparser.y"
    function yy_r41(){ $this->_retvalue = '('. $this->yystack[$this->yyidx + -2]->minor . ').(' . $this->yystack[$this->yyidx + 0]->minor. ')';     }
#line 1946 "internal.templateparser.php"
#line 257 "internal.templateparser.y"
    function yy_r45(){$this->_retvalue = '$_smarty_tpl->getConfigVariable(\''. $this->yystack[$this->yyidx + -1]->minor .'\')';    }
#line 1949 "internal.templateparser.php"
#line 268 "internal.templateparser.y"
    function yy_r50(){ $this->_retvalue = "(". $this->yystack[$this->yyidx + -1]->minor .")";     }
#line 1952 "internal.templateparser.php"
#line 271 "internal.templateparser.y"
    function yy_r51(){ $this->_retvalue = "'".$this->yystack[$this->yyidx + -1]->minor."'";     }
#line 1955 "internal.templateparser.php"
#line 272 "internal.templateparser.y"
    function yy_r52(){ $this->_retvalue = "''";     }
#line 1958 "internal.templateparser.php"
#line 274 "internal.templateparser.y"
    function yy_r53(){ $this->_retvalue = "'".str_replace('\"','"',$this->yystack[$this->yyidx + -1]->minor)."'";     }
#line 1961 "internal.templateparser.php"
#line 280 "internal.templateparser.y"
    function yy_r55(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1964 "internal.templateparser.php"
#line 281 "internal.templateparser.y"
    function yy_r56(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -3]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -6]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -1]->minor .')';     }
#line 1967 "internal.templateparser.php"
#line 283 "internal.templateparser.y"
    function yy_r57(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor.'::'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1970 "internal.templateparser.php"
#line 284 "internal.templateparser.y"
    function yy_r58(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -4]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -7]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -2]->minor .')'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1973 "internal.templateparser.php"
#line 286 "internal.templateparser.y"
    function yy_r59(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1976 "internal.templateparser.php"
#line 288 "internal.templateparser.y"
    function yy_r60(){ $this->_retvalue = $this->yystack[$this->yyidx + -4]->minor.'::$'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1979 "internal.templateparser.php"
#line 290 "internal.templateparser.y"
    function yy_r61(){ $this->_retvalue = $this->yystack[$this->yyidx + -5]->minor.'::$'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1982 "internal.templateparser.php"
#line 299 "internal.templateparser.y"
    function yy_r62(){ if ($this->yystack[$this->yyidx + 0]->minor['var'] == '\'smarty\'') { $this->_retvalue =  $this->compiler->compileTag(trim($this->yystack[$this->yyidx + 0]->minor['var'],"'"),$this->yystack[$this->yyidx + 0]->minor['index']);} else {
                                                         $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + 0]->minor['var'] .')->value'.$this->yystack[$this->yyidx + 0]->minor['index']; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + 0]->minor['var'],"'"))->nocache;}    }
#line 1986 "internal.templateparser.php"
#line 302 "internal.templateparser.y"
    function yy_r63(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->'.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 1989 "internal.templateparser.php"
#line 314 "internal.templateparser.y"
    function yy_r66(){return;    }
#line 1992 "internal.templateparser.php"
#line 318 "internal.templateparser.y"
    function yy_r67(){ $this->_retvalue = "['". $this->yystack[$this->yyidx + 0]->minor ."']";    }
#line 1995 "internal.templateparser.php"
#line 319 "internal.templateparser.y"
    function yy_r68(){ $this->_retvalue = "[". '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + 0]->minor .')->value'."]";    }
#line 1998 "internal.templateparser.php"
#line 320 "internal.templateparser.y"
    function yy_r69(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + -1]->minor ."]";    }
#line 2001 "internal.templateparser.php"
#line 322 "internal.templateparser.y"
    function yy_r70(){ $this->_retvalue = '['.$this->compiler->compileTag('smarty','[\'section\'][\''.$this->yystack[$this->yyidx + -1]->minor.'\'][\'index\']').']';    }
#line 2004 "internal.templateparser.php"
#line 326 "internal.templateparser.y"
    function yy_r72(){$this->_retvalue = '';    }
#line 2007 "internal.templateparser.php"
#line 334 "internal.templateparser.y"
    function yy_r74(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2010 "internal.templateparser.php"
#line 336 "internal.templateparser.y"
    function yy_r75(){$this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2013 "internal.templateparser.php"
#line 338 "internal.templateparser.y"
    function yy_r76(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2016 "internal.templateparser.php"
#line 343 "internal.templateparser.y"
    function yy_r77(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->value'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 2019 "internal.templateparser.php"
#line 345 "internal.templateparser.y"
    function yy_r78(){$this->_retvalue  = $this->yystack[$this->yyidx + 0]->minor;     }
#line 2022 "internal.templateparser.php"
#line 347 "internal.templateparser.y"
    function yy_r79(){$this->_retvalue  = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2025 "internal.templateparser.php"
#line 349 "internal.templateparser.y"
    function yy_r80(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2028 "internal.templateparser.php"
#line 350 "internal.templateparser.y"
    function yy_r81(){ $this->_retvalue = '->'.'{$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -1]->minor .'\')->value'.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2031 "internal.templateparser.php"
#line 352 "internal.templateparser.y"
    function yy_r82(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + -4]->minor.'{'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2034 "internal.templateparser.php"
#line 355 "internal.templateparser.y"
    function yy_r83(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2037 "internal.templateparser.php"
#line 361 "internal.templateparser.y"
    function yy_r84(){if (!$this->template->security || $this->smarty->security_handler->isTrustedPhpFunction($this->yystack[$this->yyidx + -3]->minor, $this->compiler)) {
																					            if ($this->yystack[$this->yyidx + -3]->minor == 'isset' || $this->yystack[$this->yyidx + -3]->minor == 'empty' || $this->yystack[$this->yyidx + -3]->minor == 'array' || is_callable($this->yystack[$this->yyidx + -3]->minor)) {
																					                $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";
																					            } else {
                                                       $this->compiler->trigger_template_error ("unknown function \"" . $this->yystack[$this->yyidx + -3]->minor . "\"");
                                                      }
                                                    }    }
#line 2046 "internal.templateparser.php"
#line 372 "internal.templateparser.y"
    function yy_r85(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";    }
#line 2049 "internal.templateparser.php"
#line 376 "internal.templateparser.y"
    function yy_r86(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.",".$this->yystack[$this->yyidx + 0]->minor;    }
#line 2052 "internal.templateparser.php"
#line 380 "internal.templateparser.y"
    function yy_r88(){ return;    }
#line 2055 "internal.templateparser.php"
#line 385 "internal.templateparser.y"
    function yy_r89(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,true);    }
#line 2058 "internal.templateparser.php"
#line 386 "internal.templateparser.y"
    function yy_r90(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,false);    }
#line 2061 "internal.templateparser.php"
#line 393 "internal.templateparser.y"
    function yy_r91(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2064 "internal.templateparser.php"
#line 397 "internal.templateparser.y"
    function yy_r93(){$this->_retvalue = ','.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2067 "internal.templateparser.php"
#line 398 "internal.templateparser.y"
    function yy_r94(){$this->_retvalue = ',\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2070 "internal.templateparser.php"
#line 405 "internal.templateparser.y"
    function yy_r96(){$this->_retvalue = '!'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2073 "internal.templateparser.php"
#line 410 "internal.templateparser.y"
    function yy_r98(){$this->_retvalue =$this->yystack[$this->yyidx + 0]->minor;    }
#line 2076 "internal.templateparser.php"
#line 411 "internal.templateparser.y"
    function yy_r99(){$this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2079 "internal.templateparser.php"
#line 413 "internal.templateparser.y"
    function yy_r101(){$this->_retvalue = '!('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2082 "internal.templateparser.php"
#line 414 "internal.templateparser.y"
    function yy_r102(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2085 "internal.templateparser.php"
#line 415 "internal.templateparser.y"
    function yy_r103(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2088 "internal.templateparser.php"
#line 416 "internal.templateparser.y"
    function yy_r104(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2091 "internal.templateparser.php"
#line 417 "internal.templateparser.y"
    function yy_r105(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2094 "internal.templateparser.php"
#line 418 "internal.templateparser.y"
    function yy_r106(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2097 "internal.templateparser.php"
#line 424 "internal.templateparser.y"
    function yy_r111(){$this->_retvalue = '==';    }
#line 2100 "internal.templateparser.php"
#line 425 "internal.templateparser.y"
    function yy_r112(){$this->_retvalue = '!=';    }
#line 2103 "internal.templateparser.php"
#line 426 "internal.templateparser.y"
    function yy_r113(){$this->_retvalue = '>';    }
#line 2106 "internal.templateparser.php"
#line 427 "internal.templateparser.y"
    function yy_r114(){$this->_retvalue = '<';    }
#line 2109 "internal.templateparser.php"
#line 428 "internal.templateparser.y"
    function yy_r115(){$this->_retvalue = '>=';    }
#line 2112 "internal.templateparser.php"
#line 429 "internal.templateparser.y"
    function yy_r116(){$this->_retvalue = '<=';    }
#line 2115 "internal.templateparser.php"
#line 430 "internal.templateparser.y"
    function yy_r117(){$this->_retvalue = '===';    }
#line 2118 "internal.templateparser.php"
#line 431 "internal.templateparser.y"
    function yy_r118(){$this->_retvalue = '!==';    }
#line 2121 "internal.templateparser.php"
#line 433 "internal.templateparser.y"
    function yy_r119(){$this->_retvalue = '&&';    }
#line 2124 "internal.templateparser.php"
#line 434 "internal.templateparser.y"
    function yy_r120(){$this->_retvalue = '||';    }
#line 2127 "internal.templateparser.php"
#line 439 "internal.templateparser.y"
    function yy_r121(){ $this->_retvalue = 'array('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2130 "internal.templateparser.php"
#line 441 "internal.templateparser.y"
    function yy_r123(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.','.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2133 "internal.templateparser.php"
#line 442 "internal.templateparser.y"
    function yy_r124(){ return;     }
#line 2136 "internal.templateparser.php"
#line 444 "internal.templateparser.y"
    function yy_r126(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2139 "internal.templateparser.php"
#line 445 "internal.templateparser.y"
    function yy_r127(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + -2]->minor.'\'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2142 "internal.templateparser.php"
#line 452 "internal.templateparser.y"
    function yy_r130(){$this->_retvalue = "'.".$this->yystack[$this->yyidx + 0]->minor.".'";    }
#line 2145 "internal.templateparser.php"
#line 453 "internal.templateparser.y"
    function yy_r131(){$this->_retvalue = "`".$this->yystack[$this->yyidx + -1]->minor."`";    }
#line 2148 "internal.templateparser.php"
#line 454 "internal.templateparser.y"
    function yy_r132(){$this->_retvalue = "'.".$this->yystack[$this->yyidx + -1]->minor.".'";    }
#line 2151 "internal.templateparser.php"
#line 455 "internal.templateparser.y"
    function yy_r133(){$this->_retvalue = "'.(".$this->yystack[$this->yyidx + -1]->minor.").'";    }
#line 2154 "internal.templateparser.php"
#line 456 "internal.templateparser.y"
    function yy_r134(){$this->_retvalue = addcslashes($this->yystack[$this->yyidx + 0]->minor,"'");    }
#line 2157 "internal.templateparser.php"

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
#line 2275 "internal.templateparser.php"
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
#line 2300 "internal.templateparser.php"
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
