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
    const YY_NO_ACTION = 402;
    const YY_ACCEPT_ACTION = 401;
    const YY_ERROR_ACTION = 400;

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
    const YY_SZ_ACTTAB = 915;
static public $yy_action = array(
 /*     0 */   196,  190,  191,  253,   28,  401,   49,  184,  187,  143,
 /*    10 */   185,  153,  159,  202,  163,  158,    3,   11,    5,    2,
 /*    20 */     9,    6,  154,  150,  190,  191,  155,   23,   36,  238,
 /*    30 */     7,  206,   12,  145,   62,  159,  202,  163,  158,    3,
 /*    40 */    11,    5,    2,    9,    6,  128,  106,   34,  194,  132,
 /*    50 */   219,  147,  226,  141,    8,  121,  160,   38,   55,  237,
 /*    60 */   232,  132,  208,  254,  140,  190,  191,  241,  239,  240,
 /*    70 */   257,  201,  164,  162,  174,  161,  159,  202,  163,  158,
 /*    80 */     3,   11,    5,    2,    9,    6,  145,  188,   31,  198,
 /*    90 */   133,   41,  255,  188,  165,  198,  155,  160,   36,   40,
 /*   100 */    30,  181,   12,  229,   62,  204,  190,  191,  221,  220,
 /*   110 */   112,  235,  194,  132,  219,  124,  161,  159,  202,  163,
 /*   120 */   158,    3,   11,    5,    2,    9,    6,   38,   55,  237,
 /*   130 */   232,  155,   14,   36,  140,    7,   99,   12,  165,   56,
 /*   140 */   101,  160,  132,   37,    1,  144,   65,  132,  199,  172,
 /*   150 */   125,   18,  214,  149,   29,  235,  215,  195,   46,    8,
 /*   160 */   161,   60,   38,   55,  237,  232,  188,  217,  198,  140,
 /*   170 */   165,   51,  188,  160,  198,  221,  220,  230,   68,  127,
 /*   180 */   160,  166,  171,  168,  182,   86,  160,  235,   61,  188,
 /*   190 */    64,  198,  161,   14,  155,   39,   36,  169,   30,  161,
 /*   200 */    12,  101,   62,  109,  188,  161,  198,   20,  251,  225,
 /*   210 */    37,  132,   58,  128,  200,  175,   63,  241,  239,  240,
 /*   220 */   257,  201,  164,  162,  174,   38,   55,  237,  232,  221,
 /*   230 */   220,   31,  140,   82,   41,  227,   32,  155,   22,   36,
 /*   240 */    14,   30,   97,   12,  167,   62,  247,  231,  101,   27,
 /*   250 */    15,   28,   62,  155,  192,   36,   42,   30,  231,   12,
 /*   260 */    27,   62,  179,  170,   37,   62,   28,  197,   38,   55,
 /*   270 */   237,  232,  129,   16,   25,  140,   62,  132,  206,  245,
 /*   280 */   209,  226,  140,   85,   38,   55,  237,  232,  216,   23,
 /*   290 */   132,  140,   26,  206,   43,  140,  155,   40,   36,  248,
 /*   300 */    30,  132,   12,  210,   62,   43,  140,  132,   62,  145,
 /*   310 */   230,   89,  155,  160,   36,  131,   30,  252,   12,  102,
 /*   320 */    62,  107,  250,  178,   10,   48,   13,   38,   55,  237,
 /*   330 */   232,  126,  161,  119,  140,  224,  160,   95,  140,   91,
 /*   340 */   145,  122,  249,   38,   55,  237,  232,   93,  250,   33,
 /*   350 */   140,  121,  218,   14,  145,  161,  228,   14,  250,  165,
 /*   360 */    51,  101,  160,  135,  156,  101,  160,   67,   19,   80,
 /*   370 */   166,  171,  165,   51,   86,  160,  235,  213,  145,  152,
 /*   380 */    77,  161,  236,  166,  171,  161,  169,   86,  120,  235,
 /*   390 */   165,  211,  155,  160,  161,   54,   30,  205,   12,  169,
 /*   400 */    62,  137,  134,  197,   28,  165,   51,  235,  160,  132,
 /*   410 */    28,  131,  161,   75,  145,  210,  166,  171,  165,  234,
 /*   420 */    86,  160,  235,   38,   55,  237,  232,  161,  222,  256,
 /*   430 */   140,  206,  169,  165,   51,  235,  160,  206,  123,  244,
 /*   440 */   161,   73,   62,  246,  166,  171,  165,   51,   86,  160,
 /*   450 */   235,  147,  108,  146,   74,  161,  219,  166,  171,  132,
 /*   460 */   169,   86,  145,  235,  132,   87,  165,   51,  161,  160,
 /*   470 */   186,  187,  140,  169,   71,  189,  250,  166,  171,  165,
 /*   480 */    50,   86,  160,  235,  203,  136,   81,   69,  161,   84,
 /*   490 */   166,  171,  243,  169,   86,   53,  235,  165,   51,  207,
 /*   500 */   160,  161,   15,   62,  121,   70,  169,  183,  166,  171,
 /*   510 */   165,   51,   86,  160,  235,  210,   45,   24,   72,  161,
 /*   520 */   197,  166,  171,  197,  169,   86,   21,  235,  165,   51,
 /*   530 */   116,  160,  161,  140,  219,  207,   76,  169,   98,  166,
 /*   540 */   171,  165,  104,   86,  160,  235,   52,  173,  223,  250,
 /*   550 */   161,  193,  166,  171,   57,  169,   86,  155,  235,  110,
 /*   560 */   142,   30,   19,  161,  151,   62,  210,    4,  212,  194,
 /*   570 */   177,  130,  242,  207,   35,  145,  131,  176,   59,   44,
 /*   580 */    17,   66,  165,  103,   33,  160,  105,  224,   38,   55,
 /*   590 */   237,  232,  251,  166,  171,  140,  251,   86,  251,  235,
 /*   600 */   251,  251,  157,  251,  161,  165,  103,  251,  160,  251,
 /*   610 */   251,  251,  251,  251,  251,  251,  166,  171,  165,  103,
 /*   620 */    86,  160,  235,  251,  251,  233,  251,  161,  251,  166,
 /*   630 */   171,  165,  103,   86,  160,  235,  251,  251,  148,  251,
 /*   640 */   161,  251,  166,  171,  251,  251,   86,  251,  235,  251,
 /*   650 */   251,  138,  251,  161,  251,  251,  165,  104,  251,  160,
 /*   660 */   251,  251,  251,  251,  251,  251,  251,  166,  171,  165,
 /*   670 */   118,   86,  160,  235,  251,  251,  251,  251,  161,  251,
 /*   680 */   166,  171,  165,  111,   86,  160,  235,  180,  251,  251,
 /*   690 */   251,  161,  251,  166,  171,  165,  115,   86,  160,  235,
 /*   700 */   251,  251,  251,  251,  161,  251,  166,  171,  165,  113,
 /*   710 */    86,  160,  235,  251,  251,  251,  251,  161,  251,  166,
 /*   720 */   171,  251,  251,   86,  251,  235,  251,  251,  251,  251,
 /*   730 */   161,  165,  117,  251,  160,  251,  251,  251,  251,  251,
 /*   740 */   251,  251,  166,  171,  165,   47,   86,  139,  235,  251,
 /*   750 */   251,  251,  251,  161,  251,  166,  171,  165,   92,   86,
 /*   760 */   160,  235,  251,  251,  251,  251,  161,  251,  166,  171,
 /*   770 */   165,   90,   86,  160,  235,  251,  251,  251,  251,  161,
 /*   780 */   251,  166,  171,  165,   96,   86,  160,  235,  251,  251,
 /*   790 */   251,  251,  161,  251,  166,  171,  251,  251,   86,  251,
 /*   800 */   235,  251,  251,  251,  251,  161,  165,   94,  251,  160,
 /*   810 */   251,  251,  251,  251,  251,  251,  251,  166,  171,  165,
 /*   820 */    88,   86,  160,  235,  251,  251,  251,  251,  161,  251,
 /*   830 */   166,  171,  165,  100,   86,  160,  235,  251,  251,  251,
 /*   840 */   251,  161,  251,  166,  171,  165,  114,   86,  160,  235,
 /*   850 */   251,  251,  251,  251,  161,  251,  166,  171,  165,  251,
 /*   860 */    86,  160,  235,  251,  251,  251,  251,  161,  251,  166,
 /*   870 */   171,  251,  251,   78,  165,  235,  251,  160,  251,  251,
 /*   880 */   161,  251,  251,  251,  251,  166,  171,  251,  251,   79,
 /*   890 */   251,  235,  165,  251,  251,  160,  161,  251,  251,  251,
 /*   900 */   251,  251,  251,  166,  171,  251,  251,   83,  251,  235,
 /*   910 */   251,  251,  251,  251,  161,
    );
    static public $yy_lookahead = array(
 /*     0 */    16,   40,   41,   30,    3,   68,   69,   70,   71,   30,
 /*    10 */     4,   19,   51,   52,   53,   54,   55,   56,   57,   58,
 /*    20 */    59,   60,   30,   50,   40,   41,   11,   26,   13,    4,
 /*    30 */    15,   30,   17,   27,   19,   51,   52,   53,   54,   55,
 /*    40 */    56,   57,   58,   59,   60,   30,   94,   46,   96,   25,
 /*    50 */    98,   50,   16,   73,   39,   79,   76,   42,   43,   44,
 /*    60 */    45,   25,   30,    4,   49,   40,   41,   31,   32,   33,
 /*    70 */    34,   35,   36,   37,   38,   95,   51,   52,   53,   54,
 /*    80 */    55,   56,   57,   58,   59,   60,   27,    1,   17,    3,
 /*    90 */    24,   20,    4,    1,   73,    3,   11,   76,   13,   28,
 /*   100 */    15,    9,   17,   18,   19,   84,   40,   41,   12,   13,
 /*   110 */    94,   90,   96,   25,   98,   30,   95,   51,   52,   53,
 /*   120 */    54,   55,   56,   57,   58,   59,   60,   42,   43,   44,
 /*   130 */    45,   11,   15,   13,   49,   15,   78,   17,   73,   19,
 /*   140 */    23,   76,   25,   47,   27,   28,   16,   25,   62,   84,
 /*   150 */    30,   29,    1,    2,    3,   90,    5,    6,    7,   39,
 /*   160 */    95,   10,   42,   43,   44,   45,    1,    4,    3,   49,
 /*   170 */    73,   74,    1,   76,    3,   12,   13,   73,   81,   82,
 /*   180 */    76,   84,   85,   73,   87,   88,   76,   90,   30,    1,
 /*   190 */    30,    3,   95,   15,   11,   91,   13,  100,   15,   95,
 /*   200 */    17,   23,   19,   80,    1,   95,    3,   29,   43,  105,
 /*   210 */    47,   25,   61,   30,   63,   64,   65,   31,   32,   33,
 /*   220 */    34,   35,   36,   37,   38,   42,   43,   44,   45,   12,
 /*   230 */    13,   17,   49,   72,   20,   18,    3,   11,    3,   13,
 /*   240 */    15,   15,   78,   17,   11,   19,   43,    1,   23,    3,
 /*   250 */    15,    3,   19,   11,   66,   13,   30,   15,    1,   17,
 /*   260 */     3,   19,   14,   30,   47,   19,    3,  106,   42,   43,
 /*   270 */    44,   45,   30,   21,   26,   49,   19,   25,   30,   98,
 /*   280 */     4,   16,   49,   77,   42,   43,   44,   45,   42,   26,
 /*   290 */    25,   49,    3,   30,   48,   49,   11,   28,   13,   42,
 /*   300 */    15,   25,   17,   97,   19,   48,   49,   25,   19,   27,
 /*   310 */    73,   75,   11,   76,   13,   30,   15,    4,   17,   30,
 /*   320 */    19,   21,   86,   18,   24,   80,   21,   42,   43,   44,
 /*   330 */    45,   30,   95,   73,   49,   99,   76,   75,   49,   78,
 /*   340 */    27,   79,  105,   42,   43,   44,   45,   75,   86,   22,
 /*   350 */    49,   79,   92,   15,   27,   95,   18,   15,   86,   73,
 /*   360 */    74,   23,   76,   73,   20,   23,   76,   81,   26,   72,
 /*   370 */    84,   85,   73,   74,   88,   76,   90,   48,   27,   28,
 /*   380 */    81,   95,   16,   84,   85,   95,  100,   88,    4,   90,
 /*   390 */    73,    4,   11,   76,   95,   77,   15,   48,   17,  100,
 /*   400 */    19,   84,   85,  106,    3,   73,   74,   90,   76,   25,
 /*   410 */     3,   30,   95,   81,   27,   97,   84,   85,   73,    4,
 /*   420 */    88,   76,   90,   42,   43,   44,   45,   95,   16,   84,
 /*   430 */    49,   30,  100,   73,   74,   90,   76,   30,    4,    4,
 /*   440 */    95,   81,   19,    4,   84,   85,   73,   74,   88,   76,
 /*   450 */    90,   50,   94,   30,   81,   95,   98,   84,   85,   25,
 /*   460 */   100,   88,   27,   90,   25,   75,   73,   74,   95,   76,
 /*   470 */    70,   71,   49,  100,   81,    4,   86,   84,   85,   73,
 /*   480 */    74,   88,   76,   90,    4,   83,   72,   81,   95,   72,
 /*   490 */    84,   85,   11,  100,   88,   77,   90,   73,   74,   97,
 /*   500 */    76,   95,   15,   19,   79,   81,  100,    4,   84,   85,
 /*   510 */    73,   74,   88,   76,   90,   97,   78,   26,   81,   95,
 /*   520 */   106,   84,   85,  106,  100,   88,  101,   90,   73,   74,
 /*   530 */    94,   76,   95,   49,   98,   97,   81,  100,   75,   84,
 /*   540 */    85,   73,   74,   88,   76,   90,   77,   49,   30,   86,
 /*   550 */    95,    8,   84,   85,   19,  100,   88,   11,   90,   30,
 /*   560 */    30,   15,   26,   95,   30,   19,   97,  102,   86,   96,
 /*   570 */   106,  103,  104,   97,   89,   27,   30,   87,   19,   78,
 /*   580 */    15,   92,   73,   74,   22,   76,   78,   99,   42,   43,
 /*   590 */    44,   45,  107,   84,   85,   49,  107,   88,  107,   90,
 /*   600 */   107,  107,   93,  107,   95,   73,   74,  107,   76,  107,
 /*   610 */   107,  107,  107,  107,  107,  107,   84,   85,   73,   74,
 /*   620 */    88,   76,   90,  107,  107,   93,  107,   95,  107,   84,
 /*   630 */    85,   73,   74,   88,   76,   90,  107,  107,   93,  107,
 /*   640 */    95,  107,   84,   85,  107,  107,   88,  107,   90,  107,
 /*   650 */   107,   93,  107,   95,  107,  107,   73,   74,  107,   76,
 /*   660 */   107,  107,  107,  107,  107,  107,  107,   84,   85,   73,
 /*   670 */    74,   88,   76,   90,  107,  107,  107,  107,   95,  107,
 /*   680 */    84,   85,   73,   74,   88,   76,   90,  104,  107,  107,
 /*   690 */   107,   95,  107,   84,   85,   73,   74,   88,   76,   90,
 /*   700 */   107,  107,  107,  107,   95,  107,   84,   85,   73,   74,
 /*   710 */    88,   76,   90,  107,  107,  107,  107,   95,  107,   84,
 /*   720 */    85,  107,  107,   88,  107,   90,  107,  107,  107,  107,
 /*   730 */    95,   73,   74,  107,   76,  107,  107,  107,  107,  107,
 /*   740 */   107,  107,   84,   85,   73,   74,   88,   76,   90,  107,
 /*   750 */   107,  107,  107,   95,  107,   84,   85,   73,   74,   88,
 /*   760 */    76,   90,  107,  107,  107,  107,   95,  107,   84,   85,
 /*   770 */    73,   74,   88,   76,   90,  107,  107,  107,  107,   95,
 /*   780 */   107,   84,   85,   73,   74,   88,   76,   90,  107,  107,
 /*   790 */   107,  107,   95,  107,   84,   85,  107,  107,   88,  107,
 /*   800 */    90,  107,  107,  107,  107,   95,   73,   74,  107,   76,
 /*   810 */   107,  107,  107,  107,  107,  107,  107,   84,   85,   73,
 /*   820 */    74,   88,   76,   90,  107,  107,  107,  107,   95,  107,
 /*   830 */    84,   85,   73,   74,   88,   76,   90,  107,  107,  107,
 /*   840 */   107,   95,  107,   84,   85,   73,   74,   88,   76,   90,
 /*   850 */   107,  107,  107,  107,   95,  107,   84,   85,   73,  107,
 /*   860 */    88,   76,   90,  107,  107,  107,  107,   95,  107,   84,
 /*   870 */    85,  107,  107,   88,   73,   90,  107,   76,  107,  107,
 /*   880 */    95,  107,  107,  107,  107,   84,   85,  107,  107,   88,
 /*   890 */   107,   90,   73,  107,  107,   76,   95,  107,  107,  107,
 /*   900 */   107,  107,  107,   84,   85,  107,  107,   88,  107,   90,
 /*   910 */   107,  107,  107,  107,   95,
);
    const YY_SHIFT_USE_DFLT = -40;
    const YY_SHIFT_MAX = 157;
    static public $yy_shift_ofst = array(
 /*     0 */   151,  120,   15,   15,   15,   15,   15,   15,   15,   15,
 /*    10 */    15,   15,  301,  301,  183,  183,  183,  183,  183,  183,
 /*    20 */   183,  183,  183,  183,  183,  183,  183,  183,  183,  226,
 /*    30 */   183,   85,  285,  242,  381,  546,  546,  546,  246,  257,
 /*    40 */   289,  233,  117,  423,   71,   71,  484,  282,  327,  151,
 /*    50 */    36,  186,    1,  248,  401,  203,  407,  407,  171,  407,
 /*    60 */   171,  351,  407,  171,  548,  269,  269,   66,   25,  -16,
 /*    70 */   -39,  -39,  -39,  -39,  -39,  -39,  -39,  -39,  163,  217,
 /*    80 */    92,   86,  165,   96,  188,  263,   96,    6,   88,  435,
 /*    90 */   434,  214,  265,  387,  439,  313,  276,  214,   59,  214,
 /*   100 */   384,   -8,  235,  252,  122,  214,  269,  559,  269,  562,
 /*   110 */   565,   24,  269,   24,   24,   24,  269,   24,   24,  -40,
 /*   120 */   -40,  -40,  -40,  -40,  338,  342,  178,  300,  225,  225,
 /*   130 */   305,  225,  -27,  535,  480,  349,  471,  415,  366,  491,
 /*   140 */   530,  543,  498,  536,  160,  -21,  329,   32,  130,  158,
 /*   150 */   518,  503,  534,  529,  487,  344,  481,  412,
);
    const YY_REDUCE_USE_DFLT = -64;
    const YY_REDUCE_MAX = 123;
    static public $yy_reduce_ofst = array(
 /*     0 */   -63,   97,  455,  437,  424,  299,  360,  406,  332,  373,
 /*    10 */   286,  393,  468,  583,  558,  509,  532,  545,  658,  622,
 /*    20 */   596,  609,  697,  772,  733,  635,  759,  746,  710,  671,
 /*    30 */   684,  801,  785,  819,  317,  345,   65,   21,  104,  237,
 /*    40 */   260,  110,  262,  290,  -48,   16,  -20,  272,  236,  400,
 /*    50 */   425,  425,  438,  402,  438,  161,  469,  418,  414,  206,
 /*    60 */   297,  390,  318,  417,  463,  358,  436,  465,  465,  465,
 /*    70 */   465,  465,  465,  465,  465,  465,  465,  465,  485,  485,
 /*    80 */   464,  464,  464,  485,  464,  476,  485,  482,  -24,  482,
 /*    90 */   -24,  473,  -24,  482,  -24,  482,  -24,  473,  482,  473,
 /*   100 */   -24,  489,  508,  -24,  -24,  473,  181,  490,  181,  488,
 /*   110 */   501,  -24,  181,  -24,  -24,  -24,  181,  -24,  -24,   58,
 /*   120 */   164,  123,  245,  261,
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
        /* 43 */ array(19, 30, 49, ),
        /* 44 */ array(17, 20, 28, ),
        /* 45 */ array(17, 20, 28, ),
        /* 46 */ array(19, 49, ),
        /* 47 */ array(25, 27, ),
        /* 48 */ array(22, 27, ),
        /* 49 */ array(1, 2, 3, 5, 6, 7, 10, 61, 63, 64, 65, ),
        /* 50 */ array(16, 25, 31, 32, 33, 34, 35, 36, 37, 38, ),
        /* 51 */ array(25, 31, 32, 33, 34, 35, 36, 37, 38, ),
        /* 52 */ array(3, 26, 30, 46, 50, ),
        /* 53 */ array(3, 14, 26, 30, ),
        /* 54 */ array(3, 30, 50, ),
        /* 55 */ array(1, 3, 43, ),
        /* 56 */ array(3, 30, ),
        /* 57 */ array(3, 30, ),
        /* 58 */ array(1, 3, ),
        /* 59 */ array(3, 30, ),
        /* 60 */ array(1, 3, ),
        /* 61 */ array(27, 28, ),
        /* 62 */ array(3, 30, ),
        /* 63 */ array(1, 3, ),
        /* 64 */ array(27, ),
        /* 65 */ array(28, ),
        /* 66 */ array(28, ),
        /* 67 */ array(24, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 68 */ array(4, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 69 */ array(16, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 70 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 71 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 72 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 73 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 74 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 75 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 76 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 77 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 78 */ array(4, 12, 13, 47, ),
        /* 79 */ array(12, 13, 18, 47, ),
        /* 80 */ array(1, 3, 9, ),
        /* 81 */ array(1, 3, 62, ),
        /* 82 */ array(1, 3, 43, ),
        /* 83 */ array(12, 13, 47, ),
        /* 84 */ array(1, 3, 66, ),
        /* 85 */ array(3, 26, 30, ),
        /* 86 */ array(12, 13, 47, ),
        /* 87 */ array(4, 27, ),
        /* 88 */ array(4, 25, ),
        /* 89 */ array(4, 27, ),
        /* 90 */ array(4, 25, ),
        /* 91 */ array(17, 20, ),
        /* 92 */ array(16, 25, ),
        /* 93 */ array(4, 27, ),
        /* 94 */ array(4, 25, ),
        /* 95 */ array(4, 27, ),
        /* 96 */ array(4, 25, ),
        /* 97 */ array(17, 20, ),
        /* 98 */ array(4, 27, ),
        /* 99 */ array(17, 20, ),
        /* 100 */ array(4, 25, ),
        /* 101 */ array(19, 30, ),
        /* 102 */ array(3, 15, ),
        /* 103 */ array(21, 25, ),
        /* 104 */ array(25, 29, ),
        /* 105 */ array(17, 20, ),
        /* 106 */ array(28, ),
        /* 107 */ array(19, ),
        /* 108 */ array(28, ),
        /* 109 */ array(22, ),
        /* 110 */ array(15, ),
        /* 111 */ array(25, ),
        /* 112 */ array(28, ),
        /* 113 */ array(25, ),
        /* 114 */ array(25, ),
        /* 115 */ array(25, ),
        /* 116 */ array(28, ),
        /* 117 */ array(25, ),
        /* 118 */ array(25, ),
        /* 119 */ array(),
        /* 120 */ array(),
        /* 121 */ array(),
        /* 122 */ array(),
        /* 123 */ array(),
        /* 124 */ array(15, 18, 23, ),
        /* 125 */ array(15, 23, 26, ),
        /* 126 */ array(15, 23, 29, ),
        /* 127 */ array(21, 24, ),
        /* 128 */ array(15, 23, ),
        /* 129 */ array(15, 23, ),
        /* 130 */ array(18, 21, ),
        /* 131 */ array(15, 23, ),
        /* 132 */ array(30, 50, ),
        /* 133 */ array(19, ),
        /* 134 */ array(4, ),
        /* 135 */ array(48, ),
        /* 136 */ array(4, ),
        /* 137 */ array(4, ),
        /* 138 */ array(16, ),
        /* 139 */ array(26, ),
        /* 140 */ array(30, ),
        /* 141 */ array(8, ),
        /* 142 */ array(49, ),
        /* 143 */ array(26, ),
        /* 144 */ array(30, ),
        /* 145 */ array(30, ),
        /* 146 */ array(48, ),
        /* 147 */ array(30, ),
        /* 148 */ array(16, ),
        /* 149 */ array(30, ),
        /* 150 */ array(30, ),
        /* 151 */ array(4, ),
        /* 152 */ array(30, ),
        /* 153 */ array(30, ),
        /* 154 */ array(15, ),
        /* 155 */ array(20, ),
        /* 156 */ array(11, ),
        /* 157 */ array(16, ),
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
        /* 253 */ array(),
        /* 254 */ array(),
        /* 255 */ array(),
        /* 256 */ array(),
        /* 257 */ array(),
);
    static public $yy_default = array(
 /*     0 */   400,  400,  400,  400,  400,  400,  400,  400,  400,  400,
 /*    10 */   400,  400,  385,  400,  349,  349,  349,  349,  400,  400,
 /*    20 */   400,  400,  400,  400,  400,  400,  400,  400,  400,  400,
 /*    30 */   400,  400,  400,  400,  400,  400,  400,  400,  400,  400,
 /*    40 */   400,  400,  287,  400,  318,  273,  400,  287,  287,  258,
 /*    50 */   359,  359,  325,  400,  325,  400,  400,  400,  400,  400,
 /*    60 */   400,  287,  400,  400,  287,  314,  313,  400,  400,  400,
 /*    70 */   361,  371,  370,  363,  362,  357,  367,  366,  400,  400,
 /*    80 */   400,  400,  400,  354,  400,  400,  293,  400,  400,  400,
 /*    90 */   400,  343,  400,  400,  400,  400,  400,  342,  400,  341,
 /*   100 */   400,  400,  325,  348,  386,  340,  319,  400,  316,  294,
 /*   110 */   325,  360,  337,  281,  291,  288,  315,  387,  388,  325,
 /*   120 */   325,  353,  353,  325,  400,  292,  292,  400,  292,  355,
 /*   130 */   400,  400,  400,  400,  400,  400,  400,  400,  400,  320,
 /*   140 */   400,  400,  400,  400,  400,  400,  400,  400,  400,  400,
 /*   150 */   400,  400,  400,  400,  317,  303,  400,  400,  365,  368,
 /*   160 */   320,  322,  378,  364,  377,  302,  296,  327,  328,  356,
 /*   170 */   326,  295,  297,  323,  379,  265,  290,  396,  382,  282,
 /*   180 */   384,  262,  289,  278,  259,  277,  260,  261,  398,  280,
 /*   190 */   380,  381,  267,  268,  324,  266,  358,  397,  399,  263,
 /*   200 */   264,  376,  369,  284,  299,  393,  335,  334,  321,  336,
 /*   210 */   333,  271,  285,  392,  270,  269,  312,  329,  344,  338,
 /*   220 */   300,  301,  346,  350,  352,  390,  308,  331,  330,  332,
 /*   230 */   391,  395,  306,  347,  283,  307,  345,  305,  279,  373,
 /*   240 */   374,  372,  383,  304,  276,  339,  272,  310,  311,  389,
 /*   250 */   286,  309,  274,  351,  275,  394,  298,  375,
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
    const YYNSTATE = 258;
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
#line 1799 "internal.templateparser.php"
#line 79 "internal.templateparser.y"
    function yy_r1(){$this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1802 "internal.templateparser.php"
#line 81 "internal.templateparser.y"
    function yy_r2(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1805 "internal.templateparser.php"
#line 87 "internal.templateparser.y"
    function yy_r3(){if ($this->compiler->has_code) {
                                            $tmp =''; foreach ($this->prefix_code as $code) {$tmp.=$code;} $this->prefix_code=array();
                                            $this->_retvalue = $this->cacher->processNocacheCode($tmp.$this->yystack[$this->yyidx + 0]->minor, $this->compiler,$this->nocache,true);
                                         } $this->nocache=false;    }
#line 1811 "internal.templateparser.php"
#line 100 "internal.templateparser.y"
    function yy_r4(){ $this->_retvalue = '';    }
#line 1814 "internal.templateparser.php"
#line 103 "internal.templateparser.y"
    function yy_r5(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + -1]->minor, $this->compiler,false,false);    }
#line 1817 "internal.templateparser.php"
#line 105 "internal.templateparser.y"
    function yy_r6(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->left_delimiter, $this->compiler,false,false);    }
#line 1820 "internal.templateparser.php"
#line 107 "internal.templateparser.y"
    function yy_r7(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->right_delimiter, $this->compiler,false,false);    }
#line 1823 "internal.templateparser.php"
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
#line 1834 "internal.templateparser.php"
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
#line 1845 "internal.templateparser.php"
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
#line 1856 "internal.templateparser.php"
#line 139 "internal.templateparser.y"
    function yy_r11(){$this->_retvalue = $this->cacher->processNocacheCode("<?php echo '".$this->yystack[$this->yyidx + 0]->minor."';?>\n", $this->compiler, true, true);    }
#line 1859 "internal.templateparser.php"
#line 141 "internal.templateparser.y"
    function yy_r12(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + 0]->minor, $this->compiler,false,false);    }
#line 1862 "internal.templateparser.php"
#line 149 "internal.templateparser.y"
    function yy_r13(){ $this->_retvalue = $this->compiler->compileTag('print_expression',array_merge(array('value'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1865 "internal.templateparser.php"
#line 152 "internal.templateparser.y"
    function yy_r14(){ $this->_retvalue = $this->compiler->compileTag('assign',array_merge(array('value'=>$this->yystack[$this->yyidx + -1]->minor),$this->yystack[$this->yyidx + -3]->minor));    }
#line 1868 "internal.templateparser.php"
#line 154 "internal.templateparser.y"
    function yy_r15(){$this->_retvalue = array('var'=>$this->yystack[$this->yyidx + -1]->minor, 'index'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1871 "internal.templateparser.php"
#line 158 "internal.templateparser.y"
    function yy_r16(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor,$this->yystack[$this->yyidx + -1]->minor);    }
#line 1874 "internal.templateparser.php"
#line 160 "internal.templateparser.y"
    function yy_r17(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -4]->minor,array_merge(array('object_methode'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1877 "internal.templateparser.php"
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
#line 1892 "internal.templateparser.php"
#line 176 "internal.templateparser.y"
    function yy_r19(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor.'close',$this->yystack[$this->yyidx + -1]->minor);    }
#line 1895 "internal.templateparser.php"
#line 178 "internal.templateparser.y"
    function yy_r20(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor.'close',array('object_methode'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1898 "internal.templateparser.php"
#line 180 "internal.templateparser.y"
    function yy_r21(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor,array('if condition'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1901 "internal.templateparser.php"
#line 182 "internal.templateparser.y"
    function yy_r22(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -9]->minor,array('start'=>$this->yystack[$this->yyidx + -7]->minor,'ifexp'=>$this->yystack[$this->yyidx + -5]->minor,'varloop'=>$this->yystack[$this->yyidx + -2]->minor,'loop'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1904 "internal.templateparser.php"
#line 183 "internal.templateparser.y"
    function yy_r23(){ $this->_retvalue = '='.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1907 "internal.templateparser.php"
#line 184 "internal.templateparser.y"
    function yy_r24(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1910 "internal.templateparser.php"
#line 187 "internal.templateparser.y"
    function yy_r25(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -6]->minor,array('from'=>$this->yystack[$this->yyidx + -1]->minor,'item'=>$this->yystack[$this->yyidx + -3]->minor));    }
#line 1913 "internal.templateparser.php"
#line 194 "internal.templateparser.y"
    function yy_r27(){ $this->_retvalue = array_merge($this->yystack[$this->yyidx + -1]->minor,$this->yystack[$this->yyidx + 0]->minor);    }
#line 1916 "internal.templateparser.php"
#line 198 "internal.templateparser.y"
    function yy_r29(){ $this->_retvalue = array();    }
#line 1919 "internal.templateparser.php"
#line 202 "internal.templateparser.y"
    function yy_r30(){ $this->_retvalue = array($this->yystack[$this->yyidx + -2]->minor=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1922 "internal.templateparser.php"
#line 207 "internal.templateparser.y"
    function yy_r31(){ $this->_retvalue = array($this->yystack[$this->yyidx + 0]->minor);    }
#line 1925 "internal.templateparser.php"
#line 208 "internal.templateparser.y"
    function yy_r32(){ $this->yystack[$this->yyidx + -2]->minor[]=$this->yystack[$this->yyidx + 0]->minor; $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor;    }
#line 1928 "internal.templateparser.php"
#line 210 "internal.templateparser.y"
    function yy_r33(){ $this->_retvalue = array('var' => $this->yystack[$this->yyidx + -2]->minor, 'value'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1931 "internal.templateparser.php"
#line 217 "internal.templateparser.y"
    function yy_r34(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';     }
#line 1934 "internal.templateparser.php"
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
#line 1949 "internal.templateparser.php"
#line 239 "internal.templateparser.y"
    function yy_r39(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1952 "internal.templateparser.php"
#line 241 "internal.templateparser.y"
    function yy_r40(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor . $this->yystack[$this->yyidx + -1]->minor . $this->yystack[$this->yyidx + 0]->minor;     }
#line 1955 "internal.templateparser.php"
#line 243 "internal.templateparser.y"
    function yy_r41(){ $this->_retvalue = '('. $this->yystack[$this->yyidx + -2]->minor . ').(' . $this->yystack[$this->yyidx + 0]->minor. ')';     }
#line 1958 "internal.templateparser.php"
#line 260 "internal.templateparser.y"
    function yy_r46(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1961 "internal.templateparser.php"
#line 269 "internal.templateparser.y"
    function yy_r50(){ $this->_retvalue = "(". $this->yystack[$this->yyidx + -1]->minor .")";     }
#line 1964 "internal.templateparser.php"
#line 272 "internal.templateparser.y"
    function yy_r51(){ $this->_retvalue = "'".$this->yystack[$this->yyidx + -1]->minor."'";     }
#line 1967 "internal.templateparser.php"
#line 273 "internal.templateparser.y"
    function yy_r52(){ $this->_retvalue = "''";     }
#line 1970 "internal.templateparser.php"
#line 275 "internal.templateparser.y"
    function yy_r53(){ $this->_retvalue = "'".str_replace('\"','"',$this->yystack[$this->yyidx + -1]->minor)."'";     }
#line 1973 "internal.templateparser.php"
#line 281 "internal.templateparser.y"
    function yy_r55(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1976 "internal.templateparser.php"
#line 282 "internal.templateparser.y"
    function yy_r56(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -3]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -6]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -1]->minor .')';     }
#line 1979 "internal.templateparser.php"
#line 284 "internal.templateparser.y"
    function yy_r57(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor.'::'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1982 "internal.templateparser.php"
#line 285 "internal.templateparser.y"
    function yy_r58(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -4]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -7]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -2]->minor .')'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1985 "internal.templateparser.php"
#line 287 "internal.templateparser.y"
    function yy_r59(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1988 "internal.templateparser.php"
#line 289 "internal.templateparser.y"
    function yy_r60(){ $this->_retvalue = $this->yystack[$this->yyidx + -4]->minor.'::$'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1991 "internal.templateparser.php"
#line 291 "internal.templateparser.y"
    function yy_r61(){ $this->_retvalue = $this->yystack[$this->yyidx + -5]->minor.'::$'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1994 "internal.templateparser.php"
#line 300 "internal.templateparser.y"
    function yy_r62(){ if ($this->yystack[$this->yyidx + 0]->minor['var'] == '\'smarty\'') { $this->_retvalue =  $this->compiler->compileTag(trim($this->yystack[$this->yyidx + 0]->minor['var'],"'"),$this->yystack[$this->yyidx + 0]->minor['index']);} else {
                                                         $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + 0]->minor['var'] .')->value'.$this->yystack[$this->yyidx + 0]->minor['index']; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + 0]->minor['var'],"'"))->nocache;}    }
#line 1998 "internal.templateparser.php"
#line 303 "internal.templateparser.y"
    function yy_r63(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->'.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 2001 "internal.templateparser.php"
#line 307 "internal.templateparser.y"
    function yy_r65(){$this->_retvalue = '$_smarty_tpl->getConfigVariable(\''. $this->yystack[$this->yyidx + -1]->minor .'\')';    }
#line 2004 "internal.templateparser.php"
#line 315 "internal.templateparser.y"
    function yy_r67(){return;    }
#line 2007 "internal.templateparser.php"
#line 319 "internal.templateparser.y"
    function yy_r68(){ $this->_retvalue = "['". $this->yystack[$this->yyidx + 0]->minor ."']";    }
#line 2010 "internal.templateparser.php"
#line 320 "internal.templateparser.y"
    function yy_r69(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + 0]->minor ."]";    }
#line 2013 "internal.templateparser.php"
#line 322 "internal.templateparser.y"
    function yy_r70(){ $this->_retvalue = "[".$this->yystack[$this->yyidx + 0]->minor."]";    }
#line 2016 "internal.templateparser.php"
#line 323 "internal.templateparser.y"
    function yy_r71(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + -1]->minor ."]";    }
#line 2019 "internal.templateparser.php"
#line 325 "internal.templateparser.y"
    function yy_r72(){ $this->_retvalue = '['.$this->compiler->compileTag('smarty','[\'section\'][\''.$this->yystack[$this->yyidx + -1]->minor.'\'][\'index\']').']';    }
#line 2022 "internal.templateparser.php"
#line 329 "internal.templateparser.y"
    function yy_r74(){$this->_retvalue = '';    }
#line 2025 "internal.templateparser.php"
#line 337 "internal.templateparser.y"
    function yy_r76(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2028 "internal.templateparser.php"
#line 339 "internal.templateparser.y"
    function yy_r77(){$this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2031 "internal.templateparser.php"
#line 341 "internal.templateparser.y"
    function yy_r78(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2034 "internal.templateparser.php"
#line 346 "internal.templateparser.y"
    function yy_r79(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->value'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 2037 "internal.templateparser.php"
#line 348 "internal.templateparser.y"
    function yy_r80(){$this->_retvalue  = $this->yystack[$this->yyidx + 0]->minor;     }
#line 2040 "internal.templateparser.php"
#line 350 "internal.templateparser.y"
    function yy_r81(){$this->_retvalue  = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2043 "internal.templateparser.php"
#line 352 "internal.templateparser.y"
    function yy_r82(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2046 "internal.templateparser.php"
#line 353 "internal.templateparser.y"
    function yy_r83(){ $this->_retvalue = '->{'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2049 "internal.templateparser.php"
#line 354 "internal.templateparser.y"
    function yy_r84(){ $this->_retvalue = '->{'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2052 "internal.templateparser.php"
#line 355 "internal.templateparser.y"
    function yy_r85(){ $this->_retvalue = '->{\''.$this->yystack[$this->yyidx + -4]->minor.'\'.'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2055 "internal.templateparser.php"
#line 361 "internal.templateparser.y"
    function yy_r86(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2058 "internal.templateparser.php"
#line 367 "internal.templateparser.y"
    function yy_r87(){if (!$this->template->security || $this->smarty->security_handler->isTrustedPhpFunction($this->yystack[$this->yyidx + -3]->minor, $this->compiler)) {
																					            if ($this->yystack[$this->yyidx + -3]->minor == 'isset' || $this->yystack[$this->yyidx + -3]->minor == 'empty' || $this->yystack[$this->yyidx + -3]->minor == 'array' || is_callable($this->yystack[$this->yyidx + -3]->minor)) {
																					                $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";
																					            } else {
                                                       $this->compiler->trigger_template_error ("unknown function \"" . $this->yystack[$this->yyidx + -3]->minor . "\"");
                                                      }
                                                    }    }
#line 2067 "internal.templateparser.php"
#line 378 "internal.templateparser.y"
    function yy_r88(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";    }
#line 2070 "internal.templateparser.php"
#line 382 "internal.templateparser.y"
    function yy_r89(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.",".$this->yystack[$this->yyidx + 0]->minor;    }
#line 2073 "internal.templateparser.php"
#line 386 "internal.templateparser.y"
    function yy_r91(){ return;    }
#line 2076 "internal.templateparser.php"
#line 391 "internal.templateparser.y"
    function yy_r92(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,true);    }
#line 2079 "internal.templateparser.php"
#line 392 "internal.templateparser.y"
    function yy_r93(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,false);    }
#line 2082 "internal.templateparser.php"
#line 399 "internal.templateparser.y"
    function yy_r94(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2085 "internal.templateparser.php"
#line 403 "internal.templateparser.y"
    function yy_r96(){$this->_retvalue = ','.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2088 "internal.templateparser.php"
#line 404 "internal.templateparser.y"
    function yy_r97(){$this->_retvalue = ',\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2091 "internal.templateparser.php"
#line 411 "internal.templateparser.y"
    function yy_r99(){$this->_retvalue = '!'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2094 "internal.templateparser.php"
#line 416 "internal.templateparser.y"
    function yy_r101(){$this->_retvalue =$this->yystack[$this->yyidx + 0]->minor;    }
#line 2097 "internal.templateparser.php"
#line 417 "internal.templateparser.y"
    function yy_r102(){$this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2100 "internal.templateparser.php"
#line 419 "internal.templateparser.y"
    function yy_r104(){$this->_retvalue = '!('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2103 "internal.templateparser.php"
#line 420 "internal.templateparser.y"
    function yy_r105(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2106 "internal.templateparser.php"
#line 421 "internal.templateparser.y"
    function yy_r106(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2109 "internal.templateparser.php"
#line 422 "internal.templateparser.y"
    function yy_r107(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2112 "internal.templateparser.php"
#line 423 "internal.templateparser.y"
    function yy_r108(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2115 "internal.templateparser.php"
#line 424 "internal.templateparser.y"
    function yy_r109(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2118 "internal.templateparser.php"
#line 430 "internal.templateparser.y"
    function yy_r114(){$this->_retvalue = '==';    }
#line 2121 "internal.templateparser.php"
#line 431 "internal.templateparser.y"
    function yy_r115(){$this->_retvalue = '!=';    }
#line 2124 "internal.templateparser.php"
#line 432 "internal.templateparser.y"
    function yy_r116(){$this->_retvalue = '>';    }
#line 2127 "internal.templateparser.php"
#line 433 "internal.templateparser.y"
    function yy_r117(){$this->_retvalue = '<';    }
#line 2130 "internal.templateparser.php"
#line 434 "internal.templateparser.y"
    function yy_r118(){$this->_retvalue = '>=';    }
#line 2133 "internal.templateparser.php"
#line 435 "internal.templateparser.y"
    function yy_r119(){$this->_retvalue = '<=';    }
#line 2136 "internal.templateparser.php"
#line 436 "internal.templateparser.y"
    function yy_r120(){$this->_retvalue = '===';    }
#line 2139 "internal.templateparser.php"
#line 437 "internal.templateparser.y"
    function yy_r121(){$this->_retvalue = '!==';    }
#line 2142 "internal.templateparser.php"
#line 439 "internal.templateparser.y"
    function yy_r122(){$this->_retvalue = '&&';    }
#line 2145 "internal.templateparser.php"
#line 440 "internal.templateparser.y"
    function yy_r123(){$this->_retvalue = '||';    }
#line 2148 "internal.templateparser.php"
#line 445 "internal.templateparser.y"
    function yy_r124(){ $this->_retvalue = 'array('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2151 "internal.templateparser.php"
#line 447 "internal.templateparser.y"
    function yy_r126(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.','.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2154 "internal.templateparser.php"
#line 448 "internal.templateparser.y"
    function yy_r127(){ return;     }
#line 2157 "internal.templateparser.php"
#line 450 "internal.templateparser.y"
    function yy_r129(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2160 "internal.templateparser.php"
#line 451 "internal.templateparser.y"
    function yy_r130(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + -2]->minor.'\'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2163 "internal.templateparser.php"
#line 458 "internal.templateparser.y"
    function yy_r133(){$this->_retvalue = "'.".$this->yystack[$this->yyidx + 0]->minor.".'";    }
#line 2166 "internal.templateparser.php"
#line 459 "internal.templateparser.y"
    function yy_r134(){$this->_retvalue = "`".$this->yystack[$this->yyidx + -1]->minor."`";    }
#line 2169 "internal.templateparser.php"
#line 460 "internal.templateparser.y"
    function yy_r135(){$this->_retvalue = "'.".$this->yystack[$this->yyidx + -1]->minor.".'";    }
#line 2172 "internal.templateparser.php"
#line 461 "internal.templateparser.y"
    function yy_r136(){$this->_retvalue = "'.(".$this->yystack[$this->yyidx + -1]->minor.").'";    }
#line 2175 "internal.templateparser.php"
#line 462 "internal.templateparser.y"
    function yy_r137(){$this->_retvalue = addcslashes($this->yystack[$this->yyidx + 0]->minor,"'");    }
#line 2178 "internal.templateparser.php"

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
#line 2296 "internal.templateparser.php"
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
#line 2321 "internal.templateparser.php"
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
