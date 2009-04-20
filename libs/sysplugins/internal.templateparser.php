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
    const TP_ISIN                           = 61;
    const TP_LITERALSTART                   = 62;
    const TP_LITERALEND                     = 63;
    const TP_LDELIMTAG                      = 64;
    const TP_RDELIMTAG                      = 65;
    const TP_PHPSTART                       = 66;
    const TP_PHPEND                         = 67;
    const YY_NO_ACTION = 413;
    const YY_ACCEPT_ACTION = 412;
    const YY_ERROR_ACTION = 411;

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
 /*     0 */   184,  142,  246,   36,  129,    7,  214,   12,  211,   60,
 /*    10 */   181,  175,  180,  183,  178,  179,  177,  176,   18,   18,
 /*    20 */   133,  240,  245,  158,   19,  146,  239,  226,   48,    8,
 /*    30 */   165,   58,   53,   55,  193,  194,  172,  185,   47,  149,
 /*    40 */    34,   21,   20,  157,  146,  230,  230,  187,  186,  174,
 /*    50 */   182,    4,   11,    2,    3,    9,    6,  195,   51,   31,
 /*    60 */   221,  143,   41,  253,  255,   69,  131,   13,  199,  198,
 /*    70 */    39,  166,   85,  236,  200,   90,  234,  172,  185,  222,
 /*    80 */   121,   18,  129,   61,  173,  219,  220,   64,  187,  186,
 /*    90 */   174,  182,    4,   11,    2,    3,    9,    6,   37,   13,
 /*   100 */   241,  129,  172,  185,   21,  190,  129,   90,  230,  129,
 /*   110 */    30,    1,  150,  187,  186,  174,  182,    4,   11,    2,
 /*   120 */     3,    9,    6,  146,   35,   65,  253,  255,  153,  172,
 /*   130 */   185,  142,  259,   36,  214,   25,  211,   12,  248,   60,
 /*   140 */   187,  186,  174,  182,    4,   11,    2,    3,    9,    6,
 /*   150 */   127,  142,  214,   36,  211,    7,   60,   12,  214,   63,
 /*   160 */   211,   37,   53,   55,  193,  194,  218,  136,  124,  149,
 /*   170 */   128,  221,  412,   49,  164,  217,   27,   31,  249,    8,
 /*   180 */    41,  264,   53,   55,  193,  194,  149,  191,   17,  149,
 /*   190 */   222,  142,  113,   36,  224,   25,  235,   12,  162,   60,
 /*   200 */   227,  142,  129,   36,  110,   25,  224,   12,  235,   60,
 /*   210 */   133,  142,   16,   36,  213,   25,  129,   12,  137,   60,
 /*   220 */   132,  221,   53,   55,  193,  194,   18,  111,  237,  149,
 /*   230 */   130,  235,   53,   55,  193,  194,  253,  255,  197,  149,
 /*   240 */   222,  221,   53,   55,  193,  194,  142,  104,   36,  149,
 /*   250 */    25,  122,   12,  140,   60,  142,   18,   36,  243,   25,
 /*   260 */   222,   12,   13,   60,   13,  126,  112,  260,   96,   10,
 /*   270 */    90,   37,   90,   24,   40,  117,   44,   53,   55,  193,
 /*   280 */   194,  102,  236,  230,  149,  125,   53,   55,  193,  194,
 /*   290 */   242,  129,  243,  149,  214,  233,  211,  181,  175,  180,
 /*   300 */   183,  178,  179,  177,  176,  231,   94,  195,   51,  214,
 /*   310 */   221,  211,  118,  146,  155,   73,  235,  243,  199,  198,
 /*   320 */   195,   50,   85,  221,  200,  161,  129,   34,   70,  222,
 /*   330 */   254,  199,  198,  265,  173,   85,  206,  200,  195,   51,
 /*   340 */   250,  221,  222,   15,  141,  160,   78,  173,  221,  199,
 /*   350 */   198,  208,  262,   85,   23,  200,  146,   18,   82,  123,
 /*   360 */   222,  195,   51,   13,  221,  173,   86,  222,  170,   77,
 /*   370 */   139,   90,  199,  198,  125,  171,   85,   26,  200,  195,
 /*   380 */   129,  142,  221,  222,  230,   25,  238,   12,  173,   60,
 /*   390 */   188,  189,  212,  205,  195,   51,  200,  221,  146,   42,
 /*   400 */   132,  222,   72,   56,  153,  199,  198,  146,  151,   85,
 /*   410 */    60,  200,   53,   55,  193,  194,  222,  195,   51,  149,
 /*   420 */   221,  173,   33,  238,  129,   71,  146,  146,  199,  198,
 /*   430 */   195,   51,   85,  221,  200,  262,  144,   23,   74,  222,
 /*   440 */   149,  199,  198,  129,  173,   85,   52,  200,  195,   51,
 /*   450 */   233,  221,  222,  139,   54,   33,   75,  173,  103,  199,
 /*   460 */   198,  254,  125,   85,  142,  200,  238,   83,   25,  243,
 /*   470 */   222,   84,   60,   93,  238,  173,  261,  195,  106,   79,
 /*   480 */   221,   98,   42,  132,  243,  216,  217,  125,  199,  198,
 /*   490 */    99,  114,   85,  258,  200,   53,   55,  193,  194,  222,
 /*   500 */   201,  212,  149,  195,   51,  212,  221,  134,  256,   28,
 /*   510 */    87,   76,  101,   39,  199,  198,  195,   51,   85,  221,
 /*   520 */   200,  247,   17,  243,   68,  222,  223,  199,  198,   67,
 /*   530 */   173,   85,  148,  200,  195,   95,  135,  221,  222,  204,
 /*   540 */   232,  252,  203,  173,  212,  199,  198,  195,   95,   85,
 /*   550 */   221,  200,  228,  225,  257,   62,  222,  195,  199,  198,
 /*   560 */   221,  145,   85,  163,  200,   22,  169,  156,  229,  222,
 /*   570 */   195,   95,   43,  221,  200,  168,   57,  263,   24,  222,
 /*   580 */   192,  199,  198,  195,   95,   85,  221,  200,   32,  244,
 /*   590 */   152,  215,  222,  233,  199,  198,  209,   66,   85,   91,
 /*   600 */   200,    5,   14,  159,   60,  222,  195,  106,   38,  221,
 /*   610 */   146,  167,  224,   59,  202,  210,  255,  199,  198,  255,
 /*   620 */   255,   85,  255,  200,  195,   46,  255,  221,  222,  255,
 /*   630 */   255,  255,  255,  255,  149,  199,  198,  251,  255,   85,
 /*   640 */   255,  200,  195,  115,  255,  221,  222,  255,  255,  255,
 /*   650 */   255,  255,  255,  199,  198,  255,  255,   85,  255,  200,
 /*   660 */   195,  107,  255,  221,  222,  255,  255,  255,  255,  255,
 /*   670 */   255,  199,  198,  195,  109,   85,  221,  200,  255,  255,
 /*   680 */   195,  255,  222,  221,  199,  198,  255,  255,   85,  255,
 /*   690 */   200,  154,  138,  195,   92,  222,  221,  200,  255,  255,
 /*   700 */   255,  255,  222,  255,  199,  198,  255,  255,   85,  255,
 /*   710 */   200,  195,  120,  255,  221,  222,  255,  255,  255,  255,
 /*   720 */   255,  255,  199,  198,  255,  255,   85,  255,  200,  195,
 /*   730 */   105,  255,  221,  222,  255,  255,  255,  255,  255,  255,
 /*   740 */   199,  198,  255,  255,   85,  255,  200,  195,  108,  255,
 /*   750 */   221,  222,  255,  255,  255,  255,  255,  255,  199,  198,
 /*   760 */   195,   45,   85,  147,  200,  255,  255,  195,  255,  222,
 /*   770 */   221,  199,  198,  255,  255,   85,  255,  200,  207,  255,
 /*   780 */   195,  100,  222,  221,  200,  255,  255,  255,  255,  222,
 /*   790 */   255,  199,  198,  255,  255,   85,  255,  200,  195,   97,
 /*   800 */   255,  221,  222,  255,  255,  255,  255,  255,  255,  199,
 /*   810 */   198,  255,  255,   85,  255,  200,  195,  119,  255,  221,
 /*   820 */   222,  255,  255,  255,  255,  255,  255,  199,  198,  255,
 /*   830 */   255,   85,  255,  200,  195,  116,  255,  221,  222,  255,
 /*   840 */   255,   29,  255,  255,  255,  199,  198,  195,  255,   85,
 /*   850 */   221,  200,  255,  255,  255,  255,  222,   60,  199,  198,
 /*   860 */   255,  255,   88,  255,  200,  255,  255,  195,   89,  222,
 /*   870 */   221,  255,  255,  255,  255,  255,  255,  255,  199,  198,
 /*   880 */   255,  255,   80,  255,  200,  195,  255,  149,  221,  222,
 /*   890 */   255,  255,  195,  255,  255,  221,  199,  198,  255,  255,
 /*   900 */    81,  255,  200,  196,  255,  255,  255,  222,  255,  200,
 /*   910 */   255,  255,  255,  255,  222,
    );
    static public $yy_lookahead = array(
 /*     0 */     4,   11,    4,   13,   25,   15,    1,   17,    3,   19,
 /*    10 */    31,   32,   33,   34,   35,   36,   37,   38,    3,    3,
 /*    20 */    30,    4,    1,    2,    3,   27,    5,    6,    7,   39,
 /*    30 */    14,   10,   42,   43,   44,   45,   40,   41,   81,   49,
 /*    40 */    61,   26,   26,   30,   27,   30,   30,   51,   52,   53,
 /*    50 */    54,   55,   56,   57,   58,   59,   60,   74,   75,   17,
 /*    60 */    77,   24,   20,   12,   13,   82,   83,   15,   85,   86,
 /*    70 */    28,   88,   89,   16,   91,   23,   30,   40,   41,   96,
 /*    80 */     4,    3,   25,   62,  101,   64,   65,   66,   51,   52,
 /*    90 */    53,   54,   55,   56,   57,   58,   59,   60,   47,   15,
 /*   100 */     4,   25,   40,   41,   26,   16,   25,   23,   30,   25,
 /*   110 */    29,   27,   28,   51,   52,   53,   54,   55,   56,   57,
 /*   120 */    58,   59,   60,   27,   46,   30,   12,   13,   50,   40,
 /*   130 */    41,   11,   18,   13,    1,   15,    3,   17,   18,   19,
 /*   140 */    51,   52,   53,   54,   55,   56,   57,   58,   59,   60,
 /*   150 */    30,   11,    1,   13,    3,   15,   19,   17,    1,   19,
 /*   160 */     3,   47,   42,   43,   44,   45,    9,   30,   74,   49,
 /*   170 */    30,   77,   69,   70,   71,   72,    3,   17,   30,   39,
 /*   180 */    20,    4,   42,   43,   44,   45,   49,   93,   15,   49,
 /*   190 */    96,   11,   95,   13,   97,   15,   99,   17,   50,   19,
 /*   200 */    67,   11,   25,   13,   95,   15,   97,   17,   99,   19,
 /*   210 */    30,   11,   21,   13,   63,   15,   25,   17,   74,   19,
 /*   220 */    30,   77,   42,   43,   44,   45,    3,   95,    4,   49,
 /*   230 */    30,   99,   42,   43,   44,   45,   12,   13,   74,   49,
 /*   240 */    96,   77,   42,   43,   44,   45,   11,   76,   13,   49,
 /*   250 */    15,   80,   17,   30,   19,   11,    3,   13,   87,   15,
 /*   260 */    96,   17,   15,   19,   15,   30,   21,   18,   79,   24,
 /*   270 */    23,   47,   23,   26,   30,   81,   79,   42,   43,   44,
 /*   280 */    45,   76,   16,   30,   49,   80,   42,   43,   44,   45,
 /*   290 */     4,   25,   87,   49,    1,   98,    3,   31,   32,   33,
 /*   300 */    34,   35,   36,   37,   38,    4,   76,   74,   75,    1,
 /*   310 */    77,    3,   95,   27,   19,   82,   99,   87,   85,   86,
 /*   320 */    74,   75,   89,   77,   91,   30,   25,   61,   82,   96,
 /*   330 */   100,   85,   86,    4,  101,   89,   43,   91,   74,   75,
 /*   340 */    18,   77,   96,   21,   30,   74,   82,  101,   77,   85,
 /*   350 */    86,   43,    1,   89,    3,   91,   27,    3,   73,    4,
 /*   360 */    96,   74,   75,   15,   77,  101,   78,   96,    4,   82,
 /*   370 */    19,   23,   85,   86,   80,    4,   89,   29,   91,   74,
 /*   380 */    25,   11,   77,   96,   30,   15,   98,   17,  101,   19,
 /*   390 */    85,   86,  107,   42,   74,   75,   91,   77,   27,   48,
 /*   400 */    30,   96,   82,   78,   50,   85,   86,   27,   28,   89,
 /*   410 */    19,   91,   42,   43,   44,   45,   96,   74,   75,   49,
 /*   420 */    77,  101,   22,   98,   25,   82,   27,   27,   85,   86,
 /*   430 */    74,   75,   89,   77,   91,    1,   84,    3,   82,   96,
 /*   440 */    49,   85,   86,   25,  101,   89,   78,   91,   74,   75,
 /*   450 */    98,   77,   96,   19,   78,   22,   82,  101,   76,   85,
 /*   460 */    86,  100,   80,   89,   11,   91,   98,   73,   15,   87,
 /*   470 */    96,   73,   19,   76,   98,  101,   42,   74,   75,   92,
 /*   480 */    77,   79,   48,   30,   87,   71,   72,   80,   85,   86,
 /*   490 */    79,   30,   89,  106,   91,   42,   43,   44,   45,   96,
 /*   500 */    99,  107,   49,   74,   75,  107,   77,  104,  105,  102,
 /*   510 */    73,   82,   76,   28,   85,   86,   74,   75,   89,   77,
 /*   520 */    91,   16,   15,   87,   82,   96,   49,   85,   86,   16,
 /*   530 */   101,   89,   20,   91,   74,   75,   22,   77,   96,   48,
 /*   540 */    30,   30,   48,  101,  107,   85,   86,   74,   75,   89,
 /*   550 */    77,   91,   30,    8,   94,   19,   96,   74,   85,   86,
 /*   560 */    77,   30,   89,    4,   91,   26,    4,   94,   85,   96,
 /*   570 */    74,   75,   79,   77,   91,    4,   30,   16,   26,   96,
 /*   580 */    11,   85,   86,   74,   75,   89,   77,   91,    3,   87,
 /*   590 */    94,  107,   96,   98,   85,   86,   11,   93,   89,   79,
 /*   600 */    91,  103,   15,   94,   19,   96,   74,   75,   90,   77,
 /*   610 */    27,   88,   97,   19,  106,   30,  108,   85,   86,  108,
 /*   620 */   108,   89,  108,   91,   74,   75,  108,   77,   96,  108,
 /*   630 */   108,  108,  108,  108,   49,   85,   86,  105,  108,   89,
 /*   640 */   108,   91,   74,   75,  108,   77,   96,  108,  108,  108,
 /*   650 */   108,  108,  108,   85,   86,  108,  108,   89,  108,   91,
 /*   660 */    74,   75,  108,   77,   96,  108,  108,  108,  108,  108,
 /*   670 */   108,   85,   86,   74,   75,   89,   77,   91,  108,  108,
 /*   680 */    74,  108,   96,   77,   85,   86,  108,  108,   89,  108,
 /*   690 */    91,   85,   86,   74,   75,   96,   77,   91,  108,  108,
 /*   700 */   108,  108,   96,  108,   85,   86,  108,  108,   89,  108,
 /*   710 */    91,   74,   75,  108,   77,   96,  108,  108,  108,  108,
 /*   720 */   108,  108,   85,   86,  108,  108,   89,  108,   91,   74,
 /*   730 */    75,  108,   77,   96,  108,  108,  108,  108,  108,  108,
 /*   740 */    85,   86,  108,  108,   89,  108,   91,   74,   75,  108,
 /*   750 */    77,   96,  108,  108,  108,  108,  108,  108,   85,   86,
 /*   760 */    74,   75,   89,   77,   91,  108,  108,   74,  108,   96,
 /*   770 */    77,   85,   86,  108,  108,   89,  108,   91,   85,  108,
 /*   780 */    74,   75,   96,   77,   91,  108,  108,  108,  108,   96,
 /*   790 */   108,   85,   86,  108,  108,   89,  108,   91,   74,   75,
 /*   800 */   108,   77,   96,  108,  108,  108,  108,  108,  108,   85,
 /*   810 */    86,  108,  108,   89,  108,   91,   74,   75,  108,   77,
 /*   820 */    96,  108,  108,  108,  108,  108,  108,   85,   86,  108,
 /*   830 */   108,   89,  108,   91,   74,   75,  108,   77,   96,  108,
 /*   840 */   108,    3,  108,  108,  108,   85,   86,   74,  108,   89,
 /*   850 */    77,   91,  108,  108,  108,  108,   96,   19,   85,   86,
 /*   860 */   108,  108,   89,  108,   91,  108,  108,   74,   30,   96,
 /*   870 */    77,  108,  108,  108,  108,  108,  108,  108,   85,   86,
 /*   880 */   108,  108,   89,  108,   91,   74,  108,   49,   77,   96,
 /*   890 */   108,  108,   74,  108,  108,   77,   85,   86,  108,  108,
 /*   900 */    89,  108,   91,   85,  108,  108,  108,   96,  108,   91,
 /*   910 */   108,  108,  108,  108,   96,
);
    const YY_SHIFT_USE_DFLT = -22;
    const YY_SHIFT_MAX = 162;
    static public $yy_shift_ofst = array(
 /*     0 */    21,  140,  -10,  -10,  -10,  -10,  -10,  -10,  -10,  -10,
 /*    10 */   -10,  -10,  235,  180,  180,  235,  180,  180,  180,  244,
 /*    20 */   180,  180,  180,  180,  180,  180,  180,  180,  180,  180,
 /*    30 */   180,  120,  190,  200,  370,  370,  453,  453,  453,  838,
 /*    40 */    84,  585,  137,   42,   42,  399,  399,  400,  391,   21,
 /*    50 */   266,  -21,   78,  434,   16,  293,  354,  380,    5,  253,
 /*    60 */   223,    5,  253,  223,    5,  583,  485,  485,   37,   -4,
 /*    70 */    89,   62,   62,   62,   62,   62,   62,   62,   62,  351,
 /*    80 */   224,  114,  157,  308,  133,   51,   15,  151,   51,  173,
 /*    90 */   295,  160,  177,  371,  329,  191,  160,   76,  160,  160,
 /*   100 */   355,  286,   17,   -2,   96,  301,   81,   57,  418,  418,
 /*   110 */   485,  485,  594,  485,  587,  418,  418,  433,  485,  418,
 /*   120 */   418,  -22,  -22,  -22,  -22,  -22,  348,  249,  247,  148,
 /*   130 */    52,  245,   52,   52,  322,  510,  494,  491,  562,  522,
 /*   140 */   514,  477,  512,  536,  571,  552,  531,  539,  569,  314,
 /*   150 */    95,   13,  513,   46,  364,  461,  505,  559,  546,  561,
 /*   160 */   545,  507,  511,
);
    const YY_REDUCE_USE_DFLT = -44;
    const YY_REDUCE_MAX = 125;
    static public $yy_reduce_ofst = array(
 /*     0 */   103,  -17,  320,  343,  287,  374,  233,  246,  264,  356,
 /*    10 */   442,  429,  403,  509,  496,  532,  460,  473,  655,  686,
 /*    20 */   673,  637,  550,  619,  568,  586,  599,  706,  760,  724,
 /*    30 */   742,  811,  793,  773,  305,  606,  818,  483,  693,   94,
 /*    40 */   171,  164,  144,   97,  109,  382,  205,  230,  271,  414,
 /*    50 */   407,  407,  197,  387,  352,  394,  197,  397,  285,  288,
 /*    60 */   325,  437,  376,  368,  398,  436,  132,  217,  498,  498,
 /*    70 */   498,  498,  498,  498,  498,  498,  498,  498,  498,  508,
 /*    80 */   518,  518,  484,  484,  484,  518,  495,  484,  518,  520,
 /*    90 */   504,  515,  294,  502,  502,  294,  515,  294,  515,  515,
 /*   100 */   294,  502,  502,  502,  502,  294,  294,  294,  294,  294,
 /*   110 */   401,  401,  523,  401,  493,  294,  294,  361,  401,  294,
 /*   120 */   294,  189,  -43,  411,  402,  194,
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
        /* 35 */ array(11, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 36 */ array(11, 15, 19, 30, 42, 43, 44, 45, 49, ),
        /* 37 */ array(11, 15, 19, 30, 42, 43, 44, 45, 49, ),
        /* 38 */ array(11, 15, 19, 30, 42, 43, 44, 45, 49, ),
        /* 39 */ array(3, 19, 30, 49, ),
        /* 40 */ array(15, 23, 25, 27, 28, ),
        /* 41 */ array(3, 11, 19, 30, 49, ),
        /* 42 */ array(19, 30, 49, ),
        /* 43 */ array(17, 20, 28, ),
        /* 44 */ array(17, 20, 28, ),
        /* 45 */ array(25, 27, ),
        /* 46 */ array(25, 27, ),
        /* 47 */ array(22, 27, ),
        /* 48 */ array(19, 49, ),
        /* 49 */ array(1, 2, 3, 5, 6, 7, 10, 62, 64, 65, 66, ),
        /* 50 */ array(16, 25, 31, 32, 33, 34, 35, 36, 37, 38, 61, ),
        /* 51 */ array(25, 31, 32, 33, 34, 35, 36, 37, 38, 61, ),
        /* 52 */ array(3, 26, 30, 46, 50, ),
        /* 53 */ array(1, 3, 19, 42, 48, ),
        /* 54 */ array(3, 14, 26, 30, ),
        /* 55 */ array(1, 3, 43, ),
        /* 56 */ array(3, 30, 50, ),
        /* 57 */ array(27, 28, ),
        /* 58 */ array(1, 3, ),
        /* 59 */ array(3, 30, ),
        /* 60 */ array(3, 30, ),
        /* 61 */ array(1, 3, ),
        /* 62 */ array(3, 30, ),
        /* 63 */ array(3, 30, ),
        /* 64 */ array(1, 3, ),
        /* 65 */ array(27, ),
        /* 66 */ array(28, ),
        /* 67 */ array(28, ),
        /* 68 */ array(24, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 69 */ array(4, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 70 */ array(16, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 71 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 72 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 73 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 74 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 75 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 76 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 77 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 78 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 79 */ array(1, 3, 19, 42, 48, ),
        /* 80 */ array(4, 12, 13, 47, ),
        /* 81 */ array(12, 13, 18, 47, ),
        /* 82 */ array(1, 3, 9, ),
        /* 83 */ array(1, 3, 43, ),
        /* 84 */ array(1, 3, 67, ),
        /* 85 */ array(12, 13, 47, ),
        /* 86 */ array(3, 26, 30, ),
        /* 87 */ array(1, 3, 63, ),
        /* 88 */ array(12, 13, 47, ),
        /* 89 */ array(3, 15, ),
        /* 90 */ array(19, 30, ),
        /* 91 */ array(17, 20, ),
        /* 92 */ array(4, 25, ),
        /* 93 */ array(4, 27, ),
        /* 94 */ array(4, 27, ),
        /* 95 */ array(21, 25, ),
        /* 96 */ array(17, 20, ),
        /* 97 */ array(4, 25, ),
        /* 98 */ array(17, 20, ),
        /* 99 */ array(17, 20, ),
        /* 100 */ array(4, 25, ),
        /* 101 */ array(4, 27, ),
        /* 102 */ array(4, 27, ),
        /* 103 */ array(4, 27, ),
        /* 104 */ array(4, 27, ),
        /* 105 */ array(4, 25, ),
        /* 106 */ array(25, 29, ),
        /* 107 */ array(16, 25, ),
        /* 108 */ array(25, ),
        /* 109 */ array(25, ),
        /* 110 */ array(28, ),
        /* 111 */ array(28, ),
        /* 112 */ array(19, ),
        /* 113 */ array(28, ),
        /* 114 */ array(15, ),
        /* 115 */ array(25, ),
        /* 116 */ array(25, ),
        /* 117 */ array(22, ),
        /* 118 */ array(28, ),
        /* 119 */ array(25, ),
        /* 120 */ array(25, ),
        /* 121 */ array(),
        /* 122 */ array(),
        /* 123 */ array(),
        /* 124 */ array(),
        /* 125 */ array(),
        /* 126 */ array(15, 23, 29, ),
        /* 127 */ array(15, 18, 23, ),
        /* 128 */ array(15, 23, 26, ),
        /* 129 */ array(30, 50, ),
        /* 130 */ array(15, 23, ),
        /* 131 */ array(21, 24, ),
        /* 132 */ array(15, 23, ),
        /* 133 */ array(15, 23, ),
        /* 134 */ array(18, 21, ),
        /* 135 */ array(30, ),
        /* 136 */ array(48, ),
        /* 137 */ array(48, ),
        /* 138 */ array(4, ),
        /* 139 */ array(30, ),
        /* 140 */ array(22, ),
        /* 141 */ array(49, ),
        /* 142 */ array(20, ),
        /* 143 */ array(19, ),
        /* 144 */ array(4, ),
        /* 145 */ array(26, ),
        /* 146 */ array(30, ),
        /* 147 */ array(26, ),
        /* 148 */ array(11, ),
        /* 149 */ array(30, ),
        /* 150 */ array(30, ),
        /* 151 */ array(30, ),
        /* 152 */ array(16, ),
        /* 153 */ array(30, ),
        /* 154 */ array(4, ),
        /* 155 */ array(30, ),
        /* 156 */ array(16, ),
        /* 157 */ array(4, ),
        /* 158 */ array(30, ),
        /* 159 */ array(16, ),
        /* 160 */ array(8, ),
        /* 161 */ array(15, ),
        /* 162 */ array(30, ),
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
        /* 259 */ array(),
        /* 260 */ array(),
        /* 261 */ array(),
        /* 262 */ array(),
        /* 263 */ array(),
        /* 264 */ array(),
        /* 265 */ array(),
);
    static public $yy_default = array(
 /*     0 */   411,  411,  411,  411,  411,  411,  411,  411,  411,  411,
 /*    10 */   411,  411,  396,  358,  358,  411,  358,  358,  411,  411,
 /*    20 */   411,  411,  411,  411,  411,  411,  411,  411,  411,  411,
 /*    30 */   411,  411,  411,  411,  411,  411,  411,  411,  411,  411,
 /*    40 */   295,  411,  411,  326,  281,  295,  295,  295,  411,  266,
 /*    50 */   368,  368,  334,  411,  411,  411,  334,  295,  411,  411,
 /*    60 */   411,  411,  411,  411,  411,  295,  321,  322,  411,  411,
 /*    70 */   411,  378,  377,  374,  373,  372,  382,  381,  366,  411,
 /*    80 */   411,  411,  411,  411,  411,  301,  411,  411,  363,  334,
 /*    90 */   411,  349,  411,  411,  411,  357,  351,  411,  350,  352,
 /*   100 */   411,  411,  411,  411,  411,  411,  397,  411,  289,  399,
 /*   110 */   346,  323,  411,  327,  334,  296,  369,  302,  324,  398,
 /*   120 */   299,  334,  362,  334,  334,  362,  300,  411,  300,  411,
 /*   130 */   364,  411,  411,  300,  411,  411,  411,  411,  411,  411,
 /*   140 */   344,  411,  311,  411,  411,  411,  411,  328,  411,  411,
 /*   150 */   411,  411,  411,  411,  411,  411,  411,  411,  411,  411,
 /*   160 */   411,  325,  411,  286,  267,  290,  297,  298,  288,  292,
 /*   170 */   291,  285,  391,  365,  375,  384,  390,  389,  387,  388,
 /*   180 */   385,  383,  376,  386,  287,  392,  380,  379,  371,  370,
 /*   190 */   367,  353,  312,  313,  314,  310,  305,  337,  303,  304,
 /*   200 */   315,  348,  400,  402,  403,  319,  318,  306,  317,  336,
 /*   210 */   335,  410,  408,  271,  409,  407,  268,  269,  270,  272,
 /*   220 */   273,  328,  330,  331,  333,  276,  274,  275,  404,  307,
 /*   230 */   344,  345,  332,  343,  329,  347,  316,  338,  342,  277,
 /*   240 */   280,  282,  283,  294,  293,  278,  279,  355,  341,  360,
 /*   250 */   393,  395,  359,  309,  361,  308,  394,  356,  401,  340,
 /*   260 */   339,  320,  406,  354,  405,  284,
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
    const YYNSTATE = 266;
    const YYNRULE = 145;
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
  'BOOLEAN',       'NULL',          'IN',            'ANDSYM',      
  'BACKTICK',      'HATCH',         'AT',            'ISODD',       
  'ISNOTODD',      'ISEVEN',        'ISNOTEVEN',     'ISODDBY',     
  'ISNOTODDBY',    'ISEVENBY',      'ISNOTEVENBY',   'ISDIVBY',     
  'ISNOTDIVBY',    'ISIN',          'LITERALSTART',  'LITERALEND',  
  'LDELIMTAG',     'RDELIMTAG',     'PHPSTART',      'PHPEND',      
  'error',         'start',         'template',      'template_element',
  'smartytag',     'text',          'variable',      'expr',        
  'attributes',    'varindexed',    'varvar',        'arrayindex',  
  'modifier',      'modparameters',  'ifexprs',       'statements',  
  'foraction',     'value',         'array',         'attribute',   
  'statement',     'exprs',         'math',          'function',    
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
 /*  66 */ "variable ::= DOLLAR ID COLON ID",
 /*  67 */ "arrayindex ::= arrayindex indexdef",
 /*  68 */ "arrayindex ::=",
 /*  69 */ "indexdef ::= DOT ID",
 /*  70 */ "indexdef ::= DOT INTEGER",
 /*  71 */ "indexdef ::= DOT variable",
 /*  72 */ "indexdef ::= DOT LDEL exprs RDEL",
 /*  73 */ "indexdef ::= OPENB ID CLOSEB",
 /*  74 */ "indexdef ::= OPENB exprs CLOSEB",
 /*  75 */ "indexdef ::= OPENB CLOSEB",
 /*  76 */ "varvar ::= varvarele",
 /*  77 */ "varvar ::= varvar varvarele",
 /*  78 */ "varvarele ::= ID",
 /*  79 */ "varvarele ::= LDEL expr RDEL",
 /*  80 */ "object ::= DOLLAR varvar arrayindex objectchain",
 /*  81 */ "objectchain ::= objectelement",
 /*  82 */ "objectchain ::= objectchain objectelement",
 /*  83 */ "objectelement ::= PTR ID arrayindex",
 /*  84 */ "objectelement ::= PTR variable arrayindex",
 /*  85 */ "objectelement ::= PTR LDEL expr RDEL arrayindex",
 /*  86 */ "objectelement ::= PTR ID LDEL expr RDEL arrayindex",
 /*  87 */ "objectelement ::= PTR method",
 /*  88 */ "function ::= ID OPENP params CLOSEP",
 /*  89 */ "method ::= ID OPENP params CLOSEP",
 /*  90 */ "params ::= expr COMMA params",
 /*  91 */ "params ::= expr",
 /*  92 */ "params ::=",
 /*  93 */ "modifier ::= VERT AT ID",
 /*  94 */ "modifier ::= VERT ID",
 /*  95 */ "modparameters ::= modparameters modparameter",
 /*  96 */ "modparameters ::=",
 /*  97 */ "modparameter ::= COLON exprs",
 /*  98 */ "modparameter ::= COLON ID",
 /*  99 */ "ifexprs ::= ifexpr",
 /* 100 */ "ifexprs ::= NOT ifexprs",
 /* 101 */ "ifexprs ::= OPENP ifexprs CLOSEP",
 /* 102 */ "ifexpr ::= expr",
 /* 103 */ "ifexpr ::= expr ifcond expr",
 /* 104 */ "ifexpr ::= expr ISIN array",
 /* 105 */ "ifexpr ::= expr ISIN value",
 /* 106 */ "ifexpr ::= ifexprs lop ifexprs",
 /* 107 */ "ifexpr ::= ifexprs ISDIVBY ifexprs",
 /* 108 */ "ifexpr ::= ifexprs ISNOTDIVBY ifexprs",
 /* 109 */ "ifexpr ::= ifexprs ISEVEN",
 /* 110 */ "ifexpr ::= ifexprs ISNOTEVEN",
 /* 111 */ "ifexpr ::= ifexprs ISEVENBY ifexprs",
 /* 112 */ "ifexpr ::= ifexprs ISNOTEVENBY ifexprs",
 /* 113 */ "ifexpr ::= ifexprs ISODD",
 /* 114 */ "ifexpr ::= ifexprs ISNOTODD",
 /* 115 */ "ifexpr ::= ifexprs ISODDBY ifexprs",
 /* 116 */ "ifexpr ::= ifexprs ISNOTODDBY ifexprs",
 /* 117 */ "ifcond ::= EQUALS",
 /* 118 */ "ifcond ::= NOTEQUALS",
 /* 119 */ "ifcond ::= GREATERTHAN",
 /* 120 */ "ifcond ::= LESSTHAN",
 /* 121 */ "ifcond ::= GREATEREQUAL",
 /* 122 */ "ifcond ::= LESSEQUAL",
 /* 123 */ "ifcond ::= IDENTITY",
 /* 124 */ "ifcond ::= NONEIDENTITY",
 /* 125 */ "lop ::= LAND",
 /* 126 */ "lop ::= LOR",
 /* 127 */ "array ::= OPENB arrayelements CLOSEB",
 /* 128 */ "arrayelements ::= arrayelement",
 /* 129 */ "arrayelements ::= arrayelements COMMA arrayelement",
 /* 130 */ "arrayelements ::=",
 /* 131 */ "arrayelement ::= expr",
 /* 132 */ "arrayelement ::= expr APTR expr",
 /* 133 */ "arrayelement ::= ID APTR expr",
 /* 134 */ "doublequoted ::= doublequoted doublequotedcontent",
 /* 135 */ "doublequoted ::= doublequotedcontent",
 /* 136 */ "doublequotedcontent ::= BACKTICK ID BACKTICK",
 /* 137 */ "doublequotedcontent ::= BACKTICK variable BACKTICK",
 /* 138 */ "doublequotedcontent ::= DOLLAR ID",
 /* 139 */ "doublequotedcontent ::= LDEL expr RDEL",
 /* 140 */ "doublequotedcontent ::= OTHER",
 /* 141 */ "text ::= text textelement",
 /* 142 */ "text ::= textelement",
 /* 143 */ "textelement ::= OTHER",
 /* 144 */ "textelement ::= LDEL",
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
  array( 'lhs' => 72, 'rhs' => 11 ),
  array( 'lhs' => 84, 'rhs' => 2 ),
  array( 'lhs' => 84, 'rhs' => 1 ),
  array( 'lhs' => 72, 'rhs' => 8 ),
  array( 'lhs' => 72, 'rhs' => 8 ),
  array( 'lhs' => 76, 'rhs' => 2 ),
  array( 'lhs' => 76, 'rhs' => 1 ),
  array( 'lhs' => 76, 'rhs' => 0 ),
  array( 'lhs' => 87, 'rhs' => 4 ),
  array( 'lhs' => 83, 'rhs' => 1 ),
  array( 'lhs' => 83, 'rhs' => 3 ),
  array( 'lhs' => 88, 'rhs' => 4 ),
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
  array( 'lhs' => 86, 'rhs' => 3 ),
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
        38 => 0,
        44 => 0,
        45 => 0,
        47 => 0,
        48 => 0,
        49 => 0,
        64 => 0,
        128 => 0,
        1 => 1,
        35 => 1,
        37 => 1,
        42 => 1,
        43 => 1,
        76 => 1,
        99 => 1,
        135 => 1,
        142 => 1,
        143 => 1,
        144 => 1,
        2 => 2,
        67 => 2,
        134 => 2,
        141 => 2,
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
        91 => 24,
        131 => 24,
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
        66 => 66,
        68 => 68,
        96 => 68,
        69 => 69,
        70 => 70,
        71 => 71,
        72 => 72,
        74 => 72,
        73 => 73,
        75 => 75,
        77 => 77,
        78 => 78,
        79 => 79,
        101 => 79,
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
        90 => 90,
        92 => 92,
        93 => 93,
        94 => 94,
        95 => 95,
        97 => 97,
        98 => 98,
        100 => 100,
        102 => 102,
        103 => 103,
        106 => 103,
        104 => 104,
        105 => 105,
        107 => 107,
        108 => 108,
        109 => 109,
        114 => 109,
        110 => 110,
        113 => 110,
        111 => 111,
        116 => 111,
        112 => 112,
        115 => 112,
        117 => 117,
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
        129 => 129,
        130 => 130,
        132 => 132,
        133 => 133,
        136 => 136,
        137 => 137,
        138 => 138,
        139 => 139,
        140 => 140,
    );
    /* Beginning here are the reduction cases.  A typical example
    ** follows:
    **  #line <lineno> <grammarfile>
    **   function yy_r0($yymsp){ ... }           // User supplied code
    **  #line <lineno> <thisfile>
    */
#line 73 "internal.templateparser.y"
    function yy_r0(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;     }
#line 1820 "internal.templateparser.php"
#line 79 "internal.templateparser.y"
    function yy_r1(){$this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1823 "internal.templateparser.php"
#line 81 "internal.templateparser.y"
    function yy_r2(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1826 "internal.templateparser.php"
#line 87 "internal.templateparser.y"
    function yy_r3(){if ($this->compiler->has_code) {
                                            $tmp =''; foreach ($this->prefix_code as $code) {$tmp.=$code;} $this->prefix_code=array();
                                            $this->_retvalue = $this->cacher->processNocacheCode($tmp.$this->yystack[$this->yyidx + 0]->minor, $this->compiler,$this->nocache,true);
                                         } $this->nocache=false;    }
#line 1832 "internal.templateparser.php"
#line 100 "internal.templateparser.y"
    function yy_r4(){ $this->_retvalue = '';    }
#line 1835 "internal.templateparser.php"
#line 103 "internal.templateparser.y"
    function yy_r5(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + -1]->minor, $this->compiler,false,false);    }
#line 1838 "internal.templateparser.php"
#line 105 "internal.templateparser.y"
    function yy_r6(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->left_delimiter, $this->compiler,false,false);    }
#line 1841 "internal.templateparser.php"
#line 107 "internal.templateparser.y"
    function yy_r7(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->right_delimiter, $this->compiler,false,false);    }
#line 1844 "internal.templateparser.php"
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
#line 1855 "internal.templateparser.php"
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
#line 1866 "internal.templateparser.php"
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
#line 1877 "internal.templateparser.php"
#line 139 "internal.templateparser.y"
    function yy_r11(){$this->_retvalue = $this->cacher->processNocacheCode("<?php echo '".$this->yystack[$this->yyidx + 0]->minor."';?>\n", $this->compiler, true, true);    }
#line 1880 "internal.templateparser.php"
#line 141 "internal.templateparser.y"
    function yy_r12(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + 0]->minor, $this->compiler,false,false);    }
#line 1883 "internal.templateparser.php"
#line 149 "internal.templateparser.y"
    function yy_r13(){ $this->_retvalue = $this->compiler->compileTag('print_expression',array_merge(array('value'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1886 "internal.templateparser.php"
#line 151 "internal.templateparser.y"
    function yy_r14(){ $this->_retvalue = $this->compiler->compileTag('assign',array_merge(array('value'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -4]->minor,$this->yystack[$this->yyidx + -1]->minor));    }
#line 1889 "internal.templateparser.php"
#line 152 "internal.templateparser.y"
    function yy_r15(){$this->_retvalue = array('var'=>$this->yystack[$this->yyidx + -1]->minor, 'index'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1892 "internal.templateparser.php"
#line 155 "internal.templateparser.y"
    function yy_r16(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor,$this->yystack[$this->yyidx + -1]->minor);    }
#line 1895 "internal.templateparser.php"
#line 157 "internal.templateparser.y"
    function yy_r17(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -4]->minor,array_merge(array('object_methode'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1898 "internal.templateparser.php"
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
#line 1913 "internal.templateparser.php"
#line 173 "internal.templateparser.y"
    function yy_r19(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor.'close',$this->yystack[$this->yyidx + -1]->minor);    }
#line 1916 "internal.templateparser.php"
#line 175 "internal.templateparser.y"
    function yy_r20(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor.'close',array('object_methode'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1919 "internal.templateparser.php"
#line 177 "internal.templateparser.y"
    function yy_r21(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor,array('if condition'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1922 "internal.templateparser.php"
#line 179 "internal.templateparser.y"
    function yy_r22(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -9]->minor,array('start'=>$this->yystack[$this->yyidx + -7]->minor,'ifexp'=>$this->yystack[$this->yyidx + -5]->minor,'varloop'=>$this->yystack[$this->yyidx + -2]->minor,'loop'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1925 "internal.templateparser.php"
#line 180 "internal.templateparser.y"
    function yy_r23(){ $this->_retvalue = '='.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1928 "internal.templateparser.php"
#line 181 "internal.templateparser.y"
    function yy_r24(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1931 "internal.templateparser.php"
#line 184 "internal.templateparser.y"
    function yy_r25(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -6]->minor,array('from'=>$this->yystack[$this->yyidx + -1]->minor,'item'=>$this->yystack[$this->yyidx + -3]->minor));    }
#line 1934 "internal.templateparser.php"
#line 191 "internal.templateparser.y"
    function yy_r27(){ $this->_retvalue = array_merge($this->yystack[$this->yyidx + -1]->minor,$this->yystack[$this->yyidx + 0]->minor);    }
#line 1937 "internal.templateparser.php"
#line 195 "internal.templateparser.y"
    function yy_r29(){ $this->_retvalue = array();    }
#line 1940 "internal.templateparser.php"
#line 199 "internal.templateparser.y"
    function yy_r30(){ $this->_retvalue = array($this->yystack[$this->yyidx + -2]->minor=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1943 "internal.templateparser.php"
#line 204 "internal.templateparser.y"
    function yy_r31(){ $this->_retvalue = array($this->yystack[$this->yyidx + 0]->minor);    }
#line 1946 "internal.templateparser.php"
#line 205 "internal.templateparser.y"
    function yy_r32(){ $this->yystack[$this->yyidx + -2]->minor[]=$this->yystack[$this->yyidx + 0]->minor; $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor;    }
#line 1949 "internal.templateparser.php"
#line 207 "internal.templateparser.y"
    function yy_r33(){ $this->_retvalue = array('var' => $this->yystack[$this->yyidx + -2]->minor, 'value'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1952 "internal.templateparser.php"
#line 214 "internal.templateparser.y"
    function yy_r34(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';     }
#line 1955 "internal.templateparser.php"
#line 218 "internal.templateparser.y"
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
#line 1970 "internal.templateparser.php"
#line 236 "internal.templateparser.y"
    function yy_r39(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1973 "internal.templateparser.php"
#line 238 "internal.templateparser.y"
    function yy_r40(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor . $this->yystack[$this->yyidx + -1]->minor . $this->yystack[$this->yyidx + 0]->minor;     }
#line 1976 "internal.templateparser.php"
#line 240 "internal.templateparser.y"
    function yy_r41(){ $this->_retvalue = '('. $this->yystack[$this->yyidx + -2]->minor . ').(' . $this->yystack[$this->yyidx + 0]->minor. ')';     }
#line 1979 "internal.templateparser.php"
#line 257 "internal.templateparser.y"
    function yy_r46(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1982 "internal.templateparser.php"
#line 266 "internal.templateparser.y"
    function yy_r50(){ $this->_retvalue = "(". $this->yystack[$this->yyidx + -1]->minor .")";     }
#line 1985 "internal.templateparser.php"
#line 269 "internal.templateparser.y"
    function yy_r51(){ $this->_retvalue = "'".$this->yystack[$this->yyidx + -1]->minor."'";     }
#line 1988 "internal.templateparser.php"
#line 270 "internal.templateparser.y"
    function yy_r52(){ $this->_retvalue = "''";     }
#line 1991 "internal.templateparser.php"
#line 272 "internal.templateparser.y"
    function yy_r53(){ $this->_retvalue = "'".str_replace('\"','"',$this->yystack[$this->yyidx + -1]->minor)."'";     }
#line 1994 "internal.templateparser.php"
#line 278 "internal.templateparser.y"
    function yy_r55(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1997 "internal.templateparser.php"
#line 279 "internal.templateparser.y"
    function yy_r56(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -3]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -6]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -1]->minor .')';     }
#line 2000 "internal.templateparser.php"
#line 281 "internal.templateparser.y"
    function yy_r57(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor.'::'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2003 "internal.templateparser.php"
#line 282 "internal.templateparser.y"
    function yy_r58(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -4]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -7]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -2]->minor .')'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2006 "internal.templateparser.php"
#line 284 "internal.templateparser.y"
    function yy_r59(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2009 "internal.templateparser.php"
#line 286 "internal.templateparser.y"
    function yy_r60(){ $this->_retvalue = $this->yystack[$this->yyidx + -4]->minor.'::$'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2012 "internal.templateparser.php"
#line 288 "internal.templateparser.y"
    function yy_r61(){ $this->_retvalue = $this->yystack[$this->yyidx + -5]->minor.'::$'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2015 "internal.templateparser.php"
#line 297 "internal.templateparser.y"
    function yy_r62(){ if ($this->yystack[$this->yyidx + 0]->minor['var'] == '\'smarty\'') { $this->_retvalue =  $this->compiler->compileTag(trim($this->yystack[$this->yyidx + 0]->minor['var'],"'"),$this->yystack[$this->yyidx + 0]->minor['index']);} else {
                                                         $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + 0]->minor['var'] .')->value'.$this->yystack[$this->yyidx + 0]->minor['index']; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + 0]->minor['var'],"'"))->nocache;}    }
#line 2019 "internal.templateparser.php"
#line 300 "internal.templateparser.y"
    function yy_r63(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->'.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 2022 "internal.templateparser.php"
#line 304 "internal.templateparser.y"
    function yy_r65(){$this->_retvalue = '$_smarty_tpl->getConfigVariable(\''. $this->yystack[$this->yyidx + -1]->minor .'\')';    }
#line 2025 "internal.templateparser.php"
#line 306 "internal.templateparser.y"
    function yy_r66(){$this->_retvalue = '$_smarty_tpl->getStreamVariable(\''. $this->yystack[$this->yyidx + -2]->minor .'://'. $this->yystack[$this->yyidx + 0]->minor. '\')';    }
#line 2028 "internal.templateparser.php"
#line 314 "internal.templateparser.y"
    function yy_r68(){return;    }
#line 2031 "internal.templateparser.php"
#line 318 "internal.templateparser.y"
    function yy_r69(){ $this->_retvalue = "['". $this->yystack[$this->yyidx + 0]->minor ."']";    }
#line 2034 "internal.templateparser.php"
#line 319 "internal.templateparser.y"
    function yy_r70(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + 0]->minor ."]";    }
#line 2037 "internal.templateparser.php"
#line 321 "internal.templateparser.y"
    function yy_r71(){ $this->_retvalue = "[".$this->yystack[$this->yyidx + 0]->minor."]";    }
#line 2040 "internal.templateparser.php"
#line 322 "internal.templateparser.y"
    function yy_r72(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + -1]->minor ."]";    }
#line 2043 "internal.templateparser.php"
#line 324 "internal.templateparser.y"
    function yy_r73(){ $this->_retvalue = '['.$this->compiler->compileTag('smarty','[\'section\'][\''.$this->yystack[$this->yyidx + -1]->minor.'\'][\'index\']').']';    }
#line 2046 "internal.templateparser.php"
#line 328 "internal.templateparser.y"
    function yy_r75(){$this->_retvalue = '';    }
#line 2049 "internal.templateparser.php"
#line 336 "internal.templateparser.y"
    function yy_r77(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2052 "internal.templateparser.php"
#line 338 "internal.templateparser.y"
    function yy_r78(){$this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2055 "internal.templateparser.php"
#line 340 "internal.templateparser.y"
    function yy_r79(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2058 "internal.templateparser.php"
#line 345 "internal.templateparser.y"
    function yy_r80(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->value'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 2061 "internal.templateparser.php"
#line 347 "internal.templateparser.y"
    function yy_r81(){$this->_retvalue  = $this->yystack[$this->yyidx + 0]->minor;     }
#line 2064 "internal.templateparser.php"
#line 349 "internal.templateparser.y"
    function yy_r82(){$this->_retvalue  = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2067 "internal.templateparser.php"
#line 351 "internal.templateparser.y"
    function yy_r83(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2070 "internal.templateparser.php"
#line 352 "internal.templateparser.y"
    function yy_r84(){ $this->_retvalue = '->{'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2073 "internal.templateparser.php"
#line 353 "internal.templateparser.y"
    function yy_r85(){ $this->_retvalue = '->{'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2076 "internal.templateparser.php"
#line 354 "internal.templateparser.y"
    function yy_r86(){ $this->_retvalue = '->{\''.$this->yystack[$this->yyidx + -4]->minor.'\'.'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2079 "internal.templateparser.php"
#line 356 "internal.templateparser.y"
    function yy_r87(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2082 "internal.templateparser.php"
#line 362 "internal.templateparser.y"
    function yy_r88(){if (!$this->template->security || $this->smarty->security_handler->isTrustedPhpFunction($this->yystack[$this->yyidx + -3]->minor, $this->compiler)) {
																					            if ($this->yystack[$this->yyidx + -3]->minor == 'isset' || $this->yystack[$this->yyidx + -3]->minor == 'empty' || $this->yystack[$this->yyidx + -3]->minor == 'array' || is_callable($this->yystack[$this->yyidx + -3]->minor)) {
																					                $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";
																					            } else {
                                                       $this->compiler->trigger_template_error ("unknown function \"" . $this->yystack[$this->yyidx + -3]->minor . "\"");
                                                      }
                                                    }    }
#line 2091 "internal.templateparser.php"
#line 373 "internal.templateparser.y"
    function yy_r89(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";    }
#line 2094 "internal.templateparser.php"
#line 377 "internal.templateparser.y"
    function yy_r90(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.",".$this->yystack[$this->yyidx + 0]->minor;    }
#line 2097 "internal.templateparser.php"
#line 381 "internal.templateparser.y"
    function yy_r92(){ return;    }
#line 2100 "internal.templateparser.php"
#line 386 "internal.templateparser.y"
    function yy_r93(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,true);    }
#line 2103 "internal.templateparser.php"
#line 387 "internal.templateparser.y"
    function yy_r94(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,false);    }
#line 2106 "internal.templateparser.php"
#line 394 "internal.templateparser.y"
    function yy_r95(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2109 "internal.templateparser.php"
#line 398 "internal.templateparser.y"
    function yy_r97(){$this->_retvalue = ','.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2112 "internal.templateparser.php"
#line 399 "internal.templateparser.y"
    function yy_r98(){$this->_retvalue = ',\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2115 "internal.templateparser.php"
#line 406 "internal.templateparser.y"
    function yy_r100(){$this->_retvalue = '!'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2118 "internal.templateparser.php"
#line 411 "internal.templateparser.y"
    function yy_r102(){$this->_retvalue =$this->yystack[$this->yyidx + 0]->minor;    }
#line 2121 "internal.templateparser.php"
#line 412 "internal.templateparser.y"
    function yy_r103(){$this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2124 "internal.templateparser.php"
#line 413 "internal.templateparser.y"
    function yy_r104(){$this->_retvalue = 'in_array('.$this->yystack[$this->yyidx + -2]->minor.','.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2127 "internal.templateparser.php"
#line 414 "internal.templateparser.y"
    function yy_r105(){$this->_retvalue = 'in_array('.$this->yystack[$this->yyidx + -2]->minor.',(array)'.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2130 "internal.templateparser.php"
#line 416 "internal.templateparser.y"
    function yy_r107(){$this->_retvalue = '!('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2133 "internal.templateparser.php"
#line 417 "internal.templateparser.y"
    function yy_r108(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2136 "internal.templateparser.php"
#line 418 "internal.templateparser.y"
    function yy_r109(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2139 "internal.templateparser.php"
#line 419 "internal.templateparser.y"
    function yy_r110(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2142 "internal.templateparser.php"
#line 420 "internal.templateparser.y"
    function yy_r111(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2145 "internal.templateparser.php"
#line 421 "internal.templateparser.y"
    function yy_r112(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2148 "internal.templateparser.php"
#line 427 "internal.templateparser.y"
    function yy_r117(){$this->_retvalue = '==';    }
#line 2151 "internal.templateparser.php"
#line 428 "internal.templateparser.y"
    function yy_r118(){$this->_retvalue = '!=';    }
#line 2154 "internal.templateparser.php"
#line 429 "internal.templateparser.y"
    function yy_r119(){$this->_retvalue = '>';    }
#line 2157 "internal.templateparser.php"
#line 430 "internal.templateparser.y"
    function yy_r120(){$this->_retvalue = '<';    }
#line 2160 "internal.templateparser.php"
#line 431 "internal.templateparser.y"
    function yy_r121(){$this->_retvalue = '>=';    }
#line 2163 "internal.templateparser.php"
#line 432 "internal.templateparser.y"
    function yy_r122(){$this->_retvalue = '<=';    }
#line 2166 "internal.templateparser.php"
#line 433 "internal.templateparser.y"
    function yy_r123(){$this->_retvalue = '===';    }
#line 2169 "internal.templateparser.php"
#line 434 "internal.templateparser.y"
    function yy_r124(){$this->_retvalue = '!==';    }
#line 2172 "internal.templateparser.php"
#line 436 "internal.templateparser.y"
    function yy_r125(){$this->_retvalue = '&&';    }
#line 2175 "internal.templateparser.php"
#line 437 "internal.templateparser.y"
    function yy_r126(){$this->_retvalue = '||';    }
#line 2178 "internal.templateparser.php"
#line 442 "internal.templateparser.y"
    function yy_r127(){ $this->_retvalue = 'array('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2181 "internal.templateparser.php"
#line 444 "internal.templateparser.y"
    function yy_r129(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.','.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2184 "internal.templateparser.php"
#line 445 "internal.templateparser.y"
    function yy_r130(){ return;     }
#line 2187 "internal.templateparser.php"
#line 447 "internal.templateparser.y"
    function yy_r132(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2190 "internal.templateparser.php"
#line 448 "internal.templateparser.y"
    function yy_r133(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + -2]->minor.'\'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2193 "internal.templateparser.php"
#line 455 "internal.templateparser.y"
    function yy_r136(){$this->_retvalue = "`".$this->yystack[$this->yyidx + -1]->minor."`";    }
#line 2196 "internal.templateparser.php"
#line 456 "internal.templateparser.y"
    function yy_r137(){$this->_retvalue = "'.".$this->yystack[$this->yyidx + -1]->minor.".'";    }
#line 2199 "internal.templateparser.php"
#line 457 "internal.templateparser.y"
    function yy_r138(){$this->_retvalue = "'.".'$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + 0]->minor .'\')->value'.".'"; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + 0]->minor,"'"))->nocache;    }
#line 2202 "internal.templateparser.php"
#line 458 "internal.templateparser.y"
    function yy_r139(){$this->_retvalue = "'.(".$this->yystack[$this->yyidx + -1]->minor.").'";    }
#line 2205 "internal.templateparser.php"
#line 459 "internal.templateparser.y"
    function yy_r140(){$this->_retvalue = addcslashes($this->yystack[$this->yyidx + 0]->minor,"'");    }
#line 2208 "internal.templateparser.php"

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
#line 2326 "internal.templateparser.php"
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
#line 2351 "internal.templateparser.php"
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
