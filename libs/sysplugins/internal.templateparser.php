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
    const YY_NO_ACTION = 408;
    const YY_ACCEPT_ACTION = 407;
    const YY_ERROR_ACTION = 406;

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
    const YY_SZ_ACTTAB = 941;
static public $yy_action = array(
 /*     0 */   186,  189,  204,  169,   23,  192,   22,  407,   52,  177,
 /*    10 */   176,  200,  201,  194,  198,  196,    3,    4,    5,    2,
 /*    20 */    10,   11,   64,   23,  189,  204,  145,   19,   38,  241,
 /*    30 */     8,  213,   12,  107,   64,  201,  194,  198,  196,    3,
 /*    40 */     4,    5,    2,   10,   11,  131,   19,   34,   13,   21,
 /*    50 */   213,  158,  135,  208,    6,  133,  105,   39,   57,  246,
 /*    60 */   240,   15,  133,  197,  135,  189,  204,   56,  243,  261,
 /*    70 */   166,  175,  174,  173,  172,  179,  201,  194,  198,  196,
 /*    80 */     3,    4,    5,    2,   10,   11,  140,  217,  178,   53,
 /*    90 */   152,  202,  169,  169,  192,  192,   69,  132,   35,  162,
 /*   100 */   161,   13,  183,   87,  227,  236,  189,  204,  145,  105,
 /*   110 */   203,   23,   25,   64,   12,  170,   64,  201,  194,  198,
 /*   120 */   196,    3,    4,    5,    2,   10,   11,  129,   32,  145,
 /*   130 */   108,   38,  193,    8,  219,   12,  168,   64,  213,   39,
 /*   140 */    57,  246,  240,  135,   64,  145,  135,   38,  131,   25,
 /*   150 */   259,   12,  232,   64,  223,  181,   26,    6,   13,  190,
 /*   160 */    39,   57,  246,  240,  128,  133,  105,  135,  133,   30,
 /*   170 */     1,  137,   64,  140,  135,  199,   39,   57,  246,  240,
 /*   180 */   214,  148,   24,  135,  216,  188,   50,  124,  145,   62,
 /*   190 */    38,  206,    8,   65,   12,  255,   60,  210,  211,  210,
 /*   200 */   211,   45,  135,  226,  145,  229,   38,  127,   25,  223,
 /*   210 */    12,   26,   64,  169,   31,  192,    6,   43,   64,   39,
 /*   220 */    57,  246,  240,   42,   88,   41,  135,   64,   31,  136,
 /*   230 */   233,   43,   37,  202,   37,   39,   57,  246,  240,  210,
 /*   240 */   211,   59,  135,  167,  195,   58,   13,  145,  135,   38,
 /*   250 */   221,   25,  203,   12,  105,   64,   45,  135,  191,  208,
 /*   260 */    28,  156,  250,  145,  230,   38,  131,   25,  133,   12,
 /*   270 */   121,   64,  154,  202,   37,  160,  187,  176,   39,   57,
 /*   280 */   246,  240,  129,  138,  159,  135,  202,  178,  106,  220,
 /*   290 */   202,  169,  203,  192,   39,   57,  246,  240,  162,  161,
 /*   300 */    23,  135,   87,   17,  236,  203,  145,  133,   38,  203,
 /*   310 */    25,  180,   12,  233,   64,  112,  202,  130,  248,  254,
 /*   320 */   140,  143,  145,   18,   38,  134,   25,  213,   12,   33,
 /*   330 */    64,   40,  101,  258,  140,  203,  123,   39,   57,  246,
 /*   340 */   240,  126,  140,  251,  135,  222,  178,  103,  115,  202,
 /*   350 */   193,   97,  219,   39,   57,  246,  240,  162,  161,  224,
 /*   360 */   135,   87,  251,  236,  247,   95,  231,   14,  203,  124,
 /*   370 */   178,   51,   23,  202,  164,  209,  251,  202,   70,   89,
 /*   380 */   133,  162,  161,  178,   53,   87,  202,  236,   29,  110,
 /*   390 */   251,   77,  203,  219,  162,  161,  203,  170,   87,  213,
 /*   400 */   236,  178,   53,  114,  202,  203,    9,   49,  139,   78,
 /*   410 */   170,  202,  162,  161,  178,   53,   87,  202,  236,  158,
 /*   420 */    82,  122,   73,  203,  102,  162,  161,  257,  170,   87,
 /*   430 */   203,  236,  178,  103,   85,  202,  203,  133,   93,  140,
 /*   440 */   217,  170,  133,  162,  161,  178,   53,   87,  202,  236,
 /*   450 */   140,  239,  155,   74,  203,  253,  162,  161,  178,   53,
 /*   460 */    87,  202,  236,  169,  205,  192,   76,  203,  191,  162,
 /*   470 */   161,   15,  170,   87,  140,  236,  178,   53,  140,  202,
 /*   480 */   203,  125,  153,   83,   71,  170,  109,  162,  161,  178,
 /*   490 */    53,   87,  202,  236,  218,  184,  212,   79,  203,   84,
 /*   500 */   162,  161,  133,  170,   87,  260,  236,  178,  103,  118,
 /*   510 */   202,  203,   41,  219,  124,  133,  170,  191,  162,  161,
 /*   520 */   178,   53,   87,  202,  236,  141,   27,  150,   75,  203,
 /*   530 */    98,  162,  161,  191,  124,   87,   20,  236,   90,   94,
 /*   540 */    13,  251,  203,   61,    7,  182,  133,  170,  105,   54,
 /*   550 */   251,   27,  243,  261,  166,  175,  174,  173,  172,  179,
 /*   560 */   234,   44,  178,   53,  145,  202,  249,   55,   25,  217,
 /*   570 */    72,  185,   64,  162,  161,  142,  149,   87,  245,  236,
 /*   580 */   212,   63,   35,  129,  203,   66,  163,  217,   68,  170,
 /*   590 */   237,  207,  228,  144,  235,   39,   57,  246,  240,  225,
 /*   600 */   252,   36,  135,  178,  106,  193,  202,  212,  165,   33,
 /*   610 */   209,  104,   46,   67,  162,  161,   16,  254,   87,  140,
 /*   620 */   236,  178,  103,  254,  202,  203,  254,  254,  254,  254,
 /*   630 */   254,  254,  162,  161,  238,  254,   87,  254,  236,  178,
 /*   640 */    99,  157,  202,  203,  254,  254,  254,  254,  254,  254,
 /*   650 */   162,  161,  178,  119,   87,  202,  236,  254,  254,  254,
 /*   660 */   254,  203,  254,  162,  161,  254,  254,   87,  254,  236,
 /*   670 */   178,  120,  178,  202,  203,  202,  254,  254,  254,  254,
 /*   680 */   254,  162,  161,  147,  146,   87,  254,  236,  254,  236,
 /*   690 */   178,   92,  203,  202,  203,  254,  254,  254,  254,  254,
 /*   700 */   254,  162,  161,  254,  254,   87,  254,  236,  178,   96,
 /*   710 */   254,  202,  203,  254,  254,  254,  254,  254,  254,  162,
 /*   720 */   161,  254,  254,   87,  254,  236,  178,  100,  254,  202,
 /*   730 */   203,  254,  254,  254,  254,  254,  254,  162,  161,  178,
 /*   740 */    48,   87,  151,  236,  254,  254,  254,  254,  203,  254,
 /*   750 */   162,  161,  254,  254,   87,  254,  236,  178,  113,  178,
 /*   760 */   202,  203,  202,  254,  254,  254,  254,  254,  162,  161,
 /*   770 */   242,  244,   87,  254,  236,  254,  236,  178,  117,  203,
 /*   780 */   202,  203,  254,  254,  254,  254,  254,  254,  162,  161,
 /*   790 */   254,  254,   87,  254,  236,  178,  111,  254,  202,  203,
 /*   800 */   254,  254,  254,  254,  254,  254,  162,  161,  254,  254,
 /*   810 */    87,  254,  236,  178,   91,  254,  202,  203,  254,  254,
 /*   820 */   254,  254,  254,  254,  162,  161,  178,  116,   87,  202,
 /*   830 */   236,  254,  254,  254,  254,  203,  254,  162,  161,  254,
 /*   840 */   254,   87,  254,  236,  178,   47,  178,  202,  203,  202,
 /*   850 */   254,  254,  254,  254,  254,  162,  161,  215,  254,   87,
 /*   860 */   254,  236,  254,  236,  178,  254,  203,  202,  203,  254,
 /*   870 */   254,  254,  254,  254,  254,  162,  161,  254,  254,   86,
 /*   880 */   254,  236,  178,  254,  254,  202,  203,  254,  254,  254,
 /*   890 */   254,  254,  254,  162,  161,  254,  254,   80,  254,  236,
 /*   900 */   178,  254,  178,  202,  203,  202,  254,  254,  254,  254,
 /*   910 */   254,  162,  161,  171,  254,   81,  254,  236,  178,  236,
 /*   920 */   254,  202,  203,  254,  203,  254,  254,  254,  254,  256,
 /*   930 */   254,  254,  254,  254,  254,  236,  254,  254,  254,  254,
 /*   940 */   203,
    );
    static public $yy_lookahead = array(
 /*     0 */    16,   40,   41,    1,    3,    3,    3,   69,   70,   71,
 /*    10 */    72,    9,   51,   52,   53,   54,   55,   56,   57,   58,
 /*    20 */    59,   60,   19,    3,   40,   41,   11,   26,   13,    4,
 /*    30 */    15,   30,   17,   30,   19,   51,   52,   53,   54,   55,
 /*    40 */    56,   57,   58,   59,   60,   30,   26,   46,   15,    3,
 /*    50 */    30,   50,   49,   16,   39,   25,   23,   42,   43,   44,
 /*    60 */    45,   15,   25,    4,   49,   40,   41,   78,   31,   32,
 /*    70 */    33,   34,   35,   36,   37,   38,   51,   52,   53,   54,
 /*    80 */    55,   56,   57,   58,   59,   60,   27,   98,   74,   75,
 /*    90 */    24,   77,    1,    1,    3,    3,   82,   83,   61,   85,
 /*   100 */    86,   15,   88,   89,   18,   91,   40,   41,   11,   23,
 /*   110 */    96,    3,   15,   19,   17,  101,   19,   51,   52,   53,
 /*   120 */    54,   55,   56,   57,   58,   59,   60,   30,    3,   11,
 /*   130 */    95,   13,   97,   15,   99,   17,   11,   19,   30,   42,
 /*   140 */    43,   44,   45,   49,   19,   11,   49,   13,   30,   15,
 /*   150 */     4,   17,   18,   19,    1,   30,    3,   39,   15,   67,
 /*   160 */    42,   43,   44,   45,   30,   25,   23,   49,   25,   29,
 /*   170 */    27,   28,   19,   27,   49,    4,   42,   43,   44,   45,
 /*   180 */     1,    2,    3,   49,    5,    6,    7,   80,   11,   10,
 /*   190 */    13,    4,   15,   19,   17,   42,   19,   12,   13,   12,
 /*   200 */    13,   48,   49,   18,   11,   16,   13,   30,   15,    1,
 /*   210 */    17,    3,   19,    1,   17,    3,   39,   20,   19,   42,
 /*   220 */    43,   44,   45,   30,   73,   28,   49,   19,   17,   30,
 /*   230 */    74,   20,   47,   77,   47,   42,   43,   44,   45,   12,
 /*   240 */    13,   62,   49,   64,   65,   66,   15,   11,   49,   13,
 /*   250 */    42,   15,   96,   17,   23,   19,   48,   49,  107,   16,
 /*   260 */    29,   19,  106,   11,   30,   13,   30,   15,   25,   17,
 /*   270 */    74,   19,   30,   77,   47,   63,   71,   72,   42,   43,
 /*   280 */    44,   45,   30,   74,   50,   49,   77,   74,   75,   93,
 /*   290 */    77,    1,   96,    3,   42,   43,   44,   45,   85,   86,
 /*   300 */     3,   49,   89,   21,   91,   96,   11,   25,   13,   96,
 /*   310 */    15,   14,   17,   74,   19,   30,   77,  104,  105,    4,
 /*   320 */    27,   28,   11,   26,   13,   30,   15,   30,   17,   22,
 /*   330 */    19,   92,   76,   43,   27,   96,   80,   42,   43,   44,
 /*   340 */    45,   30,   27,   87,   49,  106,   74,   75,   95,   77,
 /*   350 */    97,   76,   99,   42,   43,   44,   45,   85,   86,    4,
 /*   360 */    49,   89,   87,   91,   18,   76,   94,   21,   96,   80,
 /*   370 */    74,   75,    3,   77,   74,  100,   87,   77,   82,   76,
 /*   380 */    25,   85,   86,   74,   75,   89,   77,   91,   26,   95,
 /*   390 */    87,   82,   96,   99,   85,   86,   96,  101,   89,   30,
 /*   400 */    91,   74,   75,   21,   77,   96,   24,   81,   74,   82,
 /*   410 */   101,   77,   85,   86,   74,   75,   89,   77,   91,   50,
 /*   420 */    78,    4,   82,   96,   79,   85,   86,    4,  101,   89,
 /*   430 */    96,   91,   74,   75,   73,   77,   96,   25,   79,   27,
 /*   440 */    98,  101,   25,   85,   86,   74,   75,   89,   77,   91,
 /*   450 */    27,    4,   94,   82,   96,    4,   85,   86,   74,   75,
 /*   460 */    89,   77,   91,    1,   99,    3,   82,   96,  107,   85,
 /*   470 */    86,   15,  101,   89,   27,   91,   74,   75,   27,   77,
 /*   480 */    96,    4,   84,   73,   82,  101,   81,   85,   86,   74,
 /*   490 */    75,   89,   77,   91,    4,   88,   98,   82,   96,   73,
 /*   500 */    85,   86,   25,  101,   89,   43,   91,   74,   75,   95,
 /*   510 */    77,   96,   28,   99,   80,   25,  101,  107,   85,   86,
 /*   520 */    74,   75,   89,   77,   91,   30,   26,   94,   82,   96,
 /*   530 */    76,   85,   86,  107,   80,   89,  102,   91,   79,   76,
 /*   540 */    15,   87,   96,   19,  103,   49,   25,  101,   23,   78,
 /*   550 */    87,   26,   31,   32,   33,   34,   35,   36,   37,   38,
 /*   560 */    48,   79,   74,   75,   11,   77,   48,   78,   15,   98,
 /*   570 */    82,    8,   19,   85,   86,   30,   30,   89,   11,   91,
 /*   580 */    98,   30,   61,   30,   96,   16,    4,   98,   30,  101,
 /*   590 */     4,   30,   30,   20,    4,   42,   43,   44,   45,   16,
 /*   600 */    87,   90,   49,   74,   75,   97,   77,   98,  107,   22,
 /*   610 */   100,   79,   79,   93,   85,   86,   15,  108,   89,   27,
 /*   620 */    91,   74,   75,  108,   77,   96,  108,  108,  108,  108,
 /*   630 */   108,  108,   85,   86,  105,  108,   89,  108,   91,   74,
 /*   640 */    75,   94,   77,   96,  108,  108,  108,  108,  108,  108,
 /*   650 */    85,   86,   74,   75,   89,   77,   91,  108,  108,  108,
 /*   660 */   108,   96,  108,   85,   86,  108,  108,   89,  108,   91,
 /*   670 */    74,   75,   74,   77,   96,   77,  108,  108,  108,  108,
 /*   680 */   108,   85,   86,   85,   86,   89,  108,   91,  108,   91,
 /*   690 */    74,   75,   96,   77,   96,  108,  108,  108,  108,  108,
 /*   700 */   108,   85,   86,  108,  108,   89,  108,   91,   74,   75,
 /*   710 */   108,   77,   96,  108,  108,  108,  108,  108,  108,   85,
 /*   720 */    86,  108,  108,   89,  108,   91,   74,   75,  108,   77,
 /*   730 */    96,  108,  108,  108,  108,  108,  108,   85,   86,   74,
 /*   740 */    75,   89,   77,   91,  108,  108,  108,  108,   96,  108,
 /*   750 */    85,   86,  108,  108,   89,  108,   91,   74,   75,   74,
 /*   760 */    77,   96,   77,  108,  108,  108,  108,  108,   85,   86,
 /*   770 */    85,   86,   89,  108,   91,  108,   91,   74,   75,   96,
 /*   780 */    77,   96,  108,  108,  108,  108,  108,  108,   85,   86,
 /*   790 */   108,  108,   89,  108,   91,   74,   75,  108,   77,   96,
 /*   800 */   108,  108,  108,  108,  108,  108,   85,   86,  108,  108,
 /*   810 */    89,  108,   91,   74,   75,  108,   77,   96,  108,  108,
 /*   820 */   108,  108,  108,  108,   85,   86,   74,   75,   89,   77,
 /*   830 */    91,  108,  108,  108,  108,   96,  108,   85,   86,  108,
 /*   840 */   108,   89,  108,   91,   74,   75,   74,   77,   96,   77,
 /*   850 */   108,  108,  108,  108,  108,   85,   86,   85,  108,   89,
 /*   860 */   108,   91,  108,   91,   74,  108,   96,   77,   96,  108,
 /*   870 */   108,  108,  108,  108,  108,   85,   86,  108,  108,   89,
 /*   880 */   108,   91,   74,  108,  108,   77,   96,  108,  108,  108,
 /*   890 */   108,  108,  108,   85,   86,  108,  108,   89,  108,   91,
 /*   900 */    74,  108,   74,   77,   96,   77,  108,  108,  108,  108,
 /*   910 */   108,   85,   86,   85,  108,   89,  108,   91,   74,   91,
 /*   920 */   108,   77,   96,  108,   96,  108,  108,  108,  108,   85,
 /*   930 */   108,  108,  108,  108,  108,   91,  108,  108,  108,  108,
 /*   940 */    96,
);
    const YY_SHIFT_USE_DFLT = -40;
    const YY_SHIFT_MAX = 159;
    static public $yy_shift_ofst = array(
 /*     0 */   179,  177,   15,   15,   15,   15,   15,   15,   15,  118,
 /*    10 */    15,   15,  311,  236,  311,  236,  236,  236,  236,  236,
 /*    20 */   236,  236,  236,  236,  193,  236,  236,  236,  236,  236,
 /*    30 */   236,  134,  252,  295,   97,   97,  553,  553,  553,  208,
 /*    40 */   153,    3,  143,  125,  197,  199,  197,  412,  412,  307,
 /*    50 */    94,   37,  179,  521,    1,  297,  369,  462,   91,   91,
 /*    60 */   108,  108,   91,  293,  108,  108,  484,  484,  592,   25,
 /*    70 */   -16,   66,  -39,  -39,  -39,  -39,  -39,  -39,  -39,  -39,
 /*    80 */   185,  187,   20,   92,  290,  212,  227,  227,    2,   59,
 /*    90 */   211,  477,  355,  211,  423,  315,  243,  447,  451,  417,
 /*   100 */   490,  146,  211,  282,  211,  242,  140,   46,  484,  587,
 /*   110 */   484,   30,  601,   30,  524,  484,   30,   30,  484,   30,
 /*   120 */    30,  -40,  -40,  -40,  -40,  -40,  231,  525,   86,   33,
 /*   130 */   346,   33,  382,  234,   33,  545,  518,  558,  563,  512,
 /*   140 */   495,  500,  496,  546,  567,  573,  590,  586,  551,  582,
 /*   150 */   583,  362,  174,  171,  456,  189,  285,  569,  561,  562,
);
    const YY_REDUCE_USE_DFLT = -63;
    const YY_REDUCE_MAX = 125;
    static public $yy_reduce_ofst = array(
 /*     0 */   -62,   14,  340,  371,  384,  327,  415,  309,  296,  402,
 /*    10 */   446,  488,  213,  433,  529,  358,  547,  272,  721,  703,
 /*    20 */   596,  739,  565,  652,  665,  634,  616,  578,  683,  770,
 /*    30 */   752,  808,  826,  790,  598,  685,  844,  772,  828,  239,
 /*    40 */   156,  196,  256,  300,  253,  334,   35,  289,  454,  275,
 /*    50 */   209,  434,  205,  434,  482,  398,  482,  426,  410,  361,
 /*    60 */   471,  342,  151,  303,  -11,  489,  294,  414,  463,  441,
 /*    70 */   441,  441,  441,  441,  441,  441,  441,  441,  441,  441,
 /*    80 */   511,  511,  509,  501,  501,  501,  511,  511,  501,  513,
 /*    90 */   508,  107,  107,  508,  513,  513,  107,  513,  513,  107,
 /*   100 */   107,  513,  508,  107,  508,  520,  107,  532,  365,  510,
 /*   110 */   365,  107,  533,  107,  407,  365,  107,  107,  365,  107,
 /*   120 */   107,  359,  345,  326,  405,  459,
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
        /* 39 */ array(1, 3, 19, 42, 48, 49, ),
        /* 40 */ array(1, 3, 19, 42, 48, 49, ),
        /* 41 */ array(3, 19, 30, 49, ),
        /* 42 */ array(15, 23, 25, 27, 28, ),
        /* 43 */ array(3, 11, 19, 30, 49, ),
        /* 44 */ array(17, 20, 28, ),
        /* 45 */ array(19, 30, 49, ),
        /* 46 */ array(17, 20, 28, ),
        /* 47 */ array(25, 27, ),
        /* 48 */ array(25, 27, ),
        /* 49 */ array(22, 27, ),
        /* 50 */ array(19, 49, ),
        /* 51 */ array(16, 25, 31, 32, 33, 34, 35, 36, 37, 38, 61, ),
        /* 52 */ array(1, 2, 3, 5, 6, 7, 10, 62, 64, 65, 66, ),
        /* 53 */ array(25, 31, 32, 33, 34, 35, 36, 37, 38, 61, ),
        /* 54 */ array(3, 26, 30, 46, 50, ),
        /* 55 */ array(3, 14, 26, 30, ),
        /* 56 */ array(3, 30, 50, ),
        /* 57 */ array(1, 3, 43, ),
        /* 58 */ array(1, 3, ),
        /* 59 */ array(1, 3, ),
        /* 60 */ array(3, 30, ),
        /* 61 */ array(3, 30, ),
        /* 62 */ array(1, 3, ),
        /* 63 */ array(27, 28, ),
        /* 64 */ array(3, 30, ),
        /* 65 */ array(3, 30, ),
        /* 66 */ array(28, ),
        /* 67 */ array(28, ),
        /* 68 */ array(27, ),
        /* 69 */ array(4, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 70 */ array(16, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 71 */ array(24, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 72 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 73 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 74 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 75 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 76 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 77 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 78 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 79 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 80 */ array(12, 13, 18, 47, ),
        /* 81 */ array(4, 12, 13, 47, ),
        /* 82 */ array(3, 26, 30, ),
        /* 83 */ array(1, 3, 67, ),
        /* 84 */ array(1, 3, 43, ),
        /* 85 */ array(1, 3, 63, ),
        /* 86 */ array(12, 13, 47, ),
        /* 87 */ array(12, 13, 47, ),
        /* 88 */ array(1, 3, 9, ),
        /* 89 */ array(4, 27, ),
        /* 90 */ array(17, 20, ),
        /* 91 */ array(4, 25, ),
        /* 92 */ array(4, 25, ),
        /* 93 */ array(17, 20, ),
        /* 94 */ array(4, 27, ),
        /* 95 */ array(4, 27, ),
        /* 96 */ array(16, 25, ),
        /* 97 */ array(4, 27, ),
        /* 98 */ array(4, 27, ),
        /* 99 */ array(4, 25, ),
        /* 100 */ array(4, 25, ),
        /* 101 */ array(4, 27, ),
        /* 102 */ array(17, 20, ),
        /* 103 */ array(21, 25, ),
        /* 104 */ array(17, 20, ),
        /* 105 */ array(19, 30, ),
        /* 106 */ array(25, 29, ),
        /* 107 */ array(3, 15, ),
        /* 108 */ array(28, ),
        /* 109 */ array(22, ),
        /* 110 */ array(28, ),
        /* 111 */ array(25, ),
        /* 112 */ array(15, ),
        /* 113 */ array(25, ),
        /* 114 */ array(19, ),
        /* 115 */ array(28, ),
        /* 116 */ array(25, ),
        /* 117 */ array(25, ),
        /* 118 */ array(28, ),
        /* 119 */ array(25, ),
        /* 120 */ array(25, ),
        /* 121 */ array(),
        /* 122 */ array(),
        /* 123 */ array(),
        /* 124 */ array(),
        /* 125 */ array(),
        /* 126 */ array(15, 23, 29, ),
        /* 127 */ array(15, 23, 26, ),
        /* 128 */ array(15, 18, 23, ),
        /* 129 */ array(15, 23, ),
        /* 130 */ array(18, 21, ),
        /* 131 */ array(15, 23, ),
        /* 132 */ array(21, 24, ),
        /* 133 */ array(30, 50, ),
        /* 134 */ array(15, 23, ),
        /* 135 */ array(30, ),
        /* 136 */ array(48, ),
        /* 137 */ array(30, ),
        /* 138 */ array(8, ),
        /* 139 */ array(48, ),
        /* 140 */ array(30, ),
        /* 141 */ array(26, ),
        /* 142 */ array(49, ),
        /* 143 */ array(30, ),
        /* 144 */ array(11, ),
        /* 145 */ array(20, ),
        /* 146 */ array(4, ),
        /* 147 */ array(4, ),
        /* 148 */ array(30, ),
        /* 149 */ array(4, ),
        /* 150 */ array(16, ),
        /* 151 */ array(26, ),
        /* 152 */ array(19, ),
        /* 153 */ array(4, ),
        /* 154 */ array(15, ),
        /* 155 */ array(16, ),
        /* 156 */ array(30, ),
        /* 157 */ array(16, ),
        /* 158 */ array(30, ),
        /* 159 */ array(30, ),
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
        /* 259 */ array(),
        /* 260 */ array(),
        /* 261 */ array(),
);
    static public $yy_default = array(
 /*     0 */   406,  406,  406,  406,  406,  406,  406,  406,  406,  406,
 /*    10 */   406,  406,  391,  353,  406,  353,  353,  353,  406,  406,
 /*    20 */   406,  406,  406,  406,  406,  406,  406,  406,  406,  406,
 /*    30 */   406,  406,  406,  406,  406,  406,  406,  406,  406,  406,
 /*    40 */   406,  406,  291,  406,  277,  406,  322,  291,  291,  291,
 /*    50 */   406,  363,  262,  363,  329,  406,  329,  406,  406,  406,
 /*    60 */   406,  406,  406,  291,  406,  406,  318,  317,  291,  406,
 /*    70 */   406,  406,  369,  373,  376,  368,  377,  367,  372,  361,
 /*    80 */   406,  406,  406,  406,  406,  406,  358,  297,  406,  406,
 /*    90 */   347,  406,  406,  345,  406,  406,  406,  406,  406,  406,
 /*   100 */   406,  406,  346,  352,  344,  406,  392,  329,  323,  298,
 /*   110 */   320,  285,  329,  394,  406,  341,  393,  295,  319,  292,
 /*   120 */   364,  329,  329,  357,  357,  329,  296,  296,  406,  406,
 /*   130 */   406,  296,  406,  406,  359,  406,  406,  406,  406,  406,
 /*   140 */   406,  406,  406,  406,  406,  307,  406,  406,  406,  406,
 /*   150 */   406,  324,  406,  406,  321,  406,  406,  406,  406,  406,
 /*   160 */   267,  299,  300,  282,  332,  402,  380,  268,  331,  404,
 /*   170 */   360,  301,  384,  383,  382,  381,  265,  263,  306,  385,
 /*   180 */   286,  330,  327,  293,  294,  272,  362,  264,  270,  386,
 /*   190 */   271,  403,  405,  328,  375,  269,  371,  281,  370,  284,
 /*   200 */   266,  374,  324,  326,  387,  343,  333,  325,  312,  356,
 /*   210 */   305,  304,  338,  339,  274,  303,  273,  337,  340,  342,
 /*   220 */   348,  316,  396,  401,  400,  349,  335,  334,  354,  350,
 /*   230 */   355,  351,  336,  399,  398,  288,  311,  287,  390,  280,
 /*   240 */   310,  283,  366,  378,  365,  308,  309,  388,  389,  397,
 /*   250 */   395,  290,  289,  275,  276,  315,  302,  279,  313,  278,
 /*   260 */   314,  379,
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
    const YYNSTATE = 262;
    const YYNRULE = 144;
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
 /* 103 */ "ifexpr ::= expr ISIN array",
 /* 104 */ "ifexpr ::= expr ISIN value",
 /* 105 */ "ifexpr ::= ifexprs lop ifexprs",
 /* 106 */ "ifexpr ::= ifexprs ISDIVBY ifexprs",
 /* 107 */ "ifexpr ::= ifexprs ISNOTDIVBY ifexprs",
 /* 108 */ "ifexpr ::= ifexprs ISEVEN",
 /* 109 */ "ifexpr ::= ifexprs ISNOTEVEN",
 /* 110 */ "ifexpr ::= ifexprs ISEVENBY ifexprs",
 /* 111 */ "ifexpr ::= ifexprs ISNOTEVENBY ifexprs",
 /* 112 */ "ifexpr ::= ifexprs ISODD",
 /* 113 */ "ifexpr ::= ifexprs ISNOTODD",
 /* 114 */ "ifexpr ::= ifexprs ISODDBY ifexprs",
 /* 115 */ "ifexpr ::= ifexprs ISNOTODDBY ifexprs",
 /* 116 */ "ifcond ::= EQUALS",
 /* 117 */ "ifcond ::= NOTEQUALS",
 /* 118 */ "ifcond ::= GREATERTHAN",
 /* 119 */ "ifcond ::= LESSTHAN",
 /* 120 */ "ifcond ::= GREATEREQUAL",
 /* 121 */ "ifcond ::= LESSEQUAL",
 /* 122 */ "ifcond ::= IDENTITY",
 /* 123 */ "ifcond ::= NONEIDENTITY",
 /* 124 */ "lop ::= LAND",
 /* 125 */ "lop ::= LOR",
 /* 126 */ "array ::= OPENB arrayelements CLOSEB",
 /* 127 */ "arrayelements ::= arrayelement",
 /* 128 */ "arrayelements ::= arrayelements COMMA arrayelement",
 /* 129 */ "arrayelements ::=",
 /* 130 */ "arrayelement ::= expr",
 /* 131 */ "arrayelement ::= expr APTR expr",
 /* 132 */ "arrayelement ::= ID APTR expr",
 /* 133 */ "doublequoted ::= doublequoted doublequotedcontent",
 /* 134 */ "doublequoted ::= doublequotedcontent",
 /* 135 */ "doublequotedcontent ::= BACKTICK ID BACKTICK",
 /* 136 */ "doublequotedcontent ::= BACKTICK variable BACKTICK",
 /* 137 */ "doublequotedcontent ::= variable",
 /* 138 */ "doublequotedcontent ::= LDEL expr RDEL",
 /* 139 */ "doublequotedcontent ::= OTHER",
 /* 140 */ "text ::= text textelement",
 /* 141 */ "text ::= textelement",
 /* 142 */ "textelement ::= OTHER",
 /* 143 */ "textelement ::= LDEL",
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
  array( 'lhs' => 106, 'rhs' => 1 ),
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
        127 => 0,
        1 => 1,
        35 => 1,
        37 => 1,
        42 => 1,
        43 => 1,
        75 => 1,
        98 => 1,
        134 => 1,
        141 => 1,
        142 => 1,
        143 => 1,
        2 => 2,
        66 => 2,
        133 => 2,
        140 => 2,
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
        130 => 24,
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
        105 => 102,
        103 => 103,
        104 => 104,
        106 => 106,
        107 => 107,
        108 => 108,
        113 => 108,
        109 => 109,
        112 => 109,
        110 => 110,
        115 => 110,
        111 => 111,
        114 => 111,
        116 => 116,
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
        128 => 128,
        129 => 129,
        131 => 131,
        132 => 132,
        135 => 135,
        136 => 136,
        137 => 137,
        138 => 138,
        139 => 139,
    );
    /* Beginning here are the reduction cases.  A typical example
    ** follows:
    **  #line <lineno> <grammarfile>
    **   function yy_r0($yymsp){ ... }           // User supplied code
    **  #line <lineno> <thisfile>
    */
#line 73 "internal.templateparser.y"
    function yy_r0(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;     }
#line 1818 "internal.templateparser.php"
#line 79 "internal.templateparser.y"
    function yy_r1(){$this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1821 "internal.templateparser.php"
#line 81 "internal.templateparser.y"
    function yy_r2(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1824 "internal.templateparser.php"
#line 87 "internal.templateparser.y"
    function yy_r3(){if ($this->compiler->has_code) {
                                            $tmp =''; foreach ($this->prefix_code as $code) {$tmp.=$code;} $this->prefix_code=array();
                                            $this->_retvalue = $this->cacher->processNocacheCode($tmp.$this->yystack[$this->yyidx + 0]->minor, $this->compiler,$this->nocache,true);
                                         } $this->nocache=false;    }
#line 1830 "internal.templateparser.php"
#line 100 "internal.templateparser.y"
    function yy_r4(){ $this->_retvalue = '';    }
#line 1833 "internal.templateparser.php"
#line 103 "internal.templateparser.y"
    function yy_r5(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + -1]->minor, $this->compiler,false,false);    }
#line 1836 "internal.templateparser.php"
#line 105 "internal.templateparser.y"
    function yy_r6(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->left_delimiter, $this->compiler,false,false);    }
#line 1839 "internal.templateparser.php"
#line 107 "internal.templateparser.y"
    function yy_r7(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->right_delimiter, $this->compiler,false,false);    }
#line 1842 "internal.templateparser.php"
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
#line 1853 "internal.templateparser.php"
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
#line 1864 "internal.templateparser.php"
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
#line 1875 "internal.templateparser.php"
#line 139 "internal.templateparser.y"
    function yy_r11(){$this->_retvalue = $this->cacher->processNocacheCode("<?php echo '".$this->yystack[$this->yyidx + 0]->minor."';?>\n", $this->compiler, true, true);    }
#line 1878 "internal.templateparser.php"
#line 141 "internal.templateparser.y"
    function yy_r12(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + 0]->minor, $this->compiler,false,false);    }
#line 1881 "internal.templateparser.php"
#line 149 "internal.templateparser.y"
    function yy_r13(){ $this->_retvalue = $this->compiler->compileTag('print_expression',array_merge(array('value'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1884 "internal.templateparser.php"
#line 151 "internal.templateparser.y"
    function yy_r14(){ $this->_retvalue = $this->compiler->compileTag('assign',array_merge(array('value'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -4]->minor,$this->yystack[$this->yyidx + -1]->minor));    }
#line 1887 "internal.templateparser.php"
#line 152 "internal.templateparser.y"
    function yy_r15(){$this->_retvalue = array('var'=>$this->yystack[$this->yyidx + -1]->minor, 'index'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1890 "internal.templateparser.php"
#line 155 "internal.templateparser.y"
    function yy_r16(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor,$this->yystack[$this->yyidx + -1]->minor);    }
#line 1893 "internal.templateparser.php"
#line 157 "internal.templateparser.y"
    function yy_r17(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -4]->minor,array_merge(array('object_methode'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1896 "internal.templateparser.php"
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
#line 1911 "internal.templateparser.php"
#line 173 "internal.templateparser.y"
    function yy_r19(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor.'close',$this->yystack[$this->yyidx + -1]->minor);    }
#line 1914 "internal.templateparser.php"
#line 175 "internal.templateparser.y"
    function yy_r20(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor.'close',array('object_methode'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1917 "internal.templateparser.php"
#line 177 "internal.templateparser.y"
    function yy_r21(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor,array('if condition'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1920 "internal.templateparser.php"
#line 179 "internal.templateparser.y"
    function yy_r22(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -9]->minor,array('start'=>$this->yystack[$this->yyidx + -7]->minor,'ifexp'=>$this->yystack[$this->yyidx + -5]->minor,'varloop'=>$this->yystack[$this->yyidx + -2]->minor,'loop'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1923 "internal.templateparser.php"
#line 180 "internal.templateparser.y"
    function yy_r23(){ $this->_retvalue = '='.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1926 "internal.templateparser.php"
#line 181 "internal.templateparser.y"
    function yy_r24(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1929 "internal.templateparser.php"
#line 184 "internal.templateparser.y"
    function yy_r25(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -6]->minor,array('from'=>$this->yystack[$this->yyidx + -1]->minor,'item'=>$this->yystack[$this->yyidx + -3]->minor));    }
#line 1932 "internal.templateparser.php"
#line 191 "internal.templateparser.y"
    function yy_r27(){ $this->_retvalue = array_merge($this->yystack[$this->yyidx + -1]->minor,$this->yystack[$this->yyidx + 0]->minor);    }
#line 1935 "internal.templateparser.php"
#line 195 "internal.templateparser.y"
    function yy_r29(){ $this->_retvalue = array();    }
#line 1938 "internal.templateparser.php"
#line 199 "internal.templateparser.y"
    function yy_r30(){ $this->_retvalue = array($this->yystack[$this->yyidx + -2]->minor=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1941 "internal.templateparser.php"
#line 204 "internal.templateparser.y"
    function yy_r31(){ $this->_retvalue = array($this->yystack[$this->yyidx + 0]->minor);    }
#line 1944 "internal.templateparser.php"
#line 205 "internal.templateparser.y"
    function yy_r32(){ $this->yystack[$this->yyidx + -2]->minor[]=$this->yystack[$this->yyidx + 0]->minor; $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor;    }
#line 1947 "internal.templateparser.php"
#line 207 "internal.templateparser.y"
    function yy_r33(){ $this->_retvalue = array('var' => $this->yystack[$this->yyidx + -2]->minor, 'value'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1950 "internal.templateparser.php"
#line 214 "internal.templateparser.y"
    function yy_r34(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';     }
#line 1953 "internal.templateparser.php"
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
#line 1968 "internal.templateparser.php"
#line 236 "internal.templateparser.y"
    function yy_r39(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1971 "internal.templateparser.php"
#line 238 "internal.templateparser.y"
    function yy_r40(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor . $this->yystack[$this->yyidx + -1]->minor . $this->yystack[$this->yyidx + 0]->minor;     }
#line 1974 "internal.templateparser.php"
#line 240 "internal.templateparser.y"
    function yy_r41(){ $this->_retvalue = '('. $this->yystack[$this->yyidx + -2]->minor . ').(' . $this->yystack[$this->yyidx + 0]->minor. ')';     }
#line 1977 "internal.templateparser.php"
#line 257 "internal.templateparser.y"
    function yy_r46(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1980 "internal.templateparser.php"
#line 266 "internal.templateparser.y"
    function yy_r50(){ $this->_retvalue = "(". $this->yystack[$this->yyidx + -1]->minor .")";     }
#line 1983 "internal.templateparser.php"
#line 269 "internal.templateparser.y"
    function yy_r51(){ $this->_retvalue = "'".$this->yystack[$this->yyidx + -1]->minor."'";     }
#line 1986 "internal.templateparser.php"
#line 270 "internal.templateparser.y"
    function yy_r52(){ $this->_retvalue = "''";     }
#line 1989 "internal.templateparser.php"
#line 272 "internal.templateparser.y"
    function yy_r53(){ $this->_retvalue = "'".str_replace('\"','"',$this->yystack[$this->yyidx + -1]->minor)."'";     }
#line 1992 "internal.templateparser.php"
#line 278 "internal.templateparser.y"
    function yy_r55(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1995 "internal.templateparser.php"
#line 279 "internal.templateparser.y"
    function yy_r56(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -3]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -6]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -1]->minor .')';     }
#line 1998 "internal.templateparser.php"
#line 281 "internal.templateparser.y"
    function yy_r57(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor.'::'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2001 "internal.templateparser.php"
#line 282 "internal.templateparser.y"
    function yy_r58(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -4]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -7]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -2]->minor .')'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2004 "internal.templateparser.php"
#line 284 "internal.templateparser.y"
    function yy_r59(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2007 "internal.templateparser.php"
#line 286 "internal.templateparser.y"
    function yy_r60(){ $this->_retvalue = $this->yystack[$this->yyidx + -4]->minor.'::$'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2010 "internal.templateparser.php"
#line 288 "internal.templateparser.y"
    function yy_r61(){ $this->_retvalue = $this->yystack[$this->yyidx + -5]->minor.'::$'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2013 "internal.templateparser.php"
#line 297 "internal.templateparser.y"
    function yy_r62(){ if ($this->yystack[$this->yyidx + 0]->minor['var'] == '\'smarty\'') { $this->_retvalue =  $this->compiler->compileTag(trim($this->yystack[$this->yyidx + 0]->minor['var'],"'"),$this->yystack[$this->yyidx + 0]->minor['index']);} else {
                                                         $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + 0]->minor['var'] .')->value'.$this->yystack[$this->yyidx + 0]->minor['index']; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + 0]->minor['var'],"'"))->nocache;}    }
#line 2017 "internal.templateparser.php"
#line 300 "internal.templateparser.y"
    function yy_r63(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->'.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 2020 "internal.templateparser.php"
#line 304 "internal.templateparser.y"
    function yy_r65(){$this->_retvalue = '$_smarty_tpl->getConfigVariable(\''. $this->yystack[$this->yyidx + -1]->minor .'\')';    }
#line 2023 "internal.templateparser.php"
#line 312 "internal.templateparser.y"
    function yy_r67(){return;    }
#line 2026 "internal.templateparser.php"
#line 316 "internal.templateparser.y"
    function yy_r68(){ $this->_retvalue = "['". $this->yystack[$this->yyidx + 0]->minor ."']";    }
#line 2029 "internal.templateparser.php"
#line 317 "internal.templateparser.y"
    function yy_r69(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + 0]->minor ."]";    }
#line 2032 "internal.templateparser.php"
#line 319 "internal.templateparser.y"
    function yy_r70(){ $this->_retvalue = "[".$this->yystack[$this->yyidx + 0]->minor."]";    }
#line 2035 "internal.templateparser.php"
#line 320 "internal.templateparser.y"
    function yy_r71(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + -1]->minor ."]";    }
#line 2038 "internal.templateparser.php"
#line 322 "internal.templateparser.y"
    function yy_r72(){ $this->_retvalue = '['.$this->compiler->compileTag('smarty','[\'section\'][\''.$this->yystack[$this->yyidx + -1]->minor.'\'][\'index\']').']';    }
#line 2041 "internal.templateparser.php"
#line 326 "internal.templateparser.y"
    function yy_r74(){$this->_retvalue = '';    }
#line 2044 "internal.templateparser.php"
#line 334 "internal.templateparser.y"
    function yy_r76(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2047 "internal.templateparser.php"
#line 336 "internal.templateparser.y"
    function yy_r77(){$this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2050 "internal.templateparser.php"
#line 338 "internal.templateparser.y"
    function yy_r78(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2053 "internal.templateparser.php"
#line 343 "internal.templateparser.y"
    function yy_r79(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->value'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 2056 "internal.templateparser.php"
#line 345 "internal.templateparser.y"
    function yy_r80(){$this->_retvalue  = $this->yystack[$this->yyidx + 0]->minor;     }
#line 2059 "internal.templateparser.php"
#line 347 "internal.templateparser.y"
    function yy_r81(){$this->_retvalue  = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2062 "internal.templateparser.php"
#line 349 "internal.templateparser.y"
    function yy_r82(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2065 "internal.templateparser.php"
#line 350 "internal.templateparser.y"
    function yy_r83(){ $this->_retvalue = '->{'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2068 "internal.templateparser.php"
#line 351 "internal.templateparser.y"
    function yy_r84(){ $this->_retvalue = '->{'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2071 "internal.templateparser.php"
#line 352 "internal.templateparser.y"
    function yy_r85(){ $this->_retvalue = '->{\''.$this->yystack[$this->yyidx + -4]->minor.'\'.'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2074 "internal.templateparser.php"
#line 354 "internal.templateparser.y"
    function yy_r86(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2077 "internal.templateparser.php"
#line 360 "internal.templateparser.y"
    function yy_r87(){if (!$this->template->security || $this->smarty->security_handler->isTrustedPhpFunction($this->yystack[$this->yyidx + -3]->minor, $this->compiler)) {
																					            if ($this->yystack[$this->yyidx + -3]->minor == 'isset' || $this->yystack[$this->yyidx + -3]->minor == 'empty' || $this->yystack[$this->yyidx + -3]->minor == 'array' || is_callable($this->yystack[$this->yyidx + -3]->minor)) {
																					                $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";
																					            } else {
                                                       $this->compiler->trigger_template_error ("unknown function \"" . $this->yystack[$this->yyidx + -3]->minor . "\"");
                                                      }
                                                    }    }
#line 2086 "internal.templateparser.php"
#line 371 "internal.templateparser.y"
    function yy_r88(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";    }
#line 2089 "internal.templateparser.php"
#line 375 "internal.templateparser.y"
    function yy_r89(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.",".$this->yystack[$this->yyidx + 0]->minor;    }
#line 2092 "internal.templateparser.php"
#line 379 "internal.templateparser.y"
    function yy_r91(){ return;    }
#line 2095 "internal.templateparser.php"
#line 384 "internal.templateparser.y"
    function yy_r92(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,true);    }
#line 2098 "internal.templateparser.php"
#line 385 "internal.templateparser.y"
    function yy_r93(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,false);    }
#line 2101 "internal.templateparser.php"
#line 392 "internal.templateparser.y"
    function yy_r94(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2104 "internal.templateparser.php"
#line 396 "internal.templateparser.y"
    function yy_r96(){$this->_retvalue = ','.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2107 "internal.templateparser.php"
#line 397 "internal.templateparser.y"
    function yy_r97(){$this->_retvalue = ',\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2110 "internal.templateparser.php"
#line 404 "internal.templateparser.y"
    function yy_r99(){$this->_retvalue = '!'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2113 "internal.templateparser.php"
#line 409 "internal.templateparser.y"
    function yy_r101(){$this->_retvalue =$this->yystack[$this->yyidx + 0]->minor;    }
#line 2116 "internal.templateparser.php"
#line 410 "internal.templateparser.y"
    function yy_r102(){$this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2119 "internal.templateparser.php"
#line 411 "internal.templateparser.y"
    function yy_r103(){$this->_retvalue = 'in_array('.$this->yystack[$this->yyidx + -2]->minor.','.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2122 "internal.templateparser.php"
#line 412 "internal.templateparser.y"
    function yy_r104(){$this->_retvalue = 'in_array('.$this->yystack[$this->yyidx + -2]->minor.',(array)'.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2125 "internal.templateparser.php"
#line 414 "internal.templateparser.y"
    function yy_r106(){$this->_retvalue = '!('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2128 "internal.templateparser.php"
#line 415 "internal.templateparser.y"
    function yy_r107(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2131 "internal.templateparser.php"
#line 416 "internal.templateparser.y"
    function yy_r108(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2134 "internal.templateparser.php"
#line 417 "internal.templateparser.y"
    function yy_r109(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2137 "internal.templateparser.php"
#line 418 "internal.templateparser.y"
    function yy_r110(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2140 "internal.templateparser.php"
#line 419 "internal.templateparser.y"
    function yy_r111(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2143 "internal.templateparser.php"
#line 425 "internal.templateparser.y"
    function yy_r116(){$this->_retvalue = '==';    }
#line 2146 "internal.templateparser.php"
#line 426 "internal.templateparser.y"
    function yy_r117(){$this->_retvalue = '!=';    }
#line 2149 "internal.templateparser.php"
#line 427 "internal.templateparser.y"
    function yy_r118(){$this->_retvalue = '>';    }
#line 2152 "internal.templateparser.php"
#line 428 "internal.templateparser.y"
    function yy_r119(){$this->_retvalue = '<';    }
#line 2155 "internal.templateparser.php"
#line 429 "internal.templateparser.y"
    function yy_r120(){$this->_retvalue = '>=';    }
#line 2158 "internal.templateparser.php"
#line 430 "internal.templateparser.y"
    function yy_r121(){$this->_retvalue = '<=';    }
#line 2161 "internal.templateparser.php"
#line 431 "internal.templateparser.y"
    function yy_r122(){$this->_retvalue = '===';    }
#line 2164 "internal.templateparser.php"
#line 432 "internal.templateparser.y"
    function yy_r123(){$this->_retvalue = '!==';    }
#line 2167 "internal.templateparser.php"
#line 434 "internal.templateparser.y"
    function yy_r124(){$this->_retvalue = '&&';    }
#line 2170 "internal.templateparser.php"
#line 435 "internal.templateparser.y"
    function yy_r125(){$this->_retvalue = '||';    }
#line 2173 "internal.templateparser.php"
#line 440 "internal.templateparser.y"
    function yy_r126(){ $this->_retvalue = 'array('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2176 "internal.templateparser.php"
#line 442 "internal.templateparser.y"
    function yy_r128(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.','.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2179 "internal.templateparser.php"
#line 443 "internal.templateparser.y"
    function yy_r129(){ return;     }
#line 2182 "internal.templateparser.php"
#line 445 "internal.templateparser.y"
    function yy_r131(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2185 "internal.templateparser.php"
#line 446 "internal.templateparser.y"
    function yy_r132(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + -2]->minor.'\'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2188 "internal.templateparser.php"
#line 453 "internal.templateparser.y"
    function yy_r135(){$this->_retvalue = "`".$this->yystack[$this->yyidx + -1]->minor."`";    }
#line 2191 "internal.templateparser.php"
#line 454 "internal.templateparser.y"
    function yy_r136(){$this->_retvalue = "'.".$this->yystack[$this->yyidx + -1]->minor.".'";    }
#line 2194 "internal.templateparser.php"
#line 455 "internal.templateparser.y"
    function yy_r137(){$this->_retvalue = "'.".$this->yystack[$this->yyidx + 0]->minor.".'";    }
#line 2197 "internal.templateparser.php"
#line 456 "internal.templateparser.y"
    function yy_r138(){$this->_retvalue = "'.(".$this->yystack[$this->yyidx + -1]->minor.").'";    }
#line 2200 "internal.templateparser.php"
#line 457 "internal.templateparser.y"
    function yy_r139(){$this->_retvalue = addcslashes($this->yystack[$this->yyidx + 0]->minor,"'");    }
#line 2203 "internal.templateparser.php"

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
#line 2321 "internal.templateparser.php"
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
#line 2346 "internal.templateparser.php"
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
