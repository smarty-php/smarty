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
    const YY_NO_ACTION = 397;
    const YY_ACCEPT_ACTION = 396;
    const YY_ERROR_ACTION = 395;

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
    const YY_SZ_ACTTAB = 912;
static public $yy_action = array(
 /*     0 */   131,  237,  245,  140,   25,   13,  226,  195,   46,  123,
 /*    10 */   123,   60,  123,  173,   21,  174,  191,  196,  234,  235,
 /*    20 */   253,  192,  155,  167,  162,  154,  231,  188,  187,  157,
 /*    30 */   156,    4,   11,    2,    3,    6,    5,  191,  196,  119,
 /*    40 */    23,  112,  189,  185,  194,  202,  135,   20,  188,  187,
 /*    50 */   157,  156,    4,   11,    2,    3,    6,    5,  203,   16,
 /*    60 */    98,  197,   56,   27,  193,  169,   58,  209,  191,  196,
 /*    70 */   396,   48,  179,  182,  184,  180,   54,  221,  211,  188,
 /*    80 */   187,  157,  156,    4,   11,    2,    3,    6,    5,  191,
 /*    90 */   196,  130,  243,   34,   23,    7,  225,   12,  137,   59,
 /*   100 */   188,  187,  157,  156,    4,   11,    2,    3,    6,    5,
 /*   110 */   125,  173,   36,  174,  173,  137,  174,   30,   17,    9,
 /*   120 */    41,  209,   37,   53,  238,  233,  100,  200,   39,  149,
 /*   130 */   110,  130,   19,   34,  202,   22,  123,   12,  220,   59,
 /*   140 */   207,  134,  234,  235,  253,  192,  155,  167,  162,  154,
 /*   150 */   122,  221,  211,  248,   42,  164,  250,  223,  189,  181,
 /*   160 */   182,  123,   37,   53,  238,  233,  240,  164,   50,  149,
 /*   170 */   189,   30,  227,  210,   41,   67,  124,  197,  159,  163,
 /*   180 */   242,  177,   84,  189,  227,  173,   36,  174,  173,  197,
 /*   190 */   174,  130,  123,   34,  160,    7,  176,   12,   38,   62,
 /*   200 */    59,  130,  197,   34,  239,   22,   17,   12,  118,   59,
 /*   210 */   121,  102,  224,   17,  100,  141,  222,   29,  189,    9,
 /*   220 */   125,  100,   37,   53,  238,  233,  118,  137,  106,  149,
 /*   230 */   149,   10,   37,   53,  238,  233,  130,  197,   34,  149,
 /*   240 */    22,  146,   12,  158,   59,  130,  189,   34,   28,   22,
 /*   250 */   190,   12,  151,   59,  130,  127,   34,  218,   22,   26,
 /*   260 */    12,  175,   59,  204,   40,  197,   82,   37,   53,  238,
 /*   270 */   233,  221,  211,  120,  149,   59,   37,   53,  238,  233,
 /*   280 */   108,   45,  185,  149,  202,   37,   53,  238,  233,  130,
 /*   290 */    92,   34,  149,   22,   97,   12,  214,   59,  219,   17,
 /*   300 */   183,  246,   32,  212,   44,  149,   36,  100,  128,  123,
 /*   310 */   166,    1,  145,  164,  201,   59,  189,  123,   59,  213,
 /*   320 */    37,   53,  238,  233,  161,  164,   50,  149,  189,  165,
 /*   330 */   227,  199,   16,   73,   14,  197,  159,  163,  164,   50,
 /*   340 */    84,  189,  227,   17,   23,  149,   74,  197,  149,  159,
 /*   350 */   163,  100,  160,   84,  198,  227,  123,   39,  137,  200,
 /*   360 */   197,  164,   50,  164,  189,  160,  189,   27,  123,   72,
 /*   370 */    83,  209,  159,  163,  129,  142,   84,  137,  227,  164,
 /*   380 */   227,  242,  189,  197,  189,  197,  147,   33,  160,   93,
 /*   390 */   208,  134,  230,  116,  164,   50,  227,  189,   81,   23,
 /*   400 */   246,  197,   69,  197,  183,  159,  163,  164,   49,   84,
 /*   410 */   189,  227,  251,  247,  103,   68,  197,   85,  159,  163,
 /*   420 */    59,  160,   84,  218,  227,   26,  209,  164,   50,  197,
 /*   430 */   189,  136,  183,  123,  160,   76,  117,  225,  159,  163,
 /*   440 */   139,   59,   84,  189,  227,   23,  229,  130,   96,  197,
 /*   450 */   149,   22,  118,   12,  160,   59,  172,  123,  173,  246,
 /*   460 */   174,  244,  197,   86,  252,  228,  127,  249,   24,   79,
 /*   470 */    44,  149,  209,  150,  246,  137,  144,   90,   37,   53,
 /*   480 */   238,  233,   52,  164,   50,  149,  189,  210,  246,   51,
 /*   490 */   137,   66,  148,   18,  159,  163,  164,   50,   84,  189,
 /*   500 */   227,  205,  225,  183,   75,  197,    8,  159,  163,  225,
 /*   510 */   160,   84,   57,  227,  138,   29,  164,   50,  197,  189,
 /*   520 */    31,  133,  114,  160,   70,  137,  202,  159,  163,  164,
 /*   530 */    50,   84,  189,  227,   64,  178,  168,   71,  197,   63,
 /*   540 */   159,  163,  232,  160,   84,  206,  227,   61,  186,  107,
 /*   550 */   236,  197,  164,   91,  241,  189,  160,  171,  130,   35,
 /*   560 */   137,  210,   22,  159,  163,   15,   59,   84,   43,  227,
 /*   570 */    99,   31,  201,  170,  197,   55,  185,  127,  250,  250,
 /*   580 */    65,  250,  126,  216,  250,  250,  250,  250,  250,   37,
 /*   590 */    53,  238,  233,  250,  250,  250,  149,  164,  101,  250,
 /*   600 */   189,  250,  250,  250,  250,  250,  250,  250,  159,  163,
 /*   610 */   250,  250,   84,  250,  227,  164,   91,  215,  189,  197,
 /*   620 */   250,  250,  250,  250,  250,  250,  159,  163,  250,  250,
 /*   630 */    84,  250,  227,  250,  250,  250,  250,  197,  164,  101,
 /*   640 */   250,  189,  250,  250,  250,  250,  217,  250,  250,  159,
 /*   650 */   163,  164,  101,   84,  189,  227,  250,  250,  143,  250,
 /*   660 */   197,  250,  159,  163,  164,  101,   84,  189,  227,  250,
 /*   670 */   250,  153,  250,  197,  250,  159,  163,  250,  250,   84,
 /*   680 */   250,  227,  164,   87,  152,  189,  197,  250,  250,  250,
 /*   690 */   250,  250,  250,  159,  163,  250,  250,   84,  250,  227,
 /*   700 */   164,  111,  250,  189,  197,  250,  250,  250,  250,  250,
 /*   710 */   250,  159,  163,  164,  115,   84,  189,  227,  250,  250,
 /*   720 */   250,  250,  197,  250,  159,  163,  164,  109,   84,  189,
 /*   730 */   227,  250,  250,  250,  250,  197,  250,  159,  163,  164,
 /*   740 */   113,   84,  189,  227,  250,  250,  250,  250,  197,  250,
 /*   750 */   159,  163,  250,  250,   84,  250,  227,  164,   89,  250,
 /*   760 */   189,  197,  250,  250,  250,  250,  250,  250,  159,  163,
 /*   770 */   250,  250,   84,  250,  227,  164,  104,  250,  189,  197,
 /*   780 */   250,  250,  250,  250,  250,  250,  159,  163,  164,   47,
 /*   790 */    84,  132,  227,  250,  250,  250,  250,  197,  250,  159,
 /*   800 */   163,  164,   95,   84,  189,  227,  250,  250,  250,  250,
 /*   810 */   197,  250,  159,  163,  164,   94,   84,  189,  227,  250,
 /*   820 */   250,  250,  250,  197,  250,  159,  163,  250,  250,   84,
 /*   830 */   250,  227,  164,   88,  250,  189,  197,  250,  250,  250,
 /*   840 */   250,  250,  250,  159,  163,  250,  250,   84,  250,  227,
 /*   850 */   164,  105,  250,  189,  197,  250,  250,  250,  250,  250,
 /*   860 */   250,  159,  163,  164,  250,   84,  189,  227,  250,  250,
 /*   870 */   250,  250,  197,  250,  159,  163,  164,  250,   77,  189,
 /*   880 */   227,  250,  250,  250,  250,  197,  250,  159,  163,  164,
 /*   890 */   250,   80,  189,  227,  250,  250,  250,  250,  197,  250,
 /*   900 */   159,  163,  250,  250,   78,  250,  227,  250,  250,  250,
 /*   910 */   250,  197,
    );
    static public $yy_lookahead = array(
 /*     0 */    24,    4,    1,    2,    3,   21,    5,    6,    7,   25,
 /*    10 */    25,   10,   25,    1,   29,    3,   40,   41,   31,   32,
 /*    20 */    33,   34,   35,   36,   37,   38,   30,   51,   52,   53,
 /*    30 */    54,   55,   56,   57,   58,   59,   60,   40,   41,   73,
 /*    40 */     3,   94,   76,   96,   16,   98,   50,    3,   51,   52,
 /*    50 */    53,   54,   55,   56,   57,   58,   59,   60,   92,   15,
 /*    60 */    78,   95,   61,   26,   63,   64,   65,   30,   40,   41,
 /*    70 */    68,   69,   70,   71,   62,    4,   77,   12,   13,   51,
 /*    80 */    52,   53,   54,   55,   56,   57,   58,   59,   60,   40,
 /*    90 */    41,   11,    4,   13,    3,   15,   97,   17,   27,   19,
 /*   100 */    51,   52,   53,   54,   55,   56,   57,   58,   59,   60,
 /*   110 */    30,    1,   47,    3,    1,   27,    3,   17,   15,   39,
 /*   120 */    20,   30,   42,   43,   44,   45,   23,   16,   28,   49,
 /*   130 */    94,   11,   29,   13,   98,   15,   25,   17,   18,   19,
 /*   140 */     4,   50,   31,   32,   33,   34,   35,   36,   37,   38,
 /*   150 */    30,   12,   13,   43,   78,   73,   43,   18,   76,   70,
 /*   160 */    71,   25,   42,   43,   44,   45,   84,   73,   74,   49,
 /*   170 */    76,   17,   90,   97,   20,   81,   82,   95,   84,   85,
 /*   180 */    73,   87,   88,   76,   90,    1,   47,    3,    1,   95,
 /*   190 */     3,   11,   25,   13,  100,   15,    9,   17,   91,   19,
 /*   200 */    19,   11,   95,   13,    4,   15,   15,   17,   79,   19,
 /*   210 */    30,   30,  105,   15,   23,   73,   18,   26,   76,   39,
 /*   220 */    30,   23,   42,   43,   44,   45,   79,   27,   21,   49,
 /*   230 */    49,   24,   42,   43,   44,   45,   11,   95,   13,   49,
 /*   240 */    15,   19,   17,   73,   19,   11,   76,   13,  101,   15,
 /*   250 */    66,   17,   30,   19,   11,   30,   13,    1,   15,    3,
 /*   260 */    17,    4,   19,    4,   30,   95,   72,   42,   43,   44,
 /*   270 */    45,   12,   13,   30,   49,   19,   42,   43,   44,   45,
 /*   280 */    94,   80,   96,   49,   98,   42,   43,   44,   45,   11,
 /*   290 */    75,   13,   49,   15,   78,   17,    4,   19,   42,   15,
 /*   300 */   106,   86,    3,   16,   48,   49,   47,   23,   30,   25,
 /*   310 */    11,   27,   28,   73,   99,   19,   76,   25,   19,   16,
 /*   320 */    42,   43,   44,   45,   84,   73,   74,   49,   76,   30,
 /*   330 */    90,   18,   15,   81,   21,   95,   84,   85,   73,   74,
 /*   340 */    88,   76,   90,   15,    3,   49,   81,   95,   49,   84,
 /*   350 */    85,   23,  100,   88,    4,   90,   25,   28,   27,   16,
 /*   360 */    95,   73,   74,   73,   76,  100,   76,   26,   25,   81,
 /*   370 */    72,   30,   84,   85,   84,   85,   88,   27,   90,   73,
 /*   380 */    90,   73,   76,   95,   76,   95,   30,   46,  100,   75,
 /*   390 */    84,   50,   98,   79,   73,   74,   90,   76,   72,    3,
 /*   400 */    86,   95,   81,   95,  106,   84,   85,   73,   74,   88,
 /*   410 */    76,   90,    4,  105,   30,   81,   95,   77,   84,   85,
 /*   420 */    19,  100,   88,    1,   90,    3,   30,   73,   74,   95,
 /*   430 */    76,   30,  106,   25,  100,   81,    4,   97,   84,   85,
 /*   440 */    73,   19,   88,   76,   90,    3,   30,   11,   75,   95,
 /*   450 */    49,   15,   79,   17,  100,   19,   14,   25,    1,   86,
 /*   460 */     3,   48,   95,   75,   42,    4,   30,    4,   26,   72,
 /*   470 */    48,   49,   30,   83,   86,   27,   28,   75,   42,   43,
 /*   480 */    44,   45,   77,   73,   74,   49,   76,   97,   86,   77,
 /*   490 */    27,   81,   20,   26,   84,   85,   73,   74,   88,   76,
 /*   500 */    90,   30,   97,  106,   81,   95,  102,   84,   85,   97,
 /*   510 */   100,   88,   19,   90,   30,   26,   73,   74,   95,   76,
 /*   520 */    22,   30,   94,  100,   81,   27,   98,   84,   85,   73,
 /*   530 */    74,   88,   76,   90,   30,    4,   49,   81,   95,   16,
 /*   540 */    84,   85,    4,  100,   88,   48,   90,   30,    8,   80,
 /*   550 */    11,   95,   73,   74,   86,   76,  100,  106,   11,   89,
 /*   560 */    27,   97,   15,   84,   85,   15,   19,   88,   78,   90,
 /*   570 */    78,   22,   99,   87,   95,   19,   96,   30,  107,  107,
 /*   580 */    92,  107,  103,  104,  107,  107,  107,  107,  107,   42,
 /*   590 */    43,   44,   45,  107,  107,  107,   49,   73,   74,  107,
 /*   600 */    76,  107,  107,  107,  107,  107,  107,  107,   84,   85,
 /*   610 */   107,  107,   88,  107,   90,   73,   74,   93,   76,   95,
 /*   620 */   107,  107,  107,  107,  107,  107,   84,   85,  107,  107,
 /*   630 */    88,  107,   90,  107,  107,  107,  107,   95,   73,   74,
 /*   640 */   107,   76,  107,  107,  107,  107,  104,  107,  107,   84,
 /*   650 */    85,   73,   74,   88,   76,   90,  107,  107,   93,  107,
 /*   660 */    95,  107,   84,   85,   73,   74,   88,   76,   90,  107,
 /*   670 */   107,   93,  107,   95,  107,   84,   85,  107,  107,   88,
 /*   680 */   107,   90,   73,   74,   93,   76,   95,  107,  107,  107,
 /*   690 */   107,  107,  107,   84,   85,  107,  107,   88,  107,   90,
 /*   700 */    73,   74,  107,   76,   95,  107,  107,  107,  107,  107,
 /*   710 */   107,   84,   85,   73,   74,   88,   76,   90,  107,  107,
 /*   720 */   107,  107,   95,  107,   84,   85,   73,   74,   88,   76,
 /*   730 */    90,  107,  107,  107,  107,   95,  107,   84,   85,   73,
 /*   740 */    74,   88,   76,   90,  107,  107,  107,  107,   95,  107,
 /*   750 */    84,   85,  107,  107,   88,  107,   90,   73,   74,  107,
 /*   760 */    76,   95,  107,  107,  107,  107,  107,  107,   84,   85,
 /*   770 */   107,  107,   88,  107,   90,   73,   74,  107,   76,   95,
 /*   780 */   107,  107,  107,  107,  107,  107,   84,   85,   73,   74,
 /*   790 */    88,   76,   90,  107,  107,  107,  107,   95,  107,   84,
 /*   800 */    85,   73,   74,   88,   76,   90,  107,  107,  107,  107,
 /*   810 */    95,  107,   84,   85,   73,   74,   88,   76,   90,  107,
 /*   820 */   107,  107,  107,   95,  107,   84,   85,  107,  107,   88,
 /*   830 */   107,   90,   73,   74,  107,   76,   95,  107,  107,  107,
 /*   840 */   107,  107,  107,   84,   85,  107,  107,   88,  107,   90,
 /*   850 */    73,   74,  107,   76,   95,  107,  107,  107,  107,  107,
 /*   860 */   107,   84,   85,   73,  107,   88,   76,   90,  107,  107,
 /*   870 */   107,  107,   95,  107,   84,   85,   73,  107,   88,   76,
 /*   880 */    90,  107,  107,  107,  107,   95,  107,   84,   85,   73,
 /*   890 */   107,   88,   76,   90,  107,  107,  107,  107,   95,  107,
 /*   900 */    84,   85,  107,  107,   88,  107,   90,  107,  107,  107,
 /*   910 */   107,   95,
);
    const YY_SHIFT_USE_DFLT = -25;
    const YY_SHIFT_MAX = 153;
    static public $yy_shift_ofst = array(
 /*     0 */     1,  180,   80,   80,   80,   80,   80,   80,   80,   80,
 /*    10 */    80,   80,  243,  190,  243,  190,  190,  190,  190,  190,
 /*    20 */   190,  190,  190,  190,  190,  234,  190,  190,  190,  190,
 /*    30 */   120,  278,  225,  436,  547,  547,  547,  256,  422,  181,
 /*    40 */   284,  299,  100,  100,  401,  498,  296,  331,    1,  111,
 /*    50 */   -13,  341,  442,  113,   91,  396,  457,  396,  457,  396,
 /*    60 */   457,  448,  396,  329,  533,  329,  -24,   -3,   28,   49,
 /*    70 */    49,   49,   49,   49,   49,   49,   49,  259,  139,  110,
 /*    80 */    65,   12,  184,  187,   65,   37,   71,  432,  292,  136,
 /*    90 */   200,  -15,  350,  463,  408,  343,   88,  154,  154,  154,
 /*   100 */   222,  -16,   44,  550,  167,  167,  556,  549,  329,  167,
 /*   110 */   329,  167,  329,  167,  329,  167,  -25,  -25,  -25,  -25,
 /*   120 */   103,  191,  198,   -4,  207,  328,  313,  328,  328,  461,
 /*   130 */   472,  493,  467,  531,  471,  416,  413,  484,  489,  497,
 /*   140 */   517,  540,  538,  523,  491,  504,  384,  487,  539,  356,
 /*   150 */   257,  317,  303,  287,
);
    const YY_REDUCE_USE_DFLT = -54;
    const YY_REDUCE_MAX = 119;
    static public $yy_reduce_ofst = array(
 /*     0 */     2,   94,  321,  354,  265,  288,  252,  334,  443,  456,
 /*    10 */   410,  423,  479,  524,  542,  565,  578,  591,  741,  666,
 /*    20 */   609,  627,  728,  684,  702,  715,  759,  777,  640,  653,
 /*    30 */   816,  803,  790,  290,  240,   82,  306,  107,  308,  -34,
 /*    40 */   314,  170,  -53,  186,  367,  215,  142,  373,   89,  147,
 /*    50 */   147,   76,  390,  397,   76,  340,  326,  405,  194,   -1,
 /*    60 */   298,  388,  412,  428,  402,   36,  404,  404,  404,  404,
 /*    70 */   404,  404,  404,  404,  404,  404,  404,  470,  470,  451,
 /*    80 */   470,  451,  451,  451,  470,  464,  468,  129,  129,  129,
 /*    90 */   468,  129,  468,  468,  129,  129,  468,  480,  480,  480,
 /*   100 */   488,  129,  492,  490,  129,  129,  486,  473,  294,  129,
 /*   110 */   294,  129,  294,  129,  294,  129,  201,  216,  469,  -18,
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
        /* 37 */ array(1, 3, 19, 42, 48, 49, ),
        /* 38 */ array(1, 3, 19, 42, 48, 49, ),
        /* 39 */ array(19, 30, 49, ),
        /* 40 */ array(15, 23, 25, 27, 28, ),
        /* 41 */ array(3, 11, 19, 30, 49, ),
        /* 42 */ array(17, 20, 28, ),
        /* 43 */ array(17, 20, 28, ),
        /* 44 */ array(19, 30, 49, ),
        /* 45 */ array(22, 27, ),
        /* 46 */ array(19, 49, ),
        /* 47 */ array(25, 27, ),
        /* 48 */ array(1, 2, 3, 5, 6, 7, 10, 61, 63, 64, 65, ),
        /* 49 */ array(16, 25, 31, 32, 33, 34, 35, 36, 37, 38, ),
        /* 50 */ array(25, 31, 32, 33, 34, 35, 36, 37, 38, ),
        /* 51 */ array(3, 26, 30, 46, 50, ),
        /* 52 */ array(3, 14, 26, 30, ),
        /* 53 */ array(1, 3, 43, ),
        /* 54 */ array(3, 30, 50, ),
        /* 55 */ array(3, 30, ),
        /* 56 */ array(1, 3, ),
        /* 57 */ array(3, 30, ),
        /* 58 */ array(1, 3, ),
        /* 59 */ array(3, 30, ),
        /* 60 */ array(1, 3, ),
        /* 61 */ array(27, 28, ),
        /* 62 */ array(3, 30, ),
        /* 63 */ array(28, ),
        /* 64 */ array(27, ),
        /* 65 */ array(28, ),
        /* 66 */ array(24, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 67 */ array(4, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 68 */ array(16, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 69 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 70 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 71 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 72 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 73 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 74 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 75 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 76 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 77 */ array(4, 12, 13, 47, ),
        /* 78 */ array(12, 13, 18, 47, ),
        /* 79 */ array(1, 3, 43, ),
        /* 80 */ array(12, 13, 47, ),
        /* 81 */ array(1, 3, 62, ),
        /* 82 */ array(1, 3, 66, ),
        /* 83 */ array(1, 3, 9, ),
        /* 84 */ array(12, 13, 47, ),
        /* 85 */ array(3, 26, 30, ),
        /* 86 */ array(4, 27, ),
        /* 87 */ array(4, 25, ),
        /* 88 */ array(4, 25, ),
        /* 89 */ array(4, 25, ),
        /* 90 */ array(4, 27, ),
        /* 91 */ array(25, 29, ),
        /* 92 */ array(4, 27, ),
        /* 93 */ array(4, 27, ),
        /* 94 */ array(4, 25, ),
        /* 95 */ array(16, 25, ),
        /* 96 */ array(4, 27, ),
        /* 97 */ array(17, 20, ),
        /* 98 */ array(17, 20, ),
        /* 99 */ array(17, 20, ),
        /* 100 */ array(19, 30, ),
        /* 101 */ array(21, 25, ),
        /* 102 */ array(3, 15, ),
        /* 103 */ array(15, ),
        /* 104 */ array(25, ),
        /* 105 */ array(25, ),
        /* 106 */ array(19, ),
        /* 107 */ array(22, ),
        /* 108 */ array(28, ),
        /* 109 */ array(25, ),
        /* 110 */ array(28, ),
        /* 111 */ array(25, ),
        /* 112 */ array(28, ),
        /* 113 */ array(25, ),
        /* 114 */ array(28, ),
        /* 115 */ array(25, ),
        /* 116 */ array(),
        /* 117 */ array(),
        /* 118 */ array(),
        /* 119 */ array(),
        /* 120 */ array(15, 23, 29, ),
        /* 121 */ array(15, 23, 26, ),
        /* 122 */ array(15, 18, 23, ),
        /* 123 */ array(30, 50, ),
        /* 124 */ array(21, 24, ),
        /* 125 */ array(15, 23, ),
        /* 126 */ array(18, 21, ),
        /* 127 */ array(15, 23, ),
        /* 128 */ array(15, 23, ),
        /* 129 */ array(4, ),
        /* 130 */ array(20, ),
        /* 131 */ array(19, ),
        /* 132 */ array(26, ),
        /* 133 */ array(4, ),
        /* 134 */ array(30, ),
        /* 135 */ array(30, ),
        /* 136 */ array(48, ),
        /* 137 */ array(30, ),
        /* 138 */ array(26, ),
        /* 139 */ array(48, ),
        /* 140 */ array(30, ),
        /* 141 */ array(8, ),
        /* 142 */ array(4, ),
        /* 143 */ array(16, ),
        /* 144 */ array(30, ),
        /* 145 */ array(30, ),
        /* 146 */ array(30, ),
        /* 147 */ array(49, ),
        /* 148 */ array(11, ),
        /* 149 */ array(30, ),
        /* 150 */ array(4, ),
        /* 151 */ array(15, ),
        /* 152 */ array(16, ),
        /* 153 */ array(16, ),
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
);
    static public $yy_default = array(
 /*     0 */   395,  395,  395,  395,  395,  395,  395,  395,  395,  395,
 /*    10 */   395,  395,  380,  344,  395,  344,  344,  344,  395,  395,
 /*    20 */   395,  395,  395,  395,  395,  395,  395,  395,  395,  395,
 /*    30 */   395,  395,  395,  395,  395,  395,  395,  395,  395,  395,
 /*    40 */   283,  395,  269,  314,  395,  283,  395,  283,  254,  354,
 /*    50 */   354,  321,  395,  395,  321,  395,  395,  395,  395,  395,
 /*    60 */   395,  283,  395,  310,  283,  309,  395,  395,  395,  361,
 /*    70 */   356,  352,  358,  357,  365,  366,  362,  395,  395,  395,
 /*    80 */   349,  395,  395,  395,  289,  395,  395,  395,  395,  395,
 /*    90 */   395,  381,  395,  395,  395,  395,  395,  338,  337,  336,
 /*   100 */   395,  343,  321,  321,  277,  287,  395,  290,  315,  284,
 /*   110 */   311,  382,  333,  383,  312,  355,  348,  321,  348,  321,
 /*   120 */   288,  288,  395,  395,  395,  288,  395,  395,  350,  395,
 /*   130 */   299,  395,  316,  395,  395,  395,  395,  395,  395,  395,
 /*   140 */   395,  395,  395,  395,  395,  395,  395,  395,  395,  395,
 /*   150 */   395,  313,  395,  395,  374,  371,  360,  359,  324,  292,
 /*   160 */   351,  293,  373,  291,  298,  322,  323,  372,  319,  261,
 /*   170 */   286,  391,  278,  393,  394,  276,  258,  285,  274,  255,
 /*   180 */   273,  256,  257,  392,  259,  320,  264,  364,  363,  316,
 /*   190 */   263,  375,  370,  260,  353,  262,  376,  318,  272,  377,
 /*   200 */   304,  347,  334,  339,  325,  317,  388,  332,  295,  331,
 /*   210 */   330,  296,  341,  340,  389,  342,  378,  379,  390,  308,
 /*   220 */   328,  297,  326,  327,  385,  329,  265,  303,  279,  345,
 /*   230 */   335,  346,  280,  302,  367,  368,  300,  275,  301,  271,
 /*   240 */   294,  281,  386,  267,  387,  266,  282,  384,  305,  270,
 /*   250 */   306,  268,  307,  369,
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
    const YYNSTATE = 254;
    const YYNRULE = 141;
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
 /*  84 */ "objectelement ::= PTR ID LDEL expr RDEL arrayindex",
 /*  85 */ "objectelement ::= PTR method",
 /*  86 */ "function ::= ID OPENP params CLOSEP",
 /*  87 */ "method ::= ID OPENP params CLOSEP",
 /*  88 */ "params ::= expr COMMA params",
 /*  89 */ "params ::= expr",
 /*  90 */ "params ::=",
 /*  91 */ "modifier ::= VERT AT ID",
 /*  92 */ "modifier ::= VERT ID",
 /*  93 */ "modparameters ::= modparameters modparameter",
 /*  94 */ "modparameters ::=",
 /*  95 */ "modparameter ::= COLON exprs",
 /*  96 */ "modparameter ::= COLON ID",
 /*  97 */ "ifexprs ::= ifexpr",
 /*  98 */ "ifexprs ::= NOT ifexprs",
 /*  99 */ "ifexprs ::= OPENP ifexprs CLOSEP",
 /* 100 */ "ifexpr ::= expr",
 /* 101 */ "ifexpr ::= expr ifcond expr",
 /* 102 */ "ifexpr ::= ifexprs lop ifexprs",
 /* 103 */ "ifexpr ::= ifexprs ISDIVBY ifexprs",
 /* 104 */ "ifexpr ::= ifexprs ISNOTDIVBY ifexprs",
 /* 105 */ "ifexpr ::= ifexprs ISEVEN",
 /* 106 */ "ifexpr ::= ifexprs ISNOTEVEN",
 /* 107 */ "ifexpr ::= ifexprs ISEVENBY ifexprs",
 /* 108 */ "ifexpr ::= ifexprs ISNOTEVENBY ifexprs",
 /* 109 */ "ifexpr ::= ifexprs ISODD",
 /* 110 */ "ifexpr ::= ifexprs ISNOTODD",
 /* 111 */ "ifexpr ::= ifexprs ISODDBY ifexprs",
 /* 112 */ "ifexpr ::= ifexprs ISNOTODDBY ifexprs",
 /* 113 */ "ifcond ::= EQUALS",
 /* 114 */ "ifcond ::= NOTEQUALS",
 /* 115 */ "ifcond ::= GREATERTHAN",
 /* 116 */ "ifcond ::= LESSTHAN",
 /* 117 */ "ifcond ::= GREATEREQUAL",
 /* 118 */ "ifcond ::= LESSEQUAL",
 /* 119 */ "ifcond ::= IDENTITY",
 /* 120 */ "ifcond ::= NONEIDENTITY",
 /* 121 */ "lop ::= LAND",
 /* 122 */ "lop ::= LOR",
 /* 123 */ "array ::= OPENB arrayelements CLOSEB",
 /* 124 */ "arrayelements ::= arrayelement",
 /* 125 */ "arrayelements ::= arrayelements COMMA arrayelement",
 /* 126 */ "arrayelements ::=",
 /* 127 */ "arrayelement ::= expr",
 /* 128 */ "arrayelement ::= expr APTR expr",
 /* 129 */ "arrayelement ::= ID APTR expr",
 /* 130 */ "doublequoted ::= doublequoted doublequotedcontent",
 /* 131 */ "doublequoted ::= doublequotedcontent",
 /* 132 */ "doublequotedcontent ::= variable",
 /* 133 */ "doublequotedcontent ::= BACKTICK ID BACKTICK",
 /* 134 */ "doublequotedcontent ::= BACKTICK variable BACKTICK",
 /* 135 */ "doublequotedcontent ::= LDEL expr RDEL",
 /* 136 */ "doublequotedcontent ::= OTHER",
 /* 137 */ "text ::= text textelement",
 /* 138 */ "text ::= textelement",
 /* 139 */ "textelement ::= OTHER",
 /* 140 */ "textelement ::= LDEL",
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
        124 => 0,
        1 => 1,
        35 => 1,
        37 => 1,
        42 => 1,
        43 => 1,
        75 => 1,
        97 => 1,
        131 => 1,
        138 => 1,
        139 => 1,
        140 => 1,
        2 => 2,
        66 => 2,
        130 => 2,
        137 => 2,
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
        89 => 24,
        127 => 24,
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
        94 => 67,
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
        99 => 78,
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
        90 => 90,
        91 => 91,
        92 => 92,
        93 => 93,
        95 => 95,
        96 => 96,
        98 => 98,
        100 => 100,
        101 => 101,
        102 => 101,
        103 => 103,
        104 => 104,
        105 => 105,
        110 => 105,
        106 => 106,
        109 => 106,
        107 => 107,
        112 => 107,
        108 => 108,
        111 => 108,
        113 => 113,
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
        125 => 125,
        126 => 126,
        128 => 128,
        129 => 129,
        132 => 132,
        133 => 133,
        134 => 134,
        135 => 135,
        136 => 136,
    );
    /* Beginning here are the reduction cases.  A typical example
    ** follows:
    **  #line <lineno> <grammarfile>
    **   function yy_r0($yymsp){ ... }           // User supplied code
    **  #line <lineno> <thisfile>
    */
#line 73 "internal.templateparser.y"
    function yy_r0(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;     }
#line 1791 "internal.templateparser.php"
#line 79 "internal.templateparser.y"
    function yy_r1(){$this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1794 "internal.templateparser.php"
#line 81 "internal.templateparser.y"
    function yy_r2(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1797 "internal.templateparser.php"
#line 87 "internal.templateparser.y"
    function yy_r3(){if ($this->compiler->has_code) {
                                            $tmp =''; foreach ($this->prefix_code as $code) {$tmp.=$code;} $this->prefix_code=array();
                                            $this->_retvalue = $this->cacher->processNocacheCode($tmp.$this->yystack[$this->yyidx + 0]->minor, $this->compiler,$this->nocache,true);
                                         } $this->nocache=false;    }
#line 1803 "internal.templateparser.php"
#line 100 "internal.templateparser.y"
    function yy_r4(){ $this->_retvalue = '';    }
#line 1806 "internal.templateparser.php"
#line 103 "internal.templateparser.y"
    function yy_r5(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + -1]->minor, $this->compiler,false,false);    }
#line 1809 "internal.templateparser.php"
#line 105 "internal.templateparser.y"
    function yy_r6(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->left_delimiter, $this->compiler,false,false);    }
#line 1812 "internal.templateparser.php"
#line 107 "internal.templateparser.y"
    function yy_r7(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->right_delimiter, $this->compiler,false,false);    }
#line 1815 "internal.templateparser.php"
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
#line 1826 "internal.templateparser.php"
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
#line 1837 "internal.templateparser.php"
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
#line 1848 "internal.templateparser.php"
#line 139 "internal.templateparser.y"
    function yy_r11(){$this->_retvalue = $this->cacher->processNocacheCode("<?php echo '".$this->yystack[$this->yyidx + 0]->minor."';?>\n", $this->compiler, true, true);    }
#line 1851 "internal.templateparser.php"
#line 141 "internal.templateparser.y"
    function yy_r12(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + 0]->minor, $this->compiler,false,false);    }
#line 1854 "internal.templateparser.php"
#line 149 "internal.templateparser.y"
    function yy_r13(){ $this->_retvalue = $this->compiler->compileTag('print_expression',array_merge(array('value'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1857 "internal.templateparser.php"
#line 152 "internal.templateparser.y"
    function yy_r14(){ $this->_retvalue = $this->compiler->compileTag('assign',array_merge(array('value'=>$this->yystack[$this->yyidx + -1]->minor),$this->yystack[$this->yyidx + -3]->minor));    }
#line 1860 "internal.templateparser.php"
#line 154 "internal.templateparser.y"
    function yy_r15(){$this->_retvalue = array('var'=>$this->yystack[$this->yyidx + -1]->minor, 'index'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1863 "internal.templateparser.php"
#line 158 "internal.templateparser.y"
    function yy_r16(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor,$this->yystack[$this->yyidx + -1]->minor);    }
#line 1866 "internal.templateparser.php"
#line 160 "internal.templateparser.y"
    function yy_r17(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -4]->minor,array_merge(array('object_methode'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1869 "internal.templateparser.php"
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
#line 1884 "internal.templateparser.php"
#line 176 "internal.templateparser.y"
    function yy_r19(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor.'close',$this->yystack[$this->yyidx + -1]->minor);    }
#line 1887 "internal.templateparser.php"
#line 178 "internal.templateparser.y"
    function yy_r20(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor.'close',array('object_methode'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1890 "internal.templateparser.php"
#line 180 "internal.templateparser.y"
    function yy_r21(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor,array('if condition'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1893 "internal.templateparser.php"
#line 182 "internal.templateparser.y"
    function yy_r22(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -9]->minor,array('start'=>$this->yystack[$this->yyidx + -7]->minor,'ifexp'=>$this->yystack[$this->yyidx + -5]->minor,'varloop'=>$this->yystack[$this->yyidx + -2]->minor,'loop'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1896 "internal.templateparser.php"
#line 183 "internal.templateparser.y"
    function yy_r23(){ $this->_retvalue = '='.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1899 "internal.templateparser.php"
#line 184 "internal.templateparser.y"
    function yy_r24(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1902 "internal.templateparser.php"
#line 187 "internal.templateparser.y"
    function yy_r25(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -6]->minor,array('from'=>$this->yystack[$this->yyidx + -1]->minor,'item'=>$this->yystack[$this->yyidx + -3]->minor));    }
#line 1905 "internal.templateparser.php"
#line 194 "internal.templateparser.y"
    function yy_r27(){ $this->_retvalue = array_merge($this->yystack[$this->yyidx + -1]->minor,$this->yystack[$this->yyidx + 0]->minor);    }
#line 1908 "internal.templateparser.php"
#line 198 "internal.templateparser.y"
    function yy_r29(){ $this->_retvalue = array();    }
#line 1911 "internal.templateparser.php"
#line 202 "internal.templateparser.y"
    function yy_r30(){ $this->_retvalue = array($this->yystack[$this->yyidx + -2]->minor=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1914 "internal.templateparser.php"
#line 207 "internal.templateparser.y"
    function yy_r31(){ $this->_retvalue = array($this->yystack[$this->yyidx + 0]->minor);    }
#line 1917 "internal.templateparser.php"
#line 208 "internal.templateparser.y"
    function yy_r32(){ $this->yystack[$this->yyidx + -2]->minor[]=$this->yystack[$this->yyidx + 0]->minor; $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor;    }
#line 1920 "internal.templateparser.php"
#line 210 "internal.templateparser.y"
    function yy_r33(){ $this->_retvalue = array('var' => $this->yystack[$this->yyidx + -2]->minor, 'value'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1923 "internal.templateparser.php"
#line 217 "internal.templateparser.y"
    function yy_r34(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';     }
#line 1926 "internal.templateparser.php"
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
#line 1941 "internal.templateparser.php"
#line 239 "internal.templateparser.y"
    function yy_r39(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1944 "internal.templateparser.php"
#line 241 "internal.templateparser.y"
    function yy_r40(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor . $this->yystack[$this->yyidx + -1]->minor . $this->yystack[$this->yyidx + 0]->minor;     }
#line 1947 "internal.templateparser.php"
#line 243 "internal.templateparser.y"
    function yy_r41(){ $this->_retvalue = '('. $this->yystack[$this->yyidx + -2]->minor . ').(' . $this->yystack[$this->yyidx + 0]->minor. ')';     }
#line 1950 "internal.templateparser.php"
#line 260 "internal.templateparser.y"
    function yy_r46(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1953 "internal.templateparser.php"
#line 269 "internal.templateparser.y"
    function yy_r50(){ $this->_retvalue = "(". $this->yystack[$this->yyidx + -1]->minor .")";     }
#line 1956 "internal.templateparser.php"
#line 272 "internal.templateparser.y"
    function yy_r51(){ $this->_retvalue = "'".$this->yystack[$this->yyidx + -1]->minor."'";     }
#line 1959 "internal.templateparser.php"
#line 273 "internal.templateparser.y"
    function yy_r52(){ $this->_retvalue = "''";     }
#line 1962 "internal.templateparser.php"
#line 275 "internal.templateparser.y"
    function yy_r53(){ $this->_retvalue = "'".str_replace('\"','"',$this->yystack[$this->yyidx + -1]->minor)."'";     }
#line 1965 "internal.templateparser.php"
#line 281 "internal.templateparser.y"
    function yy_r55(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1968 "internal.templateparser.php"
#line 282 "internal.templateparser.y"
    function yy_r56(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -3]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -6]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -1]->minor .')';     }
#line 1971 "internal.templateparser.php"
#line 284 "internal.templateparser.y"
    function yy_r57(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor.'::'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1974 "internal.templateparser.php"
#line 285 "internal.templateparser.y"
    function yy_r58(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -4]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -7]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -2]->minor .')'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1977 "internal.templateparser.php"
#line 287 "internal.templateparser.y"
    function yy_r59(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1980 "internal.templateparser.php"
#line 289 "internal.templateparser.y"
    function yy_r60(){ $this->_retvalue = $this->yystack[$this->yyidx + -4]->minor.'::$'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1983 "internal.templateparser.php"
#line 291 "internal.templateparser.y"
    function yy_r61(){ $this->_retvalue = $this->yystack[$this->yyidx + -5]->minor.'::$'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1986 "internal.templateparser.php"
#line 300 "internal.templateparser.y"
    function yy_r62(){ if ($this->yystack[$this->yyidx + 0]->minor['var'] == '\'smarty\'') { $this->_retvalue =  $this->compiler->compileTag(trim($this->yystack[$this->yyidx + 0]->minor['var'],"'"),$this->yystack[$this->yyidx + 0]->minor['index']);} else {
                                                         $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + 0]->minor['var'] .')->value'.$this->yystack[$this->yyidx + 0]->minor['index']; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + 0]->minor['var'],"'"))->nocache;}    }
#line 1990 "internal.templateparser.php"
#line 303 "internal.templateparser.y"
    function yy_r63(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->'.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 1993 "internal.templateparser.php"
#line 307 "internal.templateparser.y"
    function yy_r65(){$this->_retvalue = '$_smarty_tpl->getConfigVariable(\''. $this->yystack[$this->yyidx + -1]->minor .'\')';    }
#line 1996 "internal.templateparser.php"
#line 315 "internal.templateparser.y"
    function yy_r67(){return;    }
#line 1999 "internal.templateparser.php"
#line 319 "internal.templateparser.y"
    function yy_r68(){ $this->_retvalue = "['". $this->yystack[$this->yyidx + 0]->minor ."']";    }
#line 2002 "internal.templateparser.php"
#line 320 "internal.templateparser.y"
    function yy_r69(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + 0]->minor ."]";    }
#line 2005 "internal.templateparser.php"
#line 322 "internal.templateparser.y"
    function yy_r70(){ $this->_retvalue = "[".$this->yystack[$this->yyidx + 0]->minor."]";    }
#line 2008 "internal.templateparser.php"
#line 323 "internal.templateparser.y"
    function yy_r71(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + -1]->minor ."]";    }
#line 2011 "internal.templateparser.php"
#line 325 "internal.templateparser.y"
    function yy_r72(){ $this->_retvalue = '['.$this->compiler->compileTag('smarty','[\'section\'][\''.$this->yystack[$this->yyidx + -1]->minor.'\'][\'index\']').']';    }
#line 2014 "internal.templateparser.php"
#line 329 "internal.templateparser.y"
    function yy_r74(){$this->_retvalue = '';    }
#line 2017 "internal.templateparser.php"
#line 337 "internal.templateparser.y"
    function yy_r76(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2020 "internal.templateparser.php"
#line 339 "internal.templateparser.y"
    function yy_r77(){$this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2023 "internal.templateparser.php"
#line 341 "internal.templateparser.y"
    function yy_r78(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2026 "internal.templateparser.php"
#line 346 "internal.templateparser.y"
    function yy_r79(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->value'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 2029 "internal.templateparser.php"
#line 348 "internal.templateparser.y"
    function yy_r80(){$this->_retvalue  = $this->yystack[$this->yyidx + 0]->minor;     }
#line 2032 "internal.templateparser.php"
#line 350 "internal.templateparser.y"
    function yy_r81(){$this->_retvalue  = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2035 "internal.templateparser.php"
#line 352 "internal.templateparser.y"
    function yy_r82(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2038 "internal.templateparser.php"
#line 353 "internal.templateparser.y"
    function yy_r83(){ $this->_retvalue = '->{'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2041 "internal.templateparser.php"
#line 356 "internal.templateparser.y"
    function yy_r84(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + -4]->minor.'{'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + 0]->minor.'}';    }
#line 2044 "internal.templateparser.php"
#line 359 "internal.templateparser.y"
    function yy_r85(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2047 "internal.templateparser.php"
#line 365 "internal.templateparser.y"
    function yy_r86(){if (!$this->template->security || $this->smarty->security_handler->isTrustedPhpFunction($this->yystack[$this->yyidx + -3]->minor, $this->compiler)) {
																					            if ($this->yystack[$this->yyidx + -3]->minor == 'isset' || $this->yystack[$this->yyidx + -3]->minor == 'empty' || $this->yystack[$this->yyidx + -3]->minor == 'array' || is_callable($this->yystack[$this->yyidx + -3]->minor)) {
																					                $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";
																					            } else {
                                                       $this->compiler->trigger_template_error ("unknown function \"" . $this->yystack[$this->yyidx + -3]->minor . "\"");
                                                      }
                                                    }    }
#line 2056 "internal.templateparser.php"
#line 376 "internal.templateparser.y"
    function yy_r87(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";    }
#line 2059 "internal.templateparser.php"
#line 380 "internal.templateparser.y"
    function yy_r88(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.",".$this->yystack[$this->yyidx + 0]->minor;    }
#line 2062 "internal.templateparser.php"
#line 384 "internal.templateparser.y"
    function yy_r90(){ return;    }
#line 2065 "internal.templateparser.php"
#line 389 "internal.templateparser.y"
    function yy_r91(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,true);    }
#line 2068 "internal.templateparser.php"
#line 390 "internal.templateparser.y"
    function yy_r92(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,false);    }
#line 2071 "internal.templateparser.php"
#line 397 "internal.templateparser.y"
    function yy_r93(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2074 "internal.templateparser.php"
#line 401 "internal.templateparser.y"
    function yy_r95(){$this->_retvalue = ','.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2077 "internal.templateparser.php"
#line 402 "internal.templateparser.y"
    function yy_r96(){$this->_retvalue = ',\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2080 "internal.templateparser.php"
#line 409 "internal.templateparser.y"
    function yy_r98(){$this->_retvalue = '!'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2083 "internal.templateparser.php"
#line 414 "internal.templateparser.y"
    function yy_r100(){$this->_retvalue =$this->yystack[$this->yyidx + 0]->minor;    }
#line 2086 "internal.templateparser.php"
#line 415 "internal.templateparser.y"
    function yy_r101(){$this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2089 "internal.templateparser.php"
#line 417 "internal.templateparser.y"
    function yy_r103(){$this->_retvalue = '!('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2092 "internal.templateparser.php"
#line 418 "internal.templateparser.y"
    function yy_r104(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2095 "internal.templateparser.php"
#line 419 "internal.templateparser.y"
    function yy_r105(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2098 "internal.templateparser.php"
#line 420 "internal.templateparser.y"
    function yy_r106(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2101 "internal.templateparser.php"
#line 421 "internal.templateparser.y"
    function yy_r107(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2104 "internal.templateparser.php"
#line 422 "internal.templateparser.y"
    function yy_r108(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2107 "internal.templateparser.php"
#line 428 "internal.templateparser.y"
    function yy_r113(){$this->_retvalue = '==';    }
#line 2110 "internal.templateparser.php"
#line 429 "internal.templateparser.y"
    function yy_r114(){$this->_retvalue = '!=';    }
#line 2113 "internal.templateparser.php"
#line 430 "internal.templateparser.y"
    function yy_r115(){$this->_retvalue = '>';    }
#line 2116 "internal.templateparser.php"
#line 431 "internal.templateparser.y"
    function yy_r116(){$this->_retvalue = '<';    }
#line 2119 "internal.templateparser.php"
#line 432 "internal.templateparser.y"
    function yy_r117(){$this->_retvalue = '>=';    }
#line 2122 "internal.templateparser.php"
#line 433 "internal.templateparser.y"
    function yy_r118(){$this->_retvalue = '<=';    }
#line 2125 "internal.templateparser.php"
#line 434 "internal.templateparser.y"
    function yy_r119(){$this->_retvalue = '===';    }
#line 2128 "internal.templateparser.php"
#line 435 "internal.templateparser.y"
    function yy_r120(){$this->_retvalue = '!==';    }
#line 2131 "internal.templateparser.php"
#line 437 "internal.templateparser.y"
    function yy_r121(){$this->_retvalue = '&&';    }
#line 2134 "internal.templateparser.php"
#line 438 "internal.templateparser.y"
    function yy_r122(){$this->_retvalue = '||';    }
#line 2137 "internal.templateparser.php"
#line 443 "internal.templateparser.y"
    function yy_r123(){ $this->_retvalue = 'array('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2140 "internal.templateparser.php"
#line 445 "internal.templateparser.y"
    function yy_r125(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.','.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2143 "internal.templateparser.php"
#line 446 "internal.templateparser.y"
    function yy_r126(){ return;     }
#line 2146 "internal.templateparser.php"
#line 448 "internal.templateparser.y"
    function yy_r128(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2149 "internal.templateparser.php"
#line 449 "internal.templateparser.y"
    function yy_r129(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + -2]->minor.'\'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2152 "internal.templateparser.php"
#line 456 "internal.templateparser.y"
    function yy_r132(){$this->_retvalue = "'.".$this->yystack[$this->yyidx + 0]->minor.".'";    }
#line 2155 "internal.templateparser.php"
#line 457 "internal.templateparser.y"
    function yy_r133(){$this->_retvalue = "`".$this->yystack[$this->yyidx + -1]->minor."`";    }
#line 2158 "internal.templateparser.php"
#line 458 "internal.templateparser.y"
    function yy_r134(){$this->_retvalue = "'.".$this->yystack[$this->yyidx + -1]->minor.".'";    }
#line 2161 "internal.templateparser.php"
#line 459 "internal.templateparser.y"
    function yy_r135(){$this->_retvalue = "'.(".$this->yystack[$this->yyidx + -1]->minor.").'";    }
#line 2164 "internal.templateparser.php"
#line 460 "internal.templateparser.y"
    function yy_r136(){$this->_retvalue = addcslashes($this->yystack[$this->yyidx + 0]->minor,"'");    }
#line 2167 "internal.templateparser.php"

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
#line 2285 "internal.templateparser.php"
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
#line 2310 "internal.templateparser.php"
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
