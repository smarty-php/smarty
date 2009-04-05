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
    const YY_NO_ACTION = 380;
    const YY_ACCEPT_ACTION = 379;
    const YY_ERROR_ACTION = 378;

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
    const YY_SZ_ACTTAB = 900;
static public $yy_action = array(
 /*     0 */   123,   93,  216,  191,   95,  111,  156,  379,   45,  174,
 /*    10 */   177,   13,  197,  162,  166,  182,  150,  149,   81,   88,
 /*    20 */   161,  137,  229,  145,  156,  157,  220,  151,  147,  160,
 /*    30 */   164,    9,    7,    4,   10,   11,    6,  150,  149,  167,
 /*    40 */    26,  170,  167,  157,  170,  238,  237,  172,  151,  147,
 /*    50 */   160,  164,    9,    7,    4,   10,   11,    6,  150,  149,
 /*    60 */   152,  235,   34,   19,    8,  207,   12,  227,   52,  151,
 /*    70 */   147,  160,  164,    9,    7,    4,   10,   11,    6,  116,
 /*    80 */    33,  138,  199,   32,  238,  237,  121,  136,    2,   13,
 /*    90 */   194,   36,   50,  146,  165,  150,  149,   88,  142,  121,
 /*   100 */    17,    1,  131,  178,  121,  130,  151,  147,  160,  164,
 /*   110 */     9,    7,    4,   10,   11,    6,   26,   29,   92,   33,
 /*   120 */    31,  121,  132,  191,   47,   23,  156,  188,   96,  197,
 /*   130 */   241,   63,  117,  156,  166,  182,  209,  169,   81,   28,
 /*   140 */   161,   26,  230,  227,   26,  157,   13,  152,   37,   34,
 /*   150 */   183,    8,  157,   12,   88,   54,  201,  152,  139,   34,
 /*   160 */    25,   18,  198,   12,  196,   52,  113,   19,  227,  135,
 /*   170 */   101,  227,  180,  112,  212,    2,  114,  121,   36,   50,
 /*   180 */   146,  165,   91,  121,  241,  142,  112,  156,   36,   50,
 /*   190 */   146,  165,  152,  197,   34,  142,   18,  167,   12,  170,
 /*   200 */    52,  152,   26,   34,  214,   18,  157,   12,  103,   52,
 /*   210 */   180,  116,  212,  211,  133,   21,  158,  210,  190,   44,
 /*   220 */   122,   49,   59,   36,   50,  146,  165,  130,  140,  227,
 /*   230 */   142,  156,   36,   50,  146,  165,  152,  215,   34,  142,
 /*   240 */    18,  206,   12,  200,   52,   13,  121,   43,  195,  136,
 /*   250 */   157,  193,  152,   88,   34,   38,   18,  167,   12,  170,
 /*   260 */    52,  231,  185,  107,   14,  173,  130,   36,   50,  146,
 /*   270 */   165,  119,  121,   57,  142,  187,  189,   55,  102,   29,
 /*   280 */    80,    5,   31,   36,   50,  146,  165,  152,  130,   34,
 /*   290 */   142,   18,  167,   12,  170,   52,  167,   30,  170,  155,
 /*   300 */   191,   46,  130,  156,  176,  177,  115,  121,   65,  130,
 /*   310 */   112,  166,  182,  191,  171,   81,  156,  161,   36,   50,
 /*   320 */   146,  165,  157,   78,  222,  142,   52,  183,  191,   47,
 /*   330 */   161,  156,   27,  204,  159,  157,   66,  143,   84,  166,
 /*   340 */   182,  191,   47,   81,  156,  161,  202,   99,   24,   64,
 /*   350 */   157,  212,  166,  182,  141,  183,   81,  171,  161,  191,
 /*   360 */    47,   13,  156,  157,   52,  130,  125,   72,  183,   88,
 /*   370 */   166,  182,   20,   87,   81,   16,  161,  191,   47,  234,
 /*   380 */   156,  157,  230,   30,  197,   67,  183,  154,  166,  182,
 /*   390 */    40,   48,   81,   41,  161,  191,   47,  100,  156,  157,
 /*   400 */    51,  212,  130,   71,  183,   82,  166,  182,   97,  209,
 /*   410 */    81,  206,  161,  191,   47,   20,  156,  157,  129,  153,
 /*   420 */   206,   69,  183,   75,  166,  182,  191,   47,   81,  156,
 /*   430 */   161,  202,   61,   24,   73,  157,   58,  166,  182,  171,
 /*   440 */   183,   81,   77,  161,  191,   94,  240,  156,  157,   52,
 /*   450 */   239,  124,   85,  183,  175,  166,  182,  171,  186,   81,
 /*   460 */     3,  161,  206,  197,   53,  236,  157,  221,  191,   47,
 /*   470 */   191,  156,  203,  156,  120,  233,   68,  181,   41,  166,
 /*   480 */   182,  128,  127,   81,   52,  161,  208,  161,  215,   62,
 /*   490 */   157,  168,  157,  148,   22,  183,   35,  121,  209,   96,
 /*   500 */   213,   60,  180,  218,  219,  223,  224,  228,  192,  226,
 /*   510 */   225,  191,   47,  130,  156,   15,  205,   39,  106,   70,
 /*   520 */    56,  152,  166,  182,  179,   18,   81,   12,  161,   52,
 /*   530 */   245,  152,  245,  157,  245,   18,  245,  245,  183,   52,
 /*   540 */   118,  245,  245,  245,  245,  245,  245,  245,  245,  245,
 /*   550 */   118,  245,   36,   50,  146,  165,  245,  245,  245,  142,
 /*   560 */   245,  245,   36,   50,  146,  165,  245,  245,  245,  142,
 /*   570 */   245,  191,   95,  245,  156,  245,  245,  245,  245,  245,
 /*   580 */   245,  245,  166,  182,  191,   94,   81,  156,  161,  245,
 /*   590 */   245,  144,  245,  157,  245,  166,  182,  191,   95,   81,
 /*   600 */   156,  161,  245,  245,  245,  245,  157,  245,  166,  182,
 /*   610 */   245,  245,   81,  245,  161,  232,  245,  134,  245,  157,
 /*   620 */   245,  245,  245,  245,  191,   95,  245,  156,  245,  245,
 /*   630 */   245,  245,  245,  245,  245,  166,  182,  245,  245,   81,
 /*   640 */   245,  161,  245,  121,  217,  245,  157,  245,  245,  218,
 /*   650 */   219,  223,  224,  228,  192,  226,  225,  191,   98,  245,
 /*   660 */   156,  245,  245,  245,  245,  245,  245,  245,  166,  182,
 /*   670 */   191,   83,   81,  156,  161,  245,  245,  245,  245,  157,
 /*   680 */   245,  166,  182,  191,  104,   81,  156,  161,  245,  245,
 /*   690 */   245,  245,  157,  245,  166,  182,  245,  245,   81,  245,
 /*   700 */   161,  191,   42,  245,  126,  157,  245,  245,  245,  245,
 /*   710 */   245,  245,  166,  182,  245,  245,   81,  245,  161,  245,
 /*   720 */   245,  245,  245,  157,  191,  109,  245,  156,  245,  245,
 /*   730 */   245,  245,  245,  245,  245,  166,  182,  245,  245,   81,
 /*   740 */   245,  161,  191,   90,  245,  156,  157,  245,  245,  245,
 /*   750 */   245,  245,  245,  166,  182,  191,   86,   81,  156,  161,
 /*   760 */   245,  245,  245,  245,  157,  245,  166,  182,  191,  105,
 /*   770 */    81,  156,  161,  245,  245,  245,  245,  157,  245,  166,
 /*   780 */   182,  245,  245,   81,  245,  161,  191,  108,  245,  156,
 /*   790 */   157,  245,  245,  245,  245,  245,  245,  166,  182,  191,
 /*   800 */   110,   81,  156,  161,  245,  245,  245,  245,  157,  245,
 /*   810 */   166,  182,  245,  245,   81,  245,  161,  191,   89,  245,
 /*   820 */   156,  157,  245,  245,  245,  245,  245,  245,  166,  182,
 /*   830 */   191,  245,   81,  156,  161,  245,  245,  245,  245,  157,
 /*   840 */   245,  166,  182,  191,  245,   79,  156,  161,  245,  245,
 /*   850 */   245,  245,  157,  245,  166,  182,  245,  245,   74,  191,
 /*   860 */   161,  191,  156,  245,  156,  157,  245,  245,  245,  245,
 /*   870 */   166,  182,  163,  245,   76,  245,  161,  191,  161,  245,
 /*   880 */   156,  157,  245,  157,  245,  245,  245,  245,  184,  245,
 /*   890 */   245,  245,  245,  245,  161,  245,  245,  245,  245,  157,
    );
    static public $yy_lookahead = array(
 /*     0 */    24,   75,   16,   73,   74,   79,   76,   68,   69,   70,
 /*    10 */    71,   15,   86,   16,   84,   85,   40,   41,   88,   23,
 /*    20 */    90,   73,    4,   93,   76,   95,   48,   51,   52,   53,
 /*    30 */    54,   55,   56,   57,   58,   59,   60,   40,   41,    1,
 /*    40 */     3,    3,    1,   95,    3,   12,   13,    9,   51,   52,
 /*    50 */    53,   54,   55,   56,   57,   58,   59,   60,   40,   41,
 /*    60 */    11,   30,   13,   26,   15,    4,   17,   30,   19,   51,
 /*    70 */    52,   53,   54,   55,   56,   57,   58,   59,   60,   30,
 /*    80 */    47,   50,    4,   46,   12,   13,   25,   50,   39,   15,
 /*    90 */    18,   42,   43,   44,   45,   40,   41,   23,   49,   25,
 /*   100 */    21,   27,   28,   62,   25,   27,   51,   52,   53,   54,
 /*   110 */    55,   56,   57,   58,   59,   60,    3,   17,   75,   47,
 /*   120 */    20,   25,   83,   73,   74,   29,   76,   14,   28,   86,
 /*   130 */    73,   81,   82,   76,   84,   85,   97,   87,   88,   26,
 /*   140 */    90,    3,   99,   30,    3,   95,   15,   11,   91,   13,
 /*   150 */   100,   15,   95,   17,   23,   19,    4,   11,   19,   13,
 /*   160 */    29,   15,  105,   17,   18,   19,   30,   26,   30,   30,
 /*   170 */    94,   30,   96,   79,   98,   39,   30,   25,   42,   43,
 /*   180 */    44,   45,   75,   25,   73,   49,   79,   76,   42,   43,
 /*   190 */    44,   45,   11,   86,   13,   49,   15,    1,   17,    3,
 /*   200 */    19,   11,    3,   13,    4,   15,   95,   17,   94,   19,
 /*   210 */    96,   30,   98,    1,    2,    3,  105,    5,    6,    7,
 /*   220 */    30,   77,   10,   42,   43,   44,   45,   27,   73,   30,
 /*   230 */    49,   76,   42,   43,   44,   45,   11,   16,   13,   49,
 /*   240 */    15,   97,   17,    4,   19,   15,   25,   80,   18,   50,
 /*   250 */    95,    4,   11,   23,   13,   30,   15,    1,   17,    3,
 /*   260 */    19,   18,   66,   80,   21,    4,   27,   42,   43,   44,
 /*   270 */    45,   30,   25,   61,   49,   63,   64,   65,   21,   17,
 /*   280 */    72,   24,   20,   42,   43,   44,   45,   11,   27,   13,
 /*   290 */    49,   15,    1,   17,    3,   19,    1,   22,    3,   43,
 /*   300 */    73,   74,   27,   76,   70,   71,   30,   25,   81,   27,
 /*   310 */    79,   84,   85,   73,  106,   88,   76,   90,   42,   43,
 /*   320 */    44,   45,   95,   72,   84,   49,   19,  100,   73,   74,
 /*   330 */    90,   76,  101,   16,   43,   95,   81,   30,   78,   84,
 /*   340 */    85,   73,   74,   88,   76,   90,    1,   94,    3,   81,
 /*   350 */    95,   98,   84,   85,   30,  100,   88,  106,   90,   73,
 /*   360 */    74,   15,   76,   95,   19,   27,   28,   81,  100,   23,
 /*   370 */    84,   85,   26,   75,   88,   15,   90,   73,   74,    4,
 /*   380 */    76,   95,   99,   22,   86,   81,  100,   42,   84,   85,
 /*   390 */    78,   77,   88,   48,   90,   73,   74,   94,   76,   95,
 /*   400 */    77,   98,   27,   81,  100,   72,   84,   85,   30,   97,
 /*   410 */    88,   97,   90,   73,   74,   26,   76,   95,   30,   98,
 /*   420 */    97,   81,  100,   72,   84,   85,   73,   74,   88,   76,
 /*   430 */    90,    1,   30,    3,   81,   95,   19,   84,   85,  106,
 /*   440 */   100,   88,   77,   90,   73,   74,    4,   76,   95,   19,
 /*   450 */     4,   30,   75,  100,    4,   84,   85,  106,    4,   88,
 /*   460 */   102,   90,   97,   86,   30,   30,   95,   48,   73,   74,
 /*   470 */    73,   76,   42,   76,  103,  104,   81,    8,   48,   84,
 /*   480 */    85,   84,   85,   88,   19,   90,   30,   90,   16,   16,
 /*   490 */    95,  106,   95,   49,   26,  100,   89,   25,   97,   28,
 /*   500 */    86,   92,   96,   31,   32,   33,   34,   35,   36,   37,
 /*   510 */    38,   73,   74,   27,   76,   15,   92,   78,   30,   81,
 /*   520 */    19,   11,   84,   85,   87,   15,   88,   17,   90,   19,
 /*   530 */   107,   11,  107,   95,  107,   15,  107,  107,  100,   19,
 /*   540 */    30,  107,  107,  107,  107,  107,  107,  107,  107,  107,
 /*   550 */    30,  107,   42,   43,   44,   45,  107,  107,  107,   49,
 /*   560 */   107,  107,   42,   43,   44,   45,  107,  107,  107,   49,
 /*   570 */   107,   73,   74,  107,   76,  107,  107,  107,  107,  107,
 /*   580 */   107,  107,   84,   85,   73,   74,   88,   76,   90,  107,
 /*   590 */   107,   93,  107,   95,  107,   84,   85,   73,   74,   88,
 /*   600 */    76,   90,  107,  107,  107,  107,   95,  107,   84,   85,
 /*   610 */   107,  107,   88,  107,   90,  104,  107,   93,  107,   95,
 /*   620 */   107,  107,  107,  107,   73,   74,  107,   76,  107,  107,
 /*   630 */   107,  107,  107,  107,  107,   84,   85,  107,  107,   88,
 /*   640 */   107,   90,  107,   25,   93,  107,   95,  107,  107,   31,
 /*   650 */    32,   33,   34,   35,   36,   37,   38,   73,   74,  107,
 /*   660 */    76,  107,  107,  107,  107,  107,  107,  107,   84,   85,
 /*   670 */    73,   74,   88,   76,   90,  107,  107,  107,  107,   95,
 /*   680 */   107,   84,   85,   73,   74,   88,   76,   90,  107,  107,
 /*   690 */   107,  107,   95,  107,   84,   85,  107,  107,   88,  107,
 /*   700 */    90,   73,   74,  107,   76,   95,  107,  107,  107,  107,
 /*   710 */   107,  107,   84,   85,  107,  107,   88,  107,   90,  107,
 /*   720 */   107,  107,  107,   95,   73,   74,  107,   76,  107,  107,
 /*   730 */   107,  107,  107,  107,  107,   84,   85,  107,  107,   88,
 /*   740 */   107,   90,   73,   74,  107,   76,   95,  107,  107,  107,
 /*   750 */   107,  107,  107,   84,   85,   73,   74,   88,   76,   90,
 /*   760 */   107,  107,  107,  107,   95,  107,   84,   85,   73,   74,
 /*   770 */    88,   76,   90,  107,  107,  107,  107,   95,  107,   84,
 /*   780 */    85,  107,  107,   88,  107,   90,   73,   74,  107,   76,
 /*   790 */    95,  107,  107,  107,  107,  107,  107,   84,   85,   73,
 /*   800 */    74,   88,   76,   90,  107,  107,  107,  107,   95,  107,
 /*   810 */    84,   85,  107,  107,   88,  107,   90,   73,   74,  107,
 /*   820 */    76,   95,  107,  107,  107,  107,  107,  107,   84,   85,
 /*   830 */    73,  107,   88,   76,   90,  107,  107,  107,  107,   95,
 /*   840 */   107,   84,   85,   73,  107,   88,   76,   90,  107,  107,
 /*   850 */   107,  107,   95,  107,   84,   85,  107,  107,   88,   73,
 /*   860 */    90,   73,   76,  107,   76,   95,  107,  107,  107,  107,
 /*   870 */    84,   85,   84,  107,   88,  107,   90,   73,   90,  107,
 /*   880 */    76,   95,  107,   95,  107,  107,  107,  107,   84,  107,
 /*   890 */   107,  107,  107,  107,   90,  107,  107,  107,  107,   95,
);
    const YY_SHIFT_USE_DFLT = -25;
    const YY_SHIFT_MAX = 145;
    static public $yy_shift_ofst = array(
 /*     0 */   212,  136,   49,   49,   49,   49,   49,   49,   49,   49,
 /*    10 */    49,   49,  276,  181,  276,  181,  181,  181,  181,  181,
 /*    20 */   181,  225,  181,  181,  181,  181,  181,  181,  181,  146,
 /*    30 */   241,  190,  510,  520,  520,  520,  430,  345,   74,  100,
 /*    40 */   100,  307,  282,  275,  465,  212,  472,  618,   37,  113,
 /*    50 */   256,  199,  138,  338,  138,  295,  138,  295,  138,  295,
 /*    60 */   471,  486,  471,   18,  -24,   -3,   55,   55,   55,   55,
 /*    70 */    55,   55,   55,   55,   72,   38,   33,  141,   41,   33,
 /*    80 */   196,   33,  291,  247,  262,  239,  152,  261,  139,   61,
 /*    90 */   221,  200,  375,   78,   96,   79,  488,  500,  158,  471,
 /*   100 */   471,  471,  501,  471,  158,  158,  360,  361,  158,  158,
 /*   110 */   158,  -25,  -25,  346,  230,  131,   -4,  257,   -4,   -4,
 /*   120 */   243,   31,   -4,  417,  450,  421,  468,  446,  442,  389,
 /*   130 */   388,  402,  454,  434,  473,  360,  456,  469,  435,  378,
 /*   140 */   419,  444,  324,  -22,  -14,  317,
);
    const YY_REDUCE_USE_DFLT = -75;
    const YY_REDUCE_MAX = 112;
    static public $yy_reduce_ofst = array(
 /*     0 */   -61,   50,  286,  304,  322,  268,  353,  255,  227,  340,
 /*    10 */   395,  438,  371,  498,  511,  524,  -70,  551,  669,  651,
 /*    20 */   584,  628,  597,  610,  682,  695,  744,  713,  726,  770,
 /*    30 */   757,  786,  397,  240,  804,  788,   57,  111,  -74,  114,
 /*    40 */    76,  155,  107,   43,  -52,  234,  231,  231,  312,   39,
 /*    50 */   333,  312,  323,  298,  314,  208,  365,  251,  144,  351,
 /*    60 */   253,  377,  303,  358,  358,  358,  358,  358,  358,  358,
 /*    70 */   358,  358,  358,  358,  407,  385,  407,  401,  385,  407,
 /*    80 */   385,  407,  385,   94,  406,  414,   94,  414,  409,   94,
 /*    90 */    94,  414,  414,  414,   94,   94,  424,  439,   94,  321,
 /*   100 */   321,  321,  437,  321,   94,   94,  260,  283,   94,   94,
 /*   110 */    94,  167,  183,
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
        /* 29 */ array(11, 13, 15, 17, 18, 19, 30, 42, 43, 44, 45, 49, ),
        /* 30 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 31 */ array(11, 13, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 32 */ array(11, 15, 17, 19, 30, 42, 43, 44, 45, 49, ),
        /* 33 */ array(11, 15, 19, 30, 42, 43, 44, 45, 49, ),
        /* 34 */ array(11, 15, 19, 30, 42, 43, 44, 45, 49, ),
        /* 35 */ array(11, 15, 19, 30, 42, 43, 44, 45, 49, ),
        /* 36 */ array(1, 3, 19, 42, 48, ),
        /* 37 */ array(1, 3, 19, 42, 48, ),
        /* 38 */ array(15, 23, 25, 27, 28, ),
        /* 39 */ array(17, 20, 28, ),
        /* 40 */ array(17, 20, 28, ),
        /* 41 */ array(19, 30, ),
        /* 42 */ array(25, 27, ),
        /* 43 */ array(22, 27, ),
        /* 44 */ array(19, ),
        /* 45 */ array(1, 2, 3, 5, 6, 7, 10, 61, 63, 64, 65, ),
        /* 46 */ array(16, 25, 31, 32, 33, 34, 35, 36, 37, 38, ),
        /* 47 */ array(25, 31, 32, 33, 34, 35, 36, 37, 38, ),
        /* 48 */ array(3, 26, 30, 46, 50, ),
        /* 49 */ array(3, 14, 26, 30, ),
        /* 50 */ array(1, 3, 43, ),
        /* 51 */ array(3, 30, 50, ),
        /* 52 */ array(3, 30, ),
        /* 53 */ array(27, 28, ),
        /* 54 */ array(3, 30, ),
        /* 55 */ array(1, 3, ),
        /* 56 */ array(3, 30, ),
        /* 57 */ array(1, 3, ),
        /* 58 */ array(3, 30, ),
        /* 59 */ array(1, 3, ),
        /* 60 */ array(28, ),
        /* 61 */ array(27, ),
        /* 62 */ array(28, ),
        /* 63 */ array(4, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 64 */ array(24, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 65 */ array(16, 40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 66 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 67 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 68 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 69 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 70 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 71 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 72 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 73 */ array(40, 41, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, ),
        /* 74 */ array(12, 13, 18, 47, ),
        /* 75 */ array(1, 3, 9, ),
        /* 76 */ array(12, 13, 47, ),
        /* 77 */ array(3, 26, 30, ),
        /* 78 */ array(1, 3, 62, ),
        /* 79 */ array(12, 13, 47, ),
        /* 80 */ array(1, 3, 66, ),
        /* 81 */ array(12, 13, 47, ),
        /* 82 */ array(1, 3, 43, ),
        /* 83 */ array(4, 25, ),
        /* 84 */ array(17, 20, ),
        /* 85 */ array(4, 27, ),
        /* 86 */ array(4, 25, ),
        /* 87 */ array(4, 27, ),
        /* 88 */ array(19, 30, ),
        /* 89 */ array(4, 25, ),
        /* 90 */ array(16, 25, ),
        /* 91 */ array(4, 27, ),
        /* 92 */ array(4, 27, ),
        /* 93 */ array(4, 27, ),
        /* 94 */ array(25, 29, ),
        /* 95 */ array(21, 25, ),
        /* 96 */ array(30, ),
        /* 97 */ array(15, ),
        /* 98 */ array(25, ),
        /* 99 */ array(28, ),
        /* 100 */ array(28, ),
        /* 101 */ array(28, ),
        /* 102 */ array(19, ),
        /* 103 */ array(28, ),
        /* 104 */ array(25, ),
        /* 105 */ array(25, ),
        /* 106 */ array(15, ),
        /* 107 */ array(22, ),
        /* 108 */ array(25, ),
        /* 109 */ array(25, ),
        /* 110 */ array(25, ),
        /* 111 */ array(),
        /* 112 */ array(),
        /* 113 */ array(15, 23, 26, ),
        /* 114 */ array(15, 18, 23, ),
        /* 115 */ array(15, 23, 29, ),
        /* 116 */ array(15, 23, ),
        /* 117 */ array(21, 24, ),
        /* 118 */ array(15, 23, ),
        /* 119 */ array(15, 23, ),
        /* 120 */ array(18, 21, ),
        /* 121 */ array(30, 50, ),
        /* 122 */ array(15, 23, ),
        /* 123 */ array(19, ),
        /* 124 */ array(4, ),
        /* 125 */ array(30, ),
        /* 126 */ array(26, ),
        /* 127 */ array(4, ),
        /* 128 */ array(4, ),
        /* 129 */ array(26, ),
        /* 130 */ array(30, ),
        /* 131 */ array(30, ),
        /* 132 */ array(4, ),
        /* 133 */ array(30, ),
        /* 134 */ array(16, ),
        /* 135 */ array(15, ),
        /* 136 */ array(30, ),
        /* 137 */ array(8, ),
        /* 138 */ array(30, ),
        /* 139 */ array(30, ),
        /* 140 */ array(48, ),
        /* 141 */ array(49, ),
        /* 142 */ array(30, ),
        /* 143 */ array(48, ),
        /* 144 */ array(16, ),
        /* 145 */ array(16, ),
        /* 146 */ array(),
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
 /*     0 */   378,  378,  378,  378,  378,  378,  378,  378,  378,  378,
 /*    10 */   378,  378,  363,  327,  378,  327,  327,  327,  378,  378,
 /*    20 */   378,  378,  378,  378,  378,  378,  378,  378,  378,  378,
 /*    30 */   378,  378,  378,  378,  378,  378,  378,  378,  271,  302,
 /*    40 */   257,  378,  271,  271,  378,  242,  337,  337,  308,  378,
 /*    50 */   378,  308,  378,  271,  378,  378,  378,  378,  378,  378,
 /*    60 */   297,  271,  298,  378,  378,  378,  349,  339,  345,  348,
 /*    70 */   340,  344,  335,  341,  378,  378,  310,  378,  378,  332,
 /*    80 */   378,  277,  378,  378,  321,  378,  378,  378,  378,  378,
 /*    90 */   378,  378,  378,  378,  364,  326,  378,  308,  272,  299,
 /*   100 */   300,  318,  378,  303,  365,  366,  308,  278,  338,  275,
 /*   110 */   265,  331,  331,  276,  378,  276,  276,  378,  378,  333,
 /*   120 */   378,  378,  309,  378,  378,  378,  304,  378,  378,  378,
 /*   130 */   378,  378,  378,  378,  378,  301,  378,  378,  378,  378,
 /*   140 */   378,  378,  378,  378,  378,  378,  289,  347,  287,  359,
 /*   150 */   358,  346,  288,  320,  295,  294,  304,  306,  367,  293,
 /*   160 */   342,  291,  336,  282,  343,  290,  280,  376,  374,  273,
 /*   170 */   377,  375,  246,  261,  243,  262,  244,  245,  247,  274,
 /*   180 */   307,  252,  279,  334,  281,  251,  264,  248,  266,  249,
 /*   190 */   250,  286,  355,  256,  312,  311,  313,  270,  368,  258,
 /*   200 */   259,  372,  373,  296,  324,  322,  314,  317,  305,  315,
 /*   210 */   253,  254,  319,  269,  255,  292,  323,  325,  350,  351,
 /*   220 */   370,  371,  283,  352,  353,  357,  356,  316,  354,  263,
 /*   230 */   330,  360,  362,  361,  260,  329,  328,  284,  285,  268,
 /*   240 */   267,  369,
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
    const YYNSTATE = 242;
    const YYNRULE = 136;
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
 /*  68 */ "indexdef ::= DOT exprs",
 /*  69 */ "indexdef ::= OPENB ID CLOSEB",
 /*  70 */ "indexdef ::= OPENB exprs CLOSEB",
 /*  71 */ "indexdef ::= OPENB CLOSEB",
 /*  72 */ "varvar ::= varvarele",
 /*  73 */ "varvar ::= varvar varvarele",
 /*  74 */ "varvarele ::= ID",
 /*  75 */ "varvarele ::= LDEL expr RDEL",
 /*  76 */ "object ::= DOLLAR varvar arrayindex objectchain",
 /*  77 */ "objectchain ::= objectelement",
 /*  78 */ "objectchain ::= objectchain objectelement",
 /*  79 */ "objectelement ::= PTR ID arrayindex",
 /*  80 */ "objectelement ::= PTR method",
 /*  81 */ "function ::= ID OPENP params CLOSEP",
 /*  82 */ "method ::= ID OPENP params CLOSEP",
 /*  83 */ "params ::= expr COMMA params",
 /*  84 */ "params ::= expr",
 /*  85 */ "params ::=",
 /*  86 */ "modifier ::= VERT AT ID",
 /*  87 */ "modifier ::= VERT ID",
 /*  88 */ "modparameters ::= modparameters modparameter",
 /*  89 */ "modparameters ::=",
 /*  90 */ "modparameter ::= COLON exprs",
 /*  91 */ "modparameter ::= COLON ID",
 /*  92 */ "ifexprs ::= ifexpr",
 /*  93 */ "ifexprs ::= NOT ifexprs",
 /*  94 */ "ifexprs ::= OPENP ifexprs CLOSEP",
 /*  95 */ "ifexpr ::= expr",
 /*  96 */ "ifexpr ::= expr ifcond expr",
 /*  97 */ "ifexpr ::= ifexprs lop ifexprs",
 /*  98 */ "ifexpr ::= ifexprs ISDIVBY ifexprs",
 /*  99 */ "ifexpr ::= ifexprs ISNOTDIVBY ifexprs",
 /* 100 */ "ifexpr ::= ifexprs ISEVEN",
 /* 101 */ "ifexpr ::= ifexprs ISNOTEVEN",
 /* 102 */ "ifexpr ::= ifexprs ISEVENBY ifexprs",
 /* 103 */ "ifexpr ::= ifexprs ISNOTEVENBY ifexprs",
 /* 104 */ "ifexpr ::= ifexprs ISODD",
 /* 105 */ "ifexpr ::= ifexprs ISNOTODD",
 /* 106 */ "ifexpr ::= ifexprs ISODDBY ifexprs",
 /* 107 */ "ifexpr ::= ifexprs ISNOTODDBY ifexprs",
 /* 108 */ "ifcond ::= EQUALS",
 /* 109 */ "ifcond ::= NOTEQUALS",
 /* 110 */ "ifcond ::= GREATERTHAN",
 /* 111 */ "ifcond ::= LESSTHAN",
 /* 112 */ "ifcond ::= GREATEREQUAL",
 /* 113 */ "ifcond ::= LESSEQUAL",
 /* 114 */ "ifcond ::= IDENTITY",
 /* 115 */ "ifcond ::= NONEIDENTITY",
 /* 116 */ "lop ::= LAND",
 /* 117 */ "lop ::= LOR",
 /* 118 */ "array ::= OPENB arrayelements CLOSEB",
 /* 119 */ "arrayelements ::= arrayelement",
 /* 120 */ "arrayelements ::= arrayelements COMMA arrayelement",
 /* 121 */ "arrayelements ::=",
 /* 122 */ "arrayelement ::= expr",
 /* 123 */ "arrayelement ::= expr APTR expr",
 /* 124 */ "arrayelement ::= ID APTR expr",
 /* 125 */ "doublequoted ::= doublequoted doublequotedcontent",
 /* 126 */ "doublequoted ::= doublequotedcontent",
 /* 127 */ "doublequotedcontent ::= variable",
 /* 128 */ "doublequotedcontent ::= BACKTICK ID BACKTICK",
 /* 129 */ "doublequotedcontent ::= BACKTICK variable BACKTICK",
 /* 130 */ "doublequotedcontent ::= LDEL expr RDEL",
 /* 131 */ "doublequotedcontent ::= OTHER",
 /* 132 */ "text ::= text textelement",
 /* 133 */ "text ::= textelement",
 /* 134 */ "textelement ::= OTHER",
 /* 135 */ "textelement ::= LDEL",
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
  array( 'lhs' => 96, 'rhs' => 2 ),
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
        119 => 0,
        1 => 1,
        35 => 1,
        37 => 1,
        42 => 1,
        43 => 1,
        72 => 1,
        92 => 1,
        126 => 1,
        133 => 1,
        134 => 1,
        135 => 1,
        2 => 2,
        65 => 2,
        125 => 2,
        132 => 2,
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
        84 => 24,
        122 => 24,
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
        89 => 66,
        67 => 67,
        68 => 68,
        69 => 69,
        70 => 70,
        71 => 71,
        73 => 73,
        74 => 74,
        75 => 75,
        94 => 75,
        76 => 76,
        77 => 77,
        78 => 78,
        79 => 79,
        80 => 80,
        81 => 81,
        82 => 82,
        83 => 83,
        85 => 85,
        86 => 86,
        87 => 87,
        88 => 88,
        90 => 90,
        91 => 91,
        93 => 93,
        95 => 95,
        96 => 96,
        97 => 96,
        98 => 98,
        99 => 99,
        100 => 100,
        105 => 100,
        101 => 101,
        104 => 101,
        102 => 102,
        107 => 102,
        103 => 103,
        106 => 103,
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
        118 => 118,
        120 => 120,
        121 => 121,
        123 => 123,
        124 => 124,
        127 => 127,
        128 => 128,
        129 => 129,
        130 => 130,
        131 => 131,
    );
    /* Beginning here are the reduction cases.  A typical example
    ** follows:
    **  #line <lineno> <grammarfile>
    **   function yy_r0($yymsp){ ... }           // User supplied code
    **  #line <lineno> <thisfile>
    */
#line 73 "internal.templateparser.y"
    function yy_r0(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;     }
#line 1758 "internal.templateparser.php"
#line 79 "internal.templateparser.y"
    function yy_r1(){$this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1761 "internal.templateparser.php"
#line 81 "internal.templateparser.y"
    function yy_r2(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1764 "internal.templateparser.php"
#line 87 "internal.templateparser.y"
    function yy_r3(){if ($this->compiler->has_code) {
                                            $tmp =''; foreach ($this->prefix_code as $code) {$tmp.=$code;} $this->prefix_code=array();
                                            $this->_retvalue = $this->cacher->processNocacheCode($tmp.$this->yystack[$this->yyidx + 0]->minor, $this->compiler,$this->nocache,true);
                                         } $this->nocache=false;    }
#line 1770 "internal.templateparser.php"
#line 100 "internal.templateparser.y"
    function yy_r4(){ $this->_retvalue = '';    }
#line 1773 "internal.templateparser.php"
#line 103 "internal.templateparser.y"
    function yy_r5(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + -1]->minor, $this->compiler,false,false);    }
#line 1776 "internal.templateparser.php"
#line 105 "internal.templateparser.y"
    function yy_r6(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->left_delimiter, $this->compiler,false,false);    }
#line 1779 "internal.templateparser.php"
#line 107 "internal.templateparser.y"
    function yy_r7(){$this->_retvalue = $this->cacher->processNocacheCode($this->smarty->right_delimiter, $this->compiler,false,false);    }
#line 1782 "internal.templateparser.php"
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
#line 1793 "internal.templateparser.php"
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
#line 1804 "internal.templateparser.php"
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
#line 1815 "internal.templateparser.php"
#line 139 "internal.templateparser.y"
    function yy_r11(){$this->_retvalue = $this->cacher->processNocacheCode("<?php echo '".$this->yystack[$this->yyidx + 0]->minor."';?>\n", $this->compiler, true, true);    }
#line 1818 "internal.templateparser.php"
#line 141 "internal.templateparser.y"
    function yy_r12(){$this->_retvalue = $this->cacher->processNocacheCode($this->yystack[$this->yyidx + 0]->minor, $this->compiler,false,false);    }
#line 1821 "internal.templateparser.php"
#line 149 "internal.templateparser.y"
    function yy_r13(){ $this->_retvalue = $this->compiler->compileTag('print_expression',array_merge(array('value'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1824 "internal.templateparser.php"
#line 152 "internal.templateparser.y"
    function yy_r14(){ $this->_retvalue = $this->compiler->compileTag('assign',array_merge(array('value'=>$this->yystack[$this->yyidx + -1]->minor),$this->yystack[$this->yyidx + -3]->minor));    }
#line 1827 "internal.templateparser.php"
#line 154 "internal.templateparser.y"
    function yy_r15(){$this->_retvalue = array('var'=>$this->yystack[$this->yyidx + -1]->minor, 'index'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1830 "internal.templateparser.php"
#line 158 "internal.templateparser.y"
    function yy_r16(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor,$this->yystack[$this->yyidx + -1]->minor);    }
#line 1833 "internal.templateparser.php"
#line 160 "internal.templateparser.y"
    function yy_r17(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -4]->minor,array_merge(array('object_methode'=>$this->yystack[$this->yyidx + -2]->minor),$this->yystack[$this->yyidx + -1]->minor));    }
#line 1836 "internal.templateparser.php"
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
#line 1851 "internal.templateparser.php"
#line 176 "internal.templateparser.y"
    function yy_r19(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -2]->minor.'close',$this->yystack[$this->yyidx + -1]->minor);    }
#line 1854 "internal.templateparser.php"
#line 178 "internal.templateparser.y"
    function yy_r20(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor.'close',array('object_methode'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1857 "internal.templateparser.php"
#line 180 "internal.templateparser.y"
    function yy_r21(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -3]->minor,array('if condition'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1860 "internal.templateparser.php"
#line 182 "internal.templateparser.y"
    function yy_r22(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -9]->minor,array('start'=>$this->yystack[$this->yyidx + -7]->minor,'ifexp'=>$this->yystack[$this->yyidx + -5]->minor,'varloop'=>$this->yystack[$this->yyidx + -2]->minor,'loop'=>$this->yystack[$this->yyidx + -1]->minor));    }
#line 1863 "internal.templateparser.php"
#line 183 "internal.templateparser.y"
    function yy_r23(){ $this->_retvalue = '='.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1866 "internal.templateparser.php"
#line 184 "internal.templateparser.y"
    function yy_r24(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 1869 "internal.templateparser.php"
#line 187 "internal.templateparser.y"
    function yy_r25(){ $this->_retvalue =  $this->compiler->compileTag($this->yystack[$this->yyidx + -6]->minor,array('from'=>$this->yystack[$this->yyidx + -1]->minor,'item'=>$this->yystack[$this->yyidx + -3]->minor));    }
#line 1872 "internal.templateparser.php"
#line 194 "internal.templateparser.y"
    function yy_r27(){ $this->_retvalue = array_merge($this->yystack[$this->yyidx + -1]->minor,$this->yystack[$this->yyidx + 0]->minor);    }
#line 1875 "internal.templateparser.php"
#line 198 "internal.templateparser.y"
    function yy_r29(){ $this->_retvalue = array();    }
#line 1878 "internal.templateparser.php"
#line 202 "internal.templateparser.y"
    function yy_r30(){ $this->_retvalue = array($this->yystack[$this->yyidx + -2]->minor=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1881 "internal.templateparser.php"
#line 207 "internal.templateparser.y"
    function yy_r31(){ $this->_retvalue = array($this->yystack[$this->yyidx + 0]->minor);    }
#line 1884 "internal.templateparser.php"
#line 208 "internal.templateparser.y"
    function yy_r32(){ $this->yystack[$this->yyidx + -2]->minor[]=$this->yystack[$this->yyidx + 0]->minor; $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor;    }
#line 1887 "internal.templateparser.php"
#line 210 "internal.templateparser.y"
    function yy_r33(){ $this->_retvalue = array('var' => $this->yystack[$this->yyidx + -2]->minor, 'value'=>$this->yystack[$this->yyidx + 0]->minor);    }
#line 1890 "internal.templateparser.php"
#line 217 "internal.templateparser.y"
    function yy_r34(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';     }
#line 1893 "internal.templateparser.php"
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
#line 1908 "internal.templateparser.php"
#line 239 "internal.templateparser.y"
    function yy_r39(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1911 "internal.templateparser.php"
#line 241 "internal.templateparser.y"
    function yy_r40(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor . $this->yystack[$this->yyidx + -1]->minor . $this->yystack[$this->yyidx + 0]->minor;     }
#line 1914 "internal.templateparser.php"
#line 243 "internal.templateparser.y"
    function yy_r41(){ $this->_retvalue = '('. $this->yystack[$this->yyidx + -2]->minor . ').(' . $this->yystack[$this->yyidx + 0]->minor. ')';     }
#line 1917 "internal.templateparser.php"
#line 257 "internal.templateparser.y"
    function yy_r45(){$this->_retvalue = '$_smarty_tpl->getConfigVariable(\''. $this->yystack[$this->yyidx + -1]->minor .'\')';    }
#line 1920 "internal.templateparser.php"
#line 268 "internal.templateparser.y"
    function yy_r50(){ $this->_retvalue = "(". $this->yystack[$this->yyidx + -1]->minor .")";     }
#line 1923 "internal.templateparser.php"
#line 271 "internal.templateparser.y"
    function yy_r51(){ $this->_retvalue = "'".$this->yystack[$this->yyidx + -1]->minor."'";     }
#line 1926 "internal.templateparser.php"
#line 272 "internal.templateparser.y"
    function yy_r52(){ $this->_retvalue = "''";     }
#line 1929 "internal.templateparser.php"
#line 274 "internal.templateparser.y"
    function yy_r53(){ $this->_retvalue = "'".str_replace('\"','"',$this->yystack[$this->yyidx + -1]->minor)."'";     }
#line 1932 "internal.templateparser.php"
#line 280 "internal.templateparser.y"
    function yy_r55(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1935 "internal.templateparser.php"
#line 281 "internal.templateparser.y"
    function yy_r56(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -3]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -6]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -1]->minor .')';     }
#line 1938 "internal.templateparser.php"
#line 283 "internal.templateparser.y"
    function yy_r57(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor.'::'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1941 "internal.templateparser.php"
#line 284 "internal.templateparser.y"
    function yy_r58(){ $this->prefix_number++; $this->prefix_code[] = '<?php $_tmp'.$this->prefix_number.'=$_smarty_tpl->getVariable(\''. $this->yystack[$this->yyidx + -4]->minor .'\')->value;?>'; $this->_retvalue = $this->yystack[$this->yyidx + -7]->minor.'::$_tmp'.$this->prefix_number.'('. $this->yystack[$this->yyidx + -2]->minor .')'.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1944 "internal.templateparser.php"
#line 286 "internal.templateparser.y"
    function yy_r59(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'::'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1947 "internal.templateparser.php"
#line 288 "internal.templateparser.y"
    function yy_r60(){ $this->_retvalue = $this->yystack[$this->yyidx + -4]->minor.'::$'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1950 "internal.templateparser.php"
#line 290 "internal.templateparser.y"
    function yy_r61(){ $this->_retvalue = $this->yystack[$this->yyidx + -5]->minor.'::$'.$this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1953 "internal.templateparser.php"
#line 299 "internal.templateparser.y"
    function yy_r62(){ if ($this->yystack[$this->yyidx + 0]->minor['var'] == '\'smarty\'') { $this->_retvalue =  $this->compiler->compileTag(trim($this->yystack[$this->yyidx + 0]->minor['var'],"'"),$this->yystack[$this->yyidx + 0]->minor['index']);} else {
                                                         $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + 0]->minor['var'] .')->value'.$this->yystack[$this->yyidx + 0]->minor['index']; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + 0]->minor['var'],"'"))->nocache;}    }
#line 1957 "internal.templateparser.php"
#line 302 "internal.templateparser.y"
    function yy_r63(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->'.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 1960 "internal.templateparser.php"
#line 314 "internal.templateparser.y"
    function yy_r66(){return;    }
#line 1963 "internal.templateparser.php"
#line 318 "internal.templateparser.y"
    function yy_r67(){ $this->_retvalue = "['". $this->yystack[$this->yyidx + 0]->minor ."']";    }
#line 1966 "internal.templateparser.php"
#line 319 "internal.templateparser.y"
    function yy_r68(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + 0]->minor ."]";    }
#line 1969 "internal.templateparser.php"
#line 321 "internal.templateparser.y"
    function yy_r69(){ $this->_retvalue = '['.$this->compiler->compileTag('smarty','[\'section\'][\''.$this->yystack[$this->yyidx + -1]->minor.'\'][\'index\']').']';    }
#line 1972 "internal.templateparser.php"
#line 323 "internal.templateparser.y"
    function yy_r70(){ $this->_retvalue = "[". $this->yystack[$this->yyidx + -1]->minor ."]";    }
#line 1975 "internal.templateparser.php"
#line 325 "internal.templateparser.y"
    function yy_r71(){$this->_retvalue = '';    }
#line 1978 "internal.templateparser.php"
#line 333 "internal.templateparser.y"
    function yy_r73(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.'.'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1981 "internal.templateparser.php"
#line 335 "internal.templateparser.y"
    function yy_r74(){$this->_retvalue = '\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 1984 "internal.templateparser.php"
#line 337 "internal.templateparser.y"
    function yy_r75(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 1987 "internal.templateparser.php"
#line 342 "internal.templateparser.y"
    function yy_r76(){ $this->_retvalue = '$_smarty_tpl->getVariable('. $this->yystack[$this->yyidx + -2]->minor .')->value'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor; $this->nocache=$this->template->getVariable(trim($this->yystack[$this->yyidx + -2]->minor,"'"))->nocache;    }
#line 1990 "internal.templateparser.php"
#line 344 "internal.templateparser.y"
    function yy_r77(){$this->_retvalue  = $this->yystack[$this->yyidx + 0]->minor;     }
#line 1993 "internal.templateparser.php"
#line 346 "internal.templateparser.y"
    function yy_r78(){$this->_retvalue  = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;     }
#line 1996 "internal.templateparser.php"
#line 348 "internal.templateparser.y"
    function yy_r79(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 1999 "internal.templateparser.php"
#line 351 "internal.templateparser.y"
    function yy_r80(){ $this->_retvalue = '->'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2002 "internal.templateparser.php"
#line 357 "internal.templateparser.y"
    function yy_r81(){if (!$this->template->security || $this->smarty->security_handler->isTrustedPhpFunction($this->yystack[$this->yyidx + -3]->minor, $this->compiler)) {
																					            if ($this->yystack[$this->yyidx + -3]->minor == 'isset' || $this->yystack[$this->yyidx + -3]->minor == 'empty' || $this->yystack[$this->yyidx + -3]->minor == 'array' || is_callable($this->yystack[$this->yyidx + -3]->minor)) {
																					                $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";
																					            } else {
                                                       $this->compiler->trigger_template_error ("unknown function \"" . $this->yystack[$this->yyidx + -3]->minor . "\"");
                                                      }
                                                    }    }
#line 2011 "internal.templateparser.php"
#line 368 "internal.templateparser.y"
    function yy_r82(){ $this->_retvalue = $this->yystack[$this->yyidx + -3]->minor . "(". $this->yystack[$this->yyidx + -1]->minor .")";    }
#line 2014 "internal.templateparser.php"
#line 372 "internal.templateparser.y"
    function yy_r83(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.",".$this->yystack[$this->yyidx + 0]->minor;    }
#line 2017 "internal.templateparser.php"
#line 376 "internal.templateparser.y"
    function yy_r85(){ return;    }
#line 2020 "internal.templateparser.php"
#line 381 "internal.templateparser.y"
    function yy_r86(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,true);    }
#line 2023 "internal.templateparser.php"
#line 382 "internal.templateparser.y"
    function yy_r87(){ $this->_retvalue =  array($this->yystack[$this->yyidx + 0]->minor,false);    }
#line 2026 "internal.templateparser.php"
#line 389 "internal.templateparser.y"
    function yy_r88(){ $this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2029 "internal.templateparser.php"
#line 393 "internal.templateparser.y"
    function yy_r90(){$this->_retvalue = ','.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2032 "internal.templateparser.php"
#line 394 "internal.templateparser.y"
    function yy_r91(){$this->_retvalue = ',\''.$this->yystack[$this->yyidx + 0]->minor.'\'';    }
#line 2035 "internal.templateparser.php"
#line 401 "internal.templateparser.y"
    function yy_r93(){$this->_retvalue = '!'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2038 "internal.templateparser.php"
#line 406 "internal.templateparser.y"
    function yy_r95(){$this->_retvalue =$this->yystack[$this->yyidx + 0]->minor;    }
#line 2041 "internal.templateparser.php"
#line 407 "internal.templateparser.y"
    function yy_r96(){$this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.$this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2044 "internal.templateparser.php"
#line 409 "internal.templateparser.y"
    function yy_r98(){$this->_retvalue = '!('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2047 "internal.templateparser.php"
#line 410 "internal.templateparser.y"
    function yy_r99(){$this->_retvalue = '('.$this->yystack[$this->yyidx + -2]->minor.' % '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2050 "internal.templateparser.php"
#line 411 "internal.templateparser.y"
    function yy_r100(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2053 "internal.templateparser.php"
#line 412 "internal.templateparser.y"
    function yy_r101(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2056 "internal.templateparser.php"
#line 413 "internal.templateparser.y"
    function yy_r102(){$this->_retvalue = '!(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2059 "internal.templateparser.php"
#line 414 "internal.templateparser.y"
    function yy_r103(){$this->_retvalue = '(1 & '.$this->yystack[$this->yyidx + -2]->minor.' / '.$this->yystack[$this->yyidx + 0]->minor.')';    }
#line 2062 "internal.templateparser.php"
#line 420 "internal.templateparser.y"
    function yy_r108(){$this->_retvalue = '==';    }
#line 2065 "internal.templateparser.php"
#line 421 "internal.templateparser.y"
    function yy_r109(){$this->_retvalue = '!=';    }
#line 2068 "internal.templateparser.php"
#line 422 "internal.templateparser.y"
    function yy_r110(){$this->_retvalue = '>';    }
#line 2071 "internal.templateparser.php"
#line 423 "internal.templateparser.y"
    function yy_r111(){$this->_retvalue = '<';    }
#line 2074 "internal.templateparser.php"
#line 424 "internal.templateparser.y"
    function yy_r112(){$this->_retvalue = '>=';    }
#line 2077 "internal.templateparser.php"
#line 425 "internal.templateparser.y"
    function yy_r113(){$this->_retvalue = '<=';    }
#line 2080 "internal.templateparser.php"
#line 426 "internal.templateparser.y"
    function yy_r114(){$this->_retvalue = '===';    }
#line 2083 "internal.templateparser.php"
#line 427 "internal.templateparser.y"
    function yy_r115(){$this->_retvalue = '!==';    }
#line 2086 "internal.templateparser.php"
#line 429 "internal.templateparser.y"
    function yy_r116(){$this->_retvalue = '&&';    }
#line 2089 "internal.templateparser.php"
#line 430 "internal.templateparser.y"
    function yy_r117(){$this->_retvalue = '||';    }
#line 2092 "internal.templateparser.php"
#line 435 "internal.templateparser.y"
    function yy_r118(){ $this->_retvalue = 'array('.$this->yystack[$this->yyidx + -1]->minor.')';    }
#line 2095 "internal.templateparser.php"
#line 437 "internal.templateparser.y"
    function yy_r120(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.','.$this->yystack[$this->yyidx + 0]->minor;     }
#line 2098 "internal.templateparser.php"
#line 438 "internal.templateparser.y"
    function yy_r121(){ return;     }
#line 2101 "internal.templateparser.php"
#line 440 "internal.templateparser.y"
    function yy_r123(){ $this->_retvalue = $this->yystack[$this->yyidx + -2]->minor.'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2104 "internal.templateparser.php"
#line 441 "internal.templateparser.y"
    function yy_r124(){ $this->_retvalue = '\''.$this->yystack[$this->yyidx + -2]->minor.'\'=>'.$this->yystack[$this->yyidx + 0]->minor;    }
#line 2107 "internal.templateparser.php"
#line 448 "internal.templateparser.y"
    function yy_r127(){$this->_retvalue = "'.".$this->yystack[$this->yyidx + 0]->minor.".'";    }
#line 2110 "internal.templateparser.php"
#line 449 "internal.templateparser.y"
    function yy_r128(){$this->_retvalue = "`".$this->yystack[$this->yyidx + -1]->minor."`";    }
#line 2113 "internal.templateparser.php"
#line 450 "internal.templateparser.y"
    function yy_r129(){$this->_retvalue = "'.".$this->yystack[$this->yyidx + -1]->minor.".'";    }
#line 2116 "internal.templateparser.php"
#line 451 "internal.templateparser.y"
    function yy_r130(){$this->_retvalue = "'.(".$this->yystack[$this->yyidx + -1]->minor.").'";    }
#line 2119 "internal.templateparser.php"
#line 452 "internal.templateparser.y"
    function yy_r131(){$this->_retvalue = addcslashes($this->yystack[$this->yyidx + 0]->minor,"'");    }
#line 2122 "internal.templateparser.php"

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
#line 2240 "internal.templateparser.php"
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
#line 2265 "internal.templateparser.php"
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
