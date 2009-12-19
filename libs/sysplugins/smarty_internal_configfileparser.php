<?php
/**
* Smarty Internal Plugin Configfileparser
*
* This is the config file parser.
* It is generated from the internal.configfileparser.y file
* @package Smarty
* @subpackage Compiler
* @author Uwe Tews
*/

class TPC_yyToken implements ArrayAccess
{
    public $string = '';
    public $metadata = array();

    function __construct($s, $m = array())
    {
        if ($s instanceof TPC_yyToken) {
            $this->string = $s->string;
            $this->metadata = $s->metadata;
        } else {
            $this->string = (string) $s;
            if ($m instanceof TPC_yyToken) {
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
                $x = ($value instanceof TPC_yyToken) ?
                    $value->metadata : $value;
                $this->metadata = array_merge($this->metadata, $x);
                return;
            }
            $offset = count($this->metadata);
        }
        if ($value === null) {
            return;
        }
        if ($value instanceof TPC_yyToken) {
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

class TPC_yyStackEntry
{
    public $stateno;       /* The state-number */
    public $major;         /* The major token value.  This is the code
                     ** number for the token at this stack level */
    public $minor; /* The user-supplied minor token value.  This
                     ** is the value of the token  */
};


#line 12 "smarty_internal_configfileparser.y"
class Smarty_Internal_Configfileparser#line 79 "smarty_internal_configfileparser.php"
{
#line 14 "smarty_internal_configfileparser.y"

    // states whether the parse was successful or not
    public $successful = true;
    public $retvalue = 0;
    private $lex;
    private $internalError = false;

    function __construct($lex, $compiler) {
        // set instance object
        self::instance($this); 
        $this->lex = $lex;
        $this->smarty = $compiler->smarty; 
        $this->compiler = $compiler;
        $this->current_section = null;
        $this->hidden_section = false;
    }
    public static function &instance($new_instance = null)
    {
        static $instance = null;
        if (isset($new_instance) && is_object($new_instance))
            $instance = $new_instance;
        return $instance;
    }
    
#line 107 "smarty_internal_configfileparser.php"

    const TPC_OTHER                          =  1;
    const TPC_OPENB                          =  2;
    const TPC_CLOSEB                         =  3;
    const TPC_DOT                            =  4;
    const TPC_BOOLEANTRUE                    =  5;
    const TPC_BOOLEANFALSE                   =  6;
    const TPC_SI_QSTR                        =  7;
    const TPC_DO_QSTR                        =  8;
    const TPC_EQUAL                          =  9;
    const TPC_SPACE                          = 10;
    const TPC_ID                             = 11;
    const TPC_EOL                            = 12;
    const TPC_COMMENTSTART                   = 13;
    const TPC_ML_QSTR                        = 14;
    const YY_NO_ACTION = 49;
    const YY_ACCEPT_ACTION = 48;
    const YY_ERROR_ACTION = 47;

    const YY_SZ_ACTTAB = 37;
static public $yy_action = array(
 /*     0 */     9,   21,   20,   18,    3,   14,   27,   26,   28,   12,
 /*    10 */    15,    2,   21,    6,   16,    7,    5,   14,   48,    4,
 /*    20 */    17,   11,   24,   23,    1,    8,   23,   10,   13,   21,
 /*    30 */    27,   25,   22,   43,   43,   43,   19,
    );
    static public $yy_lookahead = array(
 /*     0 */     2,    1,   19,   19,   21,   22,   22,    7,    8,   11,
 /*    10 */    12,   13,    1,    3,   14,   20,   21,   22,   16,   17,
 /*    20 */    18,    4,   19,   12,    9,    3,   12,   11,   11,    1,
 /*    30 */    22,   19,   19,   23,   23,   23,   18,
);
    const YY_SHIFT_USE_DFLT = -3;
    const YY_SHIFT_MAX = 13;
    static public $yy_shift_ofst = array(
 /*     0 */    -2,    0,   11,   11,   -2,   28,   14,   14,   14,   17,
 /*    10 */    10,   16,   15,   22,
);
    const YY_REDUCE_USE_DFLT = -18;
    const YY_REDUCE_MAX = 8;
    static public $yy_reduce_ofst = array(
 /*     0 */     2,   -5,  -17,  -16,   18,    8,   13,   12,    3,
);
    static public $yyExpectedTokens = array(
        /* 0 */ array(2, 11, 12, 13, ),
        /* 1 */ array(1, 7, 8, 14, ),
        /* 2 */ array(1, 12, ),
        /* 3 */ array(1, 12, ),
        /* 4 */ array(2, 11, 12, 13, ),
        /* 5 */ array(1, ),
        /* 6 */ array(12, ),
        /* 7 */ array(12, ),
        /* 8 */ array(12, ),
        /* 9 */ array(4, 11, ),
        /* 10 */ array(3, ),
        /* 11 */ array(11, ),
        /* 12 */ array(9, ),
        /* 13 */ array(3, ),
        /* 14 */ array(),
        /* 15 */ array(),
        /* 16 */ array(),
        /* 17 */ array(),
        /* 18 */ array(),
        /* 19 */ array(),
        /* 20 */ array(),
        /* 21 */ array(),
        /* 22 */ array(),
        /* 23 */ array(),
        /* 24 */ array(),
        /* 25 */ array(),
        /* 26 */ array(),
        /* 27 */ array(),
        /* 28 */ array(),
);
    static public $yy_default = array(
 /*     0 */    47,   47,   43,   43,   29,   38,   43,   43,   43,   47,
 /*    10 */    47,   47,   47,   47,   45,   35,   41,   30,   37,   31,
 /*    20 */    36,   46,   33,   42,   32,   34,   39,   44,   40,
);
    const YYNOCODE = 24;
    const YYSTACKDEPTH = 100;
    const YYNSTATE = 29;
    const YYNRULE = 18;
    const YYERRORSYMBOL = 15;
    const YYERRSYMDT = 'yy0';
    const YYFALLBACK = 1;
    static public $yyFallback = array(
    0,  /*          $ => nothing */
    0,  /*      OTHER => nothing */
    1,  /*      OPENB => OTHER */
    1,  /*     CLOSEB => OTHER */
    1,  /*        DOT => OTHER */
    1,  /* BOOLEANTRUE => OTHER */
    1,  /* BOOLEANFALSE => OTHER */
    1,  /*    SI_QSTR => OTHER */
    1,  /*    DO_QSTR => OTHER */
    1,  /*      EQUAL => OTHER */
    1,  /*      SPACE => OTHER */
    1,  /*         ID => OTHER */
    0,  /*        EOL => nothing */
    0,  /* COMMENTSTART => nothing */
    0,  /*    ML_QSTR => nothing */
    );
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

    static function PrintTrace()
    {
        self::$yyTraceFILE = fopen('php://output', 'w');
        self::$yyTracePrompt = '<br>';
    }

    static public $yyTraceFILE;
    static public $yyTracePrompt;
    public $yyidx;                    /* Index of top element in stack */
    public $yyerrcnt;                 /* Shifts left before out of the error */
    public $yystack = array();  /* The parser's stack */

    public $yyTokenName = array( 
  '$',             'OTHER',         'OPENB',         'CLOSEB',      
  'DOT',           'BOOLEANTRUE',   'BOOLEANFALSE',  'SI_QSTR',     
  'DO_QSTR',       'EQUAL',         'SPACE',         'ID',          
  'EOL',           'COMMENTSTART',  'ML_QSTR',       'error',       
  'start',         'config',        'config_element',  'opteol',      
  'value',         'text',          'textelement', 
    );

    static public $yyRuleName = array(
 /*   0 */ "start ::= config",
 /*   1 */ "config ::= config_element",
 /*   2 */ "config ::= config config_element",
 /*   3 */ "config_element ::= OPENB ID CLOSEB opteol",
 /*   4 */ "config_element ::= OPENB DOT ID CLOSEB opteol",
 /*   5 */ "config_element ::= ID EQUAL value opteol",
 /*   6 */ "config_element ::= EOL",
 /*   7 */ "config_element ::= COMMENTSTART opteol",
 /*   8 */ "config_element ::= COMMENTSTART text opteol",
 /*   9 */ "value ::= text",
 /*  10 */ "value ::= SI_QSTR",
 /*  11 */ "value ::= DO_QSTR",
 /*  12 */ "value ::= ML_QSTR",
 /*  13 */ "opteol ::= EOL",
 /*  14 */ "opteol ::=",
 /*  15 */ "text ::= text textelement",
 /*  16 */ "text ::= textelement",
 /*  17 */ "textelement ::= OTHER",
    );

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

    static function yy_destructor($yymajor, $yypminor)
    {
        switch ($yymajor) {
            default:  break;   /* If no destructor action specified: do nothing */
        }
    }

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

    function __destruct()
    {
        while ($this->yyidx >= 0) {
            $this->yy_pop_parser_stack();
        }
        if (is_resource(self::$yyTraceFILE)) {
            fclose(self::$yyTraceFILE);
        }
    }

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
                        $x = new TPC_yyStackEntry;
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
                        $x = new TPC_yyStackEntry;
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
            return;
        }
        $yytos = new TPC_yyStackEntry;
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

    static public $yyRuleInfo = array(
  array( 'lhs' => 16, 'rhs' => 1 ),
  array( 'lhs' => 17, 'rhs' => 1 ),
  array( 'lhs' => 17, 'rhs' => 2 ),
  array( 'lhs' => 18, 'rhs' => 4 ),
  array( 'lhs' => 18, 'rhs' => 5 ),
  array( 'lhs' => 18, 'rhs' => 4 ),
  array( 'lhs' => 18, 'rhs' => 1 ),
  array( 'lhs' => 18, 'rhs' => 2 ),
  array( 'lhs' => 18, 'rhs' => 3 ),
  array( 'lhs' => 20, 'rhs' => 1 ),
  array( 'lhs' => 20, 'rhs' => 1 ),
  array( 'lhs' => 20, 'rhs' => 1 ),
  array( 'lhs' => 20, 'rhs' => 1 ),
  array( 'lhs' => 19, 'rhs' => 1 ),
  array( 'lhs' => 19, 'rhs' => 0 ),
  array( 'lhs' => 21, 'rhs' => 2 ),
  array( 'lhs' => 21, 'rhs' => 1 ),
  array( 'lhs' => 22, 'rhs' => 1 ),
    );

    static public $yyReduceMap = array(
        0 => 0,
        1 => 1,
        9 => 1,
        16 => 1,
        17 => 1,
        2 => 2,
        15 => 2,
        3 => 3,
        4 => 4,
        5 => 5,
        6 => 6,
        7 => 6,
        8 => 6,
        10 => 10,
        11 => 11,
        12 => 11,
    );
#line 67 "smarty_internal_configfileparser.y"
    function yy_r0(){ $this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;     }
#line 577 "smarty_internal_configfileparser.php"
#line 73 "smarty_internal_configfileparser.y"
    function yy_r1(){$this->_retvalue = $this->yystack[$this->yyidx + 0]->minor;    }
#line 580 "smarty_internal_configfileparser.php"
#line 75 "smarty_internal_configfileparser.y"
    function yy_r2(){$this->_retvalue = $this->yystack[$this->yyidx + -1]->minor.$this->yystack[$this->yyidx + 0]->minor;    }
#line 583 "smarty_internal_configfileparser.php"
#line 81 "smarty_internal_configfileparser.y"
    function yy_r3(){ $this->hidden_section = false; $this->current_section = $this->yystack[$this->yyidx + -2]->minor; $this->_retvalue ='';    }
#line 586 "smarty_internal_configfileparser.php"
#line 83 "smarty_internal_configfileparser.y"
    function yy_r4(){ if ($this->smarty->config_read_hidden) {
                                                       $this->hidden_section = false; $this->current_section = $this->yystack[$this->yyidx + -2]->minor;
                                                      } else {$this->hidden_section = true; } $this->_retvalue ='';    }
#line 591 "smarty_internal_configfileparser.php"
#line 87 "smarty_internal_configfileparser.y"
    function yy_r5(){if (!$this->hidden_section) {
                                                   $value=$this->yystack[$this->yyidx + -1]->minor;
                                                   if ($this->smarty->config_booleanize) {
                                                       if (in_array(strtolower($value),array('on','yes','true')))
                                                          $value = true;
                                                       else if (in_array(strtolower($value),array('off','no','false')))
                                                         $value = false;
                                                   }
                                                   if ($this->current_section == null) {
                                                      if ($this->smarty->config_overwrite || !isset($this->compiler->config_data['vars'][$this->yystack[$this->yyidx + -3]->minor])) {
                                                           $this->compiler->config_data['vars'][$this->yystack[$this->yyidx + -3]->minor]=$value;
                                                        } else {
                                                          settype($this->compiler->config_data['vars'][$this->yystack[$this->yyidx + -3]->minor], 'array');
                                                          $this->compiler->config_data['vars'][$this->yystack[$this->yyidx + -3]->minor][]=$value;
                                                        }
                                                     } else {
                                                      if ($this->smarty->config_overwrite || !isset($this->compiler->config_data['sections'][$this->current_section]['vars'][$this->yystack[$this->yyidx + -3]->minor])) {
                                                          $this->compiler->config_data['sections'][$this->current_section]['vars'][$this->yystack[$this->yyidx + -3]->minor]=$value;
                                                      } else {
                                                          settype($this->compiler->config_data['sections'][$this->current_section]['vars'][$this->yystack[$this->yyidx + -3]->minor], 'array');
                                                          $this->compiler->config_data['sections'][$this->current_section]['vars'][$this->yystack[$this->yyidx + -3]->minor][]=$value;
                                                      }
                                                     }}  $this->_retvalue ='';    }
#line 616 "smarty_internal_configfileparser.php"
#line 111 "smarty_internal_configfileparser.y"
    function yy_r6(){ $this->_retvalue ='';    }
#line 619 "smarty_internal_configfileparser.php"
#line 116 "smarty_internal_configfileparser.y"
    function yy_r10(){$this->_retvalue = trim($this->yystack[$this->yyidx + 0]->minor,"'");    }
#line 622 "smarty_internal_configfileparser.php"
#line 117 "smarty_internal_configfileparser.y"
    function yy_r11(){$this->_retvalue = trim($this->yystack[$this->yyidx + 0]->minor,'"');    }
#line 625 "smarty_internal_configfileparser.php"

    private $_retvalue;

    function yy_reduce($yyruleno)
    {
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
            if (!self::$yyTraceFILE && $yysize) {
                $this->yyidx++;
                $x = new TPC_yyStackEntry;
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

    function yy_parse_failed()
    {
        if (self::$yyTraceFILE) {
            fprintf(self::$yyTraceFILE, "%sFail!\n", self::$yyTracePrompt);
        }
        while ($this->yyidx >= 0) {
            $this->yy_pop_parser_stack();
        }
    }

    function yy_syntax_error($yymajor, $TOKEN)
    {
#line 52 "smarty_internal_configfileparser.y"

    $this->internalError = true;
    $this->yymajor = $yymajor;
    $this->compiler->trigger_config_file_error();
#line 688 "smarty_internal_configfileparser.php"
    }

    function yy_accept()
    {
        if (self::$yyTraceFILE) {
            fprintf(self::$yyTraceFILE, "%sAccept!\n", self::$yyTracePrompt);
        }
        while ($this->yyidx >= 0) {
            $stack = $this->yy_pop_parser_stack();
        }
#line 44 "smarty_internal_configfileparser.y"

    $this->successful = !$this->internalError;
    $this->internalError = false;
    $this->retvalue = $this->_retvalue;
    //echo $this->retvalue."\n\n";
#line 706 "smarty_internal_configfileparser.php"
    }

    function doParse($yymajor, $yytokenvalue)
    {
        $yyerrorhit = 0;   /* True if yymajor has invoked an error */
        
        if ($this->yyidx === null || $this->yyidx < 0) {
            $this->yyidx = 0;
            $this->yyerrcnt = -1;
            $x = new TPC_yyStackEntry;
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
?>
