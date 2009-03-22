<?php
require_once './LexerGenerator/Regex/Parser.php';
class PHP_LexerGenerator_Regex_Lexer
{
    const MATCHSTART = PHP_LexerGenerator_Regex_Parser::MATCHSTART;
    const MATCHEND = PHP_LexerGenerator_Regex_Parser::MATCHEND;
    const CONTROLCHAR = PHP_LexerGenerator_Regex_Parser::CONTROLCHAR;
    const OPENCHARCLASS = PHP_LexerGenerator_Regex_Parser::OPENCHARCLASS;
    const FULLSTOP = PHP_LexerGenerator_Regex_Parser::FULLSTOP;
    const TEXT = PHP_LexerGenerator_Regex_Parser::TEXT;
    const BACKREFERENCE = PHP_LexerGenerator_Regex_Parser::BACKREFERENCE;
    const OPENASSERTION = PHP_LexerGenerator_Regex_Parser::OPENASSERTION;
    const COULDBEBACKREF = PHP_LexerGenerator_Regex_Parser::COULDBEBACKREF;
    const NEGATE = PHP_LexerGenerator_Regex_Parser::NEGATE;
    const HYPHEN = PHP_LexerGenerator_Regex_Parser::HYPHEN;
    const CLOSECHARCLASS = PHP_LexerGenerator_Regex_Parser::CLOSECHARCLASS;
    const BAR = PHP_LexerGenerator_Regex_Parser::BAR;
    const MULTIPLIER = PHP_LexerGenerator_Regex_Parser::MULTIPLIER;
    const INTERNALOPTIONS = PHP_LexerGenerator_Regex_Parser::INTERNALOPTIONS;
    const COLON = PHP_LexerGenerator_Regex_Parser::COLON;
    const OPENPAREN = PHP_LexerGenerator_Regex_Parser::OPENPAREN;
    const CLOSEPAREN = PHP_LexerGenerator_Regex_Parser::CLOSEPAREN;
    const PATTERNNAME = PHP_LexerGenerator_Regex_Parser::PATTERNNAME;
    const POSITIVELOOKBEHIND = PHP_LexerGenerator_Regex_Parser::POSITIVELOOKBEHIND;
    const NEGATIVELOOKBEHIND = PHP_LexerGenerator_Regex_Parser::NEGATIVELOOKBEHIND;
    const POSITIVELOOKAHEAD = PHP_LexerGenerator_Regex_Parser::POSITIVELOOKAHEAD;
    const NEGATIVELOOKAHEAD = PHP_LexerGenerator_Regex_Parser::NEGATIVELOOKAHEAD;
    const ONCEONLY = PHP_LexerGenerator_Regex_Parser::ONCEONLY;
    const COMMENT = PHP_LexerGenerator_Regex_Parser::COMMENT;
    const RECUR = PHP_LexerGenerator_Regex_Parser::RECUR;
    const ESCAPEDBACKSLASH = PHP_LexerGenerator_Regex_Parser::ESCAPEDBACKSLASH;
    private $input;
    private $N;
    public $token;
    public $value;
    public $line;

    function __construct($data)
    {
        $this->input = $data;
        $this->N = 0;
    }

    function reset($data, $line)
    {
        $this->input = $data;
        $this->N = 0;
        // passed in from parent parser
        $this->line = $line;
        $this->yybegin(self::INITIAL);
    }


    private $_yy_state = 1;
    private $_yy_stack = array();

    function yylex()
    {
        return $this->{'yylex' . $this->_yy_state}();
    }

    function yypushstate($state)
    {
        array_push($this->_yy_stack, $this->_yy_state);
        $this->_yy_state = $state;
    }

    function yypopstate()
    {
        $this->_yy_state = array_pop($this->_yy_stack);
    }

    function yybegin($state)
    {
        $this->_yy_state = $state;
    }



    function yylex1()
    {
        $tokenMap = array (
              1 => 0,
              2 => 0,
              3 => 0,
              4 => 0,
              5 => 0,
              6 => 0,
              7 => 0,
              8 => 0,
              9 => 0,
              10 => 0,
              11 => 0,
              12 => 0,
              13 => 0,
              14 => 0,
              15 => 0,
              16 => 0,
              17 => 0,
              18 => 0,
              19 => 0,
              20 => 0,
              21 => 0,
              22 => 0,
              23 => 0,
            );
        if ($this->N >= strlen($this->input)) {
            return false; // end of input
        }
        $yy_global_pattern = "/^(\\\\\\\\)|^([^[\\\\^$.|()?*+{}]+)|^(\\\\[][{}*.^$|?()+])|^(\\[)|^(\\|)|^(\\\\[frnt]|\\\\x[0-9a-fA-F][0-9a-fA-F]?|\\\\[0-7][0-7][0-7]|\\\\x\\{[0-9a-fA-F]+\\})|^(\\\\[0-9][0-9])|^(\\\\[abBGcedDsSwW0C]|\\\\c\\\\)|^(\\^)|^(\\\\A)|^(\\))|^(\\$)|^(\\*\\?|\\+\\?|[*?+]|\\{[0-9]+\\}|\\{[0-9]+,\\}|\\{[0-9]+,[0-9]+\\})|^(\\\\[zZ])|^(\\(\\?)|^(\\()|^(\\.)|^(\\\\[1-9])|^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)/";

        do {
            if (preg_match($yy_global_pattern, substr($this->input, $this->N), $yymatches)) {
                $yysubmatches = $yymatches;
                $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                if (!count($yymatches)) {
                    throw new Exception('Error: lexing failed because a rule matched' .
                        'an empty string.  Input "' . substr($this->input,
                        $this->N, 5) . '... state INITIAL');
                }
                next($yymatches); // skip global match
                $this->token = key($yymatches); // token number
                if ($tokenMap[$this->token]) {
                    // extract sub-patterns for passing to lex function
                    $yysubmatches = array_slice($yysubmatches, $this->token + 1,
                        $tokenMap[$this->token]);
                } else {
                    $yysubmatches = array();
                }
                $this->value = current($yymatches); // token value
                $r = $this->{'yy_r1_' . $this->token}($yysubmatches);
                if ($r === null) {
                    $this->N += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    // accept this token
                    return true;
                } elseif ($r === true) {
                    // we have changed state
                    // process this token in the new state
                    return $this->yylex();
                } elseif ($r === false) {
                    $this->N += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    if ($this->N >= strlen($this->input)) {
                        return false; // end of input
                    }
                    // skip this token
                    continue;
                } else {                    $yy_yymore_patterns = array(
        1 => "^([^[\\\\^$.|()?*+{}]+)|^(\\\\[][{}*.^$|?()+])|^(\\[)|^(\\|)|^(\\\\[frnt]|\\\\x[0-9a-fA-F][0-9a-fA-F]?|\\\\[0-7][0-7][0-7]|\\\\x\\{[0-9a-fA-F]+\\})|^(\\\\[0-9][0-9])|^(\\\\[abBGcedDsSwW0C]|\\\\c\\\\)|^(\\^)|^(\\\\A)|^(\\))|^(\\$)|^(\\*\\?|\\+\\?|[*?+]|\\{[0-9]+\\}|\\{[0-9]+,\\}|\\{[0-9]+,[0-9]+\\})|^(\\\\[zZ])|^(\\(\\?)|^(\\()|^(\\.)|^(\\\\[1-9])|^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        2 => "^(\\\\[][{}*.^$|?()+])|^(\\[)|^(\\|)|^(\\\\[frnt]|\\\\x[0-9a-fA-F][0-9a-fA-F]?|\\\\[0-7][0-7][0-7]|\\\\x\\{[0-9a-fA-F]+\\})|^(\\\\[0-9][0-9])|^(\\\\[abBGcedDsSwW0C]|\\\\c\\\\)|^(\\^)|^(\\\\A)|^(\\))|^(\\$)|^(\\*\\?|\\+\\?|[*?+]|\\{[0-9]+\\}|\\{[0-9]+,\\}|\\{[0-9]+,[0-9]+\\})|^(\\\\[zZ])|^(\\(\\?)|^(\\()|^(\\.)|^(\\\\[1-9])|^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        3 => "^(\\[)|^(\\|)|^(\\\\[frnt]|\\\\x[0-9a-fA-F][0-9a-fA-F]?|\\\\[0-7][0-7][0-7]|\\\\x\\{[0-9a-fA-F]+\\})|^(\\\\[0-9][0-9])|^(\\\\[abBGcedDsSwW0C]|\\\\c\\\\)|^(\\^)|^(\\\\A)|^(\\))|^(\\$)|^(\\*\\?|\\+\\?|[*?+]|\\{[0-9]+\\}|\\{[0-9]+,\\}|\\{[0-9]+,[0-9]+\\})|^(\\\\[zZ])|^(\\(\\?)|^(\\()|^(\\.)|^(\\\\[1-9])|^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        4 => "^(\\|)|^(\\\\[frnt]|\\\\x[0-9a-fA-F][0-9a-fA-F]?|\\\\[0-7][0-7][0-7]|\\\\x\\{[0-9a-fA-F]+\\})|^(\\\\[0-9][0-9])|^(\\\\[abBGcedDsSwW0C]|\\\\c\\\\)|^(\\^)|^(\\\\A)|^(\\))|^(\\$)|^(\\*\\?|\\+\\?|[*?+]|\\{[0-9]+\\}|\\{[0-9]+,\\}|\\{[0-9]+,[0-9]+\\})|^(\\\\[zZ])|^(\\(\\?)|^(\\()|^(\\.)|^(\\\\[1-9])|^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        5 => "^(\\\\[frnt]|\\\\x[0-9a-fA-F][0-9a-fA-F]?|\\\\[0-7][0-7][0-7]|\\\\x\\{[0-9a-fA-F]+\\})|^(\\\\[0-9][0-9])|^(\\\\[abBGcedDsSwW0C]|\\\\c\\\\)|^(\\^)|^(\\\\A)|^(\\))|^(\\$)|^(\\*\\?|\\+\\?|[*?+]|\\{[0-9]+\\}|\\{[0-9]+,\\}|\\{[0-9]+,[0-9]+\\})|^(\\\\[zZ])|^(\\(\\?)|^(\\()|^(\\.)|^(\\\\[1-9])|^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        6 => "^(\\\\[0-9][0-9])|^(\\\\[abBGcedDsSwW0C]|\\\\c\\\\)|^(\\^)|^(\\\\A)|^(\\))|^(\\$)|^(\\*\\?|\\+\\?|[*?+]|\\{[0-9]+\\}|\\{[0-9]+,\\}|\\{[0-9]+,[0-9]+\\})|^(\\\\[zZ])|^(\\(\\?)|^(\\()|^(\\.)|^(\\\\[1-9])|^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        7 => "^(\\\\[abBGcedDsSwW0C]|\\\\c\\\\)|^(\\^)|^(\\\\A)|^(\\))|^(\\$)|^(\\*\\?|\\+\\?|[*?+]|\\{[0-9]+\\}|\\{[0-9]+,\\}|\\{[0-9]+,[0-9]+\\})|^(\\\\[zZ])|^(\\(\\?)|^(\\()|^(\\.)|^(\\\\[1-9])|^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        8 => "^(\\^)|^(\\\\A)|^(\\))|^(\\$)|^(\\*\\?|\\+\\?|[*?+]|\\{[0-9]+\\}|\\{[0-9]+,\\}|\\{[0-9]+,[0-9]+\\})|^(\\\\[zZ])|^(\\(\\?)|^(\\()|^(\\.)|^(\\\\[1-9])|^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        9 => "^(\\\\A)|^(\\))|^(\\$)|^(\\*\\?|\\+\\?|[*?+]|\\{[0-9]+\\}|\\{[0-9]+,\\}|\\{[0-9]+,[0-9]+\\})|^(\\\\[zZ])|^(\\(\\?)|^(\\()|^(\\.)|^(\\\\[1-9])|^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        10 => "^(\\))|^(\\$)|^(\\*\\?|\\+\\?|[*?+]|\\{[0-9]+\\}|\\{[0-9]+,\\}|\\{[0-9]+,[0-9]+\\})|^(\\\\[zZ])|^(\\(\\?)|^(\\()|^(\\.)|^(\\\\[1-9])|^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        11 => "^(\\$)|^(\\*\\?|\\+\\?|[*?+]|\\{[0-9]+\\}|\\{[0-9]+,\\}|\\{[0-9]+,[0-9]+\\})|^(\\\\[zZ])|^(\\(\\?)|^(\\()|^(\\.)|^(\\\\[1-9])|^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        12 => "^(\\*\\?|\\+\\?|[*?+]|\\{[0-9]+\\}|\\{[0-9]+,\\}|\\{[0-9]+,[0-9]+\\})|^(\\\\[zZ])|^(\\(\\?)|^(\\()|^(\\.)|^(\\\\[1-9])|^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        13 => "^(\\\\[zZ])|^(\\(\\?)|^(\\()|^(\\.)|^(\\\\[1-9])|^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        14 => "^(\\(\\?)|^(\\()|^(\\.)|^(\\\\[1-9])|^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        15 => "^(\\()|^(\\.)|^(\\\\[1-9])|^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        16 => "^(\\.)|^(\\\\[1-9])|^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        17 => "^(\\\\[1-9])|^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        18 => "^(\\\\p\\{\\^?..?\\}|\\\\P\\{..?\\}|\\\\X)|^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        19 => "^(\\\\p\\{C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        20 => "^(\\\\p\\{\\^C[cfnos]?|L[lmotu]?|M[cen]?|N[dlo]?|P[cdefios]?|S[ckmo]?|Z[lps]?\\})|^(\\\\p[CLMNPSZ])|^(\\\\)",
        21 => "^(\\\\p[CLMNPSZ])|^(\\\\)",
        22 => "^(\\\\)",
        23 => "",
    );

                    // yymore is needed
                    do {
                        if (!strlen($yy_yymore_patterns[$this->token])) {
                            throw new Exception('cannot do yymore for the last token');
                        }
                        if (preg_match($yy_yymore_patterns[$this->token],
                              substr($this->input, $this->N), $yymatches)) {
                            $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                            next($yymatches); // skip global match
                            $this->token = key($yymatches); // token number
                            $this->value = current($yymatches); // token value
                            $this->line = substr_count($this->value, "\n");
                        }
                    	$r = $this->{'yy_r1_' . $this->token}();
                    } while ($r !== null || !$r);
			        if ($r === true) {
			            // we have changed state
			            // process this token in the new state
			            return $this->yylex();
			        } else {
	                    // accept
	                    $this->N += strlen($this->value);
	                    $this->line += substr_count($this->value, "\n");
	                    return true;
			        }
                }
            } else {
                throw new Exception('Unexpected input at line' . $this->line .
                    ': ' . $this->input[$this->N]);
            }
            break;
        } while (true);

    } // end function


    const INITIAL = 1;
    function yy_r1_1($yy_subpatterns)
    {

    $this->token = self::ESCAPEDBACKSLASH;
    }
    function yy_r1_2($yy_subpatterns)
    {

    $this->token = self::TEXT;
    }
    function yy_r1_3($yy_subpatterns)
    {

    $this->token = self::CONTROLCHAR;
    }
    function yy_r1_4($yy_subpatterns)
    {

    $this->token = self::OPENCHARCLASS;
    $this->yybegin(self::CHARACTERCLASSSTART);
    }
    function yy_r1_5($yy_subpatterns)
    {

    $this->token = self::BAR;
    }
    function yy_r1_6($yy_subpatterns)
    {

    $this->token = self::TEXT;
    }
    function yy_r1_7($yy_subpatterns)
    {

    $this->token = self::COULDBEBACKREF;
    }
    function yy_r1_8($yy_subpatterns)
    {

    $this->token = self::CONTROLCHAR;
    }
    function yy_r1_9($yy_subpatterns)
    {

    $this->token = self::MATCHSTART;
    }
    function yy_r1_10($yy_subpatterns)
    {

    $this->token = self::MATCHSTART;
    }
    function yy_r1_11($yy_subpatterns)
    {

    $this->token = self::CLOSEPAREN;
    $this->yybegin(self::INITIAL);
    }
    function yy_r1_12($yy_subpatterns)
    {

    $this->token = self::MATCHEND;
    }
    function yy_r1_13($yy_subpatterns)
    {

    $this->token = self::MULTIPLIER;
    }
    function yy_r1_14($yy_subpatterns)
    {

    $this->token = self::MATCHEND;
    }
    function yy_r1_15($yy_subpatterns)
    {

    $this->token = self::OPENASSERTION;
    $this->yybegin(self::ASSERTION);
    }
    function yy_r1_16($yy_subpatterns)
    {

    $this->token = self::OPENPAREN;
    }
    function yy_r1_17($yy_subpatterns)
    {

    $this->token = self::FULLSTOP;
    }
    function yy_r1_18($yy_subpatterns)
    {

    $this->token = self::BACKREFERENCE;
    }
    function yy_r1_19($yy_subpatterns)
    {

    $this->token = self::CONTROLCHAR;
    }
    function yy_r1_20($yy_subpatterns)
    {

    $this->token = self::CONTROLCHAR;
    }
    function yy_r1_21($yy_subpatterns)
    {

    $this->token = self::CONTROLCHAR;
    }
    function yy_r1_22($yy_subpatterns)
    {

    $this->token = self::CONTROLCHAR;
    }
    function yy_r1_23($yy_subpatterns)
    {

    return false;
    }


    function yylex2()
    {
        $tokenMap = array (
              1 => 0,
              2 => 0,
              3 => 0,
            );
        if ($this->N >= strlen($this->input)) {
            return false; // end of input
        }
        $yy_global_pattern = "/^(\\^)|^(\\])|^(.)/";

        do {
            if (preg_match($yy_global_pattern, substr($this->input, $this->N), $yymatches)) {
                $yysubmatches = $yymatches;
                $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                if (!count($yymatches)) {
                    throw new Exception('Error: lexing failed because a rule matched' .
                        'an empty string.  Input "' . substr($this->input,
                        $this->N, 5) . '... state CHARACTERCLASSSTART');
                }
                next($yymatches); // skip global match
                $this->token = key($yymatches); // token number
                if ($tokenMap[$this->token]) {
                    // extract sub-patterns for passing to lex function
                    $yysubmatches = array_slice($yysubmatches, $this->token + 1,
                        $tokenMap[$this->token]);
                } else {
                    $yysubmatches = array();
                }
                $this->value = current($yymatches); // token value
                $r = $this->{'yy_r2_' . $this->token}($yysubmatches);
                if ($r === null) {
                    $this->N += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    // accept this token
                    return true;
                } elseif ($r === true) {
                    // we have changed state
                    // process this token in the new state
                    return $this->yylex();
                } elseif ($r === false) {
                    $this->N += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    if ($this->N >= strlen($this->input)) {
                        return false; // end of input
                    }
                    // skip this token
                    continue;
                } else {                    $yy_yymore_patterns = array(
        1 => "^(\\])|^(.)",
        2 => "^(.)",
        3 => "",
    );

                    // yymore is needed
                    do {
                        if (!strlen($yy_yymore_patterns[$this->token])) {
                            throw new Exception('cannot do yymore for the last token');
                        }
                        if (preg_match($yy_yymore_patterns[$this->token],
                              substr($this->input, $this->N), $yymatches)) {
                            $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                            next($yymatches); // skip global match
                            $this->token = key($yymatches); // token number
                            $this->value = current($yymatches); // token value
                            $this->line = substr_count($this->value, "\n");
                        }
                    	$r = $this->{'yy_r2_' . $this->token}();
                    } while ($r !== null || !$r);
			        if ($r === true) {
			            // we have changed state
			            // process this token in the new state
			            return $this->yylex();
			        } else {
	                    // accept
	                    $this->N += strlen($this->value);
	                    $this->line += substr_count($this->value, "\n");
	                    return true;
			        }
                }
            } else {
                throw new Exception('Unexpected input at line' . $this->line .
                    ': ' . $this->input[$this->N]);
            }
            break;
        } while (true);

    } // end function


    const CHARACTERCLASSSTART = 2;
    function yy_r2_1($yy_subpatterns)
    {

    $this->token = self::NEGATE;
    }
    function yy_r2_2($yy_subpatterns)
    {

    $this->yybegin(self::CHARACTERCLASS);
    $this->token = self::TEXT;
    }
    function yy_r2_3($yy_subpatterns)
    {

    $this->yybegin(self::CHARACTERCLASS);
    return true;
    }


    function yylex3()
    {
        $tokenMap = array (
              1 => 0,
              2 => 0,
              3 => 0,
              4 => 0,
              5 => 0,
              6 => 0,
              7 => 0,
              8 => 0,
              9 => 0,
              10 => 0,
              11 => 0,
            );
        if ($this->N >= strlen($this->input)) {
            return false; // end of input
        }
        $yy_global_pattern = "/^(\\\\\\\\)|^(\\])|^(\\\\[frnt]|\\\\x[0-9a-fA-F][0-9a-fA-F]?|\\\\[0-7][0-7][0-7]|\\\\x\\{[0-9a-fA-F]+\\})|^(\\\\[bacedDsSwW0C]|\\\\c\\\\|\\\\x\\{[0-9a-fA-F]+\\}|\\\\[0-7][0-7][0-7]|\\\\x[0-9a-fA-F][0-9a-fA-F]?)|^(\\\\[0-9][0-9])|^(\\\\[1-9])|^(\\\\[]\.\-\^])|^(-(?!]))|^([^\-\\\\])|^(\\\\)|^(.)/";

        do {
            if (preg_match($yy_global_pattern, substr($this->input, $this->N), $yymatches)) {
                $yysubmatches = $yymatches;
                $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                if (!count($yymatches)) {
                    throw new Exception('Error: lexing failed because a rule matched' .
                        'an empty string.  Input "' . substr($this->input,
                        $this->N, 5) . '... state CHARACTERCLASS');
                }
                next($yymatches); // skip global match
                $this->token = key($yymatches); // token number
                if ($tokenMap[$this->token]) {
                    // extract sub-patterns for passing to lex function
                    $yysubmatches = array_slice($yysubmatches, $this->token + 1,
                        $tokenMap[$this->token]);
                } else {
                    $yysubmatches = array();
                }
                $this->value = current($yymatches); // token value
                $r = $this->{'yy_r3_' . $this->token}($yysubmatches);
                if ($r === null) {
                    $this->N += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    // accept this token
                    return true;
                } elseif ($r === true) {
                    // we have changed state
                    // process this token in the new state
                    return $this->yylex();
                } elseif ($r === false) {
                    $this->N += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    if ($this->N >= strlen($this->input)) {
                        return false; // end of input
                    }
                    // skip this token
                    continue;
                } else {                    $yy_yymore_patterns = array(
        1 => "^(\\])|^(\\\\[frnt]|\\\\x[0-9a-fA-F][0-9a-fA-F]?|\\\\[0-7][0-7][0-7]|\\\\x\\{[0-9a-fA-F]+\\})|^(\\\\[bacedDsSwW0C]|\\\\c\\\\|\\\\x\\{[0-9a-fA-F]+\\}|\\\\[0-7][0-7][0-7]|\\\\x[0-9a-fA-F][0-9a-fA-F]?)|^(\\\\[0-9][0-9])|^(\\\\[1-9])|^(\\\\[]\.\-\^])|^(-(?!]))|^([^\-\\\\])|^(\\\\)|^(.)",
        2 => "^(\\\\[frnt]|\\\\x[0-9a-fA-F][0-9a-fA-F]?|\\\\[0-7][0-7][0-7]|\\\\x\\{[0-9a-fA-F]+\\})|^(\\\\[bacedDsSwW0C]|\\\\c\\\\|\\\\x\\{[0-9a-fA-F]+\\}|\\\\[0-7][0-7][0-7]|\\\\x[0-9a-fA-F][0-9a-fA-F]?)|^(\\\\[0-9][0-9])|^(\\\\[1-9])|^(\\\\[]\.\-\^])|^(-(?!]))|^([^\-\\\\])|^(\\\\)|^(.)",
        3 => "^(\\\\[bacedDsSwW0C]|\\\\c\\\\|\\\\x\\{[0-9a-fA-F]+\\}|\\\\[0-7][0-7][0-7]|\\\\x[0-9a-fA-F][0-9a-fA-F]?)|^(\\\\[0-9][0-9])|^(\\\\[1-9])|^(\\\\[]\.\-\^])|^(-(?!]))|^([^\-\\\\])|^(\\\\)|^(.)",
        4 => "^(\\\\[0-9][0-9])|^(\\\\[1-9])|^(\\\\[]\.\-\^])|^(-(?!]))|^([^\-\\\\])|^(\\\\)|^(.)",
        5 => "^(\\\\[1-9])|^(\\\\[]\.\-\^])|^(-(?!]))|^([^\-\\\\])|^(\\\\)|^(.)",
        6 => "^(\\\\[]\.\-\^])|^(-(?!]))|^([^\-\\\\])|^(\\\\)|^(.)",
        7 => "^(-(?!]))|^([^\-\\\\])|^(\\\\)|^(.)",
        8 => "^([^\-\\\\])|^(\\\\)|^(.)",
        9 => "^(\\\\)|^(.)",
        10 => "^(.)",
        11 => "",
    );

                    // yymore is needed
                    do {
                        if (!strlen($yy_yymore_patterns[$this->token])) {
                            throw new Exception('cannot do yymore for the last token');
                        }
                        if (preg_match($yy_yymore_patterns[$this->token],
                              substr($this->input, $this->N), $yymatches)) {
                            $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                            next($yymatches); // skip global match
                            $this->token = key($yymatches); // token number
                            $this->value = current($yymatches); // token value
                            $this->line = substr_count($this->value, "\n");
                        }
                    	$r = $this->{'yy_r3_' . $this->token}();
                    } while ($r !== null || !$r);
			        if ($r === true) {
			            // we have changed state
			            // process this token in the new state
			            return $this->yylex();
			        } else {
	                    // accept
	                    $this->N += strlen($this->value);
	                    $this->line += substr_count($this->value, "\n");
	                    return true;
			        }
                }
            } else {
                throw new Exception('Unexpected input at line' . $this->line .
                    ': ' . $this->input[$this->N]);
            }
            break;
        } while (true);

    } // end function


    const CHARACTERCLASS = 3;
    function yy_r3_1($yy_subpatterns)
    {

    $this->token = self::ESCAPEDBACKSLASH;
    }
    function yy_r3_2($yy_subpatterns)
    {

    $this->yybegin(self::INITIAL);
    $this->token = self::CLOSECHARCLASS;
    }
    function yy_r3_3($yy_subpatterns)
    {

    $this->token = self::TEXT;
    }
    function yy_r3_4($yy_subpatterns)
    {

    $this->token = self::TEXT;
    }
    function yy_r3_5($yy_subpatterns)
    {

    $this->token = self::COULDBEBACKREF;
    }
    function yy_r3_6($yy_subpatterns)
    {

    $this->token = self::BACKREFERENCE;
    }
    function yy_r3_7($yy_subpatterns)
    {

    $this->token = self::TEXT;
    }
    function yy_r3_8($yy_subpatterns)
    {

    $this->token = self::HYPHEN;
    $this->yybegin(self::RANGE);
    }
    function yy_r3_9($yy_subpatterns)
    {

    $this->token = self::TEXT;
    }
    function yy_r3_10($yy_subpatterns)
    {

    return false; // ignore escaping of normal text
    }
    function yy_r3_11($yy_subpatterns)
    {

    $this->token = self::TEXT;
    }


    function yylex4()
    {
        $tokenMap = array (
              1 => 0,
              2 => 0,
              3 => 0,
              4 => 0,
              5 => 0,
              6 => 0,
              7 => 0,
            );
        if ($this->N >= strlen($this->input)) {
            return false; // end of input
        }
        $yy_global_pattern = "/^(\\\\\\\\)|^(\\\\\\])|^(\\\\[bacedDsSwW0C]|\\\\c\\\\|\\\\x\\{[0-9a-fA-F]+\\}|\\\\[0-7][0-7][0-7]|\\\\x[0-9a-fA-F][0-9a-fA-F]?)|^(\\\\[0-9][0-9])|^(\\\\[1-9])|^([^\-\\\\])|^(\\\\)/";

        do {
            if (preg_match($yy_global_pattern, substr($this->input, $this->N), $yymatches)) {
                $yysubmatches = $yymatches;
                $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                if (!count($yymatches)) {
                    throw new Exception('Error: lexing failed because a rule matched' .
                        'an empty string.  Input "' . substr($this->input,
                        $this->N, 5) . '... state RANGE');
                }
                next($yymatches); // skip global match
                $this->token = key($yymatches); // token number
                if ($tokenMap[$this->token]) {
                    // extract sub-patterns for passing to lex function
                    $yysubmatches = array_slice($yysubmatches, $this->token + 1,
                        $tokenMap[$this->token]);
                } else {
                    $yysubmatches = array();
                }
                $this->value = current($yymatches); // token value
                $r = $this->{'yy_r4_' . $this->token}($yysubmatches);
                if ($r === null) {
                    $this->N += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    // accept this token
                    return true;
                } elseif ($r === true) {
                    // we have changed state
                    // process this token in the new state
                    return $this->yylex();
                } elseif ($r === false) {
                    $this->N += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    if ($this->N >= strlen($this->input)) {
                        return false; // end of input
                    }
                    // skip this token
                    continue;
                } else {                    $yy_yymore_patterns = array(
        1 => "^(\\\\\\])|^(\\\\[bacedDsSwW0C]|\\\\c\\\\|\\\\x\\{[0-9a-fA-F]+\\}|\\\\[0-7][0-7][0-7]|\\\\x[0-9a-fA-F][0-9a-fA-F]?)|^(\\\\[0-9][0-9])|^(\\\\[1-9])|^([^\-\\\\])|^(\\\\)",
        2 => "^(\\\\[bacedDsSwW0C]|\\\\c\\\\|\\\\x\\{[0-9a-fA-F]+\\}|\\\\[0-7][0-7][0-7]|\\\\x[0-9a-fA-F][0-9a-fA-F]?)|^(\\\\[0-9][0-9])|^(\\\\[1-9])|^([^\-\\\\])|^(\\\\)",
        3 => "^(\\\\[0-9][0-9])|^(\\\\[1-9])|^([^\-\\\\])|^(\\\\)",
        4 => "^(\\\\[1-9])|^([^\-\\\\])|^(\\\\)",
        5 => "^([^\-\\\\])|^(\\\\)",
        6 => "^(\\\\)",
        7 => "",
    );

                    // yymore is needed
                    do {
                        if (!strlen($yy_yymore_patterns[$this->token])) {
                            throw new Exception('cannot do yymore for the last token');
                        }
                        if (preg_match($yy_yymore_patterns[$this->token],
                              substr($this->input, $this->N), $yymatches)) {
                            $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                            next($yymatches); // skip global match
                            $this->token = key($yymatches); // token number
                            $this->value = current($yymatches); // token value
                            $this->line = substr_count($this->value, "\n");
                        }
                    	$r = $this->{'yy_r4_' . $this->token}();
                    } while ($r !== null || !$r);
			        if ($r === true) {
			            // we have changed state
			            // process this token in the new state
			            return $this->yylex();
			        } else {
	                    // accept
	                    $this->N += strlen($this->value);
	                    $this->line += substr_count($this->value, "\n");
	                    return true;
			        }
                }
            } else {
                throw new Exception('Unexpected input at line' . $this->line .
                    ': ' . $this->input[$this->N]);
            }
            break;
        } while (true);

    } // end function


    const RANGE = 4;
    function yy_r4_1($yy_subpatterns)
    {

    $this->token = self::ESCAPEDBACKSLASH;
    }
    function yy_r4_2($yy_subpatterns)
    {

    $this->token = self::TEXT;
    $this->yybegin(self::CHARACTERCLASS);
    }
    function yy_r4_3($yy_subpatterns)
    {

    $this->token = self::TEXT;
    $this->yybegin(self::CHARACTERCLASS);
    }
    function yy_r4_4($yy_subpatterns)
    {

    $this->token = self::COULDBEBACKREF;
    }
    function yy_r4_5($yy_subpatterns)
    {

    $this->token = self::BACKREFERENCE;
    }
    function yy_r4_6($yy_subpatterns)
    {

    $this->token = self::TEXT;
    $this->yybegin(self::CHARACTERCLASS);
    }
    function yy_r4_7($yy_subpatterns)
    {

    return false; // ignore escaping of normal text
    }


    function yylex5()
    {
        $tokenMap = array (
              1 => 0,
              2 => 0,
              3 => 0,
              4 => 0,
              5 => 0,
              6 => 0,
              7 => 0,
              8 => 0,
              9 => 0,
              10 => 0,
              11 => 0,
              12 => 0,
              13 => 0,
            );
        if ($this->N >= strlen($this->input)) {
            return false; // end of input
        }
        $yy_global_pattern = "/^([imsxUX]+-[imsxUX]+|[imsxUX]+|-[imsxUX]+)|^(:)|^(\\))|^(P<[^>]+>)|^(<=)|^(<!)|^(=)|^(!)|^(>)|^(\\(\\?)|^(#[^)]+)|^(R)|^(.)/";

        do {
            if (preg_match($yy_global_pattern, substr($this->input, $this->N), $yymatches)) {
                $yysubmatches = $yymatches;
                $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                if (!count($yymatches)) {
                    throw new Exception('Error: lexing failed because a rule matched' .
                        'an empty string.  Input "' . substr($this->input,
                        $this->N, 5) . '... state ASSERTION');
                }
                next($yymatches); // skip global match
                $this->token = key($yymatches); // token number
                if ($tokenMap[$this->token]) {
                    // extract sub-patterns for passing to lex function
                    $yysubmatches = array_slice($yysubmatches, $this->token + 1,
                        $tokenMap[$this->token]);
                } else {
                    $yysubmatches = array();
                }
                $this->value = current($yymatches); // token value
                $r = $this->{'yy_r5_' . $this->token}($yysubmatches);
                if ($r === null) {
                    $this->N += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    // accept this token
                    return true;
                } elseif ($r === true) {
                    // we have changed state
                    // process this token in the new state
                    return $this->yylex();
                } elseif ($r === false) {
                    $this->N += strlen($this->value);
                    $this->line += substr_count($this->value, "\n");
                    if ($this->N >= strlen($this->input)) {
                        return false; // end of input
                    }
                    // skip this token
                    continue;
                } else {                    $yy_yymore_patterns = array(
        1 => "^(:)|^(\\))|^(P<[^>]+>)|^(<=)|^(<!)|^(=)|^(!)|^(>)|^(\\(\\?)|^(#[^)]+)|^(R)|^(.)",
        2 => "^(\\))|^(P<[^>]+>)|^(<=)|^(<!)|^(=)|^(!)|^(>)|^(\\(\\?)|^(#[^)]+)|^(R)|^(.)",
        3 => "^(P<[^>]+>)|^(<=)|^(<!)|^(=)|^(!)|^(>)|^(\\(\\?)|^(#[^)]+)|^(R)|^(.)",
        4 => "^(<=)|^(<!)|^(=)|^(!)|^(>)|^(\\(\\?)|^(#[^)]+)|^(R)|^(.)",
        5 => "^(<!)|^(=)|^(!)|^(>)|^(\\(\\?)|^(#[^)]+)|^(R)|^(.)",
        6 => "^(=)|^(!)|^(>)|^(\\(\\?)|^(#[^)]+)|^(R)|^(.)",
        7 => "^(!)|^(>)|^(\\(\\?)|^(#[^)]+)|^(R)|^(.)",
        8 => "^(>)|^(\\(\\?)|^(#[^)]+)|^(R)|^(.)",
        9 => "^(\\(\\?)|^(#[^)]+)|^(R)|^(.)",
        10 => "^(#[^)]+)|^(R)|^(.)",
        11 => "^(R)|^(.)",
        12 => "^(.)",
        13 => "",
    );

                    // yymore is needed
                    do {
                        if (!strlen($yy_yymore_patterns[$this->token])) {
                            throw new Exception('cannot do yymore for the last token');
                        }
                        if (preg_match($yy_yymore_patterns[$this->token],
                              substr($this->input, $this->N), $yymatches)) {
                            $yymatches = array_filter($yymatches, 'strlen'); // remove empty sub-patterns
                            next($yymatches); // skip global match
                            $this->token = key($yymatches); // token number
                            $this->value = current($yymatches); // token value
                            $this->line = substr_count($this->value, "\n");
                        }
                    	$r = $this->{'yy_r5_' . $this->token}();
                    } while ($r !== null || !$r);
			        if ($r === true) {
			            // we have changed state
			            // process this token in the new state
			            return $this->yylex();
			        } else {
	                    // accept
	                    $this->N += strlen($this->value);
	                    $this->line += substr_count($this->value, "\n");
	                    return true;
			        }
                }
            } else {
                throw new Exception('Unexpected input at line' . $this->line .
                    ': ' . $this->input[$this->N]);
            }
            break;
        } while (true);

    } // end function


    const ASSERTION = 5;
    function yy_r5_1($yy_subpatterns)
    {

    $this->token = self::INTERNALOPTIONS;
    }
    function yy_r5_2($yy_subpatterns)
    {

    $this->token = self::COLON;
    $this->yybegin(self::INITIAL);
    }
    function yy_r5_3($yy_subpatterns)
    {

    $this->token = self::CLOSEPAREN;
    $this->yybegin(self::INITIAL);
    }
    function yy_r5_4($yy_subpatterns)
    {

    $this->token = self::PATTERNNAME;
    $this->yybegin(self::INITIAL);
    }
    function yy_r5_5($yy_subpatterns)
    {

    $this->token = self::POSITIVELOOKBEHIND;
    $this->yybegin(self::INITIAL);
    }
    function yy_r5_6($yy_subpatterns)
    {

    $this->token = self::NEGATIVELOOKBEHIND;
    $this->yybegin(self::INITIAL);
    }
    function yy_r5_7($yy_subpatterns)
    {

    $this->token = self::POSITIVELOOKAHEAD;
    $this->yybegin(self::INITIAL);
    }
    function yy_r5_8($yy_subpatterns)
    {

    $this->token = self::NEGATIVELOOKAHEAD;
    $this->yybegin(self::INITIAL);
    }
    function yy_r5_9($yy_subpatterns)
    {

    $this->token = self::ONCEONLY;
    $this->yybegin(self::INITIAL);
    }
    function yy_r5_10($yy_subpatterns)
    {

    $this->token = self::OPENASSERTION;
    }
    function yy_r5_11($yy_subpatterns)
    {

    $this->token = self::COMMENT;
    $this->yybegin(self::INITIAL);
    }
    function yy_r5_12($yy_subpatterns)
    {

    $this->token = self::RECUR;
    }
    function yy_r5_13($yy_subpatterns)
    {

    $this->yybegin(self::INITIAL);
    return true;
    }

}
