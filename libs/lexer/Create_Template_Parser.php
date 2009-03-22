<?php
// Create Parser
passthru('C:\wamp\bin\php\php5.2.8\php ./ParserGenerator/cli.php internal.templateparser.y');

// Create Lexer
require_once './LexerGenerator.php';
$lex = new PHP_LexerGenerator('internal.templatelexer.plex');
$contents = file_get_contents('internal.templatelexer.php');
$contents = str_replace(array('SMARTYldel','SMARTYrdel'),array('".$this->ldel."','".$this->rdel."'),$contents);
//$contents = preg_replace('%/\*[\s\S]+?\*/|(?://|#).*(?:\r\n|\n)%m', '', $contents);
file_put_contents('internal.templatelexer.php', $contents);
//$contents = file_get_contents('internal.templateparser.php');
//$contents = preg_replace('%/\*[\s\S]+?\*/|(?://|#).*(?:\r\n|\n)%m', '', $contents);
//file_put_contents('internal.templateparser.php', $contents);
copy('internal.templatelexer.php','../../distribution/libs/sysplugins/internal.templatelexer.php');
copy('internal.templateparser.php','../../distribution/libs/sysplugins/internal.templateparser.php');
//unlink('internal.templatelexer.php');
//unlink('internal.templateparser.php');

?>
