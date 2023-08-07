<?php

require_once __DIR__ . '/../vendor/autoload.php';

$parser = new \SmartyGenerator\ParserGenerator();
$parser->setQuiet();
$parser->main($argv[1], $argv[2]);

$content = file_get_contents($argv[2]);
$content = preg_replace(array('#/\*\s*\d+\s*\*/#', "#'lhs'#", "#'rhs'#"), array('', 0, 1), $content);
file_put_contents($argv[2], $content);
