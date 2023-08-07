<?php

require_once __DIR__ . '/../vendor/autoload.php';

$lex = new \SmartyGenerator\LexerGenerator();
$lex->create($argv[1], $argv[2]);

