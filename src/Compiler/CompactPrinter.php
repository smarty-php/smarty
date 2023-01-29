<?php

namespace Smarty\Compiler;

class CompactPrinter extends \Nette\PhpGenerator\Printer {
	protected $indentation = "";
	protected $linesBetweenProperties = 0;
	protected $linesBetweenMethods = 0;
	protected $returnTypeColon = ': ';
}