<?php

namespace Smarty\CodeFrame;

abstract class Base {

	abstract public function renderContent(\Smarty\Template $_smarty_tpl): void;
}