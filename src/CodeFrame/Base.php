<?php

namespace Smarty\CodeFrame;

abstract class Base {

	/**
	 * @var \Smarty\Smarty
	 */
	private $smarty;

	public function __construct(\Smarty\Smarty $smarty) {
		$this->smarty = $smarty;
	}

	public function isFresh(\Smarty\Template $template): bool {
		return $template->isFresh($this->getProperties(), $this->isCache());
	}

	protected function isCache(): bool {
		return false;
	}

	abstract protected function getProperties(): array;
}