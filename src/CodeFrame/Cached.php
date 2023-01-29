<?php

namespace Smarty\CodeFrame;

abstract class Cached extends Base {

	// @TODO move
	public function isFresh(\Smarty\Template $template): bool {

		$properties = $this->getProperties();

		// on cache resources other than file check version stored in cache code
		if (\Smarty\Smarty::SMARTY_VERSION !== $properties['version']) {
			return false;
		}

		if ($template->getSmarty()->getCompileCheck() === \Smarty\Smarty::COMPILECHECK_ON) {
			if (!$this->checkFileDependencies($properties['file_dependency'], $template)) {
				return false;
			}
		}

		// CACHING_LIFETIME_SAVED cache expiry has to be validated here since otherwise we'd define the unifunc
		if ($template->caching === \Smarty\Smarty::CACHING_LIFETIME_SAVED && $properties['cache_lifetime'] >= 0
			&& (time() > ($this->timestamp + $properties['cache_lifetime']))
		) {
			return false;
		}

		return true;
	}
}